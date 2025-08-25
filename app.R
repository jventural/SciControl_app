# APLICACIÓN SHINY SCICONTROL COMPLETA CON OAUTH 2.0
# ===================================================vvv

# Cargar librerías necesarias
library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(readxl)
library(writexl)
library(shinyjs)
library(utils)
library(tools)
library(httr)
library(jsonlite)
library(base64enc)
library(ggplot2)
library(plotly)
library(dplyr)
library(httpuv)

# Configurar zona horaria
Sys.setenv(TZ = "America/Lima")

# ============================================================================
# CONFIGURACIÓN DE MODO DESARROLLADOR
# ============================================================================

# Contraseña para acceso de desarrollador (puedes cambiarla)
DEVELOPER_PASSWORD <- "scicontrol2025"

# Variable para controlar el acceso de desarrollador (se reinicia con la app)
developer_authenticated <- FALSE

# Operador auxiliar
`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================================
# CONFIGURACIÓN OAUTH 2.0 PARA DROPBOX
# ============================================================================

# Configuración de la aplicación Dropbox (tus credenciales reales)
DROPBOX_APP_KEY <- "u345780r1sjewxw"
DROPBOX_APP_SECRET <- "u2l2isg4nophe34"  # Corregido: era "j21215g4nophe34"
DROPBOX_REDIRECT_URI <- "http://localhost:1410/"

# Variables globales para tokens
DROPBOX_ACCESS_TOKEN <- ""
DROPBOX_REFRESH_TOKEN <- ""

if (nzchar(env_rt <- Sys.getenv("DROPBOX_REFRESH_TOKEN", ""))) {
  DROPBOX_REFRESH_TOKEN <<- env_rt
  cat("🔑 Refresh token cargado desde variable de entorno\n")
}


TOKEN_EXPIRY_TIME <- NULL

# Función para generar URL de autorización
generate_auth_url <- function() {
  # Base de la URL de autorización
  base_url <- "https://www.dropbox.com/oauth2/authorize"

  # Codificamos client_id y redirect_uri
  client_id_encoded    <- URLencode(DROPBOX_APP_KEY,      reserved = TRUE)
  redirect_uri_encoded <- URLencode(DROPBOX_REDIRECT_URI, reserved = TRUE)

  # Construimos la URL con token_access_type=offline para recibir refresh_token
  paste0(
    base_url,
    "?client_id=",        client_id_encoded,
    "&response_type=code",
    "&redirect_uri=",     redirect_uri_encoded,
    "&token_access_type=offline"
  )
}


# Función para iniciar servidor temporal y capturar código
capture_authorization_code <- function() {
  auth_code <- NULL

  app <- list(
    call = function(req) {
      cat("📥 Request recibida en servidor local:\n")
      cat("  - Path:", req$PATH_INFO, "\n")
      cat("  - Query:", req$QUERY_STRING, "\n")
      cat("  - Method:", req$REQUEST_METHOD, "\n")

      query_string <- req$QUERY_STRING
      if (!is.null(query_string) && grepl("code=", query_string)) {
        params <- strsplit(query_string, "&")[[1]]
        code_param <- params[grep("code=", params)]
        if (length(code_param) > 0) {
          auth_code <<- gsub("code=", "", code_param[1])
          cat("✅ Código de autorización capturado:", substr(auth_code, 1, 20), "...\n")
        }
      }

      if (!is.null(auth_code)) {
        response_body <- "<html><head><title>Autorización Exitosa</title></head><body style='font-family: Arial, sans-serif; text-align: center; margin: 50px;'><h1 style='color: green;'>¡Autorización Exitosa!</h1><p>El código ha sido capturado correctamente.</p><p><strong>Puedes cerrar esta ventana y volver a tu aplicación Shiny.</strong></p><script>setTimeout(function(){window.close();}, 3000);</script></body></html>"
        status <- 200L
      } else {
        response_body <- "<html><head><title>Esperando Autorización</title></head><body style='font-family: Arial, sans-serif; text-align: center; margin: 50px;'><h1 style='color: orange;'>Esperando Autorización...</h1><p>Si ves esta página, el servidor está funcionando.</p><p>Regresa a Dropbox y completa la autorización.</p></body></html>"
        status <- 200L
      }

      list(
        status = status,
        headers = list(
          'Content-Type' = 'text/html',
          'Access-Control-Allow-Origin' = '*',
          'Access-Control-Allow-Methods' = 'GET, POST, OPTIONS',
          'Access-Control-Allow-Headers' = 'Content-Type'
        ),
        body = response_body
      )
    }
  )

  # Intentar varios puertos si 1410 está ocupado
  ports_to_try <- c(1410, 1411, 1412, 1413, 1414)
  server <- NULL
  port_used <- NULL

  for (port in ports_to_try) {
    tryCatch({
      server <- startServer("127.0.0.1", port, app)
      port_used <- port
      cat("✅ Servidor HTTP iniciado en puerto:", port, "\n")
      cat("🌐 Servidor accesible en: http://localhost:", port, "/\n")
      break
    }, error = function(e) {
      cat("⚠️ Puerto", port, "ocupado:", e$message, "\n")
    })
  }

  if (is.null(server)) {
    stop("❌ No se pudo iniciar servidor en ningún puerto (1410-1414)")
  }

  on.exit(stopServer(server), add = TRUE)

  # Actualizar redirect URI si usamos un puerto diferente
  if (port_used != 1410) {
    DROPBOX_REDIRECT_URI <<- paste0("http://localhost:", port_used, "/")
    cat("🔄 Redirect URI actualizado a:", DROPBOX_REDIRECT_URI, "\n")
  }

  auth_url <- generate_auth_url()
  browseURL(auth_url)

  cat("\n=== INSTRUCCIONES PARA EL USUARIO ===\n")
  cat("🌐 Se abrió el navegador con la página de autorización de Dropbox.\n")
  cat("📋 Pasos a seguir:\n")
  cat("  1. En la página de Dropbox, haz clic en 'Permitir'\n")
  cat("  2. Deberías ser redirigido a: http://localhost:", port_used, "/\n")
  cat("  3. Si no funciona automáticamente, mira la URL del navegador después del clic\n")
  cat("  4. Busca '?code=' en la URL y copia el código que aparece después\n")
  cat("=====================================\n\n")
  cat("⏳ Esperando código de autorización... (máximo 5 minutos)\n")

  start_time <- Sys.time()
  timeout <- 300

  while (is.null(auth_code) && as.numeric(difftime(Sys.time(), start_time, units = "secs")) < timeout) {
    Sys.sleep(1)

    # Mostrar progreso cada 30 segundos
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed %% 30 == 0) {
      cat("⏳ Esperando... (", round(elapsed), " segundos transcurridos)\n")
    }
  }

  if (is.null(auth_code)) {
    cat("\n❌ TIMEOUT: No se recibió el código automáticamente.\n")
    cat("🔧 SOLUCIÓN MANUAL:\n")
    cat("1. Revisa si en tu navegador hay una URL que empiece con:\n")
    cat("   http://localhost:", port_used, "/?code=\n")
    cat("2. Si la hay, copia todo el texto que aparece después de 'code=' y antes de '&' (si lo hay)\n")
    cat("3. Usa la función manual que agregaré a la interfaz\n\n")

    stop("Manual intervention required - see instructions above")
  }

  return(auth_code)
}

# Función para intercambiar código por tokens
exchange_code_for_tokens <- function(auth_code) {
  cat("🔄 Intercambiando código por tokens...\n")
  cat("📝 Código recibido:", substr(auth_code, 1, 20), "...\n")

  tryCatch({
    # Preparar datos para el POST
    post_data <- list(
      code = auth_code,
      grant_type = "authorization_code",
      client_id = DROPBOX_APP_KEY,
      client_secret = DROPBOX_APP_SECRET,
      redirect_uri = DROPBOX_REDIRECT_URI
    )

    cat("📤 Enviando request a Dropbox...\n")
    cat("🔧 Client ID:", DROPBOX_APP_KEY, "\n")
    cat("🔧 Redirect URI:", DROPBOX_REDIRECT_URI, "\n")

    response <- httr::POST(
      url = "https://api.dropboxapi.com/oauth2/token",
      body = post_data,
      encode = "form",
      httr::timeout(30)  # 30 segundos timeout
    )

    cat("📥 Response status:", response$status_code, "\n")

    if (response$status_code == 200) {
      token_data <- httr::content(response, as = "parsed")

      cat("✅ Tokens obtenidos exitosamente\n")
      cat("🔑 Access token:", substr(token_data$access_token, 1, 20), "...\n")
      cat("🔄 Refresh token completo:", token_data$refresh_token, "\n")
      cat("⏰ Expira en:", token_data$expires_in, "segundos\n")

      # Guardar tokens globalmente
      DROPBOX_ACCESS_TOKEN <<- token_data$access_token
      DROPBOX_REFRESH_TOKEN <<- token_data$refresh_token
      TOKEN_EXPIRY_TIME <<- Sys.time() + as.difftime(token_data$expires_in, units = "secs")

      # Guardar tokens en archivo para persistencia
      tokens <- list(
        access_token = DROPBOX_ACCESS_TOKEN,
        refresh_token = DROPBOX_REFRESH_TOKEN,
        expires_at = as.character(TOKEN_EXPIRY_TIME)
      )

      saveRDS(tokens, "dropbox_tokens.rds")

      cat("💾 Tokens guardados en archivo\n")
      cat("✅ OAuth completado exitosamente\n")

      return(TRUE)
    } else {
      error_content <- httr::content(response, "text")
      cat("❌ Error HTTP", response$status_code, "\n")
      cat("📄 Respuesta:", error_content, "\n")

      # Diagnóstico de errores comunes
      if (response$status_code == 400) {
        if (grepl("invalid_grant", error_content)) {
          cat("🔍 Diagnóstico: El código de autorización puede haber expirado o ya fue usado\n")
          cat("💡 Solución: Intenta obtener un nuevo código de autorización\n")
        } else if (grepl("invalid_client", error_content)) {
          cat("🔍 Diagnóstico: App Key o App Secret incorrectos\n")
          cat("💡 Solución: Verifica las credenciales en la configuración de Dropbox\n")
        } else if (grepl("redirect_uri_mismatch", error_content)) {
          cat("🔍 Diagnóstico: Redirect URI no coincide con la configuración de Dropbox\n")
          cat("💡 Solución: Verifica que", DROPBOX_REDIRECT_URI, "esté configurado en tu app\n")
        }
      }

      stop("Error al obtener tokens: ", error_content)
    }
  }, error = function(e) {
    cat("❌ Error en intercambio de tokens:", e$message, "\n")
    stop("Error en intercambio de tokens: ", e$message)
  })
}

# Función para cargar tokens guardados
load_saved_tokens <- function() {
  if (file.exists("dropbox_tokens.rds")) {
    tryCatch({
      tokens <- readRDS("dropbox_tokens.rds")
      DROPBOX_ACCESS_TOKEN <<- tokens$access_token
      DROPBOX_REFRESH_TOKEN <<- tokens$refresh_token
      TOKEN_EXPIRY_TIME <<- as.POSIXct(tokens$expires_at)

      cat("Tokens cargados desde archivo.\n")
      return(TRUE)
    }, error = function(e) {
      cat("Error al cargar tokens:", e$message, "\n")
      return(FALSE)
    })
  }
  return(FALSE)
}

# Función para refrescar access token
refresh_dropbox_access_token <- function() {
  if (DROPBOX_REFRESH_TOKEN == "") {
    cat("No hay refresh token disponible.\n")
    return(FALSE)
  }

  tryCatch({
    response <- httr::POST(
      url = "https://api.dropboxapi.com/oauth2/token",
      body = list(
        grant_type = "refresh_token",
        refresh_token = DROPBOX_REFRESH_TOKEN,
        client_id = DROPBOX_APP_KEY,
        client_secret = DROPBOX_APP_SECRET
      ),
      encode = "form"
    )

    if (response$status_code == 200) {
      token_data <- httr::content(response, as = "parsed")
      DROPBOX_ACCESS_TOKEN <<- token_data$access_token
      TOKEN_EXPIRY_TIME <<- Sys.time() + as.difftime(token_data$expires_in, units = "secs")

      tokens <- list(
        access_token = DROPBOX_ACCESS_TOKEN,
        refresh_token = DROPBOX_REFRESH_TOKEN,
        expires_at = as.character(TOKEN_EXPIRY_TIME)
      )
      saveRDS(tokens, "dropbox_tokens.rds")

      cat("Access token refrescado exitosamente.\n")
      return(TRUE)
    } else {
      error_content <- httr::content(response, "text")
      cat("Error al refrescar token:", error_content, "\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("Error al refrescar token:", e$message, "\n")
    return(FALSE)
  })
}

# Función principal para obtener token válido
get_dropbox_token <- function() {
  if (DROPBOX_ACCESS_TOKEN == "") {
    load_saved_tokens()
  }

  if (!is.null(TOKEN_EXPIRY_TIME) && TOKEN_EXPIRY_TIME <= Sys.time()) {
    cat("⚠️ Token expirado o en el instante de expirar, refrescando…\n")
    if (refresh_dropbox_access_token()) {
      cat("✅ Token refrescado exitosamente a las", format(Sys.time()), "\n")
    } else {
      cat("❌ Falló el refresco automático de token\n")
    }
  }

  return(DROPBOX_ACCESS_TOKEN)
}

# Función para verificar configuración OAuth
check_dropbox_config <- function() {
  config_ok <- DROPBOX_APP_KEY != "" && DROPBOX_APP_SECRET != ""
  if (!config_ok) return(FALSE)

  token <- get_dropbox_token()
  return(token != "" && !is.null(token))
}

# Función para inicializar OAuth
initialize_dropbox_oauth <- function() {
  cat("Inicializando OAuth de Dropbox...\n")

  # 0️⃣ Intento refrescar desde el refresh token de entorno
  if (nzchar(env_rt <- Sys.getenv("DROPBOX_REFRESH_TOKEN", ""))) {
    DROPBOX_REFRESH_TOKEN <<- env_rt
    cat("🔑 Refresh token cargado desde variable de entorno\n")
    cat("🔄 Intentando refrescar access token con ese refresh token…\n")
    if (refresh_dropbox_access_token()) {
      cat("✅ Token refrescado exitosamente (vence en:", format(TOKEN_EXPIRY_TIME), ")\n")
      return(TRUE)
    } else {
      cat("⚠️ Falló refresco desde entorno, continúa flujo normal.\n")
    }
  }

  # 1️⃣ Intento cargar tokens guardados en disco
  if (load_saved_tokens()) {
    cat("✅ Tokens existentes cargados.\n")

    # 2️⃣ Si no hay expiry o caduca en más de 10 minutos, está OK
    if (is.null(TOKEN_EXPIRY_TIME) ||
        difftime(TOKEN_EXPIRY_TIME, Sys.time(), units = "mins") > 10) {
      cat("✅ Token válido hasta:", format(TOKEN_EXPIRY_TIME), "\n")
      return(TRUE)
    }

    # 3️⃣ Si está cercano a expirar (≤10 min) o ya expiró → refrescar
    cat("⚠️ Token próximo a expirar o expirado. Intentando refrescar…\n")
    if (refresh_dropbox_access_token()) {
      cat("✅ Token refrescado exitosamente. Nuevo vencimiento:",
          format(TOKEN_EXPIRY_TIME), "\n")
      return(TRUE)
    } else {
      cat("❌ Error al refrescar token.\n")
    }
  }

  # 4️⃣ Si llegamos aquí, no hay tokens válidos → OAuth manual
  cat("❌ No hay tokens válidos. Ejecutar autorización OAuth.\n")
  return(FALSE)
}

# Función para iniciar flujo OAuth completo
start_oauth_flow <- function() {
  cat("=== INICIANDO FLUJO OAUTH DE DROPBOX ===\n")
  cat("🔧 App Key:", DROPBOX_APP_KEY, "\n")
  cat("🔧 Redirect URI:", DROPBOX_REDIRECT_URI, "\n")

  # Verificar configuración
  if (DROPBOX_APP_KEY == "" || DROPBOX_APP_SECRET == "") {
    cat("❌ Error: App Key o App Secret vacíos\n")
    return(FALSE)
  }

  if (nchar(DROPBOX_APP_KEY) < 10 || nchar(DROPBOX_APP_SECRET) < 10) {
    cat("❌ Error: App Key o App Secret muy cortos\n")
    return(FALSE)
  }

  tryCatch({
    # Paso 1: Capturar código de autorización
    cat("📱 Abriendo navegador para autorización...\n")
    auth_code <- capture_authorization_code()
    cat("✅ Código de autorización obtenido:", substr(auth_code, 1, 10), "...\n")

    # Paso 2: Intercambiar código por tokens
    if (exchange_code_for_tokens(auth_code)) {
      cat("✅ Flujo OAuth completado exitosamente.\n")
      return(TRUE)
    }
  }, error = function(e) {
    cat("❌ Error en flujo OAuth:", e$message, "\n")

    # Si es un timeout, sugerir método manual
    if (grepl("Manual intervention required", e$message)) {
      cat("\n🔧 SUGERENCIA: Usa el botón 'Autorización Manual' en la interfaz.\n")
      cat("📋 Esto te permitirá copiar/pegar el código manualmente.\n")
    }

    return(FALSE)
  })

  return(FALSE)
}

# ============================================================================
# FUNCIONES DE SANITIZACIÓN Y VALIDACIÓN
# ============================================================================

sanitize_project_name <- function(project_name) {
  if (is.null(project_name) || is.na(project_name) || project_name == "") {
    return("proyecto_sin_nombre")
  }

  name <- as.character(project_name)
  name <- iconv(name, to = "ASCII//TRANSLIT")

  # Reemplazar caracteres problemáticos
  name <- gsub("[<>:\"|\\?\\*\\\\/<>\\[\\]\\{\\}\\(\\)@#\\$%\\^&\\+=;',~`]", "_", name)
  name <- gsub("\\s", "_", name)
  name <- gsub("\\.", "_", name)
  name <- gsub("[[:cntrl:]]", "", name)
  name <- gsub("[^a-zA-Z0-9_-]", "_", name)
  name <- gsub("_{2,}", "_", name)
  name <- gsub("^_+|_+$", "", name)

  if (name == "" || nchar(name) == 0) {
    name <- "proyecto_sin_nombre"
  }

  if (nchar(name) > 30) {
    name <- substr(name, 1, 30)
    name <- gsub("_+$", "", name)
  }

  return(name)
}

sanitize_file_name <- function(file_name) {
  if (is.null(file_name) || is.na(file_name) || file_name == "") {
    return("archivo_sin_nombre.txt")
  }

  file_parts <- tools::file_path_sans_ext(file_name)
  file_ext <- tools::file_ext(file_name)

  clean_name <- iconv(file_parts, to = "ASCII//TRANSLIT")
  clean_name <- gsub("[<>:\"|\\?\\*\\\\/<>\\[\\]\\{\\}\\(\\)@#\\$%\\^&\\+=;',~`]", "_", clean_name)
  clean_name <- gsub("\\s", "_", clean_name)
  clean_name <- gsub("[^a-zA-Z0-9_.-]", "_", clean_name)
  clean_name <- gsub("_{2,}", "_", clean_name)
  clean_name <- gsub("^_+|_+$", "", clean_name)

  if (clean_name == "" || nchar(clean_name) == 0) {
    clean_name <- "archivo"
  }

  if (nchar(clean_name) > 50) {
    clean_name <- substr(clean_name, 1, 50)
    clean_name <- gsub("_+$", "", clean_name)
  }

  if (file_ext != "") {
    file_ext <- gsub("[^a-zA-Z0-9]", "", file_ext)
    return(paste0(clean_name, ".", file_ext))
  } else {
    return(clean_name)
  }
}

validate_dropbox_path <- function(path) {
  if (is.null(path) || is.na(path) || path == "") {
    return("/archivo_sin_nombre.txt")
  }

  if (!startsWith(path, "/")) {
    path <- paste0("/", path)
  }

  path <- gsub("/+", "/", path)

  if (nchar(path) > 400) {
    warning("Path muy largo, puede causar problemas en Dropbox")
    path_parts <- strsplit(path, "/")[[1]]
    filename <- path_parts[length(path_parts)]
    folder_parts <- path_parts[-length(path_parts)]

    while (nchar(paste(folder_parts, collapse = "/")) + nchar(filename) > 390 && length(folder_parts) > 1) {
      folder_parts <- folder_parts[-length(folder_parts)]
    }

    path <- paste0(paste(folder_parts, collapse = "/"), "/", filename)
  }

  return(path)
}

# ============================================================================
# FUNCIONES DE DROPBOX CON OAUTH
# ============================================================================

upload_to_dropbox <- function(file_path, file_name, folder_path = NULL) {
  tryCatch({
    if (!file.exists(file_path)) {
      return(list(success = FALSE, error = "El archivo no existe"))
    }

    if (is.null(file_name) || file_name == "") {
      file_name <- basename(file_path)
    }

    clean_file_name <- sanitize_file_name(file_name)
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_base <- tools::file_path_sans_ext(clean_file_name)
    file_ext <- tools::file_ext(clean_file_name)

    if (file_ext != "") {
      unique_name <- paste0(file_base, "_", timestamp, ".", file_ext)
    } else {
      unique_name <- paste0(file_base, "_", timestamp)
    }

    if (!is.null(folder_path) && folder_path != "") {
      folder_path <- gsub("^/+|/+$", "", folder_path)
      folder_path <- gsub("/+", "/", folder_path)
      dropbox_path <- paste0("/", folder_path, "/", unique_name)
    } else {
      dropbox_path <- paste0("/", unique_name)
    }

    dropbox_path <- validate_dropbox_path(dropbox_path)

    file_content <- readBin(file_path, "raw", file.info(file_path)$size)

    dropbox_api_arg <- list(
      path = dropbox_path,
      mode = "add",
      autorename = TRUE,
      mute = FALSE
    )

    response <- POST(
      url = "https://content.dropboxapi.com/2/files/upload",
      add_headers(
        "Authorization" = paste("Bearer", get_dropbox_token()),
        "Dropbox-API-Arg" = jsonlite::toJSON(dropbox_api_arg, auto_unbox = TRUE),
        "Content-Type" = "application/octet-stream"
      ),
      body = file_content
    )

    if (response$status_code == 200) {
      result <- content(response, "parsed")

      share_url <- ""
      tryCatch({
        share_response <- POST(
          url = "https://api.dropboxapi.com/2/sharing/create_shared_link_with_settings",
          add_headers(
            "Authorization" = paste("Bearer", get_dropbox_token()),
            "Content-Type" = "application/json"
          ),
          body = jsonlite::toJSON(list(
            path = result$path_display,
            settings = list(requested_visibility = "public")
          ), auto_unbox = TRUE)
        )

        if (share_response$status_code == 200) {
          share_result <- content(share_response, "parsed")
          share_url <- gsub("\\?dl=0", "?dl=1", share_result$url)
        }
      }, error = function(e) {
        cat("Error creando enlace compartido:", e$message, "\n")
      })

      return(list(
        success = TRUE,
        path = result$path_display,
        name = result$name,
        size = result$size,
        url = share_url,
        original_name = file_name,
        clean_name = unique_name
      ))
    } else {
      error_content <- content(response, "text")
      return(list(
        success = FALSE,
        error = paste("Error HTTP:", response$status_code, "-", error_content)
      ))
    }
  }, error = function(e) {
    return(list(success = FALSE, error = as.character(e)))
  })
}

list_dropbox_files <- function(folder_path = NULL) {
  tryCatch({
    search_path <- if (is.null(folder_path)) "" else paste0("/", folder_path)

    response <- POST(
      url = "https://api.dropboxapi.com/2/files/list_folder",
      add_headers(
        "Authorization" = paste("Bearer", get_dropbox_token()),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(list(
        path = search_path,
        recursive = TRUE,
        include_media_info = FALSE,
        include_deleted = FALSE,
        include_has_explicit_shared_members = FALSE
      ), auto_unbox = TRUE)
    )

    if (response$status_code == 200) {
      result <- content(response, "parsed")
      if (length(result$entries) > 0) {
        files <- result$entries[sapply(result$entries, function(x) x$.tag == "file")]
        if (length(files) > 0) {
          files_df <- data.frame(
            name = sapply(files, function(x) basename(x$name)),
            path = sapply(files, function(x) x$path_display),
            size = sapply(files, function(x) x$size),
            modified = sapply(files, function(x) x$client_modified),
            stringsAsFactors = FALSE
          )
          return(files_df)
        }
      }
    }
    return(data.frame())
  }, error = function(e) {
    warning("Error al listar archivos: ", e$message)
    return(data.frame())
  })
}

delete_dropbox_file <- function(file_path) {
  tryCatch({
    response <- POST(
      url = "https://api.dropboxapi.com/2/files/delete_v2",
      add_headers(
        "Authorization" = paste("Bearer", get_dropbox_token()),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(list(path = file_path), auto_unbox = TRUE)
    )
    return(response$status_code == 200)
  }, error = function(e) {
    warning("Error al eliminar archivo: ", e$message)
    return(FALSE)
  })
}

# ============================================================================
# FUNCIONES DE ALMACENAMIENTO DE DATOS
# ============================================================================

save_data_to_dropbox <- function(project_data) {
  # Crear un archivo temporal .xlsx
  temp_file <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(project_data, temp_file)

  # Timestamp para los backups
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # 1) Subir el archivo principal SIN timestamp en modo overwrite
  dropbox_api_arg_main <- list(
    path       = "/scicontrol/data/project_data.xlsx",
    mode       = "overwrite",
    autorename = FALSE,
    mute       = FALSE
  )
  main_resp <- httr::POST(
    url = "https://content.dropboxapi.com/2/files/upload",
    httr::add_headers(
      "Authorization"    = paste("Bearer", get_dropbox_token()),
      "Dropbox-API-Arg"  = jsonlite::toJSON(dropbox_api_arg_main, auto_unbox = TRUE),
      "Content-Type"     = "application/octet-stream"
    ),
    body    = readBin(temp_file, "raw", file.info(temp_file)$size),
    httr::timeout(60)
  )
  if (main_resp$status_code != 200) {
    err <- httr::content(main_resp, "text")
    unlink(temp_file)
    stop("Error al subir archivo principal a Dropbox: ", err)
  }
  main_result <- httr::content(main_resp, as = "parsed")

  # 2) Subir un backup con timestamp
  backup_result <- upload_to_dropbox(
    file_path   = temp_file,
    file_name   = paste0("project_data_backup_", timestamp, ".xlsx"),
    folder_path = "scicontrol/backups"
  )

  # Limpiar archivo temporal
  unlink(temp_file)

  # Retornar rutas y estado
  return(list(
    success     = TRUE,
    main_path   = main_result$path_display,
    backup_path = if (backup_result$success) backup_result$path else NULL,
    error       = if (!backup_result$success) backup_result$error else NULL
  ))
}




load_data_from_dropbox <- function() {
  tryCatch({
    response <- GET(
      url = "https://content.dropboxapi.com/2/files/download",
      add_headers(
        "Authorization" = paste("Bearer", get_dropbox_token()),
        "Dropbox-API-Arg" = jsonlite::toJSON(list(
          path = "/scicontrol/data/project_data.xlsx"
        ), auto_unbox = TRUE)
      )
    )

    if (response$status_code == 200) {
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(content(response, "raw"), temp_file)

      project_data <- read_excel(temp_file)
      unlink(temp_file)

      required_columns <- c("Nombre", "Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                            "Revista", "Cuartil", "Estado", "Grupo", "Progreso",
                            "Fecha_Aceptado", "Fecha_Publicado", "Linea_Investigacion",
                            "Observaciones")

      missing_columns <- setdiff(required_columns, colnames(project_data))
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          project_data[[col]] <- NA
        }
      }

      date_columns <- c("Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                        "Fecha_Aceptado", "Fecha_Publicado")

      for (col in date_columns) {
        if (col %in% colnames(project_data)) {
          if (inherits(project_data[[col]], c("POSIXct", "POSIXt", "Date"))) {
            project_data[[col]] <- as.character(as.Date(project_data[[col]]))
          } else if (is.character(project_data[[col]])) {
            project_data[[col]] <- sapply(project_data[[col]], function(x) {
              if (is.na(x) || x == "" || x == "NA") return(NA_character_)
              tryCatch({
                fecha <- as.Date(x)
                if (!is.na(fecha)) return(as.character(fecha))
                return(NA_character_)
              }, error = function(e) NA_character_)
            })
          }
        }
      }

      if ("Progreso" %in% colnames(project_data)) {
        project_data$Progreso <- as.numeric(project_data$Progreso)
      }

      project_data <- as.data.frame(project_data, stringsAsFactors = FALSE)
      return(project_data)

    } else if (response$status_code == 409) {
      warning("Archivo de datos no encontrado en Dropbox, creando estructura inicial")
      return(create_initial_data_structure())
    } else {
      warning("Error al descargar desde Dropbox: ", response$status_code)
      return(create_initial_data_structure())
    }
  }, error = function(e) {
    warning("Error al cargar datos desde Dropbox: ", e$message)
    return(create_initial_data_structure())
  })
}

create_initial_data_structure <- function() {
  project_data <- data.frame(
    Nombre = character(),
    Fecha_Inicio = character(),
    Fecha_Envio = character(),
    Fecha_Respuesta = character(),
    Revista = character(),
    Cuartil = character(),
    Estado = character(),
    Grupo = character(),
    Progreso = numeric(),
    Fecha_Aceptado = character(),
    Fecha_Publicado = character(),
    Linea_Investigacion = character(),
    Observaciones = character(),
    Envio_Correo = character(),      # <— NUEVA
    stringsAsFactors = FALSE
  )
  return(project_data)
}

# Configurar límite de tamaño de archivo
options(shiny.maxRequestSize = 100*1024^2)

# Definir carpeta local para backup
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
data_file <- file.path(data_dir, "project_data.xlsx")

load_project_data <- function() {
  if (!check_dropbox_config()) {
    warning("Dropbox OAuth no configurado, usando estructura inicial")
    return(create_initial_data_structure())
  }

  dropbox_data <- load_data_from_dropbox()

  if (nrow(dropbox_data) > 0) {
    return(dropbox_data)
  }

  if (file.exists(data_file)) {
    local_data <- read_excel(data_file)

    if (nrow(local_data) > 0) {
      upload_result <- save_data_to_dropbox(local_data)
      if (upload_result$success) {
        message("Datos locales migrados a Dropbox exitosamente")
      }
      return(local_data)
    }
  }

  return(create_initial_data_structure())
}

save_project_data <- function(project_data) {
  if (!check_dropbox_config()) {
    writexl::write_xlsx(project_data, data_file)
    warning("Dropbox OAuth no configurado, guardando solo localmente")
    return(list(success = TRUE, message = "Guardado localmente"))
  }

  dropbox_result <- save_data_to_dropbox(project_data)

  tryCatch({
    writexl::write_xlsx(project_data, data_file)
  }, error = function(e) {
    warning("Error al guardar localmente: ", e$message)
  })

  return(dropbox_result)
}

# ============================================================================
# FUNCIONES DE ANÁLISIS Y VALIDACIÓN
# ============================================================================

validate_dates <- function(data) {
  safe_date_convert <- function(x) {
    if (is.na(x) || x == "" || x == "NA") return(as.Date(NA))
    tryCatch({
      if (inherits(x, c("POSIXct", "POSIXt", "Date"))) {
        return(as.Date(x))
      } else {
        return(as.Date(x))
      }
    }, error = function(e) as.Date(NA))
  }

  data$Fecha_Inicio_Date <- sapply(data$Fecha_Inicio, safe_date_convert)
  data$Fecha_Envio_Date <- sapply(data$Fecha_Envio, safe_date_convert)
  data$Fecha_Respuesta_Date <- sapply(data$Fecha_Respuesta, safe_date_convert)
  data$Fecha_Aceptado_Date <- sapply(data$Fecha_Aceptado, safe_date_convert)
  data$Fecha_Publicado_Date <- sapply(data$Fecha_Publicado, safe_date_convert)

  data$Fecha_Inicio_Date <- as.Date(data$Fecha_Inicio_Date, origin = "1970-01-01")
  data$Fecha_Envio_Date <- as.Date(data$Fecha_Envio_Date, origin = "1970-01-01")
  data$Fecha_Respuesta_Date <- as.Date(data$Fecha_Respuesta_Date, origin = "1970-01-01")
  data$Fecha_Aceptado_Date <- as.Date(data$Fecha_Aceptado_Date, origin = "1970-01-01")
  data$Fecha_Publicado_Date <- as.Date(data$Fecha_Publicado_Date, origin = "1970-01-01")

  data$Alertas <- ""

  for (i in 1:nrow(data)) {
    alertas <- c()

    if (!is.na(data$Fecha_Inicio_Date[i]) && !is.na(data$Fecha_Envio_Date[i])) {
      if (data$Fecha_Envio_Date[i] < data$Fecha_Inicio_Date[i]) {
        alertas <- c(alertas, "⚠️ Envío anterior al inicio")
      }
    }

    if (!is.na(data$Fecha_Envio_Date[i]) && !is.na(data$Fecha_Respuesta_Date[i])) {
      if (data$Fecha_Respuesta_Date[i] < data$Fecha_Envio_Date[i]) {
        alertas <- c(alertas, "⚠️ Respuesta anterior al envío")
      }
    }

    if (!is.na(data$Fecha_Respuesta_Date[i]) && !is.na(data$Fecha_Aceptado_Date[i])) {
      if (data$Fecha_Aceptado_Date[i] < data$Fecha_Respuesta_Date[i]) {
        alertas <- c(alertas, "⚠️ Aceptación anterior a respuesta")
      }
    }

    if (!is.na(data$Fecha_Aceptado_Date[i]) && !is.na(data$Fecha_Publicado_Date[i])) {
      if (data$Fecha_Publicado_Date[i] < data$Fecha_Aceptado_Date[i]) {
        alertas <- c(alertas, "⚠️ Publicación anterior a aceptación")
      }
    }

    today <- Sys.Date()
    if (!is.na(data$Fecha_Envio_Date[i]) && data$Fecha_Envio_Date[i] > today) {
      alertas <- c(alertas, "📅 Fecha de envío en el futuro")
    }

    data$Alertas[i] <- paste(alertas, collapse = "; ")
  }

  return(data)
}

calculate_days_improved <- function(data) {
  data <- validate_dates(data)

  data$Dias_Envio_Inicio <- ifelse(
    !is.na(data$Fecha_Inicio_Date) & !is.na(data$Fecha_Envio_Date) &
      data$Fecha_Envio_Date >= data$Fecha_Inicio_Date,
    as.numeric(difftime(data$Fecha_Envio_Date, data$Fecha_Inicio_Date, units = "days")),
    NA
  )

  data$Dias_Respuesta_Envio <- ifelse(
    !is.na(data$Fecha_Envio_Date) & !is.na(data$Fecha_Respuesta_Date) &
      data$Fecha_Respuesta_Date >= data$Fecha_Envio_Date,
    as.numeric(difftime(data$Fecha_Respuesta_Date, data$Fecha_Envio_Date, units = "days")),
    NA
  )

  data$Dias_Aceptado_Respuesta <- ifelse(
    !is.na(data$Fecha_Respuesta_Date) & !is.na(data$Fecha_Aceptado_Date) &
      data$Fecha_Aceptado_Date >= data$Fecha_Respuesta_Date,
    as.numeric(difftime(data$Fecha_Aceptado_Date, data$Fecha_Respuesta_Date, units = "days")),
    NA
  )

  data$Dias_Aceptado_Envio <- ifelse(
    data$Estado %in% c("Aceptado","Publicado") &
      !is.na(data$Fecha_Aceptado_Date) & !is.na(data$Fecha_Envio_Date) &
      data$Fecha_Aceptado_Date >= data$Fecha_Envio_Date,
    as.numeric(difftime(data$Fecha_Aceptado_Date, data$Fecha_Envio_Date, units = "days")),
    NA
  )

  data$Dias_Aceptado_Publicado <- ifelse(
    data$Estado == "Publicado" &
      !is.na(data$Fecha_Publicado_Date) & !is.na(data$Fecha_Aceptado_Date) &
      data$Fecha_Publicado_Date >= data$Fecha_Aceptado_Date,
    as.numeric(difftime(data$Fecha_Publicado_Date, data$Fecha_Aceptado_Date, units = "days")),
    NA
  )

  cols_to_remove <- c("Fecha_Inicio_Date", "Fecha_Envio_Date", "Fecha_Respuesta_Date",
                      "Fecha_Aceptado_Date", "Fecha_Publicado_Date")
  data <- data[, !colnames(data) %in% cols_to_remove]

  return(data)
}

# ============================================================================
# INTERFAZ DE USUARIO (UI)
# ============================================================================

ui <- dashboardPage(
  dashboardHeader(
    title = div(id = "app-title",
                "SciControl",
                style = "cursor: pointer; user-select: none;",
                title = "Doble clic para acceso de desarrollador"),
    tags$li(class = "dropdown", style = "padding: 8px;",
            div(id = "storage_status_indicator",
                textOutput("storage_status_display"),
                style = "color: white; font-weight: bold; font-size: 12px;"
            )
    )
  ),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      id = "sidebar_menu",
      # Agregar Proyecto debe ser el PRIMER elemento para que se abra por defecto
      menuItem("Agregar Proyecto", tabName = "agregar", icon = icon("plus")),

      # Resto de pestañas normales
      menuItem("Ver Proyectos", tabName = "ver", icon = icon("table")),
      menuItem("Análisis de Tiempos", tabName = "dias", icon = icon("clock")),
      menuItem("Dashboard Visual", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Seguimiento", tabName = "seguimiento", icon = icon("envelope")),
      menuItem("Subida de Evidencias", tabName = "evidencias", icon = icon("upload")),
      menuItem("Ver Archivos Subidos", tabName = "ver_evidencias", icon = icon("folder-open")),
      menuItem("Sincronización", tabName = "sync", icon = icon("sync")),
      menuItem("Descargar Datos", tabName = "descargar", icon = icon("download")),
      menuItem("Importar Datos", tabName = "importar", icon = icon("file-upload")),

      # Pestaña de configuración - Solo visible en modo desarrollador
      conditionalPanel(
        condition = "output.developer_mode == true",
        menuItem("⚙️ Configuración", tabName = "config", icon = icon("cog"))
      ),

      # Indicador de modo desarrollador y botón de salida
      conditionalPanel(
        condition = "output.developer_mode == true",
        div(style = "padding: 10px; margin: 5px; background-color: #d4edda; border-radius: 5px; border-left: 4px solid #28a745;",
            div(style = "color: #155724; font-weight: bold; font-size: 12px; text-align: center;",
                "🔓 MODO DESARROLLADOR",
                br(),
                actionButton("exit_developer_mode", "Salir",
                             class = "btn-sm btn-outline-secondary",
                             style = "margin-top: 5px; font-size: 10px;")
            )
        )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      /* Fix: el texto del <select> se ponía blanco al seleccionar la fila del DT */
      table.dataTable tbody tr.selected select,
      table.dataTable tbody tr.selected .form-control,
      table.dataTable tbody tr.selected option {
        color: #212529 !important;          /* texto oscuro, visible */
        background-color: #ffffff !important;
      }
    .skin-blue .main-header .logo { background-color: #003366; color: white; }
    .skin-blue .main-sidebar { background-color: #004c99; }
    .box { background-color: #f4f4f9; }
    .btn-primary { background-color: #336699; border-color: #336699; color: white; }
    .status-connected { color: #28a745; font-weight: bold; }
    .status-disconnected { color: #dc3545; font-weight: bold; }
    .alert-box { padding: 10px; margin: 5px; border-radius: 4px; }
    .alert-warning { background: #fff3cd; border: 1px solid #ffeaa7; color: #856404; }
    .alert-success { background: #d4edda; border: 1px solid #c3e6cb; color: #155724; }
    .metric-box { text-align: center; padding: 15px; background: #f8f9fa; border-radius: 8px; margin: 5px; }
    .metric-number { font-size: 2em; font-weight: bold; color: #336699; }
    .metric-label { font-size: 0.9em; color: #666; }
    .dataTables_wrapper .dataTables_scroll { overflow-x: auto; }
    .progress-bar-container { background-color: #f3f3f3; border-radius: 5px; overflow: hidden; }
    .progress-bar { background-color: #e74c3c; color: white; text-align: center; padding: 5px 0; transition: width 0.3s ease; }

    /* Estilos para el título clickeable */
    #app-title {
      transition: opacity 0.2s;
    }
    #app-title:hover {
      opacity: 0.8;
    }

    /* Estilos para el indicador de modo desarrollador */
    .developer-indicator {
      background-color: #28a745 !important;
      border-radius: 3px;
      padding: 2px 5px;
      animation: pulse-green 2s infinite;
    }

    @keyframes pulse-green {
      0% { background-color: #28a745; }
      50% { background-color: #34ce57; }
      100% { background-color: #28a745; }
    }")),

      # JavaScript para manejar doble clic en el título
      tags$script(HTML("
        $(document).ready(function() {
          $('#app-title').dblclick(function() {
            $('#developer-auth-modal').modal('show');
          });
        });

      "))
    ),
    tabItems(
      # Tab: Agregar Proyecto (PRIMERA pestaña que se abre por defecto)
      tabItem(tabName = "agregar",
              fluidRow(
                box(title = "Agregar o Actualizar Proyecto", width = 12, status = "primary",
                    div(id = "date_validation_alerts"),
                    textInput("project_name", "Nombre del Proyecto"),
                    dateInput("start_date", "Fecha de Inicio", format = "yyyy-mm-dd", value = NULL),
                    conditionalPanel(
                      condition = "input.status == 'Enviado' || input.status == 'Revisión' ||
                           input.status == 'Aceptado' || input.status == 'Publicado'",
                      dateInput("send_date", "Fecha de Envío", format = "yyyy-mm-dd", value = NULL)
                    ),
                    conditionalPanel(
                      condition = "input.status == 'Enviado' || input.status == 'Revisión' ||
                           input.status == 'Aceptado' || input.status == 'Publicado'",
                      textInput("journal", "Revista"),
                      selectInput("quartile", "Cuartil de la Revista", choices = c("Q1", "Q2", "Q3", "Q4"))
                    ),
                    selectInput("status", "Estado", choices = c("Introducción", "Método", "Resultados",
                                                                "Discusión", "Enviado", "Revisión",
                                                                "Aceptado", "Publicado")),
                    selectInput("group", "Grupo", choices = c("Equipo de Investigación", "Semillero de Investigación")),
                    conditionalPanel(
                      condition = "input.status == 'Revisión' || input.status == 'Aceptado' || input.status == 'Publicado'",
                      dateInput("response_date", "Fecha de Respuesta", format = "yyyy-mm-dd", value = NULL)
                    ),
                    conditionalPanel(
                      condition = "input.status == 'Aceptado' || input.status == 'Publicado'",
                      dateInput("acceptance_date", "Fecha de Aceptación", value = NULL, format = "yyyy-mm-dd")
                    ),
                    conditionalPanel(
                      condition = "input.status == 'Publicado'",
                      dateInput("publication_date", "Fecha de Publicación", value = NULL, format = "yyyy-mm-dd")
                    ),
                    textInput("research_line", "Línea de Investigación", placeholder = "Ingrese la línea de investigación"),
                    textAreaInput("observations", "Observaciones", placeholder = "Ingrese sus observaciones aquí"),
                    actionButton("save_changes", "Guardar Proyecto", class = "btn-primary"),
                    actionButton("clear_fields", "Limpiar campos", class = "btn-warning")
                )
              )
      ),

      # Tab: Ver Proyectos
      tabItem(tabName = "ver",
              fluidRow(
                box(title = "Proyectos Actuales",
                    width = 12,
                    status = "primary",
                    DTOutput("project_table"))
              ),
              fluidRow(
                box(title = "Eliminar Proyecto",
                    width = 12,
                    status = "danger",
                    selectInput("delete_project", "Seleccione un proyecto para eliminar", choices = NULL),
                    actionButton("delete_button", "Eliminar Proyecto", class = "btn-danger")
                )
              )
      ),

      # Tab: Análisis de Tiempos
      tabItem(tabName = "dias",
              fluidRow(
                box(title = "📊 Resumen de Métricas", width = 12, status = "info",
                    htmlOutput("metrics_summary")
                )
              ),
              fluidRow(
                box(title = "⚠️ Alertas de Validación", width = 12, status = "warning",
                    DTOutput("validation_alerts_table")
                )
              ),
              fluidRow(
                box(title = "📈 Análisis Detallado de Tiempos", width = 12, DTOutput("days_table_improved"))
              )
      ),

      # Tab: Dashboard Visual
      tabItem(tabName = "dashboard",
              fluidRow(
                box(title = "📊 Distribución de Estados", width = 6,
                    plotlyOutput("status_chart", height = "300px")
                ),
                box(title = "⏱️ Tiempo Promedio por Cuartil", width = 6,
                    plotlyOutput("quartile_time_chart", height = "300px")
                )
              ),
              fluidRow(
                box(title = "📅 Línea de Tiempo de Proyectos", width = 12,
                    plotlyOutput("timeline_chart", height = "400px")
                )
              ),
              fluidRow(
                box(title = "🎯 Análisis de Rendimiento", width = 6,
                    plotlyOutput("performance_chart", height = "300px")
                ),
                box(title = "📈 Tendencias Mensuales", width = 6,
                    plotlyOutput("monthly_trends", height = "300px")
                )
              )
      ),
      # === NUEVA PESTAÑA SEGUIMIENTO ===
      tabItem(
        tabName = "seguimiento",
        fluidRow(
          box(
            title = "Seguimiento de Envíos",
            width = 12, status = "warning",
            div(style = "display:flex; gap:8px; align-items:center; margin-bottom:8px;",
                downloadButton("download_seguimiento", "⬇️ Descargar Excel", class = "btn-primary"),
                actionButton("save_seguimiento", "💾 Guardar", class = "btn-success")  # <— NUEVO
            ),
            DTOutput("seguimiento_table")
          )
        )
      ),

      # Tab: Subida de Evidencias
      tabItem(tabName = "evidencias",
              fluidRow(
                box(title = "Subir Evidencias para Proyectos", width = 12, status = "primary",
                    div(id = "upload-status-indicator", textOutput("upload_system_status")),
                    br(),
                    selectInput("project_select", "Seleccione un Proyecto", choices = NULL),
                    selectInput("file_type", "Tipo de Archivo",
                                choices = c("Acta" = "actas",
                                            "Captura de Pantalla" = "capturas",
                                            "Correo Electrónico" = "correos",
                                            "Foto de Coordinaciones" = "fotos",
                                            "Resumen de la Reunión con AI" = "resumenes")),
                    fileInput("file_upload", "Subir Archivo",
                              multiple = FALSE,
                              accept = c(".pdf", ".doc", ".docx", ".jpg", ".jpeg", ".png", ".txt", ".xlsx", ".xls")),
                    actionButton("upload_btn", "Subir a Dropbox", class = "btn-primary"),
                    br(), br(),
                    verbatimTextOutput("upload_status")
                )
              )
      ),

      # Tab: Ver Archivos Subidos
      tabItem(tabName = "ver_evidencias",
              fluidRow(
                box(title = "Ver Archivos en Dropbox", width = 12, status = "primary",
                    selectInput("project_view", "Seleccione un Proyecto para ver archivos", choices = c("")),
                    actionButton("refresh_files", "Actualizar Lista", class = "btn-info"),
                    actionButton("delete_file", "Eliminar Archivo", class = "btn-danger"),
                    br(), br(),
                    DTOutput("files_table")
                )
              )
      ),

      # Tab: Sincronización
      tabItem(tabName = "sync",
              fluidRow(
                box(title = "🔄 Sincronización de Datos", width = 12, status = "info",
                    div(id = "sync_status_display",
                        htmlOutput("sync_status_info")
                    ),
                    br(),
                    fluidRow(
                      column(4,
                             actionButton("manual_backup", "📋 Crear Backup Manual",
                                          class = "btn-info", style = "width: 100%;")
                      ),
                      column(4,
                             actionButton("sync_from_dropbox", "⬇️ Cargar desde Dropbox",
                                          class = "btn-primary", style = "width: 100%;")
                      ),
                      column(4,
                             actionButton("sync_to_dropbox", "⬆️ Subir a Dropbox",
                                          class = "btn-success", style = "width: 100%;")
                      )
                    ),
                    br(),
                    verbatimTextOutput("sync_operation_status")
                )
              ),
              fluidRow(
                box(title = "📂 Archivos de Backup en Dropbox", width = 12,
                    DTOutput("backup_files_table"),
                    br(),
                    actionButton("restore_backup", "🔄 Restaurar Backup Seleccionado",
                                 class = "btn-warning")
                )
              )
      ),

      # Tab: Descargar Datos
      tabItem(tabName = "descargar",
              fluidRow(
                box(title = "Descargar Datos", width = 12, status = "primary",
                    p("Descarga los datos del proyecto en formato Excel."),
                    p(em("Nota: Los archivos están almacenados en Dropbox y no se incluyen en esta descarga.")),
                    downloadButton("download_data", "Descargar Datos Excel", class = "btn-primary")
                )
              )
      ),

      # Tab: Importar Datos
      tabItem(tabName = "importar",
              fluidRow(
                box(title = "Importar Datos desde Excel", width = 12, status = "primary",
                    fileInput("excel_upload", "Subir Archivo Excel", accept = c(".xlsx", ".xls")),
                    actionButton("import_excel_btn", "Importar Excel", class = "btn-primary"),
                    br(), br(),
                    verbatimTextOutput("import_status")
                )
              )
      ),

      # Tab: Configuración Dropbox (Solo visible en modo desarrollador)
      tabItem(tabName = "config",
              # Indicador de modo desarrollador
              fluidRow(
                box(title = "🔓 Modo Desarrollador Activo",
                    width = 12, status = "success", solidHeader = TRUE,
                    div(class = "alert alert-success",
                        HTML("✅ <strong>Acceso de desarrollador activado.</strong> Tienes acceso completo a las configuraciones OAuth de Dropbox.")),
                    div(style = "text-align: center; margin: 10px 0;",
                        actionButton("exit_developer_mode_main", "🔒 Salir del Modo Desarrollador",
                                     class = "btn-warning", style = "margin-right: 10px;"),
                        span("(Esto ocultará esta pestaña)", style = "font-size: 12px; color: #666;")
                    )
                )
              ),

              fluidRow(
                box(title = "🔧 Configuración de Dropbox OAuth 2.0",
                    width = 12, status = "primary",

                    div(id = "oauth_status_display",
                        htmlOutput("oauth_status_info")
                    ),

                    br(),

                    conditionalPanel(
                      condition = "output.show_oauth_buttons",
                      h4("🔐 Autorización OAuth"),

                      div(class = "alert alert-info",
                          HTML("📋 <strong>Importante:</strong> Asegúrate de que tu app de Dropbox tenga configurados estos Redirect URIs:<br>
                               • <code>http://localhost:1410/</code><br>
                               • <code>http://localhost:1411/</code><br>
                               • <code>http://localhost:1412/</code><br>
                               • <code>http://localhost:1413/</code><br>
                               • <code>http://localhost:1414/</code><br>
                               <br>Ve a <a href='https://www.dropbox.com/developers/apps' target='_blank'>tu app en Dropbox</a> → Settings → OAuth2 redirect URIs")
                      ),

                      p("Autoriza el acceso a tu cuenta de Dropbox:"),

                      fluidRow(
                        column(3,
                               actionButton("start_oauth", "🚀 Iniciar Autorización OAuth",
                                            class = "btn-success", style = "width: 100%;")
                        ),
                        column(3,
                               actionButton("test_auth_url", "🔗 Probar URL de Autorización",
                                            class = "btn-warning", style = "width: 100%;")
                        ),
                        column(3,
                               actionButton("manual_auth", "✍️ Autorización Manual",
                                            class = "btn-secondary", style = "width: 100%;")
                        ),
                        column(3,
                               actionButton("refresh_token", "🔄 Refrescar Token",
                                            class = "btn-info", style = "width: 100%;")
                        )
                      ),
                      br()
                    ),

                    verbatimTextOutput("oauth_operation_status")
                )
              ),

              fluidRow(
                box(title = "📊 Estado de Tokens", width = 12,
                    DTOutput("token_status_table")
                )
              )
      )
    ),

    # Usar shinyjs
    useShinyjs(),

    # Modal de autenticación de desarrollador (oculto inicialmente)
    div(id = "developer-auth-modal", class = "modal fade", role = "dialog",
        div(class = "modal-dialog",
            div(class = "modal-content",
                div(class = "modal-header",
                    tags$button(type = "button", class = "close", `data-dismiss` = "modal", "×"),
                    h4(class = "modal-title", "🔐 Acceso de Desarrollador")
                ),
                div(class = "modal-body",
                    p("Ingresa la contraseña de desarrollador para acceder a las configuraciones avanzadas:"),
                    passwordInput("developer_password_input", "Contraseña:", placeholder = "Contraseña de desarrollador"),
                    br(),
                    div(id = "developer_auth_message", style = "color: red; font-weight: bold;")
                ),
                div(class = "modal-footer",
                    tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Cancelar"),
                    actionButton("authenticate_developer", "Autenticar", class = "btn btn-primary")
                )
            )
        )
    )
  )
)

# ============================================================================
# SERVIDOR (SERVER)
# ============================================================================

server <- function(input, output, session) {

  # 🚀 Auto-refrescar token cada hora
  auto_refresh_timer <- reactiveTimer(60 * 60 * 1000)  # 1 hora en ms
  observe({
    auto_refresh_timer()
    cat("⏳ Intentando refrescar token automáticamente...\n")
    if (refresh_dropbox_access_token()) {
      cat("✅ Token refrescado automáticamente a las", format(Sys.time()), "\n")
    } else {
      cat("⚠️ No se pudo refrescar token automáticamente\n")
    }
  })


  # Usar shinyjs para funciones JavaScript
  useShinyjs()

  # Variables reactivas
  files_refresh <- reactiveVal(0)
  project_data <- reactiveVal(create_initial_data_structure())
  oauth_configured <- reactiveVal(TRUE)
  tokens_valid <- reactiveVal(FALSE)
  developer_mode <- reactiveVal(FALSE)

  progress_map <- list(
    "Introducción" = 10, "Método" = 30, "Resultados" = 50,
    "Discusión" = 70, "Enviado" = 80, "Revisión" = 90,
    "Aceptado" = 95, "Publicado" = 100
  )

  # Al iniciar la aplicación, verificar configuración OAuth
  observe({
    tokens_ok <- initialize_dropbox_oauth()
    tokens_valid(tokens_ok)

    if (tokens_ok) {
      initial_data <- load_project_data()
      project_data(initial_data)
    }
  })

  # Datos mejorados con validación
  days_data_improved <- reactive({
    data <- project_data()
    if (nrow(data) == 0) return(data)
    calculate_days_improved(data)
  })

  # ========================================================================
  # SISTEMA DE AUTENTICACIÓN DE DESARROLLADOR
  # ========================================================================

  # Output para controlar visibilidad del modo desarrollador
  output$developer_mode <- reactive({
    developer_mode()
  })
  outputOptions(output, "developer_mode", suspendWhenHidden = FALSE)

  # Autenticación de desarrollador
  observeEvent(input$authenticate_developer, {
    req(input$developer_password_input)

    if (input$developer_password_input == DEVELOPER_PASSWORD) {
      developer_mode(TRUE)
      developer_authenticated <<- TRUE

      # Limpiar contraseña y cerrar modal
      updateTextInput(session, "developer_password_input", value = "")
      runjs("$('#developer-auth-modal').modal('hide');")

      showNotification("🔓 Modo desarrollador activado", type = "message", duration = 3)

      # Limpiar mensaje de error
      runjs("document.getElementById('developer_auth_message').innerHTML = '';")

    } else {
      # Mostrar error
      runjs("document.getElementById('developer_auth_message').innerHTML = '❌ Contraseña incorrecta';")
      showNotification("❌ Contraseña incorrecta", type = "error", duration = 3)
    }
  })

  # Salir del modo desarrollador (botón del sidebar)
  observeEvent(input$exit_developer_mode, {
    developer_mode(FALSE)
    developer_authenticated <<- FALSE
    showNotification("🔒 Modo desarrollador desactivado", type = "message", duration = 3)
  })

  # Salir del modo desarrollador (botón principal de la pestaña)
  observeEvent(input$exit_developer_mode_main, {
    showModal(modalDialog(
      title = "🔒 Confirmar Salida del Modo Desarrollador",
      HTML("¿Está seguro de que desea salir del modo desarrollador?<br><br>
           <strong>Esto ocultará la pestaña de Configuración</strong> y necesitará volver a autenticarse
           para acceder a las configuraciones OAuth."),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirm_exit_developer", "Sí, Salir", class = "btn-warning")
      )
    ))
  })

  # Confirmar salida del modo desarrollador
  observeEvent(input$confirm_exit_developer, {
    removeModal()
    developer_mode(FALSE)
    developer_authenticated <<- FALSE
    showNotification("🔒 Modo desarrollador desactivado. Pestaña de Configuración oculta.", type = "message", duration = 4)
  })

  # Reset configuración OAuth (solo desarrolladores)
  observeEvent(input$reset_oauth_config, {
    if (!developer_mode()) return()

    showModal(modalDialog(
      title = "⚠️ Confirmar Reset OAuth",
      "¿Estás seguro de que quieres eliminar toda la configuración OAuth? Esto requerirá reautorizar la aplicación.",
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirm_reset_oauth", "Sí, Reset", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_reset_oauth, {
    removeModal()

    # Limpiar tokens
    DROPBOX_ACCESS_TOKEN <<- ""
    DROPBOX_REFRESH_TOKEN <<- ""
    TOKEN_EXPIRY_TIME <<- NULL
    tokens_valid(FALSE)

    # Eliminar archivos de tokens
    if (file.exists("dropbox_tokens.rds")) file.remove("dropbox_tokens.rds")

    output$oauth_operation_status <- renderText("✅ Configuración OAuth reseteada. Reautoriza la aplicación.")
    showNotification("OAuth configuration reset", type = "warning")
  })

  # ========================================================================
  # OUTPUTS DE CONFIGURACIÓN OAUTH
  # ========================================================================

  output$oauth_status_info <- renderUI({
    if (!tokens_valid()) {
      div(
        h4("⚠️ Autorización requerida"),
        div(class = "alert alert-info",
            "ℹ️ La aplicación está configurada pero necesitas autorizar el acceso a Dropbox."
        )
      )
    } else {
      div(
        h4("✅ Dropbox configurado y autorizado"),
        div(class = "alert alert-success",
            "🎉 Todo está listo. La sincronización automática está activa."
        )
      )
    }
  })

  output$show_oauth_buttons <- reactive({
    TRUE  # Siempre mostrar botones OAuth
  })
  outputOptions(output, "show_oauth_buttons", suspendWhenHidden = FALSE)

  # Autorización manual
  observeEvent(input$manual_auth, {
    # Verificar modo desarrollador
    if (!developer_mode()) {
      showNotification("❌ Acceso denegado", type = "error")
      return()
    }

    auth_url <- generate_auth_url()

    showModal(modalDialog(
      title = "✍️ Autorización Manual de Dropbox",
      HTML(paste(
        "<h4>📋 Instrucciones paso a paso:</h4>",
        "<ol>",
        "<li><strong>Copia esta URL</strong> y pégala en tu navegador:</li>",
        "<div style='background: #f8f9fa; padding: 10px; border-radius: 5px; margin: 10px 0; word-break: break-all; font-family: monospace; font-size: 12px;'>",
        auth_url,
        "</div>",
        "<li><strong>Autoriza la aplicación</strong> haciendo clic en 'Permitir'</li>",
        "<li><strong>Copia el código</strong> de la URL que aparece después. Busca algo como:</li>",
        "<div style='background: #fff3cd; padding: 10px; border-radius: 5px; margin: 10px 0; font-family: monospace; font-size: 12px;'>",
        "http://localhost:1410/?code=<strong>CODIGO_AQUI</strong>&state=...",
        "</div>",
        "<li><strong>Pega solo el código</strong> en el campo de abajo (la parte después de 'code=' y antes de '&')</li>",
        "</ol>"
      )),
      textInput("manual_auth_code",
                "Código de Autorización:",
                placeholder = "Pega aquí el código que obtuviste de la URL"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("submit_manual_code", "Procesar Código", class = "btn-primary")
      ),
      easyClose = FALSE,
      size = "l"
    ))

    # También abrir automáticamente la URL
    browseURL(auth_url)
  })

  # Procesar código manual
  observeEvent(input$submit_manual_code, {
    req(input$manual_auth_code)

    if (nchar(trimws(input$manual_auth_code)) < 10) {
      showNotification("El código parece demasiado corto. Verifica que hayas copiado el código completo.", type = "error")
      return()
    }

    removeModal()

    output$oauth_operation_status <- renderText("🔄 Procesando código de autorización manual...")

    tryCatch({
      auth_code <- trimws(input$manual_auth_code)

      # Limpiar el código si viene con URL completa
      if (grepl("code=", auth_code)) {
        auth_code <- gsub(".*code=([^&]+).*", "\\1", auth_code)
      }

      cat("📝 Código manual recibido:", substr(auth_code, 1, 20), "...\n")

      if (exchange_code_for_tokens(auth_code)) {
        tokens_valid(TRUE)
        output$oauth_operation_status <- renderText("✅ Autorización manual completada exitosamente.")
        showNotification("Dropbox autorizado correctamente", type = "message")

        # Cargar datos después de la autorización
        initial_data <- load_project_data()
        project_data(initial_data)
      } else {
        output$oauth_operation_status <- renderText("❌ Error al procesar el código de autorización.")
        showNotification("Error al procesar código", type = "error")
      }
    }, error = function(e) {
      output$oauth_operation_status <- renderText(paste("❌ Error al procesar código:", e$message))
      showNotification("Error en autorización manual", type = "error")
      cat("❌ Error en autorización manual:", e$message, "\n")
    })
  })

  # Probar URL de autorización
  observeEvent(input$test_auth_url, {
    # Verificar modo desarrollador
    if (!developer_mode()) {
      showNotification("❌ Acceso denegado", type = "error")
      return()
    }

    auth_url <- generate_auth_url()

    output$oauth_operation_status <- renderText(paste(
      "🔗 URL de autorización generada:\n\n",
      auth_url,
      "\n\n✅ Puedes copiar esta URL y pegarla manualmente en tu navegador.",
      "\n\n📋 Si funciona, verás la página de autorización de Dropbox.",
      "\nSi no funciona, revisa la configuración de tu app en Dropbox."
    ))

    # También abrir automáticamente
    browseURL(auth_url)

    showNotification("URL de autorización generada y abierta", type = "message")
  })

  # Iniciar flujo OAuth
  observeEvent(input$start_oauth, {
    # Verificar modo desarrollador
    if (!developer_mode()) {
      showNotification("❌ Acceso denegado", type = "error")
      return()
    }

    output$oauth_operation_status <- renderText("🔄 Verificando configuración...")

    # Verificar configuración antes de iniciar
    if (DROPBOX_APP_KEY == "") {
      output$oauth_operation_status <- renderText("❌ Error: App Key no configurado")
      return()
    }

    if (DROPBOX_APP_SECRET == "") {
      output$oauth_operation_status <- renderText("❌ Error: App Secret no configurado")
      return()
    }

    output$oauth_operation_status <- renderText("🔄 Iniciando autorización OAuth... Se abrirá tu navegador.")

    # Mostrar información de debugging en la consola
    cat("\n=== DEBUG INFO ===\n")
    cat("App Key:", DROPBOX_APP_KEY, "\n")
    cat("App Secret:", substr(DROPBOX_APP_SECRET, 1, 10), "...\n")
    cat("Redirect URI:", DROPBOX_REDIRECT_URI, "\n")

    # Generar y mostrar URL de autorización
    auth_url <- generate_auth_url()
    cat("URL generada:", auth_url, "\n")
    cat("==================\n\n")

    tryCatch({
      result <- start_oauth_flow()
      if (result) {
        tokens_valid(TRUE)
        output$oauth_operation_status <- renderText("✅ Autorización OAuth completada exitosamente.")
        showNotification("Dropbox autorizado correctamente", type = "message")

        # Cargar datos después de la autorización
        initial_data <- load_project_data()
        project_data(initial_data)
      } else {
        output$oauth_operation_status <- renderText("❌ Error en la autorización automática. 💡 Prueba con 'Autorización Manual' para ingresar el código manualmente.")
        showNotification("Prueba la autorización manual", type = "warning")
      }
    }, error = function(e) {
      if (grepl("Manual intervention required", e$message)) {
        output$oauth_operation_status <- renderText("⚠️ La autorización automática no funcionó. 💡 Usa el botón 'Autorización Manual' para completar el proceso copiando/pegando el código.")
        showNotification("Usa autorización manual", type = "warning")
      } else {
        output$oauth_operation_status <- renderText(paste("❌ Error:", e$message, "\n💡 Intenta con 'Autorización Manual'."))
        showNotification("Error en autorización - prueba manual", type = "error")
      }
      cat("❌ Error capturado:", e$message, "\n")
    })
  })

  # Refrescar token manualmente
  observeEvent(input$refresh_token, {
    # Verificar modo desarrollador
    if (!developer_mode()) {
      showNotification("❌ Acceso denegado", type = "error")
      return()
    }

    output$oauth_operation_status <- renderText("🔄 Refrescando token...")

    if (refresh_dropbox_access_token()) {
      tokens_valid(TRUE)
      try({
        data_new <- load_project_data()
        project_data(data_new)
        showNotification("✅ Token refrescado y datos recargados", type = "message")
        output$oauth_operation_status <- renderText("✅ Token refrescado y datos sincronizados con Dropbox.")
      }, silent = TRUE)
    } else {
      output$oauth_operation_status <- renderText("❌ Error al refrescar token.")
      showNotification("Error al refrescar token", type = "error")
    }
  })

  # Tabla de estado de tokens
  output$token_status_table <- renderDT({
    if (!tokens_valid()) {
      status_data <- data.frame(
        Estado = "No autorizado",
        Token = "No disponible",
        Expira = "N/A",
        stringsAsFactors = FALSE
      )
    } else {
      access_token_preview <- if (DROPBOX_ACCESS_TOKEN != "") {
        paste0(substr(DROPBOX_ACCESS_TOKEN, 1, 20), "...")
      } else {
        "No disponible"
      }

      refresh_token_preview <- if (DROPBOX_REFRESH_TOKEN != "") {
        paste0(substr(DROPBOX_REFRESH_TOKEN, 1, 20), "...")
      } else {
        "No disponible"
      }

      expiry_text <- if (!is.null(TOKEN_EXPIRY_TIME)) {
        format(TOKEN_EXPIRY_TIME, "%Y-%m-%d %H:%M:%S")
      } else {
        "Desconocido"
      }

      status_data <- data.frame(
        Tipo = c("Access Token", "Refresh Token"),
        Estado = c("✅ Activo", "✅ Disponible"),
        Token = c(access_token_preview, refresh_token_preview),
        Expira = c(expiry_text, "No expira"),
        stringsAsFactors = FALSE
      )
    }

    datatable(status_data,
              options = list(pageLength = 5, dom = 't'),
              rownames = FALSE,
              escape = FALSE)
  })

  # Indicador de estado en el header
  output$storage_status_display <- renderText({
    status_text <- if (tokens_valid()) {
      "🟢 Dropbox OAuth Activo"
    } else {
      "🟡 Dropbox OAuth Inactivo"
    }

    # Agregar indicador de modo desarrollador
    if (developer_mode()) {
      paste(status_text, "| 🔓 DEV")
    } else {
      status_text
    }
  })

  # ========================================================================
  # OUTPUTS DE ANÁLISIS Y MÉTRICAS
  # ========================================================================

  output$metrics_summary <- renderUI({
    data <- days_data_improved()
    if (nrow(data) == 0) return(div("No hay datos disponibles"))

    total_projects <- nrow(data)
    published <- sum(data$Estado == "Publicado", na.rm = TRUE)
    accepted <- sum(data$Estado %in% c("Aceptado", "Publicado"), na.rm = TRUE)
    avg_review_time <- round(mean(data$Dias_Respuesta_Envio, na.rm = TRUE), 1)
    avg_acceptance_time <- round(mean(data$Dias_Aceptado_Envio, na.rm = TRUE), 1)

    data_with_alerts <- data[data$Alertas != "", ]
    total_alerts <- nrow(data_with_alerts)

    div(
      fluidRow(
        column(2, div(class = "metric-box",
                      div(class = "metric-number", total_projects),
                      div(class = "metric-label", "Total Proyectos"))),
        column(2, div(class = "metric-box",
                      div(class = "metric-number", published),
                      div(class = "metric-label", "Publicados"))),
        column(2, div(class = "metric-box",
                      div(class = "metric-number", accepted),
                      div(class = "metric-label", "Aceptados"))),
        column(2, div(class = "metric-box",
                      div(class = "metric-number", ifelse(is.na(avg_review_time), "-", paste0(avg_review_time, " días"))),
                      div(class = "metric-label", "Tiempo Promedio Revisión"))),
        column(2, div(class = "metric-box",
                      div(class = "metric-number", ifelse(is.na(avg_acceptance_time), "-", paste0(avg_acceptance_time, " días"))),
                      div(class = "metric-label", "Tiempo Promedio Aceptación"))),
        column(2, div(class = "metric-box",
                      div(class = "metric-number", total_alerts),
                      div(class = "metric-label", "Alertas")))
      )
    )
  })

  output$validation_alerts_table <- renderDT({
    data <- days_data_improved()
    if (nrow(data) == 0) return(datatable(data.frame(), options = list(pageLength = 5)))

    alerts_data <- data[data$Alertas != "", c("Nombre", "Estado", "Alertas")]

    if (nrow(alerts_data) == 0) {
      no_alerts <- data.frame(
        Mensaje = "✅ No se encontraron problemas de validación en las fechas",
        stringsAsFactors = FALSE
      )
      return(datatable(no_alerts, options = list(pageLength = 5, dom = 't'), rownames = FALSE))
    }

    datatable(alerts_data,
              options = list(pageLength = 5, dom = 'tp'),
              rownames = FALSE,
              escape = FALSE) %>%
      formatStyle("Alertas", backgroundColor = "#fff3cd", color = "#856404")
  })

  output$days_table_improved <- renderDT({
    data <- days_data_improved()
    if (nrow(data) == 0) return(datatable(data.frame(), options = list(pageLength = 10)))

    display_data <- data[, c("Nombre", "Revista", "Cuartil", "Estado",
                             "Dias_Envio_Inicio", "Dias_Respuesta_Envio",
                             "Dias_Aceptado_Respuesta", "Dias_Aceptado_Envio",
                             "Dias_Aceptado_Publicado")]

    colnames(display_data) <- c("Proyecto", "Revista", "Cuartil", "Estado",
                                "Días Inicio→Envío", "Días Envío→Respuesta",
                                "Días Respuesta→Aceptado", "Días Envío→Aceptado",
                                "Días Aceptado→Publicado")

    datatable(display_data,
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle(columns = 5:9,
                  backgroundColor = styleInterval(c(0, 30, 90),
                                                  c("#ffebee", "#fff3e0", "#e8f5e8", "#c8e6c9")))
  })

  # ========================================================================
  # OUTPUTS DE GRÁFICOS
  # ========================================================================

  plotly_empty <- function() {
    plot_ly() %>%
      add_annotations(
        text = "No hay datos suficientes para mostrar",
        xref = "paper", yref = "paper",
        x = 0.5, y = 0.5, showarrow = FALSE,
        font = list(size = 16, color = "gray")
      ) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  }

  output$status_chart <- renderPlotly({
    data <- project_data()
    if (nrow(data) == 0) return(plotly_empty())

    status_counts <- data %>%
      count(Estado) %>%
      mutate(Porcentaje = round(n/sum(n)*100, 1))

    p <- ggplot(status_counts, aes(x = reorder(Estado, n), y = n, fill = Estado)) +
      geom_col() +
      coord_flip() +
      labs(title = "Distribución de Estados de Proyectos",
           x = "Estado", y = "Cantidad") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_viridis_d()

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$quartile_time_chart <- renderPlotly({
    data <- days_data_improved()
    if (nrow(data) == 0) return(plotly_empty())

    quartile_data <- data %>%
      filter(!is.na(Cuartil), !is.na(Dias_Respuesta_Envio)) %>%
      group_by(Cuartil) %>%
      summarise(
        Promedio = round(mean(Dias_Respuesta_Envio, na.rm = TRUE), 1),
        Mediana = round(median(Dias_Respuesta_Envio, na.rm = TRUE), 1),
        .groups = 'drop'
      )

    if (nrow(quartile_data) == 0) return(plotly_empty())

    p <- ggplot(quartile_data, aes(x = Cuartil, y = Promedio, fill = Cuartil)) +
      geom_col() +
      labs(title = "Tiempo Promedio de Respuesta por Cuartil",
           x = "Cuartil", y = "Días promedio") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_fill_manual(values = c("Q1" = "#1f77b4", "Q2" = "#ff7f0e",
                                   "Q3" = "#2ca02c", "Q4" = "#d62728"))

    ggplotly(p, tooltip = c("x", "y"))
  })

  output$timeline_chart <- renderPlotly({
    data <- days_data_improved()
    if (nrow(data) == 0) return(plotly_empty())

    timeline_data <- data %>%
      filter(!is.na(Fecha_Inicio)) %>%
      mutate(
        Fecha_Inicio = as.Date(Fecha_Inicio),
        Fecha_Fin = case_when(
          !is.na(Fecha_Publicado) ~ as.Date(Fecha_Publicado),
          !is.na(Fecha_Aceptado) ~ as.Date(Fecha_Aceptado),
          !is.na(Fecha_Respuesta) ~ as.Date(Fecha_Respuesta),
          !is.na(Fecha_Envio) ~ as.Date(Fecha_Envio),
          TRUE ~ Fecha_Inicio + 30
        ),
        Duracion = as.numeric(Fecha_Fin - Fecha_Inicio)
      ) %>%
      arrange(Fecha_Inicio)

    if (nrow(timeline_data) == 0) return(plotly_empty())

    p <- ggplot(timeline_data, aes(x = Fecha_Inicio, xend = Fecha_Fin,
                                   y = reorder(Nombre, Fecha_Inicio), yend = reorder(Nombre, Fecha_Inicio),
                                   color = Estado)) +
      geom_segment(size = 3) +
      labs(title = "Línea de Tiempo de Proyectos",
           x = "Fecha", y = "Proyecto") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 8)) +
      scale_color_viridis_d()

    ggplotly(p, tooltip = c("colour", "x", "xend"))
  })

  output$performance_chart <- renderPlotly({
    data <- days_data_improved()
    if (nrow(data) == 0) return(plotly_empty())

    performance_data <- data %>%
      filter(!is.na(Dias_Envio_Inicio), !is.na(Dias_Respuesta_Envio)) %>%
      mutate(
        Eficiencia = case_when(
          Dias_Envio_Inicio <= 365 & Dias_Respuesta_Envio <= 90 ~ "Alta",
          Dias_Envio_Inicio <= 730 & Dias_Respuesta_Envio <= 180 ~ "Media",
          TRUE ~ "Baja"
        )
      )

    if (nrow(performance_data) == 0) return(plotly_empty())

    p <- ggplot(performance_data, aes(x = Dias_Envio_Inicio, y = Dias_Respuesta_Envio,
                                      color = Eficiencia, size = Estado == "Publicado")) +
      geom_point(alpha = 0.7) +
      labs(title = "Análisis de Eficiencia de Proyectos",
           x = "Días desde Inicio hasta Envío",
           y = "Días desde Envío hasta Respuesta") +
      theme_minimal() +
      scale_color_manual(values = c("Alta" = "#2ca02c", "Media" = "#ff7f0e", "Baja" = "#d62728")) +
      scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2), guide = "none")

    ggplotly(p, tooltip = c("x", "y", "colour"))
  })

  output$monthly_trends <- renderPlotly({
    data <- days_data_improved()
    if (nrow(data) == 0) return(plotly_empty())

    monthly_data <- data %>%
      filter(!is.na(Fecha_Envio)) %>%
      mutate(
        Fecha_Envio = as.Date(Fecha_Envio),
        Mes_Envio = format(Fecha_Envio, "%Y-%m")
      ) %>%
      group_by(Mes_Envio) %>%
      summarise(
        Enviados = n(),
        Promedio_Respuesta = round(mean(Dias_Respuesta_Envio, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      arrange(Mes_Envio)

    if (nrow(monthly_data) == 0) return(plotly_empty())

    p <- ggplot(monthly_data, aes(x = Mes_Envio)) +
      geom_col(aes(y = Enviados), fill = "#1f77b4", alpha = 0.7) +
      geom_line(aes(y = Promedio_Respuesta, group = 1), color = "#ff7f0e", size = 2) +
      labs(title = "Tendencias Mensuales de Envíos y Tiempos de Respuesta",
           x = "Mes", y = "Cantidad / Días") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p, tooltip = c("x", "y"))
  })

  # ========================================================================
  # TABLA DE PROYECTOS
  # ========================================================================

  output$project_table <- renderDT({
    data <- tryCatch({
      days_data_improved()
    }, error = function(e) {
      project_data()
    })

    if (nrow(data) == 0) {
      return(datatable(
        data.frame(Mensaje = "No hay proyectos para mostrar"),
        options = list(pageLength = 10),
        rownames = FALSE
      ))
    }

    data$Progreso_Barra <- sapply(data$Estado, function(estado) {
      p <- progress_map[[estado]]
      if (is.null(p)) p <- 0

      color <- if (p >= 90) "#28a745"
      else if (p >= 70) "#ffc107"
      else if (p >= 40) "#fd7e14"
      else "#dc3545"

      sprintf(
        '<div style="width:100%%; background-color:#f3f3f3; border-radius:5px; height:22px; border:1px solid #ddd;">
         <div style="width:%d%%; background-color:%s; color:white; text-align:center; line-height:22px; border-radius:4px; font-size:12px; font-weight:bold;">%d%%</div>
       </div>', p, color, p)
    })

    if ("Alertas" %in% colnames(data)) {
      data$Estado_Display <- ifelse(!is.na(data$Alertas) & data$Alertas != "",
                                    paste0(data$Estado, " ⚠️"),
                                    data$Estado)
    } else {
      data$Estado_Display <- data$Estado
    }

    display_cols <- c("Nombre", "Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                      "Revista", "Cuartil", "Estado_Display", "Grupo", "Progreso_Barra",
                      "Fecha_Aceptado", "Fecha_Publicado", "Linea_Investigacion",
                      "Observaciones")

    available_cols <- intersect(display_cols, colnames(data))
    display_data <- data[, available_cols, drop = FALSE]

    col_names <- colnames(display_data)
    col_names[col_names == "Estado_Display"] <- "Estado"
    col_names[col_names == "Progreso_Barra"] <- "Progreso"
    col_names[col_names == "Linea_Investigacion"] <- "Línea de Investigación"
    colnames(display_data) <- col_names

    dt <- datatable(
      display_data,
      escape = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = 'Bfrtip',
        columnDefs = list(
          list(width = '200px', targets = 0),
          list(width = '100px', targets = 1:3),
          list(width = '120px', targets = 4),
          list(width = '60px', targets = 5),
          list(width = '100px', targets = 6),
          list(width = '120px', targets = 7),
          list(width = '150px', targets = 8),
          list(width = '100px', targets = 9:10),
          list(width = '150px', targets = 11)
        )
      ),
      rownames = FALSE,
      selection = 'single'
    )

    if ("Alertas" %in% colnames(data)) {
      rows_with_alerts <- which(!is.na(data$Alertas) & data$Alertas != "")
      if (length(rows_with_alerts) > 0) {
        dt <- dt %>% formatStyle(
          "Estado",
          target = "row",
          backgroundColor = styleEqual(rows_with_alerts, rep("#fff3cd", length(rows_with_alerts)))
        )
      }
    }

    return(dt)
  })

  # ========================================================================
  # VALIDACIÓN DE FECHAS EN TIEMPO REAL
  # ========================================================================

  observeEvent(c(input$start_date, input$send_date, input$response_date,
                 input$acceptance_date, input$publication_date), {

                   if (is.null(input$start_date) && is.null(input$send_date) &&
                       is.null(input$response_date) && is.null(input$acceptance_date) &&
                       is.null(input$publication_date)) {
                     return()
                   }

                   alerts <- c()

                   if (!is.null(input$start_date) && !is.null(input$send_date) &&
                       length(input$start_date) > 0 && length(input$send_date) > 0 &&
                       !is.na(input$start_date) && !is.na(input$send_date)) {

                     tryCatch({
                       if (as.Date(input$send_date) < as.Date(input$start_date)) {
                         alerts <- c(alerts, "⚠️ La fecha de envío debe ser posterior a la fecha de inicio")
                       }
                     }, error = function(e) {
                     })
                   }

                   if (!is.null(input$send_date) && !is.null(input$response_date) &&
                       length(input$send_date) > 0 && length(input$response_date) > 0 &&
                       !is.na(input$send_date) && !is.na(input$response_date)) {

                     tryCatch({
                       if (as.Date(input$response_date) < as.Date(input$send_date)) {
                         alerts <- c(alerts, "⚠️ La fecha de respuesta debe ser posterior a la fecha de envío")
                       }
                     }, error = function(e) {
                     })
                   }

                   if (!is.null(input$response_date) && !is.null(input$acceptance_date) &&
                       length(input$response_date) > 0 && length(input$acceptance_date) > 0 &&
                       !is.na(input$response_date) && !is.na(input$acceptance_date)) {

                     tryCatch({
                       if (as.Date(input$acceptance_date) < as.Date(input$response_date)) {
                         alerts <- c(alerts, "⚠️ La fecha de aceptación debe ser posterior a la fecha de respuesta")
                       }
                     }, error = function(e) {
                     })
                   }

                   if (!is.null(input$acceptance_date) && !is.null(input$publication_date) &&
                       length(input$acceptance_date) > 0 && length(input$publication_date) > 0 &&
                       !is.na(input$acceptance_date) && !is.na(input$publication_date)) {

                     tryCatch({
                       if (as.Date(input$publication_date) < as.Date(input$acceptance_date)) {
                         alerts <- c(alerts, "⚠️ La fecha de publicación debe ser posterior a la fecha de aceptación")
                       }
                     }, error = function(e) {
                     })
                   }

                   output$date_validation_alerts <- renderUI({
                     if (length(alerts) > 0) {
                       div(class = "alert-box alert-warning",
                           HTML(paste(alerts, collapse = "<br>")))
                     } else {
                       NULL
                     }
                   })
                 }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # ========================================================================
  # SISTEMA DE SUBIDA DE ARCHIVOS
  # ========================================================================

  output$upload_system_status <- renderText({
    if (tokens_valid()) {
      "✅ Sistema listo para subir archivos a Dropbox"
    } else {
      "⚠️ Configure primero OAuth de Dropbox en la pestaña de configuración"
    }
  })

  observe({
    data <- project_data()

    if (is.null(data) || nrow(data) == 0) return()

    if (!"Nombre" %in% colnames(data)) return()

    data$Nombre <- as.character(data$Nombre)
    display_names <- ifelse(is.na(data$Nombre) | data$Nombre == "", "(sin nombre)", data$Nombre)

    updateSelectInput(session, "delete_project", choices = display_names)
    updateSelectInput(session, "project_select", choices = data$Nombre)
    updateSelectInput(session, "project_view", choices = c("", data$Nombre))
  })

  observeEvent(input$upload_btn, {
    req(input$file_upload, input$project_select, input$file_type)

    if (!tokens_valid()) {
      output$upload_status <- renderText("❌ Error: Configure primero OAuth de Dropbox.")
      return()
    }

    project_name <- input$project_select
    file_type <- input$file_type
    file_info <- input$file_upload

    output$upload_status <- renderText("📤 Subiendo archivo a Dropbox...")

    tryCatch({
      if (is.null(project_name) || project_name == "") {
        output$upload_status <- renderText("❌ Error: Seleccione un proyecto válido.")
        return()
      }

      if (is.null(file_type) || file_type == "") {
        output$upload_status <- renderText("❌ Error: Seleccione un tipo de archivo válido.")
        return()
      }

      if (is.null(file_info) || is.null(file_info$datapath) || !file.exists(file_info$datapath)) {
        output$upload_status <- renderText("❌ Error: Archivo no válido o no encontrado.")
        return()
      }

      sanitized_project <- sanitize_project_name(project_name)
      folder_path <- paste0("scicontrol/", sanitized_project, "/", file_type)

      result <- upload_to_dropbox(file_info$datapath, file_info$name, folder_path)

      if (result$success) {
        size_text <- if (result$size < 1024) {
          paste(result$size, "B")
        } else if (result$size < 1024^2) {
          paste(round(result$size/1024, 1), "KB")
        } else {
          paste(round(result$size/1024^2, 1), "MB")
        }

        output$upload_status <- renderText(
          paste("✅ Archivo subido exitosamente a Dropbox:",
                "\n📁 Nombre original:", result$original_name,
                "\n📄 Nombre en Dropbox:", result$clean_name,
                "\n📊 Tamaño:", size_text,
                "\n📂 Carpeta:", file_type,
                "\n🔬 Proyecto:", project_name,
                "\n📍 Path completo:", result$path,
                if (result$url != "") paste("\n🔗 Enlace directo:", result$url) else "")
        )

        files_refresh(isolate(files_refresh()) + 1)
        showNotification("Archivo subido exitosamente", type = "message")

      } else {
        output$upload_status <- renderText(paste("❌ Error al subir archivo:", result$error))
        showNotification("Error al subir archivo", type = "error")
      }
    }, error = function(e) {
      error_msg <- paste("❌ Error inesperado:", e$message)
      output$upload_status <- renderText(error_msg)
      showNotification("Error inesperado al subir archivo", type = "error")
    })
  })

  # ========================================================================
  # TABLA DE ARCHIVOS EN DROPBOX
  # ========================================================================

  output$files_table <- renderDT({
    files_refresh()

    if (input$project_view == "" || !tokens_valid()) {
      empty <- data.frame(Archivo = character(), Carpeta = character(), Tamaño = character(), Fecha = character())
      return(datatable(empty, options = list(pageLength = 10), rownames = FALSE, selection = "single"))
    }

    project_name <- input$project_view
    sanitized_name <- sanitize_project_name(project_name)

    tryCatch({
      folder_prefix <- paste0("scicontrol/", sanitized_name)
      files <- list_dropbox_files(folder_prefix)

      if (nrow(files) == 0) {
        empty <- data.frame(Archivo = character(), Carpeta = character(), Tamaño = character(), Fecha = character())
        return(datatable(empty, options = list(pageLength = 10), rownames = FALSE, selection = "single"))
      }

      files$Carpeta <- sapply(files$path, function(x) {
        parts <- strsplit(x, "/")[[1]]
        if (length(parts) >= 4) parts[4] else "general"
      })

      files$Tamaño <- sapply(files$size, function(x) {
        if (x < 1024) paste(x, "B")
        else if (x < 1024^2) paste(round(x/1024, 1), "KB")
        else if (x < 1024^3) paste(round(x/1024^2, 1), "MB")
        else paste(round(x/1024^3, 1), "GB")
      })

      files$Fecha <- format(
        as.POSIXct(files$modified,
                   format = "%Y-%m-%dT%H:%M:%SZ",
                   tz     = "UTC"),
        tz     = "America/Lima",
        format = "%Y-%m-%d %H:%M")

      obs_text <- ""
      pdata <- project_data()
      if (!is.null(pdata) && nrow(pdata) > 0 && "Observaciones" %in% colnames(pdata)) {
        obs_val <- pdata$Observaciones[pdata$Nombre == project_name]
        if (length(obs_val) > 0) obs_text <- as.character(obs_val[1])
      }

      df <- data.frame(
        Archivo = files$name,
        Carpeta = files$Carpeta,
        Tamaño = files$Tamaño,
        Fecha = files$Fecha,
        Observaciones = rep(obs_text, nrow(files)),
        Path = files$path,
        stringsAsFactors = FALSE
      )

      return(datatable(df[, c(1:4,5)], options = list(pageLength = 10), rownames = FALSE, selection = "single"))
    }, error = function(e) {
      error_df <- data.frame(
        Error = paste("Error al cargar archivos:", e$message),
        Carpeta = "", Tamaño = "", Fecha = ""
      )
      return(datatable(error_df, options = list(pageLength = 10), rownames = FALSE, selection = "single"))
    })
  })

  observeEvent(input$refresh_files, {
    files_refresh(isolate(files_refresh()) + 1)
    showNotification("Lista de archivos actualizada", type = "message")
  })

  observeEvent(input$delete_file, {
    sel <- input$files_table_rows_selected
    if (is.null(sel) || length(sel) == 0 || !tokens_valid()) {
      showModal(modalDialog(
        title = "Atención",
        "Por favor, seleccione primero un archivo de la tabla.",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      return()
    }

    project_name <- input$project_view
    sanitized_name <- sanitize_project_name(project_name)

    tryCatch({
      folder_prefix <- paste0("scicontrol/", sanitized_name)
      files <- list_dropbox_files(folder_prefix)

      file_to_delete_path <- files$path[sel]
      file_name <- files$name[sel]

      if (delete_dropbox_file(file_to_delete_path)) {
        showModal(modalDialog(
          title = "Éxito",
          paste("Archivo eliminado de Dropbox:", file_name),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
        files_refresh(isolate(files_refresh()) + 1)
      } else {
        showModal(modalDialog(
          title = "Error",
          "No se pudo eliminar el archivo de Dropbox.",
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Error",
        paste("Error al eliminar archivo:", e$message),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    })
  })

  # ========================================================================
  # SELECCIÓN DE PROYECTO PARA EDITAR
  # ========================================================================

  observeEvent(input$project_table_rows_selected, {
    sel <- input$project_table_rows_selected

    if (!is.null(sel) && length(sel) > 0) {
      data <- project_data()

      if (sel <= nrow(data)) {
        proj <- data[sel, ]

        updateTextInput(session, "project_name", value = as.character(proj$Nombre %||% ""))

        tryCatch({
          if (!is.na(proj$Fecha_Inicio) && proj$Fecha_Inicio != "") {
            updateDateInput(session, "start_date", value = as.Date(proj$Fecha_Inicio))
          } else {
            updateDateInput(session, "start_date", value = NULL)
          }
        }, error = function(e) {
          updateDateInput(session, "start_date", value = NULL)
        })

        tryCatch({
          if (!is.na(proj$Fecha_Envio) && proj$Fecha_Envio != "") {
            updateDateInput(session, "send_date", value = as.Date(proj$Fecha_Envio))
          } else {
            updateDateInput(session, "send_date", value = NULL)
          }
        }, error = function(e) {
          updateDateInput(session, "send_date", value = NULL)
        })

        tryCatch({
          if (!is.na(proj$Fecha_Respuesta) && proj$Fecha_Respuesta != "") {
            updateDateInput(session, "response_date", value = as.Date(proj$Fecha_Respuesta))
          } else {
            updateDateInput(session, "response_date", value = NULL)
          }
        }, error = function(e) {
          updateDateInput(session, "response_date", value = NULL)
        })

        tryCatch({
          if (!is.na(proj$Fecha_Aceptado) && proj$Fecha_Aceptado != "") {
            updateDateInput(session, "acceptance_date", value = as.Date(proj$Fecha_Aceptado))
          } else {
            updateDateInput(session, "acceptance_date", value = NULL)
          }
        }, error = function(e) {
          updateDateInput(session, "acceptance_date", value = NULL)
        })

        tryCatch({
          if (!is.na(proj$Fecha_Publicado) && proj$Fecha_Publicado != "") {
            updateDateInput(session, "publication_date", value = as.Date(proj$Fecha_Publicado))
          } else {
            updateDateInput(session, "publication_date", value = NULL)
          }
        }, error = function(e) {
          updateDateInput(session, "publication_date", value = NULL)
        })

        updateTextInput(session, "journal", value = as.character(proj$Revista %||% ""))
        updateSelectInput(session, "quartile", selected = as.character(proj$Cuartil %||% "Q1"))
        updateSelectInput(session, "status", selected = as.character(proj$Estado %||% "Introducción"))
        updateSelectInput(session, "group", selected = as.character(proj$Grupo %||% "Equipo de Investigación"))
        updateTextInput(session, "research_line", value = as.character(proj$Linea_Investigacion %||% ""))
        updateTextAreaInput(session, "observations", value = as.character(proj$Observaciones %||% ""))
      }
    }
  })

  # ========================================================================
  # EVENTO PARA GUARDAR CAMBIOS
  # ========================================================================

  observeEvent(input$save_changes, {
    if (is.null(input$project_name) || input$project_name == "") {
      showModal(modalDialog(
        title = "Error",
        "Por favor ingrese un nombre para el proyecto.",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      return()
    }

    data <- project_data()
    idx <- which(data$Nombre == input$project_name)

    fecha_inicio <- if (!is.null(input$start_date)) as.character(input$start_date) else NA_character_
    fecha_envio <- if (input$status %in% c("Enviado", "Revisión", "Aceptado", "Publicado") && !is.null(input$send_date)) {
      as.character(input$send_date)
    } else NA_character_
    fecha_resp <- if (input$status %in% c("Revisión","Aceptado","Publicado") && !is.null(input$response_date)) {
      as.character(input$response_date)
    } else NA_character_
    fecha_acc <- if (input$status %in% c("Aceptado","Publicado") && !is.null(input$acceptance_date)) {
      as.character(input$acceptance_date)
    } else NA_character_
    fecha_pub <- if (input$status == "Publicado" && !is.null(input$publication_date)) {
      as.character(input$publication_date)
    } else NA_character_

    prog <- progress_map[[input$status]]

    # 🔹 NUEVO: conservar el valor previo de 'Envio_Correo' si existe
    envio_correo_val <- if ("Envio_Correo" %in% names(data) && length(idx) > 0) {
      as.character(data$Envio_Correo[idx][1])
    } else {
      NA_character_
    }

    new_row <- data.frame(
      Nombre = as.character(input$project_name),
      Fecha_Inicio = fecha_inicio,
      Fecha_Envio = fecha_envio,
      Fecha_Respuesta = fecha_resp,
      Revista = as.character(input$journal %||% ""),
      Cuartil = as.character(input$quartile %||% "Q1"),
      Estado = as.character(input$status),
      Grupo = as.character(input$group),
      Progreso = as.numeric(prog),
      Fecha_Aceptado = fecha_acc,
      Fecha_Publicado = fecha_pub,
      Linea_Investigacion = as.character(input$research_line %||% ""),
      Observaciones = as.character(input$observations %||% ""),
      Envio_Correo = envio_correo_val,   # ← NUEVO
      stringsAsFactors = FALSE
    )

    alertas_validacion <- c()
    if (!is.na(fecha_inicio) && !is.na(fecha_envio) && as.Date(fecha_envio) < as.Date(fecha_inicio)) {
      alertas_validacion <- c(alertas_validacion, "La fecha de envío es anterior a la fecha de inicio")
    }
    if (!is.na(fecha_envio) && !is.na(fecha_resp) && as.Date(fecha_resp) < as.Date(fecha_envio)) {
      alertas_validacion <- c(alertas_validacion, "La fecha de respuesta es anterior a la fecha de envío")
    }

    if (length(alertas_validacion) > 0) {
      showModal(modalDialog(
        title = "⚠️ Advertencia de Validación",
        HTML(paste("Se detectaron las siguientes inconsistencias:",
                   "<ul><li>", paste(alertas_validacion, collapse = "</li><li>"), "</li></ul>",
                   "<br><strong>¿Desea continuar guardando el proyecto?</strong>")),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_save", "Guardar de todas formas", class = "btn-warning")
        )
      ))
      return()
    }

    # Guardar datos
    tryCatch({
      if (length(idx) > 0) {
        data[idx, ] <- new_row
      } else {
        data <- rbind(data, new_row)
      }

      save_result <- save_project_data(data)

      if (save_result$success) {
        project_data(data)
        showModal(modalDialog(
          title = "✅ Éxito",
          HTML(paste("El proyecto ha sido guardado correctamente.",
                     "<br><strong>💾 Guardado en Dropbox:</strong>", save_result$main_path,
                     if(!is.null(save_result$backup_path)) paste("<br><strong>📋 Backup:</strong>", save_result$backup_path) else "")),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      } else {
        project_data(data)
        showModal(modalDialog(
          title = "⚠️ Advertencia",
          HTML(paste("El proyecto se guardó localmente pero hubo un problema con Dropbox:",
                     "<br><strong>Error:</strong>", save_result$error,
                     "<br><br>Los datos estarán disponibles en esta sesión pero podrían perderse al republicar.")),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "❌ Error",
        paste("Error al guardar el proyecto:", e$message),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    })
  })



  # Evento para confirmar guardado con advertencias
  observeEvent(input$confirm_save, {
    removeModal()

    data <- project_data()
    idx <- which(data$Nombre == input$project_name)

    fecha_inicio <- if (!is.null(input$start_date)) as.character(input$start_date) else NA_character_
    fecha_envio <- if (input$status %in% c("Enviado", "Revisión", "Aceptado", "Publicado") && !is.null(input$send_date)) {
      as.character(input$send_date)
    } else NA_character_
    fecha_resp <- if (input$status %in% c("Revisión","Aceptado","Publicado") && !is.null(input$response_date)) {
      as.character(input$response_date)
    } else NA_character_
    fecha_acc <- if (input$status %in% c("Aceptado","Publicado") && !is.null(input$acceptance_date)) {
      as.character(input$acceptance_date)
    } else NA_character_
    fecha_pub <- if (input$status == "Publicado" && !is.null(input$publication_date)) {
      as.character(input$publication_date)
    } else NA_character_

    prog <- progress_map[[input$status]]

    # 🔹 NUEVO: conservar el valor previo de 'Envio_Correo' si existe
    envio_correo_val <- if ("Envio_Correo" %in% names(data) && length(idx) > 0) {
      as.character(data$Envio_Correo[idx][1])
    } else {
      NA_character_
    }

    new_row <- data.frame(
      Nombre = as.character(input$project_name),
      Fecha_Inicio = fecha_inicio,
      Fecha_Envio = fecha_envio,
      Fecha_Respuesta = fecha_resp,
      Revista = as.character(input$journal %||% ""),
      Cuartil = as.character(input$quartile %||% "Q1"),
      Estado = as.character(input$status),
      Grupo = as.character(input$group),
      Progreso = as.numeric(prog),
      Fecha_Aceptado = fecha_acc,
      Fecha_Publicado = fecha_pub,
      Linea_Investigacion = as.character(input$research_line %||% ""),
      Observaciones = as.character(input$observations %||% ""),
      Envio_Correo = envio_correo_val,   # ← NUEVO
      stringsAsFactors = FALSE
    )

    tryCatch({
      if (length(idx) > 0) {
        data[idx, ] <- new_row
      } else {
        data <- rbind(data, new_row)
      }

      save_result <- save_project_data(data)

      if (save_result$success) {
        project_data(data)
        showModal(modalDialog(
          title = "✅ Éxito",
          "El proyecto ha sido guardado correctamente (con advertencias) en Dropbox.",
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      } else {
        project_data(data)
        showModal(modalDialog(
          title = "⚠️ Advertencia",
          paste("El proyecto se guardó localmente (con advertencias) pero hubo un problema con Dropbox:", save_result$error),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "❌ Error",
        paste("Error al guardar el proyecto:", e$message),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    })
  })


  # ========================================================================
  # LIMPIAR CAMPOS
  # ========================================================================

  observeEvent(input$clear_fields, {
    updateTextInput(session, "project_name", value = "")
    updateDateInput(session, "start_date", value = NULL)
    updateDateInput(session, "send_date", value = NULL)
    updateDateInput(session, "response_date", value = NULL)
    updateTextInput(session, "journal", value = "")
    updateSelectInput(session, "quartile", selected = "Q1")
    updateSelectInput(session, "status", selected = "Introducción")
    updateSelectInput(session, "group", selected = "Equipo de Investigación")
    updateDateInput(session, "acceptance_date", value = NULL)
    updateDateInput(session, "publication_date", value = NULL)
    updateTextInput(session, "research_line", value = "")
    updateTextAreaInput(session, "observations", value = "")
  })

  # ========================================================================
  # ELIMINAR PROYECTO
  # ========================================================================

  observeEvent(input$delete_button, {
    req(input$delete_project)
    data <- project_data()
    if (input$delete_project == "(sin nombre)") {
      data <- data[!(is.na(data$Nombre) | data$Nombre==""),]
    } else {
      data <- data[data$Nombre != input$delete_project,]
    }

    save_result <- save_project_data(data)
    project_data(data)

    if (save_result$success) {
      showModal(modalDialog(title="Proyecto Eliminado",
                            paste("El proyecto", input$delete_project, "ha sido eliminado y sincronizado con Dropbox."),
                            easyClose=TRUE, footer=modalButton("Cerrar")))
    } else {
      showModal(modalDialog(title="Proyecto Eliminado",
                            paste("El proyecto", input$delete_project, "ha sido eliminado localmente. Error en Dropbox:", save_result$error),
                            easyClose=TRUE, footer=modalButton("Cerrar")))
    }
  })

  # ========================================================================
  # FUNCIONES DE SINCRONIZACIÓN
  # ========================================================================

  output$sync_status_info <- renderUI({
    data <- project_data()
    last_save_time <- if (file.exists(data_file)) {
      format(
        file.mtime(data_file),
        tz     = "America/Lima",
        usetz  = FALSE,
        format = "%Y-%m-%d %H:%M:%S"
      )
    } else {
      "Nunca"
    }

    div(
      h4("📊 Estado Actual del Sistema"),
      tags$ul(
        tags$li(paste("💾 Proyectos en memoria:", nrow(data))),
        tags$li(paste("🕒 Última modificación local:", last_save_time)),
        tags$li(paste("☁️ Dropbox:", if(tokens_valid()) "✅ Conectado" else "❌ No conectado")),
        tags$li(paste("🔄 Auto-sincronización:", if(tokens_valid()) "✅ Activa" else "❌ Inactiva"))
      ),
      if (!tokens_valid()) {
        div(class = "alert alert-warning",
            "⚠️ Configure OAuth de Dropbox para habilitar la sincronización automática y evitar pérdida de datos."
        )
      } else {
        div(class = "alert alert-success",
            "✅ Sistema configurado correctamente. Los datos se sincronizan automáticamente con Dropbox."
        )
      }
    )
  })

  # Sincronización manual desde Dropbox
  observeEvent(input$sync_from_dropbox, {
    if (!tokens_valid()) {
      output$sync_operation_status <- renderText("❌ Dropbox no está configurado.")
      return()
    }

    output$sync_operation_status <- renderText("🔄 Cargando datos desde Dropbox...")

    tryCatch({
      dropbox_data <- load_data_from_dropbox()

      if (nrow(dropbox_data) > 0) {
        project_data(dropbox_data)
        output$sync_operation_status <- renderText(
          paste("✅ Sincronización exitosa desde Dropbox.",
                "Cargados", nrow(dropbox_data), "proyectos.")
        )
        showNotification("Datos sincronizados desde Dropbox", type = "message")
      } else {
        output$sync_operation_status <- renderText("⚠️ No se encontraron datos en Dropbox.")
      }
    }, error = function(e) {
      output$sync_operation_status <- renderText(paste("❌ Error al sincronizar:", e$message))
    })
  })

  # Sincronización manual a Dropbox
  observeEvent(input$sync_to_dropbox, {
    if (!tokens_valid()) {
      output$sync_operation_status <- renderText("❌ Dropbox no está configurado.")
      return()
    }

    data <- project_data()
    if (nrow(data) == 0) {
      output$sync_operation_status <- renderText("⚠️ No hay datos para sincronizar.")
      return()
    }

    output$sync_operation_status <- renderText("🔄 Subiendo datos a Dropbox...")

    tryCatch({
      save_result <- save_data_to_dropbox(data)

      if (save_result$success) {
        output$sync_operation_status <- renderText(
          paste("✅ Sincronización exitosa a Dropbox.",
                "Guardados", nrow(data), "proyectos.",
                "\n📁 Archivo principal:", save_result$main_path,
                "\n📋 Backup:", save_result$backup_path)
        )
        showNotification("Datos sincronizados a Dropbox", type = "message")
      } else {
        output$sync_operation_status <- renderText(paste("❌ Error al sincronizar:", save_result$error))
      }
    }, error = function(e) {
      output$sync_operation_status <- renderText(paste("❌ Error al sincronizar:", e$message))
    })
  })

  # Crear backup manual
  observeEvent(input$manual_backup, {
    if (!tokens_valid()) {
      output$sync_operation_status <- renderText("❌ Dropbox no está configurado.")
      return()
    }

    data <- project_data()
    if (nrow(data) == 0) {
      output$sync_operation_status <- renderText("⚠️ No hay datos para respaldar.")
      return()
    }

    output$sync_operation_status <- renderText("🔄 Creando backup manual...")

    tryCatch({
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      temp_file <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(data, temp_file)

      backup_result <- upload_to_dropbox(temp_file,
                                         paste0("manual_backup_", timestamp, ".xlsx"),
                                         "scicontrol/manual_backups")
      unlink(temp_file)

      if (backup_result$success) {
        output$sync_operation_status <- renderText(
          paste("✅ Backup manual creado exitosamente.",
                "\n📋 Archivo:", backup_result$name,
                "\n📁 Ubicación:", backup_result$path)
        )
        showNotification("Backup manual creado", type = "message")
      } else {
        output$sync_operation_status <- renderText(paste("❌ Error al crear backup:", backup_result$error))
      }
    }, error = function(e) {
      output$sync_operation_status <- renderText(paste("❌ Error al crear backup:", e$message))
    })
  })

  # Lista de seguimiento
  seguimiento_df <- reactive({
    df <- project_data()

    df <- df %>%
      dplyr::filter(
        Estado == "Enviado",
        !is.na(Fecha_Envio),
        Fecha_Envio != ""
      ) %>%
      dplyr::mutate(
        Fecha_Envio = as.Date(Fecha_Envio),
        Dias_Transcurridos = as.numeric(difftime(Sys.Date(), Fecha_Envio, units = "days")),
        Alerta = ifelse(Dias_Transcurridos > 60, "📧 Enviar correo de seguimiento", ""),
        # ID estable por fila (usa nombre + fecha) para inputs de Shiny
        CorreoID = paste0(
          "correo_",
          sapply(Nombre, sanitize_project_name),
          "_",
          format(Fecha_Envio, "%Y%m%d")
        )
      ) %>%
      dplyr::select(Revista, Cuartil, Nombre, Fecha_Envio, Dias_Transcurridos, Alerta,
                    Envio_Correo, CorreoID)

    df
  })

  # Lista de seguimiento (con columna "Se envió correo")
  output$seguimiento_table <- renderDT({
    df <- seguimiento_df()

    # Columna de selectInput, preseleccionando lo guardado si existe
    if (nrow(df) > 0) {
      df[["Se envió correo"]] <- vapply(seq_len(nrow(df)), function(i) {
        as.character(
          selectInput(
            inputId  = df$CorreoID[i],            # ID estable
            label    = NULL,
            choices  = c("SI", "NO"),
            selected = df$Envio_Correo[i] %||% "",# preselección
            width    = "90px"
          )
        )
      }, character(1))
    } else {
      df[["Se envió correo"]] <- character(0)
    }

    # Ocultamos columnas auxiliares
    df_show <- df %>% dplyr::select(-Envio_Correo, -CorreoID)

    datatable(
      df_show,
      escape   = FALSE,
      options  = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      caption  = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: left;',
        "Proyectos Enviados y días transcurridos"
      )
    )
  })

  observeEvent(input$save_seguimiento, {
    df_seg <- seguimiento_df()
    if (nrow(df_seg) == 0) {
      showNotification("No hay filas para guardar.", type = "warning")
      return()
    }

    data <- project_data()
    guardadas <- 0L

    for (i in seq_len(nrow(df_seg))) {
      val <- input[[ df_seg$CorreoID[i] ]]
      if (!is.null(val) && val %in% c("SI","NO")) {
        # Match por Nombre + Fecha_Envio (más seguro que solo nombre)
        idx <- which(
          data$Nombre == df_seg$Nombre[i] &
            as.character(data$Fecha_Envio) == as.character(df_seg$Fecha_Envio[i])
        )
        if (length(idx) == 0) {
          # fallback: por Nombre
          idx <- which(data$Nombre == df_seg$Nombre[i])
        }
        if (length(idx) > 0) {
          data$Envio_Correo[idx[1]] <- val
          guardadas <- guardadas + 1L
        }
      }
    }

    # Persistir (Dropbox + local)
    res <- save_project_data(data)
    project_data(data)

    if (isTRUE(res$success)) {
      showModal(modalDialog(
        title = "✅ Seguimiento guardado",
        HTML(paste0(
          "Valores guardados para <strong>", guardadas, "</strong> fila(s).",
          "<br><small>Archivo: ", res$main_path, "</small>"
        )),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    } else {
      showModal(modalDialog(
        title = "⚠️ Guardado parcial",
        HTML(paste0(
          "Se actualizaron <strong>", guardadas, "</strong> fila(s) en memoria, ",
          "pero hubo un problema al guardar en Dropbox.<br><small>Error: ",
          res$error, "</small>"
        )),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    }
  })

  output$download_seguimiento <- downloadHandler(
    filename = function() paste0("seguimiento_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- seguimiento_df()

      if (nrow(df) > 0) {
        # Toma lo seleccionado en pantalla si existe; si no, lo ya guardado
        df$Envio_Correo <- vapply(seq_len(nrow(df)), function(i) {
          input[[ df$CorreoID[i] ]] %||% (df$Envio_Correo[i] %||% "")
        }, character(1))
      }

      # Exporta columnas útiles
      writexl::write_xlsx(
        df %>% dplyr::select(Revista, Cuartil, Nombre, Fecha_Envio,
                             Dias_Transcurridos, Alerta, Envio_Correo),
        file
      )
    }
  )


  # Lista de archivos de backup
  output$backup_files_table <- renderDT({
    if (!tokens_valid()) {
      empty <- data.frame(Archivo = "Dropbox no configurado", Fecha = "", Tamaño = "", Tipo = "")
      return(datatable(empty, options = list(pageLength = 5), rownames = FALSE, selection = "single"))
    }

    tryCatch({
      backup_files <- list_dropbox_files("scicontrol/backups")
      manual_backups <- list_dropbox_files("scicontrol/manual_backups")

      all_backups <- rbind(
        if(nrow(backup_files) > 0) data.frame(backup_files, Tipo = "Auto") else data.frame(),
        if(nrow(manual_backups) > 0) data.frame(manual_backups, Tipo = "Manual") else data.frame()
      )

      if (nrow(all_backups) == 0) {
        empty <- data.frame(Archivo = "No hay backups disponibles", Fecha = "", Tamaño = "", Tipo = "")
        return(datatable(empty, options = list(pageLength = 5), rownames = FALSE, selection = "single"))
      }

      all_backups$Tamaño <- sapply(all_backups$size, function(x) {
        if (x < 1024) paste(x, "B")
        else if (x < 1024^2) paste(round(x/1024, 1), "KB")
        else paste(round(x/1024^2, 1), "MB")
      })

      all_backups$Fecha <- format(
        as.POSIXct(all_backups$modified,
                   format = "%Y-%m-%dT%H:%M:%SZ",
                   tz     = "UTC"),
        tz     = "America/Lima",
        format = "%Y-%m-%d %H:%M")

      display_backups <- all_backups[, c("name", "Fecha", "Tamaño", "Tipo")]
      colnames(display_backups) <- c("Archivo", "Fecha", "Tamaño", "Tipo")

      return(datatable(display_backups,
                       options = list(pageLength = 10, order = list(list(1, 'desc'))),
                       rownames = FALSE,
                       selection = "single"))
    }, error = function(e) {
      error_df <- data.frame(Archivo = paste("Error:", e$message), Fecha = "", Tamaño = "", Tipo = "")
      return(datatable(error_df, options = list(pageLength = 5), rownames = FALSE, selection = "single"))
    })
  })

  # Restaurar backup seleccionado
  observeEvent(input$restore_backup, {
    sel <- input$backup_files_table_rows_selected

    if (is.null(sel) || length(sel) == 0) {
      showModal(modalDialog(
        title = "Atención",
        "Por favor, seleccione primero un archivo de backup de la tabla.",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      return()
    }

    if (!tokens_valid()) {
      output$sync_operation_status <- renderText("❌ Dropbox no está configurado.")
      return()
    }

    tryCatch({
      backup_files <- list_dropbox_files("scicontrol/backups")
      manual_backups <- list_dropbox_files("scicontrol/manual_backups")

      all_backups <- rbind(
        if(nrow(backup_files) > 0) data.frame(backup_files, Tipo = "Auto") else data.frame(),
        if(nrow(manual_backups) > 0) data.frame(manual_backups, Tipo = "Manual") else data.frame()
      )

      if (sel > nrow(all_backups)) {
        output$sync_operation_status <- renderText("❌ Selección inválida.")
        return()
      }

      selected_backup <- all_backups[sel, ]

      showModal(modalDialog(
        title = "🔄 Confirmar Restauración",
        HTML(paste("¿Está seguro de que desea restaurar el siguiente backup?",
                   "<br><br><strong>Archivo:</strong>", selected_backup$name,
                   "<br><strong>Fecha:</strong>", format(as.POSIXct(selected_backup$modified, format="%Y-%m-%dT%H:%M:%SZ"), "%Y-%m-%d %H:%M"),
                   "<br><strong>Tipo:</strong>", selected_backup$Tipo,
                   "<br><br><span style='color: red;'>⚠️ Esto reemplazará todos los datos actuales.</span>")),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("confirm_restore", "Restaurar", class = "btn-warning")
        )
      ))

      backup_to_restore <<- selected_backup$path

    }, error = function(e) {
      output$sync_operation_status <- renderText(paste("❌ Error al preparar restauración:", e$message))
    })
  })

  # Confirmar restauración
  observeEvent(input$confirm_restore, {
    removeModal()

    if (!exists("backup_to_restore") || is.null(backup_to_restore)) {
      output$sync_operation_status <- renderText("❌ Error: No se pudo identificar el backup a restaurar.")
      return()
    }

    output$sync_operation_status <- renderText("🔄 Restaurando backup...")

    tryCatch({
      response <- GET(
        url = "https://content.dropboxapi.com/2/files/download",
        add_headers(
          "Authorization" = paste("Bearer", get_dropbox_token()),
          "Dropbox-API-Arg" = jsonlite::toJSON(list(
            path = backup_to_restore
          ), auto_unbox = TRUE)
        )
      )

      if (response$status_code == 200) {
        temp_file <- tempfile(fileext = ".xlsx")
        writeBin(content(response, "raw"), temp_file)

        restored_data <- read_excel(temp_file)
        unlink(temp_file)

        required_columns <- c("Nombre", "Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                              "Revista", "Cuartil", "Estado", "Grupo", "Progreso",
                              "Fecha_Aceptado", "Fecha_Publicado", "Linea_Investigacion",
                              "Observaciones", ,"Envio_Correo")

        missing_columns <- setdiff(required_columns, colnames(restored_data))
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            restored_data[[col]] <- NA
          }
        }

        date_columns <- c("Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                          "Fecha_Aceptado", "Fecha_Publicado")

        for (col in date_columns) {
          if (col %in% colnames(restored_data)) {
            if (inherits(restored_data[[col]], c("POSIXct", "POSIXt", "Date"))) {
              restored_data[[col]] <- as.character(as.Date(restored_data[[col]]))
            }
          }
        }

        if ("Progreso" %in% colnames(restored_data)) {
          restored_data$Progreso <- as.numeric(restored_data$Progreso)
        }

        restored_data <- as.data.frame(restored_data[, required_columns], stringsAsFactors = FALSE)

        project_data(restored_data)

        current_backup_result <- save_data_to_dropbox(restored_data)

        output$sync_operation_status <- renderText(
          paste("✅ Backup restaurado exitosamente.",
                "\n📊 Proyectos restaurados:", nrow(restored_data),
                "\n📁 Archivo restaurado:", basename(backup_to_restore),
                if(current_backup_result$success) paste("\n💾 Nuevo backup creado:", current_backup_result$backup_path) else "")
        )

        showNotification("Backup restaurado exitosamente", type = "message")

      } else {
        output$sync_operation_status <- renderText(paste("❌ Error al descargar backup. Código:", response$status_code))
      }

      backup_to_restore <<- NULL

    }, error = function(e) {
      output$sync_operation_status <- renderText(paste("❌ Error al restaurar backup:", e$message))
      backup_to_restore <<- NULL
    })
  })

  # ========================================================================
  # DESCARGAR DATOS
  # ========================================================================

  output$download_data <- downloadHandler(
    filename = function() paste0("scicontrol_datos_", Sys.Date(), ".xlsx"),
    content = function(file) {
      data_with_analysis <- days_data_improved()
      data_list <- list(
        "Proyectos" = data_with_analysis,
        "Analisis_Tiempos" = data_with_analysis[, c("Nombre","Revista","Cuartil", "Estado",
                                                    "Dias_Envio_Inicio","Dias_Respuesta_Envio",
                                                    "Dias_Aceptado_Respuesta","Dias_Aceptado_Envio",
                                                    "Dias_Aceptado_Publicado", "Alertas")],
        "Resumen_Metricas" = data.frame(
          Metrica = c("Total Proyectos", "Publicados", "Aceptados",
                      "Tiempo Promedio Revisión (días)", "Tiempo Promedio Aceptación (días)"),
          Valor = c(nrow(data_with_analysis),
                    sum(data_with_analysis$Estado == "Publicado", na.rm = TRUE),
                    sum(data_with_analysis$Estado %in% c("Aceptado", "Publicado"), na.rm = TRUE),
                    round(mean(data_with_analysis$Dias_Respuesta_Envio, na.rm = TRUE), 1),
                    round(mean(data_with_analysis$Dias_Aceptado_Envio, na.rm = TRUE), 1))
        )
      )
      writexl::write_xlsx(data_list, file)
    }
  )

  # ========================================================================
  # IMPORTAR DATOS DESDE EXCEL
  # ========================================================================

  observeEvent(input$import_excel_btn, {
    req(input$excel_upload)

    file_ext <- tools::file_ext(input$excel_upload$name)
    if (!file_ext %in% c("xlsx", "xls")) {
      output$import_status <- renderText("❌ Por favor, suba un archivo Excel válido (.xlsx o .xls).")
      return()
    }

    tryCatch({
      sheets <- readxl::excel_sheets(input$excel_upload$datapath)

      project_sheet <- NULL
      if ("Proyectos" %in% sheets) {
        project_sheet <- "Proyectos"
      } else if (length(sheets) > 0) {
        project_sheet <- sheets[1]
      }

      if (is.null(project_sheet)) {
        output$import_status <- renderText("❌ No se encontraron hojas válidas en el archivo Excel.")
        return()
      }

      imported_data <- read_excel(input$excel_upload$datapath, sheet = project_sheet)

      required_columns <- c("Nombre", "Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                            "Revista", "Cuartil", "Estado", "Grupo", "Progreso",
                            "Fecha_Aceptado", "Fecha_Publicado", "Linea_Investigacion",
                            "Observaciones","Envio_Correo")

      if (!"Nombre" %in% colnames(imported_data)) {
        output$import_status <- renderText("❌ El archivo debe contener al menos una columna 'Nombre'.")
        return()
      }

      missing_columns <- setdiff(required_columns, colnames(imported_data))
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          imported_data[[col]] <- NA
        }
      }

      date_columns <- c("Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                        "Fecha_Aceptado", "Fecha_Publicado")

      for (col in date_columns) {
        if (col %in% colnames(imported_data)) {
          if (inherits(imported_data[[col]], c("POSIXct", "POSIXt", "Date"))) {
            imported_data[[col]] <- as.character(as.Date(imported_data[[col]]))
          } else if (is.character(imported_data[[col]])) {
            imported_data[[col]] <- sapply(imported_data[[col]], function(x) {
              if (is.na(x) || x == "" || x == "NA") return(NA_character_)
              tryCatch({
                fecha <- as.Date(x)
                if (!is.na(fecha)) return(as.character(fecha))
                return(NA_character_)
              }, error = function(e) NA_character_)
            })
          }
        }
      }

      if ("Progreso" %in% colnames(imported_data)) {
        imported_data$Progreso <- as.numeric(imported_data$Progreso)
      }

      imported_data <- as.data.frame(imported_data[, required_columns], stringsAsFactors = FALSE)

      save_result <- save_project_data(imported_data)

      if (save_result$success) {
        project_data(imported_data)

        output$import_status <- renderText(paste("✅ Datos importados exitosamente a Dropbox.",
                                                 nrow(imported_data),
                                                 "proyectos fueron cargados desde la hoja:", project_sheet))

        showModal(modalDialog(
          title = "✅ Éxito",
          HTML(paste("Los datos se han importado y guardado en Dropbox correctamente.",
                     "<br><strong>Proyectos cargados:</strong>", nrow(imported_data),
                     "<br><strong>💾 Guardado en:</strong>", save_result$main_path)),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      } else {
        project_data(imported_data)
        output$import_status <- renderText(paste("⚠️ Datos importados localmente, pero error en Dropbox:", save_result$error))

        showModal(modalDialog(
          title = "⚠️ Importación Parcial",
          HTML(paste("Los datos se importaron localmente pero hubo un problema con Dropbox:",
                     "<br><strong>Error:</strong>", save_result$error,
                     "<br><br>Los datos estarán disponibles en esta sesión.")),
          easyClose = TRUE,
          footer = modalButton("Cerrar")
        ))
      }
    }, error = function(e) {
      output$import_status <- renderText(paste("❌ Error al importar datos:", e$message))
      showModal(modalDialog(
        title = "❌ Error de Importación",
        paste("Error al procesar el archivo Excel:", e$message),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    })
  })
}

# ============================================================================
# EJECUTAR LA APLICACIÓN
# ============================================================================

# Al cargar la aplicación, inicializar OAuth automáticamente
# initialize_dropbox_oauth()

# ============================================================================
# MODO DESARROLLADOR - INSTRUCCIONES COMPLETAS
# ============================================================================
#
# ACTIVAR MODO DESARROLLADOR:
# 1. Hacer doble clic en "SciControl" en el header
# 2. Ingresar contraseña: "scicontrol2025"
# 3. Acceder a las configuraciones OAuth avanzadas
#
# SALIR DEL MODO DESARROLLADOR:
# 1. Usar el botón "Salir" en el sidebar (método rápido)
# 2. Usar el botón "🔒 Salir del Modo Desarrollador" en la pestaña Configuración (método seguro con confirmación)
# 3. La pestaña de Configuración se ocultará automáticamente
#
# INDICADORES VISUALES:
# - Header: Muestra "🔓 DEV" cuando está activo
# - Sidebar: Muestra indicador verde "🔓 MODO DESARROLLADOR" con botón de salida
# - Pestaña Configuración: Solo visible en modo desarrollador
#
# PERSONALIZACIÓN:
# - Para cambiar la contraseña de desarrollador: Modificar DEVELOPER_PASSWORD
# - Para cambiar la duración de notificaciones: Modificar el parámetro duration
# ============================================================================

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
