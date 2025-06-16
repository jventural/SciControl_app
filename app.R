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

# Operador auxiliar
`%||%` <- function(a, b) if (is.null(a)) b else a

# CONFIGURACIÓN DE DROPBOX
DROPBOX_ACCESS_TOKEN <- "sl.u.AFw3SFPOo-GGDmQQdKguL7ubXk97PjeHIgn3iV-m8S0B_HG1zRLvV5kn5zBpJLu_196_NsAD6s-kxkfKdwvbewYphmVygAPiDA49HxcYGEZ2l679IVLsnsmfx7mpTrPRO7yLBVZju06lgBD1_nInt3Q4PReN7Mku1kzP-TcjgAXIv9zhfQTqlwLdOUIWzc_IA3wVR5WA3nfzM82GsBQqZ6oWM788ozblBCogJImmEX8YEOQ6PWwI5c9RQNDCX7wiXrXo6b5aoZ7C84HeOPFUBD7Y9CK0xhOk2BvpKDSGWPdX_oueN1A4lQ_SvAMxoM3cgXWWM9VsMQrSo7NfSSHBcMclUmstcHZnBfq-F4kofHlbgx8jdOkGYJLNUuqMYFk5vlVtMCD7uWDeuy73fIXf5Xz6WR5IY_xti4BVBcqMwzrAZCGTWAusxqIL0BqdVWwhEwmzZHaVamFEJUNkjYnWPayjMf5vR0x_zf5Gn5L5v3R5ysiUZNSc-uTOLmXum4Swv1a3EykyIam8rdK2z6eFionEZpQqqLKeeIo_kwJrw7O_bpijXJPcW5vDUyv7i85uDs1olTMBzfNCHRwtTySUc82mVqnmEQe1TAzQpIF7TiMmCIbaSQgJWRGIJ0rZTzaCrzXwa_KKPI3jQsSfB71AEgMb_m_WGDWMoHFh_ucRy1crnxJ_bvrspRSFFD7SCXePCQtoKJuWOHrRfo1qbrytw-ulU_2MYU_jv5AJ7V2csJFiqfpkyPwKVRv6QYBb_oHg719im0i_QFwHld86QC5vRFcaJNim4Y0tHj42WqoMQJ_7EA0-qBc7uqpaPZslr7Kt-V8E-1-tEGmtSTrkJcJQFDyW6n7Swkkt44IG3jDt-Ik2kcku5UJWMOLZtRlxg4Pi3OjkM48pR2mKkauDGdOCED92k5NeNdAQRAM2Lst1Hr9BHS_vcVnJsYLmC1QkRT6ASAAaMwnSLWSwDlzSR9o5Q2R6dwWUeP5LmdNq13rI335Cn_PI2MuWgijuhvREsjK8lbFfethU0SnYS1qm3w1G8fPRE6o8vCVlViOFM6U4nyJNDJsFM2Oo8uvnGitEfEkpC87R-0iI5rTadIggSfiGojTfZnR20t9RScClBfZKRt6iS01wfX4zgB0SX6xABjstjZt8hc8465WrCE7SdmG9IXIcawNKzA2mVjbCCStZYlumTHcXDzbwhiaO7YeZ2RpqaiQ94FXzM9cfVg5GuPwowJ7gjEjT-pqMccPxUYZDpKfa3tHsyquXFid2jkwPsL3GRqlWgH0ti5-aQt0qeY6Z7i2ruxyX2R08dA0_YDhf2LBVe_mVixxGWHLTbKx9MxC5-b8"

# Función para verificar configuración de Dropbox
check_dropbox_config <- function() {
  if (DROPBOX_ACCESS_TOKEN == "tu_dropbox_access_token_aqui" ||
      DROPBOX_ACCESS_TOKEN == "") {
    return(FALSE)
  }
  return(TRUE)
}

# Función para subir archivo a Dropbox
upload_to_dropbox <- function(file_path, file_name, folder_path = NULL) {
  tryCatch({
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_base <- tools::file_path_sans_ext(file_name)
    file_ext <- tools::file_ext(file_name)
    unique_name <- paste0(file_base, "_", timestamp, ".", file_ext)

    if (!is.null(folder_path)) {
      dropbox_path <- paste0("/", folder_path, "/", unique_name)
    } else {
      dropbox_path <- paste0("/", unique_name)
    }

    file_content <- readBin(file_path, "raw", file.info(file_path)$size)

    response <- POST(
      url = "https://content.dropboxapi.com/2/files/upload",
      add_headers(
        "Authorization" = paste("Bearer", DROPBOX_ACCESS_TOKEN),
        "Dropbox-API-Arg" = jsonlite::toJSON(list(
          path = dropbox_path,
          mode = "add",
          autorename = TRUE
        ), auto_unbox = TRUE),
        "Content-Type" = "application/octet-stream"
      ),
      body = file_content
    )

    if (response$status_code == 200) {
      result <- content(response, "parsed")
      share_response <- POST(
        url = "https://api.dropboxapi.com/2/sharing/create_shared_link_with_settings",
        add_headers(
          "Authorization" = paste("Bearer", DROPBOX_ACCESS_TOKEN),
          "Content-Type" = "application/json"
        ),
        body = jsonlite::toJSON(list(
          path = result$path_display,
          settings = list(requested_visibility = "public")
        ), auto_unbox = TRUE)
      )

      share_url <- ""
      if (share_response$status_code == 200) {
        share_result <- content(share_response, "parsed")
        share_url <- gsub("\\?dl=0", "?dl=1", share_result$url)
      }

      return(list(
        success = TRUE,
        path = result$path_display,
        name = result$name,
        size = result$size,
        url = share_url,
        original_name = file_name
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

# Función para listar archivos de Dropbox por carpeta
list_dropbox_files <- function(folder_path = NULL) {
  tryCatch({
    search_path <- if (is.null(folder_path)) "" else paste0("/", folder_path)

    response <- POST(
      url = "https://api.dropboxapi.com/2/files/list_folder",
      add_headers(
        "Authorization" = paste("Bearer", DROPBOX_ACCESS_TOKEN),
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

# Función para eliminar archivo de Dropbox
delete_dropbox_file <- function(file_path) {
  tryCatch({
    response <- POST(
      url = "https://api.dropboxapi.com/2/files/delete_v2",
      add_headers(
        "Authorization" = paste("Bearer", DROPBOX_ACCESS_TOKEN),
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

# NUEVAS FUNCIONES PARA ALMACENAMIENTO PERSISTENTE EN DROPBOX

# Función para subir datos a Dropbox
save_data_to_dropbox <- function(project_data) {
  tryCatch({
    # Crear archivo temporal
    temp_file <- tempfile(fileext = ".xlsx")
    writexl::write_xlsx(project_data, temp_file)

    # Subir a Dropbox con backup por timestamp
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

    # Archivo principal (siempre se sobrescribe)
    main_result <- upload_to_dropbox(temp_file, "project_data.xlsx", "scicontrol/data")

    # Backup con timestamp (para historial)
    backup_result <- upload_to_dropbox(temp_file,
                                       paste0("project_data_backup_", timestamp, ".xlsx"),
                                       "scicontrol/backups")

    # Limpiar archivo temporal
    unlink(temp_file)

    return(list(
      success = main_result$success && backup_result$success,
      main_path = if(main_result$success) main_result$path else NULL,
      backup_path = if(backup_result$success) backup_result$path else NULL,
      error = if(!main_result$success) main_result$error else if(!backup_result$success) backup_result$error else NULL
    ))
  }, error = function(e) {
    return(list(success = FALSE, error = as.character(e)))
  })
}

# Función para cargar datos desde Dropbox
load_data_from_dropbox <- function() {
  tryCatch({
    # Intentar descargar el archivo principal
    response <- GET(
      url = "https://content.dropboxapi.com/2/files/download",
      add_headers(
        "Authorization" = paste("Bearer", DROPBOX_ACCESS_TOKEN),
        "Dropbox-API-Arg" = jsonlite::toJSON(list(
          path = "/scicontrol/data/project_data.xlsx"
        ), auto_unbox = TRUE)
      )
    )

    if (response$status_code == 200) {
      # Crear archivo temporal para descargar
      temp_file <- tempfile(fileext = ".xlsx")
      writeBin(content(response, "raw"), temp_file)

      # Leer datos
      project_data <- read_excel(temp_file)

      # Limpiar archivo temporal
      unlink(temp_file)

      # Asegurar estructura correcta
      required_columns <- c("Nombre", "Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                            "Revista", "Cuartil", "Estado", "Grupo", "Progreso",
                            "Fecha_Aceptado", "Fecha_Publicado", "Linea_Investigacion",
                            "Observaciones")

      # Agregar columnas faltantes
      missing_columns <- setdiff(required_columns, colnames(project_data))
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          project_data[[col]] <- NA
        }
      }

      # Normalizar tipos de datos
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

      # Convertir a data.frame estándar
      project_data <- as.data.frame(project_data, stringsAsFactors = FALSE)

      return(project_data)

    } else if (response$status_code == 409) {
      # Archivo no existe, crear estructura inicial
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

# Función para crear estructura inicial
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
    stringsAsFactors = FALSE
  )
  return(project_data)
}

# Aumentar el límite de tamaño de archivo a 100 MB
options(shiny.maxRequestSize = 100*1024^2)

# Definir carpeta local para el archivo de datos (backup local)
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
data_file <- file.path(data_dir, "project_data.xlsx")

# Función para sanitizar el nombre del proyecto
sanitize_project_name <- function(project_name) {
  name <- gsub("[:/\\\\?<>\\|*\"'\\s]", "_", project_name)
  name <- gsub("_{2,}", "_", name)
  name <- trimws(name, whitespace = "_")
  if (nchar(name) > 30) {
    name <- substr(name, 1, 30)
  }
  return(name)
}

# FUNCIÓN MODIFICADA PARA CARGAR DATOS (CON DROPBOX PRIORITARIO)
load_project_data <- function() {
  if (!STORAGE_CONFIGURED) {
    warning("Dropbox no configurado, usando estructura inicial")
    return(create_initial_data_structure())
  }

  # Intentar cargar desde Dropbox primero
  dropbox_data <- load_data_from_dropbox()

  # Si hay datos en Dropbox, usarlos
  if (nrow(dropbox_data) > 0) {
    return(dropbox_data)
  }

  # Si no hay datos en Dropbox, verificar archivo local (para compatibilidad)
  if (file.exists(data_file)) {
    local_data <- read_excel(data_file)

    # Si hay datos locales, subirlos a Dropbox y usarlos
    if (nrow(local_data) > 0) {
      upload_result <- save_data_to_dropbox(local_data)
      if (upload_result$success) {
        message("Datos locales migrados a Dropbox exitosamente")
      }
      return(local_data)
    }
  }

  # Si no hay datos en ningún lado, crear estructura inicial
  return(create_initial_data_structure())
}

# FUNCIÓN MODIFICADA PARA GUARDAR DATOS (CON DROPBOX PRIORITARIO)
save_project_data <- function(project_data) {
  if (!STORAGE_CONFIGURED) {
    # Fallback: guardar solo localmente si Dropbox no está configurado
    writexl::write_xlsx(project_data, data_file)
    warning("Dropbox no configurado, guardando solo localmente")
    return(list(success = TRUE, message = "Guardado localmente"))
  }

  # Guardar en Dropbox (principal)
  dropbox_result <- save_data_to_dropbox(project_data)

  # También guardar localmente como backup
  tryCatch({
    writexl::write_xlsx(project_data, data_file)
  }, error = function(e) {
    warning("Error al guardar localmente: ", e$message)
  })

  return(dropbox_result)
}

# Verificar configuración al inicio
STORAGE_CONFIGURED <- check_dropbox_config()

# FUNCIÓN MEJORADA PARA VALIDAR FECHAS CON MANEJO DE TIPOS
validate_dates <- function(data) {
  # Función auxiliar para convertir fechas de manera segura
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

  # Convertir todas las fechas de manera segura
  data$Fecha_Inicio_Date <- sapply(data$Fecha_Inicio, safe_date_convert)
  data$Fecha_Envio_Date <- sapply(data$Fecha_Envio, safe_date_convert)
  data$Fecha_Respuesta_Date <- sapply(data$Fecha_Respuesta, safe_date_convert)
  data$Fecha_Aceptado_Date <- sapply(data$Fecha_Aceptado, safe_date_convert)
  data$Fecha_Publicado_Date <- sapply(data$Fecha_Publicado, safe_date_convert)

  # Convertir de numeric a Date (resultado de sapply)
  data$Fecha_Inicio_Date <- as.Date(data$Fecha_Inicio_Date, origin = "1970-01-01")
  data$Fecha_Envio_Date <- as.Date(data$Fecha_Envio_Date, origin = "1970-01-01")
  data$Fecha_Respuesta_Date <- as.Date(data$Fecha_Respuesta_Date, origin = "1970-01-01")
  data$Fecha_Aceptado_Date <- as.Date(data$Fecha_Aceptado_Date, origin = "1970-01-01")
  data$Fecha_Publicado_Date <- as.Date(data$Fecha_Publicado_Date, origin = "1970-01-01")

  # Crear columna de alertas
  data$Alertas <- ""

  for (i in 1:nrow(data)) {
    alertas <- c()

    # Validar orden de fechas
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

    # Verificar fechas futuras
    today <- Sys.Date()
    if (!is.na(data$Fecha_Envio_Date[i]) && data$Fecha_Envio_Date[i] > today) {
      alertas <- c(alertas, "📅 Fecha de envío en el futuro")
    }

    data$Alertas[i] <- paste(alertas, collapse = "; ")
  }

  return(data)
}

# FUNCIÓN MEJORADA PARA CALCULAR DÍAS CON TIPOS CONSISTENTES
calculate_days_improved <- function(data) {
  data <- validate_dates(data)

  # Calcular días usando las fechas convertidas
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

  # Remover columnas temporales de fechas
  cols_to_remove <- c("Fecha_Inicio_Date", "Fecha_Envio_Date", "Fecha_Respuesta_Date",
                      "Fecha_Aceptado_Date", "Fecha_Publicado_Date")
  data <- data[, !colnames(data) %in% cols_to_remove]

  return(data)
}

# UI MEJORADO CON SINCRONIZACIÓN
ui <- dashboardPage(
  dashboardHeader(
    title = "SciControl",
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
      menuItem("Agregar Proyecto", tabName = "agregar", icon = icon("plus")),
      menuItem("Ver Proyectos", tabName = "ver", icon = icon("table")),
      menuItem("Análisis de Tiempos", tabName = "dias", icon = icon("clock")),
      menuItem("Dashboard Visual", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Subida de Evidencias", tabName = "evidencias", icon = icon("upload")),
      menuItem("Ver Archivos Subidos", tabName = "ver_evidencias", icon = icon("folder-open")),
      menuItem("Sincronización", tabName = "sync", icon = icon("sync")),
      menuItem("Descargar Datos", tabName = "descargar", icon = icon("download")),
      menuItem("Importar Datos", tabName = "importar", icon = icon("file-upload"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
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

    /* ESTILOS ESPECÍFICOS PARA LA TABLA DE PROYECTOS */
    .dataTables_wrapper .dataTables_scroll {
      overflow-x: auto;
    }

    /* MEJORAR APARIENCIA DE LAS BARRAS DE PROGRESO */
    .progress-bar-container {
      background-color: #f3f3f3;
      border-radius: 5px;
      overflow: hidden;
    }

    .progress-bar {
      background-color: #e74c3c;
      color: white;
      text-align: center;
      padding: 5px 0;
      transition: width 0.3s ease;
    }
  "))
    )
    ,
    tabItems(
      # Tab: Agregar Proyecto
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
      )
      ,

      # Tab: Análisis de Tiempos MEJORADO
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

      # Tab: NUEVO Dashboard Visual
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
      )
    )
  )
)

# SERVER MEJORADO
server <- function(input, output, session) {
Sys.setenv(TZ = "America/Lima")



  # Variables reactivas
  project_data <- reactiveVal(load_project_data())
  files_refresh <- reactiveVal(0)

  progress_map <- list(
    "Introducción" = 10, "Método" = 30, "Resultados" = 50,
    "Discusión" = 70, "Enviado" = 80, "Revisión" = 90,
    "Aceptado" = 95, "Publicado" = 100
  )

  # Datos mejorados con validación
  days_data_improved <- reactive({
    data <- project_data()
    if (nrow(data) == 0) return(data)
    calculate_days_improved(data)
  })

  # Indicador de estado de almacenamiento en el header
  output$storage_status_display <- renderText({
    if (STORAGE_CONFIGURED) {
      "🟢 Dropbox Conectado"
    } else {
      "🔴 Sin Conexión"
    }
  })

  # Estado de sincronización
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
        tags$li(paste("☁️ Dropbox:", if(STORAGE_CONFIGURED) "✅ Configurado" else "❌ No configurado")),
        tags$li(paste("🔄 Auto-sincronización:", if(STORAGE_CONFIGURED) "✅ Activa" else "❌ Inactiva"))
      ),
      if (!STORAGE_CONFIGURED) {
        div(class = "alert alert-warning",
            "⚠️ Configure Dropbox para habilitar la sincronización automática y evitar pérdida de datos al republicar."
        )
      } else {
        div(class = "alert alert-success",
            "✅ Sistema configurado correctamente. Los datos se sincronizan automáticamente con Dropbox."
        )
      }
    )
  })

  # Validación de fechas en tiempo real
  observeEvent(c(input$start_date, input$send_date, input$response_date,
                 input$acceptance_date, input$publication_date), {

                   # VALIDACIÓN ROBUSTA - COMPROBAR QUE LOS INPUTS EXISTAN Y TENGAN VALORES
                   if (is.null(input$start_date) && is.null(input$send_date) &&
                       is.null(input$response_date) && is.null(input$acceptance_date) &&
                       is.null(input$publication_date)) {
                     return()
                   }

                   alerts <- c()

                   # VALIDAR FECHA DE INICIO VS ENVÍO
                   if (!is.null(input$start_date) && !is.null(input$send_date) &&
                       length(input$start_date) > 0 && length(input$send_date) > 0 &&
                       !is.na(input$start_date) && !is.na(input$send_date)) {

                     tryCatch({
                       if (as.Date(input$send_date) < as.Date(input$start_date)) {
                         alerts <- c(alerts, "⚠️ La fecha de envío debe ser posterior a la fecha de inicio")
                       }
                     }, error = function(e) {
                       # Ignorar errores de conversión de fecha
                     })
                   }

                   # VALIDAR FECHA DE ENVÍO VS RESPUESTA
                   if (!is.null(input$send_date) && !is.null(input$response_date) &&
                       length(input$send_date) > 0 && length(input$response_date) > 0 &&
                       !is.na(input$send_date) && !is.na(input$response_date)) {

                     tryCatch({
                       if (as.Date(input$response_date) < as.Date(input$send_date)) {
                         alerts <- c(alerts, "⚠️ La fecha de respuesta debe ser posterior a la fecha de envío")
                       }
                     }, error = function(e) {
                       # Ignorar errores de conversión de fecha
                     })
                   }

                   # VALIDAR FECHA DE RESPUESTA VS ACEPTACIÓN
                   if (!is.null(input$response_date) && !is.null(input$acceptance_date) &&
                       length(input$response_date) > 0 && length(input$acceptance_date) > 0 &&
                       !is.na(input$response_date) && !is.na(input$acceptance_date)) {

                     tryCatch({
                       if (as.Date(input$acceptance_date) < as.Date(input$response_date)) {
                         alerts <- c(alerts, "⚠️ La fecha de aceptación debe ser posterior a la fecha de respuesta")
                       }
                     }, error = function(e) {
                       # Ignorar errores de conversión de fecha
                     })
                   }

                   # VALIDAR FECHA DE ACEPTACIÓN VS PUBLICACIÓN
                   if (!is.null(input$acceptance_date) && !is.null(input$publication_date) &&
                       length(input$acceptance_date) > 0 && length(input$publication_date) > 0 &&
                       !is.na(input$acceptance_date) && !is.na(input$publication_date)) {

                     tryCatch({
                       if (as.Date(input$publication_date) < as.Date(input$acceptance_date)) {
                         alerts <- c(alerts, "⚠️ La fecha de publicación debe ser posterior a la fecha de aceptación")
                       }
                     }, error = function(e) {
                       # Ignorar errores de conversión de fecha
                     })
                   }

                   # RENDERIZAR ALERTAS SOLO SI HAY ALGO QUE MOSTRAR
                   output$date_validation_alerts <- renderUI({
                     if (length(alerts) > 0) {
                       div(class = "alert-box alert-warning",
                           HTML(paste(alerts, collapse = "<br>")))
                     } else {
                       NULL
                     }
                   })
                 }, ignoreNULL = FALSE, ignoreInit = FALSE)

  # Sistema de subida de archivos
  output$upload_system_status <- renderText({
    if (STORAGE_CONFIGURED) {
      "✅ Sistema listo para subir archivos a Dropbox"
    } else {
      "⚠️ Configure primero el Access Token de Dropbox"
    }
  })

  # Actualizar selectInputs dinámicamente
  observe({
    data <- project_data()

    # VALIDAR QUE data NO ESTÉ VACÍO
    if (is.null(data) || nrow(data) == 0) return()

    # VALIDAR QUE LA COLUMNA EXISTA
    if (!"Nombre" %in% colnames(data)) return()

    data$Nombre <- as.character(data$Nombre)
    display_names <- ifelse(is.na(data$Nombre) | data$Nombre == "", "(sin nombre)", data$Nombre)

    updateSelectInput(session, "delete_project", choices = display_names)
    updateSelectInput(session, "project_select", choices = data$Nombre)
    updateSelectInput(session, "project_view", choices = c("", data$Nombre))
  })


  # NUEVA TABLA DE MÉTRICAS RESUMIDAS
  output$metrics_summary <- renderUI({
    data <- days_data_improved()
    if (nrow(data) == 0) return(div("No hay datos disponibles"))

    # Calcular métricas
    total_projects <- nrow(data)
    published <- sum(data$Estado == "Publicado", na.rm = TRUE)
    accepted <- sum(data$Estado %in% c("Aceptado", "Publicado"), na.rm = TRUE)
    avg_review_time <- round(mean(data$Dias_Respuesta_Envio, na.rm = TRUE), 1)
    avg_acceptance_time <- round(mean(data$Dias_Aceptado_Envio, na.rm = TRUE), 1)

    # Calcular alertas
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

  # NUEVA TABLA DE ALERTAS DE VALIDACIÓN
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

  # TABLA MEJORADA DE ANÁLISIS DE DÍAS
  output$days_table_improved <- renderDT({
    data <- days_data_improved()
    if (nrow(data) == 0) return(datatable(data.frame(), options = list(pageLength = 10)))

    display_data <- data[, c("Nombre", "Revista", "Cuartil", "Estado",
                             "Dias_Envio_Inicio", "Dias_Respuesta_Envio",
                             "Dias_Aceptado_Respuesta", "Dias_Aceptado_Envio",
                             "Dias_Aceptado_Publicado")]

    # Renombrar columnas para mejor presentación
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

  # NUEVOS GRÁFICOS PARA EL DASHBOARD

  # Función auxiliar para gráficos vacíos
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

  # Gráfico de distribución de estados
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

  # Gráfico de tiempo por cuartil
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

  # Línea de tiempo de proyectos
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

  # Análisis de rendimiento
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

  # Tendencias mensuales
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

  # TABLA DE PROYECTOS CON ALERTAS
  output$project_table <- renderDT({
    cat("=== RENDERIZANDO TABLA COMPLETA ===\n")

    # Obtener datos validados
    data <- tryCatch({
      days_data_improved()
    }, error = function(e) {
      cat("Error en days_data_improved, usando datos básicos:", e$message, "\n")
      project_data()
    })

    if (nrow(data) == 0) {
      return(datatable(
        data.frame(Mensaje = "No hay proyectos para mostrar"),
        options = list(pageLength = 10),
        rownames = FALSE
      ))
    }

    cat("Procesando", nrow(data), "proyectos\n")

    # Crear barras de progreso
    data$Progreso_Barra <- sapply(data$Estado, function(estado) {
      p <- progress_map[[estado]]
      if (is.null(p)) p <- 0

      # Color según el progreso
      color <- if (p >= 90) "#28a745"      # Verde para casi completado
      else if (p >= 70) "#ffc107"  # Amarillo para en progreso avanzado
      else if (p >= 40) "#fd7e14"  # Naranja para progreso medio
      else "#dc3545"               # Rojo para inicio

      sprintf(
        '<div style="width:100%%; background-color:#f3f3f3; border-radius:5px; height:22px; border:1px solid #ddd;">
         <div style="width:%d%%; background-color:%s; color:white; text-align:center; line-height:22px; border-radius:4px; font-size:12px; font-weight:bold;">%d%%</div>
       </div>', p, color, p)
    })

    # Agregar indicadores de alerta si existen
    if ("Alertas" %in% colnames(data)) {
      data$Estado_Display <- ifelse(!is.na(data$Alertas) & data$Alertas != "",
                                    paste0(data$Estado, " ⚠️"),
                                    data$Estado)
    } else {
      data$Estado_Display <- data$Estado
    }

    # Seleccionar columnas para mostrar
    display_cols <- c("Nombre", "Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                      "Revista", "Cuartil", "Estado_Display", "Grupo", "Progreso_Barra",
                      "Fecha_Aceptado", "Fecha_Publicado", "Linea_Investigacion",
                      "Observaciones")

    # Verificar qué columnas existen
    available_cols <- intersect(display_cols, colnames(data))
    display_data <- data[, available_cols, drop = FALSE]

    # Renombrar columnas para mejor presentación
    col_names <- colnames(display_data)
    col_names[col_names == "Estado_Display"] <- "Estado"
    col_names[col_names == "Progreso_Barra"] <- "Progreso"
    col_names[col_names == "Linea_Investigacion"] <- "Línea de Investigación"
    colnames(display_data) <- col_names

    cat("Creando tabla con columnas:", paste(colnames(display_data), collapse = ", "), "\n")

    # Crear tabla
    dt <- datatable(
      display_data,
      escape = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = FALSE,
        dom = 'Bfrtip',
        columnDefs = list(
          list(width = '200px', targets = 0),     # Nombre
          list(width = '100px', targets = 1:3),   # Fechas
          list(width = '120px', targets = 4),     # Revista
          list(width = '60px', targets = 5),      # Cuartil
          list(width = '100px', targets = 6),     # Estado
          list(width = '120px', targets = 7),     # Grupo
          list(width = '150px', targets = 8),     # Progreso
          list(width = '100px', targets = 9:10),  # Fechas aceptado/publicado
          list(width = '150px', targets = 11)     # Línea investigación
        )
      ),
      rownames = FALSE,
      selection = 'single'
    )

    # Aplicar estilos condicionales si hay alertas
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

    cat("Tabla creada exitosamente\n")
    return(dt)
  })

  # Sincronización manual desde Dropbox
  observeEvent(input$sync_from_dropbox, {
    if (!STORAGE_CONFIGURED) {
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
        showNotification("Datos sincronizados desde Dropbox", type = "success")
      } else {
        output$sync_operation_status <- renderText("⚠️ No se encontraron datos en Dropbox.")
      }
    }, error = function(e) {
      output$sync_operation_status <- renderText(paste("❌ Error al sincronizar:", e$message))
    })
  })

  # Sincronización manual a Dropbox
  observeEvent(input$sync_to_dropbox, {
    if (!STORAGE_CONFIGURED) {
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
        showNotification("Datos sincronizados a Dropbox", type = "success")
      } else {
        output$sync_operation_status <- renderText(paste("❌ Error al sincronizar:", save_result$error))
      }
    }, error = function(e) {
      output$sync_operation_status <- renderText(paste("❌ Error al sincronizar:", e$message))
    })
  })

  # Crear backup manual
  observeEvent(input$manual_backup, {
    if (!STORAGE_CONFIGURED) {
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
        showNotification("Backup manual creado", type = "success")
      } else {
        output$sync_operation_status <- renderText(paste("❌ Error al crear backup:", backup_result$error))
      }
    }, error = function(e) {
      output$sync_operation_status <- renderText(paste("❌ Error al crear backup:", e$message))
    })
  })

  # Lista de archivos de backup
  output$backup_files_table <- renderDT({
    if (!STORAGE_CONFIGURED) {
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

    if (!STORAGE_CONFIGURED) {
      output$sync_operation_status <- renderText("❌ Dropbox no está configurado.")
      return()
    }

    tryCatch({
      # Obtener información del backup seleccionado
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

      # Guardar información del backup para usar en confirm_restore
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
      # Descargar el backup
      response <- GET(
        url = "https://content.dropboxapi.com/2/files/download",
        add_headers(
          "Authorization" = paste("Bearer", DROPBOX_ACCESS_TOKEN),
          "Dropbox-API-Arg" = jsonlite::toJSON(list(
            path = backup_to_restore
          ), auto_unbox = TRUE)
        )
      )

      if (response$status_code == 200) {
        # Crear archivo temporal
        temp_file <- tempfile(fileext = ".xlsx")
        writeBin(content(response, "raw"), temp_file)

        # Leer datos del backup
        restored_data <- read_excel(temp_file)
        unlink(temp_file)

        # Validar y normalizar datos
        required_columns <- c("Nombre", "Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta",
                              "Revista", "Cuartil", "Estado", "Grupo", "Progreso",
                              "Fecha_Aceptado", "Fecha_Publicado", "Linea_Investigacion",
                              "Observaciones")

        missing_columns <- setdiff(required_columns, colnames(restored_data))
        if (length(missing_columns) > 0) {
          for (col in missing_columns) {
            restored_data[[col]] <- NA
          }
        }

        # Normalizar tipos de datos
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

        # Actualizar datos en la aplicación
        project_data(restored_data)

        # Crear backup de los datos actuales antes de restaurar
        current_backup_result <- save_data_to_dropbox(restored_data)

        output$sync_operation_status <- renderText(
          paste("✅ Backup restaurado exitosamente.",
                "\n📊 Proyectos restaurados:", nrow(restored_data),
                "\n📁 Archivo restaurado:", basename(backup_to_restore),
                if(current_backup_result$success) paste("\n💾 Nuevo backup creado:", current_backup_result$backup_path) else "")
        )

        showNotification("Backup restaurado exitosamente", type = "success")

      } else {
        output$sync_operation_status <- renderText(paste("❌ Error al descargar backup. Código:", response$status_code))
      }

      # Limpiar variable temporal
      backup_to_restore <<- NULL

    }, error = function(e) {
      output$sync_operation_status <- renderText(paste("❌ Error al restaurar backup:", e$message))
      backup_to_restore <<- NULL
    })
  })

  # SUBIDA DE EVIDENCIAS A DROPBOX
  observeEvent(input$upload_btn, {
    req(input$file_upload, input$project_select, input$file_type)

    if (!STORAGE_CONFIGURED) {
      output$upload_status <- renderText("❌ Error: Configure primero el Access Token de Dropbox.")
      return()
    }

    project_name <- input$project_select
    file_type <- input$file_type
    file_info <- input$file_upload

    output$upload_status <- renderText("📤 Subiendo archivo a Dropbox...")

    tryCatch({
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
                "\nNombre:", result$original_name,
                "\nTamaño:", size_text,
                "\nCarpeta:", file_type,
                "\nProyecto:", project_name,
                "\nPath:", result$path,
                if (result$url != "") paste("\nEnlace:", result$url) else "")
        )
        files_refresh(isolate(files_refresh()) + 1)
      } else {
        output$upload_status <- renderText(paste("❌ Error al subir archivo:", result$error))
      }
    }, error = function(e) {
      output$upload_status <- renderText(paste("❌ Error inesperado:", e$message))
    })
  })

  # TABLA DE ARCHIVOS EN DROPBOX
  output$files_table <- renderDT({
    files_refresh()

    if (input$project_view == "" || !STORAGE_CONFIGURED) {
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

  # Actualizar lista de archivos
  observeEvent(input$refresh_files, {
    files_refresh(isolate(files_refresh()) + 1)
    showNotification("Lista de archivos actualizada", type = "message")
  })

  # Eliminar archivo de Dropbox
  observeEvent(input$delete_file, {
    sel <- input$files_table_rows_selected
    if (is.null(sel) || length(sel) == 0 || !STORAGE_CONFIGURED) {
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

  # SELECCIÓN DE PROYECTO PARA EDITAR
  observeEvent(input$project_table_rows_selected, {
    sel <- input$project_table_rows_selected

    if (!is.null(sel) && length(sel) > 0) {
      data <- project_data()

      # Validar que la selección sea válida
      if (sel <= nrow(data)) {
        proj <- data[sel, ]

        # Actualizar campos con validación segura
        updateTextInput(session, "project_name", value = as.character(proj$Nombre %||% ""))

        # Fechas con validación
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

        # Otros campos
        updateTextInput(session, "journal", value = as.character(proj$Revista %||% ""))
        updateSelectInput(session, "quartile", selected = as.character(proj$Cuartil %||% "Q1"))
        updateSelectInput(session, "status", selected = as.character(proj$Estado %||% "Introducción"))
        updateSelectInput(session, "group", selected = as.character(proj$Grupo %||% "Equipo de Investigación"))
        updateTextInput(session, "research_line", value = as.character(proj$Linea_Investigacion %||% ""))
        updateTextAreaInput(session, "observations", value = as.character(proj$Observaciones %||% ""))
      }
    }
  })

  # EVENTO PARA GUARDAR CAMBIOS CON DROPBOX
  observeEvent(input$save_changes, {
    # Validar entrada mínima
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

    # Preparar fechas como character de manera consistente
    fecha_inicio <- if (!is.null(input$start_date)) as.character(input$start_date) else NA_character_
    fecha_envio <- if (input$status %in% c("Enviado", "Revisión", "Aceptado", "Publicado") && !is.null(input$send_date)) {
      as.character(input$send_date)
    } else {
      NA_character_
    }
    fecha_resp <- if (input$status %in% c("Revisión","Aceptado","Publicado") && !is.null(input$response_date)) {
      as.character(input$response_date)
    } else {
      NA_character_
    }
    fecha_acc <- if (input$status %in% c("Aceptado","Publicado") && !is.null(input$acceptance_date)) {
      as.character(input$acceptance_date)
    } else {
      NA_character_
    }
    fecha_pub <- if (input$status == "Publicado" && !is.null(input$publication_date)) {
      as.character(input$publication_date)
    } else {
      NA_character_
    }

    prog <- progress_map[[input$status]]

    # Crear nueva fila con tipos consistentes
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
      stringsAsFactors = FALSE
    )

    # Validación de fechas antes de guardar
    alertas_validacion <- c()

    if (!is.na(fecha_inicio) && !is.na(fecha_envio)) {
      if (as.Date(fecha_envio) < as.Date(fecha_inicio)) {
        alertas_validacion <- c(alertas_validacion, "La fecha de envío es anterior a la fecha de inicio")
      }
    }

    if (!is.na(fecha_envio) && !is.na(fecha_resp)) {
      if (as.Date(fecha_resp) < as.Date(fecha_envio)) {
        alertas_validacion <- c(alertas_validacion, "La fecha de respuesta es anterior a la fecha de envío")
      }
    }

    # Mostrar advertencias pero permitir guardar
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
        # Actualizar proyecto existente
        data[idx, ] <- new_row
      } else {
        # Agregar nuevo proyecto
        data <- rbind(data, new_row)
      }

      # *** CAMBIO PRINCIPAL: Guardar en Dropbox ***
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

    # Preparar datos (mismo código que arriba)
    fecha_inicio <- if (!is.null(input$start_date)) as.character(input$start_date) else NA_character_
    fecha_envio <- if (input$status %in% c("Enviado", "Revisión", "Aceptado", "Publicado") && !is.null(input$send_date)) {
      as.character(input$send_date)
    } else {
      NA_character_
    }
    fecha_resp <- if (input$status %in% c("Revisión","Aceptado","Publicado") && !is.null(input$response_date)) {
      as.character(input$response_date)
    } else {
      NA_character_
    }
    fecha_acc <- if (input$status %in% c("Aceptado","Publicado") && !is.null(input$acceptance_date)) {
      as.character(input$acceptance_date)
    } else {
      NA_character_
    }
    fecha_pub <- if (input$status == "Publicado" && !is.null(input$publication_date)) {
      as.character(input$publication_date)
    } else {
      NA_character_
    }

    prog <- progress_map[[input$status]]

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

  # Limpiar campos
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

  # TABLA DE ARCHIVOS EN DROPBOX (VERSIÓN AJUSTADA)
  output$files_table <- renderDT({
    files_refresh()

    if (input$project_view == "" || !STORAGE_CONFIGURED) {
      empty <- data.frame(
        Nombre = character(),
        Fecha_Inicio = character(),
        Fecha_Envio = character(),
        Fecha_Respuesta = character(),
        Revista = character(),
        Cuartil = character(),
        Estado = character(),
        Grupo = character(),
        Progreso = character(),
        Fecha_Aceptado = character(),
        Fecha_Publicado = character(),
        Linea_Investigacion = character()
      )
      return(datatable(empty, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single"))
    }

    project_name <- input$project_view

    # Obtener datos del proyecto seleccionado
    data <- project_data()
    selected_project <- data[data$Nombre == project_name, ]

    if (nrow(selected_project) == 0) {
      empty <- data.frame(
        Nombre = character(),
        Fecha_Inicio = character(),
        Fecha_Envio = character(),
        Fecha_Respuesta = character(),
        Revista = character(),
        Cuartil = character(),
        Estado = character(),
        Grupo = character(),
        Progreso = character(),
        Fecha_Aceptado = character(),
        Fecha_Publicado = character(),
        Linea_Investigacion = character()
      )
      return(datatable(empty, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, selection = "single"))
    }

    # Crear la barra de progreso HTML
    progress_value <- as.numeric(selected_project$Progreso)
    if (is.na(progress_value)) progress_value <- 0

    progress_html <- sprintf(
      '<div style="width:100%%; background-color:#f3f3f3; border-radius:5px;">
       <div style="width:%d%%; background-color:#e74c3c; color:white; text-align:center; padding:5px 0; border-radius:5px;">%d%%</div>
     </div>',
      progress_value, progress_value
    )

    # Preparar datos para mostrar
    display_data <- data.frame(
      Nombre = as.character(selected_project$Nombre),
      Fecha_Inicio = as.character(selected_project$Fecha_Inicio %||% ""),
      Fecha_Envio = as.character(selected_project$Fecha_Envio %||% ""),
      Fecha_Respuesta = as.character(selected_project$Fecha_Respuesta %||% ""),
      Revista = as.character(selected_project$Revista %||% ""),
      Cuartil = as.character(selected_project$Cuartil %||% ""),
      Estado = as.character(selected_project$Estado %||% ""),
      Grupo = as.character(selected_project$Grupo %||% ""),
      Progreso = progress_html,
      Fecha_Aceptado = as.character(selected_project$Fecha_Aceptado %||% ""),
      Fecha_Publicado = as.character(selected_project$Fecha_Publicado %||% ""),
      Linea_Investigacion = as.character(selected_project$Linea_Investigacion %||% ""),
      Observaciones       = as.character(selected_project$Observaciones %||% ""),
      stringsAsFactors = FALSE
    )

    # Crear tabla con scroll horizontal para manejar muchas columnas
    return(datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = '200px', targets = 0), # Nombre
          list(width = '100px', targets = c(1,2,3,9,10)), # Fechas
          list(width = '150px', targets = 4), # Revista
          list(width = '80px', targets = 5), # Cuartil
          list(width = '100px', targets = 6), # Estado
          list(width = '120px', targets = 7), # Grupo
          list(width = '120px', targets = 8), # Progreso
          list(width = '150px', targets = 11),    # Línea investigación
          list(width = '300px', targets = 12)     # Observaciones
        )
      ),
      rownames = FALSE,
      selection = "single",
      escape = FALSE # Importante para que se renderice el HTML del progreso
    ))
  })

  # Eliminar proyecto
  observeEvent(input$delete_button, {
    req(input$delete_project)
    data <- project_data()
    if (input$delete_project == "(sin nombre)") {
      data <- data[!(is.na(data$Nombre) | data$Nombre==""),]
    } else {
      data <- data[data$Nombre != input$delete_project,]
    }

    # Guardar en Dropbox después de eliminar
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

  # Descargar datos
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

  # EVENTO PARA IMPORTAR EXCEL CON DROPBOX
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
                            "Observaciones")

      if (!"Nombre" %in% colnames(imported_data)) {
        output$import_status <- renderText("❌ El archivo debe contener al menos una columna 'Nombre'.")
        return()
      }

      # Agregar columnas faltantes
      missing_columns <- setdiff(required_columns, colnames(imported_data))
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          imported_data[[col]] <- NA
        }
      }

      # NORMALIZAR TIPOS DE DATOS DESPUÉS DE IMPORTAR
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

      # Asegurar otros tipos
      if ("Progreso" %in% colnames(imported_data)) {
        imported_data$Progreso <- as.numeric(imported_data$Progreso)
      }

      # Convertir a data.frame estándar
      imported_data <- as.data.frame(imported_data[, required_columns], stringsAsFactors = FALSE)

      # *** CAMBIO PRINCIPAL: Guardar en Dropbox ***
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

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
