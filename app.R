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

# CONFIGURACIÓN DE DROPBOX
# 1. Ve a https://www.dropbox.com/developers/apps
# 2. Crea una nueva app con "Scoped access" y "Full Dropbox"
# 3. Obtén tu Access Token desde la sección "OAuth 2" -> "Generated access token"
DROPBOX_ACCESS_TOKEN <- "sl.u.AFyq3zXGMyjN83cQGctTJlUFmaElmzxhsMIxVq8ueSkh_vLq0jeClobsSI68WZbXT7gPzqei-SvwSL_7NQWxTpTsFG7eKru4l1S06fitEJOwGlEJ41K2nluJiG_RItjftSrcKTxR-S3ARU-NQcNOrUFjx5mH5Vj4sK8wFrsdU6vsISxghoOksQ9-WLd0I9HqezZjtqaZ7cBj7XymN3LXtqrAMCyEEt0tGOWyO8klv2MOaA4VmDmZEwFl4IkDPVBxArTHavXyTAzXFowEcDGrx5eoqjj8ZxRnZ-O5MZUsHxuJolXY-GiBNYFMLRgaYZ8D7egdVF91866cA9EBWdleqzqf1bCjHZtTvmti_oTDn4SStQkyM-XL4j-lZM8xbz6GNLYkmy2AdAS2325sK6swHp72m98BL5B8TZKMgFRTd_SeSPV-svGFzRviAWxMuiYOUctTynmPdwbnrs_SCTIuTd9mb89HGu-_q5Z9EUR0PlO7Bb9RYdQ0HyVhN52KYoiVls-AgOhyr81uyJqJ73y1v62yasWw7urmr-1xHQltotMH76WYCr_CEWydyhPLz8ONPJi6HitETCvvvhP-0ogQbzRgvy8wLHa6ryiVtZfYepkMZ6meYBgbL8LeUa_0X5MI4eGJz5F2Dqt6dI_FHHCQoGc_gGxN45tLjBpsRc5DbvQJobo0S-UD0tbYm4PN2namTgoaF9c9rhj1jtNu-qKbB7dp1FQn56yEQ8jt4LX7XJrXy3hk5Mo0VzYQlGSVVF62hDZ5b0tdEX6g9Xj3SpP8teH9veVMFd4IigiUdNnuaeoHcWDlvbiI-eqJyN4oz4-sDvPmXxEQQZpfAdc9Rff3c_MdZ4LNOMUJZ6ePWUUYpqzHRsNYFna2gOcnsX0Q-fduvgN80ZrQOWdbQpzsCQKXmqP6zyDOmzIjVKg_sAqNyTmsYkfizng6oc9UG90YX7pRVdkPGwT0BYRumVDGI3gCziFtCk0I6TrQL5hSGAXA_TF6j1EO6OEve1xkxXxOnunqPgkkizlRY9Lp3p2BlW7_d36mZD2p8VaUE4Zm841AvC7pdwFgRhVdVW9BkY4_8l0d6MGByalwJSNsyg8LdLuph_zG-mUDYVxuiyg0JK3aF43EnrS1d6wnbAt0ZGIpaDNjoaALHp2k5R0gel0yo34EGWMIS3IY__4Oyo06iLF4a_GvCqBn2sIghFuUFpvR9s8GVAEInpMZ_3q8BZ4BBdloH3k0Yk0ZdjAVQLe9gwjkgNqWgtkaCnnNaz2HgMrqi1Rto2dTAEzCADw1jf9mTlw7JyuwDGfiV_sEQ3Bt83kKpx0iRg"

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
    # Crear path único
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_base <- tools::file_path_sans_ext(file_name)
    file_ext <- tools::file_ext(file_name)

    unique_name <- paste0(file_base, "_", timestamp, ".", file_ext)

    # Construir path completo en Dropbox
    if (!is.null(folder_path)) {
      dropbox_path <- paste0("/", folder_path, "/", unique_name)
    } else {
      dropbox_path <- paste0("/", unique_name)
    }

    # Leer el archivo
    file_content <- readBin(file_path, "raw", file.info(file_path)$size)

    # Subir archivo usando Dropbox API v2
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

      # Crear enlace compartido
      share_response <- POST(
        url = "https://api.dropboxapi.com/2/sharing/create_shared_link_with_settings",
        add_headers(
          "Authorization" = paste("Bearer", DROPBOX_ACCESS_TOKEN),
          "Content-Type" = "application/json"
        ),
        body = jsonlite::toJSON(list(
          path = result$path_display,
          settings = list(
            requested_visibility = "public"
          )
        ), auto_unbox = TRUE)
      )

      share_url <- ""
      if (share_response$status_code == 200) {
        share_result <- content(share_response, "parsed")
        # Convertir el enlace de vista a enlace directo
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
    return(list(
      success = FALSE,
      error = as.character(e)
    ))
  })
}

# Función para listar archivos de Dropbox por carpeta
list_dropbox_files <- function(folder_path = NULL) {
  tryCatch({
    # Path base para buscar
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
        # Filtrar solo archivos (no carpetas)
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
      body = jsonlite::toJSON(list(
        path = file_path
      ), auto_unbox = TRUE)
    )

    return(response$status_code == 200)
  }, error = function(e) {
    warning("Error al eliminar archivo: ", e$message)
    return(FALSE)
  })
}

# Función para obtener enlace de descarga/vista
get_dropbox_file_link <- function(file_path) {
  tryCatch({
    response <- POST(
      url = "https://api.dropboxapi.com/2/sharing/create_shared_link_with_settings",
      add_headers(
        "Authorization" = paste("Bearer", DROPBOX_ACCESS_TOKEN),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(list(
        path = file_path,
        settings = list(
          requested_visibility = "public"
        )
      ), auto_unbox = TRUE)
    )

    if (response$status_code == 200) {
      result <- content(response, "parsed")
      return(result$url)
    }
    return("")
  }, error = function(e) {
    return("")
  })
}

# Aumentar el límite de tamaño de archivo a 100 MB
options(shiny.maxRequestSize = 100*1024^2)

# Definir carpeta local para el archivo de datos
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

# Función para cargar los datos del proyecto
load_project_data <- function() {
  if (file.exists(data_file)) {
    project_data <- read_excel(data_file)
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
  } else {
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
    writexl::write_xlsx(project_data, data_file)
  }
  return(project_data)
}

# Función para guardar los datos
save_project_data <- function(project_data) {
  writexl::write_xlsx(project_data, data_file)
}

# Verificar configuración al inicio
STORAGE_CONFIGURED <- check_dropbox_config()

# Interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(title = "SciControl - Dropbox Storage"),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      menuItem("Agregar Proyecto", tabName = "agregar", icon = icon("plus")),
      menuItem("Ver Proyectos", tabName = "ver", icon = icon("table")),
      menuItem("Cálculo de Días", tabName = "dias", icon = icon("clock")),
      menuItem("Subida de Evidencias", tabName = "evidencias", icon = icon("upload")),
      menuItem("Ver Archivos Subidos", tabName = "ver_evidencias", icon = icon("folder-open")),
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
        .config-box { padding: 15px; margin: 10px 0; border-radius: 5px; background: #f8f9fa; border-left: 4px solid #007bff; }
        .success-box { background: #d4edda; border-left-color: #28a745; }
        .error-box { background: #f8d7da; border-left-color: #dc3545; }
      "))
    ),
    tabItems(
      # Tab: Agregar Proyecto (sin cambios)
      tabItem(tabName = "agregar",
              fluidRow(
                box(title = "Agregar o Actualizar Proyecto", width = 12, status = "primary",
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

      # Tab: Ver Proyectos (sin cambios)
      tabItem(tabName = "ver",
              fluidRow(
                box(title = "Proyectos Actuales", width = 12, DTOutput("project_table"))
              ),
              fluidRow(
                box(title = "Eliminar Proyecto", width = 12, status = "danger",
                    selectInput("delete_project", "Seleccione un proyecto para eliminar", choices = NULL),
                    actionButton("delete_button", "Eliminar Proyecto", class = "btn-danger")
                )
              )
      ),

      # Tab: Cálculo de Días (sin cambios)
      tabItem(tabName = "dias",
              fluidRow(
                box(title = "Cálculo de Días Entre Fechas", width = 12, DTOutput("days_table"))
              )
      ),

      # Tab: Subida de Evidencias
      tabItem(tabName = "evidencias",
              fluidRow(
                box(title = "Subir Evidencias para Proyectos", width = 12, status = "primary",
                    div(id = "upload-status-indicator",
                        textOutput("upload_system_status")
                    ),
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

      # Tab: Descargar Datos (sin cambios)
      tabItem(tabName = "descargar",
              fluidRow(
                box(title = "Descargar Datos", width = 12, status = "primary",
                    p("Descarga los datos del proyecto en formato Excel."),
                    p(em("Nota: Los archivos están almacenados en Dropbox y no se incluyen en esta descarga.")),
                    downloadButton("download_data", "Descargar Datos Excel", class = "btn-primary")
                )
              )
      ),

      # Tab: Importar Datos (sin cambios)
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

server <- function(input, output, session) {

  # Variables reactivas
  project_data <- reactiveVal(load_project_data())
  files_refresh <- reactiveVal(0)

  progress_map <- list(
    "Introducción" = 10,
    "Método" = 30,
    "Resultados" = 50,
    "Discusión" = 70,
    "Enviado" = 80,
    "Revisión" = 90,
    "Aceptado" = 95,
    "Publicado" = 100
  )

  # Estado para subida de archivos
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
    data$Nombre <- as.character(data$Nombre)
    display_names <- ifelse(is.na(data$Nombre) | data$Nombre == "",
                            "(sin nombre)",
                            data$Nombre)
    updateSelectInput(session, "delete_project", choices = display_names)
    updateSelectInput(session, "project_select", choices = data$Nombre)
    updateSelectInput(session, "project_view", choices = c("", data$Nombre))
  })

  # Subida de evidencias a Dropbox
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
      # Crear path de carpeta: scicontrol/proyecto/tipo_archivo
      sanitized_project <- sanitize_project_name(project_name)
      folder_path <- paste0("scicontrol/", sanitized_project, "/", file_type)

      # Subir archivo
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

  # Tabla de archivos en Dropbox
  output$files_table <- renderDT({
    files_refresh()

    if (input$project_view == "" || !STORAGE_CONFIGURED) {
      empty <- data.frame(Archivo = character(), Carpeta = character(), Tamaño = character(), Fecha = character())
      return(datatable(empty, options = list(pageLength = 10), rownames = FALSE, selection = "single"))
    }

    project_name <- input$project_view
    sanitized_name <- sanitize_project_name(project_name)

    tryCatch({
      # Buscar archivos del proyecto
      folder_prefix <- paste0("scicontrol/", sanitized_name)
      files <- list_dropbox_files(folder_prefix)

      if (nrow(files) == 0) {
        empty <- data.frame(Archivo = character(), Carpeta = character(), Tamaño = character(), Fecha = character())
        return(datatable(empty, options = list(pageLength = 10), rownames = FALSE, selection = "single"))
      }

      # Procesar datos para mostrar
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

      files$Fecha <- format(as.POSIXct(files$modified, format="%Y-%m-%dT%H:%M:%SZ"), "%Y-%m-%d %H:%M")

      df <- data.frame(
        Archivo = files$name,
        Carpeta = files$Carpeta,
        Tamaño = files$Tamaño,
        Fecha = files$Fecha,
        Path = files$path,
        stringsAsFactors = FALSE
      )

      return(datatable(df[, 1:4], options = list(pageLength = 10), rownames = FALSE, selection = "single"))
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

  # [Resto del código del servidor sin cambios - tabla de proyectos, días, etc.]

  # Tabla de proyectos
  output$project_table <- renderDT({
    data <- project_data()
    data$Progreso <- sapply(data$Estado, function(estado) {
      p <- progress_map[[estado]]
      sprintf(
        '<div style="width:100%%; background-color:#f3f3f3; border-radius:5px;">
           <div style="width:%d%%; background-color:#e74c3c; color:white; text-align:center; padding:5px 0; border-radius:5px;">%d%%</div>
         </div>', p, p)
    })
    datatable(data, escape = FALSE, options = list(pageLength = 10), rownames = FALSE)
  })

  # Cálculo de días
  days_data <- reactive({
    data <- project_data()
    data$Dias_Envio_Inicio <- ifelse(!is.na(data$Fecha_Inicio) & !is.na(data$Fecha_Envio),
                                     as.numeric(difftime(as.Date(data$Fecha_Envio), as.Date(data$Fecha_Inicio), units = "days")), NA)
    data$Dias_Respuesta_Envio <- ifelse(!is.na(data$Fecha_Envio) & !is.na(data$Fecha_Respuesta),
                                        as.numeric(difftime(as.Date(data$Fecha_Respuesta), as.Date(data$Fecha_Envio), units = "days")), NA)
    data$Dias_Aceptado_Respuesta <- ifelse(!is.na(data$Fecha_Respuesta) & !is.na(data$Fecha_Aceptado),
                                           as.numeric(difftime(as.Date(data$Fecha_Aceptado), as.Date(data$Fecha_Respuesta), units = "days")), NA)
    data$Dias_Aceptado_Envio <- ifelse(data$Estado %in% c("Aceptado","Publicado") & !is.na(data$Fecha_Aceptado) & !is.na(data$Fecha_Envio),
                                       as.numeric(difftime(as.Date(data$Fecha_Aceptado), as.Date(data$Fecha_Envio), units = "days")), NA)
    data$Dias_Aceptado_Publicado <- ifelse(data$Estado=="Publicado" & !is.na(data$Fecha_Publicado)&!is.na(data$Fecha_Aceptado),
                                           as.numeric(difftime(as.Date(data$Fecha_Publicado), as.Date(data$Fecha_Aceptado), units = "days")), NA)
    data
  })

  output$days_table <- renderDT({
    d <- days_data()
    datatable(d[, c("Nombre","Revista","Cuartil",
                    "Dias_Envio_Inicio","Dias_Respuesta_Envio",
                    "Dias_Aceptado_Respuesta","Dias_Aceptado_Envio",
                    "Dias_Aceptado_Publicado")],
              options = list(pageLength = 10), rownames = FALSE)
  })

  # Selección de proyecto para editar
  observeEvent(input$project_table_rows_selected, {
    sel <- input$project_table_rows_selected
    if (!is.null(sel)) {
      proj <- project_data()[sel, ]
      updateTextInput(session, "project_name", value = proj$Nombre)
      updateDateInput(session, "start_date", value = as.Date(proj$Fecha_Inicio))
      updateDateInput(session, "send_date", value = as.Date(proj$Fecha_Envio))
      updateDateInput(session, "response_date", value = as.Date(proj$Fecha_Respuesta))
      updateTextInput(session, "journal", value = proj$Revista)
      updateSelectInput(session, "quartile", selected = proj$Cuartil)
      updateSelectInput(session, "status", selected = proj$Estado)
      updateSelectInput(session, "group", selected = proj$Grupo)
      updateDateInput(session, "acceptance_date", value = as.Date(proj$Fecha_Aceptado))
      updateDateInput(session, "publication_date", value = as.Date(proj$Fecha_Publicado))
      updateTextInput(session, "research_line", value = proj$Linea_Investigacion)
      updateTextAreaInput(session, "observations", value = proj$Observaciones)
    }
  })

  # Guardar o actualizar proyecto
  observeEvent(input$save_changes, {
    data <- project_data()
    idx <- which(data$Nombre == input$project_name)
    fecha_resp <- if (input$status %in% c("Revisión","Aceptado","Publicado")) as.character(input$response_date) else NA
    fecha_acc <- if (input$status %in% c("Aceptado","Publicado")) as.character(input$acceptance_date) else NA
    fecha_pub <- if (input$status=="Publicado") as.character(input$publication_date) else NA
    prog <- progress_map[[input$status]]
    new_row <- data.frame(
      Nombre=input$project_name,
      Fecha_Inicio=as.character(input$start_date),
      Fecha_Envio=if (input$status %in% c("Enviado", "Revisión", "Aceptado", "Publicado")) as.character(input$send_date) else NA,
      Fecha_Respuesta=fecha_resp,
      Revista=input$journal,
      Cuartil=input$quartile,
      Estado=input$status,
      Grupo=input$group,
      Progreso=prog,
      Fecha_Aceptado=fecha_acc,
      Fecha_Publicado=fecha_pub,
      Linea_Investigacion=input$research_line,
      Observaciones=input$observations,
      stringsAsFactors=FALSE
    )
    if (length(idx)>0) {
      data[idx,] <- new_row
    } else {
      data <- rbind(data, new_row)
    }
    project_data(data)
    save_project_data(data)
    showModal(modalDialog(title="Éxito",
                          "El proyecto ha sido guardado correctamente.",
                          easyClose=TRUE, footer=modalButton("Cerrar")))
  })

  # Limpiar campos de entrada
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

  # Eliminar proyecto
  observeEvent(input$delete_button, {
    req(input$delete_project)
    data <- project_data()
    if (input$delete_project == "(sin nombre)") {
      data <- data[!(is.na(data$Nombre) | data$Nombre==""),]
    } else {
      data <- data[data$Nombre != input$delete_project,]
    }
    project_data(data)
    save_project_data(data)
    showModal(modalDialog(title="Proyecto Eliminado",
                          paste("El proyecto", input$delete_project, "ha sido eliminado."),
                          easyClose=TRUE, footer=modalButton("Cerrar")))
  })

  # Descargar datos
  output$download_data <- downloadHandler(
    filename = function() paste0("scicontrol_datos_", Sys.Date(), ".xlsx"),
    content = function(file) {
      data_list <- list(
        "Proyectos" = project_data(),
        "Calculo_Dias" = days_data()[, c("Nombre","Revista","Cuartil",
                                         "Dias_Envio_Inicio","Dias_Respuesta_Envio",
                                         "Dias_Aceptado_Respuesta","Dias_Aceptado_Envio",
                                         "Dias_Aceptado_Publicado")]
      )
      writexl::write_xlsx(data_list, file)
    }
  )

  # Importar datos desde Excel
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

      missing_columns <- setdiff(required_columns, colnames(imported_data))
      if (length(missing_columns) > 0) {
        for (col in missing_columns) {
          imported_data[[col]] <- NA
        }
      }

      imported_data <- imported_data[, required_columns]

      project_data(imported_data)
      save_project_data(imported_data)

      output$import_status <- renderText(paste("✅ Datos importados exitosamente.", nrow(imported_data), "proyectos fueron cargados desde la hoja:", project_sheet))

      showModal(modalDialog(
        title = "Éxito",
        paste("Los datos se han importado correctamente.", nrow(imported_data), "proyectos fueron cargados."),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    }, error = function(e) {
      output$import_status <- renderText(paste("❌ Error al importar datos:", e$message))
    })
  })

}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
