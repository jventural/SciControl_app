library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(readxl)
library(writexl)
library(shinyjs)
library(utils)
library(tools)  # Para file_ext()

# Crear la carpeta para almacenar los archivos subidos
upload_dir <- "Investigaciones"
if (!dir.exists(upload_dir)) {
  dir.create(upload_dir)
}

sanitize_project_name <- function(project_name) {
  sanitized_name <- gsub("[:/\\?<>\\|]", "_", project_name)
  if (nchar(sanitized_name) > 50) {
    sanitized_name <- substr(sanitized_name, 1, 50)
  }
  return(sanitized_name)
}

create_project_folders <- function(project_name) {
  sanitized_name <- sanitize_project_name(project_name)
  project_path <- file.path(upload_dir, sanitized_name)
  if (!dir.exists(project_path)) {
    dir.create(project_path)
  }
  folders <- c("Actas", "Capturas de Pantalla", "Correos Electrónicos", "Fotos de Coordinaciones", "Resumen de la Reunión con AI")
  for (folder in folders) {
    folder_path <- file.path(project_path, folder)
    if (!dir.exists(folder_path)) {
      dir.create(folder_path)
    }
  }
  return(project_path)
}

load_project_data <- function() {
  excel_file <- "project_data.xlsx"
  if (file.exists(excel_file)) {
    project_data <- read_excel(excel_file)
    required_columns <- c("Nombre", "Fecha_Inicio", "Fecha_Envio", "Fecha_Respuesta", "Revista", "Cuartil", "Estado",
                          "Grupo", "Progreso", "Fecha_Aceptado", "Fecha_Publicado", "Linea_Investigacion",
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
    writexl::write_xlsx(project_data, excel_file)
  }
  return(project_data)
}

save_project_data <- function(project_data) {
  writexl::write_xlsx(project_data, "project_data.xlsx")
}

ui <- dashboardPage(
  dashboardHeader(title = "SciControl"),
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
      "))
    ),
    tabItems(
      tabItem(tabName = "agregar",
              fluidRow(
                box(title = "Agregar o Actualizar Proyecto", width = 12, status = "primary",
                    textInput("project_name", "Nombre del Proyecto"),
                    dateInput("start_date", "Fecha de Inicio", format = "yyyy-mm-dd", value = NULL),
                    dateInput("send_date", "Fecha de Envío", format = "yyyy-mm-dd", value = NULL),
                    textInput("journal", "Revista"),
                    selectInput("quartile", "Cuartil de la Revista", choices = c("Q1", "Q2", "Q3", "Q4")),
                    selectInput("status", "Estado", choices = c("Introducción", "Método", "Resultados", "Discusión", "Enviado", "Revisión", "Aceptado", "Publicado")),
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
                    selectInput("research_line", "Línea de Investigación", choices = c("Pareja", "Educativo", "Infantil")),
                    textAreaInput("observations", "Observaciones", placeholder = "Ingrese sus observaciones aquí"),
                    actionButton("save_changes", "Guardar Proyecto", class = "btn-primary"),
                    actionButton("clear_fields", "Limpiar campos", class = "btn-warning")
                )
              )
      ),
      tabItem(tabName = "ver",
              fluidRow(
                box(title = "Proyectos Actuales", width = 12, DTOutput("project_table"))
              ),
              fluidRow(
                box(title = "Guardar Información", width = 12, status = "primary",
                    actionButton("save_projects", "Guardar Información", class = "btn-primary")
                )
              ),
              fluidRow(
                box(title = "Eliminar Proyecto", width = 12, status = "danger",
                    selectInput("delete_project", "Seleccione un proyecto para eliminar", choices = NULL),
                    actionButton("delete_button", "Eliminar Proyecto", class = "btn-danger")
                )
              )
      ),
      tabItem(tabName = "dias",
              fluidRow(
                box(title = "Cálculo de Días Entre Fechas", width = 12, DTOutput("days_table"))
              )
      ),
      tabItem(tabName = "evidencias",
              fluidRow(
                box(title = "Subir Evidencias para Proyectos", width = 12, status = "primary",
                    selectInput("project_select", "Seleccione un Proyecto", choices = NULL),
                    selectInput("file_type", "Tipo de Archivo", choices = c("Acta", "Captura de Pantalla", "Correo Electrónico", "Foto de Coordinaciones", "Resumen de la Reunión con AI")),
                    fileInput("file_upload", "Subir Archivo", multiple = FALSE),
                    actionButton("upload_btn", "Subir", class = "btn-primary"),
                    verbatimTextOutput("upload_status")
                )
              )
      ),
      tabItem(tabName = "ver_evidencias",
              fluidRow(
                box(title = "Ver Archivos Subidos", width = 12, status = "primary",
                    selectInput("project_view", "Seleccione un Proyecto para ver archivos", choices = c("")),
                    actionButton("clear_files_table", "Limpiar Archivos", class = "btn-warning"),
                    DTOutput("files_table")
                )
              )
      ),
      tabItem(tabName = "descargar",
              fluidRow(
                box(title = "Descargar Datos y Archivos", width = 12, status = "primary",
                    downloadButton("download_data", "Descargar ZIP", class = "btn-primary")
                )
              )
      ),
      tabItem(tabName = "importar",
              fluidRow(
                box(title = "Importar Datos desde ZIP", width = 12, status = "primary",
                    fileInput("zip_upload", "Subir Archivo ZIP", accept = ".zip"),
                    actionButton("import_zip_btn", "Importar ZIP", class = "btn-primary"),
                    verbatimTextOutput("import_status")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {

  project_data <- reactiveVal(load_project_data())
  files_refresh <- reactiveVal(0) # Contador para forzar la actualización de la tabla de archivos

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

  # Actualiza las opciones de selección en varios inputs
  observe({
    updateSelectInput(session, "project_select", choices = project_data()$Nombre)
    updateSelectInput(session, "project_view", choices = c("", project_data()$Nombre))
    updateSelectInput(session, "delete_project", choices = project_data()$Nombre)
  })

  observeEvent(input$upload_btn, {
    req(input$file_upload, input$project_select, input$file_type)
    project_name <- input$project_select
    file_type <- input$file_type
    file_info <- input$file_upload

    project_folder <- create_project_folders(project_name)

    folder_map <- list(
      "Acta" = "Actas",
      "Captura de Pantalla" = "Capturas de Pantalla",
      "Correo Electrónico" = "Correos Electrónicos",
      "Foto de Coordinaciones" = "Fotos de Coordinaciones",
      "Resumen de la Reunión con AI" = "Resumen de la Reunión con AI"
    )

    folder_name <- folder_map[[file_type]]
    subfolder_path <- file.path(project_folder, folder_name)
    if (!dir.exists(subfolder_path)) {
      dir.create(subfolder_path, recursive = TRUE)
    }

    file_path <- file.path(subfolder_path, file_info$name)
    if (file.copy(file_info$datapath, file_path, overwrite = TRUE)) {
      output$upload_status <- renderText({
        paste("Archivo subido correctamente a:", file_path)
      })
    } else {
      output$upload_status <- renderText({
        "Error al subir el archivo."
      })
    }
  })

  output$project_table <- renderDT({
    data <- project_data()
    data$Progreso <- sapply(data$Estado, function(estado) {
      progress_percentage <- progress_map[[estado]]
      paste0('<div style="width:100%; background-color:#f3f3f3; border-radius:5px;">
                <div style="width:', progress_percentage, '%; background-color:#e74c3c; color:white; text-align:center; padding:5px 0; border-radius:5px;">
                  ', progress_percentage, '%
                </div>
              </div>')
    })
    datatable(data, escape = FALSE, options = list(pageLength = 10), rownames = FALSE)
  })

  days_data <- reactive({
    data <- project_data()

    data$Dias_Envio_Inicio <- ifelse(!is.na(data$Fecha_Inicio) & !is.na(data$Fecha_Envio),
                                     as.numeric(difftime(as.Date(data$Fecha_Envio, "%Y-%m-%d"),
                                                         as.Date(data$Fecha_Inicio, "%Y-%m-%d"), units = "days")),
                                     NA)

    data$Dias_Aceptado_Envio <- ifelse(data$Estado %in% c("Aceptado", "Publicado") &
                                         !is.na(data$Fecha_Aceptado) & !is.na(data$Fecha_Envio),
                                       as.numeric(difftime(as.Date(data$Fecha_Aceptado, "%Y-%m-%d"),
                                                           as.Date(data$Fecha_Envio, "%Y-%m-%d"), units = "days")),
                                       NA)

    data$Dias_Aceptado_Publicado <- ifelse(data$Estado == "Publicado" &
                                             !is.na(data$Fecha_Publicado) & !is.na(data$Fecha_Aceptado),
                                           as.numeric(difftime(as.Date(data$Fecha_Publicado, "%Y-%m-%d"),
                                                               as.Date(data$Fecha_Aceptado, "%Y-%m-%d"), units = "days")),
                                           NA)

    data$Dias_Respuesta_Envio <- ifelse(!is.na(data$Fecha_Envio) & !is.na(data$Fecha_Respuesta),
                                        as.numeric(difftime(as.Date(data$Fecha_Respuesta, "%Y-%m-%d"),
                                                            as.Date(data$Fecha_Envio, "%Y-%m-%d"), units = "days")),
                                        NA)

    data$Dias_Aceptado_Respuesta <- ifelse(!is.na(data$Fecha_Aceptado) & !is.na(data$Fecha_Respuesta),
                                           as.numeric(difftime(as.Date(data$Fecha_Aceptado, "%Y-%m-%d"),
                                                               as.Date(data$Fecha_Respuesta, "%Y-%m-%d"), units = "days")),
                                           NA)
    data
  })

  output$days_table <- renderDT({
    data <- days_data()
    datatable(data[, c("Nombre", "Revista", "Cuartil",
                       "Dias_Envio_Inicio", "Dias_Respuesta_Envio", "Dias_Aceptado_Respuesta",
                       "Dias_Aceptado_Envio", "Dias_Aceptado_Publicado")],
              options = list(pageLength = 10), rownames = FALSE)
  })

  observeEvent(input$project_table_rows_selected, {
    selected_row <- input$project_table_rows_selected
    if (!is.null(selected_row)) {
      selected_project <- project_data()[selected_row, ]

      updateSelectInput(session, "status", selected = selected_project$Estado)
      updateTextInput(session, "project_name", value = selected_project$Nombre)
      updateDateInput(session, "start_date", value = as.Date(selected_project$Fecha_Inicio))
      updateDateInput(session, "send_date", value = as.Date(selected_project$Fecha_Envio))
      updateDateInput(session, "response_date", value = as.Date(selected_project$Fecha_Respuesta))
      updateTextInput(session, "journal", value = selected_project$Revista)
      updateSelectInput(session, "quartile", selected = selected_project$Cuartil)
      updateSelectInput(session, "group", selected = selected_project$Grupo)
      updateDateInput(session, "acceptance_date", value = as.Date(selected_project$Fecha_Aceptado))
      updateDateInput(session, "publication_date", value = as.Date(selected_project$Fecha_Publicado))
      updateSelectInput(session, "research_line", selected = selected_project$Linea_Investigacion)
      updateTextAreaInput(session, "observations", value = selected_project$Observaciones)
    }
  })

  observeEvent(input$save_changes, {
    data <- project_data()
    project_index <- which(data$Nombre == input$project_name)

    fecha_respuesta <- if (input$status %in% c("Revisión", "Aceptado", "Publicado")) {
      as.character(input$response_date)
    } else {
      NA
    }

    fecha_aceptado <- if (input$status %in% c("Aceptado", "Publicado")) {
      as.character(input$acceptance_date)
    } else if (length(project_index) > 0) {
      data$Fecha_Aceptado[project_index]
    } else {
      NA
    }

    fecha_publicado <- if (input$status == "Publicado") {
      as.character(input$publication_date)
    } else if (length(project_index) > 0) {
      data$Fecha_Publicado[project_index]
    } else {
      NA
    }

    progreso <- progress_map[[input$status]]

    new_entry <- data.frame(
      Nombre = input$project_name,
      Fecha_Inicio = as.character(input$start_date),
      Fecha_Envio = as.character(input$send_date),
      Fecha_Respuesta = fecha_respuesta,
      Revista = input$journal,
      Cuartil = input$quartile,
      Estado = input$status,
      Grupo = input$group,
      Progreso = progreso,
      Fecha_Aceptado = fecha_aceptado,
      Fecha_Publicado = fecha_publicado,
      Linea_Investigacion = input$research_line,
      Observaciones = input$observations,
      stringsAsFactors = FALSE
    )

    if (length(project_index) > 0) {
      data[project_index, ] <- new_entry
    } else {
      data <- rbind(data, new_entry)
    }

    project_data(data)
    save_project_data(data)  # Guarda automáticamente la información

    showModal(modalDialog(
      title = "Éxito",
      "El proyecto ha sido guardado correctamente y el archivo Excel ha sido actualizado.",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })

  # Botón para guardar información desde el panel "Ver Proyectos"
  observeEvent(input$save_projects, {
    save_project_data(project_data())
    showModal(modalDialog(
      title = "Información Guardada",
      "La información de los proyectos se ha guardado en project_data.xlsx.",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })

  observeEvent(input$clear_fields, {
    updateTextInput(session, "project_name", value = "")
    updateDateInput(session, "start_date", value = NULL)
    updateDateInput(session, "send_date", value = NULL)
    updateDateInput(session, "response_date", value = NULL)
    updateTextInput(session, "journal", value = "")
    updateSelectInput(session, "quartile", selected = "")
    updateSelectInput(session, "status", selected = "")
    updateSelectInput(session, "group", selected = "")
    updateDateInput(session, "acceptance_date", value = NULL)
    updateDateInput(session, "publication_date", value = NULL)
    updateSelectInput(session, "research_line", selected = "")
    updateTextAreaInput(session, "observations", value = "")
  })

  # Botón para eliminar archivos seleccionados en la tabla de evidencias
  observeEvent(input$clear_files_table, {
    selected_row <- input$files_table_rows_selected
    if (length(selected_row) > 0 && input$project_view != "") {
      project_name <- input$project_view
      project_folder <- file.path(upload_dir, sanitize_project_name(project_name))

      if (dir.exists(project_folder)) {
        file_list <- list.files(project_folder, recursive = TRUE, full.names = TRUE)
        if (length(file_list) > 0) {
          files_df <- data.frame(
            Archivo = basename(file_list),
            Carpeta = dirname(file_list),
            Ruta_Completa = file_list,
            stringsAsFactors = FALSE
          )
          file_to_delete <- files_df$Ruta_Completa[selected_row]
          if (file.exists(file_to_delete)) {
            file.remove(file_to_delete)
          }
        }
      }
      files_refresh(files_refresh() + 1)
    }
  })

  output$files_table <- renderDT({
    files_refresh()
    if (input$project_view == "") {
      return(datatable(data.frame(Archivo = character(), Carpeta = character(), Ruta_Completa = character()),
                       options = list(pageLength = 10), rownames = FALSE, selection = "single"))
    }
    project_name <- input$project_view
    project_folder <- file.path(upload_dir, sanitize_project_name(project_name))
    if (!dir.exists(project_folder)) {
      return(datatable(data.frame(Archivo = character(), Carpeta = character(), Ruta_Completa = character()),
                       options = list(pageLength = 10), rownames = FALSE, selection = "single"))
    }
    file_list <- list.files(project_folder, recursive = TRUE, full.names = TRUE)
    if (length(file_list) == 0) {
      return(datatable(data.frame(Archivo = character(), Carpeta = character(), Ruta_Completa = character()),
                       options = list(pageLength = 10), rownames = FALSE, selection = "single"))
    }
    files_df <- data.frame(
      Archivo = basename(file_list),
      Carpeta = dirname(file_list),
      Ruta_Completa = file_list,
      stringsAsFactors = FALSE
    )
    datatable(files_df, options = list(pageLength = 10), rownames = FALSE, selection = "single")
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("datos_", Sys.Date(), ".zip")
    },
    content = function(file) {
      tmp_dir <- tempdir()

      # Copiar el archivo project_data.xlsx actualizado al directorio temporal
      file.copy("project_data.xlsx", file.path(tmp_dir, "project_data.xlsx"), overwrite = TRUE)

      # Generar el archivo calculo_dias.xlsx
      dias <- days_data()
      dias_calculo <- dias[, c("Nombre", "Revista", "Cuartil",
                               "Dias_Envio_Inicio", "Dias_Respuesta_Envio", "Dias_Aceptado_Respuesta",
                               "Dias_Aceptado_Envio", "Dias_Aceptado_Publicado")]
      writexl::write_xlsx(dias_calculo, file.path(tmp_dir, "calculo_dias.xlsx"))

      investigations_dst <- file.path(tmp_dir, "Investigaciones")
      if (dir.exists(investigations_dst)) {
        unlink(investigations_dst, recursive = TRUE, force = TRUE)
      }

      investigations_src <- upload_dir
      dir.create(investigations_dst, showWarnings = FALSE)

      investigation_items <- list.files(investigations_src, full.names = TRUE)
      for(item in investigation_items) {
        if (file.info(item)$isdir) {
          file.copy(item, investigations_dst, recursive = TRUE)
        }
      }

      old_wd <- setwd(tmp_dir)
      on.exit(setwd(old_wd), add = TRUE)
      zip(file, c("project_data.xlsx", "calculo_dias.xlsx", "Investigaciones"), flags = "-r9X")
    }
  )

  observeEvent(input$import_zip_btn, {
    req(input$zip_upload)
    if (file_ext(input$zip_upload$name) != "zip") {
      output$import_status <- renderText("Por favor, suba un archivo ZIP válido.")
      return()
    }
    temp_import_dir <- tempfile()
    dir.create(temp_import_dir)
    unzip(input$zip_upload$datapath, exdir = temp_import_dir)

    # Para la importación, se espera encontrar project_data.xlsx
    excel_path <- file.path(temp_import_dir, "project_data.xlsx")
    if (file.exists(excel_path)) {
      file.copy(excel_path, "project_data.xlsx", overwrite = TRUE)
    } else {
      output$import_status <- renderText("El archivo ZIP no contiene 'project_data.xlsx'. Importación cancelada.")
      return()
    }

    investigations_path <- file.path(temp_import_dir, "Investigaciones")
    if (dir.exists(investigations_path)) {
      if (dir.exists("Investigaciones")) {
        unlink("Investigaciones", recursive = TRUE)
      }
      file.copy(investigations_path, ".", recursive = TRUE)
    }

    project_data(load_project_data())
    updateSelectInput(session, "project_select", choices = project_data()$Nombre)
    updateSelectInput(session, "project_view", choices = c("", project_data()$Nombre))
    updateSelectInput(session, "delete_project", choices = project_data()$Nombre)

    output$import_status <- renderText("Importación completada exitosamente.")

    showModal(modalDialog(
      title = "Éxito",
      "La importación de datos se realizó correctamente.",
      easyClose = TRUE,
      footer = modalButton("Cerrar")
    ))
  })

}

shinyApp(ui = ui, server = server)
