library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(readxl)
library(writexl)
library(shinyjs)
library(utils)
library(tools)  # Para file_ext()

# Aumentar el límite de tamaño de archivo a 100 MB
options(shiny.maxRequestSize = 100*1024^2)

# Definir carpeta fija para guardar la información
data_dir <- "data"
if (!dir.exists(data_dir)) {
  dir.create(data_dir)
}
data_file <- file.path(data_dir, "project_data.xlsx")

# Carpeta para evidencias
upload_dir <- "Investigaciones"
if (!dir.exists(upload_dir)) {
  dir.create(upload_dir)
}

# Función para sanitizar el nombre del proyecto
sanitize_project_name <- function(project_name) {
  # 1) quita los caracteres prohibidos en Windows
  name <- gsub("[:/\\\\?<>\\|]", "", project_name)
  # 2) colapsa espacios múltiples en uno
  name <- gsub("\\s+", " ", name)
  # 3) quita espacios al inicio y al final
  name <- trimws(name)
  # 4) reemplaza espacios internos por guiones bajos
  name <- gsub(" ", "_", name)
  # 5) opcional: trunca a 50 caracteres
  if (nchar(name) > 50) {
    name <- substr(name, 1, 50)
  }
  return(name)
}

# Función para crear las carpetas del proyecto
create_project_folders <- function(project_name) {
  sanitized <- sanitize_project_name(project_name)
  project_path <- file.path(upload_dir, sanitized)
  # crea toda la jerarquía de golpe si hace falta
  if (!dir.exists(project_path)) {
    dir.create(project_path, recursive = TRUE, showWarnings = FALSE)
  }
  # subcarpetas SIN espacios ni tildes
  folders <- c(
    "Actas",
    "Capturas_de_Pantalla",
    "Correos_Electronicos",
    "Fotos_de_Coordinaciones",
    "Resumen_de_la_Reunion_con_AI"
  )
  for (f in folders) {
    fp <- file.path(project_path, f)
    if (!dir.exists(fp)) {
      dir.create(fp, recursive = TRUE, showWarnings = FALSE)
    }
  }
  return(project_path)
}


# Función para cargar los datos del proyecto desde el archivo Excel
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

# Función para guardar los datos en el archivo Excel (persistente)
save_project_data <- function(project_data) {
  writexl::write_xlsx(project_data, data_file)
}

# Interfaz de usuario
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
      # Tab: Agregar Proyecto
      tabItem(tabName = "agregar",
              fluidRow(
                box(title = "Agregar o Actualizar Proyecto", width = 12, status = "primary",
                    textInput("project_name", "Nombre del Proyecto"),
                    dateInput("start_date", "Fecha de Inicio", format = "yyyy-mm-dd", value = NULL),
                    conditionalPanel(
                      condition = "input.status == 'Enviado'",
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
                    selectInput("research_line", "Línea de Investigación", choices = c("Pareja", "Educativo", "Infantil")),
                    textAreaInput("observations", "Observaciones", placeholder = "Ingrese sus observaciones aquí"),
                    actionButton("save_changes", "Guardar Proyecto", class = "btn-primary"),
                    actionButton("clear_fields", "Limpiar campos", class = "btn-warning")
                )
              )
      ),

      # Tab: Ver Proyectos
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

      # Tab: Cálculo de Días
      tabItem(tabName = "dias",
              fluidRow(
                box(title = "Cálculo de Días Entre Fechas", width = 12, DTOutput("days_table"))
              )
      ),

      # Tab: Subida de Evidencias
      tabItem(tabName = "evidencias",
              fluidRow(
                box(title = "Subir Evidencias para Proyectos", width = 12, status = "primary",
                    selectInput("project_select", "Seleccione un Proyecto", choices = NULL),
                    selectInput("file_type", "Tipo de Archivo",
                                choices = c("Acta", "Captura de Pantalla", "Correo Electrónico",
                                            "Foto de Coordinaciones", "Resumen de la Reunión con AI")),
                    fileInput("file_upload", "Subir Archivo", multiple = FALSE),
                    actionButton("upload_btn", "Subir", class = "btn-primary"),
                    verbatimTextOutput("upload_status")
                )
              )
      ),

      # Tab: Ver Archivos Subidos (modificado)
      tabItem(tabName = "ver_evidencias",
              fluidRow(
                box(title = "Ver Archivos Subidos", width = 12, status = "primary",
                    selectInput("project_view", "Seleccione un Proyecto para ver archivos", choices = c("")),
                    actionButton("delete_file", "Eliminar Archivo", class = "btn-danger"),
                    DTOutput("files_table")
                )
              )
      ),

      # Tab: Descargar Datos
      tabItem(tabName = "descargar",
              fluidRow(
                box(title = "Descargar Datos y Archivos", width = 12, status = "primary",
                    downloadButton("download_data", "Descargar ZIP", class = "btn-primary")
                )
              )
      ),

      # Tab: Importar Datos
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

  # Cargar los datos al iniciar la aplicación (se leen desde el archivo Excel)
  project_data <- reactiveVal(load_project_data())
  files_refresh <- reactiveVal(0)  # Para forzar la actualización de la tabla de archivos

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

  # Subida de evidencias
  observeEvent(input$upload_btn, {
    req(input$file_upload, input$project_select, input$file_type)
    project_name <- input$project_select
    file_type <- input$file_type
    file_info <- input$file_upload
    project_folder <- create_project_folders(project_name)
    folder_map <- list(
      "Acta"                   = "Actas",
      "Captura de Pantalla"    = "Capturas_de_Pantalla",
      "Correo Electrónico"     = "Correos_Electronicos",
      "Foto de Coordinaciones" = "Fotos_de_Coordinaciones",
      "Resumen de la Reunión con AI" = "Resumen_de_la_Reunion_con_AI"
    )

    subfolder_path <- file.path(
      project_folder,
      folder_map[[file_type]]
    )
    if (!dir.exists(subfolder_path)) {
      dir.create(subfolder_path, recursive = TRUE)
    }
    file_path <- file.path(subfolder_path, file_info$name)

    if (file.copy(file_info$datapath, file_path, overwrite = TRUE)) {
      output$upload_status <- renderText(paste("Archivo subido correctamente a:", file_path))
    } else {
      output$upload_status <- renderText("Error al subir el archivo.")
    }
  })

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
      updateSelectInput(session, "research_line", selected = proj$Linea_Investigacion)
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
      Fecha_Envio=if (input$status=="Enviado") as.character(input$send_date) else NA,
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
    updateSelectInput(session, "research_line", selected = "Pareja")
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

  # Tabla de archivos subidos
  output$files_table <- renderDT({
    files_refresh()
    if (input$project_view == "") {
      empty <- data.frame(Archivo=character(), Carpeta=character(), Ruta_Completa=character())
      return(datatable(empty, options=list(pageLength=10), rownames=FALSE, selection="single"))
    }
    proj <- input$project_view
    proj_folder <- file.path(upload_dir, sanitize_project_name(proj))
    if (!dir.exists(proj_folder)) {
      empty <- data.frame(Archivo=character(), Carpeta=character(), Ruta_Completa=character())
      return(datatable(empty, options=list(pageLength=10), rownames=FALSE, selection="single"))
    }
    files <- list.files(proj_folder, recursive=TRUE, full.names=TRUE)
    if (length(files)==0) {
      empty <- data.frame(Archivo=character(), Carpeta=character(), Ruta_Completa=character())
      return(datatable(empty, options=list(pageLength=10), rownames=FALSE, selection="single"))
    }
    df <- data.frame(
      Archivo = basename(files),
      Carpeta = dirname(files),
      Ruta_Completa = files,
      stringsAsFactors = FALSE
    )
    datatable(df, options=list(pageLength=10), rownames=FALSE, selection="single", escape=FALSE)
  })

  # Eliminar archivo seleccionado
  # Eliminar archivo seleccionado
  observeEvent(input$delete_file, {
    sel <- input$files_table_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showModal(modalDialog(
        title = "Atención",
        "Por favor, seleccione primero un archivo de la tabla.",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      return()
    }

    # Reconstruir la ruta completa del archivo
    proj_folder <- file.path(upload_dir, sanitize_project_name(input$project_view))
    all_files <- list.files(proj_folder, recursive = TRUE, full.names = TRUE)
    file_to_delete <- all_files[sel]

    # Normalizamos la ruta (Windows / Unix) y comprobamos existencia
    norm_path <- normalizePath(file_to_delete, winslash = "/", mustWork = FALSE)
    message("Intentando borrar: ", norm_path)  # log para depuración

    if (file.exists(norm_path) && file.remove(norm_path)) {
      showModal(modalDialog(
        title = "Éxito",
        paste("Archivo eliminado:", basename(norm_path)),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
      # Forzamos refresco de la tabla
      files_refresh(isolate(files_refresh()) + 1)
    } else {
      showModal(modalDialog(
        title = "Error",
        paste0("No se pudo eliminar el archivo.\nRuta probada:\n", norm_path),
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      ))
    }
  })


  # Descargar datos y archivos en ZIP
  output$download_data <- downloadHandler(
    filename = function() paste0("datos_", Sys.Date(), ".zip"),
    content = function(file) {
      tmp <- tempdir()
      file.copy(data_file, file.path(tmp, "project_data.xlsx"), overwrite=TRUE)
      dias <- days_data()[, c("Nombre","Revista","Cuartil",
                              "Dias_Envio_Inicio","Dias_Respuesta_Envio",
                              "Dias_Aceptado_Respuesta","Dias_Aceptado_Envio",
                              "Dias_Aceptado_Publicado")]
      writexl::write_xlsx(dias, file.path(tmp, "calculo_dias.xlsx"))
      inv_dst <- file.path(tmp, "Investigaciones")
      if (dir.exists(inv_dst)) unlink(inv_dst, recursive=TRUE, force=TRUE)
      dir.create(inv_dst, showWarnings=FALSE)
      items <- list.files(upload_dir, full.names=TRUE)
      for (it in items) if (file.info(it)$isdir) file.copy(it, inv_dst, recursive=TRUE)
      old <- setwd(tmp); on.exit(setwd(old), add=TRUE)
      zip(file, c("project_data.xlsx","calculo_dias.xlsx","Investigaciones"), flags="-r9X")
    }
  )

  # Importar datos desde ZIP
  observeEvent(input$import_zip_btn, {
    req(input$zip_upload)
    if (file_ext(input$zip_upload$name) != "zip") {
      output$import_status <- renderText("Por favor, suba un archivo ZIP válido.")
      return()
    }
    td <- tempfile(); dir.create(td)
    unzip(input$zip_upload$datapath, exdir=td)
    excel <- file.path(td, "project_data.xlsx")
    if (file.exists(excel)) {
      file.copy(excel, data_file, overwrite=TRUE)
    } else {
      output$import_status <- renderText("El ZIP no contiene 'project_data.xlsx'.")
      return()
    }
    inv <- file.path(td, "Investigaciones")
    if (dir.exists(inv)) {
      if (dir.exists(upload_dir)) unlink(upload_dir, recursive=TRUE)
      file.copy(inv, ".", recursive=TRUE)
    }
    project_data(load_project_data())
    showModal(modalDialog(title="Éxito",
                          "La importación se realizó correctamente.",
                          easyClose=TRUE, footer=modalButton("Cerrar")))
  })

}

shinyApp(ui = ui, server = server)
