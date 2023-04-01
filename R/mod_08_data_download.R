#' 08_data_download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_08_data_download_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny.semantic::flow_layout(
      shiny::tagList(
        add_header(paste("Data-type"), size = 5),
        shiny.semantic::selectInput(ns("data_type"),
          label = "",
          choices = get_data_names_norsk(num = 1:12),
          multiple = FALSE
        )
      ),
      # shiny::tagList(
      #   add_header( paste0("\u00c5", "r "), size = 5),
      #   shiny.semantic::selectInput(ns("year_taken"),
      #                               label = "",
      #                               choices = c("2021" = 2021,
      #                                           "2022" = 2022,
      #                                           "2023" = 2023),
      #                               selected = 2021,
      #                               multiple = TRUE)
      # ),
      shiny::tagList(
        add_header("Dataformat", size = 5),
        shiny.semantic::selectInput(ns("data_format"),
          label = "",
          choices = c(
            "csv (.csv)" = "csv",
            "excel (.xlsx)" = "xls",
            "spss (.sav)" = "spss"
          ),
          multiple = FALSE
        )
      )
    ),
    break_vspace(size = "small"),
    shiny::actionButton(
      inputId = ns("action"),
      label = "Nedlasting!",
      icon = shiny.semantic::icon("download")
    ),
    break_vspace(size = "small")
  )
}
mod_08_data_table_ui <- function(id) {
  ns <- shiny::NS(id)
  reactable::reactableOutput(ns("table"))
}
#' 08_data_download Server Functions
#'
#' @noRd
mod_08_data_download_server <- function(id, r, data_seg, data_log) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_to_download <- shiny::reactive({
      get_data(input, r, data_seg, data_log)
    })
    shiny::observeEvent(input$action, {
      shiny.semantic::show_modal("download_modal")
    })
    shiny::observeEvent(input$cancel, {
      shiny.semantic::hide_modal("download_modal")
    })
    shiny::observeEvent(
      input$action,
      {
        modal_download(ns, session_passed = shiny::getDefaultReactiveDomain())
      }
    )
    output$dataDownload <- shiny::downloadHandler(
      filename = download_handler_filename_01(input = input),
      content = download_handler_content_01(file, input, data_to_download)
    )
    output$table <- reactable::renderReactable({
      reactable::reactable(data_to_download(),
        defaultPageSize = 35
      )
    })
  })
}
