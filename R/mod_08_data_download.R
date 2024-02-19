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
          choices = get_data_names_norsk(num = 1:14),
          multiple = FALSE
        )
      ),
      shiny::tagList(
        add_header("Dataformat", size = 5),
        shiny.semantic::selectInput(ns("data_format"),
          label = "",
          choices = c(
            "csv (.csv)" = "csv",
            "excel (.xlsx)" = "xls",
            "spss (.sav)" = "spss",
            "R (.rda)" = "rda"
          ),
          multiple = FALSE
        )
      )
    ),
    shiny.semantic::flow_layout(
      shiny::tagList(
        break_vspace(size = "small"),
        shiny::actionButton(
          inputId = ns("action_data_update"),
          label = "Oppdater datasett!",
          icon = shiny.semantic::icon("home")
        )
      ),
      shiny::tagList(
        break_vspace(size = "small"),
        shiny::actionButton(
          inputId = ns("action_data_download"),
          label = "Nedlasting!",
          icon = shiny.semantic::icon("download")
        )
      )
    ),
    break_vspace(size = "small")
  )
}
mod_08_data_table_ui <- function(id) {
  ns <- shiny::NS(id)
  reactable::reactableOutput(ns("table_data_preview"))
  # shiny::tableOutput(ns("table_data_preview"))
}
#' 08_data_download Server Functions
#'
#' @noRd
mod_08_data_download_server <- function(id, r, data_seg, data_log) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_to_download <- shiny::reactive(
      {
      get_data(input, r, data_seg, data_log)
      }
    ) %>% shiny::bindEvent(gargoyle::watch("action_data_regenerate"))
    shiny::observeEvent(
      input$action_data_download_cancel,
      {
      shiny.semantic::hide_modal("download_modal")
      }
    )
    shiny::observeEvent(
      input$action_data_download,
      {
        modal_download(ns, session_passed = shiny::getDefaultReactiveDomain())
        gargoyle::trigger("action_data_regenerate")
      }
    )
    shiny::observeEvent(
      input$action_data_update,
      {
        gargoyle::trigger("action_data_regenerate")
      }
    )
    output$dataDownload <- shiny::downloadHandler(
      filename = download_handler_filename_01(input = input),
      content = download_handler_content_01(file, input, data_to_download)
    )
    output$table_data_preview <- reactable::renderReactable({
      tmp_data <- data_to_download()
      tmp_data <- get_data_no_haven_labelled(tmp_data)
      shiny::validate(
        shiny::need(
          !is.null(tmp_data), get_msg_reactable_logistics_data_null()
        )
      )
      reactable::reactable(
        tmp_data,
        defaultPageSize = 10
      )
    })
  })
}
