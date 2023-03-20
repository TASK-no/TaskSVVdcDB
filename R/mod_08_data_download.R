#' 08_data_download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_08_data_download_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
  shiny.semantic::flow_layout(
    shiny::tagList(
      add_header(paste("Data-type"), size = 5),
      shiny.semantic::selectInput(ns("data_type"),
                                  label = "",
                                  choices = c("Rådata"),
                                  multiple = FALSE)
    ),
    shiny::tagList(
      add_header( paste0("\u00c5", "r "), size = 5),
      shiny.semantic::selectInput(ns("year_taken"),
                                  label = "",
                                  choices = c("2021" = 2021,
                                              "2022" = 2022,
                                              "2023" = 2023),
                                  selected = 2021,
                                  multiple = TRUE)
    ),
    shiny::tagList(
      add_header("Dataformat", size = 5),
      shiny.semantic::selectInput(ns("data_format"),
                                  label = "",
                                  choices = c("csv (.csv)" = "csv",
                                              "excel (.xlsx)" = "xls",
                                              "spss (.sav)" = "spss"),
                                  multiple = FALSE)
    )
  ),
  break_vspace(size = "small"),
  shiny::actionButton(
    inputId = ns("action"),
    label = "Nedlasting!",
    icon = icon("download")
  ),
  break_vspace(size = "small")
  )
}
mod_08_data_table_ui <- function(id){
  ns <- shiny::NS(id)
  reactable::reactableOutput(ns("table"))
}
#' 08_data_download Server Functions
#'
#' @noRd
mod_08_data_download_server <- function(id, filename) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::observeEvent(input$action, {
      shiny.semantic::show_modal('download_modal')
    })
    shiny::observeEvent(input$cancel, {
      shiny.semantic::hide_modal('download_modal')
    })
    shiny::observeEvent(input$cancel, {
      shiny.semantic::hide_modal('download_modal')
    })
    shiny::observeEvent(
      input$action,
      {
        shiny.semantic::create_modal(
          shiny.semantic::modal(
            shiny.semantic::action_button(ns("cancel"), "Cancel/Close"),
            id = ns("download_modal"),
            header = h3("Download the file?", style = "text-align: center;"),
            footer = tagList(
              shiny::downloadLink(
                outputId = ns("dataDownload"),
                label = "Yes"
              )
            ),
            class = "tiny"
          ),
          show = TRUE
        )
      }
    )
    output$dataDownload <- shiny::downloadHandler(
      filename = function() {
        extension <- switch(input$data_format,
                            "csv" = ".csv",
                            "xls" = ".xlsx",
                            "spss" = ".sav")
        if (input[["data_type"]] == "Rådata") {
          return(paste0("data_raw_SVV_", input[["year_taken"]], extension))
        }
      },
      content = function(file){
        if (input$data_format == "csv") {
          write.csv(data_to_download(), file,
                    row.names = FALSE, fileEncoding =  "utf-16")
        } else if(input$data_format == "xls") {
          xlsx::write.xlsx(data_to_download(), file, row.names = FALSE)
        } else if(input$data_format == "spss") {
          haven::write_sav(data_to_download(), file)
        }
      }
    )
    data_to_download <- shiny::reactive({
      if (input[["data_type"]] == "Rådata") {
        get_raw_data(input[["year_taken"]])
      }
    })
    output$table <- reactable::renderReactable({
      reactable::reactable(data_to_download(),
                             defaultPageSize = 35)})
  })
}
## To be copied in the UI
# mod_08_data_download_ui("08_data_download_1")

## To be copied in the server
# mod_08_data_download_server("08_data_download_1")
