#' plot_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_overall_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    h3("Samlet Oppsummerende Figur"),
    plotly::plotlyOutput(ns("plot_overall")),
    mod_break_vspace("small")
  )
}
#' plot_overall Server Functions
#'
#' @noRd
mod_plot_overall_server <- function(id, data_set_jnd){
  stopifnot(shiny::is.reactive(data_set_jnd))
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$plot_overall <- plotly::renderPlotly({
      plot_out <- TaskAnalyticsTB::plot_summary(data_set_jnd())
      generate_plotly(plot_out)
    })
  })
}
# mod_get_data_all_server <- function(id, ...) {
#   data_list <- list(...)
#   check <- lapply(list(...), shiny::is.reactive)
#   stopifnot(all(check))
#   shiny::moduleServer(id, function(input, output, session) {
#     shiny::reactive({
#       TaskAnalyticsTB::get_data_for_summary_plot(...)
#     })
#   })
# }
