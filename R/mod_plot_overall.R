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
  FILTER_TEXT <- "Filter for ansatte som er"
  ns <- shiny::NS(id)
  shiny::tagList(
    add_header("Samlet Oppsummerende Figur", size = 3,
               UNDERLINE = TRUE, EMPHASIZE = TRUE),
    shiny.semantic::flow_layout(
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'SamAnsi'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_samansi"),
                                       type = "slider")
      ),
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'leder'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_leder"),
                                       type = "slider")
      ),
      min_cell_width = "75px",
      max_cell_width = "340px"
    ),
    plotly::plotlyOutput(ns("plot_overall"),
                         width = "1100px",
                         height = "700px"),
    break_vspace("small")
  )
}
#' plot_overall Server Functions
#'
#' @noRd
mod_plot_overall_srv <- function(id, data_set_jnd){
  stopifnot(shiny::is.reactive(data_set_jnd))
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$plot_overall <- plotly::renderPlotly({
      plot_out <- TaskAnalyticsTB::plot_overall(data_set_jnd(),
                                                return_type = "shinyDB")
      generate_plotly(plot_out)
    })
  })
}
mod_data_overall_srv <- function(id, data_sets_segmented_list) {
  check_reactive_inputs(data_sets_segmented_list)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::reactive({
      data_sets <- data_sets_segmented_list()
      num_ds   <- length(data_sets)
      year_seq <- 2020 + 1:num_ds
      ds_list <- vector("list", num_ds)
      for (i in seq_len(num_ds)) {
        ds_list[[i]] <- data_sets[[i]] %>%
          filter_for_samansi_leder(year_seq[i],
                                   SAMANSI = input[["slider_samansi"]],
                                   LEDER = input[["slider_leder"]]) %>%
          TaskAnalyticsTB::get_data_summary(year = year_seq[i], type = "report")
        ds_list[[i]]$year <- as.factor(ds_list[[i]]$year)
      }
      data_jnd <- do.call(TaskAnalyticsTB::get_data_joined, ds_list)
      return(data_jnd)
    })
  })
}
