#' plot_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_subplot_ui <- function(id, year, name_plot_out) {
  FILTER_TEXT <- "Filter for ansatte som er"
  ns <- shiny::NS(id)
  shiny::tagList(
    add_header(paste0("Delplott: velg type og ","\u00e5", "rstall"),
               size = 3, UNDERLINE = TRUE, EMPHASIZE = TRUE),
    shiny.semantic::flow_layout(
      shiny::tagList(
        shiny::tags$h5(paste(FILTER_TEXT, "'SamAnsi'")),
        shiny.semantic::checkbox_input(ns("slider_samansi"),
                                       type = "slider"),
        shiny::tags$h5(paste0("Velg ", "\u00e5", "rstall")),
        shiny.semantic::selectInput(ns("slider_year"),
                                    label = "",
                                    choices = c(2021, 2022),
                                    width = "75px")
      ),
      shiny::tagList(
        shiny::tags$h5(paste(FILTER_TEXT, "'leder'")),
        shiny.semantic::checkbox_input(ns("slider_leder"),
                                       type = "slider"),
        shiny::tags$h5("Velg type"),
        shiny.semantic::selectInput(ns("slider_type"),
                                    label = "",
                                    choices = c(`Ekspertiseomrader inndelt i kompetanse` = "type_pie",
                                                `Kompetanse inndelt i kompetanseomr` = "type_bar"),
                                    width = "310px")
      ),
      min_cell_width = "75px",
      max_cell_width = "320px",
      column_gap = "1em"
    ),
    break_vspace("small"),
    plotly::plotlyOutput(ns(name_plot_out),
                         width = "1100px",
                         height = "700px")
  )
}
#' mod_plot_pie Server Functions
#'
#' @noRd
mod_plot_subplot_server <- function(id, data_set, name_plot_out){
  stopifnot(shiny::is.reactive(data_set))
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    output[[name_plot_out]] <- plotly::renderPlotly({
      year_taken <- input[["slider_year"]]
      if (input[["slider_type"]] == "type_pie") {
        plot_out <- TaskAnalyticsTB::plot_pie_figures(data_set(),
                                                      year = year_taken,
                                                      return_type = "shinyDB")
        generate_plotly(plot_out,  list(font = list(size = 12)))
      } else if (input[["slider_type"]] == "type_bar") {
        df
        data_rader <- TaskAnalyticsTB::get_data_summary_radar(data_set())
        plot_out <- TaskAnalyticsTB::plot_radar(data_rader,
                                                year_taken,
                                                return_type = "shinyDB")
        generate_plotly(plot_out)
      }
    })
  })
}
mod_data_subplot_server <- function(id, ...,
                                    sttgs_dc,
                                    sttgs_q16,
                                    sttgs_q17,
                                    sttgs_q14,
                                    sttgs_q19) {
  stopifnot(shiny::is.reactive(sttgs_dc))
  stopifnot(shiny::is.reactive(sttgs_q16))
  stopifnot(shiny::is.reactive(sttgs_q17))
  stopifnot(shiny::is.reactive(sttgs_q14))
  stopifnot(shiny::is.reactive(sttgs_q19))
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::reactive({
      ds_raw_all <- list(...)
      year_seq   <- 2020 + 1:length(ds_raw_all)
      names(ds_raw_all) <- as.character(year_seq)
      ds_chosen <- ds_raw_all[[input[["slider_year"]]]]
      yr_chosen <- as.numeric(input[["slider_year"]])
      if (input[["slider_type"]] == "type_pie") {
        type_taken <- "final"
      } else {
        type_taken <- "report"
      }
      ds_out <-  ds_chosen %>%
        filter_for_samansi_leder(yr_chosen,
                                 SAMANSI = input[["slider_samansi"]],
                                 LEDER = input[["slider_leder"]]) %>%
        TaskAnalyticsTB::segmentation_analysis(ind1_vals = sttgs_dc(),
                                               settings_q16 = sttgs_q16(),
                                               settings_q17 = sttgs_q17(),
                                               settings_q14 = sttgs_q14(),
                                               settings_q19 = sttgs_q19()) %>%
        dplyr::select(tidyselect::any_of(var_to_use1),
                      tidyselect::starts_with(var_to_use4),
                      tidyselect::starts_with(var_to_use3)) %>%
        TaskAnalyticsTB::get_data_summary(year = yr_chosen,
                                          type = type_taken)

    })
  }
  )
}
