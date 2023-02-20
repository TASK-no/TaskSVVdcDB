#' plot_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_subplot_ui <- function(id, name_plot_out) {
  FILTER_TEXT <- "Filter for ansatte som er"
  ns <- shiny::NS(id)
  shiny::tagList(
    add_header(paste0("Delplott: velg type og ","\u00e5", "rstall"),
               size = 3, UNDERLINE = TRUE, EMPHASIZE = TRUE),
    shiny.semantic::flow_layout(
      shiny::tagList(
        shiny::tags$h5(paste(FILTER_TEXT, "'SamAnsi'")),
        shiny.semantic::checkbox_input(ns("slider_samansi"),
                                       is_marked = FALSE,
                                       type = "slider"),
        add_header(paste(FILTER_TEXT, "'Q37'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q37"),
                                       is_marked = FALSE,
                                       type = "slider"),
        shiny::tags$h5(paste0("Velg ", "\u00e5", "rstall")),
        shiny.semantic::selectInput(ns("slider_year"),
                                    label = "",
                                    choices = c(2021, 2022, 2023),
                                    width = "75px")
      ),
      shiny::tagList(
        shiny::tags$h5(paste(FILTER_TEXT, "'leder'")),
        shiny.semantic::checkbox_input(ns("slider_leder"),
                                       is_marked = FALSE,
                                       type = "slider"),
        add_header(paste(FILTER_TEXT, "'Q38'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q38"),
                                       is_marked = FALSE,
                                       type = "slider"),
        shiny::tags$h5("Velg type"),
        shiny.semantic::selectInput(ns("slider_type"),
                                    label = "",
                                    choices = c(`Ekspertiseomrader inndelt i kompetanse` = "type_pie",
                                                `Kompetanse inndelt i kompetanseomr` = "type_bar"),
                                    width = "310px")
      ),
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'Q36'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q36"),
                                       is_marked = FALSE,
                                       type = "slider"),
        add_header(paste(FILTER_TEXT, "'Q40'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q40"),
                                       is_marked = FALSE,
                                       type = "slider")
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
mod_plot_subplot_srv <- function(id, data_set, name_plot_out){
  stopifnot(shiny::is.reactive(data_set))
  shiny::moduleServer(id, function(input, output, session){
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
mod_data_subplot_srv <- function(id, data_sets_segmented_list) {
  check_reactive_inputs(data_sets_segmented_list)
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      data_sets <- data_sets_segmented_list()
      num_ds    <- length(data_sets)
      year_seq  <- 2020 + 1:num_ds

      yr_chosen <- as.numeric(input[["slider_year"]])
      ds_chosen <- which(year_seq %in% yr_chosen)
      ds_used   <- data_sets[[ds_chosen]]
      if (input[["slider_type"]] == "type_pie") {
        type_taken <- "final"
      } else {
        type_taken <- "report"
      }
      ds_used %>%
        filter_for_samansi_leder_cat(yr_chosen,
                                     SAMANSI = input[["slider_samansi"]],
                                     LEDER = input[["slider_leder"]],
                                     CAT_Q36 = input[["slider_q36"]],
                                     CAT_Q37 = input[["slider_q37"]],
                                     CAT_Q38 = input[["slider_q38"]],
                                     CAT_Q40 = input[["slider_q40"]]) %>%
        TaskAnalyticsTB::get_data_summary(year = yr_chosen,
                                          type = type_taken)
    })
  }
  )
}
