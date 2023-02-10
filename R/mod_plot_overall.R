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
    htmltools::h3(tags$u(tags$em("Samlet Oppsummerende Figur"))),
    shiny.semantic::flow_layout(
      shiny::tagList(
        shiny::tags$h5(paste(FILTER_TEXT, "'SamAnsi'")),
        shiny.semantic::checkbox_input(ns("slider_samansi"),
                                       # label = paste(FILTER_TEXT, "'SamAnsi'"),
                                       type = "slider")
      ),
      shiny::tagList(
        shiny::tags$h5(paste(FILTER_TEXT, "'leder'")),
        shiny.semantic::checkbox_input(ns("slider_leder"),
                                       # label = paste(FILTER_TEXT, "'leder'"),
                                       type = "slider")
      ),
      min_cell_width = "75px",
      max_cell_width = "340px"
    ),
    plotly::plotlyOutput(ns("plot_overall"),
                         width = "1100px",
                         height = "700px"),
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
      plot_out <- TaskAnalyticsTB::plot_overall(data_set_jnd())
      generate_plotly(plot_out, list(title = "", visible = FALSE,
                                     orientation = "h", x = 0, y = -0.25,
                                     font = list(size = 12)))
    })
  })
}
mod_data_overall_server <- function(id, ...,
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
      num_ds   <- length(ds_raw_all)
      year_seq <- 2020 + 1:num_ds
      ds_list <- vector("list", num_ds)
      for (i in seq_len(num_ds)) {
        ds_list[[i]] <- ds_raw_all[[i]] %>%
          filter_for_samansi_leder(year_seq[i],
                                   SAMANSI = input[["slider_samansi"]],
                                   LEDER = input[["slider_leder"]])
        ds_list[[i]] <- ds_list[[i]] %>%
          TaskAnalyticsTB::segmentation_analysis(ind1_vals = sttgs_dc(),
                                                 settings_q16 = sttgs_q16(),
                                                 settings_q17 = sttgs_q17(),
                                                 settings_q14 = sttgs_q14(),
                                                 settings_q19 = sttgs_q19()) %>%
          dplyr::select(tidyselect::any_of(var_to_use1),
                        tidyselect::starts_with(var_to_use4),
                        tidyselect::starts_with(var_to_use3)) %>%
          TaskAnalyticsTB::get_data_summary(year = year_seq[i], type = "report")
      }
      data_jnd <- do.call(TaskAnalyticsTB::get_data_joined, ds_list)
      data_jnd$year <- as.factor(data_jnd$year)
      return(data_jnd)
    })
  })
}
