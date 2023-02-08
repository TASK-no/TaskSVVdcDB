#' seg_data Server Functions
#'
#' @noRd
mod_seg_data_server <- function(id, data_set, sttgs_dc,
                                sttgs_q16, sttgs_q17,
                                sttgs_q14, sttgs_q19) {
  stopifnot(shiny::is.reactive(sttgs_dc))
  stopifnot(shiny::is.reactive(sttgs_q16))
  stopifnot(shiny::is.reactive(sttgs_q17))
  stopifnot(shiny::is.reactive(sttgs_q14))
  stopifnot(shiny::is.reactive(sttgs_q19))
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::reactive({
      TaskAnalyticsTB::segmentation_analysis(data_set,
                                             ind1_vals = sttgs_dc(),
                                             settings_q16 = sttgs_q16(),
                                             settings_q17 = sttgs_q17(),
                                             settings_q14 = sttgs_q14(),
                                             settings_q19 = sttgs_q19()) %>%
        dplyr::select(tidyselect::any_of(var_to_use1),
                      tidyselect::starts_with(var_to_use4),
                      tidyselect::starts_with(var_to_use3))
    })
  })
}
mod_data_summary_server <- function(id, data_set, year) {
  stopifnot(shiny::is.reactive(data_set))
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::reactive({
      TaskAnalyticsTB::get_data_summary(data_set(), year)
    })
  })
}
mod_overall_data_server <- function(id, ds_2021, ds_2022) {
  # browser()
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::reactive({
      # browser()
      ds_2021_eval <- ds_2021()
      ds_2022_eval <- ds_2022()
      data_seg_all_plot <- TaskAnalyticsTB::get_data_for_summary_plot(ds_2021_eval[[2]],
                                                                      ds_2022_eval[[2]])
      data_seg_all_plot
    })
  })
}

