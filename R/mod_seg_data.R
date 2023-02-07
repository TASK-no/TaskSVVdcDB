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
# #' seg_data UI Function
# #'
# #' @description A shiny Module.
# #'
# #' @param id,input,output,session Internal parameters for {shiny}.
# #'
# #' @noRd
# #'
# #' @importFrom shiny NS tagList
# mod_seg_data_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#
#   )
# }
#
