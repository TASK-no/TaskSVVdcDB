#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  shiny::observeEvent(input$my_logout, {
    auth0::logout()
  })
  settingsDC  <- mod_seg_competence_server("segmentation_inputs")
  settingsQ16 <- mod_seg_q_server("segmentation_inputs", num_q = 16)
  settingsQ17 <- mod_seg_q_server("segmentation_inputs", num_q = 17)
  settingsQ14 <- mod_seg_q_server("segmentation_inputs", num_q = 14)
  settingsQ19 <- mod_seg_q_server("segmentation_inputs", num_q = 19)

  data_seg_all_plot <- mod_data_overall_server("plot_01",
                                               data_raw_SVV_2021,
                                               data_raw_SVV_2022,
                                               sttgs_dc = settingsDC,
                                               sttgs_q16 = settingsQ16,
                                               sttgs_q17 = settingsQ17,
                                               sttgs_q14 = settingsQ14,
                                               sttgs_q19 = settingsQ19)
  mod_plot_overall_server("plot_01", data_seg_all_plot)

  data_plot02 <- mod_data_subplot_server("plot_02",data_raw_SVV_2021,
                                         data_raw_SVV_2022,
                                         sttgs_dc = settingsDC,
                                         sttgs_q16 = settingsQ16,
                                         sttgs_q17 = settingsQ17,
                                         sttgs_q14 = settingsQ14,
                                         sttgs_q19 = settingsQ19)
  data_plot03 <- mod_data_subplot_server("plot_03",data_raw_SVV_2021,
                                         data_raw_SVV_2022,
                                         sttgs_dc = settingsDC,
                                         sttgs_q16 = settingsQ16,
                                         sttgs_q17 = settingsQ17,
                                         sttgs_q14 = settingsQ14,
                                         sttgs_q19 = settingsQ19)
  mod_plot_subplot_server("plot_02", data_plot02, "sub_01")
  mod_plot_subplot_server("plot_03", data_plot03, "sub_02")
}
