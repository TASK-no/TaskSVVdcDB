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

  # data_flt_2021 <- mod_flt_data_server("plot02")
  # data_flt_2022 <- mod_flt_data_server("plot03")
  data_seg_2021 <- mod_seg_data_server("segmentation_inputs",
                                       data_raw_SVV_2021, settingsDC,
                                       settingsQ16, settingsQ17,
                                       settingsQ14, settingsQ19)
  data_seg_2022 <- mod_seg_data_server("segmentation_inputs",
                                       data_raw_SVV_2022, settingsDC,
                                       settingsQ16, settingsQ17,
                                       settingsQ14, settingsQ19)

  data_summary_2021 <- mod_data_summary_server("data_precompute", data_seg_2021, "2021")
  data_summary_2022 <- mod_data_summary_server("data_precompute", data_seg_2022, "2022")

  # data_seg_all_plot <- mod_overall_data_server("plot01", data_summary_2021, data_summary_2022)
  data_seg_all_plot <- mod_data_overall_server("plot01", data_raw_SVV_2021, data_raw_SVV_2022,
                                               sttgs_dc = settingsDC,
                                               sttgs_q16 = settingsQ16,
                                               sttgs_q17 = settingsQ17,
                                               sttgs_q14 = settingsQ14,
                                               sttgs_q19 = settingsQ19)
  mod_plot_overall_server("plot01", data_seg_all_plot)

  mod_plot_subplot_server("plot02", list("2021" = data_summary_2021,
                                         "2022" = data_summary_2022),
                          "pie_2021")
  mod_plot_subplot_server("plot03", list("2021" = data_summary_2021,
                                         "2022" = data_summary_2022),
                          "pie_2022")
}
