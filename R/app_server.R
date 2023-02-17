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
  settingsDC  <- mod_seg_competence_srv("seg_inputs")
  settingsQ16 <- mod_seg_q_srv("seg_inputs", num_q = 16)
  settingsQ17 <- mod_seg_q_srv("seg_inputs", num_q = 17)
  settingsQ14 <- mod_seg_q_srv("seg_inputs", num_q = 14)
  settingsQ19 <- mod_seg_q_srv("seg_inputs", num_q = 19)

  data_seg_all <- mod_data_segmentation_srv("seg_inputs",
                                            data_raw_SVV_2021,
                                            data_raw_SVV_2022,
                                            data_raw_SVV_2023,
                                            sttgs_dc = settingsDC,
                                            sttgs_q16 = settingsQ16,
                                            sttgs_q17 = settingsQ17,
                                            sttgs_q14 = settingsQ14,
                                            sttgs_q19 = settingsQ19)

  data_seg_cat_all <- mod_data_cat_choices_server("cat_inputs",
                                                  data_seg_all,
                                                  list_cat_definitions = list())
  data_plot01 <- mod_data_overall_srv("plot_01", data_seg_cat_all)
  data_plot02 <- mod_data_subplot_srv("plot_02", data_seg_cat_all)
  data_plot03 <- mod_data_subplot_srv("plot_03", data_seg_cat_all)

  mod_plot_overall_srv("plot_01", data_plot01)
  mod_plot_subplot_srv("plot_02", data_plot02, "sub_01")
  mod_plot_subplot_srv("plot_03", data_plot03, "sub_02")

  data_log01 <- mod_logistic_regression_specs_01_srv("seg_inputs",
                                                     data_seg_cat_all)
  mod_logistic_summary_srv("logistic_reg_01", data_log01, "logistic_out_01")

  # num_obs
  # mod_logistic_regression_specs_02_srv("logistic_prd_01",
                                       # )
}
