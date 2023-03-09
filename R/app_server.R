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
  r <- shiny::reactiveValues(seg_inputs = shiny::reactiveValues())
  gargoyle::init("seg_inputs")
  mod_01_seg_all_server("segmentation", r)
  browser()
  data_seg_all <- mod_data_segmentation_srv("segmentation",
                                            data_raw_SVV_2021,
                                            data_raw_SVV_2022,
                                            data_raw_SVV_2023,
                                            r = r)

  data_seg_cat_all <- mod_cat_choices_data_srv("cat_inputs", data_seg_all)
  data_plot01 <- mod_data_overall_srv("plot_01", data_seg_cat_all)
  data_plot02 <- mod_data_subplot_srv("plot_02", data_seg_cat_all)
  data_plot03 <- mod_data_subplot_srv("plot_03", data_seg_cat_all)

  mod_plot_overall_srv("plot_01", data_plot01)
  mod_plot_subplot_srv("plot_02", data_plot02, "sub_01")
  mod_plot_subplot_srv("plot_03", data_plot03, "sub_02")

  data_log01 <- mod_logistic_regression_specs_01_srv("seg_inputs",
                                                     data_seg_cat_all)
  mod_logistic_summary_srv("logistic_reg_01", data_log01, "logistic_out_01")
}
