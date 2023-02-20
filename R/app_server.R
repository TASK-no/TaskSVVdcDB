#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  r <- shiny::reactiveValues(datasets = shiny::reactiveValues(),
                             ui_inputs = shiny::reactiveValues())
  shiny::observeEvent(input$my_logout, {
    auth0::logout()
  })
  mod_01_seg_all_server("segmentation", r)
  mod_data_segmentation_srv("segmentation", r,
                            data_raw_SVV_2021,
                            data_raw_SVV_2022,
                            data_raw_SVV_2023)

  mod_cat_choices_data_srv("cat_inputs")
  mod_data_overall_srv("plot_01", r)
  mod_data_subplot_srv("plot_02", r, name_dataset = "data_plot02")
  mod_data_subplot_srv("plot_03", r, name_dataset = "data_plot03")

  mod_plot_overall_srv("plot_01", r)
  mod_plot_subplot_srv("plot_02", r, "data_plot02", "sub_01")
  mod_plot_subplot_srv("plot_03", r, "data_plot03", "sub_02")

  mod_logistic_regression_specs_01_srv("seg_inputs", r)
  mod_logistic_summary_srv("logistic_reg_01", data_log01, "logistic_out_01")
}
