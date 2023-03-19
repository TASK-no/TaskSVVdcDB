#' the application server-side
#'
#' @param input,output,session internal parameters for {shiny}.
#'     do not remove.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  shiny::observeEvent(input$my_logout, {
    auth0::logout()
  })
  r <- shiny::reactiveValues(seg_inputs = shiny::reactiveValues(),
                             datasets = shiny::reactiveValues())
  gargoyle::init("seg_inputs")
  gargoyle::init("data_seg")
  gargoyle::init("data_cat")

  data_seg_all <- DataSegmentation$new(r, data_raw_SVV_2021,
                                       data_raw_SVV_2022,
                                       data_raw_SVV_2023)
  mod_01_seg_all_server("segmentation", r)
  mod_data_segmentation_srv("segmentation", r = r,
                            data_seg = data_seg_all)

  mod_cat_choices_data_srv("cat_inputs", data_seg_all)

  mod_data_overall_srv("plot_01", r, data_seg_all)
  mod_data_subplot_srv("plot_02", r, data_seg_all, "data_plot02")
  mod_data_subplot_srv("plot_03", r, data_seg_all, "data_plot03")
  #
  mod_plot_overall_srv("plot_01", r)
  mod_plot_subplot_srv("plot_02", r, "data_plot02", "sub_01")
  mod_plot_subplot_srv("plot_03", r, "data_plot03", "sub_02")

  data_logistics_all <- DataLogistics$new(data_seg_all$get_data_segmentation())
  mod_logistic_regression_specs_01_srv("logistic_reg_01",
                                       data_logistics_all,
                                       data_seg_all$get_data_segmentation(),
                                       "logistic_out_01")
}
