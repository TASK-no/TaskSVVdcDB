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
  r <- shiny::reactiveValues(
    seg_inputs = shiny::reactiveValues(),
    datasets = shiny::reactiveValues()
  )
  gargoyle::init(
    "seg_inputs", "data_seg", "data_cat", "logistics_run",
    "compute_data_plot02", "compute_data_plot03", "action_data_regenerate",
    "action_data_download_show"
  )

  data_seg_all <- DataSegmentation$new(r)
  data_logistics_all <- DataLogistics$new(data_seg_all$get_data_segmentation())
  mod_01_seg_all_server("segmentation", r)
  mod_data_segmentation_srv("segmentation",
    r = r,
    data_seg = data_seg_all
  )

  mod_cat_choices_data_srv("cat_inputs", data_seg_all)

  mod_data_overall_srv("plot_01", r, data_seg_all)
  mod_data_subplot_srv("plot_02", r, data_seg_all, "data_plot02")
  mod_data_subplot_srv("plot_03", r, data_seg_all, "data_plot03")

  mod_observe_subplot_srv("plot_02")
  mod_observe_subplot_srv("plot_03")

  mod_plot_overall_srv("plot_01", r)
  mod_plot_subplot_srv("plot_02", r, "data_plot02", "sub_01")
  mod_plot_subplot_srv("plot_03", r, "data_plot03", "sub_02")

  mod_logistic_regression_specs_01_srv(
    "logistic_reg_01",
    data_logistics_all,
    data_seg_all,
    "logistic_out_01"
  )
  mod_logistic_regression_specs_02_srv("logistic_reg_01",
                                       data_logistics_all)
  mod_logistics_classification_run_server("logistic_cls_01",
                                          data_logistics_all)
  mod_08_data_download_server("data_download", r, data_seg_all, data_logistics_all)
}
