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

  data_seg_2021 <- mod_seg_data_server("segmentation_inputs",
                                       data_raw_SVV_2021, settingsDC,
                                       settingsQ16, settingsQ17,
                                       settingsQ14, settingsQ19)
  data_seg_2022 <- mod_seg_data_server("segmentation_inputs",
                                       data_raw_SVV_2022, settingsDC,
                                       settingsQ16, settingsQ17,
                                       settingsQ14, settingsQ19)
  output$table_2021 <- reactable::renderReactable({
    reactable::reactable(data_seg_2021())
  })

  # Your application server logic
}
