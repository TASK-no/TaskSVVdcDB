#' logistic_regression_specs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_logistic_regression_specs_02_ui <- function(id, num_obs) {
  ns <- shiny::NS(id)
  shiny::tagList(
    add_header(
      paste0(
        "Prediksjoner basert p",
        "\u00ec ",
        "logistisk klassifisering: "
      ),
      size = 5, UNDERLINE = TRUE, EMPHASIZE = TRUE
    ),
    break_vspace("small"),
    shiny.semantic::numeric_input(ns("slider_num_training"),
      label = "",
      value = 1000,
      min = 0,
      max = 2000
    )
  )
}
#' logistic_regression_specs Server Functions
#'
#' @noRd
mod_logistic_regression_specs_02_srv <- function(id, check_nobs) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(check_nobs, {
      nobs <- check_nobs()
      shiny.semantic::update_numeric_input(session,
        "slider_num_training",
        max = nobs
      )
    })
  })
}
