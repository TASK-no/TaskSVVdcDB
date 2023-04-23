#' logistic_regression_specs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_logistic_regression_specs_02_ui <- function(id) {
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
    shiny::tagList(
      shiny.semantic::flow_layout(
        shiny::tagList(
          htmltools::HTML("Antall prediksjonseksempler:"),
          shiny.semantic::numeric_input(ns("slider_num_training"),
                                        label = "",
                                        value = 1000,
                                        min = 0,
                                        max = 2000)
        ),
        shiny::tagList(
          htmltools::HTML("Antall prediksjonseksempler:"),
          shiny.semantic::numeric_input(ns("slider_num_prediction"),
                                        label = "",
                                        value = 925,
                                        min = 0,
                                        max = 2000))
      )
    )
  )
}
#' logistic_regression_specs Server Functions
#'
#' @noRd
mod_logistic_regression_specs_02_srv <- function(id, class_data_log) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(
      gargoyle::watch("logistics_run"),
      {
        shiny.semantic::update_numeric_input(
          session,
          "slider_num_training",
          max = class_data_log$get_num_obs("all")
        )
        shiny.semantic::update_numeric_input(
          session,
          "slider_num_prediction",
          max = class_data_log$get_num_obs("all")
        )
      }
    )
    shiny::observeEvent(
      input[["slider_num_training"]],
      {
        class_data_log$update_num_obs_train_prdct(
          input[["slider_num_training"]],
          type = "train")
        if (input[["slider_num_prediction"]] != class_data_log$get_num_obs("prdct")) {
          shiny.semantic::update_numeric_input(
            session,
            "slider_num_prediction",
            value = class_data_log$get_num_obs("prdct")
          )
        }
      }
    )
    shiny::observeEvent(
      input[["slider_num_prediction"]],
      {
        class_data_log$update_num_obs_train_prdct(
          input[["slider_num_prediction"]],
          type = "prdct")
        if (input[["slider_num_training"]] != class_data_log$get_num_obs("train")) {
          shiny::isolate(shiny.semantic::update_numeric_input(
            session,
            "slider_num_training",
            value = class_data_log$get_num_obs("train")
          ))
        }
      }
    )
  })
}
