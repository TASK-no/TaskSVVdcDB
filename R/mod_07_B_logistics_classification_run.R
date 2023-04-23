#' 07_B_logistics_classification_run UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
modlogistics_classification_run_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny.semantic::button(ns("run_logistic_classification"),
                           label = "Klassifisering av kjørelogistikk!",
                           shiny.semantic::icon("horizontally flipped cloud")
    ),
    shiny::plotOutput(ns("plotROC"))
  )
}

#' 07_B_logistics_classification_run Server Functions
#'
#' @noRd
mod_logistics_classification_run_server <- function(id, class_logistics_data){
  shiny::moduleServer( id, function(input, output, session) {
    shiny::observeEvent(
      {
        input[["run_logistic_classification"]]
      },
      {
      log_train_out <- TaskAnalyticsTB::logistic_learn(
        data_set = class_logistics_data$get_data_logistics(type = "train"),
        model = class_logistics_data$get_model_logistics(),
        type = "shinyDB"
      )
      data_prediction <- TaskAnalyticsTB:::get_data_for_prediction(
        class_logistics_data$get_data_logistics(type = "prdct"),
        model = class_logistics_data$get_model_logistics())
      true_ONES <- TaskAnalyticsTB:::get_true_ones(data_prediction)

      predictions <- TaskAnalyticsTB:::generate_predictions(
        log_train_out$model_run,
        data_prediction,
        type = "response"
      )

      classification_infos <- TaskAnalyticsTB:::get_cls_infos(
        true_ONES,
        predictions
      )
      if (!(is.null(true_ONES) || is.null(predictions))) {
        output$plotROC <- shiny::renderPlot({
          InformationValue::plotROC(
          true_ONES,
          predictions,
          Show.labels = TRUE)
          })
      } else {
        output$plotROC <- NULL
      }
      }
    )
  })
}
## To be copied in the UI
# mod_07_B_logistics_classification_run_ui("07_B_logistics_classification_run_1")

## To be copied in the server
# mod_07_B_logistics_classification_run_server("07_B_logistics_classification_run_1")
