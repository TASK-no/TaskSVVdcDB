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
    shiny.semantic::flow_layout(
      shiny::htmlOutput(ns("plotROC")),
      shiny::htmlOutput(ns("roc_text")),
      min_cell_width = "375px",
      column_gap = "50px")
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
        df_tmp <- data.frame(true_ones = true_ONES, preds = predictions)
        roc_plot <- ggplot2::ggplot(
          df_tmp,
          ggplot2::aes(d = .data$true_ones, m = .data$preds)) +
          plotROC::geom_roc(labelround = 2, n.cuts = 15)
        roc_plot <- roc_plot +
          plotROC::style_roc(theme = ggplot2::theme_grey) +
          ggplot2::ggtitle("ROC curve") +
          ggplot2::annotate("text", x = .75, y = .25,
                            label = paste(
                              "AUC =",
                              round(plotROC::calc_auc(roc_plot)$AUC, 4))) +
          ggplot2::scale_x_continuous("1 - Specificity (FPR)") +
          ggplot2::scale_y_continuous("Sensitivity (TPR)")
        output$plotROC <- shiny::renderUI({
          htmltools::HTML(
            plotROC::export_interactive_roc(roc_plot)
            )})
        output$roc_text <- shiny::renderUI({
          htmltools::HTML(get_roc_text())
          })
      } else {
        output$plotROC  <- NULL
        output$roc_text <- shiny::renderUI({
          htmltools::HTML("")
        })
      }
      }
    )
  })
}
## To be copied in the UI
# mod_07_B_logistics_classification_run_ui("07_B_logistics_classification_run_1")

## To be copied in the server
# mod_07_B_logistics_classification_run_server("07_B_logistics_classification_run_1")
