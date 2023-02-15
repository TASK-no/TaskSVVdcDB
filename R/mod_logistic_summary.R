#' logistic_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_logistic_summary_ou <- function(id, name_log_out){
  ns <- shiny::NS(id)
  shiny::tagList(
    add_header("Estimerte effekter av logistisk regresjon: ",
               size = 5, UNDERLINE = TRUE, EMPHASIZE = TRUE),
    break_vspace("small"),
    htmltools::tags$head(htmltools::tags$style(paste0("#", ns(name_log_out),
                                                      "{color: black;
                                                        font-size: 16px;
                                                        font-style: verbatim;}")
    )
    ),
    shiny::verbatimTextOutput(ns(name_log_out))
  )
}
#' logistic_summary Server Functions
#'
#' @noRd
mod_logistic_summary_srv <- function(id, data_log01, name_log_out){
  check_reactive_inputs(data_log01)
  shiny:: moduleServer( id, function(input, output, session){
    ns <- session$ns
    log_out <- shiny::reactive({
      data_log <- data_log01()
      data_set <- data_log[["data_chosen"]]
      mod_spec <- data_log[["model_specs"]]
      log_out_all <- TaskAnalyticsTB::logistic_learn(data_set,
                                                     model = mod_spec,
                                                     type = "shinyDB")
      log_out_all[[1]]
    })
    output[[name_log_out]] <- shiny::renderPrint({
      log_out()
    })
  })
}
