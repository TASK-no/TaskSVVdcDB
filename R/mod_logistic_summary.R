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
    htmltools::tags$head(htmltools::tags$style(paste0("#", ns(name_log_out),
                                                      "{color: black;
                                                        font-size: 16px;
                                                        font-style: verbatim;}")
    )
    ),
    shiny::verbatimTextOutput(ns(paste0(name_log_out, "_output"))),
    break_vspace("medium"),
    add_header("Odds ratioer:",
               size = 5, UNDERLINE = TRUE, EMPHASIZE = TRUE),
    shiny::verbatimTextOutput(ns(paste0(name_log_out, "_odds")))
  )
}
#' logistic_summary Server Functions
#'
#' @noRd
mod_logistic_summary_srv <- function(id, r, name_log_out){
  # check_reactive_inputs(r$datasets$data_log01)
  shiny:: moduleServer( id, function(input, output, session){
    shiny::reactive({
      data_log <- r$datasets$data_log01()
      data_set <- data_log[["data_chosen"]]
      mod_spec <- data_log[["model_specs"]]
      log_out_all <- TaskAnalyticsTB::logistic_learn(data_set,
                                                     model = mod_spec,
                                                     type = "shinyDB")
      log_out <- log_out_all
    })
    output[[paste0(name_log_out, "_output")]] <- shiny::renderPrint({
      log_out()[[1]]
    })
    output[[paste0(name_log_out, "_odds")]] <- shiny::renderPrint({
      log_out()[[2]]
    })
  })
}
