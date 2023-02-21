#' logistic_regression_specs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_logistic_regression_specs_01_ui <- function(id) {
  ns <- shiny::NS(id)
  list_ids_ui <- list("yrs", "dep", "exp", "reg")
  list_sub_ns <- list(yrs = ns("slider_year"),
                      dep = ns("slider_dep"),
                      exp = ns("slider_exp"),
                      reg = ns("slider_reg"))
  list_titles <- list(yrs = paste0("Velg ", "\u00e5", "rstall"),
                      dep = paste0("Spesifiser kompetanseomr", "\u00e5", "de:"),
                      exp = paste0("Spesifiser erfaringsniv", "\u00e5",
                                   " som skal analyseres:"),
                      reg = paste0("Spesifiser erfaringsniv", "\u00e5",
                                   " som skal analyseres:"))
  list_choices <- list(yrs = c(2021, 2022, 2023),
                       dep = settings_logistic$lab_dep_choices,
                       exp = settings_logistic$var_exp_choices,
                       reg = settings_logistic$var_reg_choices)
  list_selected <- list(yrs = 2021,
                        dep = NULL,
                        exp = NULL,
                        reg = settings_logistic$var_reg_choices[1])
  list_multiple <- list(yrs = TRUE,
                        dep = FALSE,
                        exp = FALSE,
                        reg = FALSE)
  list_out_all <- lapply(list_ids_ui, generate_log_specs_ui,
                         list_sub_ns, list_titles,
                         list_choices,list_selected,
                         list_multiple)
  do.call(shiny::tagList, list_out_all)
}
generate_log_specs_ui <- function(input_type, sub_ns, title,
                                  choice, select, mult) {
  shiny.semantic::flowLayout(
    add_header(title[[input_type]], size = 5),
    shiny.semantic::selectInput(sub_ns[[input_type]],
                                "",
                                choices = choice[[input_type]],
                                selected = select[[input_type]],
                                multiple = mult[[input_type]])
  )
}
#' logistic_regression_specs Server Functions
#'
#' @noRd
mod_logistic_regression_specs_01_srv <- function(id, data_set){
  check_reactive_inputs(data_set)
  shiny::moduleServer(id, function(input, output, session) {
    log_out <- shiny::reactive({
      data_chosen <- get_data_logistics_all(data_set(), input[["slider_year"]])
      model_specs <- deparse_input_logistic_to_model(dep = input[["slider_dep"]],
                                                     reg = input[["slider_reg"]],
                                                     exp = input[["slider_exp"]])
      list(data_chosen = data_chosen,
           model_specs = model_specs)
    })
  })
}
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
    add_header(paste0("Prediksjoner basert p",
                      "\u00ec ",
                      "logistisk klassifisering: "),
               size = 5, UNDERLINE = TRUE, EMPHASIZE = TRUE),
    break_vspace("small"),
    shiny.semantic::numeric_input(ns("slider_num_training"),
                                  label = "",
                                  value = 1000,
                                  min = 0,
                                  max = 2000)
  )
}
#' logistic_regression_specs Server Functions
#'
#' @noRd
mod_logistic_regression_specs_02_srv <- function(id, check_nobs){
  shiny::moduleServer(id, function(input, output, session){
    shiny::observeEvent(check_nobs, {
      nobs <- check_nobs()
      shiny.semantic::update_numeric_input(session,
                                           "slider_num_training",
                                           max = nobs)
    })
  })
}
