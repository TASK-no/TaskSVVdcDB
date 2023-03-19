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
#' logistic_regression_specs Server Functions
#'
#' @noRd
mod_logistic_regression_specs_01_srv <- function(id,
                                                 data_logistics,
                                                 data_seg,
                                                 name_log_out){
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(
      {
        gargoyle::watch("data_cat")
      },
      {
        data_logistics$update_data_base(data_seg)
      }
    )
    shiny::observeEvent(
      {
        gargoyle::watch("data_cat")
        input[["slider_year"]]
      },
      {
        data_logistics$update_yrs(input[["slider_year"]])
      }
    )
    shiny::observeEvent(
      {
        input[["slider_dep"]];
        input[["slider_reg"]];
        input[["slider_exp"]];
      },
      {
        data_logistics$update_mod(
          dep = input[["slider_dep"]],
          reg = input[["slider_reg"]],
          exp = input[["slider_exp"]])
      }
    )
    shiny::observeEvent(
      {
        gargoyle::watch("data_cat");
        input[["slider_year"]]
        input[["slider_dep"]];
        input[["slider_reg"]];
        input[["slider_exp"]];
      },
      {
        log_out <- TaskAnalyticsTB::logistic_learn(
          data_set = data_logistics$get_data_logistics(),
          model = data_logistics$get_model_logistics(),
          type = "shinyDB")
        output[[paste0(name_log_out, "_output")]] <- shiny::renderPrint({
          log_out[[1]]
        })
        output[[paste0(name_log_out, "_odds")]] <- shiny::renderPrint({
          log_out[[2]]
        })
      }
      )
  })
}
