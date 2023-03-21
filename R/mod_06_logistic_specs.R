#' logistic_regression_specs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_logistic_regression_specs_01_A_ui <- function(id) {
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
                        reg = TRUE)
  list_out_all <- lapply(list_ids_ui, generate_log_specs_ui,
                         list_sub_ns, list_titles,
                         list_choices,list_selected,
                         list_multiple)
  do.call(shiny::tagList, list_out_all)
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
mod_logistic_regression_specs_01_B_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny.semantic::button(ns("run_logistics"),
                         label = "Kjør logistisk regresjon!",
                         shiny.semantic::icon("horizontally flipped cloud"))
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
        data_logistics$update_data_base(data_seg$get_data_segmentation())
        gargoyle::trigger("logistics_run")
      }
    )
    shiny::observeEvent(
      {
        input[["run_logistics"]];
      },
      {
        data_logistics$update_yrs(input[["slider_year"]])
        gargoyle::trigger("logistics_run")
      }
    )
    shiny::observeEvent(
      {
        input[["run_logistics"]];
      },
      {
        data_logistics$update_mod(
          dep = input[["slider_dep"]],
          reg = input[["slider_reg"]],
          exp = input[["slider_exp"]])
        gargoyle::trigger("logistics_run")
      }
    )
    shiny::observeEvent(
      {
        gargoyle::watch("logistics_run");
      },
      {
        log_out <- TaskAnalyticsTB::logistic_learn(
          data_set = data_logistics$get_data_logistics(),
          model = data_logistics$get_model_logistics(),
          type = "shinyDB")
        output[[paste0(name_log_out, "_output")]] <- shiny::renderPrint({
          log_out[["model_summary"]]
        })
        output[[paste0(name_log_out, "_odds")]] <- shiny::renderPrint({
          log_out[["odds_info"]]
        })
        if(isTRUE(log_out[["fail_conv"]])) {
          msg_non_conv <- paste0("Algoritmen konvergerte ikke, noe som ",
                                 "gj", "\u00f8", "r estimeringsresultatene ",
                                 "svært upålitelige! Juster regressorer eller",
                                 "andre variabler og kjør estimeringen på nytt.")
            shiny.semantic::create_modal(shiny.semantic::modal(
              id = "simple-modal",
              header = htmltools::h2("Dette er et viktig budskap!"),
              msg_non_conv))
        }
        if(isTRUE(log_out[["fail_num"]])) {
          msg_non_conv <- paste0("Algoritmen fikk numeriske problemer som gjør",
                                 "estimeringsresultatene svært upålitelige!",
                                 " Juster regressorer eller andre variabler",
                                 "og kjør estimeringen på nytt.")
          shiny.semantic::create_modal(shiny.semantic::modal(
            id = "simple-modal",
            header = htmltools::h2("Dette er et viktig budskap!"),
            msg_non_conv))
        }
      }
    )
  })
}
