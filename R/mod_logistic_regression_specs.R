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
  shiny::tagList(
    shiny.semantic::flowLayout(
      shiny::tags$h5(paste0("Velg ", "\u00e5", "rstall")),
      shiny.semantic::selectInput(ns("slider_year"),
                                  label = "",
                                  choices = c(2021, 2022, 2023),
                                  selected = 2021,
                                  multiple = TRUE,
                                  width = "auto")
    ),
    shiny.semantic::flowLayout(
      shiny::tags$h5(paste0("Spesifiser kompetanseomr", "\u00e5", "de:")),
      shiny.semantic::selectInput(ns("slider_dep"),
                                  "",
                                  choices = settings_logistic$lab_dep_choices,
                                  multiple = FALSE)
    ),
    shiny.semantic::flowLayout(
      shiny::tags$h5(paste0("Spesifiser erfaringsniv", "\u00e5",
                            " som skal analyseres:")),
      shiny.semantic::selectInput(ns("slider_exp"),
                                  "",
                                  choices = settings_logistic$var_exp_choices,
                                  multiple = FALSE)
    ),
    break_vspace("small"),
    shiny.semantic::flowLayout(
      shiny::tags$h5(paste0("Spesifiser regressorvariablerl:")),
      shiny.semantic::selectInput(ns("slider_reg"),
                                  "",
                                  choices = settings_logistic$var_reg_choices,
                                  selected = settings_logistic$var_reg_choices[1],
                                  multiple = TRUE,
                                  width = '800px')
    )
  )
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
    ns <- session$ns
    shiny::observeEvent(check_nobs, {
      nobs <- check_nobs()
      shiny.semantic::update_numeric_input(session,
                                           "slider_num_training",
                                           max = nobs)
    })
  })
}
#' logistic_regression_specs Server Functions
#'
#' @noRd
mod_logistic_regression_specs_01_srv <- function(id, data_set){
  check_reactive_inputs(data_set)
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
get_data_logistics_all <- function(data_set_list, years) {
  names_ds <- paste0("data_", years)
  if (length(names_ds) > 1) {
    data_chosen  <- data_set_list[names_ds]
    names_chosen <- lapply(data_chosen, names)
    names_vars_both <- Reduce(function(x, y) {
      intersect(x, y)
    }, names_chosen)
    data_out <- lapply(data_chosen, function(x, var_names) {
      x[var_names]
    }, var_names = names_vars_both)
    for (i in 1:length(names_ds)) {
      data_out[[i]]$year <- years[i]
    }
    data_out <- Reduce(rbind, data_out)
    data_out$year <- factor(data_out$year)
    data_out <- data_out %>%
      dplyr::distinct(dplyr::pick(dplyr::all_of(setdiff(names(data_out),
                                                        "year"))),
                      .keep_all = TRUE)
    return(data_out)
  } else {
    data_chosen <- data_set_list[[names_ds]]
    return(data_chosen)
  }
}
deparse_input_logistic_to_model <- function(dep, reg, exp) {
  dep_taken <- settings_logistic$var_dep_choices[which(settings_logistic$lab_dep_choices %in% dep)]
  mod_deparsed <- list(dependent = dep_taken,
                       regressors = reg)

  experience_all1 <- "Uerfaren og Grunnleggende"
  experience_all2 <- paste0("Grunnleggende og ", paste0("Videreg",
                                                        "\u00e5",
                                                        "ende"))
  experience_all3 <- paste0(paste0("Videreg", "\u00e5", "ende og "),
                            "Avansert")

  if (exp == experience_all1) exp_taken <-  c("Uerfaren", "Grunnleggende");
  if (exp == experience_all2) exp_taken <-  c("Grunnleggende", paste0("Videreg",
                                                                      "\u00e5","ende"));
  if (exp == experience_all3) exp_taken <-  c(paste0("Videreg",
                                                     "\u00e5","ende"),
                                              "Avansert");

  mod_deparsed$experience <- exp_taken
  return(mod_deparsed)
}
