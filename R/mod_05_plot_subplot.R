#' plot_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_subplot_ui <- function(id, name_plot_out) {
  FILTER_TEXT <- "Filter for ansatte som er"
  ns <- shiny::NS(id)
  shiny::tagList(
    add_header(paste0("Delplott: velg type og ","\u00e5", "rstall"),
               size = 3, UNDERLINE = TRUE, EMPHASIZE = TRUE),
    shiny.semantic::flow_layout(
      shiny::tagList(
        shiny::tags$h5(paste(FILTER_TEXT, "'SamAnsi'")),
        shiny.semantic::checkbox_input(ns("slider_samansi"),
                                       is_marked = FALSE,
                                       type = "slider"),
        add_header(paste(FILTER_TEXT, "'Q37'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q37"),
                                       is_marked = FALSE,
                                       type = "slider"),
        add_header(paste0("Velg ", "\u00e5", "rstall"), size = 5),
        shiny.semantic::selectInput(ns("slider_year"),
                                    label = "",
                                    choices = c(2021, 2022, 2023),
                                    width = "75px")
      ),
      shiny::tagList(
        shiny::tags$h5(paste(FILTER_TEXT, "'leder'")),
        shiny.semantic::checkbox_input(ns("slider_leder"),
                                       is_marked = FALSE,
                                       type = "slider"),
        add_header(paste(FILTER_TEXT, "'Q38'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q38"),
                                       is_marked = FALSE,
                                       type = "slider"),
        add_header("Velg type", size = 5),
        shiny.semantic::selectInput(ns("slider_velg"),
                                    label = "",
                                    choices = get_choices_plot_types(),
                                    width = "310px")
      ),
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'Q36'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q36"),
                                       is_marked = FALSE,
                                       type = "slider"),
        add_header(paste(FILTER_TEXT, "'Q40'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q40"),
                                       is_marked = FALSE,
                                       type = "slider"),
        add_header(paste0("Fordelt p", "\u00e5" ," divisjoner"), size = 5),
        shiny.semantic::selectInput(ns("division_type"),
                                    label = "",
                                    choices = get_choices_divisions(kind = "into_area"))
      ),
      min_cell_width = "75px",
      max_cell_width = "320px",
      column_gap = "1em"
    ),
    break_vspace("small"),
    plotly::plotlyOutput(ns(name_plot_out),
                         width = "1100px",
                         height = "700px")
  )
}
mod_observe_subplot_srv <- mod_plot_subplot_srv <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(
      {
        input[["slider_velg"]]
      },
      {
        if (input[["slider_velg"]] == "exp_into_dig") {
          shiny.semantic::updateSelectInput(session = session,
                                            inputId = "division_type",
                                            choices = get_choices_divisions(kind = "into_competence"))
        } else if (input[["slider_velg"]] == "dig_into_exp") {
          shiny.semantic::updateSelectInput(session = session,
                                            inputId = "division_type",
                                            choices = get_choices_divisions(kind = "into_area"))
        }
      }
    )
  })
}
#' mod_plot_pie Server Functions
#'
#' @noRd
mod_plot_subplot_srv <- function(id, r, name_dataset, name_plot_out){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    shiny::observeEvent(
      {
        gargoyle::watch(paste0("compute_", name_dataset))
      },
      {
        output[[name_plot_out]] <- plotly::renderPlotly({
          year_taken <- input[["slider_year"]]
          if (input[["slider_velg"]] == "exp_into_dig") {
            if (input[["division_type"]] == "all") {
              plot_out <- TaskAnalyticsTB::plot_pie_figures(r$datasets[[name_dataset]],
                                                            year = year_taken,
                                                            return_type = "shinyDB")
              generate_plotly(plot_out,  list(font = list(size = 12)))
            } else {
              data_divisions <- r$datasets[[name_dataset]] %>%
                dplyr::filter(.data$category == input[["division_type"]])
              plot_out <- TaskAnalyticsTB::plot_division(data_divisions,
                                                         year_taken,
                                                         input[["division_type"]],
                                                         "Digital kompetanse i divisjonene (per kompetanseområde)")
              generate_plotly(plot_out)
            }
          } else if (input[["slider_velg"]] == "dig_into_exp") {
            if (input[["division_type"]] == "all") {
              data_rader <- TaskAnalyticsTB::get_data_summary_radar(r$datasets[[name_dataset]])
              plot_out <- TaskAnalyticsTB::plot_radar(data_rader,
                                                      year_taken,
                                                      return_type = "shinyDB")
              generate_plotly(plot_out)
            } else {
              data_divisions <- r$datasets[[name_dataset]] %>%
                dplyr::filter(.data$category == input[["division_type"]])
              plot_out <- TaskAnalyticsTB::plot_division(data_divisions, year_taken,
                                                         title = input[["division_type"]],
                                                         y_title = "Kompetanseområde i divisjonene (per digitalt kompetansenivå)")
              generate_plotly(plot_out)
            }
          }
        })
      }
    )
  })
}
mod_data_subplot_srv <- function(id, r, data_seg, name_dataset) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(
      {
        list(
          gargoyle::watch("data_cat"),
          input[["slider_year"]],
          input[["slider_velg"]],
          input[["division_type"]],
          SAMANSI = input[["slider_samansi"]],
          LEDER = input[["slider_leder"]],
          CAT_Q36 = input[["slider_q36"]],
          CAT_Q37 = input[["slider_q37"]],
          CAT_Q38 = input[["slider_q38"]],
          CAT_Q40 = input[["slider_q40"]]
        )
      },
      {
        data_sets <- data_seg$get_data_segmentation()
        num_ds    <- length(data_sets)
        year_seq  <- 2020 + 1:num_ds

        yr_chosen <- as.numeric(input[["slider_year"]])
        ds_chosen <- which(year_seq %in% yr_chosen)
        ds_used   <- data_sets[[ds_chosen]]
        if (input[["slider_velg"]] == "exp_into_dig") {
          if (input[["division_type"]] == "all") {
            type_taken <- "final"
          } else {
            type_taken <- "divisions_competence"
          }
        } else if (input[["slider_velg"]] == "dig_into_exp") {
          if (input[["division_type"]] == "all") {
            type_taken <- "report"
          } else {
            type_taken <- "divisions_area"
          }
        }
        r$datasets[[name_dataset]] <- ds_used %>%
          filter_for_samansi_leder_cat(yr_chosen,
                                       SAMANSI = input[["slider_samansi"]],
                                       LEDER = input[["slider_leder"]],
                                       CAT_Q36 = input[["slider_q36"]],
                                       CAT_Q37 = input[["slider_q37"]],
                                       CAT_Q38 = input[["slider_q38"]],
                                       CAT_Q40 = input[["slider_q40"]]) %>%
          TaskAnalyticsTB::get_data_summary(year = yr_chosen,
                                            type = type_taken)
        gargoyle::trigger(paste0("compute_", name_dataset))
      })
  }
  )
}
