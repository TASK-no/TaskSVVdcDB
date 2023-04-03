#' plot_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_overall_ui <- function(id) {
  FILTER_TEXT <- "Filter for ansatte som er"
  ns <- shiny::NS(id)
  shiny::tagList(
    add_header("Samlet Oppsummerende Figur",
               size = 3,
               UNDERLINE = TRUE, EMPHASIZE = TRUE
    ),
    shiny.semantic::flow_layout(
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'SamAnsi'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_samansi"),
                                       is_marked = FALSE,
                                       type = "slider"
        )
      ),
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'leder'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_leder"),
                                       is_marked = FALSE,
                                       type = "slider"
        )
      ),
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'Q36'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q36"),
                                       is_marked = FALSE,
                                       type = "slider"
        )
      ),
      min_cell_width = "75px",
      max_cell_width = "340px"
    ),
    shiny.semantic::flow_layout(
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'Q37'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q37"),
                                       is_marked = FALSE,
                                       type = "slider"
        )
      ),
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'Q38'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q38"),
                                       is_marked = FALSE,
                                       type = "slider"
        )
      ),
      shiny::tagList(
        add_header(paste(FILTER_TEXT, "'Q40'"), size = 5),
        shiny.semantic::checkbox_input(ns("slider_q40"),
                                       is_marked = FALSE,
                                       type = "slider"
        )
      ),
      # ,
      # shiny.semantic::flow_layout(
      #   add_header(paste0("Fordelt p", "\u00e5", " divisjoner"), size = 5),
      #   shiny.semantic::selectInput(ns("division_type"),
      #                               label = "",
      #                               choices = get_choices_divisions(kind = "into_area")
      #   )
      # ),
      min_cell_width = "75px",
      max_cell_width = "340px"
    ),
    plotly::plotlyOutput(ns("plot_overall"),
                         width = "1100px",
                         height = "700px"
    ),
    break_vspace("small")
  )
}
#' plot_overall Server Functions
#'
#' @noRd
mod_plot_overall_srv <- function(id, r) {
  # stopifnot(shiny::is.reactive(r$datasets$data_plot01))
  shiny::moduleServer(id, function(input, output, session) {
    output$plot_overall <- plotly::renderPlotly({
      plot_out <- TaskAnalyticsTB::plot_overall(r$datasets$data_plot01,
                                                return_type = "shinyDB"
      )
      generate_plotly(plot_out)
    })
  })
}
mod_data_overall_srv <- function(id, r, data_seg) {
  # check_reactive_inputs(r$datasets$data_cat)
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(
      {
        list(
          gargoyle::watch("data_cat"),
          input[["slider_samansi"]],
          input[["slider_leder"]],
          input[["slider_q36"]],
          input[["slider_q37"]],
          input[["slider_q38"]],
          input[["slider_q40"]]
          # ,
          # input[["division_type"]]
        )
      },
      {
        data_sets <- data_seg$get_data_segmentation()
        num_ds <- length(data_sets)
        year_seq <- 2020 + 1:num_ds
        ds_list <- vector("list", num_ds)
        type_taken <- ifelse(input[["division_type"]] == "all",
                             "report",
                             "report-divisions")
        type_taken <- "report"
        for (i in seq_len(num_ds)) {
          ds_list[[i]] <- data_sets[[i]] %>%
            filter_for_samansi_leder_cat(year_seq[i],
                                         SAMANSI = input[["slider_samansi"]],
                                         LEDER = input[["slider_leder"]],
                                         CAT_Q36 = input[["slider_q36"]],
                                         CAT_Q37 = input[["slider_q37"]],
                                         CAT_Q38 = input[["slider_q38"]],
                                         CAT_Q40 = input[["slider_q40"]]
            ) %>%
            TaskAnalyticsTB::get_data_summary(year = year_seq[i],
                                              type = type_taken)
          ds_list[[i]]$year <- as.factor(ds_list[[i]]$year)
        }
        r$datasets$data_plot01 <- do.call(
          TaskAnalyticsTB::get_data_joined,
          ds_list
        )
      }
    )
  })
}
