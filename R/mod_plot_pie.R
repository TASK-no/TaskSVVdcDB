#' plot_overall UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_subplot_ui <- function(id, year, name_plot_out) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h4(paste0("Delplott: velg type og ","\u00e5", "rstall")),
    shiny.semantic::flow_layout(
      shiny.semantic::selectInput(ns("slider_year"),
                                  label = paste0("velg ", "\u00e5", "rstall"),
                                  choices = c(2021, 2022),
                                  width = "75px"),
      shiny.semantic::selectInput(ns("slider_type"),
                                  label = "velg type",
                                  choices = c(`Ekspertiseomrader inndelt i kompetanse` = "type_pie",
                                              `Kompetanse inndelt i kompetanseomr` = "type_bar"),
                                  width = "320px"),
      min_cell_width = "75px",
      max_cell_width = "340px"
    ),
      # shiny.semantic::selectInput(ns("slider_year"),
      #                             label = paste0("velg ", "\u00e5", "rstall"),
      #                             choices = c(2021, 2022),
      #                             width = "75px"),
      # shiny.semantic::selectInput(ns("slider_type"),
      #                             label = "velg type",
      #                             choices = c(`Ekspertiseomrader inndelt i kompetanse` = "type_pie",
      #                                         `Kompetanse inndelt i kompetanseomr` = "type_bar"),
      #                             width = "320px"),
    mod_break_vspace("small"),
    plotly::plotlyOutput(ns(name_plot_out),
                         width = "1100px",
                         height = "700px",)
  )
}
#' mod_plot_pie Server Functions
#'
#' @noRd
mod_plot_subplot_server <- function(id, data_sets, name_plot_out){
  stopifnot(all(sapply(data_sets, shiny::is.reactive)))
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns
    output[[name_plot_out]] <- plotly::renderPlotly({
      year_taken <- input[["slider_year"]]
      if (input[["slider_type"]] == "type_pie") {
        df <- data_sets[[year_taken]]()[[1]]
        plot_out <- TaskAnalyticsTB::plot_pie_figures(df, year = year_taken,
                                                      return_type = "shinyDB")
        generate_plotly(plot_out,  list(font = list(size = 12)))
      } else if (input[["slider_type"]] == "type_bar") {
        df <- data_sets[[year_taken]]()[[2]]
        data_rader <- TaskAnalyticsTB::get_data_summary_radar(df)
        plot_out <- TaskAnalyticsTB::plot_radar(data_rader, year_taken)
        generate_plotly(plot_out, list(title = "",
                                       visible = FALSE,
                                       orientation = "h",
                                       x = 0,
                                       y = -0.25,
                                       font = list(size = 12)))
      }
    })
  })
}
