#' seg_competence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_seg_competence_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    add_header("Tilgjengelig digital kompetanse:", size = 4, EMPHASIZE = TRUE),
    shiny.semantic::multiple_radio(ns("seg_digital_comp"),
      "",
      choices = c(
        "mye erfaring",
        "noe erfaring og mye erfaring",
        "lite erfaring, noe erfaring og mye erfaring"
      ),
      choices_value = list(4, c(3, 4), c(2, 3, 4)),
      selected = list(c(3, 4)),
      position = "grouped"
    )
  )
}
#' seg_competence Server Functions
#'
#' @noRd
mod_seg_competence_srv <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      eval(parse(text = input[["seg_digital_comp"]]))
    })
  })
}
