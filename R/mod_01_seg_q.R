#' mod_seg_q UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_seg_q_ui <- function(id, num_q, title_text, sttgs = NULL) {
  stopifnot(!is.null(sttgs))
  ns <- shiny::NS(id)

  title_used <- paste0("Q", num_q, " - ", title_text, ":")
  sub_ns <- get_sub_ns(id_prefix = ns(""), num_q)

  competence_types <- list("grun", "vide", "avan")
  list_titles <- list(
    grun = "Grunnlegende",
    vide = paste0("Videreg", "\u00e5", "ende:"),
    avan = "Avansert:"
  )
  htmltools::tagList(
    add_header(title_used, size = 4, EMPHASIZE = TRUE),
    lapply(competence_types, generate_q_ui,
      list_titles,
      sttgs = sttgs,
      sub_ids = sub_ns
    ),
    break_vspace("small")
  )
}
generate_q_ui <- function(competence_type, titles, sttgs, sub_ids) {
  title_taken <- titles[[competence_type]]
  value_sum <- sttgs[[competence_type]]$sum_score_val
  value_type <- sttgs[[competence_type]]$type_val
  sub_id_sum <- sub_ids[["sub_ns_sum"]][[competence_type]]
  sub_id_type <- sub_ids[["sub_ns_type"]][[competence_type]]
  shiny::tagList(
    add_header(title_taken, size = 5, EMPHASIZE = TRUE),
    shiny.semantic::flow_layout(
      shiny.semantic::numericInput(sub_id_sum,
        "Total/Sum poengsum: ",
        value = value_sum,
        min = 1, max = 7, width = "150px"
      ),
      shiny.semantic::multiple_radio(sub_id_type,
        "Terskeltype",
        choices = c(
          "større/lik enn (>=):",
          "lik (=):"
        ),
        choices_value = c("larger-equal", "equal"),
        selected = value_type
      )
    )
  )
}
#' mod_seg_q Server Functions
#'
#' @noRd
mod_seg_q_srv <- function(id, num_q) {
  shiny::moduleServer(id, function(input, output, session) {
    sub_ns <- get_sub_ns(num_q = num_q)
    ns_sum <- sub_ns$sub_ns_sum
    ns_type <- sub_ns$sub_ns_type
    shiny::reactive({
      get_segmentation_sub_settings(
        input[[ns_sum[["grun"]]]],
        input[[ns_sum[["vide"]]]],
        input[[ns_sum[["avan"]]]],
        input[[ns_type[["grun"]]]],
        input[[ns_type[["vide"]]]],
        input[[ns_type[["avan"]]]]
      )
    })
  })
}
