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
  title_used <- paste0("Q", num_q, " - ", title_text, ":")
  sub_ns      <- get_sub_ns(num_q)
  sub_ns_sum  <- sub_ns$sub_ns_sum
  sub_ns_type <- sub_ns$sub_ns_type
  ns <- shiny::NS(id)
  htmltools::tagList(
    add_header(title_used, size = 4, EMPHASIZE = TRUE),
    generate_q_ui(tile = "Grunnlegende",
                  value_sum = sttgs$sum_score_val_grun,
                  value_type = sttgs$type_val_grun,
                  sub_id_sum = ns(sub_ns_sum[1]),
                  sub_id_type = ns(sub_ns_type[1])),
    generate_q_ui(tile = paste0("Videreg", "\u00e5", "ende:"),
                  value_sum = sttgs$sum_score_val_vide,
                  value_type = sttgs$type_val_vide,
                  sub_id_sum = ns(sub_ns_sum[2]),
                  sub_id_type = ns(sub_ns_type[2])),
    generate_q_ui(tile = "Avansert:",
                  value_sum = sttgs$sum_score_val_avan,
                  value_type = sttgs$type_val_avan,
                  sub_id_sum = ns(sub_ns_sum[3]),
                  sub_id_type = ns(sub_ns_type[3])),
    break_vspace("small")
  )
}
generate_q_ui <- function(title, value_sum, value_type, sub_id_sum, sub_id_type) {
  shiny::tagList(
    add_header(title, size = 5, EMPHASIZE = TRUE),
    shiny.semantic::flow_layout(
      shiny.semantic::numericInput(sub_id_sum,
                                   "Total/Sum poengsum: ",
                                   value = value_sum,
                                   min = 1, max = 7, width = "150px"),
      shiny.semantic::multiple_radio(sub_id_type,
                                     "Terskeltype",
                                     choices = c("større/lik enn (>=):",
                                                 "lik (=):"),
                                     choices_value = c("larger-equal", "equal"),
                                     selected = value_type)
    )
  )
}
#' mod_seg_q Server Functions
#'
#' @noRd
mod_seg_q_srv <- function(id, num_q) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    sub_ns      <- get_sub_ns(num_q)
    sub_ns_sum  <- sub_ns$sub_ns_sum
    sub_ns_type <- sub_ns$sub_ns_type
    shiny::reactive({
      get_segmentation_sub_settings(sum_score_val_grun = input[[sub_ns_sum[1]]],
                                    sum_score_val_vide = input[[sub_ns_sum[2]]],
                                    sum_score_val_avan = input[[sub_ns_sum[3]]],
                                    type_val_grun = input[[sub_ns_type[1]]],
                                    type_val_vide = input[[sub_ns_type[2]]],
                                    type_val_avan = input[[sub_ns_type[3]]])
    })
  })
}
get_sub_ns <- function(num_q) {
  list(sub_ns_sum = paste0("seg_q", num_q, "_",
                           paste0(c("grun", "videre", "avan"), "_sum")),
       sub_ns_type = paste0("seg_q", num_q, "_",
                            paste0(c("grun", "videre", "avan"), "_type")))
}
