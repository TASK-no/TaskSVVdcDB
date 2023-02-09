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
    htmltools::h3(tags$u(tags$em(title_used))),
    htmltools::h5(tags$u(tags$em("Grunnlegende:"))),
    shiny.semantic::flow_layout(
      htmltools::tagList(
        shiny.semantic::numericInput(ns(sub_ns_sum[1]),
                                     "Total/Sum poengsum: ",
                                     value = sttgs$sum_score_val_grun,
                                     min = 1, max = 7, width = "150px")
      ),
      htmltools::tagList(
        shiny.semantic::multiple_radio(ns(sub_ns_type[1]),
                                       "Terskeltype",
                                       choices = c("larger/equal than (>=):",
                                                   "equal to (=):"),
                                       choices_value = c("larger-equal", "equal"),
                                       selected = sttgs$type_val_grun)
        )
    ),
    htmltools::h5(tags$u(tags$em(paste0("Videreg",
                                        "\u00e5",
                                        "ende:")))),
    shiny.semantic::flow_layout(
      htmltools::tagList(
        shiny.semantic::numericInput(ns(sub_ns_sum[2]),
                                     "Total/Sum poengsum: ",
                                     value = sttgs$sum_score_val_vide,
                                     min = 1, max = 7, width = "150px")
      ),
      htmltools::tagList(
        shiny.semantic::multiple_radio(ns(sub_ns_type[2]),
                                       "Terskeltype",
                                       choices = c("larger/equal than (>=):",
                                                   "equal to (=):"),
                                       choices_value = c("larger-equal", "equal"),
                                       selected = sttgs$type_val_vide)
      )
    ),
    htmltools::h5(tags$u(tags$em("Avansert:"))),
    shiny.semantic::flow_layout(
      htmltools::tagList(
        shiny.semantic::numericInput(ns(sub_ns_sum[3]),
                                     "Total/Sum poengsum: ",
                                     value = sttgs$sum_score_val_avan,
                                     min = 1, max = 7, width = "150px")
      ),
      htmltools::tagList(
        shiny.semantic::multiple_radio(ns(sub_ns_type[3]),
                                       "Terskeltype",
                                       choices = c("larger/equal than (>=):",
                                                   "equal to (=):"),
                                       choices_value = c("larger-equal", "equal"),
                                       selected = sttgs$type_val_avan)
      )
    )
  )
}
#' mod_seg_q Server Functions
#'
#' @noRd
mod_seg_q_server <- function(id, num_q) {
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
## To be copied in the UI
# mod_mod_seg_q_ui("mod_seg_q_1")

## To be copied in the server
# mod_mod_seg_q_server("mod_seg_q_1")
