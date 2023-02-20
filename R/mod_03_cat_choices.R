#' cat_choices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cat_choices_ui <- function(id){
  ns <- shiny::NS(id)
  shiny.semantic::flow_layout(
    shiny::tagList(
      add_header("Q36 - Kjennskap til 'Digitalt på vei':",
                 size = 4, EMPHASIZE = TRUE, UNDERLINE = FALSE),
      add_header("Kod om til 'ja' hvis",
                 size = 5, EMPHASIZE = FALSE),
      shiny.semantic::multiple_radio(ns("cat_Q36"),
                                     "",
                                     choices = c("Ja, kjenner litt, noe og godt til",
                                                 "Ja, kjenner noe og godt til",
                                                 "Ja, kjenner godt til"),
                                     choices_value = list(2:4, c(3, 4), 4),
                                     selected = list(2:4),
                                     position = "grouped")),
    shiny::tagList(
      add_header("Q37 - Forståelse av kompetansekrav:",
                 size = 4, EMPHASIZE = TRUE, UNDERLINE = FALSE),
      add_header("Kodes om til 'ja' hvis",
                 size = 5, EMPHASIZE = FALSE),
      shiny.semantic::multiple_radio(ns("cat_Q37"),
                                     "",
                                     choices = c("Ja, kjenner litt, noe og godt til",
                                                 "Ja, kjenner noe og godt til",
                                                 "Ja, kjenner godt til"),
                                     choices_value = list(2:4, c(3, 4), 4),
                                     selected = list(2:4),
                                     position = "grouped")),
    shiny::tagList(
      add_header("Q38 - Gjennomføring av programmet:",
                 size = 4, EMPHASIZE = TRUE, UNDERLINE = FALSE),
      add_header("Kodes om til 'ja' hvis",
                 size = 5, EMPHASIZE = FALSE),
      shiny.semantic::multiple_radio(ns("cat_Q38"),
                                     "",
                                     choices = c("Ja, har gjennomført noe, mye og alt",
                                                 "Ja, har gjennomført mye og alt",
                                                 "Ja, har gjennomført alt"),
                                     choices_value = list(2:4, c(3, 4), 4),
                                     selected = list(2:4),
                                     position = "grouped")),
    shiny::tagList(
      add_header("Q40 - Omfanget av læring:",
                 size = 4, EMPHASIZE = TRUE, UNDERLINE = FALSE),
      add_header("Kodes om til 'ja' hvis",
                 size = 5, EMPHASIZE = FALSE),
      shiny.semantic::multiple_radio(ns("cat_Q40"),
                                     "",
                                     choices = c("I liten, noen, og stor grad",
                                                 "I noen og stor grad",
                                                 "I stor grad"),
                                     choices_value = list(2:4, c(3, 4), 4),
                                     selected = list(2:4),
                                     position = "grouped")),
    min_cell_width = "250px",
    max_cell_width = "250px"
  )
}
mod_cat_choices_data_srv <- function(id, data_set_seg_all) {
  shiny::moduleServer(id, function(input, output, session) {
    sttgs_fact <- list(ADJUST_LABELS = TRUE,
                       AS_FACTOR = TRUE,
                       ORDERED = FALSE,
                       ADD_LABELS = TRUE)
    shiny::reactive({
      list_recodes_taken <- get_list_cat_choices(input[["cat_Q36"]],
                                                 input[["cat_Q37"]],
                                                 input[["cat_Q38"]],
                                                 input[["cat_Q40"]])
      data_segs_cat_all <- data_set_seg_all()
      data_segs_cat_all[["data_2023"]] <- data_segs_cat_all[["data_2023"]] %>%
        TaskAnalyticsTB::recode_qXX_cats(q_names = c("Q36", "Q37",
                                                     "Q38", "Q40"),
                                         list_recodes = list_recodes_taken,
                                         new_names = c("Q36_c", "Q37_c",
                                                       "Q38_c", "Q40_c"),
                                         SETTINGS_FACT = sttgs_fact)
      data_segs_cat_all
    })
  })
}
get_list_cat_choices <- function(input_q36, input_q37, input_q38, input_q40) {
  list_cat_choices <- list(
    Q36 = list(
      `Nei, kjenner ikke til` = "Nei",
      `Ja, kjenner litt til` = "Ja",
      `Ja, kjenner noe til` = "Ja",
      `Ja, kjenner godt til` = "Ja"),
    Q37 = list(
      `Nei, kjenner ikke til` = "Nei",
      `Ja, kjenner litt til` = "Ja",
      `Ja, kjenner noe til` = "Ja",
      `Ja, kjenner godt til` = "Ja"),
    Q38 = list(
      `Nei, har ikke startet` = "Nei",
      `Ja, har gjennomført noe` = "Ja",
      `Ja, har gjennomført mye` = "Ja",
      `Ja, har gjennomført alt` = "Ja"),
    Q40 = list(
      `Ingen ting` = "Nei",
      `I liten grad` = "Ja",
      `I noen grad` = "Ja",
      `I stor grad` = "Ja")
  )
  list_cat_choices <- list_cat_choices

  input_q36_taken <- eval(parse(text = input_q36))
  input_q37_taken <- eval(parse(text = input_q37))
  input_q38_taken <- eval(parse(text = input_q38))
  input_q40_taken <- eval(parse(text = input_q40))

  list_cat_choices$Q36 <-update_cat_choices(input_q36_taken,
                                            list_cat_choices$Q36)
  list_cat_choices$Q37 <-update_cat_choices(input_q37_taken,
                                            list_cat_choices$Q37)
  list_cat_choices$Q38 <-update_cat_choices(input_q38_taken,
                                            list_cat_choices$Q38)
  list_cat_choices$Q40 <-update_cat_choices(input_q40_taken,
                                            list_cat_choices$Q40)

  return(list_cat_choices)
}
update_cat_choices <- function(id_to_change_Ja, names_choices) {
  choices_vec_used <- rep("Nei", time = 4)
  choices_vec_used[id_to_change_Ja] <- "Ja"
  choices_vec_used <- as.list(choices_vec_used)
  names(choices_vec_used) <- names(names_choices)
  choices_vec_used
}