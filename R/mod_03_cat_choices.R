#' cat_choices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cat_choices_A_ui <- function(id){
  ns <- shiny::NS(id)
  list_QXX <- list("Q36", "Q37", "Q38", "Q40")
  list_headers <- list(Q36 = "Q36 - Kjennskap til 'Digitalt på vei':",
                       Q37 = "Q37 - Forståelse av kompetansekrav:",
                       Q38 = "Q38 - Gjennomføring av programmet:",
                       Q40 = "Q40 - Omfanget av læring:")
  list_sub_ns <- list(Q36 = ns("cat_Q36"),
                      Q37 = ns("cat_Q37"),
                      Q38 = ns("cat_Q38"),
                      Q40 = ns("cat_Q40"))
  list_choices <- list(Q36 = c("Ja, kjenner litt, noe og godt til",
                               "Ja, kjenner noe og godt til",
                               "Ja, kjenner godt til"),
                       Q37 = c("Ja, kjenner litt, noe og godt til",
                               "Ja, kjenner noe og godt til",
                               "Ja, kjenner godt til"),
                       Q38 = c("Ja, har gjennomført noe, mye og alt",
                               "Ja, har gjennomført mye og alt",
                               "Ja, har gjennomført alt"),
                       Q40 = c("I liten, noen, og stor grad",
                               "I noen og stor grad",
                               "I stor grad"))
  list_cat_ui <- lapply(list_QXX, generate_cat_ui, list_headers,
                        list_sub_ns, list_choices)
  list_out_all <- c(list_cat_ui,
                    min_cell_width = "250px",
                    max_cell_width = "250px")
  do.call(shiny.semantic::flow_layout, list_out_all)
}
mod_cat_choices_B_ui <- function(id){
  ns <- shiny::NS(id)
  shiny.semantic::button(ns("action_cat_choices"),
                         "Oppdater definisjonen av kategorier!",
                         icon = shiny.semantic::icon("home"))
}
generate_cat_ui <- function(Qxx, headers, sub_ns, choices) {
  shiny::tagList(
    add_header(headers[[Qxx]],
               size = 4, EMPHASIZE = TRUE, UNDERLINE = FALSE),
    add_header("Kodes om til 'ja' hvis",
               size = 5, EMPHASIZE = FALSE),
    shiny.semantic::multiple_radio(sub_ns[[Qxx]],
                                   "",
                                   choices = choices[[Qxx]],
                                   choices_value = list(2:4, c(3, 4), 4),
                                   selected = list(2:4),
                                   position = "grouped"))
}
mod_cat_choices_data_srv <- function(id, data_seg) {
  shiny::moduleServer(id, function(input, output, session) {
    sttgs_fact <- list(ADJUST_LABELS = TRUE,
                       AS_FACTOR = TRUE,
                       ORDERED = FALSE,
                       ADD_LABELS = TRUE)
    shiny::observeEvent(
      {
        list(tmp1 = gargoyle::watch("data_seg"),
             tmp2 = input[["action_cat_choices"]])
      },
      {
        list_recodes_taken <- get_list_cat_choices(input[["cat_Q36"]],
                                                   input[["cat_Q37"]],
                                                   input[["cat_Q38"]],
                                                   input[["cat_Q40"]])
        data_seg$update_cat23(list_recodes_taken, sttgs_fact)
        gargoyle::trigger("data_cat")
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
