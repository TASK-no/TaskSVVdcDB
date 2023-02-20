#' 01_seg_all UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_01_seg_all_ui <- function(id){
  ns <- shiny::NS(id)
  seq_num_q <- list(16, 17, 14, 19)
  seq_title <- list(Q16 = "Kommunikasjon og samhandling",
                    Q17 = "Informasjonssikkerhet og personvern",
                    Q14 = "Bruk av programvare",
                    Q19 = "Bruk av teknologi")
  shiny::tagList(
    add_header("Segmenteringsanalyse:", size = 2,
               UNDERLINE = TRUE, EMPHASIZE = TRUE),
    mod_seg_competence_ui(ns("seg_inputs")),
    break_vspace("small"),
    lapply(seq_num_q, generate_seq_ui, id_sub = ns("seg_inputs"),
           title_seq = seq_title, sttgs_seq = settings_seg),
    auth0::logoutButton(label = "Logg ut", id = "my_logout")
  )
}
generate_seq_ui <- function(Qxx, id_sub, title_seq, sttgs_seq) {
  Qtaken <- paste0("Q", Qxx)
  mod_seg_q_ui(id_sub,
               num_q = Qxx,
               title_text = title_seq[[Qtaken]],
               sttgs = sttgs_seq[[Qtaken]])
}
#' 01_seg_all Server Functions
#'
#' @noRd
mod_01_seg_all_server <- function(id, r){
  shiny::moduleServer(id, function(input, output, session) {
    settingsDC  <- mod_seg_competence_srv("seg_inputs")
    settingsQ16 <- mod_seg_q_srv("seg_inputs", num_q = 16)
    settingsQ17 <- mod_seg_q_srv("seg_inputs", num_q = 17)
    settingsQ14 <- mod_seg_q_srv("seg_inputs", num_q = 14)
    settingsQ19 <- mod_seg_q_srv("seg_inputs", num_q = 19)
    browser()
    shiny::reactive({r$ui_inputs$input_seg <- list(sttgs_DC = settingsDC,
                                                   sttgs_Q16 = settingsQ16,
                                                   sttgs_Q17 = settingsQ17,
                                                   sttgs_Q14 = settingsQ14,
                                                   sttgs_Q19 = settingsQ19)})
  })
}
