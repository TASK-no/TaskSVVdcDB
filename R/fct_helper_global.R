#' helper_global
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_var_to_use <- function(num) {
  tmp_var_exp <- paste0("var_to_use", num)
  var_to_use1 <- c(
    "SamGender", "SamAnsi", "SamAge", "h_age", "SamDivision",
    "leder_c", "utdanning_c"
  )
  var_to_use2 <- c("Q16", "Q17", "Q14", "Q19")
  var_to_use3 <- c("Q22", paste0("Q25r", 1:7))
  # var_to_use4 <- c("Q36", "Q37", "Q38", "Q39", "Q40")
  var_to_use4 <- c("Q36", "Q37", "Q38", "Q40")
  var_to_use5 <- c("Q36_c", "Q37_c", "Q38_c", "Q40_c")
  var_to_use6 <- c("fullfortkurs",
                   "T_1_c",
                   "T_2_c",
                   "T_3_c",
                   "T_1_perc_prog",
                   "T_2_perc_prog",
                   "T_3_perc_prog",
                   "Fjernleder")
  eval(parse(text = tmp_var_exp))
}
get_var_to_use_after_seg <- function() {
  c(get_var_to_use(1),
    get_var_to_use(3),
    get_var_to_use(4),
    get_var_to_use(6))
}
get_exp_all <- function(num) {
  tmp_var_exp <- paste0("experience_all", 1)
  experience_all1 <- "Uerfaren og Grunnleggende"
  experience_all2 <- paste0("Grunnleggende og ", paste0(
    "Videreg",
    "\u00e5",
    "ende"
  ))
  experience_all3 <- paste0(
    paste0("Videreg", "\u00e5", "ende og "),
    "Avansert"
  )
  eval(parse(text = tmp_var_exp))
}
get_sttgs_log <- function(sub_elem = NULL) {
  li_out <- list(
    var_dep_choices = c(
      "kat_kommunikasjon",
      "kat_informasjon1",
      "kat_programmer1",
      "kat_utstyr1"
    ),
    lab_dep_choices = c(
      "Kommunikasjon og samhandling",
      "Informasjonssikkerhet og personvern",
      "Bruk av programvare",
      "Bruk av teknologi"
    ),
    var_reg_choices = c(
      get_var_to_use(1),
      get_var_to_use(3),
      get_var_to_use(4),
      get_var_to_use(5),
      get_var_to_use(6),
      "year"
    ),
    var_exp_choices = c(
      get_exp_all(1),
      get_exp_all(2),
      get_exp_all(3)
    )
  )
  if (is.null(sub_elem)) {
    return(li_out)
  }
  stopifnot(`Unknown value for arg 'sub_elem'` = sub_elem %in% names(li_out))
  li_out[[sub_elem]]
}
