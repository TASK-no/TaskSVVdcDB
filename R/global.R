global <- quote({
  var_to_use1 <- c(
    "SamGender", "SamAnsi", "SamAge", "h_age", "SamDivision",
    "leder_c", "utdanning_c"
  )
  var_to_use2 <- c("Q16", "Q17", "Q14", "Q19")
  var_to_use3 <- c("Q22", paste0("Q25r", 1:7))
  var_to_use4 <- c("Q36", "Q37", "Q38", "Q39", "Q40")
  var_to_use5 <- c("Q36_c", "Q37_c", "Q38_c", "Q40_c")

  var_to_use_before_seg <- var_to_use2
  var_to_use_after_seg <- c(var_to_use1, var_to_use3, var_to_use4)

  settings_seg <- list(
    Q16 = list(
      grun = list(
        sum_score_val = 4,
        type_val = "larger-equal"
      ),
      vide = list(
        sum_score_val = 2,
        type_val = "larger-equal"
      ),
      avan = list(
        sum_score_val = 2,
        type_val = "equal"
      )
    ),
    Q17 = list(
      grun = list(
        sum_score_val = 3,
        type_val = "larger-equal"
      ),
      vide = list(
        sum_score_val = 3,
        type_val = "equal"
      ),
      avan = list(
        sum_score_val = 3,
        type_val = "equal"
      )
    ),
    Q14 = list(
      grun = list(
        sum_score_val = 2,
        type_val = "larger-equal"
      ),
      vide = list(
        sum_score_val = 3,
        type_val = "equal"
      ),
      avan = list(
        sum_score_val = 2,
        type_val = "equal"
      )
    ),
    Q19 = list(
      grun = list(
        sum_score_val = 2,
        type_val = "larger-equal"
      ),
      vide = list(
        sum_score_val = 5,
        type_val = "equal"
      ),
      avan = list(
        sum_score_val = 3,
        type_val = "equal"
      )
    )
  )

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
  settings_logistic <- list(
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
      var_to_use1,
      var_to_use3,
      var_to_use4,
      var_to_use5,
      "year"
    ),
    var_exp_choices = c(
      experience_all1,
      experience_all2,
      experience_all3
    )
  )
})
