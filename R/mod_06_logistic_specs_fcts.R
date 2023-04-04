get_data_logistics_all <- function(data_set_list, years = NULL) {
  if (is.null(years)) {
    return(NULL)
  }
  names_ds <- paste0("data_", years)
  if (length(names_ds) > 1) {
    data_chosen <- data_set_list[names_ds]
    names_chosen <- lapply(data_chosen, names)
    names_vars_both <- Reduce(function(x, y) {
      intersect(x, y)
    }, names_chosen)
    data_out <- lapply(data_chosen, function(x, var_names) {
      x[var_names]
    }, var_names = names_vars_both)
    for (i in 1:length(names_ds)) {
      data_out[[i]]$year <- years[i]
    }
    data_out <- Reduce(rbind, data_out)
    data_out$year <- factor(data_out$year)
    data_out <- data_out %>%
      dplyr::distinct(
        dplyr::pick(dplyr::all_of(setdiff(
          names(data_out),
          "year"
        ))),
        .keep_all = TRUE
      )
    return(data_out)
  } else {
    data_chosen <- data_set_list[[names_ds]]
    return(data_chosen)
  }
}
deparse_input_logistic_to_model <- function(dep, reg, exp) {
  dep_taken <- get_sttgs_log("var_dep_choices")[which(get_sttgs_log("lab_dep_choices") %in% dep)]
  mod_deparsed <- list(
    dependent = dep_taken,
    regressors = reg
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

  if (exp == experience_all1) exp_taken <- c("Uerfaren", "Grunnleggende")
  if (exp == experience_all2) {
    exp_taken <- c("Grunnleggende", paste0(
      "Videreg",
      "\u00e5", "ende"
    ))
  }
  if (exp == experience_all3) {
    exp_taken <- c(
      paste0(
        "Videreg",
        "\u00e5", "ende"
      ),
      "Avansert"
    )
  }

  mod_deparsed$experience <- exp_taken
  return(mod_deparsed)
}
get_mcfadden_text <- function() {
  msg1 <- paste0(
    "McFadden R2 gjenspeiler graden av forbedring av den",
    " fullstendige modellen med prediktorer i forhold til",
    " nullmodellen (kun med intercept). En modell med en ",
    "større McFaddens har en bedre tilpasning over en annen",
    " modell med en mindre verdi."
  )

  msg2 <- paste0(
    "Tommelfingerregel: Allerede verdier på",
    " 0.2 < McFadden-R2 < 0.4 representerer en",
    " spesielt god tilpasning av modellen."
  )

  msg3 <- paste0(
    "Den korrigerte McFaddens R2 evaluerer antall prediktorer/regressorer",
    " for hvor godt en modell passer. For mange prediktorer, ",
    "som ikke bidrar nok til modellen, reduserer effektiviteten ",
    "til en modell og gjenspeiles negativt i den korrigerte ",
    "McFaddens R2 (slik at verdier mindre enn 0 også er mulig)."
  )

  paste(msg1, msg2, msg3, sep = "<br/> <br/>")
}
