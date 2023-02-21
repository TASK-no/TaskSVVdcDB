get_data_logistics_all <- function(data_set_list, years) {
  names_ds <- paste0("data_", years)
  if (length(names_ds) > 1) {
    data_chosen  <- data_set_list[names_ds]
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
      dplyr::distinct(dplyr::pick(dplyr::all_of(setdiff(names(data_out),
                                                        "year"))),
                      .keep_all = TRUE)
    return(data_out)
  } else {
    data_chosen <- data_set_list[[names_ds]]
    return(data_chosen)
  }
}
deparse_input_logistic_to_model <- function(dep, reg, exp) {
  dep_taken <- settings_logistic$var_dep_choices[which(settings_logistic$lab_dep_choices %in% dep)]
  mod_deparsed <- list(dependent = dep_taken,
                       regressors = reg)

  experience_all1 <- "Uerfaren og Grunnleggende"
  experience_all2 <- paste0("Grunnleggende og ", paste0("Videreg",
                                                        "\u00e5",
                                                        "ende"))
  experience_all3 <- paste0(paste0("Videreg", "\u00e5", "ende og "),
                            "Avansert")

  if (exp == experience_all1) exp_taken <-  c("Uerfaren", "Grunnleggende");
  if (exp == experience_all2) exp_taken <-  c("Grunnleggende", paste0("Videreg",
                                                                      "\u00e5","ende"));
  if (exp == experience_all3) exp_taken <-  c(paste0("Videreg",
                                                     "\u00e5","ende"),
                                              "Avansert");

  mod_deparsed$experience <- exp_taken
  return(mod_deparsed)
}
