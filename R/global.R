global <- quote({
  library(magrittr)
  library(TaskAnalyticsTB)

  pth_to_data <- "./data"

  data_raw_SVV_2021 <- haven::read_sav(file.path(pth_to_data, "2021",
                                                 "SNA80543_210212New_BG_weighted_Tables_JUS_ANT_Ilya.sav")) # 2021
  data_raw_SVV_2021 <- data_raw_SVV_2021 %>% dplyr::distinct()
  data_raw_SVV_2022 <- haven::read_sav(file.path(pth_to_data,
                                                 "2022",
                                                 "SNA93333_220131_enriched_weighted.sav"))
  data_raw_SVV_2023 <- haven::read_sav(file.path(pth_to_data,
                                                 "2023",
                                                 "SNA107320_230214_weighted.sav"))

  utdanning_factor <- as.factor(data_raw_SVV_2021$utdanning)
  data_raw_SVV_2021$utdanning_c <- ifelse(utdanning_factor %in% levels(utdanning_factor)[c(1:4)], 0, 1)
  data_raw_SVV_2021$utdanning_c <- factor(data_raw_SVV_2021$utdanning_c, labels = c("low", "high"))

  data_raw_SVV_2022 <- data_raw_SVV_2022 %>% dplyr::rename(utdanning = Utdanning)
  utdanning_factor <- as.factor(data_raw_SVV_2022$utdanning)
  data_raw_SVV_2022$utdanning_c <- ifelse(utdanning_factor %in% levels(utdanning_factor)[c(1:5)], 0, 1)
  data_raw_SVV_2022$utdanning_c <- factor(data_raw_SVV_2022$utdanning_c, labels = c("low", "high"))

  data_raw_SVV_2023 <- data_raw_SVV_2023 %>% dplyr::rename(utdanning = Utdanningsnivå)
  utdanning_factor <- as.factor(data_raw_SVV_2023$utdanning)
  data_raw_SVV_2023$utdanning_c <- ifelse(utdanning_factor %in% levels(utdanning_factor)[c(1:5)], 0, 1)
  data_raw_SVV_2023$utdanning_c <- factor(data_raw_SVV_2023$utdanning_c, labels = c("low", "high"))

  factor_leder <- factor(data_raw_SVV_2023$Q32, labels = c("Nei", "Ja"))
  data_raw_SVV_2023$leder_c <- factor_leder
  # table(as.numeric(data_raw_SVV_2023$leder_c))
  # table(data_raw_SVV_2023$leder_c)

  factor_leder <- factor(data_raw_SVV_2022$Q32, labels = c("Nei", "Ja"))
  data_raw_SVV_2022$leder_c <- factor_leder
  # table(as.numeric(data_raw_SVV_2022$leder_c))
  # table(data_raw_SVV_2022$leder_c)

  data_raw_SVV_2021$leder_c <- factor(replace(data_raw_SVV_2021$leder_c,
                                              data_raw_SVV_2021$leder_c == 2, 0) + 1,
                                      levels = c(1,2), labels = c("Nei", "Ja"))
  # table(as.numeric(data_raw_SVV_2021$leder_c))
  # table(data_raw_SVV_2021$leder_c)

  questions_relevel <- paste0("Q25r", 1:7)
  for (i in questions_relevel) {
    data_raw_SVV_2023[[i]] <- factor(data_raw_SVV_2023[[i]])
    levels(data_raw_SVV_2023[[i]]) <- c(levels(data_raw_SVV_2023[[i]]), 1:8)
    levels(data_raw_SVV_2023[[i]])[c(1, 2, 8)] <- "Svært uenig"
    levels(data_raw_SVV_2023[[i]])[c(2, 3, 4)] <- "enig"
    levels(data_raw_SVV_2023[[i]])[c(3, 4)]    <- "Svært enig"
  }

  questions_relevel <- paste0("Q25r", 1:7)
  for (i in questions_relevel) {
    data_raw_SVV_2022[[i]] <- factor(data_raw_SVV_2022[[i]])
    levels(data_raw_SVV_2022[[i]]) <- c(levels(data_raw_SVV_2022[[i]]), 1:8)
    levels(data_raw_SVV_2022[[i]])[c(1, 2, 8)] <- "Svært uenig"
    levels(data_raw_SVV_2022[[i]])[c(2, 3, 4)] <- "enig"
    levels(data_raw_SVV_2022[[i]])[c(3, 4)]    <- "Svært enig"
  }
  questions_relevel <- paste0("Q25r", 1:6)
  for (i in questions_relevel) {
    data_raw_SVV_2021[[i]] <- factor(data_raw_SVV_2021[[i]])
    levels(data_raw_SVV_2021[[i]]) <- c(levels(data_raw_SVV_2021[[i]]), 1:8)
    levels(data_raw_SVV_2021[[i]])[c(1, 2, 8)] <- "Svært uenig"
    levels(data_raw_SVV_2021[[i]])[c(2, 3, 4)] <- "enig"
    levels(data_raw_SVV_2021[[i]])[c(3, 4)]    <- "Svært enig"
  }

  data_raw_SVV_2023$Q22 <- factor(data_raw_SVV_2023$Q22,
                                  labels = names(attr(data_raw_SVV_2023$Q22,
                                                      "labels")[1:5]))
  levels(data_raw_SVV_2023$Q22)[c(1, 5)] <- "Ikke i det hele tatt"
  data_raw_SVV_2022$Q22 <- factor(data_raw_SVV_2022$Q22,
                                  labels = names(attr(data_raw_SVV_2022$Q22,
                                                      "labels")[1:5]))
  levels(data_raw_SVV_2022$Q22)[c(1, 5)] <- "Ikke i det hele tatt"
  data_raw_SVV_2021$Q22 <- factor(data_raw_SVV_2021$Q22,
                                  labels = names(attr(data_raw_SVV_2021$Q22,
                                                      "labels")[1:5]))
  levels(data_raw_SVV_2021$Q22)[c(1, 5)] <- "Ikke i det hele tatt"

  var_to_use1 <- c("SamGender", "SamAnsi", "SamAge", "h_age", "SamDivision",
                   "leder_c", "utdanning_c")
  var_to_use2 <- c("Q16", "Q17", "Q14", "Q19")
  var_to_use3 <- c("kat")
  var_to_use4 <- c("Q22", "Q25")
  data_raw_SVV_2021 <- data_raw_SVV_2021 %>%
    dplyr::select(tidyselect::any_of(var_to_use1),
                  tidyselect::starts_with(var_to_use2),
                  tidyselect::starts_with(var_to_use4))
  data_raw_SVV_2022 <- data_raw_SVV_2022 %>%
    dplyr::select(tidyselect::any_of(var_to_use1),
                  tidyselect::starts_with(var_to_use2),
                  tidyselect::starts_with(var_to_use4))
  data_raw_SVV_2023 <- data_raw_SVV_2023 %>%
    dplyr::select(tidyselect::any_of(var_to_use1),
                  tidyselect::starts_with(var_to_use2),
                  tidyselect::starts_with(var_to_use4))
  # data_raw_SVV <- TaskAnalyticsTB::get_data_joined(data_raw_SVV_2021, data_raw_SVV_2022)
  # data_seg_2021 <- load("./data/data_raw_SVV_2021.RData")
  # data_seg_2022 <- load("./data/data_raw_SVV_2022.RData")
  settings_seg <- list(q16 = list(sum_score_val_grun = 4,
                                  sum_score_val_vide = 2,
                                  sum_score_val_avan = 2,
                                  type_val_grun = "larger-equal",
                                  type_val_vide = "larger-equal",
                                  type_val_avan = "equal"),
                       q17 = list(sum_score_val_grun = 3,
                                  sum_score_val_vide = 3,
                                  sum_score_val_avan = 3,
                                  type_val_grun = "larger-equal",
                                  type_val_vide = "equal",
                                  type_val_avan = "equal"),
                       q14 = list(sum_score_val_grun = 2,
                                  sum_score_val_vide = 3,
                                  sum_score_val_avan = 2,
                                  type_val_grun = "larger-equal",
                                  type_val_vide = "equal",
                                  type_val_avan = "equal"),
                       q19 = list(sum_score_val_grun = 2,
                                  sum_score_val_vide = 5,
                                  sum_score_val_avan = 3,
                                  type_val_grun = "larger-equal",
                                  type_val_vide = "equal",
                                  type_val_avan = "equal"))

  experience_all1 <- "Uerfaren og Grunnleggende"
  experience_all2 <- paste0("Grunnleggende og ", paste0("Videreg",
                                                        "\u00e5",
                                                        "ende"))
  experience_all3 <- paste0(paste0("Videreg", "\u00e5", "ende og "),
                            "Avansert")
  settings_logistic <- list(var_dep_choices = c("kat_kommunikasjon",
                                                "kat_informasjon1",
                                                "kat_programmer1",
                                                "kat_utstyr1"),
                            lab_dep_choices =   c("Kommunikasjon og samhandling",
                                                  "Informasjonssikkerhet og personvern",
                                                  "Bruk av programvare",
                                                  "Bruk av teknologi"),
                            var_reg_choices = c("SamAge",
                                                "h_age",
                                                "SamAnsi",
                                                "SamGender",
                                                "SamDivision",
                                                "Q22",
                                                "utdanning_c",
                                                "leder_c",
                                                "Q25r1", "Q25r2", "Q25r3",
                                                "Q25r4", "Q25r5", "Q25r6"),
                            var_exp_choices = c(experience_all1,
                                                experience_all2,
                                                experience_all3)
  )
}
)
