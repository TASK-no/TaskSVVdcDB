library(magrittr)
library(TaskAnalyticsTB)
pth_to_data <- "./data-raw"

data_raw_SVV_2021 <- read_and_distinct(
  file.path(pth_to_data, "2021"),
  "SNA80543_210212New_BG_weighted_Tables_JUS_ANT_Ilya.sav"
)
data_raw_SVV_2022 <- read_and_distinct(
  file.path(pth_to_data, "2022"),
  "SNA93333_220131_enriched_weighted.sav"
)
data_raw_SVV_2023 <- read_and_distinct(
  file.path(pth_to_data, "2023"),
  "SNA107320_230214_weighted.sav"
)
data_raw_SVV_2024 <- read_and_distinct(
  file.path(pth_to_data, "2024"),
  "SNA127220_240207_weighted.sav"
)

data_raw_SVV_2021 <- data_raw_SVV_2021 %>%
  recode_utdanning(levels_range = 1:4) %>%
  recode_leder_c(leder_var = "leder_c", method = "replace")

data_raw_SVV_2022 <- data_raw_SVV_2022 %>%
  recode_utdanning(utdanning_name = "Utdanning", levels_range = 1:5) %>%
  recode_leder_c(leder_var = "Q32", method = "direct")

data_raw_SVV_2023 <- data_raw_SVV_2023 %>%
  recode_utdanning(utdanning_name = "Utdanningsnivå", levels_range = 1:5) %>%
  recode_leder_c(leder_var = "Q32", method = "direct")

questions_relevel <- paste0("Q25r", 1:7)
for (i in questions_relevel) {
  data_raw_SVV_2023[[i]] <- factor(data_raw_SVV_2023[[i]])
  levels(data_raw_SVV_2023[[i]]) <- c(levels(data_raw_SVV_2023[[i]]), 1:8)
  levels(data_raw_SVV_2023[[i]])[c(1, 2, 8)] <- "Svært uenig"
  levels(data_raw_SVV_2023[[i]])[c(2, 3, 4)] <- "enig"
  levels(data_raw_SVV_2023[[i]])[c(3, 4)] <- "Svært enig"
}

questions_relevel <- paste0("Q25r", 1:7)
for (i in questions_relevel) {
  data_raw_SVV_2022[[i]] <- factor(data_raw_SVV_2022[[i]])
  levels(data_raw_SVV_2022[[i]]) <- c(levels(data_raw_SVV_2022[[i]]), 1:8)
  levels(data_raw_SVV_2022[[i]])[c(1, 2, 8)] <- "Svært uenig"
  levels(data_raw_SVV_2022[[i]])[c(2, 3, 4)] <- "enig"
  levels(data_raw_SVV_2022[[i]])[c(3, 4)] <- "Svært enig"
}
questions_relevel <- paste0("Q25r", 1:6)
for (i in questions_relevel) {
  data_raw_SVV_2021[[i]] <- factor(data_raw_SVV_2021[[i]])
  levels(data_raw_SVV_2021[[i]]) <- c(levels(data_raw_SVV_2021[[i]]), 1:8)
  levels(data_raw_SVV_2021[[i]])[c(1, 2, 8)] <- "Svært uenig"
  levels(data_raw_SVV_2021[[i]])[c(2, 3, 4)] <- "enig"
  levels(data_raw_SVV_2021[[i]])[c(3, 4)] <- "Svært enig"
}

data_raw_SVV_2023$Q22 <- factor(data_raw_SVV_2023$Q22,
                                labels = names(attr(
                                  data_raw_SVV_2023$Q22,
                                  "labels"
                                )[1:5])
)
levels(data_raw_SVV_2023$Q22)[c(1, 5)] <- "Ikke i det hele tatt"
data_raw_SVV_2022$Q22 <- factor(data_raw_SVV_2022$Q22,
                                labels = names(attr(
                                  data_raw_SVV_2022$Q22,
                                  "labels"
                                )[1:5])
)
levels(data_raw_SVV_2022$Q22)[c(1, 5)] <- "Ikke i det hele tatt"
data_raw_SVV_2021$Q22 <- factor(data_raw_SVV_2021$Q22,
                                labels = names(attr(
                                  data_raw_SVV_2021$Q22,
                                  "labels"
                                )[1:5])
)
levels(data_raw_SVV_2021$Q22)[c(1, 5)] <- "Ikke i det hele tatt"

data_raw_SVV_2023 <- data_raw_SVV_2023 %>%
  TaskAnalyticsTB::recode_qXX_rVals(
    q_names = c("Q38", "Q39", "Q40"),
    from = 5, to = 1,
    na_replacement = 1,
    list(
      ADJUST_LABELS = TRUE,
      AS_FACTOR = TRUE,
      ORDERED = FALSE,
      ADD_LABELS = TRUE
    )
  )


data_raw_SVV_2021 <- data_raw_SVV_2021 %>%
  dplyr::select(
    tidyselect::any_of(var_to_use_after_seg),
    tidyselect::starts_with(var_to_use_before_seg)
  )
data_raw_SVV_2022 <- data_raw_SVV_2022 %>%
  dplyr::select(
    tidyselect::any_of(var_to_use_after_seg),
    tidyselect::starts_with(var_to_use_before_seg)
  )
data_raw_SVV_2023 <- data_raw_SVV_2023 %>%
  dplyr::select(
    tidyselect::any_of(var_to_use_after_seg),
    tidyselect::starts_with(var_to_use_before_seg)
  )
