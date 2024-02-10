library(magrittr)
library(TaskAnalyticsTB)
pth_to_data <- "./data-raw"

# Read and clean data for each year, removing duplicate rows
# 2021 dataset
data_raw_SVV_2021 <- read_and_distinct(
  file.path(pth_to_data, "2021"),
  "SNA80543_210212New_BG_weighted_Tables_JUS_ANT_Ilya.sav"
)
# 2022 dataset
data_raw_SVV_2022 <- read_and_distinct(
  file.path(pth_to_data, "2022"),
  "SNA93333_220131_enriched_weighted.sav"
)
# 2023 dataset
data_raw_SVV_2023 <- read_and_distinct(
  file.path(pth_to_data, "2023"),
  "SNA107320_230214_weighted.sav"
)
# 2024 dataset
data_raw_SVV_2024 <- read_and_distinct(
  file.path(pth_to_data, "2024"),
  # "SNA127220_240207_weighted.sav"
  "SNA127220_240209_weighted_cleaned.sav"
)

# Apply a series of cleaning and recoding functions to each dataset
# Recoding utdanning, leder_c, specific questions, and Q22 for 2021 dataset
data_raw_SVV_2021 <- data_raw_SVV_2021 %>%
  recode_utdanning(levels_range = 1:4) %>%
  recode_leder_c(leder_var = "leder_c", method = "replace") %>%
  recode_questions(questions_prefix = "Q25r", seq_questions = 1:6) %>%
  recode_Q22()

# Similar recoding steps for 2022 dataset
data_raw_SVV_2022 <- data_raw_SVV_2022 %>%
  recode_utdanning(utdanning_name = "Utdanning", levels_range = 1:5) %>%
  recode_leder_c(leder_var = "Q32", method = "direct") %>%
  recode_questions(questions_prefix = "Q25r", seq_questions = 1:7) %>%
  recode_Q22()

# Similar recoding steps for 2023 dataset
data_raw_SVV_2023 <- data_raw_SVV_2023 %>%
  recode_utdanning(utdanning_name = "Utdanningsnivå", levels_range = 1:5) %>%
  recode_leder_c(leder_var = "Q32", method = "direct") %>%
  recode_questions(questions_prefix = "Q25r", seq_questions = 1:7) %>%
  recode_Q22()

# Similar recoding steps for 2024 dataset, with a slight variation in questions
data_raw_SVV_2024 <- data_raw_SVV_2024 %>%
  recode_utdanning(utdanning_name = "Utdanning", levels_range = 1:5) %>%
  recode_leder_c(leder_var = "Q32", method = "direct") %>%
  recode_questions(questions_prefix = "Q25r", seq_questions = c(1:5, 7)) %>%
  recode_Q22()

# Additional specific recoding for the 2023 dataset using a custom function
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

# Select relevant variables for saving and verify integrity against saved
# versions Verification for 2021 dataset
data_raw_SVV_2021_to_save <- data_raw_SVV_2021 %>%
  dplyr::select(
    tidyselect::any_of(get_var_to_use_after_seg()),
    tidyselect::starts_with(get_var_to_use(2))
  )
load("./data/data_raw_SVV_2021.rda")
stopifnot(identical(data_raw_SVV_2021, data_raw_SVV_2021_to_save))

# Verification for 2022 dataset
data_raw_SVV_2022_to_save <- data_raw_SVV_2022 %>%
  dplyr::select(
    tidyselect::any_of(get_var_to_use_after_seg()),
    tidyselect::starts_with(get_var_to_use(2))
  )
load("./data/data_raw_SVV_2022.rda")
stopifnot(identical(data_raw_SVV_2022, data_raw_SVV_2022_to_save))

# Verification for 2023 dataset
data_raw_SVV_2023_to_save <- data_raw_SVV_2023 %>%
  dplyr::select(
    tidyselect::any_of(get_var_to_use_after_seg()),
    tidyselect::starts_with(get_var_to_use(2))
  )
load("./data/data_raw_SVV_2023.rda")
stopifnot(identical(data_raw_SVV_2023, data_raw_SVV_2023_to_save))

# Verification for 2024 dataset
data_raw_SVV_2024_to_save <- data_raw_SVV_2024 %>%
  dplyr::select(
    tidyselect::any_of(get_var_to_use_after_seg()),
    tidyselect::starts_with(get_var_to_use(2))
  )
load("./data/data_raw_SVV_2024.rda")
stopifnot(identical(data_raw_SVV_2024, data_raw_SVV_2024_to_save))
