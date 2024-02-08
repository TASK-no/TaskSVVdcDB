# Function to recode `leder_c`
#' Read and Distinct Data
#'
#' Reads data from a .sav file and applies distinct operation to remove
#' duplicates.
#'
#' @param path The path to the directory containing the data file.
#' @param file The filename of the data file to read.
#' @return A tibble with distinct rows from the read .sav file.
read_and_distinct <- function(path, file) {
  haven::read_sav(file.path(path, file)) %>% dplyr::distinct()
}

#' Recode Utdanning Variable
#'
#' Conditionally renames and recodes the `utdanning` variable based on specified
#' levels. Utilizes the `get_utdanning_c` function to directly generate
#' `utdanning_c`. If `utdanning_name` is not provided (NULL), no renaming is
#' performed.
#'
#' @param data The dataset containing the `utdanning` variable.
#' @param utdanning_name The name of the `utdanning` variable in the dataset to
#'        be renamed, or NULL if no renaming is needed. Defaults to NULL.
#' @param levels_range A vector indicating the specific levels to be recoded.
#'        This parameter allows for precise control over which levels are
#'        considered "low" or "high".
#' @return The dataset with the `utdanning` variable optionally renamed and
#'         recoded. This function ensures that the structure and integrity of
#'         the dataset are maintained while applying the necessary
#'         transformations.
recode_utdanning <- function(data, utdanning_name = NULL, levels_range) {
  if (!is.null(utdanning_name)) {
    data <- data %>% dplyr::rename(utdanning = {{utdanning_name}})
  }
  # Directly apply the helper function within mutate to create utdanning_c
  data <- data %>% dplyr::mutate(
    utdanning_c = get_utdanning_c(.data$utdanning, levels_range)
  )
  return(data)
}


#' Generate Utdanning_c Variable
#'
#' Creates the `utdanning_c` variable based on specified level ranges. This
#' function is designed to facilitate the recoding of educational levels into a
#' binary "low" and "high" categorization, enhancing the analytical utility of
#' the `utdanning` variable.
#'
#' @param utd The `utdanning` variable from the dataset, which may require
#'        conversion to a factor if not already one. This variable represents
#'        the educational levels of respondents.
#' @param lvl_range A vector specifying the levels to be considered "low"
#'        (coded as 0). The complement set within the specified levels of `utd`
#'        will be considered "high" (coded as 1). This parameter allows for
#'        flexible adaptation to different datasets and educational level
#'        classifications.
#' @return A factor variable `utdanning_c` with levels "low" and "high",
#'         suitable for inclusion in the dataset for subsequent analysis. This
#'         recoded variable facilitates the comparison and analysis of data
#'         based on the binary categorization of educational attainment.
get_utdanning_c <- function(utd, lvl_range) {
  utd_fac <- as.factor(utd)
  utd_c <- ifelse(utd_fac %in% levels(utd_fac)[lvl_range], 0, 1)
  utd_c <- factor(utd_c, labels = c("low", "high"))
  return(utd_c)
}

#' Recode Leder Variable
#'
#' Recodes the `leder_c` variable based on the specified leadership variable and
#' dataset structure. Adapts to different recoding needs for different years.
#'
#' @param data The dataset containing the leadership variable.
#' @param leder_var The leadership variable to be recoded. This should be the
#'        name of the column in `data` that contains leadership information,
#'        specified as a symbol or string.
#' @param method A character string specifying the recoding method. Valid
#'        options are "direct" for direct factor assignment (used for 2022 and
#'        2023 datasets) and "replace" for datasets requiring a replacement
#'        logic before factor assignment (used for the 2021 dataset).
#' @return The dataset with the `leder_c` variable recoded according to the
#'         specified method.
recode_leder_c <- function(data, leder_var, method = "direct") {
  leder_var_sym <- rlang::ensym(leder_var)
  if (method == "direct") {
    data <- data %>% dplyr::mutate(
      leder_c = factor(!!leder_var_sym,
                       # levels = c(0, 1), -> must be excluded currently
                       labels = c("Nei", "Ja"))
    )
  } else if (method == "replace") {
    data <- data %>% dplyr::mutate(
      leder_c = factor(
        replace(!!leder_var_sym, !!leder_var_sym == 2, 0) + 1,
        levels = c(1, 2), labels = c("Nei", "Ja")
      )
    )
  }
  return(data)
}

#' Relevel Questions
#'
#' Dynamically relevels specified questions within the dataset based on a given
#' prefix, number of questions, and custom releveling logic.
#'
#' @param dataThe dataset containing the questions to be releveled.
#' @param questions_prefix The prefix of the question variables to be releveled.
#' @param seq_questions The sequence of questions numbers to relevel e.g. `1:7`
#'        to generate `Q25r1-Q25r7` or `c(1:5, 7)` for the `data_raw_SVV_2024`.
#'
#' @return The dataset with questions releveled according to the specified
#'         logic.
recode_questions <- function(data,
                             questions_prefix,
                          seq_questions) {
  questions_relevel <- paste0(questions_prefix, seq_questions)
  for (i in questions_relevel) {
    data[[i]] <- factor(data[[i]])
    # Ensure all specified new levels are present
    levels(data[[i]]) <- c(levels(data[[i]]), 1:8)
    levels(data[[i]])[c(1, 2, 8)] <- "Svært uenig"
    levels(data[[i]])[c(2, 3, 4)] <- "enig"
    levels(data[[i]])[c(3, 4)] <- "Svært enig"
  }
  return(data)
}
