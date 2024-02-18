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
#' @param data The dataset containing the questions to be releveled.
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

#' Recode Q22 Variable
#'
#' Recodes the `Q22` variable in the dataset, setting specific levels to a
#' uniform label. This function is designed to standardize responses across
#' different datasets.
#'
#' @param data The dataset containing the `Q22` variable.
#' @return The dataset with the `Q22` variable recoded.
recode_Q22 <- function(data) {
  data$Q22 <- factor(data$Q22,
                     labels = names(attr(data$Q22, "labels"))[1:5])
  levels(data$Q22)[c(1, 5)] <- "Ikke i det hele tatt"
  return(data)
}

#' Generate New Variable Names Based on Training Type
#'
#' This function generates new variable names by identifying the type of training
#' information contained within the variable name. It recognizes variables related
#' to the sum of completed training sessions and the average progress in training
#' sessions, renaming them according to a standardized naming convention.
#'
#' @param name_var The original name of the variable.
#'
#' @return A character string representing the new variable name. If the
#'   original name contains "SumofCompleted", it is replaced with
#'   "T_[number]_c", indicating the training type and that it represents a
#'   count. If it contains "AverageofProgress", it is replaced with
#'   "T_[number]_perc_prog", indicating the training type and that it represents
#'   a percentage progress. Otherwise, the original name is returned.
generate_new_names <- function(name_var) {
  if (grepl("SumofCompleted", name_var)) {
    sub("SumofCompletedTrinn(\\d)", "T_\\1_c", name_var)
  } else if (grepl("AverageofProgress", name_var)) {
    sub("AverageofProgressTrinn(\\d)", "T_\\1_perc_prog", name_var)
  } else {
    name_var
  }
}

#' Enhance Data with "Recoded" Training and Status Variables
#'
#' Applies a series of transformations to a tibble, focusing on recoding
#' specific training-related variables and adjusting other categorical variables
#' according to predefined conventions. It renames variables, recodes them as
#' ordered or unordered factors, and handles missing values appropriately.
#'
#' @param df A tibble containing the variables to be transformed.
#'
#' @return A tibble with transformed variables: training variables are recoded
#'   to ordered factors with levels indicating participation status or progress
#'   percentage. The `Fjernleder` variable is recoded as an unordered factor
#'   with levels for remote leadership status, and `Statusfullførtkurs` is
#'   renamed and recoded to reflect course completion status with appropriate
#'   handling of missing values.
#'
#' @export
add_training_variables <- function(df) {
  # Check if the argument is a tibble
  if (!inherits(df, "tbl_df")) {
    stop("The input must be a tibble.", call. = FALSE)
  }
  # Identify relevant columns and their types
  columns <- names(df)
  completed_columns <- grep("SumofCompletedTrinn", columns, value = TRUE)
  progress_columns <- grep("AverageofProgressTrinn", columns, value = TRUE)

   # Check if the relevant columns are found
  if (length(completed_columns) == 0 && length(progress_columns) == 0) {
    stop("No relevant column names found in the data.", call. = FALSE)
  }

  # Rename and recode 'completed' variables
  for (col in completed_columns) {
    new_name <- generate_new_names(col)
    df[[new_name]] <- recode_variable(df[[col]], "completed")
  }

  # Rename and recode 'progress' variables
  for (col in progress_columns) {
    new_name <- generate_new_names(col)
    # Convert numeric strings to actual numeric values for proper sorting and factor creation
    df[[col]] <- as.character(as.numeric(df[[col]]))
    df[[new_name]] <- recode_variable(df[[col]], "progress")
  }

  # Remove original columns if no longer needed
  df <- df[, !(names(df) %in% c(completed_columns, progress_columns))]

  # Fix treatment of the variable Fjernleder.
  # Step 1: Replace empty strings with proper value
  df$Fjernleder[df$Fjernleder == ""] <- "Ikke fjernleder"
  df$Fjernleder[df$Fjernleder ==  "Ikke fjernleder"] <- "ikke_fjernleder"
  df$Fjernleder[df$Fjernleder ==  "Fjernleder"] <- "fjernleder"
  # Step 2: Convert to factor with specified levels and labels
  df$Fjernleder <- factor(df$Fjernleder,
                          levels = c("fjernleder", "ikke_fjernleder"),
                          labels = c("fjernleder", "ikke_fjernleder"))

  # Fix treatment of the variable 'Statusfullførtkurs'
  # Step 1: Rename the variable
  names(df)[names(df) == "Statusfullførtkurs"] <- "fullfortkurs"
  # Step 2: Replace empty strings with "none" to represent missing values
  df$fullfortkurs[df$fullfortkurs == ""] <- "ikke_deltatt"
  # df$fullfortkurs[df$fullfortkurs == ""] <- "paebegynt"
  df$fullfortkurs[df$fullfortkurs == "Påbegynt"] <- "paebegynt"
  df$fullfortkurs[df$fullfortkurs == "Fullført"] <- "fullfort"
  # Step 3: Convert to an ordered factor with specified levels and labels
  df$fullfortkurs <- factor(df$fullfortkurs,
                            # levels = c("paebegynt", "fullfort"),
                            # labels = c("paebegynt", "fullfort"),
                            levels = c("ikke_deltatt", "paebegynt", "fullfort"),
                            labels = c("ikke_deltatt", "paebegynt", "fullfort"),
                            ordered = TRUE)
  return(df)
}

#' Recode Variable to Ordered Factor Based on Type
#'
#' Recodes a given variable to an ordered factor with levels and labels
#' appropriate to the type of training data it represents. It supports recoding
#' for both completed training counts and training progress percentages,
#' including handling special cases such as non-participation.
#'
#' @param name_var The variable to be recoded, as a character vector.
#' @param type A character string indicating the type of recoding to apply:
#'   either "completed" for sum of completed trainings or "progress" for average
#'   progress in training sessions.
#'
#' @return An ordered factor with levels and labels corresponding to the
#'   specified type. For "completed" type, levels are "", "0", "1", with labels
#'   "ikke_deltatt", "paebegynt", "fullfoert". For "progress" type, levels
#'   include "none" and numeric progress levels, with labels "ikke_deltatt" and
#'   various "_perc" labels corresponding to the numeric progress levels.
recode_variable <- function(name_var, type) {
  if (type == "completed") {
    factor(name_var,
           levels = c("", "0", "1"),
           labels = c("ikke_deltatt", "paebegynt", "fullfoert"),
           ordered = TRUE)
  } else if (type == "progress") {
    ## Replace "" with "none"
    name_var <- get_clean_var(name_var, "none")
    # Prepare levels and labels, including "none" explicitly
    levels_progress <- get_lvl_progress(name_var, "none")
    labels_progress <- get_lab_progress(name_var, "none")
    # Create an ordered factor with these levels and labels
    factor(name_var,
           levels = levels_progress,
           labels = labels_progress,
           ordered = TRUE)
  } else {
    name_var
  }
}
get_clean_var <- function(raw_var_taken, set_special_val = "none") {
  raw_var_taken[is.na(raw_var_taken)] <- set_special_val
  return(raw_var_taken)
}
get_lvl_progress <- function(raw_var_taken, special_val) {
  tmp_val <- unique(raw_var_taken[!is.na(raw_var_taken)])
  tmp_val <- as.character(sort(as.numeric(setdiff(tmp_val, special_val))))
  c(special_val, tmp_val)
}
get_lab_progress <- function(raw_var_taken, special_val)  {
  tmp_lvl_names <- get_lvl_progress(raw_var_taken, special_val)
  tmp_lvl_names <- setdiff(tmp_lvl_names, "none")
  c("ikke_deltatt", paste0(tmp_lvl_names, "_perc"))
}
#' Convert Specified Variables in a Dataset to Factors
#'
#' This function iteratively converts specified variables within a dataset to
#' factors. For each variable specified, it can also designate the factor as
#' ordered based on a corresponding logical vector. The function operates on a
#' dataset (data frame or similar structure) and takes a named list where names
#' correspond to variables in the dataset and values are logical, indicating
#' whether the factor should be ordered.
#'
#' @param data_set A data frame or similar object containing the variables to be
#'   converted to factors.
#' @param list_vars A named list where each name corresponds to a variable in
#'   `data_set` that should be converted to a factor. The value for each name
#'   should be a logical value (`TRUE` or `FALSE`), indicating whether the
#'   resulting factor should be ordered (`TRUE` for ordered, `FALSE` for
#'   unordered).
#'
#' @return The modified `data_set` with specified variables converted to factors.
#'   If a variable is designated to be an ordered factor, it is converted
#'   accordingly.
#'
#' @details The function iterates over the `list_vars` names, checking each
#'   corresponding variable in `data_set`. If the variable exists, it is
#'   converted to a factor with the `ordered` parameter set as specified in
#'   `list_vars`. Variables not found in `data_set` are skipped without error.
#'   This allows for flexible data cleaning and preprocessing, especially when
#'   preparing data for statistical analysis or machine learning models where
#'   factor variables may be required.
#'
#' @export
fix_type_to_factor <- function(data_set, list_vars) {
  stopifnot(`Arg. 'list_vars' must be a names vector` =
              !is.null(names(list_vars)))
  stopifnot("Values in 'list_vars' must be logical" =
              all(is.logical(unname(list_vars))))
  stopifnot("Arg. 'data_set' must not be empty" = nrow(data_set) > 0)
  vars_to_fix <- names(list_vars)
  ordered_log <- as.logical(unname(list_vars))
  num_vars_to_fix <- length(vars_to_fix)
  for (i in seq_len(num_vars_to_fix)) {
    if (is.null(data_set[[vars_to_fix[i]]])) next
    data_set[[vars_to_fix[i]]] <- factor(data_set[[vars_to_fix[i]]],
                                         ordered = ordered_log[i])
  }
  return(data_set)
}
# update_data_set <- function(data_set,
#                             data_set_name,
#                             pth = "./data/data_raw_SVV_XXX.rda") {
#   assign(data_set_name, data_set)
#   save(eval(parse(text = data_set_name)), file = pth)
# }
# fix_leder_c <- function(data_set, var_name_leder) {
#   new_leder <- (as.integer(data_set$leder_c) - 2) * (-1) + 1
#   new_leder <- c("Nei", "Ja")[new_leder]
#   new_leder <- factor(new_leder, levels = c("Nei", "Ja"))
#   data_set[, "leder_c"] <- new_leder
# }