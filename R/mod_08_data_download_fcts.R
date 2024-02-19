get_data_names_norsk <- function(name = NULL, num = NULL) {
  vec_data_names <- c(
    raw_data_2021 = "Rådata 2021",
    raw_data_2022 = "Rådata 2022",
    raw_data_2023 = "Rådata 2023",
    raw_data_2024 = "Rådata 2024",
    seg_data_2021 = "Segmenteringsdata 2021",
    seg_data_2022 = "Segmenteringsdata 2022",
    seg_data_2023 = "Segmenteringsdata 2023",
    seg_data_2024 = "Segmenteringsdata 2024",
    overall_data = "Overordnede/hovedplottdata",
    subplot_data_01 = "Delplottdata (første)",
    subplot_data_02 = "Delplottdata (andre)",
    log_data_all = "Logistiske regresjonsdata (alle)",
    log_data_trn = "Logistiske regresjonsdata (opplæring)",
    log_data_prd = "Logistiske regresjonsdata (prediksjon)"
  )
  if (is.null(name) && !is.null(num)) {
    unname(vec_data_names[num])
  } else if (!is.null(name) && is.null(num)) {
    vec_data_names[[name]]
  } else if (!is.null(name) && !is.null(num)) {
    warning("Both arguments specified, but name argument takes precedence ...")
    vec_data_names[[name]]
  } else {
    stop("Specify exactly one argument: either 'name' or 'num'.")
  }
}
get_data <- function(input, r, data_seg, data_log) {
  if (input[["data_type"]] %in% get_data_names_norsk(num = 1:4)) {
    year_taken <- get_year_raw_data(input[["data_type"]])
    tmp_raw_data <- get_raw_data(year_taken)
    return(tmp_raw_data)
  } else if (input[["data_type"]] == get_data_names_norsk("seg_data_2021")) {
    data_seg$get_data_segmentation()[["data_2021"]]
  } else if (input[["data_type"]] == get_data_names_norsk("seg_data_2022")) {
    data_seg$get_data_segmentation()[["data_2022"]]
  } else if (input[["data_type"]] == get_data_names_norsk("seg_data_2023")) {
    data_seg$get_data_segmentation()[["data_2023"]]
  } else if (input[["data_type"]] == get_data_names_norsk("seg_data_2024")) {
    data_seg$get_data_segmentation()[["data_2024"]]
  } else if (input[["data_type"]] == get_data_names_norsk("overall_data")) {
    r$datasets$data_plot01
  } else if (input[["data_type"]] == get_data_names_norsk("subplot_data_01")) {
    r$datasets$data_plot02
  } else if (input[["data_type"]] == get_data_names_norsk("subplot_data_02")) {
    r$datasets$data_plot03
  } else if (input[["data_type"]] == get_data_names_norsk("log_data_all")) {
    data_log$get_data_logistics()
  } else if (input[["data_type"]] == get_data_names_norsk("log_data_trn")) {
    data_log$get_data_logistics(type = "train")
  } else if (input[["data_type"]] == get_data_names_norsk("log_data_prd")) {
    data_log$get_data_logistics(type = "prdct")
  }
}
get_year_raw_data <- function(data_name) {
  tmp_end <- nchar(data_name)
  tmp_start <- tmp_end - 3
  substr(data_name, tmp_start, tmp_end)
}
get_raw_data <- function(yrs) {
  raw_data_names <- c(
    "TaskSVVdcDB::data_raw_SVV_2021",
    "TaskSVVdcDB::data_raw_SVV_2022",
    "TaskSVVdcDB::data_raw_SVV_2023",
    "TaskSVVdcDB::data_raw_SVV_2024"
  )
  id_yr <- grepl(yrs, x = raw_data_names)
  eval(parse(text = raw_data_names[id_yr]))
}
modal_download <- function(ns, session_passed) {
  shiny.semantic::create_modal(
    shiny.semantic::modal(
      shiny.semantic::action_button(ns("action_data_download_cancel"),
                                    "Avbryt/lukk."),
      id = ns("download_modal"),
      header = htmltools::h3("Download the file?", style = "text-align: center;"),
      footer = htmltools::tagList(
        shiny::downloadLink(
          outputId = ns("dataDownload"),
          label = "Yes"
        )
      ),
      class = "tiny"
    ),
    show = TRUE,
    session = session_passed
  )
}
download_handler_filename_01 <- function(input) {
  function() {
    extension <- switch(input$data_format,
      "csv" = ".csv",
      "xls" = ".xlsx",
      "spss" = ".sav",
      "rda" = ".rda",
    )
    if (input[["data_type"]] %in% get_data_names_norsk(num = 1:4)) {
      tmp_year <- substr(input[["data_type"]],
                         start = nchar(input[["data_type"]]) - 3,
                         stop = 9999)
      return(paste0("data_raw_SVV_", tmp_year, extension))
    } else if (input[["data_type"]] %in% get_data_names_norsk("seg_data_2021")) {
      return(paste0("data_segmentation_2021", extension))
    } else if (input[["data_type"]] %in% get_data_names_norsk("seg_data_2022")) {
      return(paste0("data_segmentation_2022", extension))
    } else if (input[["data_type"]] %in% get_data_names_norsk("seg_data_2023")) {
      return(paste0("data_segmentation_2023", extension))
    } else if (input[["data_type"]] %in% get_data_names_norsk("seg_data_2024")) {
      return(paste0("data_segmentation_2024", extension))
    } else if (input[["data_type"]] == get_data_names_norsk("overall_data")) {
      return(paste0("data_plot_overall", extension))
    } else if (input[["data_type"]] == get_data_names_norsk("subplot_data_01")) {
      return(paste0("data_subplot_01", extension))
    } else if (input[["data_type"]] %in% get_data_names_norsk("subplot_data_02")) {
      return(paste0("data_subplot_02", extension))
    } else if (input[["data_type"]] %in% get_data_names_norsk("log_data_all")) {
      return(paste0("data_logistics_all", extension))
    } else if (input[["data_type"]] %in% get_data_names_norsk("log_data_trn")) {
      return(paste0("data_logistics_training", extension))
    } else if (input[["data_type"]] %in% get_data_names_norsk("log_data_prd")) {
      return(paste0("data_logistics_prediction", extension))
    }
  }
}
download_handler_content_01 <- function(file, input, data_set) {
  function(file) {
    if (input$data_format == "csv") {
      write.csv(data_set(), file,
        row.names = FALSE, fileEncoding = "UTF-16LE"
      )
    } else if (input$data_format == "xls") {
      writexl::write_xlsx(data_set(), file)
    } else if (input$data_format == "spss") {
      haven::write_sav(data_set(), file)
    } else if (input$data_format == "rda") {
      data_logistics_all <- data_set()
      save(data_logistics_all, file = file)
    }
  }
}
