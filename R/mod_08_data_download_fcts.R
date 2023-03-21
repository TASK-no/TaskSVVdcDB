get_data_names_norsk <- function(num) {
  c("Rådata 2021", "Rådata 2022", "Rådata 2023",
    "Overordnede/hovedplottdata",
    "Ekspertise og kompetanse data for delplott")[num]
}
get_data <- function(input, r) {
  if (input[["data_type"]] %in% get_data_names_norsk(1:3)) {
    year_taken <- get_year_raw_data(input[["data_type"]])
    get_raw_data(year_taken)
  } else if(input[["data_type"]] == get_data_names_norsk(4)) {
    r$datasets$data_plot01
  } else if(input[["data_type"]] == get_data_names_norsk(5)) {
    r$datasets$data_plot02
  }
}
get_year_raw_data <- function(data_name) {
  tmp_end <- nchar(data_name)
  tmp_start <- tmp_end - 3
  substr(data_name, tmp_start, tmp_end)
}
get_raw_data <- function(yrs) {
  raw_data_names <- c("data_raw_SVV_2021",
                      "data_raw_SVV_2022",
                      "data_raw_SVV_2023")
  id_yr <- grepl(yrs, x = raw_data_names)
  eval(parse(text = raw_data_names[id_yr]))
}
modal_download <- function(ns, session_passed) {
  shiny.semantic::create_modal(
    shiny.semantic::modal(
      shiny.semantic::action_button(ns("cancel"), "Avbryt/lukk."),
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
                        "spss" = ".sav")
    if (input[["data_type"]] %in% get_data_names_norsk(1:3)) {
      return(paste0("data_raw_SVV_", extension))
    } else if (input[["data_type"]] == get_data_names_norsk(4)) {
      return(paste0("data_plot_overall", extension))
    } else if (input[["data_type"]] == get_data_names_norsk(5)) {
      return(paste0("data_subplot_competence_expertise", extension))
    }
  }
}
download_handler_content_01 <- function(file, input, data_set){
  function(file) {
    if (input$data_format == "csv") {
      write.csv(data_set(), file,
                row.names = FALSE, fileEncoding =  "utf-16")
    } else if(input$data_format == "xls") {
      writexl::write_xlsx(data_set(), file)
      # xlsx::write.xlsx(data_set(), file, row.names = FALSE)
    } else if(input$data_format == "spss") {
      haven::write_sav(data_set(), file)
    }
  }
}
