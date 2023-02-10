mod_break_vspace <- function(size) {
  if (size == "small") {
    return(tagList(htmltools::tags$br()))
  } else if (size == "medium") {
    return(tagList(htmltools::tags$br(),
                   htmltools::tags$br(),
                   htmltools::tags$br()))
  } else if (size == "large") {
    return(tagList(htmltools::tags$br(), htmltools::tags$br(),
                   htmltools::tags$br(), htmltools::tags$br(),
                   htmltools::tags$br(), htmltools::tags$br()))
  } else {
    stop("Unknown size argument or typo!")
  }
}
get_segmentation_sub_settings <- function(sum_score_val_grun,
                                          sum_score_val_vide,
                                          sum_score_val_avan,
                                          type_val_grun,
                                          type_val_vide,
                                          type_val_avan) {
  list(sum_score_val_grun = sum_score_val_grun,
       sum_score_val_vide = sum_score_val_vide,
       sum_score_val_avan = sum_score_val_avan,
       type_val_grun = type_val_grun,
       type_val_vide = type_val_vide,
       type_val_avan = type_val_avan)
}
generate_plotly <- function(ggplot_to_use, sttgs_list) {
  plotly::ggplotly(ggplot_to_use) %>%
    plotly::layout(legend = sttgs_list)
}
filter_for_samansi_leder <- function(ds, year, SAMANSI, LEDER) {
  data_out <- ds
  if (SAMANSI && year > 2021) {
    data_out <- data_out %>% dplyr::filter(.data$SamAnsi == 1)
  }
  if(LEDER) {
    data_out <- data_out %>% dplyr::filter(.data$leder_c == "Ja")
  }
  return(data_out)
}
