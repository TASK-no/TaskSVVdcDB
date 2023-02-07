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
