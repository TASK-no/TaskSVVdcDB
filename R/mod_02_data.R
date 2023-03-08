mod_data_segmentation_srv <- function(id, ...,
                                      r) {
  # stopifnot(shiny::is.reactive(sttgs_dc))
  # stopifnot(shiny::is.reactive(sttgs_q16))
  # stopifnot(shiny::is.reactive(sttgs_q17))
  # stopifnot(shiny::is.reactive(sttgs_q14))
  # stopifnot(shiny::is.reactive(sttgs_q19))
  ds_raw_all <- list(...)
  num_ds     <- length(ds_raw_all)
  year_seq   <- 2020 + 1:num_ds
  ds_list                 <- vector("list", num_ds)
  shiny::moduleServer(id, function(input, output, session) {
    browser()
    shiny::observeEvent(gargoyle::watch("seg_inputs"), {
      browser()
      for (i in seq_len(num_ds)) {
        ds_list[[i]] <- ds_raw_all[[i]] %>%
          TaskAnalyticsTB::segmentation_analysis(ind1_vals = r[[1]](),
                                                 settings_q16 = r[[2]](),
                                                 settings_q17 = r[[3]](),
                                                 settings_q14 = r[[4]](),
                                                 settings_q19 = r[[5]]()) %>%
          dplyr::select(tidyselect::any_of(var_to_use_after_seg),
                        tidyselect::starts_with("kat"))
      }
      names(ds_list) <- paste0("data_", year_seq)
      ds_list
    })
    return(ds_list)
  })
}
