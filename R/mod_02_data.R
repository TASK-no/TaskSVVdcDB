mod_data_segmentation_srv <- function(id, ...,
                                      sttgs_dc,
                                      sttgs_q16,
                                      sttgs_q17,
                                      sttgs_q14,
                                      sttgs_q19) {
  stopifnot(shiny::is.reactive(sttgs_dc))
  stopifnot(shiny::is.reactive(sttgs_q16))
  stopifnot(shiny::is.reactive(sttgs_q17))
  stopifnot(shiny::is.reactive(sttgs_q14))
  stopifnot(shiny::is.reactive(sttgs_q19))
  ds_raw_all <- list(...)
  num_ds     <- length(ds_raw_all)
  year_seq   <- 2020 + 1:num_ds
  ds_list                 <- vector("list", num_ds)
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      for (i in seq_len(num_ds)) {
        ds_list[[i]] <- ds_raw_all[[i]] %>%
          TaskAnalyticsTB::segmentation_analysis(ind1_vals = sttgs_dc(),
                                                 settings_q16 = sttgs_q16(),
                                                 settings_q17 = sttgs_q17(),
                                                 settings_q14 = sttgs_q14(),
                                                 settings_q19 = sttgs_q19()) %>%
          dplyr::select(tidyselect::any_of(var_to_use_after_seg),
                        tidyselect::starts_with("kat"))
      }
      names(ds_list) <- paste0("data_", year_seq)
      ds_list
    })
  })
}
