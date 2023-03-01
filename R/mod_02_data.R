mod_data_segmentation_srv <- function(id, r, ...) {
  ds_raw_all <- list(...)
  num_ds     <- length(ds_raw_all)
  year_seq   <- 2020 + 1:num_ds
  ds_list    <- vector("list", num_ds)
  shiny::moduleServer(id, function(input, output, session) {
    shiny::reactive({
      browser()
      sttgs_dc  <- r$ui_inputs$input_seg$sttgs_DC
      sttgs_q16 <- r$ui_inputs$input_seg$sttgs_Q16
      sttgs_q17 <- r$ui_inputs$input_seg$sttgs_Q17
      sttgs_q14 <- r$ui_inputs$input_seg$sttgs_Q14
      sttgs_q19 <- r$ui_inputs$input_seg$sttgs_Q19
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
      r$datasets$data_seg <- ds_list
    })
  })
}
