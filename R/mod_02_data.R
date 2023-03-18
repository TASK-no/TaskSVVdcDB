mod_data_segmentation_srv <- function(id, r, data_seg) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(gargoyle::watch("seg_inputs"), {
      data_seg$update_data_segmentation(r)
      gargoyle::trigger("data_seg")
    })
  })
}
DataSegmentation <- R6::R6Class(
  classname = "DataSegmentation",
  public = list(
    initialize = function(r, ...) {
      private$..r <- r
      private$..data_raw <- list(...)
      private$..num_ds   <- length(private$..data_raw)
      private$..year_seq <- 2020 + 1:private$..num_ds
    },
    get_data_segmentation = function(id = NULL) {
      if (is.null(id)) return(private$..data_segmentation)
      return(self$data_segmentation[[id]])
    },
    update_data_segmentation = function(r) {
      private$..r <- r
      private$run_segmentation()
    },
    update_cat23 = function(list_recodes,
                            SETTINGS_FACT) {
      private$run_cat23(list_recodes, SETTINGS_FACT)
    }
  ),
  private = list(
    ..r = NULL,
    ..data_raw = NULL,
    ..num_ds = NULL,
    ..year_seq = NULL,
    ..data_segmentation = NULL,
    run_segmentation = function() {
      ds_list <- vector("list", private$..num_ds)
      for (i in seq_len(private$..num_ds)) {
        ds_list[[i]] <- private$..data_raw[[i]] %>%
          TaskAnalyticsTB::segmentation_analysis(
            ind1_vals = private$..r[["seg_inputs"]][["sttgs_DC"]],
            settings_q16 = private$..r[["seg_inputs"]][["sttgs_Q16"]],
            settings_q17 = private$..r[["seg_inputs"]][["sttgs_Q17"]],
            settings_q14 = private$..r[["seg_inputs"]][["sttgs_Q14"]],
            settings_q19 = private$..r[["seg_inputs"]][["sttgs_Q19"]]) %>%
          dplyr::select(tidyselect::any_of(var_to_use_after_seg),
                        tidyselect::starts_with("kat"))
      }
      names(ds_list) <- paste0("data_", private$..year_seq)
      private$..data_segmentation <- ds_list
      invisible(ds_list)
    },
    run_cat23 = function(list_recodes,
                         SETTINGS_FACT) {
      private$..data_segmentation[["data_2023"]] <- private$..data_segmentation[["data_2023"]] %>%
        TaskAnalyticsTB::recode_qXX_cats(q_names = c("Q36", "Q37",
                                                     "Q38", "Q40"),
                                         list_recodes = list_recodes,
                                         new_names = c("Q36_c", "Q37_c",
                                                       "Q38_c", "Q40_c"),
                                         SETTINGS_FACT = SETTINGS_FACT)
    }
  )
)
