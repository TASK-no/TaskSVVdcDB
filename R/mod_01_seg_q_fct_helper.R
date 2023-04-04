get_sttgs_seg <- function() {
 list(
    Q16 = list(
      grun = list(
        sum_score_val = 4,
        type_val = "larger-equal"
      ),
      vide = list(
        sum_score_val = 2,
        type_val = "larger-equal"
      ),
      avan = list(
        sum_score_val = 2,
        type_val = "equal"
      )
    ),
    Q17 = list(
      grun = list(
        sum_score_val = 3,
        type_val = "larger-equal"
      ),
      vide = list(
        sum_score_val = 3,
        type_val = "equal"
      ),
      avan = list(
        sum_score_val = 3,
        type_val = "equal"
      )
    ),
    Q14 = list(
      grun = list(
        sum_score_val = 2,
        type_val = "larger-equal"
      ),
      vide = list(
        sum_score_val = 3,
        type_val = "equal"
      ),
      avan = list(
        sum_score_val = 2,
        type_val = "equal"
      )
    ),
    Q19 = list(
      grun = list(
        sum_score_val = 2,
        type_val = "larger-equal"
      ),
      vide = list(
        sum_score_val = 5,
        type_val = "equal"
      ),
      avan = list(
        sum_score_val = 3,
        type_val = "equal"
      )
    )
  )
}
get_sub_ns <- function(id_prefix = NULL, num_q) {
  sum_taken <- paste0(
    "seg_q", num_q, "_",
    paste0(c("grun", "vide", "avan"), "_sum")
  )
  type_taken <- paste0(
    "seg_q", num_q, "_",
    paste0(c("grun", "vide", "avan"), "_type")
  )
  if (!is.null(id_prefix)) {
    sum_taken <- paste0(id_prefix, sum_taken)
    type_taken <- paste0(id_prefix, type_taken)
  }

  names(sum_taken) <- c("grun", "vide", "avan")
  names(type_taken) <- c("grun", "vide", "avan")

  list(sub_ns_sum = sum_taken, sub_ns_type = type_taken)
}
