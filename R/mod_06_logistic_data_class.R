DataLogistics <- R6::R6Class(
  "DataLogistics",
  public = list(
    initialize = function(data_seg) {
      private$..data_logistics_base <- data_seg
      if (!is.null(data_seg)) {
        private$..num_obs_all <- nrow(private$..data_logistics_base)
      }
    },
    get_data_logistics = function(type = "all") {
      if (type == "all") return(private$..data_logistics)
      if (type == "train") {
        return(private$..data_logistics_train)
      } else if (type == "prdct") {
        return(private$..data_logistics_prdct)
      }
    },
    get_model_logistics = function() {
      private$..mod
    },
    get_num_obs = function(type = "all") {
      if (type == "all")   return(private$..num_obs_all)
      if (type == "train") return(private$..num_obs_train)
      if (type == "prdct") return(private$..num_obs_prdct)
    },
    update_num_obs_train_prdct = function(num_obs, type = "train") {
      if (type == "train") {
        private$..num_obs_train <- min(num_obs, private$..num_obs_all)
        private$..num_obs_prdct <- private$..num_obs_all - private$..num_obs_train
      }
      if (type == "prdct") {
        private$..num_obs_prdct <- min(num_obs, private$..num_obs_all)
        private$..num_obs_train <- private$..num_obs_all - private$..num_obs_prdct
      }
      private$update_data_log_train()
      private$update_data_log_prdct()
    },
    update_data_base = function(data_seg) {
      private$..data_logistics_base <- data_seg
      private$update_data_logistics()
    },
    update_yrs = function(years) {
      private$..year_taken <- years
      private$update_data_logistics()
    },
    update_mod = function(dep, reg, exp) {
      private$..mod <- deparse_input_logistic_to_model(
        dep = dep,
        reg = reg,
        exp = exp
      )
    }
  ),
  private = list(
    ..year_taken = NULL,
    ..data_logistics_base = NULL,
    ..data_logistics = NULL,
    ..data_logistics_train = NULL,
    ..data_logistics_prdct = NULL,
    ..mod = NULL,
    ..num_obs_all = 1925,
    ..num_obs_train = 1000,
    ..num_obs_prdct = 925,
    update_data_logistics = function() {
      private$..data_logistics <- private$..data_logistics_base %>%
        get_data_logistics_all(private$..year_taken)
      if (!is.null(private$..data_logistics)) {
        private$..num_obs_all   <- nrow(private$..data_logistics)
      }
      private$..num_obs_train <- min(private$..num_obs_all,
                                     private$..num_obs_train)
      private$..num_obs_prdct <- private$..num_obs_all - private$..num_obs_train
      private$update_data_log_train()
      private$update_data_log_prdct()
    },
    update_data_log_train = function() {
      private$..data_logistics_train <- private$..data_logistics[seq_len(private$..num_obs_train), ]
    },
    update_data_log_prdct = function() {
      id_obs_prdct <- (private$..num_obs_train + 1):private$..num_obs_all
      private$..data_logistics_prdct <- private$..data_logistics[id_obs_prdct, ]
    }
  )
)
