DataLogistics <- R6::R6Class(
  "DataLogistics",
  public = list(
    initialize = function(data_seg, num_obs_train) {
      private$..data_logistics_base <- data_seg
      private$..num_obs_train <- num_obs_train
    },
    get_data_logistics = function(type = "all") {
      if(type = "all") return(private$..data_logistics)
      if(type = "train") {
        return(private$..data_logistics[seq_len(private$..num_obs_train), ])
      } else if (type = "prdct") {
        id_obs_prdct <- (private$..num_obs_train + 1):private$..num_obs_all
        return(private$..data_logistics[id_obs_prdct, ])
      }
    },
    get_model_logistics = function() {
      private$..mod
    },
    get_num_obs = function(type) {
      if(type == "all") return(nrow(private$..data_logistics))
      if(type == "train") return(private$..num_obs_train)
      if(type == "prdct") return(private$..num_obs_prdct)
    },
    update_num_obs_train = function(num_obs_train) {
      private$..num_obs_train <- num_obs_train
      private$..num_obs_prdct <- private$..num_obs_all - private$..num_obs_train
    },
    update_data_base = function(data_seg) {
      private$..data_logistics_base <- data_seg
      private$..data_logistics <- private$..data_logistics_base %>%
        get_data_logistics_all(private$..year_taken)
      private$..num_obs_all   <- nrow(private$..data_logistics)
      private$..num_obs_prdct <- private$..num_obs_all - private$..num_obs_train
    },
    update_yrs = function(years) {
      private$..year_taken <- years
      private$..data_logistics <- private$..data_logistics_base %>%
        get_data_logistics_all(private$..year_taken)
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
    ..mod = NULL,
    ..num_obs_all,
    ..num_obs_train,
    ..num_obs_prdct
  )
)
