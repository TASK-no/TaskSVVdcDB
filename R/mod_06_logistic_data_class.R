DataLogistics <- R6::R6Class(
  "DataLogistics",
  public = list(
    initialize = function(data_seg) {
      private$..data_logistics_base <- data_seg
    },
    get_data_logistics = function() {
      private$..data_logistics
    },
    get_model_logistics = function() {
      private$..mod
    },
    update_data_base = function(data_seg) {
      private$..data_logistics_base <- data_seg
      private$..data_logistics <- private$..data_logistics_base %>%
        get_data_logistics_all(private$..year_taken)
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
    ..mod = NULL
  )
)
