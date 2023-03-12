# Design of R6 object for handling logistic data
Data_logistics <- R6::R6Class("Data_logisitics",
                              public = list(
                                initialize = function(data_raw) {
                                  private$..data_raw <- data_raw
                                  },
                                class = FALSE,
                                portable = FALSE,
                                cloneable = FALSE,
                                data_logistics_all = NULL,
                                data_logistics_learn = NULL,
                                data_logistics_predict1 = NULL,
                                data_logistics_predict2 = NULL,
                                get_data_summary = function(){},
                                get_data_predcts = function(){
                                  self$data_predict1 <- data_train
                                  self$data_predict2 <- data_preds
                                  return(list(data_train = NULL,
                                              data_preds = NULL))
                                }),
                              private = list(
                                ..data_raw = NULL
                              ))
