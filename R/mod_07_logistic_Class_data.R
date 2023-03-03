# Design of R6 object for handling logistic data
Data_logistics <- R6::R6Class("Data_logisitics",
                              public = list(
                                initialize = function(...) {
                                  for (item in list(...)) {
                                    self$add(item)
                                  }
                                },
                                data_logistics_all = NULL,
                                data_logistics_learn = NULL,
                                data_logistics_predict1 = NULL,
                                data_logistics_predict2 = NULL))