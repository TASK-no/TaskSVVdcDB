
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TaskSVVdcDB

## To-Do List

- main branch has stashes locally: clarify this
- fix error in data preview in the last tab due to overcomplicated
  nesting of functions that retrieve the data set to output (C Memory
  dump errror is no memory issue but rather R’s failure to deeply nest
  function calls and/or a wrong self-recursion in the code)
- docs for data sets
- fix login layer
- fix logistic regressions (maybe) and add computations of probabilities
  from odds and this insigight to the app
- ROC analysis needs a fix whenever data sets are taken from two
  different years
- translation between english and norsk; use the google cloud version
  from Appsilon
- Questions for Vegard:
  - why do we use this app and what for exactly?
  - det norske veritas
  - costs

<!-- badges: start -->
<!-- badges: end -->

The goal of TaskSVVdcDB is to …

## Installation

You can install the development version of TaskSVVdcDB from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TASK-no/TaskSVVdcDB")
```
