
<!-- README.md is generated from README.Rmd. Please edit that file -->

# TaskSVVdcDB

## Deployment information

The app is deployed on the `shinyapps.io` server. There are two
versions:

- the development version:
  <https://cologne-analytics.shinyapps.io/TaskSVVdcDB/>
- the production version:
  <https://cologne-analytics.shinyapps.io/TaskSVVdcDBtesting/>

## To-Do List

- fix login layer
- fix logistic regressions (maybe) and add computations of probabilities
  from odds and this insigight to the app
- ROC analysis needs a fix whenever data sets are taken from a lot of
  different years or the regression is badly specified
- translation between english and norsk; use the google cloud version
  from Appsilon
- Questions for Vegard:
  - why do we use this app and what for exactly?
  - costs

<!-- badges: start -->
<!-- badges: end -->

The shiny app TaskSVVdcDB provides analysis for SVV data on their
employes and their digital competence.

## Installation

You can install the development version of TaskSVVdcDB from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TASK-no/TaskSVVdcDB")
```
