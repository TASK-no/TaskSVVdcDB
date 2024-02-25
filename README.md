
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

- fix login layer; seems difficult so get help from curso-R or thinkR
  team
- fix logistic regressions and roc curve analysis:
  - (optional) add computations of probabilities from odds to user
    output for ease of interpretability of results
  - (mandatory) fix whenever data sets are taken from a bunch of
    different years or the regressors are badly specified which will
    make roc curve analysis fail; currently, the app closes but the
    expected behavior is to show a message to the user that the analysis
    cannot be performed
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
