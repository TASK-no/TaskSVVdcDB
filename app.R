# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
# options(repos = c(getOption("repos"),
#         "https://github.com/Zeta-Display/ZetaAnalyticsTB"))
# library(auth0)
pkgload::load_all(export_all = FALSE,
                  helpers = FALSE,
                  attach_testthat = FALSE)
options("golem.app.prod" = TRUE,
        shiny.port = 8081,
        auth0_disable = TRUE,
        auth0_config_file = system.file("app/_auth0.yml",
                                        package = "golemAuth0"))
# golemAuth0::run_app_auth0()
TaskSVVdcDB::run_app() # add parameters here (if any)
