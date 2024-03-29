# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Comment this if you don't want the app to be served on a random port
options(shiny.port = httpuv::randomPort())

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Run the application
run_app(options = list(launch.browser = TRUE))
# options(
#   "golem.app.prod" = TRUE,
#   shiny.port = 8080,
#   launch.browser = TRUE,
#   auth0_disable = FALSE,
#   auth0_config_file = system.file("app/_auth0.yml", package = "TaskSVVdcDB"))
# run_app_auth0()
