# A function to stop the script when one of the variables cannot be found.
# Also to strip quotation marks from the secrets when you supplied them
# (maybe it is just easier to never use them).
error_on_missing_name <- function(name) {
  var <- Sys.getenv(name, unset = NA)
  if (is.na(var)) {
    stop(paste0("cannot find ", name, " !"), call. = FALSE)
  }
  gsub("\"", "", var)
}
# Authenticate
rsconnect::setAccountInfo(
  name = error_on_missing_name("SHINY_ACC_NAME"),
  token = error_on_missing_name("TOKEN"),
  secret = error_on_missing_name("SECRET")
)
branch_name <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
if (branch_name == "main") {branch_name <- "MASTERNAME"} else {branch_name <- "TEST_NAME"}
# Deploy the application.
rsconnect::deployApp(forceUpdate = TRUE,
                     appName = error_on_missing_name(branch_name),
                     account = "cologne-analytics")
#   appFiles = c(
#     "app.R" # , you can specify which files to deploy,
#     # or keep this NULL to deploy everything
#   ),
#   appName = error_on_missing_name("MASTERNAME"),
#   appTitle = "shinyapplication"
# )
