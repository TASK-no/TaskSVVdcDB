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
#
set_env_deploy <- function() {
  branch_name <- system("git rev-parse --abbrev-ref HEAD", intern = TRUE)
  if (branch_name != "main") {
    env_var_name <- "TEST_NAME"
  } else {
    env_var_name <- "PRODUCTION_NAME"
  }
  Sys.setenv("APP_NAME" = Sys.getenv(env_var_name))
}
#
set_golem_maintenance <- function(MAINTENANCE = FALSE) {
  stopifnot(`Value for arg. 'MAINTENANCE' must be logical` =
              is.logical(MAINTENANCE))
  write_renviron_maintenance(MAINTENANCE)
}
write_renviron_maintenance <- function(VAL, fn_name = ".Renviron") {
  STATE_TO_WRITE <- paste0("GOLEM_MAINTENANCE_ACTIVE=", VAL)
  if (file.exists(fn_name)) {
    tmp_file <- readLines(fn_name)
    check_SET_GMA <- grepl("GOLEM_MAINTENANCE_ACTIVE", tmp_file)
    if (any(check_SET_GMA)) {
      tmp_file[check_SET_GMA] <- STATE_TO_WRITE
    } else {
      tmp_file <- c(tmp_file, STATE_TO_WRITE)
    }
    writeLines(tmp_file, fn_name)
  } else {
    writeLines(STATE_TO_WRITE, fn_name)
  }
  return(invisible(NULL))
}
#
set_golem_maintenance(FALSE)
# Authenticate
rsconnect::setAccountInfo(
  name = error_on_missing_name("SHINY_ACC_NAME"),
  token = error_on_missing_name("TOKEN"),
  secret = error_on_missing_name("SECRET")
)
# Deploy the application.
# IF USED INTERNALLY RUN:
# set_env_deploy()
# IF FROM GITHUB ACTION THEN UNCOMMENT ABOVE LINE
rsconnect::deployApp(forceUpdate = TRUE,
                     appName = error_on_missing_name("APP_NAME"),
                     account = "cologne-analytics")
