get_choices_divisions <- function(kind = "into_area") {
  tmp_into_area <- c(
    `Samlet (ikke inndelt i divisjoner)` = "all",
    `Uerfaren` = "Uerfaren",
    `Grunnleggende` = "Grunnleggende",
    `Videregående` = "Videregående",
    `Avansert` = "Avansert"
  )

  tmp_into_competence <- c(
    `Samlet (ikke inndelt i divisjoner)` = "all",
    `Kommunikasjon og samhandling` = "Kommunikasjon og samhandling",
    `Informasjonssikkerhet og personvern` = "Informasjonssikkerhet og personvern",
    `Bruk av programvare` = "Bruk av programvare",
    `Bruk av teknologi` = "Bruk av teknologi"
  )
  if (kind == "into_area") {
    return(tmp_into_area)
  } else if (kind == "into_competence") {
    return(tmp_into_competence)
  } else {
    stop("Unknown value for argument 'kind'")
  }
}
get_choices_plot_types <- function(num = NULL) {
  tmp_names <- c(
    `Digital kompetanse fordelt på kompetanseområder` = "dig_into_exp",
    `Kompetanseområder inndelt i digitale kompetanser` = "exp_into_dig"
  )
  if (is.null(num)) {
    return(tmp_names)
  } else {
    return(tmp_names[[num]])
  }
}
