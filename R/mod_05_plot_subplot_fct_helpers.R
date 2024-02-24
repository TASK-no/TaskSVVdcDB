get_choices_divisions <- function(kind = "into_area") {
  tmp_into_area <- c(
    "Samlet (ikke inndelt i divisjoner)" = "all",
    "Uerfaren" = "Uerfaren",
    "Grunnleggende" = "Grunnleggende",
    "Videreg\u00e5ende" = "Videreg\u00e5ende",
    "Avansert" = "Avansert"
  )

  tmp_into_competence <- c(
    `Samlet (ikke inndelt i divisjoner)` = "all",
    `Kommunikasjon og samhandling` = "Kommunikasjon og samhandling",
    `Informasjonssikkerhet og personvern` = "Informasjonssikkerhet og personvern",
    `Bruk av programvare` = "Bruk av programvare",
    `Bruk av teknologi` = "Bruk av teknologi",
    `Alle kompetanser` = "Alle kompetanser"
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
    "Digital kompetanse fordelt p\u00e5 kompetanseomr\u00e5der" = "dig_into_exp",
    "Kompetanseomr\u00e5der inndelt i digitale kompetanser" = "exp_into_dig"
  )
  if (is.null(num)) {
    return(tmp_names)
  } else {
    return(tmp_names[[num]])
  }
}
