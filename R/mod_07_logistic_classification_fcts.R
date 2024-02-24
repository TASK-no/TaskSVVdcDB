get_roc_text <- function() {
  msg0 <- paste0("<em><u>Tolkning av ROC-kurve:</u></em>")
  msg1 <- paste0(
    "En receiver operating characteristic curve, eller ROC-kurve, er et grafisk plott som illustrerer den diagnostiske evnen til et bin\u00e6rt klassifiseringssystem (her logistisk regresjon) n\u00e5r diskrimineringsterskelen varieres. ROC-kurven opprettes ved \u00e5 plotte den sanne positive raten (TPR) mot den falske positive raten (FPR) ved forskjellige terskelinnstillinger. ")

  msg2 <- paste0(
    "Den sanne positive frekvensen er ogs\u00e5 kjent som sensitivitet, gjenkalling eller oppdagelsessannsynlighet: den gir den korrekte 1-klassifiseringen (dvs. n\u00e5r individet virkelig er 1) som en br\u00f8kdel av alle 1-klassifiseringer som inkluderer korrekte og ukorrekte (f.eks. har virkelig baseline=1 vs. uerfaren=0 digital kompetanse). Den falske positive frekvensen er ogs\u00e5 kjent som falsk alarmsannsynlighet og kan beregnes som (1 - spesifisitet): den gir br\u00f8kdelen av falske 1-klassifiseringer (dvs. n\u00e5r individet faktisk er 0) av det totale antallet sanne 0-tilfeller (f.eks. feilrapporterte basisferdigheter som faktisk er uerfarne som en br\u00f8kdel av totalt uerfarne i dataene)."
  )

  msg3 <- paste0(
    "ROC-kurven som presenteres her, er beregnet ved den 'optimale' terskelverdien. Jo st\u00f8rre arealet under kurven (AUC) er, desto bedre er modellen: n\u00e5r det gis \u00e9n tilfeldig valgt 1-instans (sanne basisferdigheter) og \u00e9n tilfeldig valgt 0-instans (sanne uerfarne ferdigheter), er AUC sannsynligheten for at klassifisereren vil kunne si hvilken som er hvilken. (<a href='https://en.wikipedia.org/wiki/Receiver_operating_characteristic' target='_blank'>mer informasjon</a>)"
  )

  paste(msg0, paste(msg1, msg2, msg3, sep = "<br/> <br/>"), sep = "<br/>")
}
plot_roc <- function(true_ones, preds) {
  if (!(is.null(true_ones) || is.null(preds))) {
    df_tmp <- data.frame(true_ones = true_ones, preds = preds)
    df_tmp <- data.frame(true_ones = true_ones, preds = preds)
    roc_plot <- ggplot2::ggplot(
      df_tmp,
      ggplot2::aes(d = .data$true_ones, m = .data$preds)) +
      plotROC::geom_roc(labelround = 2, n.cuts = 15)
    roc_plot <- roc_plot +
      plotROC::style_roc(theme = ggplot2::theme_grey) +
      ggplot2::ggtitle("ROC curve") +
      ggplot2::annotate("text", x = .75, y = .25,
                        label = paste(
                          "AUC =",
                          round(plotROC::calc_auc(roc_plot)$AUC, 4))) +
      ggplot2::scale_x_continuous("1 - Specificity (FPR)") +
      ggplot2::scale_y_continuous("Sensitivity (TPR)")
    plotROC <- shiny::renderUI({
      htmltools::HTML(
        plotROC::export_interactive_roc(roc_plot)
      )})
  } else {
    plotROC  <- NULL
  }
  return(plotROC)
}
output_roc_text <- function(true_ones, preds) {
  if (!(is.null(true_ones) || is.null(preds))) {
    roc_text <- shiny::renderUI({
      htmltools::HTML(get_roc_text())
    })
  } else {
    roc_text <- shiny::renderUI({
      htmltools::HTML("")
    })
  }
  roc_text
}
output_info_text <- function() {
  cls_info_name <- c("Optimal sannsynlighetsgrenseverdi:",
                     "Prosentandel samsvarende par:",
                     "Feilklassifiseringsfeil:")
  # Optimal cut-off value
  ocv <- paste0("optimal sannsynlighetsgrenseverdi for et gitt sett med ",
                "faktiske 1-klassifiseringer (testdata) og predikerte ",
                "sannsynlighetsverdier basert p\u00e5 treningsdata.")
  # missclassification error
  mce <- paste0("antall uoverensstemmelser mellom det predikerte (basert p\u00e5 ",
                "modell fra treningsdata) og det faktiske (i testdataene)")
  # concordance
  con <- paste0("et par er konkordant n\u00e5r den predikerte sk\u00e5ren for ",
                "1-klassifisering (hendelse = baseline) er st\u00f8rre enn for ",
                "0-klassifisering (ikke-hendelse = uerfaren).")
  info_text <- shiny::renderUI({
      htmltools::HTML(paste0(
        paste0("<em><u>", cls_info_name[1], "</u></em>", "<br/>", ocv, "<br/>"),
        paste0("<em><u>", cls_info_name[2], "</u></em>", "<br/>", mce, "<br/>"),
        paste0("<em><u>", cls_info_name[3], "</u></em>", "<br/>", con),
        sep = "<br/> <br/>"))
  })
  info_text
}
output_cls_info <- function(cls_info) {
  out_df <- get_df_for_roc_analysis(cls_info)
  shiny::renderPrint({
    shiny::validate(
      shiny::need(
        !is.null(out_df),
        get_msg_roc_logistics_data_null()
      )
    )
    out_df
  })
}
get_df_for_roc_analysis <- function(raw_cls_info) {
  if (is.null(raw_cls_info)) return(NULL)
  cls_info_name <- c("optimal sannsynlighetsgrenseverdi:",
                     "prosentandel samsvarende par:",
                     "feilklassifiseringsfeil:",
                     "sensitivitet:",
                     "\u00e9n minus spesifisitet:")
  out_df <- data.frame(
    `verdi` = round(unlist(raw_cls_info), digits = 4)[1:5])
  rownames(out_df) <- cls_info_name
  out_df
}
get_msg_roc_logistics_data_null <- function() {
  paste0("Kj\u00f8r en logistisk regresjon f\u00f8rst f\u00f8r du bruker denne funksjonen \n",
         "(se fanen logistisk regresjon): dette er n\u00f8dvendig for initialisering \n",
         "og vil ROC-kurveanalyse og relaterte funksjoner for klassifiseringsanalyse.")
}
