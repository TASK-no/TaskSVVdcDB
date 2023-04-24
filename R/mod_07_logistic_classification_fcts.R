get_roc_text <- function() {
  msg0 <- paste0("<em><u>Tolkning av ROC-kurve:</u></em>")
  msg1 <- paste0(
    "En receiver operating characteristic curve, eller ROC-kurve, er et grafisk plott som illustrerer den diagnostiske evnen til et binært klassifiseringssystem (her logistisk regresjon) når diskrimineringsterskelen varieres. ROC-kurven opprettes ved å plotte den sanne positive raten (TPR) mot den falske positive raten (FPR) ved forskjellige terskelinnstillinger. ")

  msg2 <- paste0(
    "Den sanne positive frekvensen er også kjent som sensitivitet, gjenkalling eller oppdagelsessannsynlighet: den gir den korrekte 1-klassifiseringen (dvs. når individet virkelig er 1) som en brøkdel av alle 1-klassifiseringer som inkluderer korrekte og ukorrekte (f.eks. har virkelig baseline=1 vs. uerfaren=0 digital kompetanse). Den falske positive frekvensen er også kjent som falsk alarmsannsynlighet og kan beregnes som (1 - spesifisitet): den gir brøkdelen av falske 1-klassifiseringer (dvs. når individet faktisk er 0) av det totale antallet sanne 0-tilfeller (f.eks. feilrapporterte basisferdigheter som faktisk er uerfarne som en brøkdel av totalt uerfarne i dataene)."
  )

  msg3 <- paste0(
    "ROC-kurven som presenteres her, er beregnet ved den 'optimale' terskelverdien. Jo større arealet under kurven (AUC) er, desto bedre er modellen: når det gis én tilfeldig valgt 1-instans (sanne basisferdigheter) og én tilfeldig valgt 0-instans (sanne uerfarne ferdigheter), er AUC sannsynligheten for at klassifisereren vil kunne si hvilken som er hvilken. (<a href='https://en.wikipedia.org/wiki/Receiver_operating_characteristic'>mer informasjon</a>)"
  )

  paste(msg0, paste(msg1, msg2, msg3, sep = "<br/> <br/>"), sep = "<br/>")
}
