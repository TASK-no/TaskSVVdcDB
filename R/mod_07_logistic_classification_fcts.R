get_roc_text <- function() {
  msg0 <- paste0("<em><u>Tolkning av ROC-kurve:</u></em>")
  msg1 <- paste0(
    "En receiver operating characteristic curve, eller ROC-kurve, er et ",
    "grafisk plott som illustrerer den diagnostiske evnen til et binært ",
    "klassifiseringssystem (her logistisk regresjon) når ",
    "diskrimineringsterskelen varieres.")

  msg2 <- paste0(
    "ROC-kurven opprettes ved å plotte den sanne positive raten (true positive rate = TPR) ",
    "mot den falske positive raten (false positive rate = FPR) ved forskjellige ",
    "terskelinnstillinger. Den sanne positive frekvensen er også kjent som ",
    "sensitivitet (sensitivity), tilbakekalling eller sannsynlighet for ",
    "deteksjon. Den falske positive frekvensen er også kjent som ",
    "sannsynlighet for falsk alarm og kan beregnes som ",
    "(1 - spesifisitet/specificity)."
  )

  msg3 <- paste0(
    "ROC-kurven som presenteres her, er beregnet ved den 'optimale' ",
    "terskelverdien. Jo større arealet under kurven er, desto bedre er ",
    "modellen. "
  )

  paste(msg0, msg1, msg2, msg3, sep = "<br/> <br/>")
}
