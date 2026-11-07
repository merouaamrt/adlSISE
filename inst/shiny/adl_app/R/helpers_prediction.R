# Fonctions pures : preparation de nouveaux individus et prediction.

# Verifie et met en forme de nouveaux individus (data.frame) selon la
# description `infos` des explicatives du modele. Leve une erreur francaise
# si une colonne manque, n'est pas numerique, ou contient une modalite inconnue.
preparer_nouveaux <- function(nouveau, infos) {
  attendues <- names(infos)
  absentes <- setdiff(attendues, names(nouveau))
  if (length(absentes) > 0) {
    stop("Colonnes absentes du fichier : ", paste(absentes, collapse = ", "),
         ". Colonnes attendues : ", paste(attendues, collapse = ", "), ".", call. = FALSE)
  }
  X <- nouveau[attendues]
  for (v in attendues) {
    x <- X[[v]]
    info <- infos[[v]]
    if (info$type == "numérique") {
      if (!is.numeric(x)) {
        # tolere la virgule decimale ; refuse tout texte non numerique
        converti <- suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
        if (any(is.na(converti) & !is.na(x))) {
          stop("La colonne « ", v, " » doit être numérique (valeurs non numériques trouvées).",
               call. = FALSE)
        }
        x <- converti
      }
      if (any(is.infinite(x))) stop("Valeurs infinies dans « ", v, " ».", call. = FALSE)
    } else {
      x <- as.character(x)
      inconnues <- setdiff(unique(x[!is.na(x)]), info$niveaux)
      if (length(inconnues) > 0) {
        stop("Modalité(s) inconnue(s) dans « ", v, " » : ", paste(inconnues, collapse = ", "),
             " (modalités connues : ", paste(info$niveaux, collapse = ", "), ").", call. = FALSE)
      }
      x <- factor(x, levels = info$niveaux)
    }
    X[[v]] <- x
  }
  X
}

# Classe predite et probabilites a posteriori pour chaque individu.
# Les lignes incompletes recoivent NA. Renvoie les donnees d'origine + les predictions.
predire_nouveaux <- function(modele, nouveau, infos) {
  X <- preparer_nouveaux(nouveau, infos)
  complet <- stats::complete.cases(X)
  if (!any(complet)) {
    stop("Toutes les lignes contiennent des valeurs manquantes : aucune prédiction possible.",
         call. = FALSE)
  }
  classes <- modele$classes
  prediction <- rep(NA_character_, nrow(X))
  proba <- matrix(NA_real_, nrow(X), length(classes), dimnames = list(NULL, paste0("P(", classes, ")")))
  prediction[complet] <- as.character(modele$predict(X[complet, , drop = FALSE]))
  proba[complet, ] <- modele$predict_proba(X[complet, , drop = FALSE])
  data.frame(nouveau, Classe_predite = prediction, as.data.frame(proba, check.names = FALSE),
             check.names = FALSE, row.names = NULL)
}

# Probabilites d'un individu sous forme de tableau lisible (en %)
table_probabilites <- function(pred, classes) {
  p <- unlist(pred[1, paste0("P(", classes, ")")])
  data.frame(Classe = classes, `Probabilité (%)` = fmt_num(100 * p, 2), check.names = FALSE)
}

graphique_probabilites <- function(pred, classes) {
  p <- unlist(pred[1, paste0("P(", classes, ")")])
  couleurs <- ifelse(classes == pred$Classe_predite[1], "#1e8449", "#b0bcc8")
  graphics::barplot(100 * p, names.arg = classes, col = couleurs, border = NA, ylim = c(0, 100),
                    ylab = "Probabilité (%)", main = "Probabilités a posteriori")
}
