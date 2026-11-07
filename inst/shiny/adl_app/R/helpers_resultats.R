# Fonctions pures : mise en forme des resultats (tableaux de texte) et graphiques.
# Tous les tableaux sont renvoyes en texte formate, prets pour tableOutput().

fmt_num <- function(x, digits = 4) formatC(x, format = "f", digits = digits)
fmt_p <- function(p) ifelse(is.na(p), "", ifelse(p < 1e-4, "< 0.0001", fmt_num(p, 4)))

couleurs_classes <- function(k) grDevices::hcl.colors(k, "Dark 3")

# Effectifs et probabilites a priori des classes
table_classes <- function(modele, y) {
  data.frame(Classe = modele$classes, Effectif = as.character(as.integer(table(y))),
             `A priori` = fmt_num(modele$priors, 4), check.names = FALSE, row.names = NULL)
}

# Lambda de Wilks, F de Rao et khi-deux de Bartlett
table_evaluation <- function(w) {
  data.frame(
    Test = c("Lambda de Wilks", "F de Rao", "Khi-deux de Bartlett"),
    Statistique = c(fmt_num(w$lambda, 6), fmt_num(w$F, 4), fmt_num(w$bartlett, 4)),
    ddl = c("", paste0(w$df1, " ; ", round(w$df2, 2)), as.character(w$bartlett_df)),
    `p-value` = c("", fmt_p(w$p_value), fmt_p(w$bartlett_p_value)),
    check.names = FALSE)
}

# Phrase d'interpretation du test global
phrase_evaluation <- function(w, alpha) {
  p <- if (w$p_value < 1e-4) "p < 0.0001" else paste0("p = ", fmt_num(w$p_value, 4))
  if (w$p_value < alpha) {
    paste0("Le modèle discrimine significativement les classes (", p, ", seuil alpha = ", alpha, ").")
  } else {
    paste0("Le modèle ne discrimine pas significativement les classes (", p,
           ", seuil alpha = ", alpha, ").")
  }
}

# Fonctions de classement : une ligne par variable, une colonne par classe
table_coefficients <- function(coef) {
  noms <- ifelse(rownames(coef) == "constant", "(constante)", rownames(coef))
  data.frame(Variable = noms, apply(coef, 2, fmt_num, digits = 5),
             check.names = FALSE, row.names = NULL)
}

# Pertinence des variables : Wilks.L, Partial.L, F, p-value
table_pertinence <- function(vt) {
  data.frame(Variable = rownames(vt), Wilks.L = fmt_num(vt$Wilks.L, 5),
             Partial.L = fmt_num(vt$Partial.L, 5), F = fmt_num(vt$F, 4),
             `p-value` = fmt_p(vt$p.value), check.names = FALSE, row.names = NULL)
}

# Barres horizontales des statistiques F ; plus la barre est longue, plus la
# variable est discriminante. Bleu = significative au seuil alpha.
graphique_pertinence <- function(vt, alpha) {
  vt <- vt[order(vt$F), , drop = FALSE]
  signif <- vt$p.value < alpha
  op <- graphics::par(mar = c(4.5, max(nchar(rownames(vt))) * 0.6 + 1, 3, 1))
  on.exit(graphics::par(op))
  graphics::barplot(vt$F, names.arg = rownames(vt), horiz = TRUE, las = 1, border = NA,
                    col = ifelse(signif, "#1f4e79", "#b0bcc8"),
                    xlab = "Statistique F (contribution de la variable)",
                    main = "Pertinence des variables")
  graphics::legend("bottomright", bty = "n", fill = c("#1f4e79", "#b0bcc8"),
                   legend = c(paste("significative (p <", alpha, ")"), "non significative"))
}

# Historique de la selection : colonnes formatees + action effectuee
table_historique <- function(h, methode) {
  data.frame(Étape = as.character(h$step), Variable = h$variable,
             Action = if (methode == "forward") "ajoutée" else "retirée",
             Wilks.L = fmt_num(h$Wilks.L, 5), `R² partiel` = fmt_num(h$R2.partiel, 5),
             F = fmt_num(h$F, 4), `p-value` = fmt_p(h$p.value),
             check.names = FALSE, row.names = NULL)
}

# Taux d'erreur d'une matrice de confusion
taux_erreur <- function(cm) 1 - sum(diag(cm)) / sum(cm)

# Matrice de confusion avec totaux et rappel (% bien classes) par classe
table_confusion <- function(cm) {
  m <- unclass(cm)
  rappel <- 100 * diag(m) / rowSums(m)
  corps <- data.frame(`Observée \\ Prédite` = rownames(m), m, Total = rowSums(m),
                      `Rappel (%)` = fmt_num(rappel, 1), check.names = FALSE, row.names = NULL)
  corps[] <- lapply(corps, as.character)
  total <- c("Total", as.character(colSums(m)), as.character(sum(m)),
             fmt_num(100 * sum(diag(m)) / sum(m), 1))
  names(total) <- names(corps)
  rbind(corps, total)
}

# Visuel de la matrice de confusion : couleur = part de la ligne, texte = effectif
graphique_confusion <- function(cm) {
  m <- unclass(cm)
  k <- nrow(m)
  part <- m / pmax(rowSums(m), 1)
  op <- graphics::par(mar = c(5, max(nchar(rownames(m))) * 0.6 + 2, 4, 1))
  on.exit(graphics::par(op))
  graphics::image(seq_len(k), seq_len(k), t(part)[, k:1, drop = FALSE], axes = FALSE,
                  zlim = c(0, 1), col = grDevices::colorRampPalette(c("#f4f8fb", "#1f4e79"))(50),
                  xlab = "Classe prédite", ylab = "", main = "Matrice de confusion")
  graphics::axis(1, seq_len(k), colnames(m))
  graphics::axis(2, seq_len(k), rev(rownames(m)), las = 1)
  graphics::box()
  for (i in seq_len(k)) for (j in seq_len(k)) {
    graphics::text(j, k + 1 - i, m[i, j], cex = 1.4,
                   col = if (part[i, j] > 0.5) "white" else "#1f2d3d")
  }
  graphics::mtext("Classe observée", side = 2, line = op$mar[2] - 1.5)
}

# Validation croisee : un tableau (un pli par ligne) et un graphique
table_plis <- function(scores) {
  data.frame(Pli = as.character(seq_along(scores)),
             `Taux de bonne classification` = fmt_num(scores, 4),
             `Taux d'erreur` = fmt_num(1 - scores, 4), check.names = FALSE)
}

graphique_plis <- function(scores, moyenne) {
  graphics::barplot(scores, names.arg = seq_along(scores), col = "#2a7f9e", border = NA,
                    ylim = c(0, 1), xlab = "Pli", ylab = "Taux de bonne classification",
                    main = "Validation croisée stratifiée")
  graphics::abline(h = moyenne, lty = 2, lwd = 2, col = "#c0392b")
  graphics::legend("bottomright", bty = "n", lty = 2, lwd = 2, col = "#c0392b",
                   legend = paste("moyenne =", fmt_num(moyenne, 4)))
}

# Variable illustrative numerique : statistiques par classe + ANOVA
table_illustrative_num <- function(x, y) {
  resume <- function(v) c(as.character(sum(!is.na(v))), fmt_num(mean(v, na.rm = TRUE), 3),
                          fmt_num(stats::sd(v, na.rm = TRUE), 3),
                          fmt_num(min(v, na.rm = TRUE), 3), fmt_num(max(v, na.rm = TRUE), 3))
  lignes <- rbind(t(vapply(split(x, y), resume, character(5))), Ensemble = resume(x))
  tableau <- data.frame(Classe = rownames(lignes), lignes, row.names = NULL)
  names(tableau) <- c("Classe", "Effectif", "Moyenne", "Écart-type", "Min", "Max")
  tableau
}

# p-value du test de comparaison des moyennes (ANOVA) ou NA
p_anova <- function(x, y) {
  tryCatch(stats::oneway.test(x ~ y, var.equal = TRUE)$p.value, error = function(e) NA_real_)
}

# Variable illustrative qualitative : tableau croise avec la classe (effectifs ou % ligne)
table_illustrative_qual <- function(x, y, pourcentage = FALSE) {
  tab <- table(Classe = y, x)
  if (pourcentage) {
    m <- 100 * prop.table(tab, 1)
    cellules <- apply(m, 2, fmt_num, digits = 1)
  } else {
    m <- stats::addmargins(tab)
    cellules <- apply(m, 2, as.character)
  }
  cellules <- matrix(cellules, nrow = nrow(m), dimnames = dimnames(m))
  data.frame(Classe = rownames(m), cellules, check.names = FALSE, row.names = NULL)
}

p_khi2 <- function(x, y) {
  tryCatch(suppressWarnings(stats::chisq.test(table(y, x))$p.value), error = function(e) NA_real_)
}

# Boxplot (numerique) ou barres empilees (qualitative) selon la classe
graphique_illustrative <- function(x, y, nom) {
  couleurs <- couleurs_classes(nlevels(y))
  if (is.numeric(x)) {
    graphics::boxplot(x ~ y, col = couleurs, xlab = "Classe", ylab = nom,
                      main = paste(nom, "selon la classe"))
    graphics::points(seq_len(nlevels(y)), tapply(x, y, mean, na.rm = TRUE), pch = 18, cex = 1.6)
  } else {
    prof <- t(prop.table(table(y, factor(x)), 1))
    op <- graphics::par(mar = c(4.5, 4.5, 3, 9), xpd = TRUE)
    on.exit(graphics::par(op))
    graphics::barplot(prof, col = grDevices::hcl.colors(nrow(prof), "Blues 3"), border = "white",
                      xlab = "Classe", ylab = "Proportion",
                      main = paste(nom, "selon la classe"))
    graphics::legend("topright", inset = c(-0.3, 0), legend = rownames(prof), bty = "n",
                     fill = grDevices::hcl.colors(nrow(prof), "Blues 3"), title = nom)
  }
}
