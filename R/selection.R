# Lambda de Wilks d'un sous-ensemble de variables s
# (Wb, Vb : matrices biaisees calculees une seule fois sur toutes les variables)
#' @noRd
lambda_subset <- function(Wb, Vb, s) {
  if (length(s) == 0) return(1)
  exp(as.numeric(
    determinant(Wb[s, s, drop = FALSE], logarithm = TRUE)$modulus -
      determinant(Vb[s, s, drop = FALSE], logarithm = TRUE)$modulus))
}

# Selection forward (livre PADL, p. 88-90)
# On ajoute a chaque etape la variable de plus grand F, tant que p-value <= alpha
#' @noRd
forward_selection <- function(W, V, n, K, alpha = 0.05) {
  p <- ncol(W)
  noms <- colnames(W)
  Wb <- (n - K) / n * W
  Vb <- (n - 1) / n * V

  choisies <- integer(0)     # numeros des variables selectionnees
  lambda <- 1                # lambda du modele vide
  historique <- NULL

  repeat {
    candidates <- setdiff(seq_len(p), choisies)
    if (length(candidates) == 0) break
    q <- length(choisies)

    # lambda du modele si on ajoute chaque candidate
    lambda_new <- vapply(candidates,
                         function(j) lambda_subset(Wb, Vb, c(choisies, j)),
                         numeric(1))
    F_val <- (n - K - q) / (K - 1) * (lambda / lambda_new - 1)
    p_val <- stats::pf(F_val, K - 1, n - K - q, lower.tail = FALSE)

    meilleure <- which.max(F_val)
    if (p_val[meilleure] > alpha) break      # regle d'arret

    historique <- rbind(historique, data.frame(
      step       = q + 1,
      variable   = noms[candidates[meilleure]],
      Wilks.L    = lambda_new[meilleure],
      R2.partiel = 1 - lambda_new[meilleure] / lambda,
      F          = F_val[meilleure],
      p.value    = p_val[meilleure]
    ))
    choisies <- c(choisies, candidates[meilleure])
    lambda <- lambda_new[meilleure]
  }

  list(selected = noms[choisies], history = historique)
}
