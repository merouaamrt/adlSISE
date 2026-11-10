# Selection pas a pas des variables (forward / backward) a partir du lambda de Wilks.
# Utilisee par ADL$fit() quand selection != "none".

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





# Selection backward (livre PADL, p. 94-96)
# On retire a chaque etape la variable de plus petit F, tant que p-value > alpha
#' @noRd
backward_selection <- function(W, V, n, K, alpha = 0.05) {
  p <- ncol(W)
  noms <- colnames(W)
  Wb <- (n - K) / n * W
  Vb <- (n - 1) / n * V

  gardees <- seq_len(p)                    # on part de toutes les variables
  lambda <- lambda_subset(Wb, Vb, gardees)
  historique <- NULL

  while (length(gardees) > 0) {
    q <- length(gardees)

    # lambda du modele si on retire chaque variable


    # Retirer temporairement chaque variable candidate
    # avec une indexation directe
    lambda_sans <- vapply(
      gardees,
      function(j) {
        restantes <- gardees[gardees != j]
        lambda_subset(Wb, Vb, restantes)
      },
      numeric(1)
    )


    F_val <- (n - K - q + 1) / (K - 1) * (lambda_sans / lambda - 1)
    p_val <- stats::pf(F_val, K - 1, n - K - q + 1, lower.tail = FALSE)

    pire <- which.min(F_val)
    if (p_val[pire] <= alpha) break       # regle d'arret : elle est utile

    historique <- rbind(historique, data.frame(
      step       = p - q + 1,
      variable   = noms[gardees[pire]],
      Wilks.L    = lambda_sans[pire],
      R2.partiel = 1 - lambda / lambda_sans[pire],
      F          = F_val[pire],
      p.value    = p_val[pire]
    ))
    lambda <- lambda_sans[pire]
    gardees <- gardees[-pire]
  }

  list(selected = noms[gardees], history = historique)
}
