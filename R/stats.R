# Lambda de Wilks, test F de Rao et test de Bartlett (livre PADL, p. 60-62)
# W : covariance intra-classes (en 1/(n - K))
# V : covariance totale (en 1/(n - 1))
# n : nombre d'observations, K : nombre de classes
#' @noRd
wilks_test <- function(W, V, n, K) {
  p <- ncol(W)

  # versions biaisees (divisees par n), comme dans le livre
  Wb <- (n - K) / n * W
  Vb <- (n - 1) / n * V

  # lambda = det(Wb) / det(Vb), calcule avec les logarithmes (plus precis)
  log_lambda <- determinant(Wb, logarithm = TRUE)$modulus -
    determinant(Vb, logarithm = TRUE)$modulus
  lambda <- exp(as.numeric(log_lambda))

  # transformation de Rao
  A <- n - K - (p - K + 2) / 2
  B <- p^2 + (K - 1)^2 - 5
  B <- if (B > 0) sqrt((p^2 * (K - 1)^2 - 4) / B) else 1
  C <- (p * (K - 1) - 2) / 2
  df1 <- p * (K - 1)
  df2 <- A * B - C
  F_rao <- (1 - lambda^(1 / B)) / lambda^(1 / B) * df2 / df1

  # NOUVEAU : transformation de Bartlett (grands echantillons)
  chi2 <- -(n - 1 - (p + K) / 2) * log(lambda)

  list(lambda = lambda,
       F = F_rao, df1 = df1, df2 = df2,
       p_value = stats::pf(F_rao, df1, df2, lower.tail = FALSE),
       bartlett = chi2,
       bartlett_df = df1,
       bartlett_p_value = stats::pchisq(chi2, df1, lower.tail = FALSE))
}






# Test F de chaque variable (livre PADL, p. 64-66)
# Pour chaque variable : lambda sans la variable, lambda partiel, F, p-value
# Raccourci : lambda partiel = (n-K)/(n-1) * diag(V^-1) / diag(W^-1)
#' @noRd
variable_tests <- function(W_inv, V, n, K, lambda) {
  p <- ncol(W_inv)
  V_inv <- chol2inv(chol(V))
  partial <- (n - K) / (n - 1) * diag(V_inv) / diag(W_inv)
  df1 <- K - 1
  df2 <- n - K - p + 1
  F_val <- df2 / df1 * (1 / partial - 1)
  data.frame(
    Wilks.L   = lambda / partial,
    Partial.L = partial,
    F         = F_val,
    p.value   = stats::pf(F_val, df1, df2, lower.tail = FALSE),
    row.names = colnames(W_inv)
  )
}





