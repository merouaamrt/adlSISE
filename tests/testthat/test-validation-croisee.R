test_that("La validation croisee gere les plis impossibles", {

  X <- data.frame(x = c(1, 2, 3, 4, 6, 7, 8, 9))
  y <- factor(rep(c("A", "B"), each = 4))

  # 8 individus, 4 par classe.
  # On demande 5 plis : configuration a refuser proprement.
  expect_error(
    ADL$new()$cross_validate(X, y, folds = 5, seed = 123),
    "plis|classe|folds|observations"
  )

})



test_that("La moyenne des scores est bien ponderee", {

  # 11 observations reparties en 2 classes
  X <- data.frame(
    x = c(1, 2, 3, 4, 5, 6,
          3.5, 4.5, 5.5, 6.5, 7.5)
  )

  y <- factor(c(rep("A", 6), rep("B", 5)))
  folds <- 3

  # Calcul de la validation croisee
  resultat <- ADL$new()$cross_validate(
    X, y, folds = folds, seed = 42
  )

  # Reconstruction des memes plis
  set.seed(42)
  groupe <- integer(nrow(X))

  for (classe in levels(y)) {
    ind <- which(y == classe)
    groupe[ind] <- sample(
      rep(seq_len(folds), length.out = length(ind))
    )
  }

  # Nombre d'observations dans chaque pli
  tailles <- tabulate(groupe, nbins = folds)

  # Score global correct : moyenne ponderee
  attendu <- weighted.mean(resultat$scores, tailles)

  expect_equal(resultat$accuracy, attendu)
  expect_equal(resultat$error, 1 - attendu)
})
