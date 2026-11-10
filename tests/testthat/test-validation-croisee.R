test_that("La validation croisee evite les plis vides", {

  X <- data.frame(x = c(1, 2, 3, 4, 6, 7, 8, 9))
  y <- factor(rep(c("A", "B"), each = 4))

  resultat <- ADL$new()$cross_validate(
    X, y, folds = 5, seed = 123
  )

  expect_length(resultat$scores, 5)
  expect_true(all(is.finite(resultat$scores)))

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


test_that("La validation croisee accepte les classes desequilibrees", {

  # 10 individus dans A et 4 dans B
  X <- data.frame(x = c(1:10, 14:17))
  y <- factor(c(rep("A", 10), rep("B", 4)))

  # Validation croisee a 5 plis
  resultat <- ADL$new()$cross_validate(
    X, y, folds = 5, seed = 123
  )

  # On doit obtenir 5 scores valides
  expect_length(resultat$scores, 5)
  expect_true(all(is.finite(resultat$scores)))

  # Le score global doit etre compris entre 0 et 1
  expect_gte(resultat$accuracy, 0)
  expect_lte(resultat$accuracy, 1)
})



test_that("Le nombre de plis doit etre un entier valide", {

  X <- iris[, 1:4]
  y <- iris$Species

  # Un nombre decimal n'est pas valide
  expect_error(
    ADL$new()$cross_validate(X, y, folds = 2.5),
    "entier"
  )

  # Une valeur manquante doit etre refusee
  expect_error(
    ADL$new()$cross_validate(X, y, folds = NA),
    "entier"
  )

})


test_that("La validation croisee refuse une classe avec un seul individu", {

  # 10 individus dans A et seulement 1 dans B
  X <- data.frame(x = c(1:10, 15))
  y <- factor(c(rep("A", 10), "B"))

  # La validation doit refuser cette configuration
  # avec un message clair
  expect_error(
    ADL$new()$cross_validate(X, y, folds = 3, seed = 123),
    "au moins 2 observations par classe"
  )

})




test_that("La validation Leave-One-Out fonctionne correctement", {

  X <- data.frame(
    x = c(1, 2, 3, 4, 7, 8, 9, 10)
  )

  y <- factor(rep(c("A", "B"), each = 4))

  # 8 observations = 8 plis
  resultat <- ADL$new()$cross_validate(
    X, y, folds = 8, seed = 123
  )

  # Un score par observation
  expect_length(resultat$scores, 8)

  # Tous les scores doivent etre valides
  expect_true(all(is.finite(resultat$scores)))

  # Chaque pli contient un seul individu :
  # la moyenne simple est donc correcte
  expect_equal(
    resultat$accuracy,
    mean(resultat$scores)
  )

  # Coherence du taux d'erreur
  expect_equal(
    resultat$error,
    1 - resultat$accuracy
  )

})
