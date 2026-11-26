test_that("Forward gere les variables redondantes", {

  # Donnees iris
  X <- iris[, 1:4]
  y <- iris$Species

  # Ajout d'une colonne identique a une autre
  X$Sepal.Length.copy <- X$Sepal.Length

  # Modele avec selection forward
  modele <- ADL$new(selection = "forward")

  # Le modele devrait pouvoir selectionner
  # des variables sans planter
  expect_silent(modele$fit(X, y))

  # Verification des variables retenues
  expect_gt(length(modele$selected_variables), 0)

})


test_that("Backward gere les variables redondantes", {

  X <- iris[, 1:4]
  y <- iris$Species

  # Ajout d'une variable identique
  X$Sepal.Length.copy <- X$Sepal.Length

  # Modele avec variable redondante
  modele <- ADL$new(selection = "backward")
  expect_silent(modele$fit(X, y))

  # Modele de reference sans doublon
  reference <- ADL$new(selection = "backward")
  reference$fit(iris[, 1:4], y)

  # Comparaison des predictions
  expect_identical(
    modele$predict(X),
    reference$predict(iris[, 1:4])
  )

  # Verification des variables retenues
  expect_gt(length(modele$selected_variables), 0)

})



test_that("La selection est stable si on change l'ordre des colonnes", {

  X <- iris[, 1:4]
  X$Sepal.Length.copy <- X$Sepal.Length
  y <- iris$Species

  # Memes donnees, ordre des colonnes different
  X2 <- X[, rev(names(X))]

  for (methode in c("forward", "backward")) {

    modele1 <- ADL$new(selection = methode)
    modele2 <- ADL$new(selection = methode)

    modele1$fit(X, y)
    modele2$fit(X2, y)

    # Les predictions doivent rester identiques
    expect_identical(
      modele1$predict(X),
      modele2$predict(X2)
    )

    # Les probabilites doivent rester equivalentes
    expect_equal(
      unname(modele1$predict_proba(X)),
      unname(modele2$predict_proba(X2)),
      tolerance = 1e-6
    )
  }
})
