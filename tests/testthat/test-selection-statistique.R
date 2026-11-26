test_that("Forward choisit la variable la plus discriminante", {

  X <- iris[, 1:4]
  y <- iris$Species

  # 1. Calcul independant avec une ANOVA
  F_reference <- vapply(names(X), function(nom) {
    resultat <- summary(aov(X[[nom]] ~ y))[[1]]
    resultat[["F value"]][1]
  }, numeric(1))

  meilleure_variable <- names(which.max(F_reference))

  # 2. Selection avec notre modele ADL
  modele <- ADL$new(selection = "forward")
  modele$fit(X, y)

  historique <- modele$selection_history

  # 3. Comparaison
  expect_equal(
    historique$variable[1],
    meilleure_variable
  )

  expect_equal(
    historique$F[1],
    max(F_reference),
    tolerance = 1e-6
  )

  expect_true(
    historique$p.value[1] < 0.05
  )

})




test_that("Backward elimine une variable non informative", {

  X <- iris[, 1:4]
  y <- iris$Species

  # Creation d'une variable sans information
  # supplementaire sur les classes
  set.seed(123)
  bruit <- rnorm(nrow(X))

  X$Bruit <- residuals(
    lm(bruit ~ ., data = iris)
  )

  # Selection Backward
  modele <- ADL$new(selection = "backward")
  modele$fit(X, y)

  historique <- modele$selection_history

  # La premiere variable retiree doit etre Bruit
  expect_equal(
    historique$variable[1],
    "Bruit"
  )

  # Sa p-value doit depasser le seuil de 5 %
  expect_gt(
    historique$p.value[1],
    0.05
  )

  # Elle ne doit plus figurer dans le modele final
  expect_false(
    "Bruit" %in% modele$selected_variables
  )

})


test_that("Le test F de Backward correspond a celui de R", {

  # Donnees iris avec une variable peu informative
  set.seed(123)
  d <- iris

  bruit <- rnorm(nrow(d))
  d$Bruit <- residuals(
    lm(bruit ~ ., data = iris)
  ) + 0.05 * as.numeric(d$Species)

  X <- d[, c(names(iris)[1:4], "Bruit")]

  # Notre selection Backward
  modele <- ADL$new(selection = "backward")
  modele$fit(X, d$Species)

  historique <- modele$selection_history

  expect_gt(nrow(historique), 0)
  expect_equal(historique$variable[1], "Bruit")

  # Calcul de reference avec deux regressions
  modele_reduit <- lm(
    Bruit ~ Sepal.Length + Sepal.Width +
      Petal.Length + Petal.Width,
    data = d
  )

  modele_complet <- update(
    modele_reduit,
    . ~ . + Species
  )

  comparaison <- anova(modele_reduit, modele_complet)

  # Comparaison de la statistique F
  expect_equal(
    as.numeric(historique$F[1]),
    as.numeric(comparaison$F[2]),
    tolerance = 1e-6
  )

  # Comparaison des p-values
  expect_equal(
    as.numeric(historique$p.value[1]),
    as.numeric(comparaison$`Pr(>F)`[2]),
    tolerance = 1e-6
  )

})
