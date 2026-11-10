test_that("Le lambda de Wilks correspond a celui de R", {

  # Notre modele ADL
  modele <- ADL$new()
  modele$fit(iris[, 1:4], iris$Species)

  lambda_adl <- modele$wilks$lambda

  # Calcul de reference avec MANOVA de R
  reference <- manova(
    cbind(Sepal.Length, Sepal.Width,
          Petal.Length, Petal.Width) ~ Species,
    data = iris
  )

  lambda_r <- summary(
    reference,
    test = "Wilks"
  )$stats[1, "Wilks"]

  # Comparaison des deux resultats
  expect_equal(
    as.numeric(lambda_adl),
    as.numeric(lambda_r),
    tolerance = 1e-8
  )

})




test_that("Le test F de Rao correspond a celui de R", {

  # Notre modele ADL
  modele <- ADL$new()
  modele$fit(iris[, 1:4], iris$Species)

  # Reference avec MANOVA
  reference <- manova(
    cbind(Sepal.Length, Sepal.Width,
          Petal.Length, Petal.Width) ~ Species,
    data = iris
  )

  stats_r <- summary(reference, test = "Wilks")$stats

  # Comparaison de la statistique F
  expect_equal(
    as.numeric(modele$wilks$F),
    as.numeric(stats_r["Species", "approx F"]),
    tolerance = 1e-6
  )

  # Comparaison des degres de liberte
  expect_equal(
    as.numeric(modele$wilks$df1),
    as.numeric(stats_r["Species", "num Df"])
  )

  expect_equal(
    as.numeric(modele$wilks$df2),
    as.numeric(stats_r["Species", "den Df"])
  )

  # Comparaison des p-values
  expect_equal(
    log(as.numeric(modele$wilks$p_value)),
    log(as.numeric(stats_r["Species", "Pr(>F)"])),
    tolerance = 1e-6
  )

})




test_that("Le test de Bartlett est correctement calcule", {

  # Notre modele ADL
  modele <- ADL$new()
  modele$fit(iris[, 1:4], iris$Species)

  # Reference du lambda avec MANOVA
  reference <- manova(
    cbind(Sepal.Length, Sepal.Width,
          Petal.Length, Petal.Width) ~ Species,
    data = iris
  )

  lambda_r <- summary(reference, test = "Wilks")$
    stats["Species", "Wilks"]

  # Parametres
  n <- nrow(iris)
  p <- 4
  K <- nlevels(iris$Species)

  # Calcul de reference de Bartlett
  chi2_attendu <- -(n - 1 - (p + K) / 2) * log(lambda_r)
  ddl_attendus <- p * (K - 1)

  pvalue_attendue <- pchisq(
    chi2_attendu,
    df = ddl_attendus,
    lower.tail = FALSE
  )

  # Verification
  expect_equal(
    as.numeric(modele$wilks$bartlett),
    as.numeric(chi2_attendu),
    tolerance = 1e-8
  )

  expect_equal(
    modele$wilks$bartlett_df,
    ddl_attendus
  )

  expect_equal(
    as.numeric(modele$wilks$bartlett_p_value),
    as.numeric(pvalue_attendue),
    tolerance = 1e-8
  )

})
