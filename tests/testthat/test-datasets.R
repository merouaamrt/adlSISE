# Jeux de donnees d'exemple : dimensions, cible en facteur, et ADL tourne.

taux_erreur <- function(modele) 1 - sum(diag(modele$confusion)) / sum(modele$confusion)

test_that("wine : dimensions, cible et ajustement avec selection forward", {
  expect_equal(dim(wine), c(178, 14))
  expect_s3_class(wine$class, "factor")
  expect_equal(nlevels(wine$class), 3)
  expect_equal(as.integer(table(wine$class)), c(59, 71, 48))
  expect_false(anyNA(wine))

  m <- ADL$new(selection = "forward")$fit(wine[, 1:13], wine$class)
  expect_s3_class(m, "ADL")
  expect_gt(length(m$selected_variables), 0)
  expect_lt(taux_erreur(m), 0.05)
})

test_that("breast_cancer : dimensions, NA conserves et ajustement", {
  expect_equal(dim(breast_cancer), c(699, 10))
  expect_s3_class(breast_cancer$class, "factor")
  expect_equal(levels(breast_cancer$class), c("benign", "malignant"))
  expect_false("id" %in% names(breast_cancer))
  expect_equal(sum(is.na(breast_cancer)), 16)

  complet <- na.omit(breast_cancer)
  expect_equal(nrow(complet), 683)
  m <- ADL$new()$fit(complet[, 1:9], complet$class)
  expect_s3_class(m, "ADL")
  expect_lt(taux_erreur(m), 0.1)
})

test_that("heart_disease : dimensions, facteurs, NA conserves et ajustement", {
  expect_equal(dim(heart_disease), c(303, 14))
  expect_s3_class(heart_disease$disease, "factor")
  expect_equal(levels(heart_disease$disease), c("no", "yes"))
  for (v in c("sex", "cp", "fbs", "restecg", "exang", "slope", "thal")) {
    expect_s3_class(heart_disease[[v]], "factor")
  }
  expect_equal(sum(is.na(heart_disease)), 6)

  complet <- na.omit(heart_disease)
  expect_equal(nrow(complet), 297)
  m <- ADL$new()$fit(complet[, 1:13], complet$disease)
  expect_s3_class(m, "ADL")
  expect_lt(taux_erreur(m), 0.2)
})
