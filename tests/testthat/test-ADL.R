# Comparaison avec MASS::lda sur iris
X <- iris[, 1:4]
y <- iris$Species

test_that("ADL gives the same results as MASS::lda", {
  skip_if_not_installed("MASS")
  ref <- MASS::lda(X, y)
  ref_pred <- predict(ref, X)
  m <- ADL$new()$fit(X, y)

  expect_equal(unname(m$priors), unname(ref$prior))
  expect_equal(unname(m$means), unname(ref$means))
  expect_identical(m$predict(X), ref_pred$class)
  expect_equal(unname(m$predict_proba(X)), unname(ref_pred$posterior), tolerance = 1e-6)
})

test_that("uniform priors match MASS::lda with equal priors", {
  skip_if_not_installed("MASS")
  # sous-echantillon a effectifs inegaux, pour que "uniform" differe de "proportional"
  sub <- c(1:50, 51:80, 101:150)
  ref <- MASS::lda(X[sub, ], y[sub], prior = rep(1 / 3, 3))
  m <- ADL$new(priors = "uniform")$fit(X[sub, ], y[sub])
  expect_equal(unname(m$predict_proba(X)), unname(predict(ref, X)$posterior),
               tolerance = 1e-6)
})

test_that("confusion is the read-only training confusion matrix", {
  skip_if_not_installed("MASS")
  m <- ADL$new()$fit(X, y)
  ref_class <- predict(MASS::lda(X, y), X)$class
  expect_equal(as.vector(m$confusion), as.vector(table(y, ref_class)))
  expect_equal(sum(diag(m$confusion)) / nrow(X), m$score(X, y))
  expect_error(m$confusion <- NULL)
})

test_that("rows of predict_proba sum to 1", {
  m <- ADL$new()$fit(X, y)
  expect_equal(rowSums(m$predict_proba(X)), rep(1, nrow(X)), ignore_attr = TRUE)
})

test_that("invalid arguments are rejected", {
  expect_error(ADL$new(priors = c(0.5, 0.6)))
  expect_error(ADL$new(alpha = 2))
  expect_error(ADL$new()$fit(X, y[1:10]))
  expect_error(ADL$new()$predict(X))
})
