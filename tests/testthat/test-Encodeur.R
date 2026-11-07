test_that("Encodeur builds dummy variables with the first level as reference", {
  enc <- Encodeur$new()
  M <- enc$fit_transform(iris)
  expect_equal(colnames(M), c(names(iris)[1:4], "Species_versicolor", "Species_virginica"))
  expect_equal(unname(M[, "Species_versicolor"]), as.numeric(iris$Species == "versicolor"))
  expect_equal(unname(M[, 1:4]), unname(as.matrix(iris[, 1:4])))
})

test_that("Encodeur reuses the learned levels on new data", {
  enc <- Encodeur$new()$fit(iris)
  M <- enc$transform(iris[101, ])
  expect_equal(unname(M[1, c("Species_versicolor", "Species_virginica")]), c(0, 1))
  expect_warning(enc$transform(data.frame(iris[1, 1:4], Species = "inconnue")))
})
