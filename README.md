# adlSISE

adlSISE is an R package for **predictive Linear Discriminant Analysis**.
We built it as part of the R programming course of the M2 SISE
(Université Lumière Lyon 2, 2026–2027).

The idea was simple: write an LDA that is correct, fast, and pleasant to use.
The model is an R6 class with an interface close to scikit-learn
(`fit`, `predict`, `predict_proba`, `summary`), it accepts categorical
predictors, it can select variables on its own, and everything can be
explored through a Shiny application.

## Installation

```r
# install.packages("remotes")
remotes::install_github("merouaamrt/adlSISE")
library(adlSISE)
```

If the online installation fails, the package can also be installed
from the `.tar.gz` archive provided with the project:

```r
install.packages("adlSISE_0.0.0.9000.tar.gz", repos = NULL, type = "source")
```

## Quick start

```r
model <- ADL$new()
model$fit(iris[, 1:4], iris$Species)
model
```

Printing the model gives a short overview: number of observations,
classes and their sizes, and the **classification functions**.
Each class has one linear function; a new observation is assigned to the
class whose function gives the highest score.

## Understanding the results

```r
model$summary()
```

The summary goes further:

- **Wilks' lambda** for the whole model, with Rao's F and Bartlett's
  chi-square. A small lambda and a small p-value mean the classes are
  well separated.
- **One F test per variable**, which tells how much each variable adds
  once the others are already in the model.
- The **confusion matrix** on the training data.

The same information is available as properties, which is handy if you
want to reuse it in your own code:

```r
model$coefficients       # classification functions
model$wilks              # global test
model$variable_tests     # F test for each variable
model$confusion          # confusion matrix
```

## Making predictions

```r
new_flowers <- iris[c(1, 51, 101), 1:4]

model$predict(new_flowers)          # predicted class
model$predict_proba(new_flowers)    # posterior probabilities
model$score(iris[, 1:4], iris$Species)
```

On iris, the training accuracy is 98%.

## Categorical predictors

Factors are handled automatically: each one is turned into 0/1 dummy
variables, keeping the first level as reference. The same coding is
reused at prediction time.

```r
model <- ADL$new()
model$fit(warpbreaks[, c("breaks", "wool")], warpbreaks$tension)
model$summary()
```

## Variable selection

Forward and backward selection are based on Wilks' lambda, following
the stepwise approach described in R. Rakotomalala's book (see References).
The model is then re-estimated on the selected variables only.

```r
model <- ADL$new(selection = "backward", alpha = 0.01)
model$fit(iris[, 1:4], iris$Species)

model$selected_variables
model$selection_history
```

On iris, backward selection drops `Sepal.Length` and keeps the three
other variables.

## Cross-validation

```r
model <- ADL$new()
cv <- model$cross_validate(iris[, 1:4], iris$Species, folds = 10, seed = 1)
cv$accuracy
```

Folds are stratified, so each fold keeps the class proportions. When
selection is used, it is redone inside each fold, so the estimate stays honest.

## Example datasets

Three datasets from the UCI Machine Learning Repository come with the package:

```r
head(wine)
?wine
?breast_cancer
?heart_disease
```

```r
X <- wine[, names(wine) != "class"]
model <- ADL$new(selection = "forward")
model$fit(X, wine$class)
model$summary()
```

## The Shiny application

```r
run_app()
```

The application lets you:

1. load a data file (CSV with tab separator by default, or Excel),
2. choose the target, the explanatory variables and, if needed,
   illustrative variables,
3. choose the priors and the selection method,
4. browse the results: global evaluation, coefficients, relevance of
   each variable, selection steps, confusion matrix, cross-validation,
5. classify new observations, one at a time or from a file.

A demo button loads iris directly, so you can try it without any file.

## Performance

All computations are matrix-based, with no loop over observations.
On 1 million rows and 21 variables, fitting takes about 0.6 seconds and
prediction about 0.2 seconds. Results were checked against `MASS::lda`
and against the examples of the reference book: coefficients,
probabilities and tests are identical.

## References

- Rakotomalala R., *Pratique de l'Analyse Discriminante Linéaire*,
  version 1.0, Université Lumière Lyon 2, 2020.
- Venables W. N., Ripley B. D., *Modern Applied Statistics with S*,
  4th edition, Springer, 2002 (`MASS` package).
- Dua D., Graff C., *UCI Machine Learning Repository*, University of
  California, Irvine.

## License

MIT
