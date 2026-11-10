#' @importFrom R6 R6Class
NULL

#' Predictive Linear Discriminant Analysis
#'
#' @description
#' R6 class implementing predictive linear discriminant analysis (LDA).
#' The model estimates one linear classification function per class from
#' the pooled within-class covariance matrix and assigns each observation
#' to the class with the highest score. Qualitative predictors are
#' automatically recoded into 0/1 dummy variables with [Encodeur].
#' Optional forward or backward stepwise selection keeps only the
#' variables that are significant according to Wilks' lambda F tests.
#'
#' @field classes Character vector of the class labels (read-only).
#' @field priors Named numeric vector of the prior probabilities (read-only).
#' @field means Matrix of the class-conditional means, one row per class
#'   (read-only).
#' @field coefficients Matrix of the classification functions: the first
#'   row is the constant, the other rows are the variable coefficients and
#'   there is one column per class (read-only).
#' @field wilks List with Wilks' lambda, Rao's F test and Bartlett's
#'   chi-squared test of the global model (read-only).
#' @field variable_tests Data frame with, for each variable, Wilks' lambda
#'   without the variable, the partial lambda, the F statistic and its
#'   p-value (read-only).
#' @field selected_variables Character vector of the variables used by the
#'   final model (read-only).
#' @field selection_history Data frame describing each step of the
#'   variable selection, or `NULL` when no selection was done (read-only).
#' @field confusion Confusion matrix (a `table`) of the observed classes
#'   versus the classes predicted on the training data (read-only).
#'
#' @examples
#' model <- ADL$new()
#' model$fit(iris[, 1:4], iris$Species)
#' model
#' model$coefficients
#' model$confusion
#'
#' # predictions and posterior probabilities
#' head(model$predict(iris[, 1:4]))
#' head(round(model$predict_proba(iris[, 1:4]), 4))
#' model$score(iris[, 1:4], iris$Species)
#'
#' # detailed summary with the statistical tests
#' model$summary()
#'
#' # uniform priors and forward selection of the variables
#' model2 <- ADL$new(priors = "uniform", selection = "forward")
#' model2$fit(iris[, 1:4], iris$Species)
#' model2$selected_variables
#'
#' # stratified 5-fold cross-validation
#' model$cross_validate(iris[, 1:4], iris$Species, folds = 5, seed = 1)$accuracy
#' @export
ADL <- R6::R6Class("ADL",
                   public = list(
                     #' @description Create a new (untrained) LDA model.
                     #' @param priors Prior probabilities of the classes:
                     #'   `"proportional"` (class frequencies in the training
                     #'   data), `"uniform"` (equal priors) or a numeric vector
                     #'   of non-negative values summing to 1, in the order of
                     #'   the class levels.
                     #' @param selection Variable selection method: `"none"`,
                     #'   `"forward"` or `"backward"`.
                     #' @param alpha Significance level used by the variable
                     #'   selection, strictly between 0 and 1.
                     #' @return A new `ADL` object.
                     initialize = function(priors = "proportional", selection = "none",
                                           alpha = 0.05) {
                       if (is.character(priors)) {
                         priors <- match.arg(priors, c("proportional", "uniform"))
                       } else if (!is.numeric(priors) || any(priors < 0) ||
                                  abs(sum(priors) - 1) > 1e-8) {
                         stop("'priors' doit valoir \"proportional\", \"uniform\" ",
                              "ou un vecteur numerique positif de somme 1.")
                       }
                       selection <- match.arg(selection, c("none", "forward", "backward"))
                       if (!is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
                         stop("'alpha' doit etre strictement compris entre 0 et 1.")
                       }
                       private$FPriorsOption <- priors
                       private$FSelection <- selection
                       private$FAlpha <- alpha
                       invisible(self)
                     },

                     #' @description Train the model.
                     #' @param X Data frame (or matrix) of predictors, numeric
                     #'   and/or qualitative, without missing values.
                     #' @param y Vector or factor of the classes, with at least
                     #'   two levels and one value per row of `X`.
                     #' @return The `ADL` object itself, invisibly (so calls
                     #'   can be chained).
                     fit = function(X, y) {
                       # 1. Verification des entrees
                       if (is.matrix(X)) X <- as.data.frame(X)
                       if (!is.data.frame(X)) stop("X doit etre un data.frame.")
                       if (anyNA(X)) stop("X contient des valeurs manquantes.")
                       y <- droplevels(as.factor(y))
                       if (length(y) != nrow(X)) stop("X et y n'ont pas le meme nombre de lignes.")
                       if (anyNA(y)) stop("y contient des valeurs manquantes.")
                       K <- nlevels(y)
                       if (K < 2) stop("y doit comporter au moins 2 classes.")

                       # 2. Recodage des qualitatives par l'Encodeur
                       private$FEncodeur <- Encodeur$new()
                       M <- private$FEncodeur$fit_transform(X)

                       classes <- levels(y)
                       g <- as.integer(y)
                       n_k <- tabulate(g, nbins = K)
                       n <- nrow(M)
                       if (n <= K) stop("Il faut plus d'observations que de classes.")

                       # 3. Probabilites a priori
                       pi_k <- private$compute_priors(n_k, classes)

                       # Verification de la colinearite avant la selection
                       if (private$FSelection != "none") {

                         # Centrage des variables par rapport a leur classe
                         moyennes <- rowsum(M, g) / n_k
                         M_centre <- M - moyennes[g, , drop = FALSE]

                         # Detection des colonnes lineairement dependantes
                         decomposition <- qr(M_centre)

                         if (decomposition$rank == 0) {
                           stop("Aucune variable exploitable pour la selection.")
                         }

                         # Conservation des colonnes independantes
                         if (decomposition$rank < ncol(M)) {
                           indices <- sort(
                             decomposition$pivot[seq_len(decomposition$rank)]
                           )

                           M <- M[, indices, drop = FALSE]
                         }
                       }


                       # 4. Estimation sur toutes les variables
                       est <- private$estimate(M, g, n_k, pi_k, K, classes)

                       # 5. Selection de variables, puis nouvelle estimation
                       sel <- NULL
                       if (private$FSelection != "none") {
                         methode <- if (private$FSelection == "forward") {
                           forward_selection
                         } else {
                           backward_selection
                         }
                         sel <- methode(est$W, est$V, n, K, private$FAlpha)
                         if (length(sel$selected) == 0) {
                           stop("Aucune variable n'est significative au seuil alpha = ",
                                private$FAlpha, " : augmentez 'alpha' ou essayez ",
                                "selection = \"backward\".", call. = FALSE)
                         }
                         M <- M[, sel$selected, drop = FALSE]
                         est <- private$estimate(M, g, n_k, pi_k, K, classes)
                       }

                       # 6. Memorisation
                       private$FN <- n
                       private$FNk <- stats::setNames(n_k, classes)
                       private$FVarNames <- colnames(X)
                       private$FSelected <- colnames(M)
                       private$FSelectionResult <- sel
                       private$FClasses <- classes
                       private$FPriors <- stats::setNames(pi_k, classes)
                       private$FMeans <- est$mu
                       private$FW <- est$W
                       private$FWinv <- est$W_inv
                       private$FV <- est$V
                       private$FWilks <- est$wilks
                       private$FVarTests <- est$var_tests
                       private$FCoef <- est$coef
                       pred <- max.col(cbind(1, M) %*% est$coef, ties.method = "first")
                       private$FConfusion <- table(observe = y,
                                                   predit = factor(classes[pred], levels = classes))
                       private$FFitted <- TRUE
                       invisible(self)
                     },

                     #' @description Predict the class of new observations.
                     #' @param X Data frame (or matrix) with the same columns as
                     #'   the training data.
                     #' @return A factor of predicted classes, one per row of `X`.
                     predict = function(X) {
                       # classe de plus grand score de classement
                       S <- private$scores(X)
                       factor(private$FClasses[max.col(S, ties.method = "first")],
                              levels = private$FClasses)
                     },

                     #' @description Compute the posterior probabilities of the
                     #'   classes for new observations.
                     #' @param X Data frame (or matrix) with the same columns as
                     #'   the training data.
                     #' @return A numeric matrix with one row per observation and
                     #'   one column per class; each row sums to 1.
                     predict_proba = function(X) {
                       # softmax des scores ; on retire le max de chaque ligne
                       # pour eviter un depassement numerique dans exp()
                       S <- private$scores(X)
                       S <- S - apply(S, 1, max)
                       P <- exp(S)
                       P / rowSums(P)
                     },

                     #' @description Compute the accuracy (proportion of correctly
                     #'   classified observations).
                     #' @param X Data frame (or matrix) of predictors.
                     #' @param y Observed classes, one per row of `X`.
                     #' @return A number between 0 and 1.
                     score = function(X, y) {
                       if (length(y) != nrow(X)) stop("X et y n'ont pas le meme nombre de lignes.")
                       mean(as.character(self$predict(X)) == as.character(y))
                     },

                     #' @description Estimate the accuracy by stratified
                     #'   k-fold cross-validation. The model is refitted on each
                     #'   training fold with the same options (priors, selection
                     #'   and alpha), so the variable selection is included in
                     #'   the validation.
                     #' @param X Data frame (or matrix) of predictors.
                     #' @param y Observed classes, one per row of `X`.
                     #' @param folds Number of folds, between 2 and `nrow(X)`
                     #'   (`folds = nrow(X)` gives leave-one-out).
                     #' @param seed Optional random seed, for reproducible folds.
                     #' @return A list with `scores` (accuracy of each fold),
                     #'   `accuracy` (mean accuracy) and `error` (1 - accuracy).
                     cross_validate = function(X, y, folds = 5, seed = NULL) {
                       if (is.matrix(X)) X <- as.data.frame(X)
                       y <- as.factor(y)
                       n <- nrow(X)
                       if (length(y) != n) stop("X et y n'ont pas le meme nombre de lignes.")
                       # Verification du nombre de plis
                       # Il doit etre numerique, entier, fini et compris entre 2 et n
                       if (length(folds) != 1L ||
                           !is.numeric(folds) ||
                           !is.finite(folds) ||
                           folds != floor(folds) ||
                           folds < 2 ||
                           folds > n) {

                         stop(
                           "'folds' doit etre un entier compris entre 2 et le nombre d'observations.",
                           call. = FALSE
                         )
                       }

                       # Verification des effectifs des classes
                       y <- droplevels(y)

                       if (anyNA(y)) {
                         stop("y contient des valeurs manquantes.", call. = FALSE)
                       }

                       if (nlevels(y) < 2) {
                         stop("La validation croisee necessite au moins 2 classes.",
                              call. = FALSE)
                       }

                       if (any(table(y) < 2)) {
                         stop("La validation croisee necessite au moins 2 observations par classe.",
                              call. = FALSE)
                       }





                       if (!is.null(seed)) set.seed(seed)

                       # numero de pli de chaque individu, en gardant les proportions des classes
                       if (folds == n) {
                         # Leave-One-Out : un individu par pli
                         groupe <- seq_len(n)

                       } else {
                         groupe <- integer(n)

                         if (folds <= min(table(y))) {

                           # Cas classique : repartition stratifiee par classe
                           for (classe in levels(y)) {
                             ind <- which(y == classe)

                             groupe[ind] <- sample(
                               rep(seq_len(folds), length.out = length(ind))
                             )
                           }

                         } else {

                           # Classes desequilibrees :
                           # repartir les individus sur tous les plis
                           ordre <- unlist(lapply(levels(y), function(classe) {
                             ind <- which(y == classe)
                             ind[sample.int(length(ind))]
                           }), use.names = FALSE)

                           groupe[ordre] <- rep(seq_len(folds), length.out = n)
                         }
                       }

                       # pour chaque pli : apprendre sur les autres (selection comprise),
                       # tester sur celui-ci
                       scores <- vapply(seq_len(folds), function(f) {
                         test <- groupe == f
                         modele <- ADL$new(priors = private$FPriorsOption,
                                           selection = private$FSelection,
                                           alpha = private$FAlpha)
                         modele$fit(X[!test, , drop = FALSE], y[!test])
                         modele$score(X[test, , drop = FALSE], y[test])
                       }, numeric(1))

                       # Moyenne ponderee selon le nombre d'individus par pli
                       accuracy <- weighted.mean(scores, tabulate(groupe, nbins = folds))

                       list(scores = scores, accuracy = accuracy, error = 1 - accuracy)
                     },
                     #' @description Print a detailed summary of the trained
                     #'   model: class sizes, priors, Wilks' lambda tests,
                     #'   classification functions with the F test of each
                     #'   variable, selection steps and confusion matrix.
                     #' @param ... Not used.
                     #' @return The `ADL` object itself, invisibly.
                     summary = function(...) {
                       if (!private$FFitted) {
                         cat("Analyse Discriminante Lineaire (non entrainee)\n")
                         return(invisible(self))
                       }
                       w <- private$FWilks
                       coef <- private$FCoef
                       vars <- rownames(coef)[-1]

                       cat("==================================================\n")
                       cat("  Analyse Discriminante Lineaire - Resume\n")
                       cat("==================================================\n\n")
                       cat("Observations :", private$FN, "\n")
                       cat("Classes      :", paste0(private$FClasses, " (", private$FNk, ")",
                                                    collapse = ", "), "\n")
                       cat("Priors       :", paste0(private$FClasses, " = ",
                                                    round(private$FPriors, 4), collapse = ", "), "\n\n")

                       cat("--- Evaluation globale ---\n")
                       cat(sprintf("Lambda de Wilks      : %.6f\n", w$lambda))
                       cat(sprintf("F de Rao             : %.4f   ddl = (%g, %g)   p-value = %s\n",
                                   w$F, w$df1, round(w$df2, 2), format.pval(w$p_value, digits = 4)))
                       cat(sprintf("Khi-deux de Bartlett : %.4f   ddl = %g   p-value = %s\n\n",
                                   w$bartlett, w$bartlett_df,
                                   format.pval(w$bartlett_p_value, digits = 4)))

                       cat("--- Fonctions de classement et pertinence des variables ---\n")
                       tests <- private$FVarTests[vars, , drop = FALSE]
                       tableau <- data.frame(round(coef[vars, , drop = FALSE], 6),
                                             Wilks.L = round(tests$Wilks.L, 5),
                                             Partial.L = round(tests$Partial.L, 5),
                                             F = round(tests$F, 4),
                                             p.value = signif(tests$p.value, 4),
                                             row.names = vars, check.names = FALSE)
                       print(tableau)
                       cat("constante :", paste0(colnames(coef), " = ", round(coef[1, ], 6),
                                                 collapse = ", "), "\n\n")

                       if (!is.null(private$FSelectionResult)) {
                         cat("--- Selection de variables :", private$FSelection,
                             "(alpha =", private$FAlpha, ") ---\n")
                         h <- private$FSelectionResult$history
                         if (is.null(h)) {
                           cat("Aucune variable retiree.\n")
                         } else {
                           print(h, row.names = FALSE, digits = 6)
                         }
                         cat("Variables retenues :", paste(private$FSelected, collapse = ", "), "\n\n")
                       }

                       cat("--- Matrice de confusion (donnees d'apprentissage) ---\n")
                       print(private$FConfusion)
                       erreur <- 1 - sum(diag(private$FConfusion)) / private$FN
                       cat(sprintf("\nTaux d'erreur en resubstitution : %.4f\n", erreur))
                       cat("(estimation optimiste : utilisez $cross_validate() pour une mesure honnete)\n")
                       invisible(self)
                     },

                     #' @description Print a short description of the model and
                     #'   its classification functions.
                     #' @param ... Not used.
                     #' @return The `ADL` object itself, invisibly.
                     print = function(...) {
                       if (!private$FFitted) {
                         cat("Analyse Discriminante Lineaire (non entrainee)\n")
                         cat("Appelez $fit(X, y) pour entrainer le modele.\n")
                         return(invisible(self))
                       }
                       cat("Analyse Discriminante Lineaire\n\n")
                       cat("Observations :", private$FN, "\n")
                       cat("Variables    :", length(private$FVarNames), "\n")
                       if (!is.null(private$FSelectionResult)) {
                         cat("Selection    :", private$FSelection, "(alpha =", private$FAlpha,
                             ") ->", length(private$FSelected), "variable(s) retenue(s)\n")
                       }
                       cat("Classes      :", paste0(private$FClasses, " (", private$FNk, ")",
                                                    collapse = ", "), "\n\n")
                       cat("Fonctions de classement :\n")
                       print(round(private$FCoef, 6))
                       invisible(self)
                     }
                   ),

                   # Proprietes en lecture seule : sans argument 'value',
                   # toute affectation (m$priors <- ...) provoque une erreur
                   active = list(
                     classes = function() private$FClasses,
                     priors = function() private$FPriors,
                     means = function() private$FMeans,
                     coefficients = function() private$FCoef,
                     wilks = function() private$FWilks,
                     variable_tests = function() private$FVarTests,
                     selected_variables = function() private$FSelected,
                     selection_history = function() private$FSelectionResult$history,
                     confusion = function() private$FConfusion
                   ),

                   # Etat interne : options, resultats du fit et calculs
                   private = list(
                     FPriorsOption = NULL,
                     FSelection = NULL,
                     FAlpha = NULL,
                     FFitted = FALSE,
                     FEncodeur = NULL,
                     FN = NULL,
                     FNk = NULL,
                     FVarNames = NULL,
                     FSelected = NULL,
                     FSelectionResult = NULL,
                     FClasses = NULL,
                     FPriors = NULL,
                     FMeans = NULL,
                     FW = NULL,
                     FWinv = NULL,
                     FV = NULL,
                     FWilks = NULL,
                     FVarTests = NULL,
                     FCoef = NULL,
                     FConfusion = NULL,

                     # Probabilites a priori selon l'option choisie au constructeur
                     compute_priors = function(n_k, classes) {
                       opt <- private$FPriorsOption
                       K <- length(classes)
                       if (identical(opt, "proportional")) return(n_k / sum(n_k))
                       if (identical(opt, "uniform")) return(rep(1 / K, K))
                       if (length(opt) != K) {
                         stop("'priors' doit contenir une valeur par classe (", K, ").")
                       }
                       as.numeric(opt)
                     },

                     # Tous les calculs du modele sur une matrice M (deja recodee)
                     estimate = function(M, g, n_k, pi_k, K, classes) {
                       n <- nrow(M)

                       # moyennes conditionnelles
                       mu <- rowsum(M, g) / n_k
                       rownames(mu) <- classes

                       # covariance intra-classes poolee W, en 1/(n - K)
                       centre <- M - mu[g, , drop = FALSE]
                       W <- crossprod(centre) / (n - K)

                       # inversion de W par Cholesky
                       R <- tryCatch(chol(W), error = function(e) {
                         stop("La matrice W n'est pas inversible : variables constantes ",
                              "ou colineaires.", call. = FALSE)
                       })
                       W_inv <- chol2inv(R)
                       dimnames(W_inv) <- dimnames(W)

                       # evaluation globale et test de chaque variable
                       V <- stats::cov(M)
                       wilks <- wilks_test(W, V, n, K)
                       var_tests <- variable_tests(W_inv, V, n, K, wilks$lambda)

                       # fonctions de classement
                       A <- W_inv %*% t(mu)
                       a0 <- log(pi_k) - 0.5 * colSums(t(mu) * A)
                       coef <- rbind(constant = a0, A)
                       colnames(coef) <- classes

                       list(mu = mu, W = W, W_inv = W_inv, V = V,
                            wilks = wilks, var_tests = var_tests, coef = coef)
                     },

                     # Scores de classement : [1, x] %*% coefficients, une colonne par classe
                     scores = function(X) {
                       if (!private$FFitted) {
                         stop("Le modele n'est pas entraine : appelez $fit() d'abord.")
                       }
                       if (is.matrix(X)) X <- as.data.frame(X)
                       if (!is.data.frame(X)) stop("X doit etre un data.frame.")
                       manquantes <- setdiff(private$FVarNames, colnames(X))
                       if (length(manquantes) > 0) {
                         stop("Colonnes absentes de X : ", paste(manquantes, collapse = ", "))
                       }
                       if (anyNA(X[private$FVarNames])) stop("X contient des valeurs manquantes.")
                       # meme recodage qu'au fit, puis seulement les variables retenues
                       M <- private$FEncodeur$transform(X)[, private$FSelected, drop = FALSE]
                       cbind(1, M) %*% private$FCoef
                     }
                   )
)
