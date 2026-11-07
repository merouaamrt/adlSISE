#' @importFrom R6 R6Class
NULL

#' Predictive Linear Discriminant Analysis
#' @export
ADL <- R6::R6Class("ADL",
                   public = list(
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
                       private$FFitted <- TRUE
                       invisible(self)
                     },

                     predict = function(X) {
                       S <- private$scores(X)
                       factor(private$FClasses[max.col(S, ties.method = "first")],
                              levels = private$FClasses)
                     },

                     predict_proba = function(X) {
                       S <- private$scores(X)
                       S <- S - apply(S, 1, max)
                       P <- exp(S)
                       P / rowSums(P)
                     },

                     score = function(X, y) {
                       if (length(y) != nrow(X)) stop("X et y n'ont pas le meme nombre de lignes.")
                       mean(as.character(self$predict(X)) == as.character(y))
                     },

                     # Validation croisee stratifiee : score moyen sur des donnees non vues
                     cross_validate = function(X, y, folds = 5, seed = NULL) {
                       if (is.matrix(X)) X <- as.data.frame(X)
                       y <- as.factor(y)
                       n <- nrow(X)
                       if (length(y) != n) stop("X et y n'ont pas le meme nombre de lignes.")
                       if (folds < 2 || folds > n) {
                         stop("'folds' doit etre compris entre 2 et le nombre d'observations.")
                       }
                       if (!is.null(seed)) set.seed(seed)

                       # numero de pli de chaque individu, en gardant les proportions des classes
                       if (folds == n) {
                         groupe <- seq_len(n)
                       } else {
                         groupe <- integer(n)
                         for (classe in levels(y)) {
                           ind <- which(y == classe)
                           groupe[ind] <- sample(rep(seq_len(folds), length.out = length(ind)))
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

                       list(scores = scores, accuracy = mean(scores), error = 1 - mean(scores))
                     },

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

                   active = list(
                     classes = function() private$FClasses,
                     priors = function() private$FPriors,
                     means = function() private$FMeans,
                     coefficients = function() private$FCoef,
                     wilks = function() private$FWilks,
                     variable_tests = function() private$FVarTests,
                     selected_variables = function() private$FSelected,
                     selection_history = function() private$FSelectionResult$history
                   ),

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
