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

                       # NOUVEAU (1) - recodage des qualitatives par l'Encodeur
                       private$FEncodeur <- Encodeur$new()
                       M <- private$FEncodeur$fit_transform(X)

                       classes <- levels(y)
                       g <- as.integer(y)
                       n_k <- tabulate(g, nbins = K)

                       # 2. Probabilites a priori
                       pi_k <- private$compute_priors(n_k, classes)

                       # 3. Moyennes conditionnelles
                       mu <- rowsum(M, g) / n_k
                       rownames(mu) <- classes

                       # 4. Covariance intra-classes poolee W, en 1/(n - K)
                       n <- nrow(M)
                       if (n <= K) stop("Il faut plus d'observations que de classes.")
                       centre <- M - mu[g, , drop = FALSE]
                       W <- crossprod(centre) / (n - K)

                       # 5. Inversion de W par Cholesky
                       R <- tryCatch(chol(W), error = function(e) {
                         stop("La matrice W n'est pas inversible : variables constantes ",
                              "ou colineaires.", call. = FALSE)
                       })
                       W_inv <- chol2inv(R)
                       dimnames(W_inv) <- dimnames(W)

                       # 6. Fonctions de classement
                       A <- W_inv %*% t(mu)
                       a0 <- log(pi_k) - 0.5 * colSums(t(mu) * A)
                       coef <- rbind(constant = a0, A)
                       colnames(coef) <- classes

                       # Memorisation
                       private$FN <- n
                       private$FNk <- stats::setNames(n_k, classes)
                       private$FVarNames <- colnames(X)
                       private$FClasses <- classes
                       private$FPriors <- stats::setNames(pi_k, classes)
                       private$FMeans <- mu
                       private$FW <- W
                       private$FWinv <- W_inv
                       private$FCoef <- coef
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

                     print = function(...) {
                       if (!private$FFitted) {
                         cat("Analyse Discriminante Lineaire (non entrainee)\n")
                         cat("Appelez $fit(X, y) pour entrainer le modele.\n")
                         return(invisible(self))
                       }
                       cat("Analyse Discriminante Lineaire\n\n")
                       cat("Observations :", private$FN, "\n")
                       cat("Variables    :", length(private$FVarNames), "\n")
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
                     coefficients = function() private$FCoef
                   ),

                   private = list(
                     FPriorsOption = NULL,
                     FSelection = NULL,
                     FAlpha = NULL,
                     FFitted = FALSE,
                     FEncodeur = NULL,   # NOUVEAU (2)
                     FN = NULL,
                     FNk = NULL,
                     FVarNames = NULL,
                     FClasses = NULL,
                     FPriors = NULL,
                     FMeans = NULL,
                     FW = NULL,
                     FWinv = NULL,
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
                       # NOUVEAU (3) - meme recodage qu'au fit
                       M <- private$FEncodeur$transform(X)
                       cbind(1, M) %*% private$FCoef
                     }
                   )
)
