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

                     # NOUVEAU : apprentissage (partie 1)
                     fit = function(X, y) {
                       # 1. Verification des entrees
                       if (is.matrix(X)) X <- as.data.frame(X)
                       if (!is.data.frame(X)) stop("X doit etre un data.frame.")
                       if (anyNA(X)) stop("X contient des valeurs manquantes.")
                       if (!all(vapply(X, is.numeric, logical(1)))) {
                         stop("Pour l'instant, seules les variables quantitatives sont acceptees.")
                       }
                       y <- droplevels(as.factor(y))
                       if (length(y) != nrow(X)) stop("X et y n'ont pas le meme nombre de lignes.")
                       if (anyNA(y)) stop("y contient des valeurs manquantes.")
                       K <- nlevels(y)
                       if (K < 2) stop("y doit comporter au moins 2 classes.")

                       M <- as.matrix(X)
                       classes <- levels(y)
                       g <- as.integer(y)               # numero de classe de chaque individu
                       n_k <- tabulate(g, nbins = K)    # effectif de chaque classe

                       # 2. Probabilites a priori
                       pi_k <- private$compute_priors(n_k, classes)

                       # 3. Moyennes conditionnelles (une ligne par classe)
                       mu <- rowsum(M, g) / n_k
                       rownames(mu) <- classes

                       # Memorisation
                       private$FClasses <- classes
                       private$FPriors <- stats::setNames(pi_k, classes)
                       private$FMeans <- mu
                       invisible(self)
                     }
                   ),

                   # NOUVEAU : proprietes en lecture seule
                   active = list(
                     priors = function() private$FPriors,
                     means = function() private$FMeans
                   ),

                   private = list(
                     FPriorsOption = NULL,
                     FSelection = NULL,
                     FAlpha = NULL,
                     FClasses = NULL,   # NOUVEAU
                     FPriors = NULL,    # NOUVEAU
                     FMeans = NULL,     # NOUVEAU

                     # NOUVEAU : calcule les priors selon l'option choisie
                     compute_priors = function(n_k, classes) {
                       opt <- private$FPriorsOption
                       K <- length(classes)
                       if (identical(opt, "proportional")) return(n_k / sum(n_k))
                       if (identical(opt, "uniform")) return(rep(1 / K, K))
                       if (length(opt) != K) {
                         stop("'priors' doit contenir une valeur par classe (", K, ").")
                       }
                       as.numeric(opt)
                     }
                   )
)
