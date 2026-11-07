#' Categorical encoder (dummy coding)
#'
#' @description
#' R6 class that recodes the qualitative columns of a data frame into 0/1
#' dummy variables, so that they can be used by [ADL]. Each qualitative
#' variable with \eqn{m} levels becomes \eqn{m - 1} indicator columns named
#' `<variable>_<level>`; the first level is the reference and is dropped.
#' Numeric columns are kept unchanged. The levels learned by `$fit()` are
#' reused by `$transform()`, so new data are encoded the same way.
#'
#' @field categories Named list giving, for each column, the levels of the
#'   qualitative variables (`NULL` for numeric columns) (read-only).
#'
#' @examples
#' enc <- Encodeur$new()
#' M <- enc$fit_transform(iris)
#' head(M)
#' enc$categories$Species
#'
#' # the same encoding is applied to new data
#' enc$transform(iris[c(1, 51, 101), ])
#' @export
Encodeur <- R6::R6Class("Encodeur",
                        public = list(

                          #' @description Learn the levels of each qualitative
                          #'   column.
                          #' @param X Data frame (or matrix) to encode. Each
                          #'   qualitative column must have at least 2 levels.
                          #' @return The `Encodeur` object itself, invisibly.
                          fit = function(X) {
                            # memorise les modalites de chaque variable qualitative
                            if (is.matrix(X)) X <- as.data.frame(X)
                            if (!is.data.frame(X)) stop("X doit etre un data.frame.")
                            private$FCategories <- lapply(X, function(col) {
                              if (is.numeric(col)) return(NULL)       # variable numerique : rien a faire
                              niv <- levels(as.factor(col))
                              if (length(niv) < 2) {
                                stop("Une variable qualitative n'a qu'une seule modalite.")
                              }
                              niv
                            })
                            private$FVarNames <- names(X)
                            private$FFitted <- TRUE
                            invisible(self)
                          },

                          #' @description Encode a data frame with the levels
                          #'   learned by `$fit()`. Unknown levels give a warning
                          #'   and are treated as the reference level.
                          #' @param X Data frame (or matrix) containing the
                          #'   columns seen by `$fit()`.
                          #' @return A numeric matrix: numeric columns unchanged
                          #'   and one 0/1 column per non-reference level.
                          transform = function(X) {
                            # cree la matrice numerique avec les indicatrices 0/1
                            if (!private$FFitted) {
                              stop("L'encodeur n'est pas entraine : appelez $fit() d'abord.")
                            }
                            if (is.matrix(X)) X <- as.data.frame(X)
                            manquantes <- setdiff(private$FVarNames, names(X))
                            if (length(manquantes) > 0) {
                              stop("Colonnes absentes de X : ", paste(manquantes, collapse = ", "))
                            }
                            blocs <- lapply(private$FVarNames,
                                            function(v) private$encode_var(X[[v]], v))
                            do.call(cbind, blocs)
                          },

                          #' @description Learn the levels and encode the same
                          #'   data in one call.
                          #' @param X Data frame (or matrix) to encode.
                          #' @return A numeric matrix, as returned by
                          #'   `$transform()`.
                          fit_transform = function(X) {
                            # fit puis transform en une seule fois
                            self$fit(X)
                            self$transform(X)
                          }
                        ),

                        active = list(
                          categories = function() private$FCategories
                        ),

                        private = list(
                          FCategories = NULL,
                          FVarNames = NULL,
                          FFitted = FALSE,

                          # recode une seule variable
                          encode_var = function(x, v) {
                            niv <- private$FCategories[[v]]
                            if (is.null(niv)) {                       # variable numerique
                              if (!is.numeric(x)) stop("La variable ", v, " doit etre numerique.")
                              return(matrix(x, ncol = 1, dimnames = list(NULL, v)))
                            }
                            idx <- match(as.character(x), niv)        # numero de modalite de chaque ligne
                            if (anyNA(idx)) {
                              warning("Modalite inconnue pour ", v,
                                      " : traitee comme la reference.", call. = FALSE)
                            }
                            m <- matrix(0, nrow = length(x), ncol = length(niv) - 1,
                                        dimnames = list(NULL, paste0(v, "_", niv[-1])))
                            lignes <- which(idx > 1)                  # lignes qui ne sont pas la reference
                            m[cbind(lignes, idx[lignes] - 1)] <- 1    # place tous les 1 d'un coup
                            m
                          }
                        )
)
