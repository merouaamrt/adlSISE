#' Categorical encoder (dummy coding)
#' @export
Encodeur <- R6::R6Class("Encodeur",
                        public = list(

                          # memorise les modalites de chaque variable qualitative
                          fit = function(X) {
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

                          # cree la matrice numerique avec les indicatrices 0/1
                          transform = function(X) {
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

                          # fit puis transform en une seule fois
                          fit_transform = function(X) {
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
