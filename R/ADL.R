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
                     }
                   ),
                   private = list(
                     FPriorsOption = NULL,
                     FSelection = NULL,
                     FAlpha = NULL
                   )
)
