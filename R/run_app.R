#' Launch the interactive LDA application
#'
#' @description
#' Starts the Shiny application shipped with the package. The application
#' (in French) lets you load a CSV or Excel file, choose the target, the
#' explanatory and the illustrative variables, fit an [ADL] model, inspect
#' the results (global tests, classification functions, variable relevance,
#' confusion matrix, cross-validation, illustrative variables) and use the
#' model to classify a single individual or a file of new individuals.
#'
#' Reading Excel files requires the suggested package \pkg{readxl}.
#'
#' @param launch.browser Should the application open in the default web
#'   browser? Defaults to `TRUE`.
#' @param ... Further arguments passed to [shiny::runApp()], for example
#'   `port`.
#'
#' @return Called for its side effect: the application runs until it is
#'   stopped.
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#' @export
run_app <- function(launch.browser = TRUE, ...) {
  dossier <- system.file("shiny", "adl_app", package = "adlSISE")
  if (!nzchar(dossier)) {
    stop("Application introuvable : reinstallez le package adlSISE.", call. = FALSE)
  }
  shiny::runApp(dossier, launch.browser = launch.browser, ...)
}
