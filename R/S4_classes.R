# S4 classes

#' S4 class for the marginal settings
#'
#' Specify which marginal models (`individual_spec` & `default_specs`) are
#'  fitted and how often they are refit as well as how big the training data
#'  set is. Remember that the forecasting is done in a rolling window fashion.
#'
#' For specifying the list for `individual_spec` or the argument `default_spec`
#'  the function [default_garch_spec()] might
#'  come in handy.
#'
#' @slot train_size Positive count specifying the training data size.
#' @slot refit_size Positive count specifying size of the forecasting
#'  window.
#' @slot individual_spec A named list. Specify ARMA-GARCH models for individual
#' assets by naming the list entry as the asset and providing a
#' [`rugarch::ugarchspec`] object.
#' @slot default_spec [`rugarch::ugarchspec`]
#'  object specifing the default marginal model (used if the marginal model
#'   is not specified through `individual_spec`)
#'
#' @return Object of class `marginal_settings`
#' @export
#'
#' @seealso [default_garch_spec()]
#'
#' @examples marginal_settings(100, 10)
setClass("marginal_settings",
  slots = list(train_size = "numeric",
               refit_size = "numeric",
               individual_spec = "list",
               default_spec = "uGARCHspec"),
  prototype = list(train_size = NA_real_,
                   refit_size = NA_real_,
                   individual_spec = list(),
                   default_spec = default_garch_spec()),
  validity = function(object) {
    error_mess <- character(0)
    train_seq_refit <- object@train_size <= object@refit_size
    train_refit_nocount <- !(checkmate::test_count(object@train_size,
                                                   positive = TRUE) &
                               checkmate::test_count(object@refit_size,
                                                     positive = TRUE))
    list_no_specs <- !checkmate::test_list(object@individual_spec,
                                           types = "uGARCHspec",
                                           names = "unique")
    if (train_seq_refit) error_mess <- "It must hold: train_size > refit_size."
    if (train_refit_nocount) error_mess <- c(error_mess, "train_size and refit_size must be positve integers.")
    if (list_no_specs) error_mess <- c(error_mess, "There are missspecified or unnamed entries in the individual_spec list.")
    if (length(error_mess)) error_mess else TRUE
  }
)

#' @describeIn marginal_settings Class constructor taking the arguments specified below
marginal_settings <- function(
  train_size, refit_size,
  individual_spec = list(), default_spec = default_garch_spec()
) {
  new("marginal_settings",
      train_size = train_size,
      refit_size = refit_size,
      individual_spec = individual_spec,
      default_spec = default_spec)
}

# simple print method for the marginal_settings class
setMethod("show", c("marginal_settings"), function(object) {
  cat("An object of class <marginal_settings>\n")
  cat("train_size:", object@train_size, "\n")
  cat("refit_size:", object@refit_size, "\n")
  if (length(object@individual_spec) == 0) {
    cat("No custom specifications.\n")
  } else {
    cat("Custom specifications were given for assets:\n ")
    for (ind_names in names(object@individual_spec)) {
      cat(ind_names, " ")
    }
    cat("\n")
  }
})
