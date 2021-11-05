# S4 classes --------------------------------------------------------

# marginal_settings --------------------------------------------------------

#' S4 class for the marginal settings
#'
#' Specify which marginal models (`individual_spec` & `default_specs`) are
#'  fitted and how often they are refit as well as how big the training data
#'  set is. Remember that the forecasting is done in a rolling window fashion.
#'
#' For specifying the list for `individual_spec` or the argument `default_spec`
#'  the function [`default_garch_spec()`] might
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
#' @include default_garch_spec.R
#' @seealso [default_garch_spec()]
#'
#' @examples # marginal_settings(100, 10)
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


#' Constructor
#'
#' @param train_size equivalent to the slot definition below
#' @param refit_size equivalent to the slot definition below
#' @param individual_spec equivalent to the slot definition below
#' @param default_spec equivalent to the slot definition below
#' @export
#' @describeIn marginal_settings Class constructor taking the arguments
#'  specified in the slots below
marginal_settings <- function(
  train_size, refit_size,
  individual_spec = list(), default_spec = default_garch_spec()
) {
  methods::new("marginal_settings",
               train_size = train_size,
               refit_size = refit_size,
               individual_spec = individual_spec,
               default_spec = default_spec)
}


#' @export
#' @param object An object of class `marginal_settings`
#' @rdname marginal_settings-class
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


# vine_settings --------------------------------------------------------


#' S4 class for the vine settings
#'
#' Specify which vine copula models are
#'  fitted and how often they are refit as well as how big the training data
#'  set is. Remember that the estimation process is done in a rolling window
#'  fashion.
#'
#' @slot train_size Positive count specifying the training data size.
#' @slot refit_size Positive count specifying for how many periods a vine is
#' used
#' @slot family_set Character vector specifying the family of copulas that are
#' used. For possible choices see [`rvinecopulib::bicop`].
#' @slot vine_type character value that specifies which vine class should be
#' fitted. Possible choices right now are `rvine` (regular vine) and `dvine`
#' (drawable vine).
#'
#' @return Object of class `vine_settings`
#' @export
#'
#' @examples # vine_settings(100, 25)
setClass("vine_settings",
         slots = list(train_size = "numeric",
                      refit_size = "numeric",
                      family_set = "character",
                      vine_type = "character"),
         prototype = list(train_size = NA_real_,
                          refit_size = NA_real_,
                          family_set = "all",
                          vine_type = "rvine"),
         validity = function(object) {
           error_mess <- character(0)
           train_seq_refit <- object@train_size <= object@refit_size
           train_refit_nocount <- !(checkmate::test_count(object@train_size,
                                                          positive = TRUE) &
                                      checkmate::test_count(object@refit_size,
                                                            positive = TRUE))
           invalid_type <- !checkmate::test_choice(
             object@vine_type, choices = c("rvine", "dvine"))
           if (train_seq_refit) error_mess <- "It must hold: train_size > refit_size."
           if (train_refit_nocount) error_mess <- c(error_mess, "train_size and refit_size must be positve integers.")
           if (invalid_type) error_mess <- c(error_mess, "vine_type is invalid.")
           if (length(error_mess)) error_mess else TRUE
         }
)

#' Constructor
#'
#' @param train_size equivalent to the slot definition below
#' @param refit_size equivalent to the slot definition below
#' @param family_set equivalent to the slot definition below
#' @param vine_type equivalent to the slot definition below
#' @export
#' @describeIn vine_settings Class constructor taking the arguments
#'  specified in the slots below
vine_settings <- function(
  train_size, refit_size,
  family_set = "all", vine_type = "rvine"
) {
  methods::new("vine_settings",
               train_size = train_size,
               refit_size = refit_size,
               family_set = family_set,
               vine_type = vine_type)
}


#' @export
#' @param object An object of class `vine_settings`
#' @rdname vine_settings-class
setMethod("show", c("vine_settings"), function(object) {
  cat("An object of class <vine_settings>\n")
  cat("train_size:", object@train_size, "\n")
  cat("refit_size:", object@refit_size, "\n")
  cat("family_set:", object@family_set, "\n")
  cat("vine_type:", object@vine_type, "\n")
})

















