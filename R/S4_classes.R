# S4 classes --------------------------------------------------------

########################################################################
# marginal_settings ----------------------------------------------------
########################################################################

#' S4 class for the marginal settings
#'
#' Specify which marginal models (`individual_spec` & `default_specs`) are
#'  fitted and how often they are refit as well as how big the training data
#'  set is. Remember that the forecasting is done in a rolling window fashion
#'  and the arguments (train and refit size) will have to match with
#'  the arguments of the also to be specified [`vine_settings`].
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
#'
#' @include default_garch_spec.R
#' @seealso [`default_garch_spec()`], [`vine_settings`]
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
#' @importFrom methods show
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

########################################################################
# vine_settings --------------------------------------------------------
########################################################################

#' S4 class for the vine settings
#'
#' Specify which vine copula models are
#'  fitted and how often they are refit as well as how big the training data
#'  set is. Remember that the estimation process is done in a rolling window
#'  fashion and the arguments (train and refit size) will have to match with
#'  the arguments of the also to be specified [`marginal_settings`].
#'
#' @slot train_size Positive count specifying the training data size.
#' @slot refit_size Positive count specifying for how many periods a vine is
#' used
#' @slot family_set Character vector specifying the family of copulas that are
#' used. For possible choices see [`rvinecopulib::bicop`]. Note for conditional
#' sampling just parametric copula families are possible so do not use the
#' family arguments `all` and `tll`.
#' @slot vine_type character value that specifies which vine class should be
#' fitted. Possible choices right now are `rvine` (regular vine) and `dvine`
#' (drawable vine).
#'
#' @return Object of class `vine_settings`
#'
#' @seealso [`marginal_settings`]
#'
#' @examples vine_settings(100, 25)
setClass("vine_settings",
         slots = list(train_size = "numeric",
                      refit_size = "numeric",
                      family_set = "character",
                      vine_type = "character"),
         prototype = list(train_size = NA_real_,
                          refit_size = NA_real_,
                          family_set = "parametric",
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
#' @importFrom methods show
#' @rdname vine_settings-class
setMethod("show", c("vine_settings"), function(object) {
  cat("An object of class <vine_settings>\n")
  cat("train_size:", object@train_size, "\n")
  cat("refit_size:", object@refit_size, "\n")
  cat("family_set:", object@family_set, "\n")
  cat("vine_type:", object@vine_type, "\n")
})

########################################################################
# portvine_roll --------------------------------------------------------
########################################################################

#' S4 output class for the function `estimate_risk_roll()`
#'
#' The main output class for the function [`estimate_risk_roll()`]is
#' `portvine_roll` but in the conditional case
#' the child class `cond_portvine_roll` with some extra slots (below visible
#' by the !C!) is returned.
#'
#' For easy access for the most important slots and some filtering functionality
#' have a look at the accessor methods [`risk_estimates()`], [`fitted_vines()`],
#' [`fitted_marginals()`].
#'
#' @slot risk_estimates data.table with the columns `risk_measure`,
#' `risk_est`, `alpha`, `row_num`, `vine_window` and `realized` (here all
#' samples also in the conditional case are used)
#' @slot fitted_marginals named list with an entry for each asset containing a
#' [`rugarch::ugarchroll`] class object that encompasses the marginal model fit.
#' @slot fitted_vines list of [`rvinecopulib::vinecop`] class objects each entry
#'  corresponds to one vine window.
#' @slot marginal_settings containing the specification used for the ARMA-GARCH
#'  fitting i.e. marginal models. Is of class `marginal_settings`.
#' @slot vine_settings containing the specifications used for the vine fitting.
#'  Is of class `vine_settings`.
#' @slot risk_measures a character vector displaying the estimated risk
#'  measures.
#' @slot alpha numeric vector in (0,1) displaying the confidence levels used
#' when estimating the risk measures.
#' @slot weights the numeric positive weights of the assets.
#' @slot cond_estimation logical value indicating whether the conditional
#'  estimation approach for the risk measures was used.
#' @slot n_samples positive numeric count displaying how many return samples
#' were used for the risk measure estimation.
#' @slot time_taken numeric value displaying how many minutes the whole estimation process
#' took.
#'
#' @seealso [`estimate_risk_roll()`], [`risk_estimates()`], [`fitted_vines()`],
#' [`fitted_marginals()`]
#'
#' @return object of class `portvine_roll`
#'
setClass("portvine_roll",
         slots = list(
           risk_estimates = "data.table",
           fitted_marginals = "list", # uGARCHroll entries S4
           fitted_vines = "list", # vinecop objects
           marginal_settings = "marginal_settings",
           vine_settings = "vine_settings",
           risk_measures = "character",
           alpha = "numeric",
           weights = "numeric",
           cond_estimation = "logical",
           n_samples = "numeric",
           time_taken = "numeric"
         ),
         validity = function(object) {
           error_mess <- character(0)
           col_risk_est <- !checkmate::test_subset(
             colnames(object@risk_estimates), c("risk_measure", "risk_est",
                                                "alpha", "row_num",
                                                "vine_window", "realized")
           )
           marg_mod_entries <- !checkmate::test_list(
             object@fitted_marginals, types = "uGARCHroll", any.missing = FALSE,
             names = "unique"
           )
           fit_vines_entries <- !checkmate::test_list(
             object@fitted_vines, types = "vinecop", any.missing = FALSE
           )

           if (col_risk_est) error_mess <- "risk_estimates is missspecified."
           if (marg_mod_entries) error_mess <- c(error_mess, "fitted_marginals has invalid entries/ is not named.")
           if (fit_vines_entries) error_mess <- c(error_mess, "fitted_vines has invalid entries.")
           if (length(error_mess)) error_mess else TRUE
         }
)

#' @slot cond_risk_estimates !C! data.table with the same columns as the
#'  `risk_estimate` slot has + the additional conditional columns with the
#'  respective conditioning value and the column `cond_alpha` that indicates
#'   the used conditional quantile level.
#' @slot cond_vars !C! character vector with the names of the variables that were
#' used to sample conditionally from.
#' @slot cond_alpha !C! a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures were calculated.
#'
#' @return object of class `cond_portvine_roll`
#'
#' @rdname portvine_roll-class
setClass("cond_portvine_roll",
         contains = "portvine_roll",
         slots = list(
           cond_risk_estimates = "data.table",
           cond_vars = "character",
           cond_alpha = "numeric"
         ),
         validity = function(object) {
           error_mess <- character(0)
           col_crisk_est <- !checkmate::test_subset(
             c("risk_measure", "risk_est", "alpha", "row_num", "vine_window",
               "realized"),
             colnames(object@cond_risk_estimates)

           )
           if (col_crisk_est) error_mess <- "cond_risk_estimates is missspecified."
           if (length(error_mess)) error_mess else TRUE
         }
)

### print methods ---------------------------------------------------------

#' @export
#' @param object An object of class `portvine_roll` or `cond_portvine_roll`
#' @importFrom methods show
#' @rdname portvine_roll-class
setMethod("show", c("portvine_roll"), function(object) {
  if (!object@cond_estimation) cat("An object of class <portvine_roll>\n")
  cat("Number of ARMA-GARCH/ marginal windows:",
      object@fitted_marginals[[1]]@model$n.refits, "\n")
  cat("Number of vine windows:", length(object@fitted_vines), "\n")
  cat("Risk measures estimated:", object@risk_measures, "\n")
  cat("Alpha levels used:", object@alpha, "\n")
  cat("\nTime taken:", round(object@time_taken, 4), "minutes",
      "\n")
})

#' @export
#' @importFrom methods show
#' @rdname portvine_roll-class
setMethod("show", c("cond_portvine_roll"), function(object) {
  cat("An object of class <cond_portvine_roll>\n")
  cat("Conditional variable(s):", object@cond_vars, "\n")
  methods::callNextMethod()
})


### summary methods ---------------------------------------------------------

#' @export
#' @param object An object of class `portvine_roll` or `cond_portvine_roll`
#' @rdname portvine_roll-class
setMethod("summary", c("portvine_roll"), function(object) {
  if (!object@cond_estimation) cat("An object of class <portvine_roll>\n")
  cat("\n--- Marginal models ---\n")
  cat("Number of ARMA-GARCH/ marginal windows:",
      object@fitted_marginals[[1]]@model$n.refits, "\n")
  cat("Train size: ", object@marginal_settings@train_size, "\n")
  cat("Refit size: ", object@marginal_settings@refit_size, "\n")

  cat("\n--- Vine copula models ---\n")
  cat("Number of vine windows:", length(object@fitted_vines), "\n")
  cat("Train size: ", object@vine_settings@train_size, "\n")
  cat("Refit size: ", object@vine_settings@refit_size, "\n")
  cat("Vine copula type: ", object@vine_settings@vine_type, "\n")
  cat("Vine family set: ", object@vine_settings@family_set, "\n")

  cat("\n--- Risk estimation ---\n")
  cat("Risk measures estimated:", object@risk_measures, "\n")
  cat("Alpha levels used:", object@alpha, "\n")
  cat("Number of estimated risk measures:", nrow(object@risk_estimates), "\n")
  cat("Number of samples for each risk estimation:", object@n_samples, "\n")

  cat("\nTime taken:", round(object@time_taken, 4), "minutes.",
      "\n")
  invisible(NULL)
})

#' @export
#' @rdname portvine_roll-class
setMethod("summary", c("cond_portvine_roll"), function(object) {
  cat("An object of class <cond_portvine_roll>\n")

  cat("\n--- Conditional settings ---\n")
  cat("Conditional variable(s):", object@cond_vars, "\n")
  cat("Number of conditional estimated risk measures:",
      nrow(object@cond_risk_estimates), "\n")
  cat("Conditioning quantiles:", object@cond_alpha, "\n")
  methods::callNextMethod()
  invisible(NULL)
})

### accessor methods for the risk estimates --------------------------------

#' Accessor methods for the risk estimates of `(cond_)portvine_roll` objects
#'
#' @param roll Object of class `portvine_roll` or a child class
#' @param risk_measures Character vector of risk measures to filter for. Note
#' that they must be fitted in the `roll` argument. The default will return all
#' fitted risk measures
#' @param alpha Numeric \eqn{\alpha} levels of the estimated risk measures to
#' filter
#' for. Note that they must be fitted in the `roll` argument. The default will
#' return all fitted \eqn{\alpha} levels
#' @param df Logical value if `TRUE` a `data.frame` is returned otherwise a
#' `data.table` is returned.
#' @param exceeded Logical value. If set to `TRUE` a column named `exceeded`
#' will be appended that contains logical values telling whether the realized
#' portfolio value exceeded the estimated risk.
#' @param ... Additional parameters for child class methods
#'
#' @return (Un-)filtered `data.frame` or `data.table` (see `df` argument) with
#'  at least the columns
#' `risk_measure`, `risk_est`, `alpha`, `row_num`, `vine_window` and `realized`.
#' `exceeded` column if the corresponding argument is set to `TRUE`.
#' In the conditional case further columns are available (see:
#'  [`portvine_roll-class`].
#' @export
#'
#' @seealso [`portvine_roll-class`]
setGeneric(
  "risk_estimates",
  function(roll, risk_measures = NULL, alpha = NULL,
           df = TRUE, exceeded = FALSE, ...) {
    standardGeneric("risk_estimates")
  }
)

#' @rdname risk_estimates
setMethod("risk_estimates",
  signature = c("portvine_roll"),
  function(roll, risk_measures = NULL, alpha = NULL,
           df = TRUE, exceeded = FALSE) {
    # evade CMD check note:
    . <- NULL
    # check whether the risk_measures and alpha levels were fitted for this roll
    checkmate::assert_subset(risk_measures, roll@risk_measures, empty.ok = TRUE)
    if (is.null(risk_measures)) risk_measures <- roll@risk_measures
    checkmate::assert_subset(alpha, roll@alpha, empty.ok = TRUE)
    if (is.null(alpha)) alpha <- roll@alpha
    # check the flags
    checkmate::assert_flag(df)
    checkmate::assert_flag(exceeded)

    roll@risk_estimates %>%
      dtplyr::lazy_dt() %>%
      filter(.data$risk_measure %in% risk_measures,
             .data$alpha %in% (!!alpha)) %>%
      {if (exceeded) mutate(., exceeded = .data$realized < .data$risk_est) else .} %>%
      {if (df) as.data.frame(.) else data.table::as.data.table(.)}
  }
)

#'
#' @param cond If set to TRUE returns the conditional risk estimates and
#' otherwise returns the overall risk estimates.
#' @param cond_alpha Numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures were calculated to filter for. Note
#' that they must be fitted in the `roll` argument. The default will return all
#' fitted risk measures.
#'
#'
#' @rdname risk_estimates
setMethod("risk_estimates",
  signature = c("cond_portvine_roll"),
  function(roll, risk_measures = NULL, alpha = NULL,
           df = TRUE, exceeded = FALSE, cond = TRUE,
           cond_alpha = NULL) {
    # evade CMD check note:
    . <- NULL
    # check whether the risk_measures and alpha levels were fitted for this roll
    checkmate::assert_subset(risk_measures, roll@risk_measures, empty.ok = TRUE)
    if (is.null(risk_measures)) risk_measures <- roll@risk_measures
    checkmate::assert_subset(alpha, roll@alpha, empty.ok = TRUE)
    if (is.null(alpha)) alpha <- roll@alpha
    checkmate::assert_subset(cond_alpha, roll@cond_alpha, empty.ok = TRUE)
    if (is.null(cond_alpha)) cond_alpha <- roll@cond_alpha
    # check the flags
    checkmate::assert_flag(df)
    checkmate::assert_flag(exceeded)
    checkmate::assert_flag(cond)

     {if (cond) roll@cond_risk_estimates else roll@risk_estimates} %>%
      dtplyr::lazy_dt() %>%
      filter(.data$risk_measure %in% risk_measures,
             .data$alpha %in% (!!alpha)) %>%
      {if (cond) filter(., .data$cond_alpha %in% (!!cond_alpha)) else .} %>%
      {if (exceeded) mutate(., exceeded = .data$realized < .data$risk_est) else .} %>%
      {if (df) as.data.frame(.) else data.table::as.data.table(.)}
  }
)


### accessor methods for fitted_vines &  fitted_marginals---------------------

#' Accessor method for the fitted vine copula models of `(cond_)portvine_roll`
#'  objects
#'
#' @param roll Object of class `portvine_roll` or a child class
#' @param ... Additional parameters for child class methods
#'
#' @return List of [`rvinecopulib::vinecop`] class objects each entry
#'  corresponds to one fitted vine copula model for the respective vine window.
#' @export
#'
#' @seealso [`portvine_roll-class`]
setGeneric(
  "fitted_vines",
  function(roll, ...) {
    standardGeneric("fitted_vines")
  }
)

#' @rdname fitted_vines
setMethod("fitted_vines",
          signature = c("portvine_roll"),
          function(roll) {
            roll@fitted_vines
          }
)

#' Accessor method for the fitted marginal models of `(cond_)portvine_roll`
#'  objects
#'
#' The marginal models are ARMA-GARCH models which were fitted in a rolling
#' window fashion using [`rugarch::ugarchroll`]. For the residual analysis of
#' the models encompassed in such a [`rugarch::ugarchroll`] class object one
#' can have a look at the utility function [`roll_residuals()`].
#'
#' @param roll Object of class `portvine_roll` or a child class
#' @param ... Additional parameters for child class methods
#'
#' @return Named list with an entry for each asset containing a
#' [`rugarch::ugarchroll`] class object that encompasses the marginal model fit.
#' @export
#'
#' @seealso [`portvine_roll-class`], [`roll_residuals()`]
setGeneric(
  "fitted_marginals",
  function(roll, ...) {
    standardGeneric("fitted_marginals")
  }
)

#' @rdname fitted_marginals
setMethod("fitted_marginals",
          signature = c("portvine_roll"),
          function(roll) {
            roll@fitted_marginals
          }
)
