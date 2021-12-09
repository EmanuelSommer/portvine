### First the functions that do the actual sampling and then the wrapper
### function rcondvinecop that enables the call of the correct sampling function

#' Sample conditioned on the rightmost edge variable from a D-vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_alpha a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
#' @param fitted_vine object of class  [`rvinecopulib::vinecop`] containing all
#' the information about the fitted vine copula
#'
#' @return list with 2 entries:
#' - `sample_dt` data.table with `n_samples` times `length(cond_alpha)` rows
#'  each corresponding to one multivariate sample, each column is one of the
#' assets (named)
#' - `cond_alpha_vec` dimensions s.t. it would fit as an extra column to
#'  `sample_dt` indicating the level of `cond_alpha` used.
#'
#' @noRd
r1conddvine <- function(n_samples, cond_alpha, fitted_vine){
  asset_names <- fitted_vine$names
  n_assets <- length(asset_names)
  # for each cond_alpha level get the n_samples samples
  sample_dt <- lapply(cond_alpha, function(cond_alpha_level) {
    # call lower level cpp function for the actual sampling i.e. speed up
    # memory consumption down :)
    cond_dvine1_cpp(
      n_samples = n_samples,
      cond_alpha = cond_alpha_level,
      vinecop_r = fitted_vine
    ) %>%
      data.table::as.data.table()
  }) %>% data.table::rbindlist()

  # reorder the columns in the original order
  reorder_indices <- sapply(seq(asset_names), function(ind) {
    which(fitted_vine$structure$order == ind)}, simplify = TRUE)
  data.table::setcolorder(sample_dt, reorder_indices)
  colnames(sample_dt) <- asset_names

  list(sample_dt = sample_dt,
       cond_alpha_vec = sample_dt[[fitted_vine$structure$order[1]]])
}



#' Sample conditioned on the two rightmost edge variables from a D-vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_alpha a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
#' @param fitted_vine object of class  [`rvinecopulib::vinecop`] containing all
#' the information about the fitted vine copula
#'
#' @return list with 2 entries:
#' - `sample_dt` data.table with `n_samples` times `length(cond_alpha)` rows
#'  each corresponding to one multivariate sample, each column is one of the
#' assets (named)
#' - `cond_alpha_vec` dimensions s.t. it would fit as an extra column to
#'  `sample_dt` indicating the level of `cond_alpha` used.
#'
#' @noRd
r2conddvine <- function(n_samples, cond_alpha, fitted_vine) {
  asset_names <- fitted_vine$names
  n_assets <- length(asset_names)

  # for each cond_alpha level get the n_samples samples
  sample_dt <- lapply(cond_alpha, function(cond_alpha_level) {
    # call lower level cpp function for the actual sampling i.e. speed up
    # memory consumption down :)
    cond_dvine2_cpp(
      n_samples = n_samples,
      cond_alpha = cond_alpha_level,
      vinecop_r = fitted_vine
    ) %>%
      data.table::as.data.table()
  }) %>% data.table::rbindlist()

  # reorder the columns in the original order
  reorder_indices <- sapply(seq(asset_names), function(ind) {
    which(fitted_vine$structure$order == ind)}, simplify = TRUE)
  data.table::setcolorder(sample_dt, reorder_indices)
  colnames(sample_dt) <- asset_names

  list(sample_dt = sample_dt,
       cond_alpha_vec = sample_dt[[fitted_vine$structure$order[2]]])
}

#' Sample conditionally from a vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_alpha a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
#' @param cond_vars character vector with the conditioning variables
#' @param fitted_vine object of class  [`rvinecopulib::vinecop`] containing all
#' the information about the fitted vine copula
#' @param vine_type character vector specifing the vine copula type fitted. One
#' of `rvine` and `dvine` currently.
#'
#' @return list with 2 entries:
#' - `sample_dt` data.table with `n_samples` times `length(cond_alpha)` rows
#'  each corresponding to one multivariate sample, each column is one of the
#' assets (named)
#' - `cond_alpha_vec` dimensions s.t. it would fit as an extra column to
#'  `sample_dt` indicating the level of `cond_alpha` used.
#'
#' @noRd
rcondvinecop <- function(
  n_samples, cond_alpha,
  cond_vars,
  fitted_vine, vine_type
) {
  checkmate::assert_class(fitted_vine, "vinecop")
  if (vine_type == "dvine") {
    if (length(cond_vars) == 1) {
      r1conddvine(n_samples, cond_alpha, fitted_vine)
    } else if ( length(cond_vars) == 2) {
      r2conddvine(n_samples, cond_alpha, fitted_vine)
    } else {
      stop("Only the sampling of dvines with up to 2 conditioning variables
           is implemented yet.")
    }
  } else {
    stop("This vine type is not implemented for conditional sampling.")
  }
}
