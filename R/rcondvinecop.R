#' Sample cond from a vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_alpha a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
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
  fitted_vine, vine_type
) {
  stop("NOT yet implemented")
}
