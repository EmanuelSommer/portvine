# risk measures:
# they just take a numeric vector and output a value for each alpha level,
# note extra inputs for ES_mc (n_mc_samples as  extra argument for main fun)

#' Estimate the Value-at-risk
#'
#' The VaR is defined as the empirical \eqn{\alpha} level quantile of the
#' empirical distribution based on a return sample.
#'
#' @param sample numeric vector representing the sample upon which the value at
#' risk is calculated.
#' @param alpha numeric vector with entries in (0,1) specifying the levels at
#' which the VaR is calculated
#'
#' @return numeric vector with VaR estimates
#'  (same length as `alpha`)
#' @export
#'
#' @seealso [`est_es()`]
#'
#' @examples est_var(0:100, c(0.1, 0.2, 0.3))
est_var <- function(sample, alpha) {
  checkmate::assert_numeric(sample, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_numeric(alpha, any.missing = FALSE, null.ok = FALSE,
                            lower = 0, upper = 1)
  stats::quantile(x = sample, probs = alpha, names = FALSE)
}


#' Estimate the Expected Shortfall (ES)
#'
#' The expected shortfall at level \eqn{\alpha} is defined as the expected value
#' of the returns under the condition that the returns are smaller than the
#' value at risk for the same \eqn{\alpha} level.
#' The three estimation methods are:
#'   - `mean` the mean of the samples that fall under the corresponding VaR
#'   - `median` the median of the samples that fall under the corresponding VaR
#'   - `mc` Calculation of the expected value using Monte Carlo integration over
#'   the \eqn{\alpha} levels. The confidence level is assumed to be uniformly
#'   distributed and `mc_samples` Monte Carlo samples are drawn.
#'
#' @param sample numeric vector representing the sample upon which the expected
#' shortfall is calculated.
#' @param alpha numeric vector with entries in (0,1) specifying the levels at
#' which the ES is calculated
#' @param method method of estimation one of `mean`, `median`, `mc`. For more
#'  information see the Description section.
#' @param mc_samples Number of Monte Carlo samples used for the `mc` method.
#'
#' @return numeric vector with expected shortfall estimates
#'  (same length as `alpha`)
#' @export
#'
#' @seealso [`est_var()`]
#'
#' @examples est_es(0:100, c(0.1, 0.2, 0.3))
est_es <- function(
  sample, alpha,
  method = c("mean", "median", "mc"),
  mc_samples = 100) {
  method <- match.arg(method)
  checkmate::assert_numeric(sample, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_numeric(alpha, any.missing = FALSE, null.ok = FALSE,
                            lower = 0, upper = 1)
  checkmate::assert_count(mc_samples, positive = TRUE)

  if (method %in% c("mean", "median")) {
    value_at_risk <- est_var(sample, alpha)
    sapply(value_at_risk, function(var_est) {
      do.call(method, list(x = sample[sample <= var_est]))
    }, simplify = TRUE)
  } else if (method == "mc") {
    # sample alpha levels and then average over them
    sapply(alpha, function(alp) {
      mean(est_var(sample, runif(mc_samples, max = alp)))
    }, simplify = TRUE)
  }
}


#' Utility to estimate multiple risk measures on the same sample
#'
#' @param risk_measures character vector of valid risk measures (currently
#' `VaR`, `ES_mean`, `ES_median`, `ES_mc` )
#' @param sample numeric sample
#' @param alpha numeric vector with entries in (0,1) specifying the levels at
#' which the ES is calculated
#' @param n_mc_samples Number of Monte Carlo samples used for the `mc` method.
#' @param row_num positive count that notes the row number in an additional
#' column
#'
#' @return data.table with the columns `risk_measure`, `risk_est`, `alpha` and
#' `row_num`
#' @noRd
est_risk_measures <- function(risk_measures, sample, alpha,
                              n_mc_samples, row_num) {
  lapply(
    risk_measures,
    function(risk_measure) {
      if (risk_measure == "VaR") {
        data.table::data.table(
          risk_measure = risk_measure,
          risk_est = est_var(sample, alpha = alpha),
          alpha = alpha,
          row_num = row_num
        )
      } else if (risk_measure %in% c("ES_mean", "ES_median", "ES_mc")) {
        data.table::data.table(
          risk_measure = risk_measure,
          risk_est = est_es(sample, alpha = alpha,
                            method = substring(risk_measure, 4),
                            mc_samples = n_mc_samples),
          alpha = alpha,
          row_num = row_num
        )
      }
    }) %>% data.table::rbindlist()
}
