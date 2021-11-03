# risk measures:
# they just take a numeric vector and output a value for each alpha level,
# note extra inputs for ES_mc (n_mc_samples as  extra argument for main fun)

#' Estimate the Value-at-risk
#'
#' The VaR is defined as the empirical alpha level quantile of the
#' empirical distribution based on a return sample.
#'
#' @param sample numeric vector representing the sample upon which the value at
#' risk is calculated.
#' @param alpha numeric vector with entries in (0,1) specifying the levels at
#' which the VaR is calculated
#'
#' @return numeric vector with VaR estimates
#' @export
#'
#' @examples est_var(0:100, c(0.1,0.2,0.3))
est_var <- function(sample, alpha) {
  checkmate::assert_numeric(sample, any.missing = FALSE, null.ok = FALSE)
  checkmate::assert_numeric(alpha, any.missing = FALSE, null.ok = FALSE,
                            lower = 0, upper = 1)
  stats::quantile(x = sample, probs = alpha, names = FALSE)
}


#' Title
#'
#' @param sample
#' @param alpha
#' @param method
#' @param mc_samples
#'
#' @return
#' @export
#'
#' @examples est_es(0:100, c(0.1,0.2,0.3))
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



