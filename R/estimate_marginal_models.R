#' Title
#'
#' @param data
#' @param n_all_obs
#' @param n_marg_train
#' @param n_marg_refit
#' @param n_vine_train
#' @param all_asset_names
#' @param marginal_specs_list
#' @param trace
#'
#' @return
#' @export
#'
#' @import dplyr
#'
#' @examples
estimate_marginal_models <- function(
  data,
  n_all_obs, n_marg_train, n_marg_refit, n_vine_train,
  all_asset_names,
  marginal_specs_list,
  trace
) {
  if (trace) cat("Fit marginal models:\n")
  garch_rolls_list <- sapply(all_asset_names, function(asset_name) {
    if (trace) cat(asset_name, " (", which(asset_name == all_asset_names),
                   "/", length(all_asset_names), ") ", sep = "")
    # fit the model in a rolling window fashion
    roll <- rugarch::ugarchroll(
      spec = marginal_specs_list[[asset_name]],
      data = data[asset == asset_name]$returns,
      forecast.length = n_all_obs - n_marg_train,
      refit.every = n_marg_refit, refit.window = "moving",
      keep.coef = TRUE, solver = "hybrid"
    )
    # extract the density parameters for the window [n_marg_train+1, n_all_obs]
    roll_density_params <- roll@forecast$density %>%
      dtplyr::lazy_dt() %>%
      select(Mu, Sigma, Realized, Shape, Skew) %>%
      mutate(row_num = seq(n_marg_train + 1, n_all_obs)) %>%
      data.table::as.data.table()
    # extract the conditional innovations distribution
    roll_distribution <- roll@model$spec@model$modeldesc$distribution
    # get the residuals for each window and store all relevant residuals
    # for the current asset in one data.table whose columns are specified below
    residuals_dt <- sapply(
      seq.int(roll@model$n.refits),
      function(window) {
        # first get the fitted residuals
        # extract the coefficients for the window
        coefs <- roll@model$coef[[window]]$coef[, 1]
        spec <- rugarch::ugarchspec(distribution.model = roll_distribution,
                                    fixed.pars = coefs)
        start_window <- 1 + n_marg_train - n_vine_train +
          (window - 1) * n_marg_refit
        end_window <- min(n_all_obs, n_marg_train + n_marg_refit * window)
        window_returns <- data[asset == asset_name]$returns[seq(start_window,
                                                                end_window)]
        filtered_model <- rugarch::ugarchfilter(
          spec = spec,
          data = window_returns,
          out.sample = end_window - start_window + 1 - n_vine_train
        )
        fitted_residuals <- as.numeric(
          rugarch::residuals(filtered_model, standardize = TRUE))
        window_residuals_fitted <- data.table::data.table(
          resid = fitted_residuals,
          shape = coefs["shape"],
          skew = coefs["skew"],
          row_num = seq.int(start_window, start_window + n_vine_train - 1)
        )
        # now get the forecasted residuals and append the fitted residuals
        roll_density_params %>%
          dtplyr::lazy_dt() %>%
          filter(row_num <= end_window & row_num >= (start_window + n_vine_train)) %>%
          arrange(row_num) %>%
          mutate(resid = (Realized - Mu) / Sigma) %>%
          select(resid, shape = Shape, skew = Skew, row_num) %>%
          data.table::as.data.table() %>%
          rbind(window_residuals_fitted) %>%
          dtplyr::lazy_dt() %>%
          arrange(row_num) %>%
          # now one has all needed standardized residuals for this window.
          # Thus the transformed copula scale residuals are still appended using
          # the skew and shape parameters as well as the marginal distribution
          # as well as the asset name, the index of the marginal window and
          # the used marginal distribution used for the fitting.
          mutate(
            marg_window_num = window,
            asset = asset_name,
            marg_dist = roll_distribution,
            copula_scale_resid = rugarch::pdist(
              distribution = roll_distribution,
              q = resid, shape = shape, skew = skew)) %>%
          data.table::as.data.table()
      }, simplify = FALSE) %>%
      # bind the per marginal window data.tables together (possible as the
      # column marg_window_num ensures identifyabilty)
      data.table::rbindlist()
    list(residuals_dt = residuals_dt, roll_model_fit = roll)
  }, simplify = FALSE, USE.NAMES = TRUE)
  if (trace) cat("\n")
  garch_rolls_list
}


# TBD:
# Documentation
# Tests (extract from sample returns a super small example data set (copy and add some noise then add to package))



