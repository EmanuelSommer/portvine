#' Estimate the marginals models in a rolling window fashion
#'
#' Internal function for the estimation of the marginal models for each asset
#' and each marginal window. In addition based on these models the standardized
#' residuals and corresponding copula data are calculated for the respective
#'  windows.
#'
#' The marginal model fitting is parallelized using the future framework i.e.
#'  all assets individually in parallel. Details can be found in the doc of the
#'  `estimate_risk_roll` function.
#'
#' @param data data.table with the three columns `row_num`, `asset` and `returns.`
#'  `row_num` specifies the row number for the wide format input data, `asset`
#'  is the unique name of the asset and `returns` gives the numeric returns.
#' @param n_all_obs integer specifying the number of all observations
#' @param n_marg_train Positive count specifying the training data size for
#' the ARMA-GARCH models.
#' @param n_marg_refit Positive count specifying size of the forecasting window.
#' @param n_vine_train Positive count specifying the training data size for
#' the vine copula models.
#' @param all_asset_names character vector with all the asset names
#' @param marginal_specs_list named list containing the specification for the
#' marginal ARMA-GARCH model for each asset.
#' @param trace if set to TRUE the algorithm will print information while
#'  running.
#'
#' @return named list with entry (list again) for each asset with two elements:
#'  `roll_model_fit` a [`rugarch::ugarchroll`] object (marginal models) and
#'  `residuals_dt` a datatable containing the standardized residuals `resid`,
#'  copula scale residuals `copula_scale_resid`, for the corresponding marginal
#'  window `marg_window_num`, row number `row_num`, asset `asset` and the most
#'  important marginal model information `shape`, `skew` and `marg_dist`.
#'
#' @import dplyr
#' @importFrom stats resid
#' @noRd
estimate_marginal_models <- function(
  data,
  n_all_obs, n_marg_train, n_marg_refit, n_vine_train,
  all_asset_names,
  marginal_specs_list,
  trace
) {
  # very basic input checks as the function is internal
  checkmate::assert_data_table(data, any.missing = FALSE,
                               ncols = 3, col.names = "unique")
  checkmate::assert_subset(colnames(data), c("asset", "returns", "row_num"),
                           empty.ok = FALSE)
  checkmate::assert_subset(all_asset_names, unique(data$asset),empty.ok = FALSE)
  checkmate::assert_list(marginal_specs_list, types = "uGARCHspec",
                         len = length(all_asset_names), any.missing = FALSE,
                         names = "unique")

  if (trace) cat("\nFit marginal models:\n")
  garch_rolls_list <- future.apply::future_sapply(all_asset_names,
                                                  function(asset_name) {
    if (trace) cat(asset_name, " ")
    # fit the model in a rolling window fashion
    roll <- rugarch::ugarchroll(
      spec = marginal_specs_list[[asset_name]],
      data = data[data$asset == asset_name, ]$returns,
      forecast.length = n_all_obs - n_marg_train,
      refit.every = n_marg_refit, refit.window = "moving",
      keep.coef = TRUE, solver = "hybrid"
    )
    # extract the density parameters for the window [n_marg_train+1, n_all_obs]
    roll_density_params <- roll@forecast$density %>%
      dtplyr::lazy_dt() %>%
      select("Mu", "Sigma", "Realized", "Shape", "Skew") %>%
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
          mu = NA_real_,
          sigma = NA_real_,
          row_num = seq.int(start_window, start_window + n_vine_train - 1)
        )
        # now get the forecasted residuals and append the fitted residuals
        roll_density_params %>%
          dtplyr::lazy_dt() %>%
          filter(row_num <= end_window & row_num >= (start_window + n_vine_train)) %>%
          arrange(row_num) %>%
          mutate(resid = (Realized - Mu) / Sigma) %>%
          select(resid, shape = Shape, skew = Skew, mu = Mu, sigma = Sigma,
                 row_num) %>%
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
  }, simplify = FALSE, USE.NAMES = TRUE, future.seed = TRUE)
  if (trace) cat("\n")
  garch_rolls_list
}





