#import dplyr
estimate_marginal_models <- function(
  data,
  n_all_obs, n_marg_train, n_marg_refit, n_vine_train,
  all_asset_names,
  marginal_specs_list,
  trace
) {
  if (trace) cat("Fit marginal models:\n")
  garch_rolls_list <- sapply(all_asset_names, function(asset_name) {
    if (trace) cat(asset_name," ")
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
    # extract the conditional innovations density
    roll_density <- roll@model$spec@model$modeldesc$distribution
    # extract the fitted residuals for each window
    residuals_per_window <- sapply(
      seq.int(roll@model$n.refits),
      function(window) {
        # extract the coefficients for the window
        coefs <- roll@model$coef[[window]]$coef[, 1]
        spec <- rugarch::ugarchspec(distribution.model = roll_density,
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
        fitted_residuals
      }, simplify = FALSE)
    list(residuals_per_window, roll_density, roll_density_params, roll)
  }, simplify = FALSE, USE.NAMES = TRUE)
  if (trace) cat("\n")
  garch_rolls_list
}


ttt$AAPL@forecast$density %>%
  dtplyr::lazy_dt() %>%
  select(Mu, Sigma, Realized, Shape, Skew) %>%
  mutate(row_num = seq(750 + 1, 1000)) %>%
  data.table::as.data.table() %>%
  dim()

ttt$AAPL@model$n.refits
# coefficients of the first marginal window fitting
ttt$AAPL@model$coef[[1]]$coef[, 1]
# distribution used for fitting
ttt$AAPL@model$spec@model$modeldesc$distribution
rugarch::ugarchspec(distribution.model = "sstd", fixed.pars = ttt$AAPL@model$coef[[1]]$coef[, 1])

tttt <- estimate_marginal_models(
  data_margmod_input,
  1000, 750, 50, 100,
  c("AAPL", "GOOG", "AMZN"),
  list("AAPL" = default_garch_spec(),
       "GOOG" = default_garch_spec(),
       "AMZN" = default_garch_spec()),
  TRUE)
