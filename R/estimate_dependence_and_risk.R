estimate_dependence_and_risk <- function(
  combined_residuals_dt,
  n_all_obs, n_marg_train, n_marg_refit, n_vine_train, n_vine_refit,
  all_asset_names,
  vine_specs,
  alpha,
  risk_measures,
  weights,
  conditional_logical,
  cond_vars,
  n_samples,
  n_cond_samples,
  trace
) {
  lapply(
    seq(ceiling((n_all_obs - n_marg_train) / n_vine_refit)),
    function(vine_window) {
      # filter the corresponding estimated copula data from the respective
      # marginal model
      window_residuals_dt <- combined_residuals_dt %>%
        dtplyr::lazy_dt() %>%
        filter(
          marg_window_num == ceiling(n_vine_refit * vine_window / n_marg_refit),
          row_num >= 1 + n_marg_train - n_vine_train +
            n_vine_refit * (vine_window - 1),
          row_num <= n_marg_train + n_vine_refit * (vine_window - 1)
        ) %>%
        data.table::as.data.table()
      # now an appropriate vine copula model has to be fitted
      # in order to do this first determine whether there will be vine structure
      # restrictions
      vine_train_data <- window_residuals_dt %>%
        dtplyr::lazy_dt() %>%
        select(asset, copula_scale_resid, row_num) %>%
        tidyr::pivot_wider(names_from = asset, values_from = copula_scale_resid) %>%
        select(-row_num) %>%
        as.data.frame()

      if (vine_type == "rvine") {
        vine_struct <- NA
      } else if (vine_type == "dvine") {
        vine_struct <- rvinecopulib::dvine_structure(
          order = greedy_tau_ordering(
            vine_train_data,
            cond_vars
          )
        )
      }
      fitted_vine <- rvinecopulib::vinecop(
        data = vine_train_data,
        structure = vine_struct,
        family_set = family_set,
        presel = FALSE,
      )
      # now loop over the days k of the forecasted vine window
        # simulate for the d assets returns
        # transform them back to log return scale
        # weighted portfolio
        # estimate risk measures
      # data structures must accomodate also extra columns for conditional values
      # for further analysis
  })
}






















