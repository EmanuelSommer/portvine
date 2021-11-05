# include greedy tau and risk measures

#' Title
#'
#' @param combined_residuals_dt
#' @param n_all_obs
#' @param n_marg_train
#' @param n_marg_refit
#' @param n_vine_train
#' @param n_vine_refit
#' @param all_asset_names
#' @param family_set
#' @param vine_type
#' @param alpha
#' @param risk_measures
#' @param weights
#' @param cond_vars
#' @param n_samples
#' @param n_cond_samples
#' @param n_mc_samples
#' @param trace
#'
#' @return
#'
#' @noRd
estimate_dependence_and_risk <- function(
  combined_residuals_dt,
  n_all_obs,
  n_marg_train, n_marg_refit,
  n_vine_train, n_vine_refit,
  all_asset_names,
  family_set, vine_type,
  alpha,
  risk_measures,
  weights,
  cond_vars,
  n_samples,
  n_cond_samples,
  n_mc_samples,
  trace
) {
  if (trace) cat("\nFit & simulate from vine copula models. Vine window:\n")
  window_results_list <- lapply(
    seq(ceiling((n_all_obs - n_marg_train) / n_vine_refit)),
    function(vine_window) {
      if (trace) cat("(", vine_window, "/",
                     ceiling((n_all_obs - n_marg_train) / n_vine_refit),
                     ") ", sep = "")
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
        tidyr::pivot_wider(names_from = asset,
                           values_from = copula_scale_resid) %>%
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
      # simulate, transform back to original scale, get full portfolio value
      # and estimate the risk measures
      list_risk_est <- lapply(
        seq(n_marg_train + n_vine_refit * (vine_window - 1) + 1,
            min(n_all_obs,
                n_marg_train + n_vine_refit * vine_window)),
        function(row_num_window) {
          # simulate from the fitted vine
          # get a data.table with n_samples rows or n_samples*n_cond_samples
          # rows, each column is one of the assets
          if (length(cond_vars) == 0) {
            sim_dt <- data.table::as.data.table(
              rvinecopulib::rvinecop(n_samples, fitted_vine)
            )
          } else {
            sim_dt <- rcondvinecop(n_samples, n_cond_samples,
                                  fitted_vine, vine_type)
          }
          # transform the simulated data on the copula scale to the original
          # scale and then compute the weighted sum of the return in order to
          # get the total portfolio value
          trans_vals <- combined_residuals_dt %>%
            dtplyr::lazy_dt() %>%
            filter(
              marg_window_num == ceiling(n_vine_refit * vine_window /
                                           n_marg_refit),
              row_num == row_num_window) %>%
            as.data.frame()
          sim_dt <- sim_dt %>%
            dtplyr::lazy_dt() %>%
            mutate(sample_id = seq(nrow(sim_dt))) %>%
            tidyr::pivot_longer(-sample_id, names_to = "asset",
                         values_to = "sample") %>%
            group_by(asset) %>%
            # here transform from copula to original scale
            mutate(
              sample = trans_vals[["mu"]][trans_vals[["asset"]] == asset] +
                trans_vals[["sigma"]][trans_vals[["asset"]] == asset] *
                rugarch::qdist(
                  distribution = trans_vals[["marg_dist"]][trans_vals[["asset"]]
                                                           == asset],
                  p = sample,
                  skew = trans_vals[["skew"]][trans_vals[["asset"]] == asset]
                )
            ) %>%
            # add the corresponding weight
            mutate(weight = weights[asset]) %>%
            ungroup() %>%
            group_by(sample_id) %>%
            # get the portfolio value by a weighted sum
            mutate(portfolio_value = sum(sample * weight)) %>%
            ungroup() %>%
            select(-weight) %>%
            tidyr::pivot_wider(names_from = asset, values_from = sample) %>%
            arrange(sample_id) %>%
            # retrieve the portfolio value as well as the conditioning vars if
            # the conditional approach is taken
            select(portfolio_value, all_of(cond_vars)) %>%
            data.table::as.data.table()

          # overall risk measures (all samples used)
          overall_risk_estimates <- est_risk_measures(
            risk_measures = risk_measures,
            sample = sim_dt$portfolio_value,
            alpha = alpha,
            n_mc_samples = n_mc_samples,
            row_num = row_num_window
          )

          # conditional risk estimates
          if (length(cond_vars) == 0) {
            cond_risk_estimates <- NULL
          } else if (length(cond_vars) == 1) {
            unique_cond <- unique(sim_dt[, -1])
            cond_risk_estimates <- lapply(unique_cond[[1]], function(cond_val) {
              names(cond_val) <- colnames(unique_cond)
              cbind(
                est_risk_measures(
                  risk_measures = risk_measures,
                  sample = sim_dt$portfolio_value[sim_dt[[2]] == cond_val],
                  alpha = alpha,
                  n_mc_samples = n_mc_samples,
                  row_num = row_num_window
                ),
                cond_val
              )
            }) %>% data.table::rbindlist()
          } else if (length(cond_vars) == 2) {
            unique_cond <- unique(sim_dt[, -1])
            cond_vals1 <- unique_cond[[1]]
            cond_vals2 <- unique_cond[[2]]
            cond_risk_estimates <- lapply(seq(length(cond_vals1)),
                                          function(i) {
              cond_val1 <- cond_vals1[i]
              cond_val2 <- cond_vals2[i]
              names(cond_val1) <- colnames(unique_cond)[1]
              names(cond_val2) <- colnames(unique_cond)[2]
              cbind(
                est_risk_measures(
                  risk_measures = risk_measures,
                  sample = sim_dt$portfolio_value[sim_dt[[2]] == cond_val1 &
                                                    sim_dt[[3]] == cond_val2],
                  alpha = alpha,
                  n_mc_samples = n_mc_samples,
                  row_num = row_num_window
                ),
                cond_val1,
                cond_val2
              )
            }) %>% data.table::rbindlist()
          }
          list(overall_risk_estimates = overall_risk_estimates,
               cond_risk_estimates = cond_risk_estimates)
      })
      # collect the results in two data.tables
      overall_risk_estimates <- lapply(list_risk_est, function(row_num_entry) {
        row_num_entry[["overall_risk_estimates"]]
      }) %>% data.table::rbindlist()

      if (length(cond_vars) == 0) {
        cond_risk_estimates <- NULL
      } else {
        cond_risk_estimates <- lapply(list_risk_est, function(row_num_entry) {
          row_num_entry[["cond_risk_estimates"]]
        }) %>% data.table::rbindlist()
      }
      list(overall_risk_estimates = overall_risk_estimates,
           cond_risk_estimates = cond_risk_estimates,
           fitted_vine = fitted_vine, vine_window = vine_window)
  })
  # collect the results again in two data.tables
  overall_risk_estimates <- lapply(
    window_results_list,
    function(vine_window_entry) {
      cbind(
        vine_window_entry[["overall_risk_estimates"]],
        "vine_window" = vine_window_entry[["vine_window"]]
      )
  }) %>% data.table::rbindlist()

  if (length(cond_vars) == 0) {
    cond_risk_estimates <- NULL
  } else {
    cond_risk_estimates <- lapply(
      window_results_list,
      function(vine_window_entry) {
        cbind(
          vine_window_entry[["cond_risk_estimates"]],
          "vine_window" = vine_window_entry[["vine_window"]]
        )
      }) %>% data.table::rbindlist()
  }
  # extract also the fitted vines in one list
  fitted_vines <- lapply(window_results_list, function(vine_window_entry) {
    vine_window_entry[["fitted_vine"]]
  })

  if (trace) cat("\n")
  list(fitted_vines = fitted_vines,
       overall_risk_estimates = overall_risk_estimates,
       cond_risk_estimates = cond_risk_estimates)
}



# t1 <- estimate_dependence_and_risk(
#   combined_residuals_dt,
#   1000,
#   750, 50,
#   100, 25,
#   unique(combined_residuals_dt$asset),
#   "all", "rvine",
#   c(0.1, 0.2),
#   c("ES_median", "VaR"),
#   c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
#   NULL,
#   1000,
#   100,
#   100,
#   TRUE
# )
