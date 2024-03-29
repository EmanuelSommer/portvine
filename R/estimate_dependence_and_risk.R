#' Estimate the dependence and the ris in a rolling window fashion
#'
#' Internal function for the estimation of the vine copula models for each
#' vine window. In addition based on these models one samples (un-)conditional
#' asset returns and estimates risk measures from these sample returns.
#'
#' The vine windows are parallelized using the future framework i.e. the risk
#'  estimates and the corresponding vine copula models are computed in parallel
#'   for each rolling vine window. Moreover the parallelization can be nested
#'   so one can also parallelize addtitionally the calculations in each time
#'   unit of the vine window. Details can be found in the doc of the
#'  `estimate_risk_roll` function.
#'
#' @param combined_residuals_dt data.table with all the combined data from the
#' marginal window fittings. 10 columns: `resid`, `shape`, `skew`, `mu`,
#'  `sigma`, `row_num`, `marg_window_num`, `asset`, `marg_dist`,
#'  `copula_scale_resid`
#' @param n_all_obs integer specifying the number of all observations
#' @param n_marg_train Positive count specifying the training data size for
#' the ARMA-GARCH models.
#' @param n_marg_refit Positive count specifying size of the forecasting window.
#' @param n_vine_train Positive count specifying the training data size for
#' the vine copula models.
#' @param n_vine_refit Positive count specifying loength of the vine model
#' usage window.
#' @param all_asset_names character vector with all the asset names
#' @param family_set Character vector specifying the family of copulas that are
#' used. For possible choices see [`rvinecopulib::bicop`].
#' @param vine_type character value that specifies which vine class should be
#' fitted. Possible choices right now are `rvine` (regular vine) and `dvine`
#' (drawable vine).
#' @param alpha a numeric vector specifying the levels in (0,1) at which the
#' risk measures should be calculated
#' @param risk_measures a character vector with valid choices for risk
#'  measures to compute
#' @param weights matrix with colnames: asset names, rows one for each vine
#' window with weights, 0 for conditional variables.
#' @param cond_vars colnames of the variables to sample conditionally from
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_u a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
#' @param n_mc_samples number of samples for the Monte Carlo integration
#' if the risk measure `ES_mc` is used. (See [`est_es()`])
#' @param trace if set to TRUE the algorithm will print information while
#'  running.
#' @param cutoff_depth positive count that specifies the depth up to which the
#' edges of the to be constructed D-vine copula are considered in the algorithm
#' that determines the ordering using partial correlations. The default Inf
#' considers all edges and seems in most use cases reasonable.
#' @param prior_resid_strategy Logical flag that indicates whether as the
#'  additionally
#' used conditioning values the prior day residual (if this flag is TRUE) or the
#' realized residuals are used. The default are the realized residuals. Note
#' that the resulting conditional risk measures use realized data so they are
#' only for comparisons as they suffer from information leakage.
#'
#' @return list with 3 entries:
#'   -  `fitted_vines` a list of all fitted vines one element for each vine
#'   window
#'   - `overall_risk_estimates` a data.table with the columns `risk_measure`,
#'   `risk_est`, `alpha`, `row_num` and `vine_window` (here all samples also
#'   in the conditional case are used)
#'   - `cond_risk_estimates` a data.table with the same format like the overall
#'   one but with the additional column(s) containing the conditional values and
#'   the column `cond_u` that indicates the used conditional quantile level.
#'   NULL if the unconditional approach is taken.
#'
#' @import dplyr
#'
#' @include dvine_ordering.R rcondvinecop.R risk_measures.R
#'
#' @noRd
estimate_dependence_and_risk <- function(combined_residuals_dt,
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
                                         cond_u,
                                         n_mc_samples,
                                         trace,
                                         cutoff_depth = Inf,
                                         prior_resid_strategy = FALSE) {
  # very basic input checks as the function is internal
  checkmate::assert_data_table(combined_residuals_dt,
    all.missing = FALSE,
    ncols = 10, col.names = "unique"
  )
  checkmate::assert_subset(colnames(combined_residuals_dt),
    c(
      "resid", "shape", "skew", "mu", "sigma", "row_num",
      "marg_window_num", "asset", "marg_dist",
      "copula_scale_resid"
    ),
    empty.ok = FALSE
  )
  checkmate::assert_subset(all_asset_names, unique(combined_residuals_dt$asset),
    empty.ok = FALSE
  )

  if (trace) cat("\nFit vine copula models and estimate risk.\nVine windows:\n")
  window_results_list <- future.apply::future_lapply(
    seq(ceiling((n_all_obs - n_marg_train) / n_vine_refit)),
    function(vine_window) {
      if (trace) {
        cat("(", vine_window, "/",
          ceiling((n_all_obs - n_marg_train) / n_vine_refit),
          ") ",
          sep = ""
        )
      }
      # filter the corresponding estimated copula data from the respective
      # marginal model
      window_residuals_dt <- combined_residuals_dt %>%
        dtplyr::lazy_dt() %>%
        filter(
          .data$marg_window_num == ceiling(n_vine_refit * vine_window /
            n_marg_refit),
          .data$row_num >= 1 + n_marg_train - n_vine_train +
            n_vine_refit * (vine_window - 1),
          .data$row_num <= n_marg_train + n_vine_refit * (vine_window - 1)
        ) %>%
        data.table::as.data.table()
      # now an appropriate vine copula model has to be fitted
      # in order to do this first determine whether there will be vine structure
      # restrictions
      vine_train_data <- window_residuals_dt %>%
        dtplyr::lazy_dt() %>%
        select("asset", "copula_scale_resid", "row_num") %>%
        tidyr::pivot_wider(
          names_from = "asset",
          values_from = "copula_scale_resid"
        ) %>%
        select(-"row_num") %>%
        as.data.frame()

      if (vine_type == "rvine") {
        vine_struct <- NA
      } else if (vine_type == "dvine") {
        vine_struct <- rvinecopulib::dvine_structure(
          order = dvine_ordering(
            vine_train_data,
            cond_vars,
            cutoff_depth = cutoff_depth
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
      # and estimate the risk measures (iterate over each time unit in the vine
      # window)
      list_risk_est <- future.apply::future_lapply(
        seq(
          n_marg_train + n_vine_refit * (vine_window - 1) + 1,
          min(
            n_all_obs,
            n_marg_train + n_vine_refit * vine_window
          )
        ),
        function(row_num_window) {
          # simulate from the fitted vine
          # get a data.table with n_samples rows or n_samples*length(cond_u)
          # rows, each column is one of the assets
          if (length(cond_vars) == 0) {
            sim_dt <- data.table::as.data.table(
              rvinecopulib::rvinecop(n_samples, fitted_vine)
            )
          } else {
            # named vector containing the copula scale residuals of the prior
            # time unit for all conditioning variables
            prior_resid_strategy_diff <- ifelse(prior_resid_strategy, 1, 0)
            cond_pre_resid <- sapply(cond_vars, function(cond_asset) {
              combined_residuals_dt$copula_scale_resid[
                combined_residuals_dt$asset == cond_asset &
                  combined_residuals_dt$row_num ==
                    row_num_window - prior_resid_strategy_diff &
                  combined_residuals_dt$marg_window_num == ceiling(
                    n_vine_refit * vine_window / n_marg_refit
                  )
              ]
            })
            rcondvinecop_res <- rcondvinecop(
              n_samples = n_samples,
              cond_u = cond_u,
              cond_pre_resid = cond_pre_resid,
              cond_vars = cond_vars,
              fitted_vine = fitted_vine,
              vine_type = vine_type
            )
            cond_u_vec <- rcondvinecop_res$cond_u_vec
            sim_dt <- rcondvinecop_res$sample_dt
          }
          # transform the simulated data on the copula scale to the original
          # scale and then compute the weighted sum of the return in order to
          # get the total portfolio value
          trans_vals <- combined_residuals_dt %>%
            dtplyr::lazy_dt() %>%
            filter(
              .data$marg_window_num == ceiling(n_vine_refit * vine_window /
                n_marg_refit),
              .data$row_num == row_num_window
            ) %>%
            as.data.frame()
          sim_dt <- sim_dt %>%
            dtplyr::lazy_dt() %>%
            mutate(sample_id = seq(nrow(sim_dt))) %>%
            tidyr::pivot_longer(-"sample_id",
              names_to = "asset",
              values_to = "sample"
            ) %>%
            group_by(.data$asset) %>%
            # here transform from copula to original scale
            mutate(
              sample = trans_vals[["mu"]][trans_vals[["asset"]] ==
                .data$asset] +
                trans_vals[["sigma"]][trans_vals[["asset"]] == .data$asset] *
                  rugarch::qdist(
                    distribution = trans_vals[["marg_dist"]][
                      trans_vals[["asset"]] == .data$asset
                    ],
                    p = sample,
                    skew = trans_vals[["skew"]][trans_vals[["asset"]] ==
                      .data$asset],
                    shape = trans_vals[["shape"]][trans_vals[["asset"]] ==
                      .data$asset]
                  )
            ) %>%
            # add the corresponding weight
            mutate(weight = weights[vine_window, .data$asset]) %>%
            ungroup() %>%
            group_by(.data$sample_id) %>%
            # get the portfolio value by a weighted sum
            mutate(portfolio_value = sum(.data$sample * .data$weight)) %>%
            ungroup() %>%
            select(-"weight") %>%
            tidyr::pivot_wider(
              names_from = "asset",
              values_from = "sample"
            ) %>%
            arrange(.data$sample_id) %>%
            # retrieve the portfolio value as well as the conditioning vars if
            # the conditional approach is taken
            select("portfolio_value", all_of(cond_vars)) %>%
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
            cond_name <- colnames(sim_dt[, -1])
            sim_dt <- cbind(sim_dt, cond_u_vec)
            # estimate the risk for each conditional quantile
            cond_risk_estimates <- lapply(
              c(cond_u, "prior_resid"),
              function(cond_level) {
                cond_level_char <- as.character(ifelse(
                  cond_level == "prior_resid" & (!prior_resid_strategy),
                  "resid",
                  cond_level
                ))
                cond_val <- sim_dt[[2]][sim_dt[["cond_u_vec"]] ==
                  cond_level][1]
                est_risk_measures(
                  risk_measures = risk_measures,
                  sample = sim_dt$portfolio_value[sim_dt[["cond_u_vec"]] ==
                    cond_level],
                  alpha = alpha,
                  n_mc_samples = n_mc_samples,
                  row_num = row_num_window
                ) %>%
                  dtplyr::lazy_dt() %>%
                  mutate(
                    !!cond_name := cond_val,
                    "cond_u" = cond_level_char
                  ) %>%
                  data.table::as.data.table()
              }
            ) %>% data.table::rbindlist()
          } else if (length(cond_vars) == 2) {
            cond_names <- colnames(sim_dt[, -1])
            sim_dt <- cbind(sim_dt, cond_u_vec)
            # estimate the risk for each conditional quantile
            cond_risk_estimates <- lapply(
              c(cond_u, "prior_resid"),
              function(cond_level) {
                cond_level_char <- as.character(ifelse(
                  cond_level == "prior_resid" & (!prior_resid_strategy),
                  "resid",
                  cond_level
                ))
                cond_val1 <- sim_dt[[2]][sim_dt[["cond_u_vec"]] ==
                  cond_level][1]
                cond_val2 <- sim_dt[[3]][sim_dt[["cond_u_vec"]] ==
                  cond_level][1]
                est_risk_measures(
                  risk_measures = risk_measures,
                  sample = sim_dt$portfolio_value[sim_dt[["cond_u_vec"]] ==
                    cond_level],
                  alpha = alpha,
                  n_mc_samples = n_mc_samples,
                  row_num = row_num_window
                ) %>%
                  dtplyr::lazy_dt() %>%
                  mutate(
                    !!cond_names[1] := cond_val1,
                    !!cond_names[2] := cond_val2,
                    "cond_u" = cond_level_char
                  ) %>%
                  data.table::as.data.table()
              }
            ) %>% data.table::rbindlist()
          }
          list(
            overall_risk_estimates = overall_risk_estimates,
            cond_risk_estimates = cond_risk_estimates
          )
        },
        future.seed = TRUE
      )
      # collect the observation level results in two data.tables
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
      list(
        overall_risk_estimates = overall_risk_estimates,
        cond_risk_estimates = cond_risk_estimates,
        fitted_vine = fitted_vine, vine_window = vine_window
      )
    },
    future.seed = TRUE
  )
  # collect the windowwise results again in two data.tables
  overall_risk_estimates <- lapply(
    window_results_list,
    function(vine_window_entry) {
      cbind(
        vine_window_entry[["overall_risk_estimates"]],
        "vine_window" = vine_window_entry[["vine_window"]]
      )
    }
  ) %>% data.table::rbindlist()

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
      }
    ) %>% data.table::rbindlist()
  }
  # extract also the fitted vines in one list
  fitted_vines <- lapply(window_results_list, function(vine_window_entry) {
    vine_window_entry[["fitted_vine"]]
  })

  if (trace) cat("\n")
  list(
    fitted_vines = fitted_vines,
    overall_risk_estimates = overall_risk_estimates,
    cond_risk_estimates = cond_risk_estimates
  )
}
