# create some valid inputs for testing
vine_input_sample_returns_small <- lazy_dt(sample_returns_small) %>%
  mutate(row_num = seq.int(nrow(sample_returns_small))) %>%
  pivot_longer(-row_num, names_to = "asset", values_to = "returns") %>%
  data.table::as.data.table()

test_basic_marginal_models_est_forvines <- estimate_marginal_models(
  vine_input_sample_returns_small,
  n_all_obs = 1000, n_marg_train = 750,
  n_marg_refit = 50, n_vine_train = 100,
  all_asset_names = unique(vine_input_sample_returns_small$asset),
  marginal_specs_list = list(
    "AAPL" = default_garch_spec(),
    "GOOG" = default_garch_spec(),
    "AMZN" = default_garch_spec()
  ),
  trace = FALSE
)

test_combined_residuals_dt <- data.table::rbindlist(
  lapply(test_basic_marginal_models_est_forvines,
         function(asset) asset$residuals_dt)
)

estimate_dependence_and_risk(
  test_combined_residuals_dt,
  1000,
  750, 50,
  100, 25,
  unique(test_combined_residuals_dt$asset),
  "all", "rvine",
  c(0.1, 0.2),
  c("ES_median", "VaR"),
  c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
  NULL,
  1000,
  0.05,
  100,
  FALSE
)

test_that("unconditional case", {
  # rvine
  expect_true(
    is.null(
      estimate_dependence_and_risk(
        test_combined_residuals_dt,
        1000,
        750, 50,
        100, 25,
        unique(test_combined_residuals_dt$asset),
        "all", "rvine",
        c(0.1, 0.2),
        c("ES_median", "VaR"),
        c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
        NULL,
        1000,
        0.05,
        100,
        FALSE
      )[["cond_risk_estimates"]]
    )
  )
  expect_s3_class(
    estimate_dependence_and_risk(
      test_combined_residuals_dt,
      1000,
      750, 50,
      100, 25,
      unique(test_combined_residuals_dt$asset),
      "all", "rvine",
      c(0.1, 0.2),
      c("ES_median", "VaR"),
      c("GOOG" = 2, "AAPL" = 30, "AMZN" = 5),
      NULL,
      1000,
      0.05,
      100,
      FALSE
    )[["overall_risk_estimates"]],
    "data.table"
  )
  expect_equal(
    colnames(estimate_dependence_and_risk(
      test_combined_residuals_dt,
      1000,
      750, 50,
      100, 25,
      unique(test_combined_residuals_dt$asset),
      "all", "rvine",
      c(0.1, 0.2),
      c("ES_median", "VaR"),
      c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
      NULL,
      1000,
      0.05,
      100,
      FALSE
    )[["overall_risk_estimates"]]),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window")
  )
  expect_true(
    checkmate::test_list(
      estimate_dependence_and_risk(
        test_combined_residuals_dt,
        1000,
        750, 50,
        100, 25,
        unique(test_combined_residuals_dt$asset),
        "all", "rvine",
        c(0.1, 0.2),
        c("ES_median", "VaR"),
        c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
        NULL,
        1000,
        0.05,
        100,
        FALSE
      )[["fitted_vines"]],
      types = "vinecop"
    )
  )
  # dvine
  expect_true(
    is.null(
      estimate_dependence_and_risk(
        test_combined_residuals_dt,
        1000,
        750, 50,
        100, 25,
        unique(test_combined_residuals_dt$asset),
        "all", "dvine",
        c(0.1, 0.2),
        c("ES_median", "VaR"),
        c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
        NULL,
        1000,
        0.05,
        100,
        FALSE
      )[["cond_risk_estimates"]]
    )
  )
  expect_s3_class(
    estimate_dependence_and_risk(
      test_combined_residuals_dt,
      1000,
      750, 50,
      100, 25,
      unique(test_combined_residuals_dt$asset),
      "all", "dvine",
      c(0.1, 0.2),
      c("ES_median", "VaR"),
      c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
      NULL,
      1000,
      0.05,
      100,
      FALSE
    )[["overall_risk_estimates"]],
    "data.table"
  )
  expect_equal(
    colnames(estimate_dependence_and_risk(
      test_combined_residuals_dt,
      1000,
      750, 50,
      100, 25,
      unique(test_combined_residuals_dt$asset),
      "all", "dvine",
      c(0.1, 0.2),
      c("ES_median", "VaR"),
      c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
      NULL,
      1000,
      0.05,
      100,
      FALSE
    )[["overall_risk_estimates"]]),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window")
  )
  expect_true(
    checkmate::test_list(
      estimate_dependence_and_risk(
        test_combined_residuals_dt,
        1000,
        750, 50,
        100, 25,
        unique(test_combined_residuals_dt$asset),
        "all", "dvine",
        c(0.1, 0.2),
        c("ES_median", "VaR"),
        c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
        NULL,
        1000,
        0.05,
        100,
        FALSE
      )[["fitted_vines"]],
      types = "vinecop"
    )
  )
  # different family_set
  expect_true(
    checkmate::test_list(
      estimate_dependence_and_risk(
        test_combined_residuals_dt,
        1000,
        750, 50,
        100, 25,
        unique(test_combined_residuals_dt$asset),
        c("clayton", "gumbel", "frank"), "dvine",
        c(0.1, 0.2),
        c("ES_median", "VaR"),
        c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
        NULL,
        1000,
        0.05,
        100,
        FALSE
      )[["fitted_vines"]],
      types = "vinecop"
    )
  )
  # other alphas and risk measure
  expect_equal(
    colnames(estimate_dependence_and_risk(
      test_combined_residuals_dt,
      1000,
      750, 50,
      100, 25,
      unique(test_combined_residuals_dt$asset),
      "all", "rvine",
      0.01,
      "ES_mc",
      c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
      NULL,
      1000,
      0.05,
      100,
      FALSE
    )[["overall_risk_estimates"]]),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window")
  )
})

test_that("conditional case", {
  # dvine single conditional
  dep_risk_test_result_cond1 <- estimate_dependence_and_risk(
    combined_residuals_dt = test_combined_residuals_dt,
    n_all_obs = 1000, n_marg_train = 750,
    n_marg_refit = 50, n_vine_train = 100, n_vine_refit = 25,
    all_asset_names = unique(test_combined_residuals_dt$asset),
    family_set = "all", vine_type = "dvine",
    alpha = c(0.1, 0.2),
    risk_measures = c("ES_median", "VaR"),
    weights = c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
    cond_vars = "AAPL",
    n_samples = 10,
    cond_alpha = c(0.05, 0.5),
    n_mc_samples = 100,
    trace = FALSE
  )
  expect_true(
    checkmate::test_data_table(
      dep_risk_test_result_cond1$overall_risk_estimates,
      any.missing = FALSE,
      ncols = 5, col.names = "unique"
    )
  )
  expect_true(
    checkmate::test_data_table(
      dep_risk_test_result_cond1$cond_risk_estimates,
      any.missing = FALSE,
      ncols = 7, col.names = "unique"
    )
  )
  expect_equal(
    colnames(dep_risk_test_result_cond1$overall_risk_estimates),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window")
  )
  expect_equal(
    colnames(dep_risk_test_result_cond1$cond_risk_estimates),
    c("risk_measure", "risk_est", "alpha", "row_num", "AAPL",
      "cond_alpha", "vine_window")
  )
  expect_true(
    checkmate::test_list(
      dep_risk_test_result_cond1[["fitted_vines"]],
      types = "vinecop"
    )
  )
  # dvine two conditional
  dep_risk_test_result_cond2 <- estimate_dependence_and_risk(
    combined_residuals_dt = test_combined_residuals_dt,
    n_all_obs = 1000, n_marg_train = 750,
    n_marg_refit = 50, n_vine_train = 100, n_vine_refit = 25,
    all_asset_names = unique(test_combined_residuals_dt$asset),
    family_set = "all", vine_type = "dvine",
    alpha = c(0.1, 0.2),
    risk_measures = c("ES_mean", "VaR"),
    weights = c("GOOG" = 2, "AAPL" = 3, "AMZN" = 5),
    cond_vars = c("AAPL", "GOOG"),
    n_samples = 10,
    cond_alpha = c(0.05, 0.5),
    n_mc_samples = 100,
    trace = FALSE
  )
  expect_true(
    checkmate::test_data_table(
      dep_risk_test_result_cond2$overall_risk_estimates,
      any.missing = FALSE,
      ncols = 5, col.names = "unique"
    )
  )
  expect_true(
    checkmate::test_data_table(
      dep_risk_test_result_cond2$cond_risk_estimates,
      any.missing = FALSE,
      ncols = 8, col.names = "unique"
    )
  )
  expect_equal(
    colnames(dep_risk_test_result_cond2$overall_risk_estimates),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window")
  )
  expect_true(
    checkmate::test_subset(
      colnames(dep_risk_test_result_cond2$cond_risk_estimates),
      c("risk_measure", "risk_est", "alpha", "row_num", "AAPL", "GOOG",
      "cond_alpha", "vine_window")
    )
  )
  expect_true(
    checkmate::test_list(
      dep_risk_test_result_cond2[["fitted_vines"]],
      types = "vinecop"
    )
  )
})

