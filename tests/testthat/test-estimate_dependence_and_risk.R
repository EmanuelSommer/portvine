# create some valid inputs for testing
vine_input_sample_returns_small <- dtplyr::lazy_dt(sample_returns_small) %>%
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
  lapply(
    test_basic_marginal_models_est_forvines,
    function(asset) asset$residuals_dt
  )
)

test_that("unconditional case", {
  # rvine
  edr_rvine <- estimate_dependence_and_risk(
    test_combined_residuals_dt,
    1000,
    750, 50,
    100, 25,
    unique(test_combined_residuals_dt$asset),
    "parametric", "rvine",
    c(0.1, 0.2),
    c("ES_median", "VaR"),
    matrix(rep(c(2, 3, 5), 10),
      byrow = TRUE, ncol = 3,
      dimnames = list(NULL, c("GOOG", "AAPL", "AMZN"))
    ),
    NULL,
    1000,
    0.05,
    100,
    FALSE
  )
  expect_true(
    is.null(
      edr_rvine[["cond_risk_estimates"]]
    )
  )
  expect_s3_class(
    edr_rvine[["overall_risk_estimates"]],
    "data.table"
  )
  expect_equal(
    colnames(edr_rvine[["overall_risk_estimates"]]),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window")
  )
  expect_true(
    checkmate::test_list(
      edr_rvine[["fitted_vines"]],
      types = "vinecop"
    )
  )
  # dvine
  edr_dvine <- estimate_dependence_and_risk(
    test_combined_residuals_dt,
    1000,
    750, 50,
    100, 25,
    unique(test_combined_residuals_dt$asset),
    "parametric", "dvine",
    c(0.1, 0.2),
    c("ES_median", "VaR"),
    matrix(rep(c(2, 3, 5), 10),
      byrow = TRUE, ncol = 3,
      dimnames = list(NULL, c("GOOG", "AAPL", "AMZN"))
    ),
    NULL,
    1000,
    0.05,
    100,
    FALSE
  )

  expect_true(
    is.null(
      edr_dvine[["cond_risk_estimates"]]
    )
  )
  expect_s3_class(
    edr_dvine[["overall_risk_estimates"]],
    "data.table"
  )
  expect_equal(
    colnames(edr_dvine[["overall_risk_estimates"]]),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window")
  )
  expect_true(
    checkmate::test_list(
      edr_dvine[["fitted_vines"]],
      types = "vinecop"
    )
  )
  # different family_set, alphas and risk measure
  edr_dvine2 <- estimate_dependence_and_risk(
    test_combined_residuals_dt,
    1000,
    750, 50,
    100, 25,
    unique(test_combined_residuals_dt$asset),
    c("clayton", "gumbel", "frank"), "dvine",
    c(0.1),
    c("ES_mc"),
    matrix(rep(c(2, 3, 5), 10),
      byrow = TRUE, ncol = 3,
      dimnames = list(NULL, c("GOOG", "AAPL", "AMZN"))
    ),
    NULL,
    1000,
    0.05,
    100,
    FALSE
  )
  expect_true(
    checkmate::test_list(
      edr_dvine2[["fitted_vines"]],
      types = "vinecop"
    )
  )
  # other alphas and risk measure
  expect_equal(
    colnames(edr_dvine2[["overall_risk_estimates"]]),
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
    family_set = "parametric", vine_type = "dvine",
    alpha = c(0.1, 0.2),
    risk_measures = c("ES_median", "VaR"),
    weights = matrix(rep(c(2, 3, 5), 10),
      byrow = TRUE, ncol = 3,
      dimnames = list(NULL, c("GOOG", "AAPL", "AMZN"))
    ),
    cond_vars = "AAPL",
    n_samples = 10,
    cond_u = c(0.05, 0.5),
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
    c(
      "risk_measure", "risk_est", "alpha", "row_num", "AAPL",
      "cond_u", "vine_window"
    )
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
    family_set = "parametric", vine_type = "dvine",
    alpha = c(0.1, 0.2),
    risk_measures = c("ES_mean", "VaR"),
    weights = matrix(rep(c(2, 3, 5), 10),
      byrow = TRUE, ncol = 3,
      dimnames = list(NULL, c("GOOG", "AAPL", "AMZN"))
    ),
    cond_vars = c("AAPL", "GOOG"),
    n_samples = 10,
    cond_u = c(0.05, 0.5),
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
      c(
        "risk_measure", "risk_est", "alpha", "row_num", "AAPL", "GOOG",
        "cond_u", "vine_window"
      )
    )
  )
  expect_true(
    checkmate::test_list(
      dep_risk_test_result_cond2[["fitted_vines"]],
      types = "vinecop"
    )
  )
})
