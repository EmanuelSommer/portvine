marg_input_sample_returns_small <- dtplyr::lazy_dt(sample_returns_small) %>%
  mutate(row_num = seq.int(nrow(sample_returns_small))) %>%
  pivot_longer(-row_num, names_to = "asset", values_to = "returns") %>%
  data.table::as.data.table()

test_basic_marginal_models_est <- estimate_marginal_models(
  marg_input_sample_returns_small,
  n_all_obs = 1000, n_marg_train = 900,
  n_marg_refit = 50, n_vine_train = 100,
  all_asset_names = unique(marg_input_sample_returns_small$asset),
  marginal_specs_list = list(
    "AAPL" = default_garch_spec(),
    "GOOG" = default_garch_spec(),
    "AMZN" = default_garch_spec()
  ),
  trace = FALSE
)

test_that("roll_residuals", {
  # input checks
  expect_error(
    roll_residuals(
      test_basic_marginal_models_est$AAPL$roll_model_fit,
      3
    )
  )
  expect_error(
    roll_residuals(
      test_basic_marginal_models_est$AAPL$residuals_dt,
      2
    )
  )
  # basic functionality
  expect_true(
    checkmate::test_numeric(
      roll_residuals(test_basic_marginal_models_est$AAPL$roll_model_fit),
      len = 900
    )
  )
  expect_equal(
    roll_residuals(test_basic_marginal_models_est$AAPL$roll_model_fit),
    roll_residuals(test_basic_marginal_models_est$AAPL$roll_model_fit, 1)
  )
  expect_true(
    checkmate::test_numeric(
      roll_residuals(test_basic_marginal_models_est$AAPL$roll_model_fit, 2),
      len = 900
    )
  )
})
