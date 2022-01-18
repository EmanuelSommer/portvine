marg_input_sample_returns_small <- dtplyr::lazy_dt(sample_returns_small) %>%
  mutate(row_num = seq.int(nrow(sample_returns_small))) %>%
  pivot_longer(-row_num, names_to = "asset", values_to = "returns") %>%
  data.table::as.data.table()

test_basic_marginal_models_est <- estimate_marginal_models(
  marg_input_sample_returns_small,
  n_all_obs = 1000, n_marg_train = 750,
  n_marg_refit = 50, n_vine_train = 100,
  all_asset_names = unique(marg_input_sample_returns_small$asset),
  marginal_specs_list = list(
    "AAPL" = default_garch_spec(),
    "GOOG" = default_garch_spec(),
    "AMZN" = default_garch_spec()
  ),
  trace = FALSE
)

test_that("basic output types", {
  for (i in seq(3)) {
    expect_s4_class(
      test_basic_marginal_models_est[[i]]$roll_model_fit,
      "uGARCHroll"
    )
    expect_true(
      checkmate::test_data_table(
        test_basic_marginal_models_est[[i]]$residuals_dt,
        ncols = 10
      )
    )
  }
})

test_that("basic checks on the datatable output", {
  for (i in seq(3)) {
    expect_equal(
      sort(unique(
        test_basic_marginal_models_est[[i]]$residuals_dt$marg_window_num
      )),
      1:5
    )
    expect_true(
      all(
        test_basic_marginal_models_est[[i]]$residuals_dt$copula_scale_resid <= 1
      )
    )
    expect_true(
      all(
        test_basic_marginal_models_est[[i]]$residuals_dt$copula_scale_resid >= 0
      )
    )
    expect_true(
      min(test_basic_marginal_models_est[[i]]$residuals_dt$row_num) == 651
    )
    expect_true(
      max(test_basic_marginal_models_est[[i]]$residuals_dt$row_num) == 1000
    )
  }
})

test_that("input checks work", {
  expect_error(
    estimate_marginal_models(
      marg_input_sample_returns_small[, 1:2],
      n_all_obs = 1000, n_marg_train = 750,
      n_marg_refit = 50, n_vine_train = 100,
      all_asset_names = unique(marg_input_sample_returns_small$asset),
      marginal_specs_list = list(
        "AAPL" = default_garch_spec(),
        "GOOG" = default_garch_spec(),
        "AMZN" = default_garch_spec()
      ),
      trace = TRUE
    )
  )
  expect_error(
    estimate_marginal_models(
      marg_input_sample_returns_small,
      n_all_obs = 1000, n_marg_train = 750,
      n_marg_refit = 50, n_vine_train = 100,
      all_asset_names = c("AAPL", "GOOG", "AMAZON"),
      marginal_specs_list = list(
        "AAPL" = default_garch_spec(),
        "GOOG" = default_garch_spec(),
        "AMZN" = default_garch_spec()
      ),
      trace = TRUE
    )
  )
  expect_error(
    estimate_marginal_models(
      marg_input_sample_returns_small,
      n_all_obs = 1000, n_marg_train = 750,
      n_marg_refit = 50, n_vine_train = 100,
      all_asset_names = unique(marg_input_sample_returns_small$asset),
      marginal_specs_list = list(
        "GOOG" = default_garch_spec(),
        "AMZN" = default_garch_spec()
      ),
      trace = TRUE
    )
  )
})
