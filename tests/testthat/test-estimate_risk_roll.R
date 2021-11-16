### TBD: conditional case

test_that("input checks", {
  valid_marg_settings <- marginal_settings(
    train_size = 800,
    refit_size = 100
  )
  valid_vine_settings <- vine_settings(
    train_size = 100,
    refit_size = 50,
    family_set = "onepar",
    vine_type = "rvine"
  )
  # data argument
  expect_error(
    estimate_risk_roll(
      1:10,
      weights = NULL,
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      apply(as.matrix(sample_returns_small), 1, as.character),
      weights = NULL,
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      lm(c(1:10)~1),
      weights = NULL,
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  # weights argument
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = c(1, 2, 1),
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = c("GOOG" = 0, "AMZN" = 0, "AAPL" = -1),
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = c("GOOG" = "1", "AMZN" = "0", "AAPL" = "1"),
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  # marginal and vine settings arguments
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = "some_settings",
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = valid_marg_settings,
      vine_settings = "some_settings",
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = marginal_settings(
        train_size = 1000,
        refit_size = 100
      ),
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = marginal_settings(800, 202),
      vine_settings = vine_settings(300, 202),
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = marginal_settings(800, 100),
      vine_settings = vine_settings(801, 50),
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = marginal_settings(800, 100),
      vine_settings = vine_settings(200, 51),
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  # alpha argument
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c("0.01", 0.05),
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = 1.1,
      risk_measures = c("VaR", "ES_mean"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  # risk measure argument
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_other"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  # cond vars argument
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      cond_vars = c("AAPL", "GOOG", "AMZN"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = NULL,
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      cond_vars = c("AAPL", "COPULA"),
      n_samples = 1000,
      trace = FALSE
    )
  )
  expect_error(
    estimate_risk_roll(
      sample_returns_small,
      weights = c("AAPL" = 1, "GOOG" = 1, "AMZN" = 1),
      marginal_settings = valid_marg_settings,
      vine_settings = valid_vine_settings,
      alpha = c(0.01, 0.05),
      risk_measures = c("VaR", "ES_mean"),
      cond_vars = "AAPL",
      n_samples = 1000,
      trace = FALSE
    )
  )
})

test_that("basic functionality (unconditionally)", {
  # rvine and n_all_obs - train_size is dividable by refit size
  t1_marg_settings <- marginal_settings(
    train_size = 800,
    refit_size = 100
  )
  t1_vine_settings <- vine_settings(
    train_size = 100,
    refit_size = 50,
    family_set = "onepar",
    vine_type = "rvine"
  )
  t1_risk_roll <- estimate_risk_roll(
    sample_returns_small,
    weights = NULL, # default -> equal weights
    marginal_settings = t1_marg_settings,
    vine_settings = t1_vine_settings,
    alpha = c(0.01, 0.05),
    risk_measures = c("VaR", "ES_mean"),
    n_samples = 1000,
    trace = FALSE
  )
  expect_s4_class(t1_risk_roll, "portvine_roll")
  expect_equal(
    colnames(t1_risk_roll@risk_estimates),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window", "realized")
  )
  expect_equal(
    dim(t1_risk_roll@risk_estimates),
    c(200 * 2 * 2, 6)
  )
  expect_equal(
    t1_risk_roll@weights,
    c('AAPL' = 1, 'GOOG' = 1, 'AMZN' = 1)
  )
  expect_false(
    t1_risk_roll@cond_estimation
  )
  expect_equal(
    length(t1_risk_roll@fitted_vines),
    4
  )
  expect_equal(
    t1_risk_roll@marg_models_fit[[1]]@model$n.refits,
    2
  )
  # dvine and n_all_obs - train_size is NOT dividable by refit size
  t2_marg_settings <- marginal_settings(
    train_size = 800,
    refit_size = 199
  )
  t2_vine_settings <- vine_settings(
    train_size = 200,
    refit_size = 199,
    family_set = "onepar",
    vine_type = "dvine"
  )
  expect_message(
    t2_risk_roll <- estimate_risk_roll(
      sample_returns_small,
      weights = c("GOOG" = 1,"AAPL" =  2, "AMZN" = 19),
      marginal_settings = t2_marg_settings,
      vine_settings = t2_vine_settings,
      alpha = 0.01,
      risk_measures = c("VaR", "ES_median", "ES_mc"),
      n_samples = 1000,
      n_mc_samples = 1000,
      trace = FALSE
    ), regexp = "^The last window of interest is shorter*"
  )
  expect_s4_class(t2_risk_roll, "portvine_roll")
  expect_equal(
    dim(t2_risk_roll@risk_estimates),
    c(200 * 1 * 3, 6)
  )
  expect_equal(
    length(t2_risk_roll@fitted_vines),
    2
  )
  expect_equal(
    t2_risk_roll@marg_models_fit[[1]]@model$n.refits,
    2
  )
  ### conditional estimation with dvines ------------------------------

  # TBD


})
