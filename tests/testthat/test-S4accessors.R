

test_that("risk_estimates() basic functionality & input checks", {
  t1_marg_settings <- marginal_settings(
    train_size = 900,
    refit_size = 50
  )
  t1_vine_settings <- vine_settings(
    train_size = 50,
    refit_size = 25,
    family_set = c("clayton", "t"),
    vine_type = "rvine"
  )
  t1_risk_roll <- estimate_risk_roll(
    sample_returns_small,
    weights = NULL, # default -> equal weights
    marginal_settings = t1_marg_settings,
    vine_settings = t1_vine_settings,
    alpha = c(0.01, 0.05),
    risk_measures = c("VaR", "ES_mean", "ES_mc"),
    n_samples = 10,
    trace = FALSE
  )

  # input checks
  expect_error(
    risk_estimates(data.table::data.table(risk_estimates = 1:10))
  )
  expect_error(
    risk_estimates(
      roll = t1_risk_roll,
      risk_measures = NULL,
      alpha = 0.03
    )
  )
  expect_error(
    risk_estimates(
      roll = t1_risk_roll,
      risk_measures = c("VaR", "ES_copula"),
      alpha = NULL
    )
  )
  expect_error(
    risk_estimates(
      roll = t1_risk_roll,
      risk_measures = NULL,
      alpha = NULL,
      df = "Yes"
    )
  )
  expect_error(
    risk_estimates(
      roll = t1_risk_roll,
      risk_measures = NULL,
      alpha = NULL,
      exceeded = 2
    )
  )

  # basic unconditional functionality
  expect_true(
    checkmate::test_data_table(
      risk_estimates(
        roll = t1_risk_roll,
        risk_measures = NULL,
        alpha = NULL,
        df = FALSE
      ),
      any.missing = FALSE, nrows = 6 * 100
    )
  )
  expect_true(
    checkmate::test_data_frame(
      risk_estimates(
        roll = t1_risk_roll,
        risk_measures = NULL,
        alpha = NULL
      ),
      any.missing = FALSE, nrows = 6 * 100
    )
  )
  expect_equal(
    colnames(risk_estimates(
      roll = t1_risk_roll,
      risk_measures = NULL,
      alpha = NULL
    )),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window", "realized")
  )
  expect_equal(
    colnames(risk_estimates(
      roll = t1_risk_roll,
      risk_measures = NULL,
      alpha = NULL,
      exceeded = TRUE
    )),
    c(
      "risk_measure", "risk_est", "alpha", "row_num", "vine_window", "realized",
      "exceeded"
    )
  )
  expect_equal(
    colnames(risk_estimates(
      roll = t1_risk_roll,
      risk_measures = "ES_mean",
      alpha = NULL
    )),
    c("risk_measure", "risk_est", "alpha", "row_num", "vine_window", "realized")
  )
  expect_equal(
    unique(risk_estimates(
      roll = t1_risk_roll,
      risk_measures = "ES_mean",
      alpha = NULL
    )[["risk_measure"]]),
    "ES_mean"
  )
  expect_equal(
    unique(risk_estimates(
      roll = t1_risk_roll,
      risk_measures = NULL,
      alpha = 0.05
    )[["alpha"]]),
    0.05
  )
  expect_equal(
    sort(unique(risk_estimates(
      roll = t1_risk_roll,
      risk_measures = c("VaR", "ES_mc"),
      alpha = NULL
    )[["risk_measure"]])),
    sort(c("VaR", "ES_mc"))
  )
  expect_equal(
    sort(unique(risk_estimates(
      roll = t1_risk_roll,
      risk_measures = NULL,
      alpha = NULL
    )[["risk_measure"]])),
    sort(c("VaR", "ES_mc", "ES_mean"))
  )
  expect_equal(
    sort(unique(risk_estimates(
      roll = t1_risk_roll,
      risk_measures = NULL,
      alpha = NULL
    )[["alpha"]])),
    sort(c(0.01, 0.05))
  )

  # basic conditional functionality
  t2_marg_settings <- marginal_settings(
    train_size = 900,
    refit_size = 50
  )
  t2_vine_settings <- vine_settings(
    train_size = 200,
    refit_size = 50,
    family_set = c("t"),
    vine_type = "dvine"
  )

  t2_risk_roll <- estimate_risk_roll(
    sample_returns_small,
    weights = NULL,
    marginal_settings = t2_marg_settings,
    vine_settings = t2_vine_settings,
    alpha = c(0.01, 0.05),
    risk_measures = c("ES_median", "ES_mc", "ES_mean"),
    n_samples = 10,
    n_mc_samples = 100,
    cond_vars = c("GOOG", "AMZN"),
    cond_u = c(0.1, 0.5, 0.9),
    trace = FALSE
  )
  expect_true(
    checkmate::test_data_table(
      risk_estimates(
        roll = t2_risk_roll,
        risk_measures = NULL,
        alpha = NULL,
        df = FALSE
      ),
      any.missing = FALSE, nrows = 6 * 100 * 4
    )
  )
  expect_true(
    checkmate::test_data_table(
      risk_estimates(
        roll = t2_risk_roll,
        risk_measures = NULL,
        alpha = NULL,
        df = FALSE,
        exceeded = TRUE
      ),
      any.missing = FALSE, nrows = 6 * 100 * 4
    )
  )
  expect_true(
    checkmate::test_data_frame(
      risk_estimates(
        roll = t2_risk_roll,
        risk_measures = NULL,
        alpha = NULL,
        df = TRUE
      ),
      any.missing = FALSE, nrows = 6 * 100 * 4
    )
  )
  expect_equal(
    unique(risk_estimates(
      roll = t2_risk_roll,
      risk_measures = "ES_mean",
      alpha = NULL
    )[["risk_measure"]]),
    "ES_mean"
  )
  expect_equal(
    sort(unique(risk_estimates(
      roll = t2_risk_roll,
      risk_measures = NULL,
      alpha = NULL
    )[["risk_measure"]])),
    sort(c("ES_median", "ES_mc", "ES_mean"))
  )
  expect_equal(
    sort(unique(risk_estimates(
      roll = t2_risk_roll,
      risk_measures = NULL,
      alpha = NULL
    )[["alpha"]])),
    sort(c(0.01, 0.05))
  )
  expect_equal(
    sort(unique(risk_estimates(
      roll = t2_risk_roll,
      risk_measures = NULL,
      alpha = NULL
    )[["cond_u"]])),
    sort(c(0.1, 0.5, 0.9, "prior_resid"))
  )
  expect_true(
    checkmate::test_data_frame(
      risk_estimates(
        roll = t2_risk_roll,
        risk_measures = c("ES_median", "ES_mean"),
        alpha = 0.05
      ),
      any.missing = FALSE, nrows = 2 * 100 * 4
    )
  )
  expect_true(
    checkmate::test_data_table(
      risk_estimates(
        roll = t2_risk_roll,
        risk_measures = c("ES_median", "ES_mean"),
        alpha = 0.05,
        cond_u = c(0.5, 0.9),
        df = FALSE
      ),
      any.missing = FALSE, nrows = 2 * 100 * 2
    )
  )
  expect_true(
    checkmate::test_data_table(
      risk_estimates(
        roll = t2_risk_roll,
        risk_measures = NULL,
        alpha = NULL,
        cond_u = NULL,
        cond = FALSE,
        df = FALSE
      ),
      any.missing = FALSE, nrows = 6 * 100 * 1
    )
  )
  expect_true(
    checkmate::test_data_table(
      risk_estimates(
        roll = t2_risk_roll,
        risk_measures = NULL,
        alpha = NULL,
        cond_u = c("prior_resid", 0.1),
        cond = TRUE,
        df = FALSE
      ),
      any.missing = FALSE, nrows = 6 * 100 * 2
    )
  )
  # test print methods
  expect_output(print(t1_risk_roll), "<portvine_roll>")
  expect_output(print(t2_risk_roll), "<cond_portvine_roll>")
  expect_output(summary(t1_risk_roll), "<portvine_roll>")
  expect_output(summary(t1_risk_roll), "Train size")
  expect_output(summary(t2_risk_roll), "<cond_portvine_roll>")

  expect_output(print(t1_vine_settings), "<vine_settings>")
  expect_output(print(t1_marg_settings), "<marginal_settings>")
})

test_that("fitted_vines() & fitted_marginals() basic functionality", {
  t1_marg_settings <- marginal_settings(
    train_size = 960,
    refit_size = 20
  )
  t1_vine_settings <- vine_settings(
    train_size = 100,
    refit_size = 20,
    family_set = "onepar",
    vine_type = "dvine"
  )
  t1_risk_roll <- estimate_risk_roll(
    sample_returns_small,
    weights = NULL, # default -> equal weights
    marginal_settings = t1_marg_settings,
    vine_settings = t1_vine_settings,
    alpha = c(0.01),
    risk_measures = c("VaR"),
    n_samples = 10,
    cond_vars = "AAPL",
    cond_u = 0.5,
    trace = FALSE
  )
  expect_true(
    checkmate::test_list(
      fitted_vines(t1_risk_roll),
      types = "vinecop", any.missing = FALSE, null.ok = FALSE, len = 2
    )
  )
  expect_true(
    checkmate::test_list(
      fitted_marginals(t1_risk_roll),
      types = "uGARCHroll", any.missing = FALSE, null.ok = FALSE, len = 3,
      names = "unique"
    )
  )
})
