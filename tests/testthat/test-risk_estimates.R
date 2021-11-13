### TBD: conditional case

test_that("basic functionality & input checks", {
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
    risk_measures = c("VaR", "ES_mean", "ES_mc"),
    n_samples = 1000,
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

  # basic unconditional functionality
  expect_s3_class(
    risk_estimates(
      roll = t1_risk_roll,
      risk_measures = NULL,
      alpha = NULL
    ),
    "data.table"
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
})
