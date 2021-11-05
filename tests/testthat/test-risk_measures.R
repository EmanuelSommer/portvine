test_that("value at risk", {
  expect_equal(est_var(0:100, c(0.1, 0.2, 0.3)), c(10, 20, 30))
  expect_equal(est_var(0:100, 0.5), 50)
  expect_error(est_var(0:100, c(0.1, 0.2, 2)))
  expect_error(est_var(0:100, c(0.1, 0.2, NA)))
  expect_error(est_var(data.frame(0:100), c(0.1, 0.2)))
})

test_that("expected shortfall", {
  expect_equal(est_es(0:100, c(0.1, 0.2, 0.3)), c(5, 10, 15))
  expect_equal(est_es(0:100, 0.5), 25)
  expect_equal(est_es(0:100, 0.5, method = "median"), 25)
  expect_equal(length(est_es(0:100, 0.5, method = "mc", mc_samples = 100)), 1)
  expect_error(est_es(0:100, 0.5, method = "copula"))
  expect_error(est_es(0:100, 0.5, method = c("mean", "median")))
  expect_error(est_es(data.frame(0:100), 0.5, method = "median"))
  expect_error(est_es(0:100, NA))
  expect_error(est_es(0:100, 0.5, method = "mc", mc_samples = 0))
})

test_that("multi risk measures utility", {
  basic_est_risk_measures <- est_risk_measures(c("ES_mean", "VaR"),
                                               0:100, c(0.1, 0.2), 100, 999)
  expect_equal(basic_est_risk_measures$row_num, rep(999, 4))
  expect_equal(basic_est_risk_measures$alpha, rep(c(0.1, 0.2), 2))
  expect_equal(basic_est_risk_measures$risk_est, c(5, 10, 10, 20))
  expect_equal(basic_est_risk_measures$risk_measure, rep(c("ES_mean", "VaR"),
                                                         each = 2))
  expect_equal(dim(est_risk_measures(c("ES_mean", "VaR", "ES_median", "ES_mc"),
                                     0:100, c(1:4)/10, 100, 999)), c(16, 4))
  expect_s3_class(basic_est_risk_measures, "data.table")
})


