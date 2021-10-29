test_that("input checks", {
  expect_error(default_garch_spec(ar = 2, ma = NA))
  expect_error(default_garch_spec(garch = 0))
  expect_error(default_garch_spec(arch = 2.3))
  expect_error(default_garch_spec(dist = "binom"))
  expect_error(default_garch_spec(dist = 1))
})

test_that("basic functionality", {
  expect_s4_class(default_garch_spec(), "uGARCHspec")
  basic_garch_spec <- default_garch_spec(
    dist = "norm", ma = 2, arch = 2)
  expect_s4_class(basic_garch_spec, "uGARCHspec")
  expect_equal(basic_garch_spec@model$modeldesc$distribution, "norm")
})
