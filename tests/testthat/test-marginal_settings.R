test_that("validity works", {
  expect_error(marginal_settings())
  expect_error(marginal_settings(1))
  expect_error(marginal_settings(train_size = 5,
                                 refit_size = 5))
  expect_error(marginal_settings(train_size = 5.1,
                                 refit_size = 5))
  expect_error(marginal_settings(train_size = -3,
                                 refit_size = -5))
  expect_error(marginal_settings(train_size = 10,
                                 refit_size = 5,
                                 individual_spec = list(),
                                 default_spec = list()))
  expect_error(marginal_settings(train_size = 10,
                                 refit_size = 5,
                                 individual_spec = list("ugarch")))
  expect_error(marginal_settings(train_size = 10,
                                 refit_size = 5,
                                 individual_spec = list(default_garch_spec())))
  expect_error(marginal_settings(train_size = 10,
                                 refit_size = 5,
                                 individual_spec =
                                   list("a" = default_garch_spec(),
                                        default_garch_spec())))
  expect_error(marginal_settings(train_size = 10,
                                 refit_size = 5,
                                 individual_spec =
                                   list("a" = default_garch_spec(),
                                        123)))
})

test_that("basic functionality", {
  basic_marginal_settings <- marginal_settings(
    train_size = 10,
    refit_size = 5,
    individual_spec =
      list("a" = default_garch_spec(ma = 4)))
  expect_s4_class(basic_marginal_settings, "marginal_settings")
  expect_equal(basic_marginal_settings@train_size, 10)
  expect_equal(basic_marginal_settings@refit_size, 5)
  expect_s4_class(basic_marginal_settings@default_spec, "uGARCHspec")
  expect_equal(basic_marginal_settings@default_spec, default_garch_spec())
  expect_equal(basic_marginal_settings@individual_spec[["a"]],
               default_garch_spec(ma = 4))
})
