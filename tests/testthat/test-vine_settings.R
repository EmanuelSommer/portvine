test_that("validity works", {
  expect_error(vine_settings())
  expect_error(vine_settings(1))
  expect_error(vine_settings(
    train_size = 5,
    refit_size = 5
  ))
  expect_error(vine_settings(
    train_size = 5.1,
    refit_size = 5
  ))
  expect_error(vine_settings(
    train_size = -3,
    refit_size = -5
  ))
  expect_error(vine_settings(10, 5, family_set = 1))
  expect_error(vine_settings(10, 5, vine_type = "copula"))
  expect_error(vine_settings(10, 5, vine_type = c("rvine", "dvine")))
})


test_that("basic functionality", {
  basic_vine_settings <- vine_settings(
    train_size = 10,
    refit_size = 5,
    family_set = c("gumbel", "norm")
  )
  expect_s4_class(basic_vine_settings, "vine_settings")
  expect_equal(basic_vine_settings@train_size, 10)
  expect_equal(basic_vine_settings@refit_size, 5)
  expect_equal(basic_vine_settings@family_set, c("gumbel", "norm"))
  expect_equal(basic_vine_settings@vine_type, "rvine")
})
