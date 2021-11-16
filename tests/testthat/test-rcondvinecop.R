### r1conddvine -----------------------------------------------------
rcondvine_test_input <- data.frame(a = runif(100), b = rbeta(100, 2, 6))
rcondvine_test_input$c <- (rcondvine_test_input$a + rcondvine_test_input$b) / 2
rcondvine_test_input$d <- (rcondvine_test_input$a + rcondvine_test_input$b +
                             rcondvine_test_input$c) / 3
rcondvine_test_input$e <- (rcondvine_test_input$d + rcondvine_test_input$b) / 2

rcondvine_test_struct <- rvinecopulib::dvine_structure(c(3,4,1,5,2))

rcondvine_test_fitted <- rvinecopulib::vinecop(
  data = rcondvine_test_input,
  family_set = "all", structure = rcondvine_test_struct,
  presel = FALSE
)


test_that("r1conddvine", {
  r1conddvine_res <- r1conddvine(111, c(0.1, 0.8), rcondvine_test_fitted)
  expect_true(
    checkmate::test_list(r1conddvine_res, len = 2)
  )
  expect_true(
    checkmate::test_data_table(
      r1conddvine_res$sample_dt,
      types = "numeric", any.missing = FALSE,
      ncols = ncol(rcondvine_test_input),
      col.names = "unique"
    )
  )
  expect_equal(
    colnames(r1conddvine_res$sample_dt),
    colnames(rcondvine_test_input)
  )
  expect_equal(
    r1conddvine_res$sample_dt[[3]],
    rep(c(0.1, 0.8), each = 111)
  )
  expect_equal(
    r1conddvine_res$cond_alpha_vec,
    rep(c(0.1, 0.8), each = 111)
  )
})


### r2conddvine -----------------------------------------------------

test_that("r2conddvine", {
  r2conddvine_res <- r2conddvine(111, c(0.1, 0.8), rcondvine_test_fitted)
  expect_true(
    checkmate::test_list(r2conddvine_res, len = 2)
  )
  expect_true(
    checkmate::test_data_table(
      r2conddvine_res$sample_dt,
      types = "numeric", any.missing = FALSE,
      ncols = ncol(rcondvine_test_input),
      col.names = "unique"
    )
  )
  expect_equal(
    colnames(r2conddvine_res$sample_dt),
    colnames(rcondvine_test_input)
  )
  expect_equal(
    r2conddvine_res$sample_dt[[3]],
    rep(c(0.1, 0.8), each = 111)
  )
  expect_equal(
    r2conddvine_res$cond_alpha_vec,
    rep(c(0.1, 0.8), each = 111)
  )
})

### rcondvinecop ----------------------------------------------------

test_that("rcondvinecop input checks", {
  expect_error(
    rcondvinecop(n_samples = 100, cond_alpha = c(0.01, 0.1),
                 cond_vars = "a",
                 fitted_vine = rcondvine_test_fitted,
                 vine_type = "rvine")
  )
  expect_error(
    rcondvinecop(n_samples = 100, cond_alpha = c(0.01, 0.1),
                 cond_vars = c("a", "b", "c"),
                 fitted_vine = rcondvine_test_fitted,
                 vine_type = "dvine")
  )
})

test_that("rcondvinecop functionality", {
  # check that the behavior of the function resembles the correct sampling
  # function. First up: single conditional dvine
  rcondvinecop_res <- rcondvinecop(n_samples = 100, cond_alpha = c(0.01, 0.1),
                                   cond_vars = c("c"),
                                   fitted_vine = rcondvine_test_fitted,
                                   vine_type = "dvine")
  expect_true(
    checkmate::test_list(rcondvinecop_res, len = 2)
  )
  expect_true(
    checkmate::test_data_table(
      rcondvinecop_res$sample_dt,
      types = "numeric", any.missing = FALSE,
      ncols = ncol(rcondvine_test_input),
      col.names = "unique"
    )
  )
  expect_equal(
    colnames(rcondvinecop_res$sample_dt),
    colnames(rcondvine_test_input)
  )
  expect_equal(
    rcondvinecop_res$sample_dt[[3]],
    rep(c(0.01, 0.1), each = 100)
  )
  expect_equal(
    rcondvinecop_res$cond_alpha_vec,
    rep(c(0.01, 0.1), each = 100)
  )
  # now 2 conditional dvine
  rcondvinecop_res <- rcondvinecop(n_samples = 100, cond_alpha = c(0.01, 0.1),
                                   cond_vars = c("c", "d"),
                                   fitted_vine = rcondvine_test_fitted,
                                   vine_type = "dvine")
  expect_true(
    checkmate::test_list(rcondvinecop_res, len = 2)
  )
  expect_true(
    checkmate::test_data_table(
      rcondvinecop_res$sample_dt,
      types = "numeric", any.missing = FALSE,
      ncols = ncol(rcondvine_test_input),
      col.names = "unique"
    )
  )
  expect_equal(
    colnames(rcondvinecop_res$sample_dt),
    colnames(rcondvine_test_input)
  )
  expect_equal(
    rcondvinecop_res$sample_dt[[3]],
    rep(c(0.01, 0.1), each = 100)
  )
  expect_equal(
    rcondvinecop_res$cond_alpha_vec,
    rep(c(0.01, 0.1), each = 100)
  )
})


