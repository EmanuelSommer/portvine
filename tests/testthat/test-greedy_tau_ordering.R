
test_that("test funcionality", {
  expect_equal(
    greedy_tau_ordering(vine_train_data = mtcars[, 3:7], NULL),
    c(4, 1, 2, 5, 3)
  )
  expect_equal(
    greedy_tau_ordering(vine_train_data = mtcars[, 3:8], NULL),
    c(4, 1, 2, 6, 5, 3)
  )
  expect_equal(
    greedy_tau_ordering(vine_train_data = mtcars[, 3:8], "wt"),
    c(4, 1, 2, 6, 5, 3)
  )
  expect_equal(
    greedy_tau_ordering(vine_train_data = mtcars[, 3:8], "drat"),
    c(3, 4, 1, 2, 6, 5)
  )
  expect_equal(
    greedy_tau_ordering(vine_train_data = mtcars[, 3:8], c("drat", "hp")),
    c(3, 2, 1, 4, 6, 5)
  )
})

test_that("test input checks", {
  expect_error(
    greedy_tau_ordering(vine_train_data = mtcars[, 3:7], "copulas")
  )
  expect_error(
    greedy_tau_ordering(
      vine_train_data = as.matrix(mtcars[, 3:8]), NULL
    )
  )
  expect_error(
    greedy_tau_ordering(vine_train_data = mtcars[, 3:4], "wt")
  )
  expect_error(
    greedy_tau_ordering(vine_train_data = mtcars[, 3:8],
                        c("wt", "drat", "disp"))
  )
})
