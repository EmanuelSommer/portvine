
order_test_df <- data.frame(
  x1 = c(0.1, 0.3, 0.2, 0.1, 0.8, 0.9),
  x2 = c(0.7, 0.6, 0.5, 0.3, 0.5, 0.2),
  x3 = c(0.4, 0.4, 0.3, 0.3, 0.5, 0.5),
  x4 = c(0.5, 0.6, 0.9, 0.8, 0.6, 0.77),
  x5 = c(0.2, 0.1, 0.22, 0.15, 0.5, 0.95)
)

test_that("test funcionality", {
  expect_equal(
    dvine_ordering(vine_train_data = order_test_df, NULL),
    c(2, 4, 3, 1, 5)
  )
  expect_equal(
    dvine_ordering(vine_train_data = order_test_df, "x2"),
    c(2, 4, 3, 1, 5)
  )
  expect_equal(
    dvine_ordering(order_test_df, c("x4", "x1")),
    c(1, 4, 2, 3, 5)
  )
  expect_equal(
    dvine_ordering(order_test_df, c("x4", "x1"), 2),
    c(1, 4, 2, 3, 5)
  )
  expect_equal(
    dvine_ordering(order_test_df, c("x4", "x1"), 1),
    c(1, 4, 2, 5, 3)
  )
  expect_equal(
    dvine_ordering(order_test_df[, c(1, 5, 4)], c("x4", "x1"), 1),
    c(3, 1, 2)
  )
})

test_that("test input checks", {
  expect_error(
    dvine_ordering(vine_train_data = order_test_df, "copulas")
  )
  expect_error(
    dvine_ordering(
      vine_train_data = as.matrix(order_test_df), NULL
    )
  )
  expect_error(
    dvine_ordering(vine_train_data = order_test_df[, 1:2], NULL)
  )
  expect_error(
    dvine_ordering(
      vine_train_data = order_test_df,
      c("x1", "x2", "x3")
    )
  )
})
