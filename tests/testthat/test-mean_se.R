test_that("mean_se returns correct mean", {
  out <- mean_se(c(1, 2, 3, 4, 5))
  expect_equal(unname(out["mean"]), 3)
})

test_that("mean_se returns correct standard error", {
  out <- mean_se(c(1, 2, 3, 4, 5))
  expect_equal(unname(out["se"]), sd(c(1, 2, 3, 4, 5)) / sqrt(5))
})

test_that("mean_se removes missing values", {
  out <- mean_se(c(1, 2, 3, NA), na_rm = TRUE)
  expect_equal(unname(out["mean"]), 2)
})

test_that("mean_se errors on non-numeric input", {
  expect_error(mean_se("a"))
})
