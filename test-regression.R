test_that("civic_fit_regression returns civic_model with linear", {
  df <- tibble::tibble(
    y  = rnorm(100, 50, 10),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  m <- civic_fit_regression(y ~ x1 + x2, df, model = "linear")
  expect_s3_class(m, "civic_model")
  expect_equal(m$type, "regression")
  expect_equal(m$model, "linear")
})

test_that("civic_fit_regression works with CART", {
  df <- tibble::tibble(y = rnorm(80), x1 = rnorm(80), x2 = rnorm(80))
  m  <- civic_fit_regression(y ~ x1 + x2, df, model = "cart")
  expect_s3_class(m, "civic_model")
  expect_equal(m$model, "cart")
})

test_that("predict.civic_model works for regression", {
  df   <- tibble::tibble(y = rnorm(100), x = rnorm(100))
  m    <- civic_fit_regression(y ~ x, df, model = "linear")
  phat <- predict(m, newdata = df)
  expect_equal(length(phat), 100)
  expect_true(is.numeric(phat))
})
