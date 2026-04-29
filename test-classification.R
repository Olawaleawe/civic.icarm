test_that("civic_fit_classification returns civic_model with CART", {
  df <- tibble::tibble(
    y  = factor(sample(c("yes", "no"), 200, replace = TRUE)),
    x1 = rnorm(200),
    x2 = rbinom(200, 1, 0.4),
    x3 = runif(200)
  )
  m <- civic_fit_classification(y ~ x1 + x2 + x3, df, model = "cart", seed = 42)

  expect_s3_class(m, "civic_model")
  expect_equal(m$type, "classification")
  expect_equal(m$model, "cart")
  expect_equal(m$seed, 42)
  expect_equal(m$n_train, 200)
  expect_true(!is.null(m$data_hash))
  expect_true(!is.null(m$trained_at))
})

test_that("civic_fit_classification returns civic_model with logistic", {
  df <- tibble::tibble(
    y  = factor(sample(c("yes", "no"), 150, replace = TRUE)),
    x1 = rnorm(150),
    x2 = rnorm(150)
  )
  m <- civic_fit_classification(y ~ x1 + x2, df, model = "logistic")
  expect_s3_class(m, "civic_model")
  expect_equal(m$model, "logistic")
})

test_that("predict.civic_model works for CART classification", {
  df <- tibble::tibble(
    y  = factor(sample(c("A", "B"), 100, replace = TRUE)),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  m    <- civic_fit_classification(y ~ x1 + x2, df)
  phat <- predict(m, newdata = df, type = "class")
  expect_equal(length(phat), 100)
  expect_true(all(phat %in% c("A", "B")))
})

test_that("predict.civic_model returns probabilities for CART", {
  df <- tibble::tibble(
    y  = factor(sample(c("A", "B"), 100, replace = TRUE)),
    x1 = rnorm(100)
  )
  m    <- civic_fit_classification(y ~ x1, df)
  probs <- predict(m, newdata = df, type = "prob")
  expect_true(is.matrix(probs))
  expect_true(all(probs >= 0 & probs <= 1))
  expect_equal(ncol(probs), 2)
})

test_that("print.civic_model does not error", {
  df <- tibble::tibble(y = factor(c("a","b","a","b")), x = 1:4)
  m  <- civic_fit_classification(y ~ x, df)
  expect_output(print(m), "civic_model")
})
