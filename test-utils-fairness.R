test_that("civic_split returns correct proportions", {
  df    <- tibble::tibble(y = factor(rep(c("a","b"), 500)), x = rnorm(1000))
  splits <- civic_split(df, prop = 0.75, seed = 99)

  expect_named(splits, c("train", "test", "seed", "prop"))
  expect_equal(nrow(splits$train) + nrow(splits$test), 1000)
  expect_true(nrow(splits$train) >= 740 && nrow(splits$train) <= 760)
  expect_equal(splits$seed, 99)
})

test_that("civic_split stratification maintains roughly equal class ratios", {
  df     <- tibble::tibble(y = factor(rep(c("yes","no"), each = 500)), x = rnorm(1000))
  splits <- civic_split(df, stratify = "y", prop = 0.8)
  tr_tab <- prop.table(table(splits$train$y))
  te_tab <- prop.table(table(splits$test$y))
  expect_true(abs(tr_tab["yes"] - 0.5) < 0.02)
  expect_true(abs(te_tab["yes"] - 0.5) < 0.02)
})

test_that("civic_metrics works for classification", {
  y    <- factor(c("yes","no","yes","yes","no","no","yes","no"))
  yhat <- factor(c("yes","no","no","yes","no","yes","yes","no"))
  m    <- civic_metrics(y, yhat, positive = "yes")
  expect_named(m, c("accuracy","precision","recall","specificity","f1","balanced_acc"))
  expect_true(all(m >= 0 & m <= 1))
})

test_that("civic_metrics works for regression", {
  y    <- rnorm(100, 50, 10)
  yhat <- y + rnorm(100, 0, 5)
  m    <- civic_metrics(y, yhat, type = "regression")
  expect_named(m, c("mae","rmse","r2"))
  expect_true(m["mae"] > 0)
})

test_that("civic_thresholds returns a tibble with correct columns", {
  y <- factor(sample(c("yes","no"), 200, replace = TRUE))
  p <- runif(200)
  thr <- civic_thresholds(y, p, positive = "yes")
  expect_s3_class(thr, "tbl_df")
  expect_true("threshold" %in% names(thr))
  expect_true("accuracy"  %in% names(thr))
  expect_true("recall"    %in% names(thr))
})

test_that("civic_fairness_report returns civic_fairness for classification", {
  set.seed(7)
  df <- tibble::tibble(
    y     = factor(sample(c("yes","no"), 300, replace = TRUE)),
    grp   = factor(sample(c("A","B","C"), 300, replace = TRUE)),
    x1    = rnorm(300),
    x2    = rnorm(300)
  )
  m    <- civic_fit_classification(y ~ x1 + x2, df)
  fair <- civic_fairness_report(m, df, outcome = "y",
                                protected = "grp", positive = "yes")
  expect_s3_class(fair, "civic_fairness")
  expect_true("tpr" %in% names(fair))
  expect_true("dp_ratio" %in% names(fair))
  expect_equal(nrow(fair), 3)
})

test_that("civic_audit_trail returns valid JSON string", {
  df <- tibble::tibble(y = factor(c("a","b","a","b")), x = 1:4)
  m  <- civic_fit_classification(y ~ x, df)
  json <- civic_audit_trail(m, notes = "unit test", analyst = "tester")
  expect_type(json, "character")
  parsed <- jsonlite::fromJSON(json)
  expect_equal(parsed$notes, "unit test")
  expect_equal(parsed$analyst, "tester")
  expect_equal(parsed$learner, "cart")
  expect_true(!is.null(parsed$data_hash))
})

test_that("civic_explain extracts importance for CART", {
  df <- tibble::tibble(
    y  = factor(sample(c("yes","no"), 100, replace = TRUE)),
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  m  <- civic_fit_classification(y ~ x1 + x2, df)
  ex <- civic_explain(m)
  expect_s3_class(ex, "civic_explainer")
  expect_true(!is.null(ex$importance))
  expect_true("feature" %in% names(ex$importance))
})

test_that("civic_plot_importance returns ggplot", {
  df <- tibble::tibble(
    y  = factor(sample(c("yes","no"), 100, replace = TRUE)),
    x1 = rnorm(100), x2 = rnorm(100)
  )
  m  <- civic_fit_classification(y ~ x1 + x2, df)
  ex <- civic_explain(m)
  p  <- civic_plot_importance(ex)
  expect_s3_class(p, "ggplot")
})
