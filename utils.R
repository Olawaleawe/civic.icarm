# ============================================================
# civic.icarm: Internal utilities
# ============================================================

#' Null-coalescing operator (internal)
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

#' civic.icarm colour palette (DataCitizen-Pro brand)
#' @noRd
.civic_palette <- c(
  primary    = "#1B4F72",   # deep civic blue
  secondary  = "#2E86AB",   # mid blue
  accent     = "#F39C12",   # democratic amber
  fair       = "#27AE60",   # equity green
  unfair     = "#C0392B",   # disparity red
  neutral    = "#7F8C8D",   # grey
  background = "#FDFEFE"
)

#' Base ggplot2 theme for civic.icarm
#' @noRd
.civic_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(
        colour = .civic_palette["primary"], face = "bold", size = base_size + 2),
      plot.subtitle = ggplot2::element_text(colour = .civic_palette["neutral"]),
      plot.caption  = ggplot2::element_text(
        colour = .civic_palette["neutral"], size = base_size - 2, hjust = 0),
      axis.title    = ggplot2::element_text(colour = .civic_palette["primary"]),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      strip.text    = ggplot2::element_text(face = "bold")
    )
}

#' Validate that object is a civic_model
#' @noRd
.check_civic_model <- function(x, call = rlang::caller_env()) {
  if (!inherits(x, "civic_model")) {
    rlang::abort(
      c("Expected a `civic_model` object.",
        i = "Use `civic_fit_classification()` or `civic_fit_regression()` first."),
      call = call
    )
  }
  invisible(x)
}

#' Compute binary confusion matrix components
#' @noRd
.confusion <- function(y_true, y_pred, positive) {
  tp <- sum(y_pred == positive & y_true == positive)
  tn <- sum(y_pred != positive & y_true != positive)
  fp <- sum(y_pred == positive & y_true != positive)
  fn <- sum(y_pred != positive & y_true == positive)
  list(tp = tp, tn = tn, fp = fp, fn = fn,
       n = length(y_true),
       tpr = tp / max(tp + fn, 1),
       tnr = tn / max(tn + fp, 1),
       fpr = fp / max(fp + tn, 1),
       fnr = fn / max(fn + tp, 1),
       ppv = tp / max(tp + fp, 1),
       acc = (tp + tn) / max(length(y_true), 1))
}

# ============================================================
#' Train/test split utility
#'
#' A simple reproducible train/test split following ICARM transparency
#' principles: seed is always recorded and returned.
#'
#' @param data A data.frame or tibble.
#' @param prop Proportion assigned to training set (default 0.75).
#' @param seed Integer seed for reproducibility.
#' @param stratify Optional character name of a column to stratify on.
#'
#' @return A named list with elements `train`, `test`, and `seed`.
#' @export
#'
#' @examples
#' splits <- civic_split(civic_voting, prop = 0.75, stratify = "voted")
#' nrow(splits$train)
civic_split <- function(data, prop = 0.75, seed = 2025, stratify = NULL) {
  stopifnot(is.data.frame(data), prop > 0, prop < 1)
  set.seed(seed)

  if (!is.null(stratify)) {
    idx <- unlist(lapply(split(seq_len(nrow(data)), data[[stratify]]), function(i) {
      sample(i, floor(length(i) * prop))
    }))
  } else {
    idx <- sample(seq_len(nrow(data)), floor(nrow(data) * prop))
  }

  list(
    train = data[ idx, , drop = FALSE],
    test  = data[-idx, , drop = FALSE],
    seed  = seed,
    prop  = prop
  )
}

# ============================================================
#' Compute standard classification or regression metrics
#'
#' @param y_true True outcomes (factor for classification, numeric for regression).
#' @param y_pred Predicted values (factor/character for classification, numeric for regression).
#' @param y_prob Optional numeric probability vector for the positive class
#'   (used for AUC; classification only).
#' @param positive The positive class level (classification only).
#' @param type One of `"auto"`, `"classification"`, `"regression"`.
#'
#' @return A named numeric vector of metrics.
#' @export
#'
#' @examples
#' y <- factor(c("yes","no","yes","yes","no"))
#' yhat <- factor(c("yes","no","no","yes","no"))
#' civic_metrics(y, yhat, positive = "yes")
civic_metrics <- function(y_true, y_pred, y_prob = NULL,
                          positive = NULL, type = "auto") {
  if (type == "auto") {
    type <- if (is.factor(y_true) || is.character(y_true)) "classification" else "regression"
  }

  if (type == "classification") {
    y_true <- factor(y_true)
    y_pred <- factor(y_pred, levels = levels(y_true))
    if (is.null(positive)) positive <- levels(y_true)[1]
    cm <- .confusion(y_true, y_pred, positive)
    out <- c(
      accuracy  = cm$acc,
      precision = cm$ppv,
      recall    = cm$tpr,
      specificity = cm$tnr,
      f1        = 2 * cm$ppv * cm$tpr / max(cm$ppv + cm$tpr, 1e-9),
      balanced_acc = (cm$tpr + cm$tnr) / 2
    )
    if (!is.null(y_prob) && requireNamespace("pROC", quietly = TRUE)) {
      roc_obj <- tryCatch(
        pROC::roc(response = y_true, predictor = y_prob,
                  levels = c(setdiff(levels(y_true), positive), positive),
                  quiet = TRUE),
        error = function(e) NULL
      )
      if (!is.null(roc_obj)) out["auc"] <- as.numeric(pROC::auc(roc_obj))
    }
    return(out)
  }

  # regression
  resid <- as.numeric(y_true) - as.numeric(y_pred)
  c(
    mae   = mean(abs(resid)),
    rmse  = sqrt(mean(resid^2)),
    r2    = 1 - sum(resid^2) / sum((as.numeric(y_true) - mean(as.numeric(y_true)))^2)
  )
}

# ============================================================
#' Threshold sweep for binary classification
#'
#' Computes performance metrics across a grid of classification thresholds,
#' supporting threshold selection under fairness constraints — a core
#' DataCitizen-Pro teaching tool for democratic judgment.
#'
#' @param y_true Factor of true outcomes.
#' @param y_prob Numeric vector of predicted probabilities for the positive class.
#' @param positive Positive class level.
#' @param thresholds Numeric vector of thresholds (default: seq(0.1, 0.9, 0.05)).
#'
#' @return A tibble with one row per threshold and columns for accuracy,
#'   precision, recall, specificity, F1, and balanced accuracy.
#' @export
#'
#' @examples
#' y <- factor(sample(c("yes","no"), 200, replace = TRUE))
#' p <- runif(200)
#' civic_thresholds(y, p, positive = "yes")
civic_thresholds <- function(y_true, y_prob,
                              positive = NULL,
                              thresholds = seq(0.1, 0.9, by = 0.05)) {
  y_true <- factor(y_true)
  if (is.null(positive)) positive <- levels(y_true)[1]
  negative <- setdiff(levels(y_true), positive)[1]

  purrr::map_dfr(thresholds, function(thr) {
    y_pred <- factor(ifelse(y_prob >= thr, positive, negative),
                     levels = levels(y_true))
    cm <- .confusion(y_true, y_pred, positive)
    tibble::tibble(
      threshold    = thr,
      accuracy     = cm$acc,
      precision    = cm$ppv,
      recall       = cm$tpr,
      specificity  = cm$tnr,
      f1           = 2 * cm$ppv * cm$tpr / max(cm$ppv + cm$tpr, 1e-9),
      balanced_acc = (cm$tpr + cm$tnr) / 2,
      rate_positive = mean(y_pred == positive)
    )
  })
}
