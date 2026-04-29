# ============================================================
# civic.icarm: Probability Calibration
# ============================================================

#' Assess and plot probability calibration
#'
#' @description
#' Checks whether a classifier's predicted probabilities are well-calibrated —
#' i.e., a predicted probability of 0.7 should correspond to about 70% of
#' instances actually belonging to the positive class.
#'
#' Calibration is a DataCitizen-Pro *statistical reasoning* competency:
#' learners explore how uncalibrated models mislead decision-makers even
#' when accuracy is high, and why calibration matters for democratic
#' systems that communicate risk to citizens.
#'
#' @param object A `civic_model` (classification only).
#' @param data A data.frame for evaluation.
#' @param outcome Character. Name of the outcome column.
#' @param positive Character. Positive class level.
#' @param n_bins Integer. Number of probability bins (default 10).
#'
#' @return An object of class `civic_calibration` — a list with:
#'   \item{bins}{Tibble of bin midpoints, mean predicted prob, observed freq, count.}
#'   \item{brier_score}{Brier score (lower = better, 0 = perfect).}
#'   \item{ece}{Expected Calibration Error (weighted mean |predicted - observed|).}
#'   \item{model}{The civic_model.}
#'
#' @export
#'
#' @examples
#' splits <- civic_split(civic_voting, stratify = "voted")
#' m      <- civic_fit_classification(voted ~ age + education, splits$train)
#' cal    <- civic_calibration(m, splits$test, "voted", "yes")
#' print(cal)
#' civic_plot_calibration(cal)
civic_calibration <- function(object, data, outcome, positive = NULL,
                               n_bins = 10) {
  .check_civic_model(object)
  if (object$type != "classification") {
    rlang::abort("Calibration is only available for classification models.")
  }

  y <- factor(data[[outcome]])
  if (is.null(positive)) positive <- levels(y)[1]

  probs <- tryCatch(
    predict.civic_model(object, newdata = data, type = "prob"),
    error = function(e) rlang::abort(paste("predict failed:", conditionMessage(e)))
  )
  if (is.matrix(probs)) {
    ppos <- if (positive %in% colnames(probs)) probs[, positive]
            else probs[, ncol(probs)]
  } else {
    ppos <- as.numeric(probs)
  }

  y_bin <- as.integer(y == positive)

  # Brier score
  brier <- mean((ppos - y_bin)^2)

  # Calibration bins
  breaks   <- seq(0, 1, length.out = n_bins + 1)
  bin_idx  <- findInterval(ppos, breaks, rightmost.closed = TRUE)
  bin_idx  <- pmin(bin_idx, n_bins)

  bins <- purrr::map_dfr(seq_len(n_bins), function(b) {
    idx  <- bin_idx == b
    n_b  <- sum(idx)
    tibble::tibble(
      bin        = b,
      bin_mid    = (breaks[b] + breaks[b + 1]) / 2,
      bin_lower  = breaks[b],
      bin_upper  = breaks[b + 1],
      n          = n_b,
      mean_pred  = if (n_b > 0) mean(ppos[idx]) else NA_real_,
      obs_freq   = if (n_b > 0) mean(y_bin[idx]) else NA_real_
    )
  }) |> dplyr::filter(!is.na(mean_pred))

  # Expected Calibration Error (ECE)
  ece <- with(bins, sum(n * abs(mean_pred - obs_freq), na.rm = TRUE) / sum(n))

  out <- list(
    bins        = bins,
    brier_score = round(brier, 4),
    ece         = round(ece, 4),
    positive    = positive,
    outcome     = outcome,
    model       = object
  )
  class(out) <- "civic_calibration"
  out
}

#' Print a civic_calibration object
#' @export
print.civic_calibration <- function(x, ...) {
  cat(cli_rule("civic_calibration"), "\n")
  cat(sprintf("  Model       : %s / %s\n", x$model$type, x$model$model))
  cat(sprintf("  Outcome     : %s (positive = '%s')\n", x$outcome, x$positive))
  cat(sprintf("  Brier Score : %.4f  (0 = perfect, 0.25 = uninformative)\n",
              x$brier_score))
  cat(sprintf("  ECE         : %.4f  (expected calibration error)\n", x$ece))
  cat(sprintf("  Calibration : %s\n",
              if (x$ece < 0.05) "GOOD (ECE < 0.05)"
              else if (x$ece < 0.10) "MODERATE (ECE 0.05-0.10)"
              else "POOR (ECE >= 0.10) — consider Platt scaling"))
  invisible(x)
}

# ============================================================
#' Plot a calibration curve
#'
#' @description
#' Produces a reliability diagram (calibration curve) for a
#' `civic_calibration` object. A perfectly calibrated model produces
#' points along the diagonal y = x.
#'
#' @param calibration A `civic_calibration` from [civic_calibration()].
#' @param title Optional title.
#'
#' @return A `ggplot2` object.
#' @export
civic_plot_calibration <- function(calibration, title = NULL) {
  stopifnot(inherits(calibration, "civic_calibration"))
  bins <- calibration$bins

  ggplot2::ggplot(bins, ggplot2::aes(x = mean_pred, y = obs_freq)) +
    # Perfect calibration reference
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", colour = .civic_palette["neutral"],
                         linewidth = 0.8) +
    # Confidence band (±0.1)
    ggplot2::annotate("ribbon", x = c(0, 1),
                      ymin = c(-0.1, 0.9), ymax = c(0.1, 1.1),
                      alpha = 0.06, fill = .civic_palette["fair"]) +
    # Calibration points sized by n
    ggplot2::geom_point(ggplot2::aes(size = n),
                        colour = .civic_palette["primary"], alpha = 0.85) +
    ggplot2::geom_line(colour = .civic_palette["blue"], linewidth = 0.9) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("n=", n)),
      hjust = -0.2, vjust = 0.5, size = 2.8,
      colour = .civic_palette["grey"]
    ) +
    ggplot2::scale_size_continuous(range = c(2, 8), guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x       = "Mean predicted probability",
      y       = "Observed frequency",
      title   = title %||% paste0("Calibration Curve — ", calibration$model$model),
      subtitle = sprintf(
        "Brier score: %.4f  |  ECE: %.4f  |  Positive class: '%s'",
        calibration$brier_score, calibration$ece, calibration$positive
      ),
      caption = "Dashed line = perfect calibration | DataCitizen-Pro / civic.icarm"
    ) +
    .civic_theme()
}

# ============================================================
#' Compute equalized odds curves across thresholds
#'
#' @description
#' Produces a tibble of TPR and FPR for each group at each threshold,
#' enabling equalized odds analysis — a key DataCitizen-Pro fairness tool.
#'
#' @param object A `civic_model`.
#' @param data A data.frame.
#' @param outcome Character. Outcome column name.
#' @param protected Character. Protected attribute column name.
#' @param positive Character. Positive class level.
#' @param thresholds Numeric vector of thresholds.
#'
#' @return A tibble with columns: threshold, group, tpr, fpr, tnr.
#' @export
#'
#' @examples
#' m   <- civic_fit_classification(voted ~ age + education, civic_voting)
#' eoc <- civic_equalized_odds_curve(m, civic_voting, "voted", "gender", "yes")
#' civic_plot_roc_groups(eoc)
civic_equalized_odds_curve <- function(object, data, outcome, protected,
                                        positive  = NULL,
                                        thresholds = seq(0.05, 0.95, 0.05)) {
  .check_civic_model(object)
  if (object$type != "classification") {
    rlang::abort("Equalized odds curves require a classification model.")
  }

  y   <- factor(data[[outcome]])
  grp <- factor(data[[protected]])
  if (is.null(positive)) positive <- levels(y)[1]
  negative <- setdiff(levels(y), positive)[1]

  probs <- tryCatch(
    predict.civic_model(object, newdata = data, type = "prob"),
    error = function(e) rlang::abort(conditionMessage(e))
  )
  ppos <- if (is.matrix(probs)) {
    if (positive %in% colnames(probs)) probs[, positive] else probs[, ncol(probs)]
  } else as.numeric(probs)

  purrr::map_dfr(levels(grp), function(g) {
    idx <- grp == g
    y_g <- y[idx]; p_g <- ppos[idx]
    purrr::map_dfr(thresholds, function(thr) {
      yhat_g <- factor(ifelse(p_g >= thr, positive, negative), levels = levels(y))
      cm <- .confusion(y_g, yhat_g, positive)
      tibble::tibble(threshold = thr, group = g,
                     tpr = cm$tpr, fpr = cm$fpr, tnr = cm$tnr)
    })
  })
}

#' Plot group-level ROC curves (equalized odds visualisation)
#'
#' @param eoc_tbl A tibble from [civic_equalized_odds_curve()].
#' @param title Optional title.
#' @return A `ggplot2` object.
#' @export
civic_plot_roc_groups <- function(eoc_tbl, title = NULL) {
  ggplot2::ggplot(eoc_tbl, ggplot2::aes(
    x = fpr, y = tpr,
    colour = group, group = group
  )) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                         linetype = "dashed", colour = "grey70") +
    ggplot2::geom_path(linewidth = 1) +
    ggplot2::geom_point(size = 1.5, alpha = 0.6) +
    ggplot2::scale_colour_brewer(palette = "Set1", name = "Group") +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x       = "False Positive Rate",
      y       = "True Positive Rate",
      title   = title %||% "Group ROC Curves (Equalized Odds)",
      subtitle= "Perfectly equalized odds: all curves overlap",
      caption = "DataCitizen-Pro / civic.icarm"
    ) +
    .civic_theme()
}
