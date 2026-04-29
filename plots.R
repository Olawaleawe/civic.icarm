# ============================================================
# civic.icarm: Visualisation Layer
# All plots return ggplot2 objects (modifiable with + theme())
# ============================================================

#' Plot feature importance
#'
#' @description
#' Horizontal bar chart of global feature importances from a
#' `civic_explainer`. The chart uses the DataCitizen-Pro colour palette
#' and is designed to be interpretable by non-specialist learners.
#'
#' @param explainer A `civic_explainer` from [civic_explain()].
#' @param n_features Maximum features to display (default 15).
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' m  <- civic_fit_classification(voted ~ age + education, civic_voting)
#' ex <- civic_explain(m, data = civic_voting)
#' civic_plot_importance(ex)
civic_plot_importance <- function(explainer, n_features = 15, title = NULL) {
  stopifnot(inherits(explainer, "civic_explainer"))
  if (is.null(explainer$importance) || nrow(explainer$importance) == 0) {
    rlang::abort("No importance data in this explainer. Check the model type.")
  }

  df <- utils::head(
    dplyr::arrange(explainer$importance, dplyr::desc(importance)),
    n_features
  )
  df$feature <- factor(df$feature, levels = rev(df$feature))

  method_label <- switch(explainer$importance_method,
    rpart_impurity  = "Tree impurity (Gini)",
    abs_coefficient = "Absolute coefficient value",
    vip_permutation = "Permutation importance (vip)",
    explainer$importance_method
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = importance,
    y    = feature,
    fill = importance_scaled
  )) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.7) +
    ggplot2::scale_fill_gradient(
      low  = "#AED6F1",
      high = .civic_palette["primary"]
    ) +
    ggplot2::labs(
      x       = method_label,
      y       = NULL,
      title   = title %||% "Feature Importance (ICARM)",
      caption = paste0(
        "Model: ", explainer$model$model,
        " | DataCitizen-Pro / civic.icarm"
      )
    ) +
    .civic_theme()

  # Add value labels
  p + ggplot2::geom_text(
    ggplot2::aes(label = round(importance, 3)),
    hjust = -0.1, size = 3,
    colour = .civic_palette["neutral"]
  ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
}

# ============================================================
#' Plot fairness disparity bars
#'
#' @description
#' Visualises group-level fairness metrics from a `civic_fairness` report.
#' Designed for the DataCitizen-Pro classroom: learners identify which groups
#' face accuracy or opportunity gaps, and discuss the democratic implications.
#'
#' @param fairness A `civic_fairness` tibble from [civic_fairness_report()].
#' @param metric One of `"acc"`, `"tpr"`, `"fpr"`, `"rate_pos"`, `"dp_ratio"`,
#'   `"eo_gap"`, `"mae"`, `"rmse"`.
#' @param title Optional plot title.
#' @param ref_line Numeric. Draw a horizontal reference line (e.g., 0.8 for
#'   the 80% disparate impact rule). Default `NULL`.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' m    <- civic_fit_classification(voted ~ age + education, civic_voting)
#' fair <- civic_fairness_report(m, civic_voting, "voted", "gender", "yes")
#' civic_plot_fairness(fair, metric = "tpr")
civic_plot_fairness <- function(fairness,
                                 metric   = "acc",
                                 title    = NULL,
                                 ref_line = NULL) {
  stopifnot(inherits(fairness, "civic_fairness"))
  if (!metric %in% names(fairness)) {
    rlang::abort(paste0(
      "Metric '", metric, "' not found. Available: ",
      paste(names(fairness), collapse = ", ")
    ))
  }

  protected <- attr(fairness, "protected") %||% "group"
  df <- fairness
  df$value <- df[[metric]]

  metric_labels <- c(
    acc      = "Accuracy",
    tpr      = "True Positive Rate (Equal Opportunity)",
    tnr      = "True Negative Rate (Specificity)",
    fpr      = "False Positive Rate",
    fnr      = "False Negative Rate",
    ppv      = "Precision (PPV)",
    rate_pos = "Positive Prediction Rate (Demographic Parity)",
    dp_ratio = "Disparate Impact Ratio",
    eo_gap   = "Equalized Odds Gap",
    mae      = "Mean Absolute Error",
    rmse     = "Root Mean Squared Error"
  )
  y_label <- metric_labels[metric] %||% metric

  # Colour by gap: gap columns are green near zero, red at large values
  gap_metrics <- c("acc_gap", "tpr_gap", "fpr_gap", "mae_gap", "rmse_gap", "eo_gap")
  is_gap <- metric %in% gap_metrics

  fill_col <- if (is_gap) {
    ifelse(abs(df$value) < 0.05, .civic_palette["fair"], .civic_palette["unfair"])
  } else {
    rep(.civic_palette["secondary"], nrow(df))
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = grp, y = value)) +
    ggplot2::geom_col(fill = fill_col, width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(value, 3)),
      vjust = -0.4, size = 3.5,
      colour = .civic_palette["primary"]
    ) +
    ggplot2::labs(
      x       = protected,
      y       = y_label,
      title   = title %||% paste0("Fairness Report: ", y_label),
      subtitle= paste0("Protected attribute: ", protected,
                       " | Positive class: ",
                       attr(fairness, "positive") %||% ""),
      caption = "DataCitizen-Pro / civic.icarm"
    ) +
    .civic_theme()

  if (!is.null(ref_line)) {
    p <- p + ggplot2::geom_hline(
      yintercept = ref_line,
      linetype   = "dashed",
      colour     = .civic_palette["accent"],
      linewidth  = 0.8
    ) +
      ggplot2::annotate("text", x = Inf, y = ref_line,
                        label = paste0("ref = ", ref_line),
                        hjust = 1.1, vjust = -0.4, size = 3,
                        colour = .civic_palette["accent"])
  }
  p
}

# ============================================================
#' Plot a confusion matrix
#'
#' @description
#' Tile-based confusion matrix heatmap.
#'
#' @param y_true Factor of true outcomes.
#' @param y_pred Factor of predicted outcomes.
#' @param title Optional title.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' y <- factor(c("yes","no","yes","yes","no","no","yes"))
#' p <- factor(c("yes","no","no","yes","no","yes","yes"))
#' civic_plot_confusion(y, p)
civic_plot_confusion <- function(y_true, y_pred, title = NULL) {
  y_true <- factor(y_true)
  y_pred <- factor(y_pred, levels = levels(y_true))

  df <- as.data.frame(table(Predicted = y_pred, Actual = y_true))
  df$Correct <- df$Predicted == df$Actual

  ggplot2::ggplot(df, ggplot2::aes(x = Actual, y = Predicted, fill = Freq)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = Freq),
                       size = 5, fontface = "bold",
                       colour = "white") +
    ggplot2::scale_fill_gradient(
      low  = "#AED6F1",
      high = .civic_palette["primary"],
      name = "Count"
    ) +
    ggplot2::labs(
      title   = title %||% "Confusion Matrix",
      caption = "DataCitizen-Pro / civic.icarm"
    ) +
    .civic_theme() +
    ggplot2::theme(
      legend.position = "right",
      axis.text       = ggplot2::element_text(size = 11)
    )
}

# ============================================================
#' Plot partial dependence profile (PDP / ICE)
#'
#' @description
#' Marginal effect of a single feature on model predictions, averaged
#' over the data distribution (PDP) and optionally with individual
#' conditional expectation curves (ICE). Requires the `DALEX` and
#' `ingredients` packages.
#'
#' @param explainer A `civic_explainer` with a DALEX explainer attached.
#' @param variable Character. Name of the variable to profile.
#' @param type `"partial"` (PDP, default) or `"conditional"` (ICE).
#' @param n_sample Number of observations to sample for ICE (default 100).
#' @param title Optional title.
#'
#' @return A `ggplot2` object or NULL if DALEX is unavailable.
#' @export
civic_plot_pdp <- function(explainer, variable, type = "partial",
                            n_sample = 100, title = NULL) {
  if (is.null(explainer$dalex)) {
    rlang::abort("PDP requires a DALEX explainer. Pass `data` to civic_explain().")
  }
  if (!requireNamespace("DALEX", quietly = TRUE)) {
    rlang::abort("Package `DALEX` required. Install with install.packages('DALEX').")
  }

  pdp <- DALEX::model_profile(
    explainer$dalex,
    variables = variable,
    type      = type,
    N         = n_sample
  )

  df <- as.data.frame(pdp$agr_profiles)
  if (nrow(df) == 0) {
    rlang::abort("No profile data returned. Check the variable name.")
  }

  var_col   <- "_x_"
  pred_col  <- "_yhat_"
  if (!all(c(var_col, pred_col) %in% names(df))) {
    rlang::warn("Unexpected DALEX column names; plotting raw data frame.")
    return(plot(pdp))
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data[[var_col]],
                                    y = .data[[pred_col]])) +
    ggplot2::geom_line(colour = .civic_palette["primary"],
                       linewidth = 1.2) +
    ggplot2::geom_rug(sides = "b", alpha = 0.3,
                      colour = .civic_palette["neutral"]) +
    ggplot2::labs(
      x       = variable,
      y       = "Average predicted response",
      title   = title %||% paste0("Partial Dependence: ", variable),
      caption = "DataCitizen-Pro / civic.icarm"
    ) +
    .civic_theme()
}

# ============================================================
#' Plot threshold performance curves
#'
#' @description
#' Visualises accuracy, recall, precision, and positive rate across
#' classification thresholds — a key DataCitizen-Pro teaching tool for
#' understanding the tradeoffs behind algorithmic decision boundaries.
#'
#' @param thresholds_tbl A tibble from [civic_thresholds()].
#' @param metrics Character vector of metrics to plot.
#' @param title Optional title.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' y <- factor(sample(c("yes","no"), 300, replace = TRUE))
#' p <- runif(300)
#' thr_tbl <- civic_thresholds(y, p, positive = "yes")
#' civic_plot_thresholds(thr_tbl)
civic_plot_thresholds <- function(thresholds_tbl,
                                   metrics = c("accuracy", "recall",
                                               "precision", "rate_positive"),
                                   title   = NULL) {
  long <- tidyr::pivot_longer(
    thresholds_tbl,
    cols      = dplyr::any_of(metrics),
    names_to  = "metric",
    values_to = "value"
  )

  pal <- c(
    accuracy      = .civic_palette["primary"],
    recall        = .civic_palette["fair"],
    precision     = .civic_palette["accent"],
    rate_positive = .civic_palette["secondary"],
    specificity   = .civic_palette["unfair"],
    f1            = .civic_palette["neutral"]
  )

  ggplot2::ggplot(long, ggplot2::aes(x = threshold, y = value,
                                      colour = metric)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = "dashed",
                        colour = "grey60") +
    ggplot2::scale_colour_manual(
      values = pal[intersect(names(pal), unique(long$metric))],
      name   = "Metric"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0.1, 0.9, 0.1)) +
    ggplot2::labs(
      x       = "Classification threshold",
      y       = "Metric value",
      title   = title %||% "Performance vs. Decision Threshold",
      subtitle= "Dashed line = default threshold (0.5)",
      caption = "DataCitizen-Pro / civic.icarm"
    ) +
    .civic_theme()
}
