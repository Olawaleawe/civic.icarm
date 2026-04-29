# ============================================================
# civic.icarm: Model Comparison
# ============================================================

#' Compare multiple civic_models on a held-out test set
#'
#' @description
#' Evaluates a list of `civic_model` objects on a common test set, returning
#' a tidy comparison table of performance, fairness, and interpretability
#' ratings. Designed as a DataCitizen-Pro deliberation tool: learners
#' argue which model should be *deployed* given the joint tradeoffs —
#' not simply which model is most accurate.
#'
#' @param models A **named** list of `civic_model` objects.
#'   Names become row labels in the output.
#' @param test_data A data.frame used for all evaluations.
#' @param outcome Character. Name of the outcome column.
#' @param protected Character. Name of the protected attribute column.
#'   Pass `NULL` to skip fairness metrics.
#' @param positive Positive class level (classification).
#' @param threshold Decision threshold (default 0.5).
#'
#' @return An object of class `civic_comparison` — a tibble with one row
#'   per model and columns for:
#'   \item{model_name}{User-supplied name.}
#'   \item{learner}{Learner type (cart, logistic, etc.).}
#'   \item{interpretability}{Categorical rating: HIGH / MEDIUM.}
#'   \item{Performance metrics}{accuracy, f1, auc (classification) or
#'     mae, rmse, r2 (regression).}
#'   \item{Fairness metrics}{max_acc_gap, max_tpr_gap, dp_ratio_min,
#'     disparate_impact_pass, equal_opp_pass (if protected supplied).}
#'   \item{n_train}{Training set size from each model.}
#'
#' @export
#'
#' @examples
#' splits <- civic_split(civic_voting, stratify = "voted")
#'
#' m_cart  <- civic_fit_classification(voted ~ age + education, splits$train,
#'                                     model = "cart")
#' m_logit <- civic_fit_classification(voted ~ age + education, splits$train,
#'                                     model = "logistic")
#'
#' cmp <- civic_compare(
#'   models    = list(CART = m_cart, Logistic = m_logit),
#'   test_data = splits$test,
#'   outcome   = "voted",
#'   protected = "gender",
#'   positive  = "yes"
#' )
#' print(cmp)
#' civic_plot_comparison(cmp)
civic_compare <- function(models,
                           test_data,
                           outcome,
                           protected = NULL,
                           positive  = NULL,
                           threshold = 0.5) {
  if (!is.list(models) || is.null(names(models)) ||
      any(nchar(names(models)) == 0)) {
    rlang::abort(
      c("'models' must be a *named* list of civic_model objects.",
        i = "Example: list(CART = m1, Logistic = m2)")
    )
  }

  purrr::imap_dfr(models, function(obj, nm) {
    .check_civic_model(obj)

    y_true <- test_data[[outcome]]

    # ── Performance ──────────────────────────────────────
    if (obj$type == "classification") {
      y_prob <- tryCatch({
        p <- predict.civic_model(obj, newdata = test_data,
                                  type = "prob", threshold = threshold)
        if (is.matrix(p)) p[, ncol(p)] else as.numeric(p)
      }, error = function(e) NULL)

      y_hat <- tryCatch(
        predict.civic_model(obj, newdata = test_data,
                             type = "class", threshold = threshold),
        error = function(e) factor(rep(NA, nrow(test_data)))
      )

      perf <- civic_metrics(y_true, y_hat, y_prob = y_prob,
                            positive = positive)
      perf_row <- tibble::tibble(
        accuracy     = perf["accuracy"],
        balanced_acc = perf["balanced_acc"],
        f1           = perf["f1"],
        precision    = perf["precision"],
        recall       = perf["recall"],
        auc          = if ("auc" %in% names(perf)) perf["auc"] else NA_real_
      )

    } else {
      y_hat <- tryCatch(
        predict.civic_model(obj, newdata = test_data),
        error = function(e) rep(NA_real_, nrow(test_data))
      )
      perf <- civic_metrics(y_true, y_hat, type = "regression")
      perf_row <- tibble::tibble(
        mae  = perf["mae"],
        rmse = perf["rmse"],
        r2   = perf["r2"],
        accuracy = NA_real_, balanced_acc = NA_real_,
        f1 = NA_real_, precision = NA_real_,
        recall = NA_real_, auc = NA_real_
      )
    }

    # ── Fairness ─────────────────────────────────────────
    fair_row <- tibble::tibble(
      max_acc_gap   = NA_real_,
      max_tpr_gap   = NA_real_,
      max_fpr_gap   = NA_real_,
      min_dp_ratio  = NA_real_,
      max_eo_gap    = NA_real_,
      di_pass       = NA,
      eo_pass       = NA
    )

    if (!is.null(protected)) {
      tryCatch({
        fair <- civic_fairness_report(
          obj, test_data,
          outcome   = outcome,
          protected = protected,
          positive  = positive,
          threshold = threshold
        )
        eq <- civic_equity_summary(fair)
        fair_row <- tibble::tibble(
          max_acc_gap  = eq$max_acc_gap  %||% NA_real_,
          max_tpr_gap  = eq$max_tpr_gap  %||% NA_real_,
          max_fpr_gap  = eq$max_fpr_gap  %||% NA_real_,
          min_dp_ratio = eq$min_dp_ratio %||% NA_real_,
          max_eo_gap   = eq$max_eo_gap   %||% NA_real_,
          di_pass      = eq$disparate_impact_pass %||% NA,
          eo_pass      = eq$equal_opp_pass        %||% NA
        )
      }, error = function(e) {
        rlang::warn(paste0("Fairness failed for '", nm, "': ", conditionMessage(e)))
      })
    }

    # ── Interpretability ─────────────────────────────────
    interp <- switch(obj$model,
      cart        = "HIGH",
      logistic    = "HIGH",
      logistic_l1 = "HIGH",
      linear      = "HIGH",
      gam         = "MEDIUM",
      "UNKNOWN"
    )

    dplyr::bind_cols(
      tibble::tibble(
        model_name        = nm,
        learner           = obj$model,
        interpretability  = interp,
        n_train           = obj$n_train
      ),
      perf_row,
      fair_row
    )
  })  |>
    (\(x) { class(x) <- c("civic_comparison", class(x)); x })()
}

#' Print a civic_comparison
#' @export
print.civic_comparison <- function(x, digits = 3, ...) {
  cat(cli_rule("civic_comparison"), "\n\n")
  num_cols <- names(x)[sapply(x, is.numeric)]
  x_print  <- x
  x_print[num_cols] <- lapply(x_print[num_cols], round, digits = digits)
  print(tibble::as_tibble(x_print), n = Inf, ...)
  invisible(x)
}

# ============================================================
#' Plot a multi-model comparison
#'
#' @description
#' Visualises model comparison results from [civic_compare()] as a
#' faceted bar chart. Each facet shows one metric; models are coloured
#' by interpretability rating.
#'
#' For the DataCitizen-Pro classroom: learners use this plot to argue
#' which model to deploy — balancing accuracy, fairness gaps, and
#' interpretability.
#'
#' @param comparison A `civic_comparison` tibble from [civic_compare()].
#' @param metrics Character vector of metric columns to plot.
#' @param title Optional plot title.
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#' # See civic_compare() examples
civic_plot_comparison <- function(comparison,
                                   metrics = c("accuracy", "f1",
                                               "max_tpr_gap", "min_dp_ratio"),
                                   title   = NULL) {
  stopifnot(inherits(comparison, "civic_comparison"))

  # Keep only metrics that exist and are numeric
  available <- intersect(metrics, names(comparison))
  available <- available[sapply(comparison[available], is.numeric)]

  if (length(available) == 0) {
    rlang::abort("None of the requested metrics found in the comparison table.")
  }

  long <- tidyr::pivot_longer(
    comparison[, c("model_name", "interpretability", available)],
    cols      = dplyr::all_of(available),
    names_to  = "metric",
    values_to = "value"
  ) |> dplyr::filter(!is.na(value))

  # Human-readable metric labels
  metric_labels <- c(
    accuracy     = "Accuracy",
    balanced_acc = "Balanced Accuracy",
    f1           = "F1 Score",
    precision    = "Precision",
    recall       = "Recall (TPR)",
    auc          = "AUC",
    mae          = "MAE",
    rmse         = "RMSE",
    r2           = "R\u00B2",
    max_tpr_gap  = "Max TPR Gap (fairness)",
    max_fpr_gap  = "Max FPR Gap (fairness)",
    min_dp_ratio = "Min Disparate Impact Ratio",
    max_eo_gap   = "Max Equalized Odds Gap",
    max_acc_gap  = "Max Accuracy Gap"
  )
  long$metric_label <- dplyr::recode(long$metric, !!!metric_labels)

  pal <- c(HIGH = .civic_palette["primary"],
           MEDIUM = .civic_palette["secondary"],
           UNKNOWN = .civic_palette["neutral"])

  ggplot2::ggplot(long, ggplot2::aes(
    x    = model_name,
    y    = value,
    fill = interpretability
  )) +
    ggplot2::geom_col(width = 0.65, show.legend = TRUE) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(value, 3)),
      vjust = -0.4, size = 3,
      colour = .civic_palette["neutral"]
    ) +
    ggplot2::facet_wrap(~ metric_label, scales = "free_y") +
    ggplot2::scale_fill_manual(values = pal, name = "Interpretability") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(
      x       = NULL,
      y       = NULL,
      title   = title %||% "Model Comparison — Performance & Fairness",
      subtitle= "Colour = interpretability rating | DataCitizen-Pro framework",
      caption = "civic.icarm | Deliberate about accuracy vs fairness tradeoffs"
    ) +
    .civic_theme() +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 20, hjust = 1),
      legend.position = "top"
    )
}
