# ============================================================
# civic.icarm: Fairness & Equity Module
# ============================================================

#' Compute group-level fairness metrics
#'
#' @description
#' Produces a comprehensive fairness report across levels of a protected
#' attribute, computing standard algorithmic fairness metrics. This function
#' embodies the **DataCitizen-Pro** democratic judgment competency: learners
#' engage with *why* different group-level outcomes matter, not just *that*
#' they differ.
#'
#' **Metrics computed (classification):**
#' \describe{
#'   \item{`n`}{Group size.}
#'   \item{`acc`}{Accuracy.}
#'   \item{`tpr`}{True positive rate (recall / sensitivity).}
#'   \item{`tnr`}{True negative rate (specificity).}
#'   \item{`fpr`}{False positive rate.}
#'   \item{`fnr`}{False negative rate.}
#'   \item{`ppv`}{Positive predictive value (precision).}
#'   \item{`rate_pos`}{Positive prediction rate (demographic parity denominator).}
#'   \item{`acc_gap`}{Accuracy gap vs. best-accuracy group.}
#'   \item{`tpr_gap`}{TPR gap (equal opportunity violation).}
#'   \item{`dp_ratio`}{Demographic parity ratio (disparate impact).}
#'   \item{`eo_gap`}{Equalized odds gap (max of |tpr_gap|, |fpr_gap|).}
#' }
#'
#' **Metrics computed (regression):**
#' \describe{
#'   \item{`n`, `mae`, `rmse`, `mae_gap`, `rmse_gap`}{}
#' }
#'
#' @param object A `civic_model`.
#' @param data A data.frame containing features, outcome, and protected column.
#' @param outcome Character. Name of the outcome column.
#' @param protected Character. Name of the protected attribute column.
#' @param positive Character. Positive class level (classification only).
#'   If `NULL`, uses the first factor level.
#' @param threshold Decision threshold (classification only, default 0.5).
#'
#' @return A tibble of class `civic_fairness` with one row per group.
#' @export
#'
#' @examples
#' m <- civic_fit_classification(voted ~ age + education + income, civic_voting)
#' fair <- civic_fairness_report(m, civic_voting,
#'                               outcome   = "voted",
#'                               protected = "gender",
#'                               positive  = "yes")
#' print(fair)
civic_fairness_report <- function(object, data, outcome, protected,
                                   positive = NULL, threshold = 0.5) {
  .check_civic_model(object)
  stopifnot(outcome   %in% names(data),
            protected %in% names(data))

  y   <- data[[outcome]]
  grp <- factor(data[[protected]])

  if (object$type == "classification") {
    y <- factor(y)
    if (is.null(positive)) positive <- levels(y)[1]
    negative <- setdiff(levels(y), positive)[1]

    # Get predicted probabilities
    probs <- tryCatch(
      predict.civic_model(object, newdata = data, type = "prob"),
      error = function(e) NULL
    )
    if (is.null(probs)) {
      rlang::abort("Could not compute predicted probabilities from this model.")
    }
    if (is.matrix(probs)) {
      if (positive %in% colnames(probs)) {
        ppos <- probs[, positive]
      } else {
        ppos <- probs[, ncol(probs)]
      }
    } else {
      ppos <- as.numeric(probs)
    }

    y_hat <- factor(ifelse(ppos >= threshold, positive, negative),
                    levels = levels(y))

    # Group metrics
    tab <- dplyr::group_by(
      tibble::tibble(grp = grp, y = y, y_hat = y_hat, ppos = ppos),
      grp
    ) |>
      dplyr::summarise(
        n        = dplyr::n(),
        acc      = mean(y_hat == y),
        tpr      = mean(y_hat == positive & y == positive) /
          max(mean(y == positive), 1e-9),
        tnr      = mean(y_hat == negative & y == negative) /
          max(mean(y == negative), 1e-9),
        fpr      = mean(y_hat == positive & y == negative) /
          max(mean(y == negative), 1e-9),
        fnr      = mean(y_hat == negative & y == positive) /
          max(mean(y == positive), 1e-9),
        ppv      = mean(y == positive & y_hat == positive) /
          max(mean(y_hat == positive), 1e-9),
        rate_pos = mean(y_hat == positive),
        mean_prob= mean(ppos),
        .groups  = "drop"
      )

    # Disparities vs reference (best accuracy group)
    ref_row <- tab[which.max(tab$acc), ]
    ref_grp <- ref_row$grp

    tab <- tab |>
      dplyr::mutate(
        acc_gap  = acc - ref_row$acc,
        tpr_gap  = tpr - ref_row$tpr,
        fpr_gap  = fpr - ref_row$fpr,
        dp_ratio = rate_pos / max(ref_row$rate_pos, 1e-9),  # disparate impact
        eo_gap   = pmax(abs(tpr_gap), abs(fpr_gap)),         # equalized odds
        reference_group = as.character(ref_grp)
      )

    attr(tab, "positive")   <- positive
    attr(tab, "threshold")  <- threshold
    attr(tab, "protected")  <- protected
    attr(tab, "outcome")    <- outcome

  } else {
    # Regression fairness
    preds <- as.numeric(predict.civic_model(object, newdata = data))
    err   <- abs(as.numeric(y) - preds)
    se    <- (as.numeric(y) - preds)^2

    tab <- dplyr::group_by(
      tibble::tibble(grp = grp, err = err, se = se),
      grp
    ) |>
      dplyr::summarise(
        n    = dplyr::n(),
        mae  = mean(err),
        rmse = sqrt(mean(se)),
        .groups = "drop"
      )

    ref_row <- tab[which.min(tab$mae), ]
    tab <- tab |>
      dplyr::mutate(
        mae_gap  = mae  - ref_row$mae,
        rmse_gap = rmse - ref_row$rmse,
        reference_group = as.character(ref_row$grp)
      )
  }

  class(tab) <- c("civic_fairness", class(tab))
  tab
}

#' Print a civic_fairness report
#' @export
print.civic_fairness <- function(x, ...) {
  cat(cli_rule("civic_fairness report"), "\n")
  protected <- attr(x, "protected") %||% "protected"
  outcome   <- attr(x, "outcome")   %||% "outcome"
  cat(sprintf("  Protected: %s  |  Outcome: %s\n\n", protected, outcome))
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

# ============================================================
#' Compute a civic equity summary (DataCitizen-Pro scorecard component)
#'
#' @description
#' Condenses a `civic_fairness` report into a one-row summary of key
#' equity indicators. Used internally by [civic_scorecard()].
#'
#' @param fairness A `civic_fairness` tibble from [civic_fairness_report()].
#'
#' @return A named list of scalar equity indicators.
#' @export
civic_equity_summary <- function(fairness) {
  stopifnot(inherits(fairness, "civic_fairness"))

  if ("dp_ratio" %in% names(fairness)) {
    # classification
    list(
      max_acc_gap   = max(abs(fairness$acc_gap)),
      max_tpr_gap   = max(abs(fairness$tpr_gap)),
      max_fpr_gap   = max(abs(fairness$fpr_gap)),
      min_dp_ratio  = min(fairness$dp_ratio),
      max_eo_gap    = max(fairness$eo_gap),
      n_groups      = nrow(fairness),
      disparate_impact_pass = all(fairness$dp_ratio >= 0.8),
      equal_opp_pass        = max(abs(fairness$tpr_gap)) < 0.1
    )
  } else {
    list(
      max_mae_gap  = max(abs(fairness$mae_gap)),
      max_rmse_gap = max(abs(fairness$rmse_gap)),
      n_groups     = nrow(fairness)
    )
  }
}
