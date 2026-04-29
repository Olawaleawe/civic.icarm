# ============================================================
# civic.icarm: Audit Trail & Civic Scorecard
# ============================================================

#' Generate a reproducible audit trail for a civic_model
#'
#' @description
#' Produces a structured, machine-readable audit record for a `civic_model`,
#' capturing all provenance metadata required for civic accountability:
#' who built the model, with what data (hashed), which algorithm, what seed,
#' and any analyst notes.
#'
#' The audit trail connects to the **DataCitizen-Pro** framework for
#' democratic accountability of algorithmic systems: learners are asked to
#' treat audit trails as *public documents*, not internal memos.
#'
#' @param object A `civic_model`.
#' @param metrics Optional named numeric vector of evaluation metrics
#'   (e.g., from [civic_metrics()]).
#' @param fairness Optional `civic_fairness` tibble from [civic_fairness_report()].
#' @param notes Optional character string of analyst notes.
#' @param analyst Optional character name of the analyst (for accountability).
#' @param path Optional file path to write the JSON audit file.
#'   If `NULL`, returns the JSON string only.
#' @param format One of `"json"` (default) or `"list"`.
#'
#' @return Invisibly, the JSON string (if `format = "json"`) or a list.
#'   If `path` is provided, also writes the file.
#' @export
#'
#' @examples
#' m     <- civic_fit_classification(voted ~ age + education, civic_voting)
#' trail <- civic_audit_trail(m, notes = "Baseline CART model", analyst = "O.O. Awe")
#' cat(trail)
civic_audit_trail <- function(object,
                               metrics  = NULL,
                               fairness = NULL,
                               notes    = NULL,
                               analyst  = NULL,
                               path     = NULL,
                               format   = c("json", "list")) {
  .check_civic_model(object)
  format <- match.arg(format)

  equity_summary <- if (!is.null(fairness) && inherits(fairness, "civic_fairness")) {
    tryCatch(civic_equity_summary(fairness), error = function(e) NULL)
  } else NULL

  meta <- list(
    civic_icarm_version = utils::packageVersion("civic.icarm") |>
      as.character() |>
      tryCatch(error = function(e) "dev"),
    timestamp    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    analyst      = analyst %||% Sys.info()[["user"]],
    model_type   = object$type,
    learner      = object$model,
    formula      = deparse(object$formula),
    seed         = object$seed,
    n_train      = object$n_train,
    data_hash    = object$data_hash,
    trained_at   = format(object$trained_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    metrics      = if (!is.null(metrics)) as.list(metrics) else NULL,
    equity       = equity_summary,
    notes        = notes,
    datacitizenpro_pillars = list(
      data_literacy       = "Model built with ICARM-approved interpretable learner",
      statistical_reasoning = paste0(
        "Seed = ", object$seed, "; n_train = ", object$n_train,
        "; data hash recorded for reproducibility"
      ),
      democratic_judgment = if (!is.null(equity_summary)) {
        paste0("Fairness audit completed. Disparate impact pass: ",
               equity_summary$disparate_impact_pass %||% "N/A")
      } else {
        "No fairness audit attached — consider adding one."
      }
    )
  )

  if (format == "list") return(meta)

  json <- jsonlite::toJSON(meta, pretty = TRUE, auto_unbox = TRUE, null = "null")
  if (!is.null(path)) {
    writeLines(json, path)
    rlang::inform(paste0("Audit trail written to: ", path))
  }
  invisible(json)
}

# ============================================================
#' Generate a civic accountability scorecard
#'
#' @description
#' Produces a comprehensive, human-readable civic scorecard that synthesises
#' model interpretability, predictive performance, and equity metrics into
#' a single structured report. Designed for the DataCitizen-Pro teaching
#' context: learners generate a scorecard as a *democratic accountability
#' artefact* — the kind of document a civic institution would publish when
#' deploying an algorithmic decision-making system.
#'
#' The scorecard is returned as a formatted character vector (printable) and
#' optionally written to a JSON file.
#'
#' @param object A `civic_model`.
#' @param test_data A data.frame of held-out test data.
#' @param outcome Character. Name of the outcome column.
#' @param protected Character. Name of the protected attribute column (optional).
#' @param positive Positive class level (classification).
#' @param analyst Optional analyst name.
#' @param project Optional project name (e.g., "DataCitizen-Pro pilot").
#' @param path Optional path to write JSON scorecard.
#'
#' @return Invisibly, a named list representing the full scorecard.
#' @export
#'
#' @examples
#' splits <- civic_split(civic_voting, stratify = "voted")
#' m      <- civic_fit_classification(voted ~ age + education, splits$train)
#' civic_scorecard(m, splits$test, outcome = "voted",
#'                 protected = "gender", positive = "yes",
#'                 project   = "DataCitizen-Pro Pilot Study")
civic_scorecard <- function(object,
                             test_data,
                             outcome,
                             protected = NULL,
                             positive  = NULL,
                             analyst   = NULL,
                             project   = "civic.icarm",
                             path      = NULL) {
  .check_civic_model(object)

  y_true <- test_data[[outcome]]

  # Performance metrics
  if (object$type == "classification") {
    y_prob <- tryCatch({
      p <- predict.civic_model(object, newdata = test_data, type = "prob")
      if (is.matrix(p)) p[, ncol(p)] else as.numeric(p)
    }, error = function(e) NULL)

    y_hat  <- predict.civic_model(object, newdata = test_data, type = "class")
    perf   <- civic_metrics(y_true, y_hat, y_prob = y_prob, positive = positive)

  } else {
    y_hat  <- predict.civic_model(object, newdata = test_data)
    perf   <- civic_metrics(y_true, y_hat, type = "regression")
  }

  # Fairness
  fairness_tab <- NULL
  equity_sum   <- NULL
  if (!is.null(protected)) {
    tryCatch({
      fairness_tab <- civic_fairness_report(
        object, test_data, outcome = outcome,
        protected = protected, positive = positive
      )
      equity_sum <- civic_equity_summary(fairness_tab)
    }, error = function(e) {
      rlang::warn(paste("Fairness computation failed:", conditionMessage(e)))
    })
  }

  # Interpretability score (heuristic: higher for simpler models)
  interpretability <- switch(object$model,
    cart       = "HIGH   (decision tree — fully inspectable)",
    logistic   = "HIGH   (logistic regression — coefficients readable)",
    logistic_l1= "HIGH   (sparse logistic — fewer coefficients)",
    linear     = "HIGH   (linear regression — coefficients readable)",
    gam        = "MEDIUM (GAM — smooth terms require marginal effect plots)",
    "UNKNOWN"
  )

  scorecard <- list(
    project         = project,
    analyst         = analyst %||% Sys.info()[["user"]],
    generated_at    = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
    model           = list(
      type    = object$type,
      learner = object$model,
      formula = deparse(object$formula),
      n_train = object$n_train,
      seed    = object$seed,
      data_hash = object$data_hash
    ),
    interpretability = interpretability,
    performance      = as.list(round(perf, 4)),
    n_test           = nrow(test_data),
    equity           = equity_sum,
    datacitizenpro   = list(
      data_literacy    = "Interpretable learner; full provenance recorded.",
      stat_reasoning   = paste0("n_test = ", nrow(test_data),
                                "; metrics: ", paste(names(perf), collapse = ", ")),
      democratic_judgment = if (!is.null(equity_sum)) {
        dip <- equity_sum$disparate_impact_pass
        eop <- equity_sum$equal_opp_pass
        paste0("Disparate impact (80% rule): ", if (isTRUE(dip)) "PASS" else "FAIL",
               " | Equal opportunity: ", if (isTRUE(eop)) "PASS" else "FAIL")
      } else "No protected attribute specified — fairness audit not conducted."
    )
  )

  # Pretty print
  cat(cli_rule(paste0("CIVIC SCORECARD — ", toupper(project))), "\n\n")
  cat(sprintf("  Analyst  : %s\n", scorecard$analyst))
  cat(sprintf("  Generated: %s\n", scorecard$generated_at))
  cat(sprintf("  Model    : %s / %s\n", object$type, object$model))
  cat(sprintf("  Formula  : %s\n", deparse(object$formula)))
  cat(sprintf("  N train / N test: %d / %d\n", object$n_train, nrow(test_data)))
  cat("\n  [Interpretability]\n")
  cat(sprintf("    %s\n", interpretability))
  cat("\n  [Performance — test set]\n")
  for (nm in names(perf)) cat(sprintf("    %-20s: %.4f\n", nm, perf[nm]))
  if (!is.null(equity_sum)) {
    cat("\n  [Equity — DataCitizen-Pro democratic judgment]\n")
    cat(sprintf("    %s\n", scorecard$datacitizenpro$democratic_judgment))
    for (nm in names(equity_sum)) {
      val <- equity_sum[[nm]]
      cat(sprintf("    %-30s: %s\n", nm,
                  if (is.logical(val)) ifelse(val, "PASS", "FAIL")
                  else round(as.numeric(val), 4)))
    }
  }
  cat("\n", cli_rule("end scorecard"), "\n")

  if (!is.null(path)) {
    json <- jsonlite::toJSON(scorecard, pretty = TRUE, auto_unbox = TRUE, null = "null")
    writeLines(json, path)
    rlang::inform(paste0("Scorecard written to: ", path))
  }

  invisible(scorecard)
}
