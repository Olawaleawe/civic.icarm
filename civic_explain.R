# ============================================================
# civic.icarm: Unified Explanation Engine
# ============================================================

#' Generate global and local model explanations
#'
#' @description
#' Creates a unified `civic_explainer` object containing:
#' \itemize{
#'   \item **Global importance** — variable importance from the model
#'     (tree impurity, absolute coefficients, or permutation-based via DALEX).
#'   \item **DALEX explainer** — if the `DALEX` package is available,
#'     a full DALEX explainer for downstream PDP/ICE and SHAP computation.
#' }
#'
#' This function implements the DataCitizen-Pro principle of
#' *transparent model explanation*: every explanation object carries the
#' method used, so learners understand *how* the explanation was produced.
#'
#' @param object A `civic_model` object.
#' @param data Data frame used for DALEX explainer (should be the full
#'   training or test set, not just model-frame head).
#' @param label Optional character label for the DALEX explainer.
#'
#' @return An object of class `civic_explainer` (a list) with:
#'   \item{importance}{Tibble of feature importances.}
#'   \item{importance_method}{Character: how importance was computed.}
#'   \item{dalex}{A DALEX explainer object (if DALEX available), else NULL.}
#'   \item{model}{The originating civic_model.}
#'
#' @export
#'
#' @examples
#' m  <- civic_fit_classification(voted ~ age + education, civic_voting)
#' ex <- civic_explain(m, data = civic_voting)
#' print(ex)
#' civic_plot_importance(ex)
civic_explain <- function(object, data = NULL, label = NULL) {
  .check_civic_model(object)

  fit    <- object$fit
  y_name <- all.vars(object$formula)[1]
  out    <- list(model = object, dalex = NULL, importance = NULL,
                 importance_method = "none")

  # ---- Global importance -------------------------------------------
  if (inherits(fit, "rpart")) {
    imp_raw <- fit$variable.importance
    if (!is.null(imp_raw) && length(imp_raw) > 0) {
      out$importance <- tibble::tibble(
        feature    = names(imp_raw),
        importance = as.numeric(imp_raw),
        importance_scaled = as.numeric(imp_raw) / max(as.numeric(imp_raw))
      ) |> dplyr::arrange(dplyr::desc(importance))
      out$importance_method <- "rpart_impurity"
    }

  } else if (inherits(fit, "glm")) {
    coefs <- stats::coef(fit)
    coefs <- coefs[names(coefs) != "(Intercept)"]
    out$importance <- tibble::tibble(
      feature    = names(coefs),
      importance = abs(coefs),
      coefficient = as.numeric(coefs),
      importance_scaled = abs(coefs) / max(abs(coefs) + 1e-12)
    ) |> dplyr::arrange(dplyr::desc(importance))
    out$importance_method <- "abs_coefficient"

  } else if (inherits(fit, "lm")) {
    coefs <- stats::coef(fit)
    coefs <- coefs[names(coefs) != "(Intercept)"]
    out$importance <- tibble::tibble(
      feature    = names(coefs),
      importance = abs(coefs),
      coefficient = as.numeric(coefs),
      importance_scaled = abs(coefs) / max(abs(coefs) + 1e-12)
    ) |> dplyr::arrange(dplyr::desc(importance))
    out$importance_method <- "abs_coefficient"

  } else if (requireNamespace("vip", quietly = TRUE)) {
    tryCatch({
      vi <- vip::vi(fit)
      out$importance <- tibble::tibble(
        feature    = vi$Variable,
        importance = vi$Importance,
        importance_scaled = vi$Importance / max(vi$Importance + 1e-12)
      ) |> dplyr::arrange(dplyr::desc(importance))
      out$importance_method <- "vip_permutation"
    }, error = function(e) NULL)
  }

  # ---- DALEX explainer --------------------------------------------
  if (!is.null(data) && requireNamespace("DALEX", quietly = TRUE)) {
    tryCatch({
      y_vec <- data[[y_name]]

      # Build a predict_function that always returns numeric probabilities
      predict_fn <- if (object$type == "classification") {
        function(m, newdata) {
          p <- predict.civic_model(object, newdata = newdata, type = "prob")
          if (is.matrix(p)) p[, ncol(p)] else as.numeric(p)
        }
      } else {
        function(m, newdata) {
          as.numeric(predict.civic_model(object, newdata = newdata))
        }
      }

      feat_data <- data[, setdiff(names(data), y_name), drop = FALSE]
      expl <- DALEX::explain(
        model            = object,
        data             = feat_data,
        y                = if (is.factor(y_vec)) as.numeric(y_vec) - 1 else as.numeric(y_vec),
        predict_function = predict_fn,
        label            = label %||% paste0("civic_", object$model),
        verbose          = FALSE
      )
      out$dalex <- expl
    }, error = function(e) {
      rlang::warn(c(
        "DALEX explainer could not be built.",
        i = conditionMessage(e)
      ))
    })
  }

  class(out) <- "civic_explainer"
  out
}

#' Print a civic_explainer
#' @export
print.civic_explainer <- function(x, ...) {
  cat(cli_rule("civic_explainer"), "\n")
  cat(sprintf("  Learner          : %s / %s\n",
              x$model$type, x$model$model))
  cat(sprintf("  Importance method: %s\n", x$importance_method))
  cat(sprintf("  DALEX available  : %s\n",
              if (!is.null(x$dalex)) "yes" else "no (install DALEX)"))
  if (!is.null(x$importance)) {
    cat("\n  Top features:\n")
    top <- utils::head(x$importance, 5)
    for (i in seq_len(nrow(top))) {
      bar <- paste(rep("|", round(top$importance_scaled[i] * 20)), collapse = "")
      cat(sprintf("    %-20s %s\n", top$feature[i], bar))
    }
  }
  invisible(x)
}

# ============================================================
#' Generate a local (instance-level) explanation
#'
#' @description
#' Produces a local explanation for one or more specific observations —
#' answering "why did the model make *this* prediction for *this person*?"
#' Uses DALEX/iml break-down plots if available; falls back to a
#' coefficient-weighted contribution table.
#'
#' @param explainer A `civic_explainer` object from [civic_explain()].
#' @param newdata A single-row (or multi-row) data.frame of observations to explain.
#' @param n_features Maximum number of features to include in breakdown.
#'
#' @return A list (one element per row in `newdata`), each a tibble of
#'   feature contributions.
#' @export
#'
#' @examples
#' m  <- civic_fit_classification(voted ~ age + education, civic_voting)
#' ex <- civic_explain(m, data = civic_voting)
#' local_ex <- civic_explain_local(ex, newdata = civic_voting[1, ])
civic_explain_local <- function(explainer, newdata, n_features = 10) {
  stopifnot(inherits(explainer, "civic_explainer"))

  if (!is.null(explainer$dalex) && requireNamespace("DALEX", quietly = TRUE)) {
    results <- lapply(seq_len(nrow(newdata)), function(i) {
      obs <- newdata[i, , drop = FALSE]
      bd  <- tryCatch(
        DALEX::predict_parts(explainer$dalex, new_observation = obs,
                             type = "break_down"),
        error = function(e) NULL
      )
      if (is.null(bd)) return(tibble::tibble())
      tibble::as_tibble(bd) |>
        dplyr::select(dplyr::any_of(c("variable", "contribution", "cumulative",
                                       "variable_name", "variable_value"))) |>
        utils::head(n_features)
    })
    return(results)
  }

  # Fallback: coefficient contributions for GLM / LM
  fit <- explainer$model$fit
  if (inherits(fit, c("glm", "lm"))) {
    results <- lapply(seq_len(nrow(newdata)), function(i) {
      obs  <- newdata[i, , drop = FALSE]
      mf   <- tryCatch(
        stats::model.matrix(explainer$model$formula, obs)[1, ],
        error = function(e) NULL
      )
      if (is.null(mf)) return(tibble::tibble())
      coefs <- stats::coef(fit)[names(mf)]
      tibble::tibble(
        variable     = names(mf),
        coefficient  = as.numeric(coefs),
        value        = as.numeric(mf),
        contribution = as.numeric(coefs) * as.numeric(mf)
      ) |>
        dplyr::filter(variable != "(Intercept)") |>
        dplyr::arrange(dplyr::desc(abs(contribution))) |>
        utils::head(n_features)
    })
    return(results)
  }

  rlang::warn("Local explanations require DALEX or a GLM/LM model.")
  list()
}
