# ============================================================
# civic.icarm: Interpretable Classification
# ============================================================

#' Train an interpretable classifier (ICARM-first)
#'
#' @description
#' Trains a classification model from the ICARM-approved family of
#' interpretable learners. Every model object records provenance metadata
#' (data hash, seed, parameters) to support downstream audit trails and
#' civic accountability reporting.
#'
#' Supports three model types:
#' \describe{
#'   \item{`"cart"`}{Classification and Regression Tree (rpart). Default.
#'     Inherently interpretable; visualisable as a decision tree.}
#'   \item{`"logistic"`}{Logistic regression via `stats::glm()`.
#'     Coefficient-level interpretability.}
#'   \item{`"logistic_l1"`}{L1-penalised logistic regression via `glmnet`.
#'     Sparse, auditable coefficient vector. Requires the `glmnet` package.}
#' }
#'
#' @param formula A model formula (e.g., `voted ~ age + education + income`).
#' @param data A data.frame or tibble containing training data.
#' @param model Character. One of `"cart"`, `"logistic"`, `"logistic_l1"`.
#' @param seed Integer seed for reproducibility (recorded in model object).
#' @param cart_control Optional [rpart::rpart.control()] list for CART tuning.
#' @param ... Additional arguments passed to the underlying model function.
#'
#' @return An object of class `civic_model` (a list) with elements:
#'   \item{fit}{The underlying fitted model object.}
#'   \item{type}{`"classification"`}
#'   \item{model}{Model type string.}
#'   \item{formula}{The model formula.}
#'   \item{levels}{Factor levels of the outcome.}
#'   \item{seed}{The seed used.}
#'   \item{data_hash}{SHA-256 digest of the training data.}
#'   \item{n_train}{Number of training rows.}
#'   \item{trained_at}{POSIXct timestamp.}
#'
#' @export
#'
#' @examples
#' # CART classifier on civic voting data
#' m <- civic_fit_classification(
#'   voted ~ age + education + political_interest,
#'   data  = civic_voting,
#'   model = "cart",
#'   seed  = 2025
#' )
#' print(m)
#'
#' # Logistic regression
#' m2 <- civic_fit_classification(
#'   voted ~ age + education + political_interest,
#'   data  = civic_voting,
#'   model = "logistic"
#' )
civic_fit_classification <- function(formula,
                                      data,
                                      model = c("cart", "logistic", "logistic_l1"),
                                      seed  = 2025,
                                      cart_control = NULL,
                                      ...) {
  model <- match.arg(model)
  set.seed(seed)

  # Validate outcome
  y_name <- all.vars(formula)[1]
  if (!is.factor(data[[y_name]])) {
    data[[y_name]] <- as.factor(data[[y_name]])
    rlang::inform(c(
      "!" = paste0("Outcome `", y_name, "` coerced to factor."),
      i  = "Set it explicitly to control level ordering."
    ))
  }
  lvls <- levels(data[[y_name]])

  fit <- switch(model,

    cart = {
      ctrl <- cart_control %||% rpart::rpart.control(cp = 0.01, minsplit = 20)
      rpart::rpart(formula, data = data, method = "class", control = ctrl, ...)
    },

    logistic = {
      stats::glm(formula, data = data, family = stats::binomial(), ...)
    },

    logistic_l1 = {
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        rlang::abort("Package `glmnet` is required for model = 'logistic_l1'. Install it with install.packages('glmnet').")
      }
      mf  <- stats::model.frame(formula, data = data)
      X   <- stats::model.matrix(formula, data = mf)[, -1, drop = FALSE]
      y   <- mf[[1]]
      glmnet::cv.glmnet(X, y, family = "binomial", alpha = 1, ...)
    }
  )

  structure(
    list(
      fit        = fit,
      type       = "classification",
      model      = model,
      formula    = formula,
      levels     = lvls,
      seed       = seed,
      data_hash  = digest::digest(data, algo = "sha256"),
      n_train    = nrow(data),
      trained_at = Sys.time(),
      call       = match.call()
    ),
    class = "civic_model"
  )
}

# ============================================================
# S3 methods for civic_model (classification & regression share these)
# ============================================================

#' Print a civic_model
#' @export
print.civic_model <- function(x, ...) {
  cat(cli_rule("civic_model"), "\n")
  cat(sprintf("  Type    : %s\n", x$type))
  cat(sprintf("  Learner : %s\n", x$model))
  cat(sprintf("  Formula : %s\n", deparse(x$formula)))
  cat(sprintf("  N train : %d\n", x$n_train))
  cat(sprintf("  Seed    : %d\n", x$seed))
  cat(sprintf("  Trained : %s\n", format(x$trained_at, "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("  Hash    : %s\n", substr(x$data_hash, 1, 16)))
  invisible(x)
}

#' @noRd
cli_rule <- function(label) {
  w <- min(getOption("width", 80), 80)
  dashes <- paste(rep("-", max(w - nchar(label) - 4, 4)), collapse = "")
  paste0("-- ", label, " ", dashes)
}

#' Summary of a civic_model
#' @export
summary.civic_model <- function(object, ...) {
  cat(sprintf("\ncivic_model [%s / %s]\n", object$type, object$model))
  cat(sprintf("Formula : %s\n\n", deparse(object$formula)))
  summary(object$fit, ...)
}

#' Predict from a civic_model
#'
#' @param object A `civic_model`.
#' @param newdata New data for prediction. If `NULL`, uses training-data head (for quick checks).
#' @param type For classification: `"class"` (default) returns factor labels;
#'   `"prob"` returns a matrix of class probabilities.
#' @param threshold Decision threshold for binary classification (default 0.5).
#' @param ... Ignored.
#'
#' @export
predict.civic_model <- function(object, newdata = NULL,
                                 type = c("class", "prob"),
                                 threshold = 0.5, ...) {
  type <- match.arg(type)
  .check_civic_model(object)

  if (object$type == "classification") {
    if (object$model == "cart") {
      probs <- stats::predict(object$fit, newdata = newdata, type = "prob")
      if (type == "prob") return(probs)
      pos   <- object$levels[1]
      factor(ifelse(probs[, pos] >= threshold, pos,
                    setdiff(object$levels, pos)[1]),
             levels = object$levels)

    } else if (object$model == "logistic") {
      p <- stats::predict(object$fit, newdata = newdata, type = "response")
      if (type == "prob") {
        neg <- object$levels[object$levels != object$levels[2]][1]
        return(cbind(stats::setNames(1 - p, neg), stats::setNames(p, object$levels[2])))
      }
      factor(ifelse(p >= threshold, object$levels[2], object$levels[1]),
             levels = object$levels)

    } else {  # logistic_l1
      if (!requireNamespace("glmnet", quietly = TRUE))
        rlang::abort("Package `glmnet` required.")
      mf <- stats::model.frame(object$formula, data = newdata)
      X  <- stats::model.matrix(object$formula, data = mf)[, -1, drop = FALSE]
      p  <- as.numeric(stats::predict(object$fit, newx = X, type = "response", s = "lambda.min"))
      if (type == "prob") return(cbind(`0` = 1 - p, `1` = p))
      factor(ifelse(p >= threshold, object$levels[2], object$levels[1]),
             levels = object$levels)
    }

  } else {
    # regression
    if (object$model == "gam") {
      if (!requireNamespace("mgcv", quietly = TRUE))
        rlang::abort("Package `mgcv` required for GAM prediction.")
    }
    stats::predict(object$fit, newdata = newdata)
  }
}
