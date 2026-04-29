# ============================================================
# civic.icarm: Interpretable Regression
# ============================================================

#' Train an interpretable regressor (ICARM-first)
#'
#' @description
#' Trains a regression model from the ICARM-approved family of interpretable
#' learners. Identical provenance metadata to [civic_fit_classification()].
#'
#' Supports three model types:
#' \describe{
#'   \item{`"cart"`}{Regression tree (rpart, `method = "anova"`).}
#'   \item{`"linear"`}{Ordinary least squares linear regression.}
#'   \item{`"gam"`}{Generalised Additive Model via `mgcv`. Requires `mgcv`.
#'     For smooth GAM terms use `s()` syntax in your formula.}
#' }
#'
#' @param formula A model formula (e.g., `civic_score ~ age + education + region`).
#' @param data A data.frame or tibble.
#' @param model Character. One of `"cart"`, `"linear"`, `"gam"`.
#' @param seed Integer seed for reproducibility.
#' @param cart_control Optional [rpart::rpart.control()] for CART.
#' @param gam_family A `mgcv` family object (default: [mgcv::gaussian()]). Only
#'   used when `model = "gam"`.
#' @param ... Additional arguments passed to the underlying fitter.
#'
#' @return An object of class `civic_model`.
#' @export
#'
#' @examples
#' # Linear model
#' m <- civic_fit_regression(
#'   civic_knowledge_score ~ age + education + news_consumption,
#'   data  = civic_education,
#'   model = "linear"
#' )
#'
#' # Regression tree
#' m2 <- civic_fit_regression(
#'   civic_knowledge_score ~ age + education + news_consumption,
#'   data  = civic_education,
#'   model = "cart"
#' )
civic_fit_regression <- function(formula,
                                  data,
                                  model        = c("cart", "linear", "gam"),
                                  seed         = 2025,
                                  cart_control = NULL,
                                  gam_family   = NULL,
                                  ...) {
  model <- match.arg(model)
  set.seed(seed)

  y_name <- all.vars(formula)[1]
  if (!is.numeric(data[[y_name]])) {
    data[[y_name]] <- as.numeric(data[[y_name]])
    rlang::inform(c("!" = paste0("Outcome `", y_name, "` coerced to numeric.")))
  }

  fit <- switch(model,

    cart = {
      ctrl <- cart_control %||% rpart::rpart.control(cp = 0.01, minsplit = 20)
      rpart::rpart(formula, data = data, method = "anova", control = ctrl, ...)
    },

    linear = {
      stats::lm(formula, data = data, ...)
    },

    gam = {
      if (!requireNamespace("mgcv", quietly = TRUE)) {
        rlang::abort("Package `mgcv` required for model = 'gam'. Install with install.packages('mgcv').")
      }
      fam <- gam_family %||% mgcv::gaussian()
      mgcv::gam(formula, data = data, family = fam, ...)
    }
  )

  structure(
    list(
      fit        = fit,
      type       = "regression",
      model      = model,
      formula    = formula,
      seed       = seed,
      data_hash  = digest::digest(data, algo = "sha256"),
      n_train    = nrow(data),
      trained_at = Sys.time(),
      call       = match.call()
    ),
    class = "civic_model"
  )
}
