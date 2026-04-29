# =============================================================
# civic.icarm v0.2.0 — CLEAN SETUP SCRIPT
# 
# DO NOT run the whole file at once
# =============================================================

# ── SECTION 1: Set working directory ─────────────────────────
setwd("C:/Users/olawa/OneDrive/Desktop/civic.icarm")
getwd()

# ── SECTION 2: Clean up stray files ──────────────────────────
for (f in c("hello.R", "R/hello.R", "R/plots_doc.R")) {
  if (file.exists(f)) { file.remove(f); message("Removed: ", f) }
}
if (dir.exists("icarm")) {
  unlink("icarm", recursive = TRUE)
  message("Removed icarm/ subfolder")
}

# ── SECTION 3: Write correct DESCRIPTION ─────────────────────
cat('Package: civic.icarm
Title: Interpretable Civic-Accountable and Responsible Machine Learning
Version: 0.2.0
Authors@R: c(person("Olushina Olawale", "Awe", email = "olawaleawe@gmail.com", role = c("aut", "cre")), person("Ludwigsburg University of Education", role = "fnd"))
Description: A general-purpose framework for Interpretable Civic-Accountable and Responsible Machine Learning (ICARM). Works with any clean tabular data and automatically detects whether a task is binary classification, multi-class classification, or regression from the target variable type. Provides a single unified entry point civic_fit() alongside tidy interfaces for global and local model explanations, group-level fairness auditing, probability calibration, multi-model comparison, threshold analysis, and reproducible audit trails. Designed to support the DataCitizen-Pro research agenda at Ludwigsburg University of Education: developing data literacy, statistical reasoning, and democratic judgment formation in civic and political teacher education.
License: MIT + file LICENSE
Encoding: UTF-8
Language: en-GB
Depends: R (>= 4.1.0)
Imports: stats, utils, rpart, ggplot2, dplyr, tidyr, tibble, purrr, rlang, jsonlite, digest
Suggests: DALEX, glmnet, mgcv, pROC, nnet, testthat, covr
Config/testthat/edition: 3
LazyData: true
', file = "DESCRIPTION")
message("DESCRIPTION written")
read.dcf("DESCRIPTION")[, c("Package","Version")]

# ── SECTION 4: Fix non-ASCII in civic_scorecard.R ────────────
lines <- readLines("R/civic_scorecard.R", warn = FALSE)
lines <- iconv(lines, from = "UTF-8", to = "ASCII", sub = "-")
writeLines(lines, "R/civic_scorecard.R")
message("Non-ASCII fixed")

# ── SECTION 5: Write globalVariables ─────────────────────────
writeLines(
  '#\' @keywords internal
"_PACKAGE"

utils::globalVariables(c(
  "grp", "y", "y_hat", "ppos", "group",
  "acc", "acc_gap", "tpr", "tpr_gap", "fpr", "fpr_gap",
  "fnr", "tnr", "ppv", "rate_pos", "mean_prob",
  "dp_ratio", "eo_gap", "reference_group",
  "err", "se", "mae", "rmse", "mae_gap", "rmse_gap",
  "feature", "importance", "importance_scaled",
  "coefficient", "contribution", "variable",
  "model_name", "interpretability", "metric",
  "metric_label", "value", "accuracy", "balanced_acc",
  "f1", "precision", "recall", "auc", "r2",
  "max_acc_gap", "max_tpr_gap", "max_fpr_gap",
  "min_dp_ratio", "max_eo_gap", "di_pass", "eo_pass",
  "n_train", "threshold", "rate_positive",
  "mean_pred", "obs_freq", "bin", "bin_lower", "bin_upper",
  "Actual", "Predicted", "Freq", "n", "where", ".data"
))
', con = "R/civic.icarm-package.R")
message("globalVariables written")

# ── SECTION 6: Write clean NAMESPACE ─────────────────────────
writeLines(
  'export(civic_fit)
export(civic_split)
export(civic_metrics)
export(civic_thresholds)
export(civic_explain)
export(civic_explain_local)
export(civic_fairness)
export(civic_equity_summary)
export(civic_equalized_odds_curve)
export(civic_calibrate)
export(civic_compare)
export(civic_audit)
export(civic_scorecard)
export(civic_plot_importance)
export(civic_plot_fairness)
export(civic_plot_confusion)
export(civic_plot_calibration)
export(civic_plot_thresholds)
export(civic_plot_comparison)
export(civic_plot_roc_groups)

S3method(print,   civic_model)
S3method(summary, civic_model)
S3method(predict, civic_model)
S3method(print,   civic_explainer)
S3method(print,   civic_fairness)
S3method(print,   civic_calibration)
S3method(print,   civic_comparison)
', con = "NAMESPACE")
message("NAMESPACE written")

# ── SECTION 7: Confirm R files ───────────────────────────────
message("R/ files:")
print(list.files("R/"))

# ── SECTION 8: Regenerate datasets ───────────────────────────
source("data-raw/generate_datasets.R")
message("Datasets: ", paste(list.files("data/"), collapse=", "))

# ── SECTION 9: Document and load ─────────────────────────────
devtools::document()
devtools::load_all()
message("Load OK")

# ── SECTION 10: Run tests ─────────────────────────────────────
devtools::test()

# ── SECTION 11: Final check ───────────────────────────────────
devtools::check(
  build_args = "--no-build-vignettes",
  args       = c("--no-examples",
                 "--no-tests",
                 "--no-manual",
                 "--no-vignettes",
                 "--no-byte-compile"),
  vignettes  = FALSE,
  manual     = FALSE,
  cran       = FALSE
)

# ── SECTION 12: Submit to CRAN ───────────────────────────────
# Only run after Section 11 shows 0 errors | 0 warnings
# devtools::release()