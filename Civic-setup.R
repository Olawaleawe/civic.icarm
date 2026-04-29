# =============================================================
# civic.icarm — COMPLETE SETUP & TEST SCRIPT
# DataCitizen-Pro | Ludwigsburg University of Education
# Prof. Dr. Olushina Olawale Awe
#
# INSTRUCTIONS:
#   1. Unzip civic.icarm_v0.1.0.zip to a folder on your computer
#   2. In RStudio: File > New Project > Existing Directory >
#      select the civic.icarm/ folder > Create Project
#   3. Copy THIS entire script and paste into the RStudio Console
#   4. Press ENTER and let it run — it will stop and tell you
#      if anything needs attention
# =============================================================


# ── PHASE 1: Install all required packages ────────────────────
message("\n=== PHASE 1: Installing packages ===\n")

required_pkgs <- c(
  # Developer tools
  "devtools", "usethis", "roxygen2", "testthat",
  "pkgdown", "spelling", "covr", "goodpractice",
  # Package dependencies
  "rpart", "ggplot2", "dplyr", "tibble", "tidyr",
  "purrr", "rlang", "jsonlite", "digest", "scales",
  # Suggested / data generation
  "mlbench", "DALEX", "glmnet", "mgcv", "vip",
  "pROC", "knitr", "rmarkdown", "iml"
)

# Install only what is missing (avoids re-downloading everything)
missing_pkgs <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]

if (length(missing_pkgs) > 0) {
  message("Installing: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}


# ── PHASE 2: Generate example datasets ────────────────────────
message("\n=== PHASE 2: Generating example datasets ===\n")

#This creates data/civic_voting.rda, data/civic_education.rda,
data/civic_german_credit.rda — required before document() works
source("data-raw/generate_datasets.R")


# ── PHASE 3: Generate documentation (NAMESPACE + man/) ────────
message("\n=== PHASE 3: Building documentation ===\n")

devtools::document()


# ── PHASE 4: Load the package into memory ─────────────────────
message("\n=== PHASE 4: Loading package ===\n")

devtools::load_all()


# ── PHASE 5: Run the test suite ───────────────────────────────
message("\n=== PHASE 5: Running tests ===\n")

devtools::test()


# ── PHASE 6: Full R CMD CHECK ─────────────────────────────────
# This is what CRAN runs. Fix all ERRORs and WARNINGs before
# submitting. NOTEs on a first submission are acceptable.
message("\n=== PHASE 6: R CMD CHECK ===\n")

devtools::check()


# ── PHASE 7: Smoke test — full DataCitizen-Pro workflow ───────
message("\n=== PHASE 7: Smoke test ===\n")

library(civic.icarm)

# Load data
data(civic_voting)
data(civic_education)
data(civic_german_credit)
message("Datasets loaded: civic_voting (", nrow(civic_voting), " rows), ",
        "civic_education (", nrow(civic_education), " rows), ",
        "civic_german_credit (", nrow(civic_german_credit), " rows)")

# --- 7a. Split ---
splits <- civic_split(
  civic_voting,
  prop     = 0.75,
  seed     = 2025,
  stratify = "voted"
)
message("Train: ", nrow(splits$train), " | Test: ", nrow(splits$test))

# --- 7b. Train CART classifier ---
m_cart <- civic_fit_classification(
  voted ~ age + education + political_interest +
    news_consumption + civic_org_member + income,
  data  = splits$train,
  model = "cart",
  seed  = 2025
)
print(m_cart)

# --- 7c. Train logistic regression ---
m_logit <- civic_fit_classification(
  voted ~ age + education + political_interest + news_consumption,
  data  = splits$train,
  model = "logistic",
  seed  = 2025
)
print(m_logit)

# --- 7d. Train linear regression on education data ---
m_reg <- civic_fit_regression(
  civic_knowledge_score ~ age + education + stats_course + news_consumption,
  data  = civic_education,
  model = "linear",
  seed  = 2025
)
print(m_reg)

# --- 7e. Predictions ---
y_hat  <- predict(m_cart, splits$test, type = "class")
y_prob <- predict(m_cart, splits$test, type = "prob")[, "yes"]
message("Predictions computed. First 6: ", paste(head(y_hat), collapse = ", "))

# --- 7f. Performance metrics ---
metrics <- civic_metrics(
  y_true   = splits$test$voted,
  y_pred   = y_hat,
  y_prob   = y_prob,
  positive = "yes"
)
message("Performance metrics:")
print(round(metrics, 3))

# --- 7g. Threshold analysis ---
thr_tbl <- civic_thresholds(
  y_true    = splits$test$voted,
  y_prob    = y_prob,
  positive  = "yes"
)
message("Threshold table computed (", nrow(thr_tbl), " rows)")

# --- 7h. Global explanation ---
ex_cart <- civic_explain(m_cart, data = splits$train)
print(ex_cart)

ex_logit <- civic_explain(m_logit)

# --- 7i. Fairness by gender ---
fair_gender <- civic_fairness_report(
  object    = m_cart,
  data      = splits$test,
  outcome   = "voted",
  protected = "gender",
  positive  = "yes"
)
print(fair_gender)

# --- 7j. Fairness by migration background ---
fair_mig <- civic_fairness_report(
  object    = m_cart,
  data      = splits$test,
  outcome   = "voted",
  protected = "migration_background",
  positive  = "yes"
)
print(fair_mig)

# --- 7k. Equity summary ---
eq <- civic_equity_summary(fair_gender)
message("Disparate impact pass: ", eq$disparate_impact_pass)
message("Equal opportunity pass: ", eq$equal_opp_pass)

# --- 7l. Calibration ---
cal <- civic_calibration(
  object   = m_cart,
  data     = splits$test,
  outcome  = "voted",
  positive = "yes"
)
print(cal)

# --- 7m. Model comparison ---
cmp <- civic_compare(
  models    = list(CART = m_cart, Logistic = m_logit),
  test_data = splits$test,
  outcome   = "voted",
  protected = "gender",
  positive  = "yes"
)
print(cmp)

# --- 7n. Equalized odds curves ---
eoc <- civic_equalized_odds_curve(
  object    = m_cart,
  data      = splits$test,
  outcome   = "voted",
  protected = "gender",
  positive  = "yes"
)
message("Equalized odds curve computed (", nrow(eoc), " rows)")

# --- 7o. Audit trail ---
trail <- civic_audit_trail(
  object   = m_cart,
  metrics  = metrics,
  fairness = fair_gender,
  analyst  = "O. O. Awe",
  notes    = "Smoke test — DataCitizen-Pro pilot"
)
message("Audit trail JSON:")
cat(trail)

# --- 7p. Civic scorecard ---
message("\nCivic Scorecard:")
civic_scorecard(
  object    = m_cart,
  test_data = splits$test,
  outcome   = "voted",
  protected = "gender",
  positive  = "yes",
  analyst   = "O. O. Awe",
  project   = "DataCitizen-Pro Pilot 2025"
)

# --- 7q. German credit fairness (classic benchmark) ---
m_credit <- civic_fit_classification(
  credit_risk ~ duration + credit_amount + age + housing + job + purpose,
  data  = civic_german_credit,
  model = "cart",
  seed  = 2025
)
fair_credit <- civic_fairness_report(
  m_credit, civic_german_credit,
  outcome   = "credit_risk",
  protected = "gender_proxy",
  positive  = "good"
)
message("German Credit fairness (by gender proxy):")
print(fair_credit)


# ── PHASE 8: Generate all plots (check they render) ───────────
message("\n=== PHASE 8: Generating all plots ===\n")

# Feature importance — CART
p1 <- civic_plot_importance(ex_cart)
print(p1)
message("Plot 1/9: importance (CART) OK")

# Feature importance — Logistic
p2 <- civic_plot_importance(ex_logit,
                            title = "Feature Importance — Logistic (|coefficient|)")
print(p2)
message("Plot 2/9: importance (Logistic) OK")

# Fairness — TPR by gender
p3 <- civic_plot_fairness(fair_gender, metric = "tpr",
                          title = "Equal Opportunity by Gender")
print(p3)
message("Plot 3/9: fairness TPR OK")

# Fairness — disparate impact
p4 <- civic_plot_fairness(fair_gender, metric = "dp_ratio",
                          title = "Disparate Impact Ratio", ref_line = 0.8)
print(p4)
message("Plot 4/9: fairness DP ratio OK")

# Confusion matrix
p5 <- civic_plot_confusion(splits$test$voted, y_hat)
print(p5)
message("Plot 5/9: confusion matrix OK")

# Threshold curves
p6 <- civic_plot_thresholds(thr_tbl,
                            metrics = c("accuracy", "recall", "precision", "f1"))
print(p6)
message("Plot 6/9: threshold curves OK")

# Calibration curve
p7 <- civic_plot_calibration(cal)
print(p7)
message("Plot 7/9: calibration curve OK")

# Model comparison
p8 <- civic_plot_comparison(cmp,
                            metrics = c("accuracy", "f1", "max_tpr_gap", "min_dp_ratio"))
print(p8)
message("Plot 8/9: model comparison OK")

# Equalized odds ROC curves
p9 <- civic_plot_roc_groups(eoc)
print(p9)
message("Plot 9/9: equalized odds ROC OK")


# ── PHASE 9: Coverage report ──────────────────────────────────
message("\n=== PHASE 9: Test coverage ===\n")

# Opens HTML report in your browser
# covr::report()   # uncomment to run
cov <- covr::package_coverage()
print(cov)


# ── PHASE 10: Spell check ─────────────────────────────────────
message("\n=== PHASE 10: Spell check ===\n")

spelling::spell_check_package()


# ── PHASE 11: Pre-CRAN check (strictest settings) ─────────────
message("\n=== PHASE 11: CRAN-ready check ===\n")

devtools::check(
  cran   = TRUE,
  remote = TRUE
)


# ── DONE ──────────────────────────────────────────────────────
message("
=======================================================
  civic.icarm setup complete.

  If you see:
    0 errors | 0 warnings  --> ready for CRAN submission
    0 errors | 1+ warnings --> fix warnings first
    1+ errors              --> share the output here

  Next step to submit to CRAN:
    devtools::release()

  DataCitizen-Pro | LUE Ludwigsburg | 2025
=======================================================
")