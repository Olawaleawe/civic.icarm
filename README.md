# civic.icarm <img src="man/figures/logo.png" align="right" height="139"/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/oluwale-awe/civic.icarm/workflows/R-CMD-check/badge.svg)](https://github.com/oluwale-awe/civic.icarm/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
<!-- badges: end -->

**civic.icarm** provides a unified, pedagogically-grounded R framework for
**I**nterpretable, **C**ivic-**A**ccountable, and **R**esponsible **M**achine Learning.

It is the computational backbone of the **DataCitizen-Pro** project —
a DFG-funded research programme at [Ludwigsburg University of Education (LUE)](https://www.ph-ludwigsburg.de)
developing **data literacy**, **statistical reasoning**, and **democratic judgment**
in civic and political teacher education.

> *"Algorithmic decisions that affect civic life must be interpretable,
> auditable, and fair — not merely accurate."*
> — DataCitizen-Pro, DFG Sachbeihilfe 2025

---

## Installation

```r
# Development version from GitHub
remotes::install_github("oluwale-awe/civic.icarm")

# With all suggested packages
remotes::install_github("oluwale-awe/civic.icarm", dependencies = TRUE)
```

---

## Quick Start

```r
library(civic.icarm)

# 1. Split data reproducibly
splits <- civic_split(civic_voting, prop = 0.75,
                      seed = 2025, stratify = "voted")

# 2. Train an interpretable CART classifier
m <- civic_fit_classification(
  voted ~ age + education + political_interest + news_consumption,
  data = splits$train, model = "cart", seed = 2025
)
print(m)

# 3. Explain globally
ex <- civic_explain(m, data = splits$train)
civic_plot_importance(ex)

# 4. Evaluate on test set
y_hat <- predict(m, splits$test, type = "class")
civic_metrics(splits$test$voted, y_hat, positive = "yes")

# 5. Audit fairness across groups
fair <- civic_fairness_report(m, splits$test,
                              outcome   = "voted",
                              protected = "gender",
                              positive  = "yes")
civic_plot_fairness(fair, metric = "tpr")

# 6. Generate civic accountability scorecard
civic_scorecard(m, splits$test, outcome = "voted",
                protected = "gender", positive = "yes",
                project   = "DataCitizen-Pro Pilot")

# 7. Produce reproducible audit trail (JSON)
civic_audit_trail(m, metrics = civic_metrics(splits$test$voted, y_hat),
                  fairness = fair, analyst = "O. O. Awe",
                  path = "audit_trail.json")
```

---

## Package Architecture

```
civic.icarm/
├── R/
│   ├── civic_fit_classification.R  # CART, logistic, L1-penalised
│   ├── civic_fit_regression.R      # CART, linear, GAM
│   ├── civic_explain.R             # Global + local explanations (DALEX)
│   ├── civic_fairness_report.R     # Fairness metrics & equity summary
│   ├── civic_audit_trail.R         # Audit trail + civic scorecard
│   ├── plots.R                     # All ggplot2 visualisations
│   ├── utils.R                     # split, metrics, thresholds
│   └── data.R                      # Dataset documentation
├── data/                           # civic_voting, civic_education,
│                                   # civic_german_credit
├── tests/testthat/                 # 20+ unit tests
└── vignettes/                      # Full DataCitizen-Pro workflow
```

---

## DataCitizen-Pro Connection

The three **DataCitizen-Pro** competency pillars map directly to package modules:

| Pillar | civic.icarm module | Key function |
|---|---|---|
| Data Literacy | Modelling + Audit | `civic_fit_*()`, `civic_audit_trail()` |
| Statistical Reasoning | Metrics + Thresholds | `civic_metrics()`, `civic_thresholds()` |
| Democratic Judgment | Fairness + Scorecard | `civic_fairness_report()`, `civic_scorecard()` |

---

## Supported Learners

| Model | Type | Function |
|---|---|---|
| CART (decision tree) | Classification & Regression | `civic_fit_classification(model="cart")` |
| Logistic regression | Classification | `civic_fit_classification(model="logistic")` |
| L1-penalised logistic | Classification | `civic_fit_classification(model="logistic_l1")` |
| Linear regression | Regression | `civic_fit_regression(model="linear")` |
| GAM | Regression | `civic_fit_regression(model="gam")` |

---

## Example Datasets

| Dataset | Rows | Description |
|---|---|---|
| `civic_voting` | 1,000 | Synthetic voter participation (with protected attributes) |
| `civic_education` | 800 | Teacher education civic knowledge outcomes |
| `civic_german_credit` | 1,000 | German credit scoring fairness benchmark |

---

## Citation

```bibtex
@software{awe2025civicicarm,
  author  = {Awe, Olushina Olawale},
  title   = {{civic.icarm}: Interpretable, Civic-Accountable, and Responsible
             Machine Learning for Education},
  year    = {2025},
  url     = {https://github.com/oluwale-awe/civic.icarm},
  note    = {R package v0.1.0. Developed under the DataCitizen-Pro research
             programme, DFG Sachbeihilfe, Ludwigsburg University of Education.}
}
```

---

## Author

**Prof. Dr. Olushina Olawale Awe**  
Alexander von Humboldt Foundation Visiting Professor of Statistical and Data Science Literacy  
Ludwigsburg University of Education (LUE), Germany  
[awe@ph-ludwigsburg.de](mailto:awe@ph-ludwigsburg.de)

---

## Acknowledgements

Developed within the **DataCitizen-Pro** project, submitted to the
Deutsche Forschungsgemeinschaft (DFG) Sachbeihilfe programme.
The Alexander von Humboldt Foundation is thanked for supporting the Visiting
Professorship at LUE that made this work possible.
