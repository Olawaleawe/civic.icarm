#' civic.icarm: Interpretable, Civic-Accountable, and Responsible Machine Learning
#'
#' @description
#' The `civic.icarm` package provides a unified pedagogical framework for
#' interpretable, civic-accountable, and responsible machine learning (ICARM).
#'
#' It is designed to support the **DataCitizen-Pro** research agenda:
#' developing data literacy, statistical reasoning, and democratic judgment
#' formation in civic and political teacher education. The package connects
#' algorithmic transparency to civic accountability — helping learners and
#' educators critically interrogate how data-driven systems make decisions
#' that affect democratic participation and public life.
#'
#' @section Core modules:
#' \describe{
#'   \item{**Modelling**}{[civic_fit_classification()], [civic_fit_regression()]}
#'   \item{**Explanation**}{[civic_explain()], [civic_explain_local()]}
#'   \item{**Fairness**}{[civic_fairness_report()], [civic_fairness_plot()]}
#'   \item{**Audit**}{[civic_audit_trail()], [civic_scorecard()]}
#'   \item{**Visualisation**}{[civic_plot_importance()], [civic_plot_pdp()],
#'         [civic_plot_fairness()], [civic_plot_confusion()]}
#'   \item{**Utilities**}{[civic_split()], [civic_metrics()], [civic_thresholds()]}
#' }
#'
#' @section DataCitizen-Pro connection:
#' The package embodies the three DataCitizen-Pro competency pillars:
#' \enumerate{
#'   \item **Data Literacy** — transparent model building with auditable parameters
#'   \item **Statistical Reasoning** — calibrated uncertainty, metric interpretation
#'   \item **Democratic Judgment** — fairness audits, equity metrics, civic scorecards
#' }
#'
#' @section Example data:
#' \describe{
#'   \item{[civic_german_credit]}{German credit scoring dataset (civic fairness benchmark)}
#'   \item{[civic_voting]}{Synthetic civic participation dataset for teaching}
#'   \item{[civic_education]}{Education outcome dataset with protected attributes}
#' }
#'
#' @references
#' Awe, O. O. (2025). DataCitizen-Pro: Data Literacy, Statistical Reasoning,
#' and Democratic Judgment Formation in Civic and Political Teacher Education.
#' DFG Sachbeihilfe Proposal, Ludwigsburg University of Education.
#'
#' @author Olushina Olawale Awe \email{awe@@ph-ludwigsburg.de}
#'
#' @docType package
#' @name civic.icarm-package
"_PACKAGE"
