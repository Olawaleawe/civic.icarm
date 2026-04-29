## Data generation script for civic.icarm example datasets
## Run once: source("data-raw/generate_datasets.R")
## Output written to data/ via usethis::use_data()

set.seed(2025)

# ============================================================
# 1. civic_voting — synthetic voter participation dataset
# ============================================================
n <- 1000

age       <- sample(18:85, n, replace = TRUE)
education <- ordered(
  sample(c("primary","secondary","vocational","university","postgraduate"),
         n, replace = TRUE,
         prob = c(0.05, 0.30, 0.20, 0.30, 0.15)),
  levels = c("primary","secondary","vocational","university","postgraduate")
)
income    <- round(rnorm(n, mean = 0, sd = 1.5), 2)  # standardised
pol_int   <- sample(1:5, n, replace = TRUE,
                    prob = c(0.10, 0.15, 0.30, 0.25, 0.20))
news      <- pmax(0, round(rnorm(n, mean = 5, sd = 4)))
civic_org <- rbinom(n, 1, 0.35) == 1
region    <- factor(sample(
  c("Baden-Wurttemberg","Bavaria","Berlin","Brandenburg",
    "North-Rhine-Westphalia","Saxony","Hamburg"),
  n, replace = TRUE))
gender    <- factor(sample(c("female","male","diverse"), n,
                            replace = TRUE, prob = c(0.49, 0.49, 0.02)))
mig_bg    <- rbinom(n, 1, 0.25) == 1

# Latent voting propensity (structural model)
logit_p <- -1.5 +
  0.03  * (age - 40) +
  0.40  * as.numeric(education) +
  0.20  * income +
  0.35  * pol_int +
  0.10  * news +
  0.50  * civic_org -
  0.10  * (gender == "diverse") -
  0.08  * mig_bg +
  rnorm(n, 0, 0.5)

p_vote <- 1 / (1 + exp(-logit_p))
voted  <- factor(ifelse(rbinom(n, 1, p_vote) == 1, "yes", "no"))

civic_voting <- tibble::tibble(
  voted,
  age,
  education,
  income,
  political_interest = pol_int,
  news_consumption   = news,
  civic_org_member   = civic_org,
  region,
  gender,
  migration_background = mig_bg
)

# ============================================================
# 2. civic_education — teacher education student outcomes
# ============================================================
n2 <- 800

age2        <- sample(18:45, n2, replace = TRUE)
edu2        <- factor(sample(c("Abitur","Bachelor","Master","Other"), n2,
                             replace = TRUE, prob = c(0.20, 0.45, 0.30, 0.05)))
news2       <- pmax(0, round(rnorm(n2, mean = 4, sd = 3)))
stats_crs   <- rbinom(n2, 1, 0.55) == 1
subj        <- factor(sample(
  c("Social Studies","Geography","History","Ethics","German","Other"),
  n2, replace = TRUE))
gender2     <- factor(sample(c("female","male","diverse"), n2,
                              replace = TRUE, prob = c(0.65, 0.33, 0.02)))
mig2        <- rbinom(n2, 1, 0.20) == 1

civic_score <- round(pmin(100, pmax(0,
  40 +
  0.5  * (age2 - 25) +
  8    * (edu2 == "Master") +
  4    * (edu2 == "Bachelor") +
  5    * stats_crs +
  2    * news2 -
  3    * mig2 +
  rnorm(n2, 0, 12)
)))

data_lit_score <- round(pmin(100, pmax(0,
  35 +
  6    * stats_crs +
  3    * news2 +
  4    * (edu2 %in% c("Master","Bachelor")) +
  rnorm(n2, 0, 14)
)))

civic_education <- tibble::tibble(
  civic_knowledge_score = civic_score,
  data_literacy_score   = data_lit_score,
  age                   = age2,
  education             = edu2,
  news_consumption      = news2,
  stats_course          = stats_crs,
  teaching_subject      = subj,
  gender                = gender2,
  migration_background  = mig2
)

# ============================================================
# 3. civic_german_credit — cleaned UCI German Credit (simplified)
# ============================================================
if (requireNamespace("mlbench", quietly = TRUE)) {
  data("GermanCredit", package = "mlbench")
  gc <- as.data.frame(GermanCredit)

  gender_proxy <- factor(
    ifelse(gc$Personal.male.married.widowed == 1 | gc$Personal.male.single == 1,
           "male", "female")
  )

  civic_german_credit <- tibble::tibble(
    credit_risk   = factor(ifelse(gc$Class == "Good", "good", "bad")),
    duration      = gc$Duration,
    credit_amount = gc$Amount,
    age           = gc$Age,
    housing       = factor(dplyr::case_when(
      gc$Housing.free    == 1 ~ "free",
      gc$Housing.own     == 1 ~ "own",
      TRUE                    ~ "rent"
    )),
    job           = factor(dplyr::case_when(
      gc$Job.unskilled.resident    == 1 ~ "unskilled",
      gc$Job.skilled               == 1 ~ "skilled",
      gc$Job.management.self.employed == 1 ~ "management",
      TRUE                          ~ "other"
    )),
    gender_proxy,
    purpose       = factor(dplyr::case_when(
      gc$Purpose.car        == 1 ~ "car",
      gc$Purpose.education  == 1 ~ "education",
      gc$Purpose.furniture.equipment == 1 ~ "furniture",
      TRUE                      ~ "other"
    ))
  )
} else {
  # Fallback synthetic if mlbench not available
  n3 <- 1000
  civic_german_credit <- tibble::tibble(
    credit_risk   = factor(sample(c("good","bad"), n3, TRUE, prob=c(0.7,0.3))),
    duration      = sample(6:72, n3, TRUE),
    credit_amount = round(runif(n3, 500, 15000)),
    age           = sample(18:75, n3, TRUE),
    housing       = factor(sample(c("own","rent","free"), n3, TRUE)),
    job           = factor(sample(c("skilled","unskilled","management","other"), n3, TRUE)),
    gender_proxy  = factor(sample(c("male","female"), n3, TRUE)),
    purpose       = factor(sample(c("car","education","furniture","other"), n3, TRUE))
  )
}

# Save
usethis::use_data(civic_voting, overwrite = TRUE)
usethis::use_data(civic_education, overwrite = TRUE)
usethis::use_data(civic_german_credit, overwrite = TRUE)

message("Datasets generated and saved to data/")
