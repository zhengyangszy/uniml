# Simulate cardiovascular in-hospital mortality cohorts.
#
# The data are intended for package examples and end-to-end workflow checks.
# They are synthetic, not real patient data. The target event probability is
# 6.5%, which is in the range commonly reported for cardiovascular in-hospital
# mortality cohorts. The development and new-admission cohorts are generated
# independently from the same data-generating process.

args <- commandArgs(trailingOnly = FALSE)
script_arg <- args[grep("^--file=", args)]
if (length(script_arg) == 0L) {
  stop("This script must be run with Rscript.", call. = FALSE)
}
script_path <- normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE)
package_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)

logit <- function(x) {
  stats::qlogis(x)
}

inv_logit <- function(x) {
  stats::plogis(x)
}

standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
}

trim_numeric <- function(x, lower, upper) {
  round(pmin(pmax(x, lower), upper), 3)
}

make_missing <- function(n, base_rate, driver, seed) {
  intercept <- stats::uniroot(
    function(beta) mean(inv_logit(beta + driver)) - base_rate,
    interval = c(-20, 20)
  )$root
  probability <- inv_logit(intercept + driver)
  set.seed(seed)
  stats::runif(n) < probability
}

rank_auc <- function(outcome, probability) {
  n_event <- sum(outcome == 1L)
  n_non_event <- sum(outcome == 0L)
  ranks <- rank(probability, ties.method = "average")
  (sum(ranks[outcome == 1L]) - n_event * (n_event + 1) / 2) /
    (n_event * n_non_event)
}

simulate_cvd_cohort <- function(n, seed, id_offset = 0L, target_rate = 0.065) {
set.seed(seed)

# Demographics and baseline characteristics ---------------------------------

age <- round(stats::rnorm(n, mean = 68, sd = 12))
age <- pmin(pmax(age, 25), 95)
age_z <- standardize(age)
sex <- sample(
  c("male", "female"),
  n,
  replace = TRUE,
  prob = c(0.58, 0.42)
)

smoker_prob <- inv_logit(logit(0.30) - 0.25 * age_z + 0.20 * (sex == "male"))
smoker <- ifelse(stats::runif(n) < smoker_prob, "yes", "no")

diabetes_prob <- inv_logit(logit(0.34) + 0.20 * age_z + 0.15 * (sex == "male"))
diabetes <- ifelse(stats::runif(n) < diabetes_prob, "yes", "no")

hypertension_prob <- inv_logit(logit(0.64) + 0.35 * age_z)
hypertension <- ifelse(stats::runif(n) < hypertension_prob, "yes", "no")

prior_mi <- ifelse(stats::runif(n) < 0.24, "yes", "no")
prior_stroke <- ifelse(stats::runif(n) < 0.12, "yes", "no")
ckd <- ifelse(stats::runif(n) < 0.21, "yes", "no")
copd <- ifelse(stats::runif(n) < 0.12, "yes", "no")
atrial_fibrillation <- ifelse(stats::runif(n) < 0.17, "yes", "no")
anemia <- ifelse(stats::runif(n) < 0.25, "yes", "no")

admission_type <- sample(
  c("emergency", "urgent", "elective"),
  n,
  replace = TRUE,
  prob = c(0.62, 0.27, 0.11)
)

acs_type <- sample(
  c("stemi", "nstemi", "unstable_angina", "other"),
  n,
  replace = TRUE,
  prob = c(0.14, 0.46, 0.28, 0.12)
)

killip_latent <- -1.45 +
  0.030 * (age - 68) +
  0.55 * (prior_mi == "yes") +
  0.45 * (ckd == "yes") +
  0.30 * (diabetes == "yes") +
  0.25 * (atrial_fibrillation == "yes") +
  stats::rnorm(n, sd = 0.85)
killip_class <- cut(
  killip_latent,
  breaks = c(-Inf, -0.25, 1.10, 2.15, Inf),
  labels = c("1", "2", "3", "4"),
  right = FALSE
)

reperfusion_prob <- inv_logit(
  logit(0.45) -
    0.25 * age_z -
    0.45 * (admission_type == "elective") +
    0.20 * (acs_type == "stemi")
)
reperfusion <- ifelse(
  stats::runif(n) < reperfusion_prob,
  "yes",
  "no"
)

# A latent severity factor creates realistic correlation among the markers.
severity <- 0.50 * age_z +
  0.45 * (ckd == "yes") +
  0.35 * (prior_mi == "yes") +
  0.35 * (atrial_fibrillation == "yes") +
  0.25 * (copd == "yes") +
  0.60 * (as.integer(killip_class) - 1L) +
  stats::rnorm(n, sd = 0.65)

# Vital signs and laboratory measurements ------------------------------------

bmi <- trim_numeric(
  stats::rnorm(
    n,
    mean = 27 + 1.6 * (diabetes == "yes") + 0.7 * (hypertension == "yes"),
    sd = 4.8
  ),
  15,
  50
)
sbp <- trim_numeric(
  stats::rnorm(n, 130 - 6.5 * severity - 0.20 * (age - 68) + 2.5 * (hypertension == "yes"), 18),
  70,
  220
)
dbp <- trim_numeric(
  stats::rnorm(n, 78 - 2.0 * severity + 2.0 * (hypertension == "yes"), 11),
  40,
  120
)
heart_rate <- trim_numeric(
  stats::rnorm(n, 78 + 3.5 * severity + 2.0 * (anemia == "yes"), 14),
  40,
  160
)
respiratory_rate <- trim_numeric(
  stats::rnorm(n, 18 + 1.2 * severity, 4),
  8,
  40
)
temperature <- trim_numeric(
  stats::rnorm(n, 36.8 + 0.08 * severity, 0.65),
  35,
  41
)
spo2 <- trim_numeric(
  stats::rnorm(n, 96 - 1.7 * severity - 0.20 * (age - 68), 2.6),
  70,
  100
)

creatinine <- trim_numeric(
  exp(
    log(0.90) +
      0.18 * severity +
      0.020 * (age - 68) +
      0.45 * (ckd == "yes") +
      stats::rnorm(n, sd = 0.22)
  ),
  0.30,
  12
)
bun <- trim_numeric(
  exp(
    log(15) +
      0.12 * severity +
      0.012 * (age - 68) +
      0.25 * (ckd == "yes") +
      stats::rnorm(n, sd = 0.22)
  ),
  3,
  120
)
hemoglobin <- trim_numeric(
  stats::rnorm(
    n,
    13.5 -
      0.70 * severity -
      1.20 * (anemia == "yes") -
      0.45 * (ckd == "yes") -
      0.018 * (age - 68),
    1.7
  ),
  5,
  20
)
wbc <- trim_numeric(
  exp(
    log(9) +
      0.10 * severity +
      0.10 * (acs_type == "stemi") +
      stats::rnorm(n, sd = 0.26)
  ),
  1,
  35
)
platelet <- trim_numeric(
  stats::rnorm(n, 230 - 12 * severity - 15 * (ckd == "yes"), 55),
  30,
  600
)
glucose <- trim_numeric(
  exp(
    log(120) +
      0.10 * severity +
      0.25 * (diabetes == "yes") +
      stats::rnorm(n, sd = 0.20)
  ),
  40,
  500
)
potassium <- trim_numeric(
  stats::rnorm(n, 4.1 + 0.06 * severity + 0.15 * (ckd == "yes"), 0.45),
  2.5,
  7
)
sodium <- trim_numeric(
  stats::rnorm(n, 139 - 0.35 * severity - 0.020 * (age - 68), 3),
  120,
  155
)
nt_probnp <- trim_numeric(
  exp(
    log(180) +
      0.24 * severity +
      0.015 * (age - 68) +
      0.25 * (ckd == "yes") +
      0.30 * (atrial_fibrillation == "yes") +
      stats::rnorm(n, sd = 0.50)
  ),
  10,
  40000
)
troponin <- trim_numeric(
  exp(
    log(6) +
      0.14 * severity +
      0.30 * (acs_type == "stemi") +
      0.10 * (ckd == "yes") +
      stats::rnorm(n, sd = 0.70)
  ),
  0.01,
  100
)
albumin <- trim_numeric(
  stats::rnorm(
    n,
    40 - 1.2 * severity - 1.5 * (ckd == "yes") - 0.6 * (anemia == "yes"),
    4
  ),
  15,
  55
)
lvef <- trim_numeric(
  stats::rnorm(
    n,
    55 -
      5.0 * severity -
      0.15 * (age - 68) -
      2.0 * (prior_mi == "yes") -
      1.0 * (atrial_fibrillation == "yes"),
    8
  ),
  10,
  75
)

# Noise columns are deliberately unrelated to the outcome.
lab_noise_1 <- trim_numeric(stats::rnorm(n, mean = 5, sd = 1), -2, 12)
lab_noise_2 <- trim_numeric(stats::rnorm(n, mean = 100, sd = 15), 40, 160)
ward_code <- sample(
  c("A", "B", "C", "D"),
  n,
  replace = TRUE,
  prob = c(0.30, 0.30, 0.25, 0.15)
)
admission_quarter <- sample(
  c("Q1", "Q2", "Q3", "Q4"),
  n,
  replace = TRUE
)

# Outcome model --------------------------------------------------------------

killip_numeric <- as.integer(killip_class)
linear_predictor <-
  0.045 * (age - 68) -
  0.040 * (lvef - 55) +
  0.45 * log(pmax(nt_probnp, 1) / 180) +
  0.60 * log(pmax(creatinine, 0.05) / 0.90) +
  0.34 * (killip_numeric - 1L) -
  0.014 * (sbp - 130) +
  0.012 * (heart_rate - 75) -
  0.080 * (spo2 - 96) +
  0.22 * (ckd == "yes") +
  0.20 * (prior_mi == "yes") +
  0.15 * (prior_stroke == "yes") +
  0.15 * (copd == "yes") +
  0.12 * (atrial_fibrillation == "yes") +
  0.12 * (diabetes == "yes") +
  0.08 * (smoker == "yes") -
  0.35 * (admission_type == "elective") -
  0.25 * (reperfusion == "yes") +
  0.06 * log(pmax(troponin, 0.01) / 6) -
  0.05 * (hemoglobin - 13.5) -
  0.03 * (albumin - 40) +
  0.005 * (bun - 15) +
  0.01 * (wbc - 9)

outcome_intercept <- stats::uniroot(
  function(beta) mean(inv_logit(beta + linear_predictor)) - target_rate,
  interval = c(-20, 20)
)$root
event_probability <- inv_logit(outcome_intercept + linear_predictor)
died <- stats::rbinom(n, size = 1L, prob = event_probability)

# MAR missingness, related to age and admission type -------------------------

emergency <- as.numeric(admission_type == "emergency")
elective <- as.numeric(admission_type == "elective")
missing_seed <- seed + 200L

missing_nt_probnp <- make_missing(
  n = n,
  0.20,
  0.45 * age_z + 0.35 * emergency - 0.20 * elective,
  seed = missing_seed + 1L
)
missing_troponin <- make_missing(
  n = n,
  0.15,
  0.25 * age_z + 0.30 * emergency - 0.15 * elective,
  seed = missing_seed + 2L
)
missing_lvef <- make_missing(
  n = n,
  0.12,
  0.20 * age_z + 0.30 * emergency - 0.15 * elective,
  seed = missing_seed + 3L
)
missing_albumin <- make_missing(
  n = n,
  0.10,
  0.30 * age_z + 0.25 * emergency - 0.15 * elective,
  seed = missing_seed + 4L
)
missing_bmi <- make_missing(
  n = n,
  0.08,
  -0.15 * age_z + 0.15 * emergency,
  seed = missing_seed + 5L
)
missing_smoker <- make_missing(
  n = n,
  0.06,
  -0.10 * age_z + 0.20 * emergency,
  seed = missing_seed + 6L
)
missing_hemoglobin <- make_missing(
  n = n,
  0.05,
  0.20 * age_z + 0.20 * emergency,
  seed = missing_seed + 7L
)
missing_spo2 <- make_missing(
  n = n,
  0.04,
  0.15 * age_z + 0.25 * emergency,
  seed = missing_seed + 8L
)
missing_creatinine <- make_missing(
  n = n,
  0.03,
  0.10 * age_z + 0.20 * emergency,
  seed = missing_seed + 9L
)

nt_probnp[missing_nt_probnp] <- NA_real_
troponin[missing_troponin] <- NA_real_
lvef[missing_lvef] <- NA_real_
albumin[missing_albumin] <- NA_real_
bmi[missing_bmi] <- NA_real_
smoker[missing_smoker] <- NA_character_
hemoglobin[missing_hemoglobin] <- NA_real_
spo2[missing_spo2] <- NA_real_
creatinine[missing_creatinine] <- NA_real_

data <- data.frame(
  id = id_offset + seq_len(n),
  age = age,
  sex = sex,
  bmi = bmi,
  smoker = smoker,
  diabetes = diabetes,
  hypertension = hypertension,
  prior_mi = prior_mi,
  prior_stroke = prior_stroke,
  ckd = ckd,
  copd = copd,
  atrial_fibrillation = atrial_fibrillation,
  anemia = anemia,
  admission_type = admission_type,
  acs_type = acs_type,
  killip_class = as.character(killip_class),
  reperfusion = reperfusion,
  sbp = sbp,
  dbp = dbp,
  heart_rate = heart_rate,
  respiratory_rate = respiratory_rate,
  temperature = temperature,
  spo2 = spo2,
  creatinine = creatinine,
  bun = bun,
  hemoglobin = hemoglobin,
  wbc = wbc,
  platelet = platelet,
  glucose = glucose,
  potassium = potassium,
  sodium = sodium,
  nt_probnp = nt_probnp,
  troponin = troponin,
  albumin = albumin,
  lvef = lvef,
  lab_noise_1 = lab_noise_1,
  lab_noise_2 = lab_noise_2,
  ward_code = ward_code,
  admission_quarter = admission_quarter,
  died = ifelse(died == 1L, "yes", "no"),
  stringsAsFactors = FALSE
)

list(
  data = data,
  event_probability = event_probability,
  observed_events = sum(died == 1L),
  oracle_auc = rank_auc(died, event_probability),
  missing_rate = mean(is.na(data[, setdiff(names(data), "id")]))
)
}

write_cohort <- function(data, file_name) {
  output_path <- file.path(
    package_root,
    "inst",
    "extdata",
    file_name
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data,
    file = output_path,
    row.names = FALSE,
    na = "NA",
    quote = FALSE,
    fileEncoding = "UTF-8"
  )
  output_path
}

cohorts <- list(
  development = simulate_cvd_cohort(
    n = 2000L,
    seed = 123L,
    id_offset = 0L
  ),
  new_admissions = simulate_cvd_cohort(
    n = 2000L,
    seed = 124L,
    id_offset = 2000L
  )
)

output_paths <- c(
  write_cohort(
    cohorts$development$data,
    "uniml-cvd-mortality.csv"
  ),
  write_cohort(
    cohorts$new_admissions$data,
    "uniml-cvd-mortality-new.csv"
  )
)

for (i in seq_along(cohorts)) {
  cohort <- cohorts[[i]]
  message(sprintf(
    "Wrote %d rows to %s",
    nrow(cohort$data),
    output_paths[[i]]
  ))
  message(sprintf(
    "Observed in-hospital mortality: %d/%d = %.3f%%",
    cohort$observed_events,
    nrow(cohort$data),
    100 * cohort$observed_events / nrow(cohort$data)
  ))
  message(sprintf(
    "Mean simulated event probability: %.3f%%",
    100 * mean(cohort$event_probability)
  ))
  message(sprintf(
    "Oracle AUC from the simulation model: %.3f",
    cohort$oracle_auc
  ))
  message(sprintf(
    "Overall cell missingness: %.2f%%",
    100 * cohort$missing_rate
  ))
}
