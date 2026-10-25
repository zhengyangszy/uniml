# Reporting checklist (TRIPOD+AI) ---------------------------------------------
#
# `tripod_ai_check()` audits what a `uniml_result` actually contains against the
# domains of the TRIPOD+AI statement. It reports where the information is, and
# where it is missing or out of scope, instead of claiming compliance: several
# TRIPOD+AI items (study design, participant eligibility, risk-of-bias
# assessment, code availability) cannot be derived from a fitted workflow and
# must be written by the author.

.tripod_status_levels <- c(
  "reported", "partial", "not_reported", "not_supported"
)

.tripod_item <- function(section, domain, requirement, status, evidence, note) {
  stopifnot(status %in% .tripod_status_levels)
  data.frame(
    section = section,
    domain = domain,
    requirement = requirement,
    status = status,
    evidence = evidence,
    note = note,
    stringsAsFactors = FALSE
  )
}

.tripod_has <- function(x) {
  !is.null(x) && length(x) > 0L
}

.tripod_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return("NA")
  }
  as.character(x)[1L]
}

.tripod_metric_names <- function(result) {
  if (is.null(result$metrics) || !is.data.frame(result$metrics)) {
    return(character())
  }
  setdiff(names(result$metrics), c("model", "set", "probability"))
}

.tripod_preprocess_method <- function(result, kind) {
  steps <- result$preprocess$steps
  if (is.null(steps) || is.null(steps[[kind]])) {
    return(NULL)
  }
  method <- steps[[kind]]$method
  if (is.null(method) || identical(method, "none")) {
    return(NULL)
  }
  method
}

.tripod_calibration_methods <- function(result) {
  calibration <- result$calibration
  if (is.null(calibration)) {
    return(character())
  }
  methods <- vapply(calibration, function(entry) {
    if (is.list(entry) && !is.null(entry$method)) entry$method else NA_character_
  }, character(1))
  unique(stats::na.omit(methods))
}

#' Audit a result against the TRIPOD+AI reporting domains
#'
#' Reads a [run_workflow()] result and reports, domain by domain, whether the
#' information required by the TRIPOD+AI statement (Collins et al., 2024) is
#' present in the object, partially present, absent, or out of scope for the
#' package. The checklist is an audit aid: it never claims compliance, and the
#' items marked `"not_supported"` must be written by the author.
#'
#' @param result A `uniml_result` from [run_workflow()].
#' @param file Optional path. When supplied, the checklist is also written to
#'   CSV.
#'
#' @return A data frame with columns `section`, `domain`, `requirement`,
#'   `status` (`"reported"`, `"partial"`, `"not_reported"`, or
#'   `"not_supported"`), `evidence`, and `note`.
#'
#' @references
#' Collins GS, Moons KGM, Dhiman P, et al. (2024). TRIPOD+AI statement: updated
#' guidance for reporting clinical prediction models that use regression or
#' machine learning methods. *BMJ*, 385, e078378. doi:10.1136/bmj-2023-078378
#'
#' Collins GS, Reitsma JB, Altman DG, Moons KGM (2015). Transparent reporting of
#' a multivariable prediction model for individual prognosis or diagnosis
#' (TRIPOD): the TRIPOD statement. *Annals of Internal Medicine*, 162(1), 55-63.
#' doi:10.7326/M14-0697
#'
#' @export
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation, test)
#' checklist <- tripod_ai_check(result)
#' table(checklist$status)
#' }
tripod_ai_check <- function(result, file = NULL) {
  validate_uniml_result(result)

  data_info <- result$data_info
  n_train <- if (is.null(data_info$n_train)) NA_integer_ else data_info$n_train
  n_external <- if (is.null(data_info$n_external)) 0L else data_info$n_external
  metric_names <- .tripod_metric_names(result)
  calibration_methods <- .tripod_calibration_methods(result)
  impute_method <- .tripod_preprocess_method(result, "impute")
  scale_method <- .tripod_preprocess_method(result, "scale")
  encode_method <- .tripod_preprocess_method(result, "encode")
  balance_method <- .tripod_preprocess_method(result, "balance")
  model_names <- names(result$models)
  has_cv <- .tripod_has(result$cv)
  has_tuning <- .tripod_has(result$tuning)
  has_selection <- .tripod_has(result$selection)
  has_shap <- .tripod_has(result$shap$objects)
  has_dca <- .tripod_has(result$dca)
  has_ci <- .tripod_has(result$ci)
  has_thresholds <- .tripod_has(result$meta$thresholds)
  has_failures <- .tripod_has(result$meta$failures) &&
    nrow(result$meta$failures) > 0L
  external_rows <- if (is.data.frame(result$metrics) &&
                       "set" %in% names(result$metrics)) {
    sum(result$metrics$set == "external")
  } else {
    0L
  }

  rows <- list(
    .tripod_item(
      "Methods", "Data source and setting",
      "Source of data, study setting, and recruitment period",
      "not_supported",
      "Not recorded by the workflow",
      "Describe the data source, setting, and dates in the manuscript."
    ),
    .tripod_item(
      "Methods", "Participants",
      "Eligibility criteria, sampling, and data collection",
      "not_supported",
      "Not recorded by the workflow",
      "Describe inclusion/exclusion criteria and measurement procedures."
    ),
    .tripod_item(
      "Methods", "Outcome",
      "Outcome definition, timing, and prevalence",
      if (is.null(data_info$outcome)) "not_reported" else "reported",
      sprintf(
        "outcome = %s; positive = %s; n_train = %s",
        .tripod_chr(data_info$outcome),
        .tripod_chr(data_info$positive),
        .tripod_chr(n_train)
      ),
      "Add the outcome definition and the event count per set."
    ),
    .tripod_item(
      "Methods", "Sample size",
      "Number of participants and events in each data set",
      if (is.na(n_train)) "not_reported" else "reported",
      sprintf(
        "n_train = %s, n_validation = %s, n_test = %s, n_external = %s",
        .tripod_chr(n_train),
        .tripod_chr(data_info$n_validation),
        .tripod_chr(data_info$n_test),
        .tripod_chr(n_external)
      ),
      "Report the sample size calculation (Riley et al., 2020) separately."
    ),
    .tripod_item(
      "Methods", "Missing data",
      "How missing values were handled, including the number of imputations",
      if (is.null(impute_method)) "not_reported" else "reported",
      if (is.null(impute_method)) {
        "No imputation step in the fitted preprocessor"
      } else {
        sprintf("impute method = %s", impute_method)
      },
      "State whether complete cases or imputation were used and why."
    ),
    .tripod_item(
      "Methods", "Predictors and preprocessing",
      "Predictor definitions and any transformation, scaling, or encoding",
      if (is.null(scale_method) && is.null(encode_method)) {
        "partial"
      } else {
        "reported"
      },
      sprintf(
        "scale = %s; encode = %s; balance = %s",
        ifelse(is.null(scale_method), "none", scale_method),
        ifelse(is.null(encode_method), "none", encode_method),
        ifelse(is.null(balance_method), "none", balance_method)
      ),
      "List the candidate predictors and their measurement scale."
    ),
    .tripod_item(
      "Methods", "Feature selection",
      "Whether predictor selection was performed and how",
      if (has_selection) "reported" else "not_reported",
      if (has_selection) "result$selection" else "No feature selection step",
      "Describe the selection method and the number of retained predictors."
    ),
    .tripod_item(
      "Methods", "Model development",
      "Algorithm, hyperparameter values, and the tuning procedure",
      if (length(model_names) == 0L) {
        "not_reported"
      } else if (has_tuning) {
        "reported"
      } else {
        "partial"
      },
      sprintf(
        "models = %s; tuned = %s",
        if (length(model_names) == 0L) {
          "none"
        } else {
          paste(model_names, collapse = ", ")
        },
        ifelse(has_tuning, "yes", "no (learner defaults)")
      ),
      "Report the final hyperparameters of the reported model."
    ),
    .tripod_item(
      "Methods", "Internal validation",
      "Internal validation procedure such as cross-validation or bootstrapping",
      if (has_cv) "reported" else "not_reported",
      if (has_cv) "result$cv" else "No cross-validation stored",
      "Report the number of folds/repeats and whether tuning was nested."
    ),
    .tripod_item(
      "Methods", "External validation",
      "Whether the model was evaluated in an independent data set",
      if (external_rows > 0L) {
        "reported"
      } else {
        "not_reported"
      },
      if (external_rows > 0L) {
        sprintf("external rows in result$metrics: %d", external_rows)
      } else {
        "No external data set was supplied"
      },
      "Pass `external = ` to run_workflow() when an independent cohort exists."
    ),
    .tripod_item(
      "Results", "Discrimination",
      "Discrimination performance with a confidence interval",
      if (!"auc" %in% metric_names) {
        "not_reported"
      } else if (has_ci) {
        "reported"
      } else {
        "partial"
      },
      sprintf(
        "auc present = %s; confidence intervals = %s",
        ifelse("auc" %in% metric_names, "yes", "no"),
        ifelse(has_ci, "yes", "no")
      ),
      "Report the C-statistic with a 95% confidence interval per data set."
    ),
    .tripod_item(
      "Results", "Calibration",
      "Calibration of predicted probabilities",
      if (length(calibration_methods) == 0L) {
        "not_reported"
      } else {
        "reported"
      },
      if (length(calibration_methods) == 0L) {
        "calibration = none"
      } else {
        sprintf("calibration = %s", paste(calibration_methods, collapse = ", "))
      },
      "Show a calibration plot and a calibration-in-the-large or slope."
    ),
    .tripod_item(
      "Results", "Classification threshold",
      "How the decision threshold was chosen",
      if (has_thresholds) "reported" else "not_reported",
      if (has_thresholds) "result$meta$thresholds" else "No threshold stored",
      "Report the threshold and the criterion used (Youden, cost, fixed)."
    ),
    .tripod_item(
      "Results", "Clinical utility",
      "Net benefit or decision curve analysis",
      if (has_dca) "reported" else "not_reported",
      if (has_dca) "result$dca" else "No decision curve stored",
      "Report net benefit across plausible threshold probabilities."
    ),
    .tripod_item(
      "Results", "Explainability",
      "Model interpretation such as SHAP values or variable importance",
      if (has_shap) "reported" else "not_reported",
      if (has_shap) {
        sprintf("SHAP models = %s", paste(result$shap$models, collapse = ", "))
      } else {
        "No SHAP explanation stored"
      },
      "Report global importance and, where relevant, individual explanations."
    ),
    .tripod_item(
      "Results", "Fairness and subgroups",
      "Performance across subgroups and fairness considerations",
      "not_supported",
      "The workflow does not stratify performance automatically",
      "Stratify the exported predictions by subgroup and report the metrics."
    ),
    .tripod_item(
      "Other", "Reproducibility",
      "Random seed, software versions, and a failure log",
      if (!is.null(result$meta$seed) && has_failures) {
        "reported"
      } else if (!is.null(result$meta$seed)) {
        "partial"
      } else {
        "not_reported"
      },
      sprintf(
        "seed = %s; session info = %s; recorded failures = %s",
        .tripod_chr(result$meta$seed),
        ifelse(is.null(result$meta$session_info), "no", "yes"),
        ifelse(has_failures, nrow(result$meta$failures), 0L)
      ),
      "Report the software versions and any model that failed."
    ),
    .tripod_item(
      "Other", "Code and data availability",
      "Availability of the analysis code, data, and model",
      "not_supported",
      "Not recorded by the workflow",
      "State where the code, data, and fitted model can be obtained."
    )
  )

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$status <- factor(out$status, levels = .tripod_status_levels)
  if (!is.null(file)) {
    utils::write.csv(as.data.frame(out), file, row.names = FALSE)
  }
  out
}
