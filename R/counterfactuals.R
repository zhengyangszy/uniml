# Interpretation extras - counterfactuals and the micro ROC --------------------
#
# A counterfactual answers a patient-level question the global plots cannot:
# "what is the smallest change to this patient that flips the decision?" The
# implementation is an exhaustive what-if search over each feature's observed
# values - no optimiser, no assumptions beyond the model itself - so the
# result is the *smallest single-feature change that crosses the threshold*,
# in the same units the clinic measures. Multi-feature changes are out of
# scope: their search space grows combinatorially, and the counterfactuals
# package's optimisers are the right tool when that is needed.

#' Single-feature counterfactual search
#'
#' For each selected patient, searches every feature's observed values for the
#' smallest change that moves the model's predicted probability across
#' `target`. Numeric features sweep the observed quantiles of the column;
#' categorical features try every level. The change is reported in the
#' original units, and the probability after the change goes through the
#' model's full deployment pipeline (preprocessing, calibration, threshold),
#' exactly as `predict_uniml()` would compute it for a new patient.
#'
#' @param result A binary `uniml_result` from [run_workflow()].
#' @param newdata Data frame containing the patients. Must contain the
#'   outcome column and every feature.
#' @param rows Integer row positions of `newdata` to explain.
#' @param target The predicted probability the counterfactual should reach.
#' @param direction `"increase"`, `"decrease"`, or `NULL` to infer it from the
#'   patient's current prediction relative to `target`.
#' @param grid Number of grid points swept per numeric feature.
#' @param model A model key, or `"best"`.
#'
#' @return A data frame with one row per counterfactual found: `row`,
#'   `feature`, `original`, `counterfactual`, `prob_before`, `prob_after`,
#'   `change`. Patients whose prediction already meets `target`, or for whom
#'   no single-feature change works, return a row with `feature = NA` and a
#'   note in `counterfactual`.
#' @references
#' Wachter S, Mittelstadt B, Russell C (2017). Counterfactual explanations
#' without opening the black box. *Harvard Journal of Law & Technology*, 31,
#' 841-887.
#'
#' Dandl S, Molnar C, Binder M, Bischl B (2023). counterfactuals: Counterfactual
#' Explanations. R package. doi:10.32614/cran.package.counterfactuals
#' @export
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation)
#' counterfactuals(result, validation, rows = 1:3, target = 0.5)
#' }
counterfactuals <- function(result, newdata, rows = 1L, target = 0.5,
                            direction = NULL, grid = 20L, model = "best") {
  if (!is.data.frame(newdata)) {
    .abort("`newdata` must be a data frame.")
  }
  .assert_scalar_number(target, "target", min = 0, max = 1)
  .assert_scalar_number(grid, "grid", integer = TRUE, min = 3)
  key <- if (identical(model, "best")) {
    result$ranking$model[[which.min(result$ranking$rank)]]
  } else {
    model
  }
  if (is.null(result$models[[key]])) {
    .abort(sprintf("Unknown model \"%s\".", key))
  }
  if (!identical(result$meta$spec$task_type, "binary")) {
    .abort("Counterfactuals are implemented for binary results only.")
  }
  outcome <- result$meta$spec$outcome
  features <- setdiff(
    names(newdata),
    c(outcome, result$meta$spec$id, result$meta$spec$exclude)
  )
  if (length(features) == 0L) {
    .abort("`newdata` has no feature columns to search.")
  }
  rows <- as.integer(rows)
  if (any(rows < 1L | rows > nrow(newdata))) {
    .abort("`rows` must be row positions inside `newdata`.")
  }
  if (!is.null(direction)) {
    direction <- match.arg(direction, c("increase", "decrease"))
  }
  row_results <- lapply(rows, function(row_index) {
    patient <- newdata[row_index, , drop = FALSE]
    before <- as.numeric(
      predict_uniml(result, newdata = patient, model = key)$prob
    )
    wanted <- if (is.null(direction)) {
      if (before < target) "increase" else "decrease"
    } else {
      direction
    }
    already <- (wanted == "increase" && before >= target) ||
      (wanted == "decrease" && before <= target)
    if (already) {
      return(data.frame(
        row = row_index, feature = NA_character_,
        original = NA_character_, counterfactual = NA_character_,
        prob_before = before, prob_after = before, change = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    best <- NULL
    for (feature in features) {
      column <- newdata[[feature]]
      candidates <- if (is.numeric(column) && !is.factor(column)) {
        observed <- column[is.finite(column)]
        values <- unique(as.numeric(stats::quantile(
          observed,
          probs = seq(0, 1, length.out = grid),
          names = FALSE
        )))
        data.frame(value = values, change = abs(values - patient[[feature]]))
      } else {
        levels <- sort(unique(as.character(stats::na.omit(column))))
        data.frame(value = levels, change = 1)
      }
      candidates <- candidates[candidates$value != patient[[feature]], ,
                               drop = FALSE]
      if (nrow(candidates) == 0L) {
        next
      }
      candidates <- candidates[order(candidates$change), ]
      for (b in seq_len(nrow(candidates))) {
        pushed <- patient
        pushed[[feature]] <- candidates$value[[b]]
        after <- as.numeric(
          predict_uniml(result, newdata = pushed, model = key)$prob
        )
        crosses <- (wanted == "increase" && after >= target) ||
          (wanted == "decrease" && after <= target)
        if (crosses) {
          best <- data.frame(
            row = row_index,
            feature = feature,
            original = format(patient[[feature]]),
            counterfactual = format(candidates$value[[b]]),
            prob_before = before,
            prob_after = after,
            change = candidates$change[[b]],
            stringsAsFactors = FALSE
          )
          break
        }
      }
      if (!is.null(best)) {
        break
      }
    }
    if (is.null(best)) {
      data.frame(
        row = row_index, feature = NA_character_,
        original = NA_character_,
        counterfactual = sprintf(
          "No single-feature change reaches p = %.2f.", target
        ),
        prob_before = before, prob_after = before, change = NA_real_,
        stringsAsFactors = FALSE
      )
    } else {
      best
    }
  })
  table <- do.call(rbind, row_results)
  rownames(table) <- NULL
  table
}
