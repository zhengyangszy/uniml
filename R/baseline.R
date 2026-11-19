# Baseline comparability table ------------------------------------------------
#
# External validation starts with a question the metrics cannot answer: is the
# new cohort the same kind of population? The reporting convention (TRIPOD,
# and every clinical paper's Table 1) answers it with a baseline table and
# standardised mean differences. The package reports SMD and deliberately not
# p-values: with hundreds of patients a p-value flags trivial differences,
# while |SMD| > 0.1 is the conventional "difference worth mentioning" line.

# One column's rows: numeric variables give mean (sd); categorical variables
# give n (%) per level. Both give the SMD against the reference.
.baseline_column_rows <- function(name, reference, comparison) {
  if (is.numeric(reference)) {
    reference <- as.numeric(reference)
    comparison <- as.numeric(comparison)
    mean_reference <- mean(reference, na.rm = TRUE)
    sd_reference <- stats::sd(reference, na.rm = TRUE)
    mean_comparison <- mean(comparison, na.rm = TRUE)
    sd_comparison <- stats::sd(comparison, na.rm = TRUE)
    pooled <- sqrt((sd_reference^2 + sd_comparison^2) / 2)
    smd <- if (is.finite(pooled) && pooled > 0) {
      (mean_comparison - mean_reference) / pooled
    } else {
      NA_real_
    }
    data.frame(
      variable = name,
      level = NA_character_,
      reference_stat = sprintf("%.1f (%.1f)", mean_reference, sd_reference),
      comparison_stat = sprintf("%.1f (%.1f)", mean_comparison, sd_comparison),
      smd = smd,
      stringsAsFactors = FALSE
    )
  } else {
    reference <- as.character(stats::na.omit(reference))
    comparison <- as.character(stats::na.omit(comparison))
    levels <- sort(unique(c(reference, comparison)))
    n_reference <- length(reference)
    n_comparison <- length(comparison)
    rows <- lapply(levels, function(level) {
      p_reference <- sum(reference == level) / max(n_reference, 1L)
      p_comparison <- sum(comparison == level) / max(n_comparison, 1L)
      pooled <- sqrt((p_reference * (1 - p_reference) +
                        p_comparison * (1 - p_comparison)) / 2)
      smd <- if (is.finite(pooled) && pooled > 0) {
        (p_comparison - p_reference) / pooled
      } else {
        NA_real_
      }
      data.frame(
        variable = name,
        level = level,
        reference_stat = sprintf("%d (%.1f%%)",
                                 sum(reference == level), 100 * p_reference),
        comparison_stat = sprintf("%d (%.1f%%)",
                                  sum(comparison == level), 100 * p_comparison),
        smd = smd,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }
}

#' Baseline comparability table (Table 1 with standardised mean differences)
#'
#' Compares two cohorts feature by feature - the table external-validation
#' papers report before anything else, to show that the new cohort is the same
#' kind of population. Numeric variables are summarised as mean (sd),
#' categorical variables as n (%) per level, and every row carries the
#' standardised mean difference (SMD): the difference divided by its pooled
#' standard deviation, which is barely affected by sample size.
#'
#' The table reports SMD and deliberately **not p-values**: with a few hundred
#' patients a p-value flags trivial differences, while the conventional reading
#' line is `|SMD| > 0.1`.
#'
#' @param reference The reference cohort's data frame (usually the development
#'   or validation data).
#' @param comparison The comparison cohort's data frame (usually the external
#'   or test data).
#' @param spec Optional specification; its outcome, `id`, and excluded columns
#'   are left out of the table.
#'
#' @return A data frame with `variable`, `level`, `reference_stat`,
#'   `comparison_stat`, and `smd`.
#' @references
#' Austin PC (2009). Balance diagnostics for comparing the distribution of
#' baseline covariates between treatment groups in propensity-score matched
#' samples. *Statistics in Medicine*, 28(25), 3083-3107. doi:10.1002/sim.3697
#'
#' Collins GS, Moons KGM, Dhiman P, et al. (2024). TRIPOD+AI statement. *BMJ*,
#' 385, e078378. doi:10.1136/bmj-2023-078378
#' @export
#' @examples
#' \dontrun{
#' result <- run_workflow(spec, train, validation, test = test,
#'                        external = external)
#' result$meta$baseline
#' }
baseline_table <- function(reference, comparison, spec = NULL) {
  if (!is.data.frame(reference) || !is.data.frame(comparison)) {
    .abort("`reference` and `comparison` must be data frames.")
  }
  skip <- character()
  if (!is.null(spec)) {
    skip <- c(spec$outcome, spec$id, spec$exclude, spec$survival$time,
              spec$survival$status)
  }
  shared <- setdiff(intersect(names(reference), names(comparison)), skip)
  if (length(shared) == 0L) {
    .abort("The two cohorts share no columns to compare.")
  }
  rows <- lapply(shared, function(column) {
    .baseline_column_rows(
      column, reference[[column]], comparison[[column]]
    )
  })
  table <- do.call(rbind, rows)
  rownames(table) <- NULL
  table
}
