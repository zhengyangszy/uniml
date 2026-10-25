# uniml 0.0.0.9000

First development version. The binary-classification stack is implemented end
to end; `multiclass` and `survival` are registered as placeholders and raise a
"not implemented" error.

## Reporting and interoperability

* New `tripod_ai_check()` audits a result against the TRIPOD+AI domains and
  reports which information is present, absent, or out of scope. The checklist
  is written to `reporting/tripod_ai.csv` by `export_results()`.
* New `as_uniml_binary()` converts an mlr3 `PredictionClassif` into the
  `uniml_binary` object used by the evaluation and plotting helpers;
  `plot_roc()`, `plot_calibration()`, and `plot_confusion()` also accept an
  mlr3 prediction directly.
* Performance plots accept `models = "best"` and `models = "all"`, matching the
  vocabulary of the SHAP plots.
* `explain_shap()` on a `uniml_result` gains a `model` argument and falls back
  to the stored training features when `data` is omitted.
* `inst/REFERENCES.md` maps every layer to the literature it implements.
* Bug fix: model comparison plots collapsed every model into one "model" series.
  Multi-model prediction frames are now recognised before the single-model path,
  and the prediction table keeps `model` and `set` in separate columns instead of
  a combined `"model:set"` label.

## Data layer

* `uniml_spec()` declares the task type, outcome, positive class, identifier and
  excluded columns, leakage policy, defaults, and output options.
* `as_uniml_data()` validates the binary outcome and records column roles.
* `make_split()` builds two- or three-way splits, optionally stratified.

## Preprocessing

* Imputation with `median_mode`, `mice`, `missForest`, `missRanger`, or `none`.
* Numeric scaling with `zscore` or `minmax`.
* Categorical encoding with one-hot and drop-first one-hot.
* Class balancing with ROSE, SMOTE, or both.
* `fit_preprocessor()` / `apply_preprocessor()` keep fitted state separate from
  the data it is applied to.

## Feature selection

* Univariate and multivariable logistic screening, LASSO, Boruta, random-forest
  importance, and recursive feature elimination.
* `combine_selections()` intersects or unions methods; `remove_collinear()`
  prunes by correlation or VIF.

## Modeling and evaluation

* mlr3 learner registry with lazily built search spaces.
* Grid, random, and Bayesian tuning, with optional nested resampling. The
  terminator can be `"evals"` or `"seconds"`.
* Cross-validation now reports per-fold rows and a summary. `repeats > 1`
  instantiates repeated cross-validation and the summary averages folds within
  a repeat before summarising across repeats.
* Metrics with bootstrap confidence intervals, DeLong and Hosmer-Lemeshow
  tests, NRI/IDI, calibration, decision-curve analysis, and threshold search.
* NRI/IDI are computed by the workflow against `reference_model` (default: the
  model with the best training AUC) with bootstrap confidence intervals, and
  exported as `tests/nri_idi.csv`.
* `set_explain(thresholds = list(targets = ...))` extracts SHAP zero-crossing
  thresholds for the requested features and exports `thresholds/zero_crossings.csv`
  and `thresholds/paper_ready_thresholds.csv`.
* The SHAP interaction plot is removed (native `sv_interaction()` support is
  limited to xgboost/lightgbm). A single-observation waterfall plot
  (`plot_shap_waterfall()`) is added instead, and the default plots are
  importance, beeswarm, dependence, and waterfall.
* `export_results()` now writes `predictions/predictions.csv` and
  `performance/thresholds.csv`, so figures can be rebuilt from CSVs without
  loading the RDS.
* `run_workflow()` gains an optional `external` data set for external
  validation. `test` and `external` are reporting-only; model selection uses
  `validation` by default (`select_on = "validation"`, `"auto"` now means the
  same). Selecting on `test` is still possible but warns about optimism.
* Plots take a `set` argument and default to the comparison set, so train,
  validation, test, and external rows are no longer pooled into one curve.
* `performance/metrics_external.csv` is written when an external set is given.
* Calibration defaults to `"none"`. Platt, segment Platt, and isotonic
  calibration are fitted on out-of-fold training predictions, and the raw and
  calibrated probabilities are both kept in the result.
* Classification thresholds are derived from the same out-of-fold predictions.
* Out-of-fold predictions refit the leakage-policy steps per fold, so
  calibration and thresholds stay honest under `strict_in`.
* Bug fix: `id` and `exclude` columns are now actually dropped before
  preprocessing (the previous `setdiff()` removed nothing, so identifiers could
  enter feature selection and models).
* Bug fix: `apply_imputer()` honours the imputation index stored on a step, and
  cross-validation for multiple imputations splits the completed data set
  instead of always transforming folds with the first imputation.
* Multiple imputation now uses strict Rubin's rules: `calc_ci()` keeps the
  bootstrap standard error, `pool_mi()` combines the per-imputation estimates
  (`T = U + (1 + 1/m) B`, Barnard-Rubin degrees of freedom, t interval), and
  cross-validation runs once per completed imputation before pooling. Pooled
  intervals are clamped to each metric's range (for example AUC in `[0, 1]`).
* A glmnet learner that receives a single feature falls back to
  `classif.log_reg` and records the downgrade in `meta$failures`.
* Step-level arguments are forwarded explicitly: `impute_args`, `scale_args`,
  `encode_args`, `balance_args`, selection `args` per method, `terminator` /
  `seconds`, and `calibration_args`. Unknown argument names are rejected
  instead of being silently ignored.

## Results, plots, and explanation

* `assemble_result()` and `rank_models()` produce the comparison table and the
  composite ranking; `export_results()` writes the layout to disk.
* ROC, calibration, DCA, and confusion-matrix figures.
* Plot helpers accept `theme`, `title`, `subtitle`, and `caption`; `palette`
  takes a `ggsci` name, a single colour, or a colour vector.
* `export_results(plot_args = list(...))` controls figure size, DPI, and device
  (for example `device = "png", dpi = 600`).
* `explain_shap()` plus importance, beeswarm, dependence, waterfall, and force
  plots, and `extract_thresholds()` for candidate cut points.
* `explain_shap()` now computes model-agnostic SHAP values with `kernelshap`
  (CRAN) instead of the archived `fastshap`. The new `n_explain` (rows to
  explain, default 100) and `bg_n` (background rows, default 100) arguments keep
  the run time bounded, and any other `kernelshap()` argument can be passed
  through `...`.
* The `lgb`, `randomForest`, and `gbm` shortcuts are retired: their mlr3
  learners live in `mlr3extralearners`, which is not on CRAN, so the package can
  now pass CRAN's dependency checks. The shortcuts fail with a message that
  names the mlr3 learner key to use instead; install and load
  `mlr3extralearners` and pass `"classif.lightgbm"` (or `"classif.randomForest"`,
  `"classif.gbm"`) directly.

* Bug fix: `calc_metrics()`, `as_uniml_binary()`, and every `plot_*()` method
  that accepts an mlr3 `PredictionClassif` read the positive class from the last
  probability column. mlr3 puts the positive class in the *first* column, so the
  confusion-matrix metrics (sensitivity, specificity, PPV, NPV, F1, F2, PR-AUC)
  were computed for the negative class while the ranking-based metrics looked
  correct. The class is now taken from the mlr3 task when the prediction carries
  it and from the first probability column otherwise; an explicit `positive`
  still overrides both.
* Bug fix: the internal Hosmer-Lemeshow fallback grouped probabilities with
  `right = FALSE` while `ResourceSelection::hoslem.test()` uses `right = TRUE`,
  so `hl_test()` returned a different statistic when `ResourceSelection` was not
  installed. The fallback now reproduces the package result exactly.
* `hl_test()` no longer returns `NaN`. When a group's probabilities are all 0 or
  all 1 the expected cell is empty and the statistic is not defined; the result
  is now `NA` with `method = "not_estimable"` and a message, on both paths.

## Verification

* Added `tests/testthat/test-consistency.R`, which pins the package to
  independent implementations: ROC AUC to `mlr3measures`, `pROC`, and the
  Mann-Whitney U statistic; PR-AUC to `PRROC`; the 2x2 metrics to a direct
  recomputation; Platt calibration to `stats::glm()` on the logit scale;
  isotonic calibration to `stats::isoreg()`; net benefit to the decision-curve
  formula; DeLong AUCs to `calc_metrics()`; and the Hosmer-Lemeshow fallback to
  `ResourceSelection`.
* Added `vignette("uniml-realdata")`, an end-to-end run on `pROC::aSAH`
  (113 patients, 12-month Glasgow outcome) rather than on the simulated cohort,
  including the `gos6` leakage guard and the reporting behaviour when a statistic
  cannot be estimated.

## Workflow

* `run_workflow()` chains the layers with `strict_in`, `strict_out`, or `custom`
  leakage handling, and `predict_uniml()` scores new data from the fitted state.

## Example data

* Added a 2000-row cardiovascular in-hospital mortality development cohort and
  an independent 2000-row new-admission cohort for external validation examples.

* Added an end-to-end workflow regression test that trains, ranks, and scores
  the synthetic cardiovascular example cohorts.
