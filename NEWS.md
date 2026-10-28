# uniml 0.5.1

The CRAN pre-test for 0.5.0 asked for three things, and profiling to fix the third
one found a real performance defect.

## Kernel SHAP no longer dominates the run time

* `.workflow_shap()` spent **87% of a `run_workflow()` call** inside
  `kernelshap()`, with the sizes fixed at `explain_shap()`'s defaults (100 rows
  explained against a 100-row background). A complete report therefore took about
  a minute on a 800-patient cohort, almost all of it in one explanation.

* `n_explain` and `bg_n` are now part of the specification:
  `uniml_spec()$explain` defaults to 40 and 40, and
  `set_explain(spec, n_explain = , bg_n = )` changes them. The same workflow now
  takes about 30 seconds instead of 90, and a paper-quality explanation is one
  argument away. `set_explain(spec, model = "none")` skips SHAP entirely.

* Measured on the cardiovascular example (2 learners, 3 data sets):
  87 s → 29 s.

## CRAN pre-test fixes

* `inst/WORDLIST` declares "SHAP", "interpretability", and "multiclass", which the
  incoming spell check flagged in `DESCRIPTION`.

* The bilingual link at the top of `README.md` pointed at `README.zh.md`, which is
  excluded from the built package, so the relative URI was broken inside the
  tarball. Both READMEs now link to the GitHub copy, which resolves everywhere.

* Vignettes run smaller bootstrap and SHAP samples (`ci_R` 100/200 → 25/40, and
  20 to 30 explained rows against a 14 to 20 row background), so `R CMD check`
  finishes inside CRAN's ten-minute budget. Every vignette still runs the whole
  workflow; none of them is `eval = FALSE` and none of the numbers is typed in by
  hand.

* The end-to-end run on the largest shipped cohort (`test-cvd-workflow.R`, by far
  the slowest test) is marked `skip_on_cran()`. It covers assembly, ranking,
  selection, and prediction, all of which `test-workflow.R` covers on small
  synthetic data that does run on CRAN, and it runs on three platforms in CI on
  every commit.

# uniml 0.5.0

Recalibration for multiclass models, which 0.4.0 could measure but not fix.

## Temperature scaling

* `calibrate_probabilities()` gains a multiclass method. It fits the single scalar
  `T` that makes a model's reported probabilities match how often it is right, and
  returns `softmax(log(p) / T)`: `T > 1` softens an over-confident model, `T < 1`
  sharpens an under-confident one (Guo et al. 2017).

* Because the division is monotone and applied to every class in the same way,
  **temperature scaling cannot change which class the model ranks first.** The
  confusion matrix, the accuracy, and every threshold metric come out identical;
  only the probabilities attached to them move.

* `T` is fitted by minimising the log loss, and `T = 1` is kept whenever it is at
  least as good, so the fitted scalar never degrades the data it was fitted on. It
  is a bounded one-dimensional search, not a matrix fit, so a few hundred patients
  is enough.

* `calibrated` is cross-fitted (each fold's scalar comes from the other folds), so
  `metrics_before` / `metrics_after` are an honest before/after. That honesty has a
  cost, stated in the documentation: on a model that was already calibrated the
  cross-fitted log loss can come out a hair worse, because each fold's scalar is a
  noisy estimate of 1.

* `new_probs` / `new_labels` apply a scalar fitted on the validation set to the
  test set, with the before/after table reported on that set. `method = "none"`
  returns the raw probabilities, as in the binary path.

* A probability matrix can be passed directly; a matrix with fewer than three
  class columns is refused with a pointer to the binary calibrators, and a hard 0/1
  prediction is clamped rather than breaking the fit.

* `set_evaluation(spec, calibration = )` is now task-type aware: a multiclass
  specification accepts `"temperature"` or `"none"`, a binary one accepts
  `"platt"`, `"platt_segment"`, `"isotonic"`, or `"none"`. The two sets are
  disjoint, so a specification cannot ask for a method its task type does not have.

* **Deliberately not implemented:** Dirichlet calibration (Kull et al. 2019), the
  stronger method, fits `K^2 + K` parameters, can change the predicted class, and
  is not identifiable on the sample size this package targets. The documentation
  says so where the method would be expected.

# uniml 0.4.0

Multiclass calibration, which the package previously declined to compute because
"calibration" means more than one thing once a model outputs a probability for
every class. Both meanings are implemented and named.

## Multiclass calibration

* New `multiclass_calibration()` reports calibration under two conventions:
  **top-label** (the largest probability of each row against whether the
  predicted class was right, Guo et al. 2017) and **class-wise** (a one-vs-rest
  comparison for every class, averaged without weighting, Kull et al. 2019). It
  returns the summary, a per-class table, and the long reliability table the
  figure is drawn from.
* `calc_metrics()` on a probability matrix gains `ece` (expected calibration
  error, top-label), `mce` (maximum calibration error), and `ece_classwise`, plus
  per-class `ece`/`mce` in `per_class`. `calc_ci()` bootstraps all three.
* New `plot_calibration()` method for `uniml_multiclass`: `scope = "top_label"`,
  `"class"`, or `"both"`. The caption carries ECE, MCE, the level bias and the
  class-wise ECE, and `annotate = FALSE` removes it.
* The two conventions are reported together on purpose: top-label ECE ignores
  every non-maximal probability, so a model can look calibrated while one class
  is badly wrong, and class-wise ECE is what catches that.
* There is no multiclass slope or intercept. A multinomial model has no unique
  univariate recalibration, and its analogue (temperature scaling) is a
  recalibration method, not a diagnostic. What is reported instead is the level
  bias, `mean confidence - accuracy`.
* `calibration_metrics()` now refuses a probability matrix with a message that
  names the two functions to use instead, rather than failing inside the binary
  input checks.

## Binary calibration error

* `calc_metrics()` gains `ece` and `mce` for binary outcomes too, so the two
  paths expose the same numbers. The binary ECE bins the probability of the
  event, which is not the same estimand as the multiclass top-label ECE; the
  positive class of `multiclass_calibration()$per_class` is the bridge, and a test
  pins the equality.
* Both paths now share one binning rule, `.binned_calibration()`, so the
  calibration table used by the workflow and `calc_metrics()` cannot drift apart.

## Export

* `export_results()` writes `calibration/summary.csv` whenever the task type
  reports a per-time-point calibration summary. `unimlsurv` uses it for the ICI
  and the recalibration coefficients; without this the numbers existed only
  inside R.

# uniml 0.3.5

The last two gaps from the reporting audit: a precision-recall figure, and the
macro/micro split for multiclass discrimination.

## Precision-recall

* New `plot_pr_curve()` draws the precision-recall curve with the prevalence as a
  reference line, for the imbalanced outcomes where the ROC curve flatters a model.
  The curve and the label come from `PRROC::pr.curve()`, the same code
  `mlr3measures::prauc()` calls, so the figure and `calc_metrics()$pr_auc` cannot
  disagree.

## Multiclass discrimination, named properly

* **Correction.** `auc` was documented as a "macro one-vs-rest AUC". It is not:
  `mlr3measures::mauc_au1u` is the Hand and Till multiclass AUC, the mean over
  ordered class pairs computed on class `i`'s own score column. The documentation,
  the vignette table, and the tests said the wrong thing; they now say the right
  one.
* New `auc_ovr`: the macro one-vs-rest AUC, the unweighted mean of the per-class
  one-vs-rest AUCs. This is the number a paper means by "macro-average AUC".
* New `micro_auc`: all (patient, class) pairs pooled into one binary problem.
  Micro high with macro low means the model is carried by the common classes.
* New `per_class$auc`: one-vs-rest AUC for each class, so a class that the model
  cannot separate is visible instead of averaged away.
* New `micro_specificity`. There is deliberately no micro precision, recall, or F1:
  for a single-label multiclass outcome all three are identically the accuracy, and
  a test pins that identity.
* `plot_roc()` for multiclass predictions adds the micro-average curve beside the
  per-class curves and puts all three AUCs in the subtitle; `micro = FALSE` draws
  only the per-class curves.

# uniml 0.3.4

* `set_feature_selection(methods = "none")` is now a documented choice: it keeps
  every candidate instead of running a selector. A task type whose outcome is not
  a class label (a survival time and status, say) needs this, because every other
  method screens the outcome column. `select_features()` and the workflow treat
  `"none"` and an empty method vector the same way.

# uniml 0.3.3

* `select_features()` treats an empty `methods` vector as "no selection" and keeps
  every candidate instead of failing in `combine_selections()`. That is what a task
  type with no selection method of its own needs, and it makes the behaviour the
  same whether the specification is read by the workflow or by hand.

# uniml 0.3.2

The five reporting domains a clinical prediction model is judged on are now all
reachable from `calc_metrics()`, and a fitted result can be handed to someone else
without handing them the fitting code.

## Calibration as numbers, not only as a figure

* New `calibration_metrics()` returns the calibration intercept, the calibration
  slope, and calibration-in-the-large, with percentile bootstrap intervals. They
  come from one logistic recalibration, `logit(observed) = a + b * logit(predicted)`,
  which is the convention the calibration literature uses, not the slope of the
  binned reliability line the package computed internally before.
* `calc_metrics()` now reports `calib_slope`, `calib_intercept`, and `calib_large`
  alongside the other binary metrics, and the workflow's bootstrap covers them.
* `plot_calibration()` adds the three numbers to the caption by default
  (`annotate = FALSE` turns it off), computed on the predictions rather than on
  the binned points.
* Calibration is reported as `NA` when it cannot be estimated: fewer than 20
  patients, fewer than 5 events or 5 non-events, or separation. A slope that ran
  to infinity because of separation is not a number anyone should read.

## Confidence intervals for multiclass

* `calc_ci()` accepts a probability matrix and bootstraps the multiclass metric
  set, so macro one-vs-rest AUC, accuracy, balanced accuracy, kappa, log loss, the
  multiclass Brier score, and the macro rates all get intervals.

## A result you can hand over

* New `save_result()` and `read_result()` write and read a fitted result with a
  version stamp and validation on both ends.
* New `calc_metrics.data.frame()` scores the prediction table that
  `predict_uniml()` returns, which is how subgroup or other post-hoc analyses are
  meant to be done: this package does not define subgroups, but it hands you a
  model and a scorer.
* New `calc_metrics.matrix()` scores a multiclass probability matrix directly.

## Task-type seam

* New optional adapter hook `features(data, spec)`, and `set_preprocess(vars = )`
  is honoured when a task type declares no feature selection of its own: the
  workflow keeps the columns the task type calls features instead of borrowing a
  classification selector.

# uniml 0.3.1

Two additions that a task type with more than one outcome column needs.

* `set_preprocess()` gains `vars`, naming the columns the preprocessor may
  transform. Columns outside it are passed through untouched, so a task type can
  keep its own outcome columns out of scaling and encoding. `workflow_engine()`
  forwards it to `fit_preprocessor()`.
* `register_workflow_adapter()` accepts two optional hooks, `train()` and
  `tune()`, which default to `train_model()` and `tune_model()`. A task type whose
  learners are not `TaskClassif` learners supplies both, and the engine fits and
  tunes them without knowing what they are.

# uniml 0.3.0

The workflow is now split into a task-type-neutral engine and a per-task-type
report, so a new outcome shape plugs in through a registered adapter instead of
editing the pipeline.

## Shared workflow engine

* New `workflow_engine()` performs the half of a workflow that does not depend on
  the outcome: preparing the data, fitting imputation, preprocessing and feature
  selection, tuning, training one model per learner and imputation, and predicting
  the training, validation, test, and external sets. It returns predictions and
  never metrics.
* New `register_workflow_adapter()` and `workflow_adapter()` declare how a task
  type prepares data, builds its mlr3 task, extracts a prediction, pools
  predictions across imputations, names its metrics, and turns a prediction into
  metrics. The binary adapter ships with the package.
* `run_workflow()` is now the binary report on top of that engine; its arguments,
  return value, and numerical behaviour are unchanged.
* `run_workflow()` still rejects non-binary outcomes, and the message now says
  what is genuinely binary about it (thresholds, calibration, DeLong and
  Hosmer-Lemeshow tests, NRI/IDI, decision curves) and points at
  `workflow_engine()` instead of leaving the reader with a dead end.

## Multiclass

Multiclass outcomes are a supported task type. They are modelled at the layer
level; the full workflow is still binary-only and says so.

* `uniml_spec(task_type = "multiclass", outcome = "y")` no longer needs a
  `positive` class, and passing one is an error: a positive class only exists
  when the outcome has two levels.
* `as_uniml_data()` accepts three or more classes, keeps all of them, and sorts
  the levels so the class order is the same on every machine.
* `make_task()` builds a `TaskClassif` with no positive class.
* `calc_metrics()` gains a multiclass method (and switches to it automatically
  for an mlr3 prediction with more than two probability columns). It reports
  macro one-vs-rest AUC (`mlr3measures::mauc_au1u`), accuracy, balanced accuracy,
  Cohen's kappa, log loss, the multi-class Brier score, macro one-vs-rest
  sensitivity/specificity/PPV/NPV/F1, the k-by-k confusion table, and a per-class
  table. Each of those conventions is pinned by a test against `mlr3measures`
  and a direct recomputation.
* New `as_uniml_multiclass()` converts an mlr3 `PredictionClassif` or a
  probability matrix into the object the multiclass helpers accept.
* `plot_roc()` draws one-vs-rest ROC curves, one per class, and
  `plot_confusion()` draws a k-by-k table with the accuracy as the subtitle.
* `tune_model()` chooses a measure that exists for the task: `auc` maps to
  `classif.mauc_au1u` and `brier` to `classif.mbrier` on a multiclass task, and
  two-class-only measures such as `pr_auc` are refused instead of silently
  mistranslated.
* Calibration, decision curves, NRI/IDI, DeLong, and Hosmer-Lemeshow stay
  binary-only, and `plot_calibration()` and `plot_dca()` say why when handed a
  multiclass prediction.
* `run_workflow()` stops with a message pointing at `vignette("uniml-multiclass")`
  instead of half-running: its prediction table, thresholds, calibrators, and
  post-model tests all assume a single probability column.

## Task-type seams

These are the two places where a task type used to be hard-coded to the binary
case. Both are now declared by the task type itself, so the next task type can be
added without editing the callers.

* `task_type_spec()` gains `composite_metrics`, the metric names that
  `rank_models()` averages into its default composite score. Binary declares
  AUC, sensitivity, specificity, F2, balanced accuracy, and Youden's index;
  multiclass declares AUC, accuracy, balanced accuracy, and F1.
* `rank_models()` gains `task_type = "auto"`. It reads the task type from a
  `uniml_result` and otherwise assumes `"binary"`, so ranking a multiclass metric
  table is now correct instead of quietly averaging two-class metrics. A task
  type that declares no composite set returns an error asking for an explicit
  `best_model_metric` rather than falling back to the binary one.
* The workflow no longer calls the binary metric calculator from two places.
  `.workflow_metric_values()` is the single point that turns a prediction into a
  metric row, and it goes through the task type's adapter, so the next task type
  has one obvious place to plug in instead of editing the fold loop.

# uniml 0.1.0

First release. The binary-classification stack is implemented end to end;
`multiclass` and `survival` are registered as placeholders and raise a
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
