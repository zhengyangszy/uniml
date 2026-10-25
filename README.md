# uniml

**English** | [简体中文](README.zh.md)

`uniml` is a unified predictive modeling toolkit for binary classification. It
chains preprocessing, feature selection, model training and tuning, evaluation,
visualization, and SHAP interpretability behind one small set of objects, while
keeping a strict separation between the data that fits a model and the data
that judges it.

The package is organized as a stack of layers. Every layer can be used on its
own, and `run_workflow()` wires them together.

| Layer | Entry points | Purpose |
| --- | --- | --- |
| L0 data | `uniml_spec()`, `as_uniml_data()`, `make_split()` | Declare the task, roles, and splits |
| L1 task type | `list_task_types()`, `task_type_spec()` | Extensible per-task behaviour |
| L2 preprocessing | `fit_preprocessor()`, `impute_data()`, `scale_numeric()`, `encode_categorical()`, `balance_classes()` | Imputation, scaling, encoding, balancing |
| L3 selection | `select_features()`, `select_lasso()`, `select_boruta()`, `select_rf_importance()`, `select_rfe()`, `select_univariate()`, `select_multivariate()`, `combine_selections()`, `remove_collinear()` | Feature selection and collinearity handling |
| L4 modeling | `make_task()`, `resolve_learners()`, `train_model()`, `tune_model()` | mlr3 learners, tuning, cross validation |
| L5 statistics | `calc_metrics()`, `calc_ci()`, `calibrate_probabilities()`, `delong_test()`, `hl_test()`, `calc_nri_idi()`, `dca()`, `find_threshold()` | Metrics with confidence intervals, calibration, tests, decision curves |
| L6 results | `assemble_result()`, `rank_models()`, `export_results()` | Ranking and on-disk export |
| L7 plots | `plot_roc()`, `plot_calibration()`, `plot_dca()`, `plot_confusion()`, `theme_uniml()` | Performance figures |
| L8 SHAP | `explain_shap()`, `plot_shap_importance()`, `plot_shap_beeswarm()`, `plot_shap_dependence()`, `plot_shap_waterfall()`, `extract_thresholds()` | Interpretability |
| L9 workflow | `run_workflow()`, `predict_uniml()` | End-to-end run and reuse of fitted state |

## Installation

The package is not on CRAN yet. Install it from a local checkout:

```r
install.packages("uniml", repos = NULL, type = "source")
```

## Quick start

```r
library(uniml)

development <- read.csv(
  system.file("extdata", "uniml-cvd-mortality.csv", package = "uniml")
)
new_admissions <- read.csv(
  system.file("extdata", "uniml-cvd-mortality-new.csv", package = "uniml")
)

spec <- uniml_spec(
  task_type = "binary",
  outcome = "died",
  positive = "yes",
  id = "id"
)
spec <- set_preprocess(spec, impute = "median_mode", scale = "zscore", encode = "onehot")
spec <- set_feature_selection(spec, methods = c("univariate", "lasso"), combine = "intersect")
spec <- set_learners(spec, c("lr", "cart"), tune = FALSE)
spec <- set_tuning(spec, folds = 5, evals = 10)
spec <- set_evaluation(
  spec,
  ci = "bootstrap",
  calibration = "platt",
  select_on = "validation"
)

split <- make_split(
  as_uniml_data(development, spec),
  prop = c(0.8, 0.2),
  stratify = TRUE
)
result <- run_workflow(
  spec,
  train = split$train$data,
  validation = split$validation$data,
  test = new_admissions
)
result
```

Predictions for new data reuse the fitted preprocessing, selection, and models;
they never refit anything:

```r
predict_uniml(result, newdata = new_admissions, model = "best")
```

## Example cohorts

`inst/extdata` contains two synthetic cardiovascular cohorts:

| File | Rows | Purpose |
| --- | ---: | --- |
| `uniml-cvd-mortality.csv` | 2000 | Development cohort; split into training and validation |
| `uniml-cvd-mortality-new.csv` | 2000 | Independent new admissions; external test cohort |

Both cohorts target an in-hospital mortality probability of 6.5%. Their
observed event rates differ slightly because the binary outcomes are sampled.
The new-admission cohort is generated independently from the development
cohort and has non-overlapping identifiers.

## Available models

Every model in `uniml` is an mlr3 `Learner`, every data set becomes an mlr3
`Task`, and tuning and resampling use mlr3 objects directly. The package adds a
thin layer on top instead of hiding mlr3:

- `make_task()` builds a `TaskClassif` from your data frame and specification.
- `resolve_learners()` returns mlr3 learners plus their search spaces, which you
  can also tune yourself with `mlr3tuning`.
- `run_workflow()` stores the fitted learners in `result$models[[key]]$learner`,
  so anything that accepts an mlr3 learner keeps working.
- `as_uniml_binary()` converts an mlr3 `PredictionClassif` into the object used
  by the evaluation and plotting helpers, and `plot_roc()`,
  `plot_calibration()`, and `plot_confusion()` accept an mlr3 prediction
  directly.

The registry provides eight stable default binary-classification learners:

| Key | mlr3 learner |
| --- | --- |
| `lr` | `classif.cv_glmnet` |
| `rf` | `classif.ranger` |
| `xgb` | `classif.xgboost` |
| `lgb` | `classif.lightgbm` |
| `nnet` | `classif.nnet` |
| `knn` | `classif.kknn` |
| `cart` | `classif.rpart` |
| `nb` | `classif.naive_bayes` |

The `lgb` key relies on `mlr3extralearners`, which is not on CRAN and is
therefore listed in `Suggests`. Install it from GitHub to enable `lgb`
(`remotes::install_github("mlr-org/mlr3extralearners")`); without it, `lgb` is
reported as a recorded failure instead of being silently dropped.

It also provides an optional set of common learners that can be requested by
key but are not installed or tuned by default. These entries use their mlr3
defaults unless an explicit search space is supplied:

| Key | mlr3 learner | Optional package |
| --- | --- | --- |
| `log_reg` | `classif.log_reg` | stats |
| `svm` | `classif.svm` | e1071 |
| `lda` | `classif.lda` | MASS |
| `qda` | `classif.qda` | MASS |
| `randomForest` | `classif.randomForest` | randomForest |
| `gbm` | `classif.gbm` | gbm |

The package is not limited to those keys. Any installed mlr3 classification
learner can be passed to `set_learners()`. Learners outside the registry use
their mlr3 defaults and are not tuned unless a search space is supplied.

```r
spec <- set_learners(
  spec,
  c("lr", "classif.log_reg", "classif.svm", "classif.lda"),
  tune = TRUE
)
```

For a custom search space, register the learner explicitly:

```r
register_learner(
  "svm_custom",
  mlr3::lrn("classif.svm", predict_type = "prob", probability = TRUE),
  paradox::ps(
    cost = paradox::p_dbl(0.1, 10),
    gamma = paradox::p_dbl(0.001, 1)
  )
)
spec <- set_learners(spec, "svm_custom", tune = TRUE)
```

## Step-level arguments

Every stage accepts an explicit argument list, and unknown names are rejected
instead of being silently stored:

```r
spec <- set_preprocess(
  spec,
  impute = "mice",
  impute_args = list(m = 5, max_iter = 10),
  scale_args = list(vars = c("age", "bmi")),
  encode_args = list(max_levels = 20)
)
spec <- set_feature_selection(
  spec,
  methods = c("lasso", "boruta"),
  args = list(
    lasso = list(nfolds = 5, alpha = 0.8),
    boruta = list(tentative = TRUE)
  )
)
spec <- set_tuning(
  spec,
  folds = 5,
  repeats = 5,
  terminator = "seconds",
  seconds = 600
)
spec <- set_evaluation(
  spec,
  calibration = "platt",
  calibration_args = list(folds = 5, bins = 10, fit = "out_of_fold")
)
```

## Leakage policy

Validation and test data never take part in imputation, scaling, encoding,
selection, balancing, tuning, threshold selection, or calibration. Fitting
happens on the training data only, and the resulting parameters are applied to
the held-out data. `set_leakage_policy()` controls whether selection, balancing,
and tuning are re-run inside each cross-validation fold (`strict_in`), left
outside the folds (`strict_out`), or configured step by step (`custom`).

## Data sets and selection

`run_workflow()` takes up to four data sets. `test` and `external` are both
optional:

```r
run_workflow(spec, train, validation,
             test = internal_holdout,      # optional
             external = new_admissions)    # optional
```

`validation` is used for model comparison and selection; `test` and `external`
are reporting-only and never take part in fitting, tuning, calibration,
threshold selection, or ranking. Requesting `select_on = "test"` is allowed but
warns that the test performance becomes optimistic.

## Cross-validation and calibration

`run_workflow()` reports per-fold cross-validation rows and a summary;
`repeats > 1` instantiates repeated cross-validation, and the summary averages
folds within a repeat before summarising across repeats. Calibration is off by
default (`calibration = "none"`); when requested, the calibrator is fitted on
out-of-fold training predictions and both raw and calibrated probabilities are
kept. Classification thresholds come from the same out-of-fold predictions.
Failed or downgraded steps are never silent: they are listed in
`result$meta$failures`.

With multiple imputation (`impute = "mice", m > 1`) the workflow fits each
completed data set, pools metric estimates and confidence intervals with strict
Rubin's rules (`pool_mi()`, Barnard-Rubin degrees of freedom), and runs
cross-validation once per imputation before pooling. Prediction probabilities
are averaged across imputations ("pooled predictions") and are labelled
separately from the Rubin-pooled metric table.

## Reporting

NRI/IDI are computed against `reference_model` (default: the model with the
best training AUC) with bootstrap intervals, and written to
`tests/nri_idi.csv`. SHAP zero-crossing thresholds are opt-in:

```r
spec <- set_explain(
  spec,
  model = "best",
  plots = c("importance", "beeswarm", "dependence", "waterfall"),
  thresholds = list(targets = c("age", "bmi"))
)
```

Plots accept `theme`, `title`, `subtitle`, and `caption`, and `palette` can be
a `ggsci` name, a colour, or a colour vector. Export size and format are
controlled with `export_results(..., plot_args = list(dpi = 600, device = "png"))`.

## Reusing exported results

Everything needed to redraw or re-tabulate is exported as plain CSV plus one
RDS: `predictions/predictions.csv` (model, set, raw and calibrated
probabilities, truth) is enough to rebuild ROC/calibration/DCA/confusion
figures with `plot_roc()`/`plot_calibration()`/`plot_dca()`/`plot_confusion()`,
`performance/thresholds.csv` stores the raw and calibrated thresholds per
model, and `shap/values_matrix_*.csv` plus `shap/feature_values_*.csv` allow
custom SHAP figures. `result.rds` contains the complete fitted state (models,
preprocessor, calibrators) for `predict_uniml()` on new data.

## Vignettes

```r
vignette("uniml-atomic", package = "uniml")
vignette("uniml-workflow", package = "uniml")
vignette("uniml-shap", package = "uniml")
```

## Development status

`uniml` is at version `0.0.0.9000`. The binary task type is implemented;
`multiclass` and `survival` are registered as placeholders only. See
`NEWS.md` for changes.
