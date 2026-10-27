## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local: Windows 11 x64, R 4.6.0 (2026-04-24 ucrt)
  `R CMD check --as-cran --no-manual`
* GitHub Actions, R release: ubuntu-latest, macos-latest, windows-latest
  https://github.com/zhengyangszy/uniml/actions

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes

* This is the first CRAN release of `uniml` (0.4.0).

* Every package in `Imports` and `Suggests` is available on CRAN. An earlier
  development version suggested `mlr3extralearners`, which is not on CRAN; that
  dependency and the three learner shortcuts that needed it were removed, so the
  package now installs from CRAN alone. Learners from other packages can still be
  passed to `set_learners()` by their mlr3 key once the user installs them.

* The two example cohorts in `inst/extdata` are **simulated** by
  `data-raw/simulate-cvd-mortality.R`. They make the examples runnable and are
  never presented as patient data; the README states this under "Scope and
  limitations", and `vignette("uniml-realdata")` runs the same workflow on
  `pROC::aSAH` so that at least one worked example uses a published cohort. That
  data set ships with the 'pROC' package and is not downloaded.

* Checking examples takes about 15 seconds. The test suite takes about five
  minutes; individual tests skip when an optional `Suggests` package is missing.

* Vignettes execute the full workflow, including model training and tuning, so
  `--as-cran` spends most of its time rebuilding them.

* Binary and multiclass outcomes are implemented. `run_workflow()` is binary-only
  and stops with an explanatory message on a multiclass specification; the
  multiclass path is used layer by layer and has its own vignette. The `survival`
  task type is registered as a placeholder and raises a "not implemented" error.
  This is documented in `?list_task_types`, in the README's scope section, and in
  `vignette("uniml-multiclass")`.

* Multiclass calibration is reported under two stated conventions (top-label
  ECE/MCE and class-wise ECE) by `multiclass_calibration()`, rather than under a
  convention chosen silently. Multiclass *recalibration* is deliberately absent:
  Platt scaling and isotonic regression are one-vs-rest and do not produce a
  calibrated multinomial distribution, and the text says so where the function
  would be expected.

* The remote CRAN incoming check could not reach CRAN from the machine used for
  this submission (`PACKAGES.in` timed out), so it was run with
  `_R_CHECK_CRAN_INCOMING_REMOTE_=false`. The two URLs in `DESCRIPTION` were
  fetched manually and return HTTP 200.
