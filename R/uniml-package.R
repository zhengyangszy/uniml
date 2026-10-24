#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom stats predict
## usethis namespace: end
NULL

# Packages below register learners, filters, or parallel backends through their
# own load hooks. They are imported here so the declared dependencies are
# explicit and visible to tooling.
#' @importFrom Boruta Boruta
#' @importFrom future plan
#' @importFrom mlr3learners LearnerClassifRanger
#' @importFrom mlr3misc map_dtr
#' @importFrom mlr3pipelines po
#' @importFrom ranger ranger
NULL

utils::globalVariables(c(".data", ".env"))
