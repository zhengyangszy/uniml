make_workflow_data <- function(n = 90L, seed = 2026L) {
  set.seed(seed)
  y <- factor(rep(c("no", "yes"), length.out = n), levels = c("no", "yes"))
  signal <- stats::rnorm(n) + ifelse(y == "yes", 1.2, 0)
  data.frame(
    y = y,
    x1 = signal,
    x2 = stats::rnorm(n),
    cat = sample(c("a", "b"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

make_workflow_split <- function(data) {
  set.seed(11)
  index <- sample.int(nrow(data))
  list(
    train = data[index[1:60], , drop = FALSE],
    validation = data[index[61:75], , drop = FALSE],
    test = data[index[76:90], , drop = FALSE]
  )
}

make_workflow_spec <- function(mode = "strict_out", ...) {
  spec <- uniml_spec(task_type = "binary", outcome = "y", positive = "yes")
  spec <- set_preprocess(
    spec,
    impute = "median_mode",
    scale = "zscore",
    encode = "onehot",
    balance = "none"
  )
  spec <- set_feature_selection(
    spec,
    methods = "univariate",
    p_threshold = 1,
    representation = "full_levels"
  )
  spec <- set_learners(spec, "cart", tune = FALSE)
  spec <- set_tuning(
    spec,
    tuner = "random_search",
    folds = 2L,
    repeats = 1L,
    evals = 1L,
    metric = "auc",
    nested = FALSE,
    seed = 123
  )
  spec <- set_evaluation(
    spec,
    ci = "none",
    calibration = "none",
    tests = character(),
    dca = FALSE,
    select_on = "auto"
  )
  spec <- set_explain(spec, model = "none", plots = character())
  spec <- set_leakage_policy(spec, mode = mode)
  dots <- list(...)
  if (length(dots) > 0L) {
    spec <- do.call(set_leakage_policy, c(list(spec = spec, mode = mode), dots))
  }
  spec
}

make_workflow_mice_data <- function() {
  data <- make_workflow_data()
  data$x1[c(2L, 9L, 17L)] <- NA_real_
  data$x2[c(24L, 38L)] <- NA_real_
  data
}

make_workflow_mice_spec <- function() {
  spec <- make_workflow_spec("strict_out")
  set_preprocess(
    spec,
    impute = "mice",
    m = 2L,
    scale = "zscore",
    encode = "onehot",
    balance = "none"
  )
}
