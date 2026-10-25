# L5 post-processing tests ---------------------------------------------------

.model_names_for_tests <- function(pred_list) {
  if (is.null(names(pred_list)) || any(!nzchar(names(pred_list)))) {
    names(pred_list) <- paste0("model_", seq_along(pred_list))
  }
  names(pred_list)
}

.format_delong_p <- function(p) {
  if (is.na(p)) {
    return("")
  }
  significance <- if (p < 0.001) {
    "***"
  } else if (p < 0.01) {
    "**"
  } else if (p < 0.05) {
    "*"
  } else {
    ""
  }
  sprintf("%.4f%s", p, significance)
}

#' Compare binary model AUCs with DeLong tests
#'
#' @param pred_list Named list of predicted-probability vectors.
#' @param labels Binary truth labels.
#' @param positive Positive class label.
#'
#' @return A list containing AUC values, the best model, a pairwise p-value
#'   matrix, a formatted matrix, and a summary data frame. Fewer than two
#'   models returns a skipped result without an error.
#' @references
#' DeLong ER, DeLong DM, Clarke-Pearson DL (1988). Comparing the areas under two
#' or more correlated receiver operating characteristic curves: a nonparametric
#' approach. *Biometrics*, 44(3), 837-845. doi:10.2307/2531595
#' @export
#' @examples
#' delong_test(
#'   list(a = c(0.1, 0.7, 0.8, 0.2), b = c(0.2, 0.6, 0.7, 0.3)),
#'   c(0, 1, 1, 0)
#' )
delong_test <- function(pred_list, labels, positive = NULL) {
  if (!is.list(pred_list) || length(pred_list) == 0L) {
    .abort("`pred_list` must be a non-empty list of probability vectors.")
  }
  model_names <- .model_names_for_tests(pred_list)
  if (length(labels) == 0L) {
    .abort("`labels` must not be empty.")
  }
  truth <- .binary_event_vector(labels, positive = positive)$event
  common_keep <- !is.na(truth)
  for (i in seq_along(pred_list)) {
    .validate_probability_vector(
      pred_list[[i]],
      sprintf("pred_list[[%d]]", i)
    )
    if (length(pred_list[[i]]) != length(truth)) {
      .abort("All probability vectors and `labels` must have equal lengths.")
    }
    common_keep <- common_keep & !is.na(pred_list[[i]])
  }

  if (length(model_names) < 2L) {
    return(list(
      status = "skipped",
      message = "DeLong testing requires at least two models.",
      p_matrix = NULL,
      formatted = NULL,
      auc_values = NULL,
      best_model = model_names[[1L]],
      summary = data.frame(
        model = model_names,
        auc = NA_real_,
        stringsAsFactors = FALSE
      ),
      n_models = 1L,
      n_samples = as.integer(sum(common_keep))
    ))
  }

  event <- as.numeric(truth[common_keep])
  probs <- lapply(pred_list, function(x) as.numeric(x[common_keep]))
  auc_values <- stats::setNames(rep(NA_real_, length(model_names)), model_names)
  rocs <- vector("list", length(model_names))
  if (length(unique(event)) == 2L) {
    for (i in seq_along(model_names)) {
      roc_fit <- tryCatch(
        pROC::roc(
          event,
          probs[[i]],
          levels = c(0, 1),
          direction = "<",
          quiet = TRUE
        ),
        error = function(e) NULL
      )
      rocs[i] <- list(roc_fit)
      if (!is.null(roc_fit)) {
        auc_values[[i]] <- as.numeric(roc_fit$auc)
      }
    }
  }

  p_matrix <- matrix(
    NA_real_,
    nrow = length(model_names),
    ncol = length(model_names),
    dimnames = list(model_names, model_names)
  )
  for (i in seq_len(length(model_names) - 1L)) {
    for (j in seq.int(i + 1L, length(model_names))) {
      if (is.null(rocs[[i]]) || is.null(rocs[[j]])) {
        next
      }
      test <- tryCatch(
        pROC::roc.test(rocs[[i]], rocs[[j]], method = "delong"),
        error = function(e) NULL
      )
      if (!is.null(test)) {
        p_matrix[i, j] <- test$p.value
        p_matrix[j, i] <- test$p.value
      }
    }
  }
  diag(p_matrix) <- NA_real_

  formatted <- matrix(
    "",
    nrow = length(model_names),
    ncol = length(model_names),
    dimnames = list(model_names, model_names)
  )
  for (i in seq_along(model_names)) {
    for (j in seq_along(model_names)) {
      formatted[i, j] <- if (i == j) {
        "-"
      } else {
        .format_delong_p(p_matrix[i, j])
      }
    }
  }

  summary <- data.frame(
    model = model_names,
    auc = unname(auc_values),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  best_model <- if (all(is.na(auc_values))) {
    NA_character_
  } else {
    model_names[[which.max(auc_values)]]
  }
  status <- if (length(unique(event)) < 2L) {
    "insufficient_classes"
  } else {
    "ok"
  }
  list(
    status = status,
    message = if (status == "ok") {
      NULL
    } else {
      "DeLong testing requires both outcome classes."
    },
    p_matrix = p_matrix,
    formatted = formatted,
    auc_values = auc_values,
    best_model = best_model,
    summary = summary,
    n_models = length(model_names),
    n_samples = as.integer(length(event))
  )
}

.hl_undefined_message <- function() {
  paste(
    "The Hosmer-Lemeshow statistic is undefined because at least one group has",
    "no expected events or no expected non-events.",
    "Use fewer groups (a smaller `g`)."
  )
}

.hl_group_count <- function(n, g, n_unique) {
  adaptive <- if (n < 100L) {
    min(g, 5L)
  } else if (n < 1000L) {
    min(g, 8L)
  } else {
    min(g, 10L)
  }
  as.integer(max(2L, min(adaptive, n_unique)))
}

.custom_hl <- function(probs, event, g) {
  order_index <- order(probs)
  probs <- probs[order_index]
  event <- event[order_index]
  breaks <- unique(stats::quantile(
    probs,
    probs = seq(0, 1, length.out = g + 1L),
    na.rm = TRUE,
    names = FALSE,
    type = 7
  ))
  if (length(breaks) < 3L) {
    return(list(
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n = length(event),
      n_groups = 0L,
      method = "custom",
      groups = NULL,
      message = "Fewer than three distinct probability groups."
    ))
  }
  group <- cut(
    probs,
    breaks = breaks,
    include.lowest = TRUE,
    right = TRUE,
    labels = FALSE
  )
  groups <- lapply(
    sort(unique(group)),
    function(group_id) which(group == group_id)
  )
  # A group whose probabilities are all 0 or all 1 leaves an expected cell at
  # zero, which is the same condition that makes ResourceSelection's statistic
  # undefined. Both paths must agree on that verdict.
  degenerate <- any(vapply(groups, function(index) {
    prob <- probs[index]
    all(prob == 0) || all(prob == 1)
  }, logical(1)))
  if (degenerate) {
    return(list(
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n = length(event),
      n_groups = length(groups),
      method = "not_estimable",
      groups = NULL,
      message = .hl_undefined_message()
    ))
  }
  stats <- lapply(seq_along(groups), function(i) {
    index <- groups[[i]]
    prob <- pmin(pmax(probs[index], 1e-10), 1 - 1e-10)
    observed_events <- sum(event[index] == 1)
    expected_events <- sum(prob)
    observed_nonevents <- sum(event[index] == 0)
    expected_nonevents <- sum(1 - prob)
    chi <- (observed_events - expected_events)^2 / expected_events +
      (observed_nonevents - expected_nonevents)^2 / expected_nonevents
    data.frame(
      group = i,
      n = length(index),
      avg_pred_prob = mean(prob),
      observed_events = observed_events,
      expected_events = expected_events,
      observed_nonevents = observed_nonevents,
      expected_nonevents = expected_nonevents,
      chi_sq = chi
    )
  })
  group_stats <- do.call(rbind, stats)
  if (nrow(group_stats) < 3L) {
    return(list(
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n = length(event),
      n_groups = nrow(group_stats),
      method = "custom",
      groups = group_stats,
      message = "Fewer than three valid groups."
    ))
  }
  statistic <- sum(group_stats$chi_sq)
  df <- nrow(group_stats) - 2L
  if (!is.finite(statistic)) {
    return(list(
      statistic = NA_real_,
      df = as.integer(df),
      p_value = NA_real_,
      n = length(event),
      n_groups = nrow(group_stats),
      method = "not_estimable",
      groups = group_stats,
      message = .hl_undefined_message()
    ))
  }
  list(
    statistic = statistic,
    df = as.integer(df),
    p_value = stats::pchisq(statistic, df, lower.tail = FALSE),
    n = length(event),
    n_groups = nrow(group_stats),
    method = "custom",
    groups = group_stats,
    message = NULL
  )
}

#' Hosmer-Lemeshow goodness-of-fit test
#'
#' Uses `ResourceSelection::hoslem.test()` when available and falls back to a
#' quantile-grouped implementation.
#'
#' @param probs Predicted probabilities.
#' @param labels Binary truth labels.
#' @param g Requested number of groups.
#' @param positive Positive class label.
#'
#' @return A list with the test statistic, degrees of freedom, p value,
#'   group count, method, group details, and a status message.
#' @references
#' Hosmer DW, Lemeshow S (1980). A goodness-of-fit test for the multiple
#' logistic regression model. *Communications in Statistics - Theory and
#' Methods*, 10, 1043-1069. doi:10.1080/03610928008827941
#' @export
#' @examples
#' set.seed(1)
#' p <- stats::runif(80)
#' y <- stats::rbinom(80, 1, p)
#' hl_test(p, y, g = 10)
hl_test <- function(probs, labels, g = 10L, positive = NULL) {
  .assert_scalar_number(g, "g", integer = TRUE, min = 2)
  cleaned <- .clean_binary_inputs(probs, labels, positive = positive)
  if (cleaned$n < 3L || length(unique(cleaned$event)) < 2L) {
    return(list(
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n = as.integer(cleaned$n),
      n_groups = 0L,
      method = "insufficient_data",
      groups = NULL,
      message = "Hosmer-Lemeshow testing requires both outcome classes."
    ))
  }

  group_count <- .hl_group_count(
    cleaned$n,
    g,
    length(unique(cleaned$prob))
  )
  if (group_count < 3L) {
    return(list(
      statistic = NA_real_,
      df = NA_integer_,
      p_value = NA_real_,
      n = as.integer(cleaned$n),
      n_groups = as.integer(group_count),
      method = "custom",
      groups = NULL,
      message = "Fewer than three groups are available."
    ))
  }

  package_result <- NULL
  if (requireNamespace("ResourceSelection", quietly = TRUE)) {
    package_result <- tryCatch(
      suppressWarnings(
        ResourceSelection::hoslem.test(
          cleaned$event,
          cleaned$prob,
          g = group_count
        )
      ),
      error = function(e) NULL
    )
  }
  if (!is.null(package_result)) {
    statistic <- as.numeric(package_result$statistic)
    if (!is.finite(statistic)) {
      return(list(
        statistic = NA_real_,
        df = as.integer(package_result$parameter),
        p_value = NA_real_,
        n = as.integer(cleaned$n),
        n_groups = group_count,
        method = "not_estimable",
        groups = as.data.frame(package_result$observed),
        message = .hl_undefined_message()
      ))
    }
    return(list(
      statistic = statistic,
      df = as.integer(package_result$parameter),
      p_value = as.numeric(package_result$p.value),
      n = as.integer(cleaned$n),
      n_groups = group_count,
      method = "ResourceSelection",
      groups = as.data.frame(package_result$observed),
      message = NULL
    ))
  }

  .custom_hl(cleaned$prob, cleaned$event, group_count)
}
