.validate_pls_ema_inputs <- function(data, x, mediators, y, br, conf.level) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  groups <- list(x = x, mediators = mediators, y = y)
  if (any(!vapply(groups, is.character, logical(1))) ||
      any(lengths(groups) < 1L)) {
    stop("`x`, `mediators`, and `y` must be non-empty character vectors.")
  }
  variables <- unlist(groups, use.names = FALSE)
  missing <- setdiff(variables, names(data))
  if (length(missing)) {
    stop("Variables not found in `data`: ", paste(missing, collapse = ", "))
  }
  if (anyDuplicated(variables)) {
    stop("`x`, `mediators`, and `y` must contain distinct variables.")
  }
  if (!all(vapply(data[, variables, drop = FALSE], is.numeric, logical(1)))) {
    stop("All PLS-EMA variables must be numeric.")
  }
  if (!is.numeric(br) || length(br) != 1L || br < 0 ||
      br != as.integer(br)) {
    stop("`br` must be a non-negative integer.")
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1) {
    stop("`conf.level` must lie strictly between zero and one.")
  }
}

.pls_ema_fit <- function(x_block, mediator_block, y_block) {
  blocks <- list(x_block, mediator_block, y_block)
  zero_variance <- vapply(
    blocks,
    function(z) any(apply(z, 2L, stats::sd) == 0),
    logical(1)
  )
  if (any(zero_variance)) {
    stop("Every PLS-EMA variable must have nonzero variance.")
  }

  tau <- matrix(
    c(0, 1, -1,
      1, 0, 1,
      -1, 1, 0),
    nrow = 3L,
    byrow = TRUE,
    dimnames = list(c("X", "M", "Y"), c("X", "M", "Y"))
  )
  fit <- loadings::multipls_geigen(
    X = list(exposure = x_block, mediator = mediator_block),
    Y = y_block,
    tau = tau
  )

  score_x <- as.numeric(fit$T[[1L]][, 1L])
  score_m <- as.numeric(fit$T[[2L]][, 1L])
  score_y <- as.numeric(fit$U[, 1L])
  weight_m <- as.numeric(fit$P[[2L]][, 1L])

  # A global sign reversal leaves every pairwise covariance unchanged.
  x_reference <- rowMeans(scale(x_block))
  if (stats::cor(score_x, x_reference) < 0) {
    score_x <- -score_x
    score_m <- -score_m
    score_y <- -score_y
    weight_m <- -weight_m
  }

  scores <- data.frame(
    X = as.numeric(scale(score_x)),
    M = as.numeric(scale(score_m)),
    Y = as.numeric(scale(score_y))
  )
  model_m <- stats::lm(M ~ X, data = scores)
  model_y <- stats::lm(Y ~ X + M, data = scores)
  a <- unname(stats::coef(model_m)[["X"]])
  b <- unname(stats::coef(model_y)[["M"]])
  direct <- unname(stats::coef(model_y)[["X"]])

  list(
    fit = fit,
    scores = scores,
    weights = weight_m,
    loadings = apply(mediator_block, 2L, stats::cor, y = score_m),
    covariance = c(
      cov_X_M = stats::cov(score_x, score_m),
      cov_M_Y = stats::cov(score_m, score_y),
      cov_X_Y = stats::cov(score_x, score_y),
      objective = stats::cov(score_x, score_m) +
        stats::cov(score_m, score_y) -
        stats::cov(score_x, score_y)
    ),
    effects = c(
      path_a = a,
      path_b = b,
      indirect = a * b,
      direct = direct,
      total = direct + a * b
    )
  )
}

#' Partial Least Squares Exploratory Mediation Analysis
#'
#' Extracts a multivariate mediator component from three variable blocks by
#' maximizing
#' \deqn{cov(Xw, Mc) + cov(Mc, Yu) - cov(Xw, Yu).}
#' Standardized score regressions report paths a and b, their product, the
#' direct effect, and the total effect. Each case-bootstrap sample repeats both
#' component extraction and path estimation.
#'
#' PLS-EMA is exploratory. Its path products are not identified causal
#' mediation effects without an appropriate design and assumptions.
#'
#' @param data Data frame containing all analysis variables.
#' @param x Character vector naming exposure-block variables.
#' @param mediators Character vector naming candidate mediator variables.
#' @param y Character vector naming outcome-block variables.
#' @param br Number of case-bootstrap samples. Zero omits bootstrapping.
#' @param seed Random seed.
#' @param conf.level Percentile-bootstrap confidence level.
#' @return An object of class \code{"pls_ema"}.
pls_ema <- function(data, x, mediators, y, br = 1000, seed = 123,
                    conf.level = 0.95) {
  .validate_pls_ema_inputs(data, x, mediators, y, br, conf.level)
  variables <- c(x, mediators, y)
  complete <- stats::complete.cases(data[, variables, drop = FALSE])
  analysis_data <- data[complete, variables, drop = FALSE]
  if (nrow(analysis_data) < 3L) {
    stop("At least three complete observations are required.")
  }

  x_block <- as.matrix(analysis_data[, x, drop = FALSE])
  mediator_block <- as.matrix(analysis_data[, mediators, drop = FALSE])
  y_block <- as.matrix(analysis_data[, y, drop = FALSE])
  original <- .pls_ema_fit(x_block, mediator_block, y_block)
  effects <- data.frame(
    effect = names(original$effects),
    estimate = as.numeric(original$effects),
    stringsAsFactors = FALSE
  )
  bootstrap <- NULL
  failed_bootstrap <- 0L

  if (br > 0L) {
    set.seed(seed)
    bootstrap <- matrix(
      NA_real_, nrow = br, ncol = length(original$effects),
      dimnames = list(NULL, names(original$effects))
    )
    for (b in seq_len(br)) {
      index <- sample.int(nrow(analysis_data), replace = TRUE)
      candidate <- tryCatch(
        .pls_ema_fit(
          x_block[index, , drop = FALSE],
          mediator_block[index, , drop = FALSE],
          y_block[index, , drop = FALSE]
        ),
        error = function(e) NULL
      )
      if (is.null(candidate)) {
        failed_bootstrap <- failed_bootstrap + 1L
      } else {
        bootstrap[b, ] <- candidate$effects
      }
    }
    if (failed_bootstrap == br) stop("All bootstrap samples failed.")
    alpha <- (1 - conf.level) / 2
    effects$boot_se <- apply(bootstrap, 2L, stats::sd, na.rm = TRUE)
    effects$conf_low <- apply(
      bootstrap, 2L, stats::quantile, probs = alpha,
      na.rm = TRUE, names = FALSE
    )
    effects$conf_high <- apply(
      bootstrap, 2L, stats::quantile, probs = 1 - alpha,
      na.rm = TRUE, names = FALSE
    )
  }

  mediator_table <- data.frame(
    variable = mediators,
    weight = original$weights,
    loading = as.numeric(original$loadings),
    stringsAsFactors = FALSE
  )
  mediator_table$abs_loading <- abs(mediator_table$loading)
  mediator_table <- mediator_table[
    order(-mediator_table$abs_loading), , drop = FALSE
  ]
  covariance_table <- data.frame(
    quantity = names(original$covariance),
    estimate = as.numeric(original$covariance),
    stringsAsFactors = FALSE
  )

  out <- list(
    effects = effects,
    mediators = mediator_table,
    covariance = covariance_table,
    scores = original$scores,
    multiset_fit = original$fit,
    bootstrap = bootstrap,
    nobs = nrow(analysis_data),
    complete_cases = complete,
    failed_bootstrap = failed_bootstrap,
    br = as.integer(br),
    conf.level = conf.level,
    variables = list(x = x, mediators = mediators, y = y),
    call = match.call()
  )
  class(out) <- "pls_ema"
  out
}

#' @export
print.pls_ema <- function(x, ...) {
  cat("PLS-EMA: Partial Least Squares Exploratory Mediation Analysis\n")
  cat("Observations:", x$nobs, "\n")
  if (x$br > 0L) {
    cat("Bootstrap samples:", x$br,
        "(failed:", x$failed_bootstrap, ")\n")
  }
  cat("\nObjective covariances\n")
  print(x$covariance, row.names = FALSE, ...)
  cat("\nPLS-score path effects\n")
  print(x$effects, row.names = FALSE, ...)
  cat("\nMediator variables\n")
  print(x$mediators, row.names = FALSE, ...)
  invisible(x)
}
