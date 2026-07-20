test_that("pls_ema uses the fixed covariance objective", {
  set.seed(11)
  n <- 100
  lx <- rnorm(n)
  lm <- 0.7 * lx + rnorm(n, sd = 0.6)
  ly <- 0.8 * lm + rnorm(n, sd = 0.6)
  dat <- data.frame(
    x1 = lx + rnorm(n, sd = 0.1),
    x2 = lx + rnorm(n, sd = 0.1),
    m1 = lm + rnorm(n, sd = 0.1),
    m2 = lm + rnorm(n, sd = 0.1),
    y1 = ly + rnorm(n, sd = 0.1),
    y2 = ly + rnorm(n, sd = 0.1)
  )
  fit <- pls_ema(
    dat, c("x1", "x2"), c("m1", "m2"), c("y1", "y2"),
    br = 25, seed = 9
  )
  covs <- setNames(fit$covariance$estimate, fit$covariance$quantity)
  expect_s3_class(fit, "pls_ema")
  expect_equal(
    unname(covs["objective"]),
    unname(covs["cov_X_M"] + covs["cov_M_Y"] - covs["cov_X_Y"])
  )
  expect_gt(fit$effects$estimate[fit$effects$effect == "indirect"], 0)
  expect_equal(nrow(fit$bootstrap), 25)
  expect_equal(fit$failed_bootstrap, 0)
})

test_that("pls_ema validates inputs and handles complete cases", {
  set.seed(12)
  dat <- as.data.frame(matrix(rnorm(80), ncol = 4))
  names(dat) <- c("x", "m1", "m2", "y")
  dat$m1[1] <- NA_real_
  fit <- pls_ema(dat, "x", c("m1", "m2"), "y", br = 0)
  expect_equal(fit$nobs, nrow(dat) - 1)
  expect_null(fit$bootstrap)
  expect_error(pls_ema(dat, "x", "missing", "y"), "not found")
  expect_error(pls_ema(dat, "x", "m1", "x"), "distinct")
})
