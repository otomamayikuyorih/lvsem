measurement_summary <- function(dat, blocks, scores) {
  calc_alpha <- function(x) {
    x <- as.data.frame(x)
    k <- ncol(x)
    total <- rowSums(x)
    k / (k - 1) * (1 - sum(apply(x, 2, stats::var)) / stats::var(total))
  }

  reliability <- do.call(rbind, lapply(names(blocks), function(lv) {
    x <- scale(dat[, blocks[[lv]], drop = FALSE])
    loads <- as.numeric(stats::cor(scores[[lv]], x))
    data.frame(
      block = lv,
      alpha = calc_alpha(x),
      composite_reliability = sum(loads)^2 / (sum(loads)^2 + sum(1 - loads^2)),
      ave = mean(loads^2),
      row.names = NULL
    )
  }))

  loadings <- do.call(rbind, lapply(names(blocks), function(lv) {
    x <- scale(dat[, blocks[[lv]], drop = FALSE])
    data.frame(
      block = lv,
      item = blocks[[lv]],
      loading = as.numeric(stats::cor(scores[[lv]], x)),
      row.names = NULL
    )
  }))

  list(reliability = reliability, loadings = loadings)
}
