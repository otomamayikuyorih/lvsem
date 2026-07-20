make_lv_scores <- function(dat, blocks, path_matrix) {
  block_data <- lapply(blocks, function(vars) {
    x <- dat[, vars, drop = FALSE]
    x[] <- lapply(x, function(v) {
      v <- as.numeric(v)
      v[!is.finite(v)] <- NA_real_
      if (anyNA(v)) {
        v[is.na(v)] <- stats::median(v, na.rm = TRUE)
      }
      v
    })
    scale(x)
  })

  multipls <- loadings::unsv_multipls(block_data, path_matrix)
  scores <- as.data.frame(do.call(cbind, lapply(multipls$T, function(x) x[, 1])))
  names(scores) <- names(blocks)

  for (lv in names(blocks)) {
    avg_cor <- mean(stats::cor(scores[[lv]], block_data[[lv]]))
    if (is.finite(avg_cor) && avg_cor < 0) {
      scores[[lv]] <- -scores[[lv]]
    }
  }

  scores
}
