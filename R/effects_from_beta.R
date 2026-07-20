effects_from_beta <- function(beta_df) {
  sp <- strsplit(beta_df$path, "_to_")
  from <- vapply(sp, `[`, character(1), 1)
  to <- vapply(sp, `[`, character(1), 2)

  vars <- sort(unique(c(from, to)))
  nvar <- length(vars)

  B <- matrix(0, nrow = nvar, ncol = nvar, dimnames = list(vars, vars))

  for (i in seq_len(nrow(beta_df))) {
    B[to[i], from[i]] <- beta_df$beta[i]
  }

  total_mat <- solve(diag(nvar) - B) - diag(nvar)

  res_list <- list()
  k <- 1
  for (i in seq_len(nvar)) {
    for (j in seq_len(nvar)) {
      if (i == j) {
        next
      }

      te <- total_mat[i, j]
      if (abs(te) < 1e-10) {
        next
      }

      de <- B[i, j]
      ie <- te - de

      res_list[[k]] <- data.frame(
        relationships = paste(vars[j], "->", vars[i]),
        direct = de,
        indirect = ie,
        total = te,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }

  res <- do.call(rbind, res_list)
  if (is.null(res)) {
    return(data.frame(
      relationships = character(),
      direct = numeric(),
      indirect = numeric(),
      total = numeric()
    ))
  }

  tmp <- do.call(rbind, strsplit(res$relationships, " -> "))
  res$from <- tmp[, 1]
  res$to <- tmp[, 2]
  res <- res[order(res$from, res$to), ]
  res$from <- NULL
  res$to <- NULL

  rownames(res) <- NULL
  res
}
