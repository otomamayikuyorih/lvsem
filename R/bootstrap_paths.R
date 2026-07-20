bootstrap_paths <- function(dat, blocks, path_matrix, br = 1000, seed = 123) {
  set.seed(seed)
  original_scores <- make_lv_scores(dat, blocks, path_matrix)
  original_beta <- estimate_paths(original_scores, path_matrix)

  boot_mat <- matrix(NA_real_, nrow = br, ncol = nrow(original_beta))
  colnames(boot_mat) <- original_beta$path

  for (b in seq_len(br)) {
    idx <- sample(seq_len(nrow(dat)), replace = TRUE)
    boot_scores <- make_lv_scores(dat[idx, , drop = FALSE], blocks, path_matrix)
    boot_beta <- estimate_paths(boot_scores, path_matrix)
    boot_mat[b, boot_beta$path] <- boot_beta$beta
  }

  se <- apply(boot_mat, 2, stats::sd, na.rm = TRUE)
  data.frame(
    path = original_beta$path,
    beta = original_beta$beta,
    boot_se = se[original_beta$path],
    t_value = abs(original_beta$beta / se[original_beta$path]),
    row.names = NULL
  )
}
