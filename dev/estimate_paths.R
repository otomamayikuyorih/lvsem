estimate_paths <- function(score_pls, path_matrix, scale_scores = TRUE) {
  
  lv <- as.data.frame(score_pls)
  if (scale_scores) {
    lv <- as.data.frame(scale(lv))
  }
  
  # 列名が未設定なら行名をコピー
  if (is.null(colnames(path_matrix))) {
    colnames(path_matrix) <- rownames(path_matrix)
  }
  
  beta <- c()
  dep_vars <- rownames(path_matrix)
  
  for (dv in dep_vars) {
    preds <- names(which(path_matrix[dv, ] == 1))
    if (length(preds) == 0) next
    
    fml <- as.formula(paste(dv, "~", paste(preds, collapse = " + ")))
    fit <- lm(fml, data = lv)
    cf  <- coef(fit)
    
    for (pv in preds) {
      # ★ここが重要：正しいパス名
      path_name <- paste0(pv, "_to_", dv)
      beta[path_name] <- cf[pv]
    }
  }
  
  data.frame(
    path = names(beta),
    beta = as.numeric(beta),
    row.names = NULL
  )
}
