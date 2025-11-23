# beta_df : estimate_paths() の結果（列: path, beta）
#           path は "IMAG_to_EXPE" のような文字列を想定
#
# 戻り値 : plspm::summary(satpls)$effects と似た形式の data.frame
#          relationships, direct, indirect, total

effects_from_beta <- function(beta_df) {
  ## 1) "IMAG_to_EXPE" を from, to に分解 ------------------------------
  sp  <- strsplit(beta_df$path, "_to_")
  from <- vapply(sp, `[`, character(1), 1)
  to   <- vapply(sp, `[`, character(1), 2)
  
  vars <- sort(unique(c(from, to)))
  nvar <- length(vars)
  
  ## 2) 係数行列 B を作る（行 = to, 列 = from） -----------------------
  B <- matrix(0, nrow = nvar, ncol = nvar,
              dimnames = list(vars, vars))
  
  for (i in seq_len(nrow(beta_df))) {
    B[to[i], from[i]] <- beta_df$beta[i]
  }
  
  ## 3) total effects の行列を計算 -----------------------------------
  # 線形モデルの理論より：
  #   Total = (I - B)^(-1) - I
  # で、直接＋間接の総効果が計算できる
  I <- diag(nvar)
  Total_mat <- solve(I - B) - I
  
  ## 4) long 形式に変換（from -> to） --------------------------------
  res_list <- list()
  k <- 1
  for (i in seq_len(nvar)) {        # 行 = to
    for (j in seq_len(nvar)) {      # 列 = from
      if (i == j) next              # 自己効果は除外
      te <- Total_mat[i, j]
      if (abs(te) < 1e-10) next     # 実質 0 は無視
      
      de <- B[i, j]                 # direct（なければ 0）
      ie <- te - de                 # indirect = total - direct
      
      res_list[[k]] <- data.frame(
        relationships = paste(vars[j], "->", vars[i]),
        direct  = de,
        indirect = ie,
        total    = te,
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
  
  res <- do.call(rbind, res_list)
  
  # 見やすさのために from, to でソート
  tmp <- do.call(rbind, strsplit(res$relationships, " -> "))
  res$from <- tmp[,1]
  res$to   <- tmp[,2]
  res <- res[order(res$from, res$to), ]
  res$from <- res$to <- NULL
  
  rownames(res) <- NULL
  res
}
