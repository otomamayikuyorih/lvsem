## データ行列 X から共分散行列 S を作るヘルパー
cov_from_data <- function(X, use_correlation = FALSE) {
  X <- scale(X, center = TRUE, scale = !use_correlation)
  if (use_correlation) {
    return(cor(X))
  } else {
    return(cov(X))
  }
}
