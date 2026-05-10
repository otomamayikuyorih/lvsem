# install.packages("Rdimtools")
library(Rdimtools)

set.seed(1)
X <- matrix(rnorm(100 * 10), nrow = 100, ncol = 10)

# 次元2までの確率的主成分分析（PPCA）
out <- do.dppca(X, ndim = 2)

Y      <- out$Y          # n × ndim の低次元スコア
W      <- out$projection # p × ndim の射影行列（ローディングに相当）
sigma2 <- out$sigma2     # 推定されたノイズ分散
