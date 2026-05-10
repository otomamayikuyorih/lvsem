## ダミーデータ
set.seed(1)
X <- matrix(rnorm(250 * 10), nrow = 250, ncol = 10)
S <- cov_from_data(X)  # 共分散行列

q <- 3  # 因子数

fit_adachi  <- fa_adachi(S, q)
fit_socan   <- fa_socan(S, q)
fit_unkel   <- fa_unkel_simple(S, q)

str(fit_adachi)
str(fit_socan)
str(fit_unkel)
