fa_unkel_simple <- function(S, q, maxiter = 1000, tol = 1e-6) {
  p <- nrow(S)
  stopifnot(ncol(S) == p)
  
  ## 初期値：S の rank-q SVD から L を作る
  sv <- svd(S)
  L <- sv$u[, 1:q, drop = FALSE] %*% 
    diag(sqrt(pmax(sv$d[1:q], 0)), nrow = q)
  
  ## 列方向に正規化（因子のスケールを揃える）
  col_norms <- sqrt(colSums(L^2))
  L <- sweep(L, 2, col_norms, "/")
  
  Psi <- diag(diag(S - L %*% t(L)))
  
  for (iter in 1:maxiter) {
    L_old <- L
    
    ## 1. Psi 更新（残差の対角）
    R <- S - L %*% t(L)
    Psi <- diag(pmax(diag(R), 0))
    
    ## 2. L 更新：M = S - Psi の rank-q SVD
    M <- S - Psi
    svM <- svd(M)
    L <- svM$u[, 1:q, drop = FALSE] %*% 
      diag(sqrt(pmax(svM$d[1:q], 0)), nrow = q)
    
    ## 3. L の列方向を正規化
    col_norms <- sqrt(colSums(L^2))
    L <- sweep(L, 2, col_norms, "/")
    
    ## 収束判定
    if (max(abs(L - L_old)) < tol) {
      break
    }
  }
  
  Sigma_hat <- L %*% t(L) + Psi
  
  list(
    L = L,
    Psi = Psi,
    Sigma_hat = Sigma_hat,
    iter = iter,
    converged = (iter < maxiter)
  )
}
