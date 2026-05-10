fa_socan <- function(S, q, maxiter = 1000, tol = 1e-6) {
  p <- nrow(S)
  stopifnot(ncol(S) == p)
  
  ## 初期値：S の rank-q SVD から L を作る
  sv <- svd(S)
  L <- sv$u[, 1:q, drop = FALSE] %*% 
    diag(sqrt(pmax(sv$d[1:q], 0)), nrow = q)
  
  Psi <- diag(diag(S - L %*% t(L)))
  
  for (iter in 1:maxiter) {
    L_old <- L
    
    ## 1. Psi 更新
    R <- S - L %*% t(L)
    Psi <- diag(pmax(diag(R), 0))
    
    ## 2. L 更新：M = S - Psi を rank-q 近似（SVD）
    M <- S - Psi
    svM <- svd(M)
    L <- svM$u[, 1:q, drop = FALSE] %*% 
      diag(sqrt(pmax(svM$d[1:q], 0)), nrow = q)
    
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
