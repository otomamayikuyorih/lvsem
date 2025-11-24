fa_adachi <- function(S, q, maxiter = 1000, tol = 1e-6) {
  p <- nrow(S)
  stopifnot(ncol(S) == p)
  
  ## 初期値：Σ の固有値分解
  eig <- eigen(S, symmetric = TRUE)
  L <- eig$vectors[, 1:q, drop = FALSE] %*% 
    diag(sqrt(pmax(eig$values[1:q], 0)), nrow = q)
  
  Psi <- diag(diag(S - L %*% t(L)))
  
  for (iter in 1:maxiter) {
    L_old <- L
    
    ## 1. Psi 更新（対角成分を残差から取る）
    R <- S - L %*% t(L)
    Psi <- diag(pmax(diag(R), 0))  # 負の分散を防ぐため pmax
    
    ## 2. L 更新（Σ - Ψ の rank-q 近似）
    M <- S - Psi
    eigM <- eigen(M, symmetric = TRUE)
    L <- eigM$vectors[, 1:q, drop = FALSE] %*% 
      diag(sqrt(pmax(eigM$values[1:q], 0)), nrow = q)
    
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
