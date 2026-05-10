rm(list=ls(all=TRUE))

library(loadings)
library(plspm)

data(satisfaction)

blocks <- list(
  IMAG = satisfaction[, 1:5],   # イメージ
  EXPE = satisfaction[, 6:10],  # 経験・期待
  QUAL = satisfaction[, 11:15], # 品質評価
  VAL  = satisfaction[, 16:19], # 価値
  SAT  = satisfaction[, 20:23], # 満足度
  LOY  = satisfaction[, 24:27]  # ロイヤルティ
)

# path matrix
IMAG <- c(0,0,0,0,0,0)
EXPE <- c(1,0,0,0,0,0)
QUAL <- c(0,1,0,0,0,0)
VAL <- c(0,1,1,0,0,0)
SAT <- c(1,1,1,1,0,0)
LOY <- c(1,0,0,0,1,0)
path <- rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)

### マルチセットPLS
unsv <- unsv_multipls(blocks, path)
score_pls <- sapply(unsv$T, function(x) x[,1])
colnames(score_pls) <- names(blocks)

source("C:/Users/hyama/Documents/R/lvsem/estimate_paths.R")
b <- estimate_paths(score_pls,path)
print(b)

source("C:/Users/hyama/Documents/R/lvsem/effects_from_beta.R")
d <- effects_from_beta(b)

### PLS-SEM
sat_blocks <- list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
sat_mod <- rep("A", 6)
satpls <- plspm(satisfaction, path, sat_blocks, modes = sat_mod, scaled = TRUE)

satpls$effects[satpls$effects$direct!=0,]
