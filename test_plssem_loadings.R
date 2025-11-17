library(plspm)
library(loadings)

## Not run:
## typical example of PLS-PM in customer satisfaction analysis
## model with six LVs and reflective indicators
# load dataset satisfaction
data(satisfaction)

sat_blocks <- list(
  IMAG = satisfaction[, 1:5],
  EXPE = satisfaction[, 6:10],
  QUAL = satisfaction[, 11:15],
  VAL  = satisfaction[, 16:19],
  SAT  = satisfaction[, 20:23],
  LOY  = satisfaction[, 24:27]
)

X_list <- sat_blocks

# path matrix
IMAG = c(0,0,0,0,0,0)
EXPE = c(1,0,0,0,0,0)
QUAL = c(0,1,0,0,0,0)
VAL = c(0,1,1,0,0,0)
SAT = c(1,1,1,1,0,0)
LOY = c(1,0,0,0,1,0)
sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)

tau <- sat_path

unsv <- unsv_multipls(X_list, tau)

score_pls <- cbind(unsv$T[[1]][,1],unsv$T[[2]][,1],unsv$T[[3]][,1],unsv$T[[4]][,1],unsv$T[[5]][,1],unsv$T[[6]][,1])

# plot diagram of path matrix
innerplot(sat_path)
# blocks of outer model
sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
# vector of modes (reflective indicators)
sat_mod = rep("A", 6)
# apply plspm
satpls = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod,
               scaled = TRUE)
# plot diagram of the inner model
innerplot(satpls)
# plot loadings
outerplot(satpls, what = "loadings")
# plot outer weights
outerplot(satpls, what = "weights")

# --------------------------------------------

R <- NULL
for(i in 1:6){
  r <- NULL
  for(j in 1:6){
    r[j] <- cor.test(score_pls[,i],satpls$scores[,j])$estimate
  }
  R <- rbind(R,r)
}

# --------------------------------------------

score_pls <- cbind(unsv$T[[1]][,1],
                   unsv$T[[2]][,1],
                   unsv$T[[3]][,1],
                   unsv$T[[4]][,1],
                   unsv$T[[5]][,1],
                   unsv$T[[6]][,1])

colnames(score_pls) <- c("IMAG","EXPE","QUAL","VAL","SAT","LOY")

#lv <- as.data.frame(score_pls)
lv <- as.data.frame(scale(score_pls))  # ここを変える
#lv <- as.data.frame(scale(satpls$score))  # ここを変える

m_EXPE <- lm(EXPE ~ IMAG, data = lv)
m_QUAL <- lm(QUAL ~ EXPE, data = lv)
m_VAL  <- lm(VAL  ~ EXPE + QUAL, data = lv)
m_SAT  <- lm(SAT  ~ IMAG + EXPE + QUAL + VAL, data = lv)
m_LOY  <- lm(LOY  ~ IMAG + SAT, data = lv)

beta_multiset <- c(
  IMAG_to_EXPE = coef(m_EXPE)["IMAG"],
  EXPE_to_QUAL = coef(m_QUAL)["EXPE"],
  EXPE_to_VAL  = coef(m_VAL)["EXPE"],
  QUAL_to_VAL  = coef(m_VAL)["QUAL"],
  IMAG_to_SAT  = coef(m_SAT)["IMAG"],
  EXPE_to_SAT  = coef(m_SAT)["EXPE"],
  QUAL_to_SAT  = coef(m_SAT)["QUAL"],
  VAL_to_SAT   = coef(m_SAT)["VAL"],
  IMAG_to_LOY  = coef(m_LOY)["IMAG"],
  SAT_to_LOY   = coef(m_LOY)["SAT"]
)

path <- satpls$path_coefs

beta_plspm <- c(
  IMAG_to_EXPE = path["EXPE","IMAG"],
  EXPE_to_QUAL = path["QUAL","EXPE"],
  EXPE_to_VAL  = path["VAL","EXPE"],
  QUAL_to_VAL  = path["VAL","QUAL"],
  IMAG_to_SAT  = path["SAT","IMAG"],
  EXPE_to_SAT  = path["SAT","EXPE"],
  QUAL_to_SAT  = path["SAT","QUAL"],
  VAL_to_SAT   = path["SAT","VAL"],
  IMAG_to_LOY  = path["LOY","IMAG"],
  SAT_to_LOY   = path["LOY","SAT"]
)

res_coef <- data.frame(
  path          = names(beta_multiset),
  beta_multiset = as.numeric(beta_multiset),
  beta_plspm    = as.numeric(beta_plspm)
)

res_coef$diff <- res_coef$beta_multiset - res_coef$beta_plspm

res_coef
