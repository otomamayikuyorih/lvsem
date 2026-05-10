rm(list=ls(all=TRUE))

## well-beingデータ整形

df0 <- read.csv(
  "C:/Users/hyama/Documents/R/lvsem/data/wellbeing_rev.csv",
   check.names = FALSE, fileEncoding = "CP932"
)

category <- unique(colnames(df0))[-1]
variable <- as.character(df0[1,])[-1]
varname <- colnames(df0)[-1]
  
index <- match(varname,category)

df <- read.csv(
  "C:/Users/hyama/Documents/R/lvsem/data/wellbeing_rev.csv",
  check.names = FALSE, fileEncoding = "CP932",
  skip=1
)
colnames(df) <- NULL

vars <- c(
  "wellBeing", "healthcare", "shoppingDining", "housing", "mobility",
  "recreation", "childcare", "primarySecondaryEdu", "localGovernment",
  "digitalLife", "publicSpace", "urbanLandscape", "naturalLandscape",
  "naturalResources", "environmentalHarmony", "naturalDisasters",
  "accidentsCrime", "communityConnectedness", "diversityTolerance",
  "selfEfficacy", "healthStatus", "cultureArts", "educationalOpportunities",
  "employmentIncome", "entrepreneurship"
)

blocks <- setNames(lapply(1:25, function(i) df[, which(index == i)]), vars)
varname_jp <- setNames(lapply(1:25, function(i) variable[which(index==i)]), category)

# well-beingのデータ
# well-beingのpath


