rm(list=ls(all=TRUE))
library(loadings)

# --- 0) データ読み込み（plspmと同じ） ---
df <- read.csv(
  "C:/Users/hyama/Documents/R/wellbeingR/data.csv",
  check.names = FALSE, fileEncoding = "UTF-8-BOM"
)

# 0) 列名をユニーク化（A, A.1, A.2 …）
names(df) <- make.unique(names(df), sep=".")

# 1) 基底名でグループ化（末尾 .数字 を剥がす）
base <- sub("[.]\\d+$", "", names(df))
grp  <- split(names(df), base)

# 2) 5ブロック定義（plspm確定版と同じ）
blocks5_names <- list(
  Wellbeing = grp[["幸福度・満足度"]],
  Health    = c(grp[["健康状態"]], grp[["医療・福祉"]]),
  Social    = grp[["地域とのつながり"]],
  Env       = c(grp[["住宅環境"]], grp[["移動・交通"]],
                grp[["公共空間"]], grp[["自然景観"]], grp[["都市景観"]]),
  EcoCult   = c(grp[["雇用・所得"]], grp[["文化・芸術"]],
                grp[["教育機会の豊かさ"]], grp[["買物・飲食"]])
)

# 存在チェック（NULL を落とす）※plspmと同じ
blocks5_names <- lapply(blocks5_names, function(v) v[!is.null(v) & v %in% names(df)])

# --- 1) 対象列だけ抜き出し & 数値化（plspmと同じ） ---
vars <- unique(unlist(blocks5_names))
X <- df[, vars, drop=FALSE]

# 数値化
is_num <- sapply(X, is.numeric)
X[!is_num] <- lapply(X[!is_num], function(z) as.numeric(as.character(z)))

# 非有限は NA に
for (j in names(X)) X[[j]][!is.finite(X[[j]])] <- NA_real_

# 全欠損 or ゼロ分散 or 80%同一値 → 除外し blocks 同期（plspmと同じ）
too_const <- vapply(X, function(v){
  v2 <- v[!is.na(v)]; if (!length(v2)) return(TRUE)
  sd(v2) == 0 || (max(table(v2))/length(v2) >= 0.80)
}, logical(1))
all_na <- vapply(X, function(v) all(is.na(v)), logical(1))

drop_cols <- names(X)[too_const | all_na]
if (length(drop_cols)) {
  message("自動除外: ", paste(drop_cols, collapse = ", "))
  X <- X[, setdiff(names(X), drop_cols), drop = FALSE]
  blocks5_names <- lapply(blocks5_names, function(b) setdiff(b, drop_cols))
}

# 単純インピュテーション（中央値）（plspmと同じ）
X[] <- lapply(X, function(v) ifelse(is.na(v), median(v, na.rm = TRUE), v))

# --- 2) unsv_multipls 用：データ列のリストに変換 ---
blocks5_data <- lapply(blocks5_names, function(cols) as.matrix(X[, cols, drop=FALSE]))

# --- 3) tau（スター型：Wellbeing とだけ結合） ---
m <- length(blocks5_data)
tau <- matrix(0, m, m)
rownames(tau) <- colnames(tau) <- names(blocks5_data)

w <- 1/(m-1)
for (nm in setdiff(names(blocks5_data), "Wellbeing")) {
  tau[nm, "Wellbeing"] <- w
  tau["Wellbeing", nm] <- w
}
diag(tau) <- 0

# --- 4) multiset PLS ---
unsv <- unsv_multipls(blocks5_data, tau)

# --- 5) 潜在スコア（第1成分） ---
score_mat <- do.call(cbind, lapply(unsv$T, function(Ti) Ti[, 1]))
colnames(score_mat) <- names(blocks5_data)
score <- as.data.frame(score_mat)

# --- 6) 構造式（Wellbeing を説明） ---
fit <- lm(Wellbeing ~ Health + Social + Env + EcoCult, data = score)
summary(fit)
