# ===== 高松市データ PLS-SEM：確定版スクリプト =====
library(plspm)

# --- 0) データ（必要なら読込を有効化） ---
# df <- read.csv("C:/Users/hyama/Documents/R/wellbeingR/data.csv",
#                check.names = FALSE, fileEncoding = "UTF-8-BOM")

# --- 1) 測定（外部）モデル：列名で指定（.1 は除外） ---
# 0) 列名をユニーク化（A, A.1, A.2 … を自動付与）
names(df) <- make.unique(names(df), sep=".")

# 1) 基底名でグループ化（末尾の .数字 を剥がす）
base <- sub("[.]\\d+$", "", names(df))
grp  <- split(names(df), base)

# 2) ブロック（観測変数の束）を超シンプルに定義
blocks <- list(
  Wellbeing      = grp[["幸福度・満足度"]],
  Health         = c(grp[["健康状態"]], grp[["医療・福祉"]]),
  Social         = grp[["地域とのつながり"]],
  Environment    = c(grp[["住宅環境"]], grp[["移動・交通"]],
                     grp[["公共空間"]], grp[["自然景観"]], grp[["都市景観"]]),
  EconomyCulture = c(grp[["雇用・所得"]], grp[["文化・芸術"]],
                     grp[["教育機会の豊かさ"]], grp[["買物・飲食"]])
)
# 存在チェック（NULL を落とす）
blocks <- lapply(blocks, function(v) v[!is.null(v) & v %in% names(df)])
modes  <- rep("A", length(blocks))

# 3) 対象列だけ抜き出し & 数値化（簡易）
vars <- unique(unlist(blocks))
X <- df[, vars, drop=FALSE]

# --- 2) 前処理（型→数値、欠損・定数対策） ---
# 数値化
is_num <- sapply(X, is.numeric)
X[!is_num] <- lapply(X[!is_num], function(z) as.numeric(as.character(z)))
# 非有限はNAに
for (j in names(X)) X[[j]][!is.finite(X[[j]])] <- NA_real_
# 全欠損 or ゼロ分散 or 80%同一値 → 除外し blocks 同期
too_const <- vapply(X, function(v){
  v2 <- v[!is.na(v)]; if (!length(v2)) return(TRUE)
  sd(v2) == 0 || (max(table(v2))/length(v2) >= 0.80)
}, logical(1))
all_na <- vapply(X, function(v) all(is.na(v)), logical(1))
drop_cols <- names(X)[too_const | all_na]
if (length(drop_cols)) {
  message("自動除外: ", paste(drop_cols, collapse = ", "))
  X <- X[, setdiff(names(X), drop_cols), drop = FALSE]
  blocks <- lapply(blocks, function(b) setdiff(b, drop_cols))
}
# 単純インピュテーション（中央値）
X[] <- lapply(X, function(v) ifelse(is.na(v), median(v, na.rm = TRUE), v))

# --- 3) 構造（内部）モデル：下三角で Wellbeing を最後に ---
# 行 = 従属, 列 = 説明。対角線より左（下三角）に 1 を置く。
Health    <- c(0,0,0,0,0)
Social    <- c(0,0,0,0,0)
Env       <- c(0,0,0,0,0)
EcoCult   <- c(0,0,0,0,0)
Wellbeing <- c(1,1,1,1,0)  # Health/Social/Env/EcoCult → Wellbeing
path_matrix <- rbind(Health, Social, Env, EcoCult, Wellbeing)
colnames(path_matrix) <- rownames(path_matrix) <- c("Health","Social","Env","EcoCult","Wellbeing")

# --- 4) 推定（まず非標準化→標準化） ---
res0 <- plspm(X, path_matrix, blocks, modes = modes, scaled = FALSE)
res  <- plspm(X, path_matrix, blocks, modes = modes, scaled = TRUE)

# --- 5) 主な結果 ---
print(res$path_coefs)   # Health/Social/Env/EcoCult → Wellbeing
print(res$inner_model)  # R2 など

# 図（余白エラー対策）
op <- par(no.readonly = TRUE); par(mar = c(4,4,1,1))
outerplot(res, what = "loadings")
innerplot(res)
par(op)

# --- 6) （任意）ブートストラップ ---
# resb <- plspm(X, path_matrix, blocks, modes = modes, scaled = TRUE, boot.val = TRUE, br = 500)
# resb$boot$paths; resb$boot$loadings
