# lvsem

`lvsem` は、マルチセットPartial Least Squares（PLS）で構成した潜在変数
スコアを用いて、構造方程式モデルの経路を軽量かつ透明に推定するRパッケージ
です。

潜在変数スコアの作成、構造経路の回帰推定、直接・間接・総効果の分解、
ブートストラップ、測定モデルの要約に加えて、探索的媒介分析
PLS-EMA（Partial Least Squares Exploratory Mediation Analysis）を提供します。

## 特徴

- 複数の観測項目からマルチセットPLS潜在変数スコアを作成
- 潜在変数間の構造経路を通常最小二乗回帰で明示的に推定
- 直接効果、間接効果、総効果を計算
- ケース・ブートストラップにより経路の標準誤差を評価
- loading、Cronbachのα、composite reliability、AVEを要約
- PLS-EMAにより多数の媒介候補から媒介構造と整合する成分を探索

## インストール

ソースアーカイブからインストールできます。

```r
install.packages(
  "lvsem_0.1.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

開発用ソースディレクトリからインストールする場合は次のようにします。

```r
install.packages("lvsem", repos = NULL, type = "source")
```

`lvsem` はマルチセットPLSの計算に
[`loadings`](https://cran.r-project.org/package=loadings) パッケージを使用します。

## 主要関数

| 関数 | 内容 |
|---|---|
| `make_lv_scores()` | 観測変数ブロックから潜在変数スコアを作成 |
| `estimate_paths()` | 潜在変数スコア間の構造経路を推定 |
| `bootstrap_paths()` | スコアと経路を再推定するケース・ブートストラップ |
| `effects_from_beta()` | 直接・間接・総効果を計算 |
| `measurement_summary()` | loading、信頼性、AVEを要約 |
| `pls_ema()` | Partial Least Squaresによる探索的媒介分析 |

## 基本的なPLS-SEM

観測変数のデータフレーム、潜在変数ごとの項目リスト、構造パス行列を
用意します。パス行列では、行を結果側、列を説明側とします。

```r
library(lvsem)

blocks <- list(
  X = c("x1", "x2", "x3"),
  M = c("m1", "m2", "m3"),
  Y = c("y1", "y2", "y3")
)

path_matrix <- rbind(
  X = c(X = 0, M = 0, Y = 0),
  M = c(X = 1, M = 0, Y = 0),
  Y = c(X = 1, M = 1, Y = 0)
)

scores <- make_lv_scores(
  dat = dat,
  blocks = blocks,
  path_matrix = path_matrix
)

paths <- estimate_paths(scores, path_matrix)
effects <- effects_from_beta(paths)
measurement <- measurement_summary(dat, blocks, scores)
```

経路の標本変動を評価する場合は、ケース・ブートストラップを使用します。

```r
boot <- bootstrap_paths(
  dat = dat,
  blocks = blocks,
  path_matrix = path_matrix,
  br = 1000,
  seed = 123
)
```

## PLS-EMA

PLS-EMAは、説明変数ブロック \(X\)、媒介候補ブロック \(M\)、結果ブロック
\(Y\) から、媒介構造と整合する多変量成分を探索します。

固定目的関数は次のとおりです。

\[
\operatorname{cov}(Xw,Mc)
+
\operatorname{cov}(Mc,Yu)
-
\operatorname{cov}(Xw,Yu)
\]

成分抽出には `loadings::multipls_geigen()` を使用します。抽出後の3スコアを
標準化し、経路 \(a\)、経路 \(b\)、間接効果 \(ab\)、直接効果、総効果を
計算します。ブートストラップでは、各標本についてPLS成分から再推定します。

### 顧客満足度データの例

```r
library(lvsem)
data("satisfaction", package = "plspm")

names(satisfaction)[6:10] <- paste0("EXPE", 1:5)
names(satisfaction)[11:15] <- paste0("QUAL", 1:5)
names(satisfaction)[16:19] <- paste0("VAL", 1:4)
names(satisfaction)[20:23] <- paste0("SAT", 1:4)

fit <- pls_ema(
  data = satisfaction,
  x = paste0("EXPE", 1:5),
  mediators = c(paste0("QUAL", 1:5), paste0("VAL", 1:4)),
  y = paste0("SAT", 1:4),
  br = 1000,
  seed = 20260720
)

print(fit)
```

返り値には次の要素が含まれます。

| 要素 | 内容 |
|---|---|
| `effects` | 経路 \(a\)、\(b\)、間接・直接・総効果と区間 |
| `mediators` | 媒介候補のweightとloading |
| `covariance` | 3つのブロック間共分散と目的関数値 |
| `scores` | 標準化された \(X\)、\(M\)、\(Y\) スコア |
| `bootstrap` | ブートストラップ効果推定値 |
| `multiset_fit` | `loadings` が返すマルチセットPLS結果 |

## 解釈上の注意

PLS-EMAは探索的な成分抽出法です。結果ブロック \(Y\) の情報を使って媒介成分
を構成するため、通常の確認的PLS-SEM媒介分析や、今井らの因果媒介分析とは
異なります。

PLS-EMAが報告する \(ab\) は、抽出されたPLSスコア上の記述的な間接効果です。
適切な研究デザイン、時間順序、交絡に関する識別仮定なしに、ACMEなどの
因果媒介効果として解釈することはできません。

## パッケージ構成

```text
lvsem/
├── DESCRIPTION
├── NAMESPACE
├── R/
├── man/
└── tests/
```

## ライセンス

MIT License
