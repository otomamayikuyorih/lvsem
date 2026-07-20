# lvsem

**lvsem** は、事前に計算された潜在変数スコア（latent variable scores）を入力として、  
構造方程式モデル（SEM）を軽量・透明性の高い方法で推定する R パッケージです。  

多くの SEM / PLS-SEM パッケージが内部で複雑な反復アルゴリズムを必要とするのに対し、  
lvsem は **スコアが既に存在することを前提に、SEM の構造部分を回帰モデルとして明示的に推定**します。

## PLS-EMA

`pls_ema()` は Partial Least Squares Exploratory Mediation Analysisを
実装する。`loadings::multipls_geigen()` により、固定目的関数

`cov(Xw, Mc) + cov(Mc, Yu) - cov(Xw, Yu)`

から探索的媒介成分を抽出し、標準化スコア上の間接・直接・総効果と、
成分抽出を毎回やり直すケース・ブートストラップ区間を返す。
