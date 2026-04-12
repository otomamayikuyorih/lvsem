# PLS-SEMの社会科学応用論文調査ノート

更新日: 2026-04-13  
目的: PLS-SEM（Partial Least Squares Structural Equation Modeling）が社会科学でどのように応用されているかを、実証研究中心に俯瞰できるように整理する。  
範囲: 教育、情報システム、SNS・プライバシー、観光、サステナビリティ消費、公共部門、組織行動、金融行動。  
注意: これは厳密なPRISMA準拠レビューではなく、主要レビュー論文と代表的な応用研究を組み合わせた実務的サーベイである。

## 1. 先に結論

- 社会科学でのPLS-SEMは、`複雑な媒介・調整モデル`、`潜在変数が多い理論モデル`、`予測志向の研究`、`形成的指標や高次構成概念` を扱うときに特によく使われる。
- 応用が厚い分野は、`情報システム`、`教育工学・高等教育`、`観光・ホスピタリティ`、`消費者行動・サステナビリティ`。
- 近年は、PLS-SEM単独よりも `PLS-SEM + fsQCA`、`PLS-SEM + ANN`、`PLSpredict`、`IPMA`、`多群分析` のような拡張的な使い方が増えている。
- 2023年以降のレビューでは、社会科学系でも `なぜPLS-SEMを選んだのか`、`予測評価をしたか`、`形成的測定を適切に評価したか` が質の分かれ目として強調されている。

## 2. 分野別マップ

| 分野 | 典型テーマ | PLS-SEMが使われやすい理由 | 代表論文 |
| --- | --- | --- | --- |
| 情報システム | 技術受容、信頼、継続利用、プライバシー、フィンテック | TAM/UTAUT/期待確認モデルなどの複雑な媒介構造と予測重視 | Sharma et al. (2018), Nemec Zlatolas et al. (2019), Wei et al. (2025) |
| 教育 | 学生満足、e-learning受容、ブレンド学習、学習動機 | 潜在変数が多く、心理・制度・サービス要因を統合しやすい | Haverila et al. (2021), Huang (2021), Singh et al. (2024) |
| 観光・ホスピタリティ | ブランド価値、満足、忠誠、住民態度、環境配慮行動 | 高次概念や連鎖的媒介モデルを組みやすい | Abbasi et al. (2024), Li et al. (2023) |
| サステナビリティ消費 | グリーン購買意図、e-WOM、規範、CSR | 心理・社会規範・企業要因の統合に向く | Munerah et al. (2021), Parveen & Chaudhary (2025) |
| 公共部門 | 電子政府、市民満足、信頼 | 説明と予測を比較しやすい | Sharma et al. (2018) |
| 組織行動 | CSR、仕事エンゲージメント、組織エンゲージメント | 媒介や予測評価との相性が良い | Low & Memon (2023) |
| 心理・デジタル行動 | 自己開示、ウェルビーイング、心理的苦痛 | 観測しづらい構成概念を統合しやすい | Nemec Zlatolas et al. (2019), Shu et al. (2025) |

## 3. 分野別の重要レビュー

### 3.1 情報システム

1. Misty Sabol, Joe Hair, Gabriel Cepeda-Carrion, José L. Roldán, Alain Yee Loong Chong (2023)  
   `PLS-SEM in information systems: seizing the opportunity and marching ahead full speed to adopt methodological updates`  
   Industrial Management & Data Systems, 123(12), 2997-3017.  
   DOI: https://doi.org/10.1108/IMDS-07-2023-0429  
   メモ: 2012-2022年のIS研究135本を対象に、PLS-SEMの成熟化を確認。`予測評価`、`高次概念`、`媒介・調整`、`競合モデル比較` が重要だと整理している。

2. Sarah W. Mahamadou Kante, Babri Michel (2023)  
   `Use of partial least squares structural equation modelling (PLS-SEM) in privacy and disclosure research on social network sites: A systematic review`  
   Computers in Human Behavior Reports, 10, 100291.  
   DOI: https://doi.org/10.1016/j.chbr.2023.100291  
   メモ: SNS上の自己開示・プライバシー研究21本をレビュー。高度なPLS-SEM手法はまだ十分使われていないと指摘。

### 3.2 教育

3. Servet Demir, Muhammet Uşak (2025)  
   `Analyzing the Implementation of PLS-SEM in Educational Technology Research: A Review of the Past 10 Years`  
   SAGE Open, 15(2).  
   DOI: https://doi.org/10.1177/21582440251345950  
   メモ: 2013-2023年の57本をレビュー。主要テーマは `technology adoption and use`、`online learning systems`、`performance and satisfaction`。一方で `形成的モデル評価` と `予測関連指標` は弱い。

### 3.3 観光・ホスピタリティ

4. Ahmet Usakli, Kemal Gurkan Kucukergin (2018)  
   `Using partial least squares structural equation modeling in hospitality and tourism`  
   International Journal of Contemporary Hospitality Management, 30(11), 3462-3512.  
   DOI: https://doi.org/10.1108/IJCHM-11-2017-0753  
   メモ: 2000年から2017年4月までの206本をレビュー。形成的測定と構造モデル評価の誤用がまだ多いと報告。

### 3.4 社会科学一般の方法選択

5. Ganesh Dash, Justin Paul (2021)  
   `CB-SEM vs PLS-SEM methods for research in social sciences and technology forecasting`  
   Technological Forecasting and Social Change, 173, 121092.  
   DOI: https://doi.org/10.1016/j.techfore.2021.121092  
   メモ: 社会科学でのPLS-SEMとCB-SEMの使い分けに便利。論文冒頭の方法選択の根拠づけに使いやすい。

6. Joe F. Hair Jr., Jeffrey J. Risher, Marko Sarstedt, Christian M. Ringle (2019)  
   `When to use and how to report the results of PLS-SEM`  
   European Business Review, 31(1), 2-24.  
   DOI: https://doi.org/10.1108/EBR-11-2018-0203  
   メモ: 社会科学の実証論文を書くときの最重要ガイドの一つ。サンプルサイズ、評価指標、PLSpredict、モデル比較の説明がコンパクト。

## 4. 代表的な応用論文

### 4.1 教育・高等教育

1. Matti Haverila, Kai Haverila, Caitlin McLaughlin, Mehak Arora (2021)  
   `Towards a comprehensive student satisfaction model`  
   The International Journal of Management Education, 19(3), 100558.  
   DOI: https://doi.org/10.1016/j.ijme.2021.100558  
   使いどころ: 高等教育で `学生満足` を多面的に測りたいときの代表例。  
   要点: 教育サービスと非教育サービスを統合した満足モデルを構築し、`educational service quality` と `value for money` が強い影響を持つと報告。

2. Chun-Hsiung Huang (2021)  
   `Using PLS-SEM Model to Explore the Influencing Factors of Learning Satisfaction in Blended Learning`  
   Education Sciences, 11(5), 249.  
   DOI: https://doi.org/10.3390/educsci11050249  
   使いどころ: ブレンド学習における `有用性`、`使いやすさ`、`学習動機` と満足の関係を見るときに近い。  
   要点: 学習満足の説明にTAM系の変数を接続した比較的教科書的な応用例。

3. Harendra Singh, Vikrant Vikram Singh, Aditya Kumar Gupta, P. K. Kapur et al. (2024)  
   `Assessing e-learning platforms in higher education with reference to student satisfaction: a PLS-SEM approach`  
   International Journal of System Assurance Engineering and Management, 15, 4885-4896.  
   DOI: https://doi.org/10.1007/s13198-024-02497-3  
   使いどころ: インド高等教育におけるオンライン教育の満足要因の整理。  
   要点: e-learningの質評価と学生満足を結びつけており、パンデミック後の教育研究の参照点として便利。

### 4.2 情報システム・SNS・デジタル行動

4. Lili Nemec Zlatolas, Tatjana Welzer, Marko Hölbl, Marjan Heričko, Aida Kamišalić (2019)  
   `A Model of Perception of Privacy, Trust, and Self-Disclosure on Online Social Networks`  
   Entropy, 21(8), 772.  
   DOI: https://doi.org/10.3390/e21080772  
   使いどころ: SNSにおける `プライバシー認知`、`信頼`、`自己開示` の関係を扱う定番テーマ。  
   要点: Facebook利用者602名の調査に基づき、`privacy value`、`privacy risk`、`trust`、`privacy concerns`、`self-disclosure` を統合したモデルを検証。

5. Pratyush Nidhi Sharma, Forrest V. Morgeson III, Sunil Mithas, Salman Aljazzaf (2018)  
   `An empirical and comparative analysis of E-government performance measurement models: Model selection via explanation, prediction, and parsimony`  
   Government Information Quarterly, 35(4), 515-535.  
   DOI: https://doi.org/10.1016/j.giq.2018.07.003  
   使いどころ: 公共部門や行政満足研究で、`説明重視` と `予測重視` のモデル比較をしたい場合。  
   要点: 電子政府の `quality-satisfaction-trust` 関係を比較し、何が説明に向くか、何が予測に向くかを切り分けている。

6. Na Wei, Yikai Liang, Haiqing Wang, Mengqing Liu et al. (2025)  
   `Analysis of mobile fintech adoption based on perceived value and risk theory: findings from PLS-SEM and fsQCA`  
   Humanities and Social Sciences Communications, 12, 973.  
   DOI: https://doi.org/10.1038/s41599-025-05142-x  
   使いどころ: フィンテックやデジタル金融サービスの受容研究。  
   要点: 価値とリスク理論を統合し、中国利用者の採用意図を分析。PLS-SEMで説明力を確認しつつ、fsQCAで複数経路を示している。

7. Bo Shu, Zhigao Dong, Fang Su, Zheng Wang (2025)  
   `Social media sharing, psychological distress, and student well-being: a PLS-SEM and fsQCA analysis of Chinese college students`  
   Frontiers in Psychology, 16, 1554882.  
   DOI: https://doi.org/10.3389/fpsyg.2025.1554882  
   使いどころ: SNS利用、精神的苦痛、学生ウェルビーイングのような心理・教育横断研究。  
   要点: 2時点・534名データをもとに、ソーシャルメディア共有行動、心理的苦痛、支援知覚、ウェルビーイングの関係を分析。

### 4.3 観光・ホスピタリティ

8. Amir Zaib Abbasi, Raouf Ahmad Rather, Ding Hooi Ting, Saima Nisar, Khalil Hussain, Muddasar Ghani Khwaja, Amjad Shamim (2024)  
   `Exploring tourism-generated social media communication, brand equity, satisfaction, and loyalty: A PLS-SEM-based multi-sequential approach`  
   Journal of Vacation Marketing, 30(1).  
   DOI: https://doi.org/10.1177/13567667221118651  
   使いどころ: 観光地ブランド、SNSコミュニケーション、満足、忠誠の連鎖モデル。  
   要点: 目的地マーケティング組織発信と観光客発信のSNSコミュニケーションが `brand awareness`、`image`、`perceived quality`、`satisfaction`、`loyalty` にどうつながるかを多段階で検証。

9. Jiaqi Li, Timothy J. Lee, Nan Chen, Keun-Soo Park (2023)  
   `Pro-environmental behaviour of the residents in sensitive tourism destinations`  
   Journal of Vacation Marketing, 29(2).  
   DOI: https://doi.org/10.1177/13567667221101406  
   使いどころ: 観光地住民の `環境配慮行動` と `持続可能な観光` を結びたいとき。  
   要点: 住民の環境知識、知覚された公正、満足、場所愛着が、環境配慮行動につながる構造を検証。

### 4.4 サステナビリティ・消費者行動

10. Siti Munerah, Kian Yeik Koay, Seethaletchumy Thambiah (2021)  
    `Factors influencing non-green consumers’ purchase intention: A partial least squares structural equation modelling (PLS-SEM) approach`  
    Journal of Cleaner Production, 280(1), 124192.  
    DOI: https://doi.org/10.1016/j.jclepro.2020.124192  
    使いどころ: グリーン購買意図研究の基礎例。  
    要点: `Norm Activation Theory` に `social norm` と `environmental corporate social responsibility` を加え、非グリーン消費者がグリーン美容製品を買う意図を説明。

11. Afsa Parveen, Rashmi Chaudhary (2025)  
    `Do Attitude and Subjective Norm Mediate the Relationship Between Social Media e-WOM and Green Purchase Intention? An Empirical Investigation Using PLS-SEM`  
    Vikalpa: The Journal for Decision Makers, 50(1).  
    DOI: https://doi.org/10.1177/02560909241297015  
    使いどころ: SNS上の口コミ、主観的規範、態度、購買意図の媒介モデル。  
    要点: `e-WOM usefulness`、`trust`、`credibility` から `e-WOM adoption` を経由し、`attitude` と `subjective norm` がグリーン購買意図を部分媒介することを示す。

### 4.5 組織行動・HRM

12. Mei Peng Low, Mumtaz Ali Memon (2023)  
    `The interrelations between micro-level CSR, Job engagement and organizational engagement during the COVID-19 pandemic: A PLSpredict model assessment`  
    Human Systems Management, 42(4).  
    DOI: https://doi.org/10.3233/HSM-220086  
    使いどころ: 組織内CSR、従業員エンゲージメント、組織レジリエンスを扱う研究。  
    要点: パンデミック下で、`micro-level CSR` が `job engagement` と `organizational engagement` にどう関わるかを検証し、PLSpredictによる予測評価も実施。

## 5. いま社会科学で見えている傾向

### 5.1 PLS-SEMが特に向いている研究デザイン

- `複数の媒介変数を含む理論モデル`
- `心理・制度・技術要因を一つのモデルに入れる研究`
- `形成的構成概念` や `高次概念` を含むモデル
- `説明` だけでなく `予測` も見たい研究
- `fsQCA` や `ANN` などと組み合わせた複線的分析

### 5.2 よくある理論枠組み

- `TAM / UTAUT / Expectation-Confirmation Model`
- `Theory of Planned Behavior`
- `Norm Activation Theory`
- `Social Exchange Theory`
- `Brand equity / service quality / satisfaction / loyalty` の連鎖モデル

### 5.3 最近の方法論的アップデート

- `PLSpredict` を使った外的予測力の確認
- `IPMA` による重要度とパフォーマンスの分解
- `多群分析` による属性差の検討
- `fsQCA` 併用による「複数の成功パターン」の把握
- `CB-SEMとの役割分担` を明示する書き方

## 6. 研究設計のヒント

### 6.1 社会科学でPLS-SEMを選ぶと説得力が出やすい状況

- 仮説が `予測志向` である
- モデルが `複雑` で、複数の媒介や調整を含む
- 変数の一部が `形成的` である
- 実務データで `正規性` が弱い、またはサンプルに制約がある
- 教育・観光・SNS行動のように、`潜在概念の連鎖構造` を検証したい

### 6.2 逆に注意すべき点

- 「サンプルが小さいからPLS-SEM」というだけでは弱い
- 形成的指標の評価を省略しない
- `R^2` だけで満足せず `predictive relevance` や `PLSpredict` を見る
- 共分散ベースSEMより優れていると一般化しない
- 論文内で `なぜCB-SEMではなくPLS-SEMか` を必ず説明する

## 7. 読む順番のおすすめ

### 7.1 まず方法を押さえる

1. Hair et al. (2019)
2. Dash & Paul (2021)
3. Sabol et al. (2023)

### 7.2 その後に自分の分野へ入る

- 教育なら: Demir & Uşak (2025) → Haverila et al. (2021) → Singh et al. (2024)
- SNS・情報行動なら: Kante & Michel (2023) → Nemec Zlatolas et al. (2019) → Wei et al. (2025)
- 観光なら: Usakli & Kucukergin (2018) → Abbasi et al. (2024) → Li et al. (2023)
- サステナビリティ消費なら: Munerah et al. (2021) → Parveen & Chaudhary (2025)
- 組織行動なら: Hair et al. (2019) → Low & Memon (2023)

## 8. 参考文献リンク一覧

- Hair et al. (2019): https://doi.org/10.1108/EBR-11-2018-0203
- Dash & Paul (2021): https://doi.org/10.1016/j.techfore.2021.121092
- Sabol et al. (2023): https://doi.org/10.1108/IMDS-07-2023-0429
- Kante & Michel (2023): https://doi.org/10.1016/j.chbr.2023.100291
- Demir & Uşak (2025): https://doi.org/10.1177/21582440251345950
- Usakli & Kucukergin (2018): https://doi.org/10.1108/IJCHM-11-2017-0753
- Haverila et al. (2021): https://doi.org/10.1016/j.ijme.2021.100558
- Huang (2021): https://doi.org/10.3390/educsci11050249
- Singh et al. (2024): https://doi.org/10.1007/s13198-024-02497-3
- Nemec Zlatolas et al. (2019): https://doi.org/10.3390/e21080772
- Sharma et al. (2018): https://doi.org/10.1016/j.giq.2018.07.003
- Wei et al. (2025): https://doi.org/10.1038/s41599-025-05142-x
- Shu et al. (2025): https://doi.org/10.3389/fpsyg.2025.1554882
- Abbasi et al. (2024): https://doi.org/10.1177/13567667221118651
- Li et al. (2023): https://doi.org/10.1177/13567667221101406
- Munerah et al. (2021): https://doi.org/10.1016/j.jclepro.2020.124192
- Parveen & Chaudhary (2025): https://doi.org/10.1177/02560909241297015
- Low & Memon (2023): https://doi.org/10.3233/HSM-220086

## 9. 次に広げるなら

- `心理学寄り` に広げるなら、SNS自己開示・ウェルビーイング・心理的苦痛の系列を追う
- `政策研究寄り` に広げるなら、電子政府、市民信頼、公共サービス品質へ伸ばす
- `教育寄り` に広げるなら、e-learning受容から学習成果・継続利用意図までのモデルに展開する
- `観光寄り` に広げるなら、住民態度、場所愛着、持続可能性、ブランド価値を接続すると厚みが出る

