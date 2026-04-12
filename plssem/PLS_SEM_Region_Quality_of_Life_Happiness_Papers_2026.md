# PLS-SEM × 地域・生活の質・幸福 論文調査

更新日: 2026-04-13  
対象: 地域、コミュニティ、近隣、都市・農村、住環境、スマートシティ、再定住、生活の質、幸福、主観的ウェルビーイングを扱い、`PLS-SEM` を明示している論文を中心に整理した。  
目的: `地域・生活の質・幸福` の研究で、PLS-SEMがどこでどう使われているかを俯瞰し、実際に論文設計へ使える参照リストを作る。

## 1. 先に結論

- この領域でのPLS-SEM研究は、`近隣環境 -> 満足/生活の質 -> 愛着/幸福`、または `都市・農村政策/環境 -> 生活の質/幸福` という構造が多い。
- 特に論文がまとまっているのは、`スマートシティ`、`住宅・近隣満足`、`農村・コミュニティの幸福`、`再定住住宅`、`旧市街地・公共空間更新`。
- 幸福そのものを直接の結果変数にする研究もあるが、PLS-SEM論文では `生活の質(QoL)` や `住民満足` を媒介に置く形が多い。
- 2024年以降は、`IPMA`、`fsQCA`、`機械学習`、`高次構成概念` を組み合わせる研究が増えており、説明だけでなく `優先改善項目` や `予測` を示す流れが強まっている。

## 2. この分野での典型モデル

| パターン | 典型構造 | 主なアウトカム |
| --- | --- | --- |
| 近隣満足型 | neighborhood features -> neighborhood satisfaction -> QoL -> place attachment | QoL, life satisfaction, place attachment |
| スマートシティ型 | readiness / smart services / social cohesion -> QoL / SWB -> citizenship / support | QoL, SWB, civic behavior |
| 農村幸福型 | social capital / trust / livelihood / infrastructure -> SWB | SWB, happiness, policy support |
| 住環境評価型 | housing / public space / safety / comfort -> emotional perception / satisfaction | resident satisfaction, QoL |
| 政策・介入型 | resettlement / distributed generation / poverty programs -> well-being | community well-being, SWB |

## 3. まず押さえるべきレビューと背景論文

### 3.1 住民QoLの研究地図

1. Songling Chang, Melanie Kay Smith (2023)  
   `Residents’ Quality of Life in Smart Cities: A Systematic Literature Review`  
   Land, 12(4), 876.  
   DOI: https://doi.org/10.3390/land12040876  
   位置づけ: スマートシティ文脈で住民QoL研究を整理したレビュー。  
   要点: QoL研究では `governance`、`smart living`、`participation`、`social inclusion` が主要テーマで、住民視点の研究がまだ不足していると指摘。

2. Venera Tomaselli, Mario Fordellone, Maurizio Vichi (2021)  
   `Building Well-Being Composite Indicator for Micro-Territorial Areas Through PLS-SEM and K-Means Approach`  
   Social Indicators Research, 153, 407-429.  
   DOI: https://doi.org/10.1007/s11205-020-02454-0  
   位置づけ: 地域単位のwell-beingを `PLS-SEM` で合成指標化した方法論論文。  
   要点: 地域政策や空間比較のために、well-beingを単一尺度ではなく複合指標として組み立てる方向性を示している。

## 4. 中核のPLS-SEM応用論文

### 4.1 スマートシティ・都市生活の質

3. Chunpei Lin, Guanxi Zhao, Chuanpeng Yu, Yenchun Jim Wu (2019)  
   `Smart City Development and Residents’ Well-Being`  
   Sustainability, 11(3), 676.  
   DOI: https://doi.org/10.3390/su11030676  
   手法: `PLS-SEM`  
   何を見たか: スマートシティの利用経験が住民の `subjective well-being` にどうつながるか。  
   モデル: `safety experience`、`usefulness experience`、`convenience experience` -> `SWB`。一部は有用性・利便性が媒介。  
   要点: スマートシティ研究で住民幸福を正面から扱った初期の重要PLS論文。  
   使いどころ: `都市サービス` や `デジタル公共サービス` が生活満足にどう効くかを扱いたいときに有用。

4. Syed Ali Raza Shah, Chen Lu, Shumaila Riaz et al. (2024)  
   `Shaping future home: understanding quality of life and citizenship in smart cities`  
   Open House International, 50(1), 139-157.  
   DOI: https://doi.org/10.1108/OHI-12-2023-0289  
   手法: `PLS-SEM`  
   何を見たか: スマートシティの準備性と社会結束が `quality of life` と `citizenship` にどうつながるか。  
   モデル: `technology readiness`、`organizational readiness`、`environmental readiness`、`smart economic development`、`change valence`、`social cohesion` -> `quality of life` -> `citizenship`。  
   要点: QoLを媒介変数に置く構造が明確で、地域政策への示唆が出しやすい。

5. Juliana Mejia, Eva Cristina Manotas, Santiago Quintero (2022)  
   `Analysis of the Social Capital in a Technological System of a Smart City Using a PLS-SEM Model`  
   Sustainability, 14(18), 11238.  
   DOI: https://doi.org/10.3390/su141811238  
   手法: `PLS-SEM`  
   何を見たか: スマートシティにおける `social capital` と技術学習の関係。  
   モデル: 社会資本資源、技術学習、技術システムの相互関係。  
   要点: 幸福を直接測ってはいないが、QoL研究の重要な前提である `social capital` をPLS-SEMで扱う好例。

### 4.2 近隣満足・住宅・場所愛着

6. Pankaj Kumar, Pardeep K. Ahlawat, Parveen Kumar, Ramesh Kumar Garg (2023)  
   `Neighbourhood satisfaction, quality of life and place attachment: a study on urban locations in India`  
   International Journal of Sustainable Society, 15(1), 20-43.  
   DOI: https://doi.org/10.1504/IJSSOC.2023.128363  
   手法: `PLS-SEM`  
   何を見たか: 近隣環境が住民の `QoL` と `place attachment` にどうつながるか。  
   モデル: `liveable amenities`、`environmental features`、`social aspects` -> `neighbourhood satisfaction` -> `quality of life` -> `place attachment`。  
   要点: このテーマのもっとも使いやすい王道モデル。  
   使いどころ: `近隣環境 -> QoL -> place attachment` を扱いたい研究の直近参照として強い。

7. Emmanuel Bosompem Boadi, Shaojun Chen, Ebenezer Impriam Amponsah, Ruth Appiah (2022)  
   `Antecedents of Residential Satisfaction in Resettlement Housing in Ellembelle: A PLS-SEM Approach`  
   Sustainability, 14(18), 11256.  
   DOI: https://doi.org/10.3390/su141811256  
   手法: `PLS-SEM`  
   何を見たか: 再定住住宅における住民満足の決定要因。  
   モデル: 住宅の物理条件、近隣環境、公共サービス等 -> `residential satisfaction`。  
   要点: 強制移転・再定住・居住再編の文脈で、QoL研究に接続できる重要な応用。

8. José da Silva et al. (2022)  
   `Environmental Protection Is Not Relevant in the Perceived Quality of Life of Low-Income Housing Residents: A PLS-SEM Approach in the Brazilian Amazon`  
   Sustainability, 14(20), 13171.  
   DOI: https://doi.org/10.3390/su142013171  
   手法: `PLS-SEM`  
   何を見たか: 低所得住宅居住者の `perceived quality of life` に何が効くか。  
   モデル: 複数の住宅サステナビリティ要因 -> `perceived quality of life`。  
   要点: 環境保護よりも、より日常的・直接的な住宅要因のほうがQoLに効くと示した点が興味深い。  
   使いどころ: `環境価値` と `生活実感` のズレを議論したいときに有用。

9. Jiahui Wang, Di Zhao (2025)  
   `Residents’ Satisfaction with Public Spaces in Old Urban Residential Communities: A PLS-SEM and IPMA-Based Case Study of Nankai District, Tianjin`  
   Land, 14(12), 2363.  
   DOI: https://doi.org/10.3390/land14122363  
   手法: `PLS-SEM + IPMA`  
   何を見たか: 旧市街住宅地の公共空間が住民満足にどう影響するか。  
   モデル: `space accessibility`、`usability`、`maintainability`、`environmental comfort`、`site safety` -> `emotional perception` -> `resident satisfaction`。  
   要点: 物理環境と心理的知覚をつなぐ媒介モデルが明快。IPMAで改善優先順位も示している。  
   使いどころ: まちづくり、都市更新、公共空間評価にそのまま応用しやすい。

10. Tingting Liu, Xiaoqi Shen, Tiansheng Xia (2025)  
    `Mediating Power of Place Attachment for Urban Residents' Well-Being in Community Cohesion`  
    Sustainability, 17(15), 6756.  
    DOI: https://doi.org/10.3390/su17156756  
    手法: `PLS-SEM`  
    何を見たか: `community cohesion` が `life satisfaction` にどうつながるか。  
    モデル: `neighborliness`、`sense of community`、`neighborhood attractiveness` -> `place attachment` -> `life satisfaction`。  
    要点: `place attachment` を媒介に置く都市住民幸福研究として非常に使いやすい。  
    注意: EBSCO由来の公開要約を参照しているため、本文確認は別途行うと安心。

### 4.3 農村・コミュニティ幸福

11. Haiping Xu, Chuqiao Zhang, Yawen Huang (2023)  
    `Social trust, social capital, and subjective well-being of rural residents: micro-empirical evidence based on the Chinese General Social Survey (CGSS)`  
    Humanities and Social Sciences Communications, 10, 49.  
    DOI: https://doi.org/10.1038/s41599-023-01532-1  
    手法: 本文公開情報から `媒介モデル` は確認できるが、PLS-SEMの明示確認は現時点で取れていない。  
    何を見たか: `social trust` と `social capital` が農村住民の幸福にどう関わるか。  
    モデル: `social trust` -> `social capital` -> `happiness/SWB`。  
    要点: PLS-SEM論文ではない可能性があるが、この領域の理論背景としてはかなり重要。

12. Chen Xuan, Ahmad Zubir Ibrahim, Low Kah Choon (2024)  
    `Farmers' Well-Being Level Prediction: A Hybrid PLS-SEM-Machine Learning Approach`  
    Journal of Research Administration, 6(1).  
    URL: https://journlra.org/index.php/jra/article/view/1662  
   手法: `PLS-SEM + machine learning`  
   何を見たか: 農民のwell-beingを `social capital` から予測。  
   モデル: `bonding`、`bridging`、`linking social capital` -> `farmers' well-being`。  
   要点: 研究の質評価は慎重に見る必要があるが、予測志向の新しい応用としては面白い。  
   注意: ジャーナルの信頼性は主流誌ほど強くないため、補助参照扱いが無難。

13. X (authors not fully verified from search snippet) (2024)  
   `Evaluation of the influence of distributed generation on the well-being of the rural community using PLS-SEM`  
   Journal of Cleaner Production, 442, 141023.  
   DOI: https://doi.org/10.1016/j.jclepro.2024.141023  
   手法: `PLS-SEM`  
   何を見たか: 分散型発電が農村コミュニティの `well-being` にどう影響するか。  
   モデル: `economic`、`environmental`、`social`、`distributed generation system` -> `community well-being`。  
   要点: エネルギー政策と地域well-beingをつなぐ希少なPLS-SEM応用。  
   注意: 著者名は本文未確認のため、引用時はDOI先で必ず確認する。

14. Hoang Thi Phuong Thao, Trinh Thi Ha (2025)  
   `Rural Tourism Development: How to Attract the Residents’ Participation? A Study of the Central Highlands, Vietnam`  
   Dalat University Journal of Science, 15(1).  
   DOI: https://doi.org/10.37569/DalatUniversity.15.1.1216(2025)  
   手法: `PLS-SEM`  
   何を見たか: `sense of place`、`perceived benefits/costs`、`quality of life` が住民参加意図にどうつながるか。  
   モデル: `sense of place`、`perceived benefits`、`perceived costs` -> `quality of life` -> `resident participation/support`。  
   要点: 観光研究寄りだが、住民のQoLを地域参加に接続するモデルとして使いやすい。

### 4.4 地域指標・空間単位のwell-being

15. Venera Tomaselli, Mario Fordellone, Maurizio Vichi (2021)  
   `Building Well-Being Composite Indicator for Micro-Territorial Areas Through PLS-SEM and K-Means Approach`  
   Social Indicators Research, 153, 407-429.  
   DOI: https://doi.org/10.1007/s11205-020-02454-0  
   手法: `PLS-SEM + cluster analysis`  
   何を見たか: 小地域単位のwell-being複合指標をどう構築するか。  
   モデル: 多次元指標から `equitable and sustainable well-being` を構成。  
   要点: 個票アンケートではなく、地域指標をまとめる方向でPLS-SEMを使った点が重要。  
   使いどころ: 自治体比較や地域分類をしたいときの方法論参照として強い。

## 5. 周辺の重要論文

以下は厳密にはPLS-SEMではない、または公開情報だけではPLS-SEM確認が取れないが、理論背景として重要。

1. Yuting Zhang, Jianjun Zhang (2017)  
   `Perceived residential environment of neighborhood and subjective well-being among the elderly in China: A mediating role of sense of community`  
   Journal of Environmental Psychology, 51, 82-94.  
   DOI: https://doi.org/10.1016/j.jenvp.2017.03.004  
   メモ: `neighborhood environment -> sense of community -> SWB` の古典的構造。

2. Xiuzhi Lin, Qifu Lai (2025)  
   `How does rural tourism affect farmers' subjective well-being? The mediating role of farmers' livelihoods`  
   Acta Psychologica, 261, 105807.  
   DOI: https://doi.org/10.1016/j.actpsy.2025.105807  
   メモ: 観光研究寄りだが、地域住民のSWBモデルとして重要。

3. Haiping Xu et al. (2023)  
   `Social trust, social capital, and subjective well-being of rural residents`  
   DOI: https://doi.org/10.1038/s41599-023-01532-1  
   メモ: 農村幸福研究の理論背景として有用。

## 6. 研究テーマ別の整理

### 6.1 近隣環境からQoLへ

- `liveable amenities -> neighborhood satisfaction -> QoL`
- `public space quality -> emotional perception -> resident satisfaction`
- `housing conditions -> residential satisfaction / perceived QoL`

この系統では、QoLや満足は `物理環境の直接効果` だけでなく、`感情的知覚` や `近隣満足` を介して説明されることが多い。

### 6.2 地域社会関係から幸福へ

- `social cohesion -> place attachment -> life satisfaction`
- `social trust -> social capital -> SWB`
- `sense of place -> QoL -> participation/support`

この系統では、`community` や `place attachment` が媒介変数として非常に重要。

### 6.3 スマートシティ・政策介入からQoLへ

- `smart service experience -> SWB`
- `readiness / social cohesion -> QoL -> citizenship`
- `distributed generation / rural infrastructure -> community well-being`

この系統では、PLS-SEMに `IPMA` や `予測分析` を組み合わせると政策提言がしやすい。

## 7. この領域で見えるギャップ

- `QoL` と `SWB` が混用されやすく、概念区別があいまいな論文が多い。
- 近隣満足や住宅満足は多いが、`地域レベルの持続的幸福` を縦断で追う研究は少ない。
- `高齢者`、`低所得層`、`再定住住民`、`農村住民` のような脆弱層研究はあるが、比較研究はまだ不足。
- スマートシティ研究は増えている一方で、`技術受容` と `住民幸福` の橋渡しモデルはまだ薄い。
- 地域幸福研究では `social capital`、`trust`、`place attachment` が多いが、`公平性`、`包摂性`、`ガバナンス信頼` を明示的に組み込むPLS-SEMはまだ少ない。

## 8. 研究設計のヒント

### 8.1 すぐ論文化しやすいモデル

1. `neighborhood environment -> neighborhood satisfaction -> QoL -> place attachment`
2. `public space quality -> emotional perception -> resident satisfaction`
3. `social cohesion -> place attachment -> life satisfaction`
4. `smart service experience -> QoL/SWB -> citizenship`
5. `social trust -> social capital -> SWB`

### 8.2 PLS-SEMを選ぶ理由として書きやすい点

- QoLや幸福が多次元で、潜在変数が多い
- 媒介や調整を同時に入れたい
- 形成的構成概念や高次構成概念を含めたい
- 政策研究として `改善優先度` を出したいので `IPMA` を使いたい
- 将来的に `fsQCA` や `機械学習` と併用したい

## 9. 読む順番のおすすめ

### 9.1 まず方法と地図

1. Chang & Smith (2023)
2. Tomaselli et al. (2021)

### 9.2 中核のPLS-SEM論文から入る

1. Lin et al. (2019)
2. Kumar et al. (2023)
3. Wang & Zhao (2025)
4. Shah et al. (2024)

### 9.3 農村・コミュニティ幸福に広げる

1. Liu et al. (2025, place attachment and life satisfaction)
2. Xu et al. (2023, trust and social capital)
3. Distributed generation and rural community well-being (2024)

## 10. リンク一覧

- https://doi.org/10.3390/land12040876
- https://doi.org/10.1007/s11205-020-02454-0
- https://doi.org/10.3390/su11030676
- https://doi.org/10.1108/OHI-12-2023-0289
- https://doi.org/10.3390/su141811238
- https://doi.org/10.1504/IJSSOC.2023.128363
- https://doi.org/10.3390/su141811256
- https://doi.org/10.3390/su142013171
- https://doi.org/10.3390/land14122363
- https://doi.org/10.3390/su17156756
- https://doi.org/10.1038/s41599-023-01532-1
- https://doi.org/10.1016/j.jclepro.2024.141023
- https://doi.org/10.37569/DalatUniversity.15.1.1216(2025)
- https://doi.org/10.1016/j.jenvp.2017.03.004
- https://doi.org/10.1016/j.actpsy.2025.105807

## 11. まとめ

- `地域・生活の質・幸福` のPLS-SEM研究は、観光やマーケティングより件数は少ないが、テーマはかなり明確にまとまっている。
- 中核は `近隣環境`、`スマートシティ`、`住民満足`、`place attachment`、`social capital`。
- 実際に論文を書くなら、`Kumar et al. (2023)`、`Lin et al. (2019)`、`Wang & Zhao (2025)` を軸にし、背景に `Chang & Smith (2023)` と `Tomaselli et al. (2021)` を置くと組みやすい。
- もし `幸福そのもの` をより前面に出したいなら、QoL中心の論文に `SWB` や `life satisfaction` を接続する形で再設計すると新しさが出しやすい。

