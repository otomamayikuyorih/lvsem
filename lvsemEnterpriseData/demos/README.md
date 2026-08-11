# lvsemEnterpriseData PLS-SEM demos

このフォルダには、`lvsemEnterpriseData` に収録した企業・組織内データを使った PLS-SEM デモを置いています。

各デモは、測定項目から潜在変数スコアを作成し、構造パスを標準化回帰として推定する `lvsem` の軽量ワークフローです。研究論文の完全再現ではなく、データパッケージ利用例としての探索的デモです。

## デモ一覧

| Demo | データ | 主なテーマ |
| --- | --- | --- |
| [Demo 1](vietnam_soe_commitment_job_performance.md) | Vietnam SOE | 組織コミットメント、OCB、職務パフォーマンス |
| [Demo 2](vietnam_electronics_pos_stay.md) | Vietnam electronics | 組織支援、職務満足、WLB、定着意図 |
| [Demo 3](vietnam_hospitality_supervisor_support.md) | Vietnam hospitality | 上司支援、満足、コミットメント、職務パフォーマンス |
| [Demo 4](iran_hotel_role_conflict_turnover.md) | Iranian hotels | 役割葛藤、不安定雇用、情緒的消耗、離職意図 |
| [Demo 5](green_hotel_gai_employee_happiness.md) | Spanish green hotels | 生成AI、社員幸福、環境パフォーマンス |

## 再生成

```r
source("data-raw/run_plssem_demos.R")
```
