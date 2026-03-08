# ノード順番問題の分析レポート

## 問題の概要

密度計算の結果が1付近に小さくなる問題の原因を調査した結果、`*02_graph.rds`と`*02_membership.rds`間でノード順番が不一致であることが判明しました。

## 検証結果

### 3000ノードテストデータでの検証（3バージョン全て）

| バージョン | Graph頂点数 | Membership行数 | 順番一致 | Community ID一致（同位置） | コミュニティノード集合一致 |
|-----------|------------|---------------|---------|--------------------------|----------------------|
| sm        | 3531       | 3531          | **FALSE** (3530/3531不一致) | TRUE | **0/10** (全て不一致) |
| test      | 3531       | 3531          | **FALSE** (3530/3531不一致) | TRUE | **0/10** (全て不一致) |
| test_no_rds | 3531     | 3531          | **FALSE** (3530/3531不一致) | TRUE | **0/10** (全て不一致) |

### 問題の深刻さ

```
Position 2:
  graph:      'ACGACGCCATGTGAGAGA.e1'       (community_id = X)
  membership: 'AAAAAAAAACAACTAAAAAGG.CTCF.t1' (community_id = X)

  → 同じcommunity_id Xだが、実際には全く異なるノード
  → community Xに属するノード集合も完全に異なる
```

この不一致により：
1. **密度計算が誤る**: membershipからノード名を取得してgraphからサブグラフを抽出すると、全く違うコミュニティのノードを取得
2. **他の統計量も影響を受ける**: 同様の処理をする全ての分析が誤った結果を出力

## 原因の特定

### 02_clustering.R (59-60行目)

```r
df$vertices <- merge(df$vertices, as.data.frame(test_wide),
                     by.x = "name", by.y = "to", all.x = TRUE)
```

**問題**: `merge()`はデフォルトで結果を**ソートする**（`sort = TRUE`）
- `df$vertices`の行順番が変わる
- しかし、その後の`clusters`と`community_id`はgraphの頂点順番に対応
- 135-141行目で`cbind`すると、順番が不一致のままマージされる

```r
df_membership <- cbind(
  df$vertices,           # ← ソートされている
  subgraph_id  = clusters,      # ← graphの頂点順番
  community_id = community_id   # ← graphの頂点順番
)
```

### 動作するバージョン: IBMseq_notparallel_process_data_ike.R (60-61行目)

```r
df$vertices <- df$vertices %>%
  left_join(test_wide, by = c("name" = "to"))
```

**成功の理由**: `left_join()`は左側のテーブルの**順番を保持**
- `df$vertices`の行順番が維持される
- graphの頂点順番と一致したまま

## 解決策

### オプション1: merge()に sort = FALSE を追加（最小限の変更）

```r
df$vertices <- merge(df$vertices, as.data.frame(test_wide),
                     by.x = "name", by.y = "to", all.x = TRUE,
                     sort = FALSE)
```

### オプション2: left_join()を使用（ike.Rと同じ方法）

```r
# data.tableのdcastはdata.table形式を返すので、as.data.frame()は不要
# またはdplyrを使う場合:
library(dplyr)
df$vertices <- df$vertices %>%
  left_join(as.data.frame(test_wide), by = c("name" = "to"))
```

### オプション3: data.tableで順番を保持してマージ

```r
# setDTは参照渡しなので、コピーを作成
vertices_dt <- as.data.table(df$vertices)
test_wide_dt <- as.data.table(test_wide)
setnames(test_wide_dt, "to", "name")

# data.tableのマージは順番を保持するオプションあり
df$vertices <- vertices_dt[test_wide_dt, on = "name"]
```

## 推奨

**オプション1（sort = FALSE）を推奨**:
- 最小限のコード変更
- 既存のロジックを維持
- 副作用のリスクが最小

## 修正後の検証手順

1. 修正版スクリプトで3000ノードデータを再処理
2. `verify_node_order.R`で検証:
   - `order_identical = TRUE`になることを確認
   - `mismatched_comms = 0`になることを確認
3. 密度計算を再実行して、値が正常になることを確認

## その他の考慮事項

### SMモードの影響

現時点では、SMモード自体が問題を引き起こしているわけではありません。
merge()のソート問題は、SMモードの有無に関わらず発生しています。

### 既存データへの影響

この問題は、02_clustering.Rで処理された**全ての既存データ**に影響します：
- 密度計算
- 直径計算
- Ego sizeなどのコミュニティレベルの統計
- community_idを使用する全ての分析

**対応**: 修正後、影響を受けるすべてのデータを再処理する必要があります。
