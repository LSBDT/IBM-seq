# 密度計算問題の最終報告書

## 検証日時
2026-03-09

## 問題の発見
実データで実行した際に、密度計算の結果が1付近ではなく**0付近**に小さくなっている問題が発生。

## 原因の特定

### 根本原因
**pipeline/r_script/02_clustering.R の 59-60行目**

```r
df$vertices <- merge(df$vertices, as.data.frame(test_wide),
                     by.x = "name", by.y = "to", all.x = TRUE)
```

`merge()`関数はデフォルトで`sort = TRUE`のため、結果を**アルファベット順にソート**します。
これにより、`df$vertices`の行順番がgraphの頂点順番と不一致になりました。

### 問題の連鎖

1. **48行目**: `df <- igraph::as_data_frame(graph, "both")`
   - `df$vertices`はgraphの頂点順番と一致

2. **59-60行目**: `merge()`でソートされる
   - `df$vertices`の行順番が変化（アルファベット順）
   - graphの頂点順番とは無関係に

3. **71行目**: `clusters <- components(graph)$membership`
   - `clusters`はgraphの頂点順番に対応

4. **135-141行目**: `cbind(df$vertices, clusters, community_id)`
   - ソートされた`df$vertices`（アルファベット順）
   - graphの順番の`clusters`と`community_id`
   - **異なる順番のデータを横に並べてしまう**

### 具体例

```
Position 2:
  graph頂点順:       'ACGACGCCATGTGAGAGA.e1'       (本来の community_id = 591)
  membershipの同位置: 'AAAAAAAAACAACTAAAAAGG.CTCF.t1' (実際には community_id = 593)

  → df_membershipでは、ノード'AAAAAAAAACAACTAAAAAGG.CTCF.t1'に
     community_id = 591が割り当てられる（誤り）
```

## 検証結果

### 3000ノードテストデータでの比較

#### ノード順番の検証

| バージョン | ノード順番一致 | Community ID同位置一致 | コミュニティノード集合一致 |
|-----------|--------------|---------------------|----------------------|
| **ORIGINAL** (元) | **FALSE** (3530/3531不一致) | TRUE | **0/1477** (全て不一致) |
| **FIXED** (修正版) | **TRUE** (完全一致) | TRUE | **1477/1477** (全て一致) |

#### 密度計算の比較

| 統計量 | FIXED (修正版) | ORIGINAL (元) | 差分 |
|-------|--------------|--------------|------|
| **平均密度** | **0.891** | **0.000** | **+0.891** |
| **中央値** | **1.000** | **0.000** | **+1.000** |
| **標準偏差** | **0.177** | **0.000** | **+0.177** |
| **最小値** | 0.074 | 0.000 | +0.074 |
| **最大値** | 1.000 | 0.000 | +1.000 |

#### 密度分布

| 密度範囲 | FIXED | ORIGINAL |
|---------|-------|----------|
| 0.0-0.1 | 1 | **1476** (全て) |
| 0.1-0.2 | 1 | 0 |
| 0.2-0.3 | 3 | 0 |
| 0.3-0.4 | 12 | 0 |
| 0.4-0.5 | 76 | 0 |
| 0.6-0.7 | 335 | 0 |
| **0.9-1.0** | **1049** | 0 |
| **合計** | **1477** | **1476** |

### なぜ密度が0になるのか

元のバージョンでの処理フロー：

1. membershipからコミュニティ591のノード名を取得
   - 例: `c("A", "B", "C")` を取得

2. しかし、これらのノード名は**間違ったcommunity_id**が付いている
   - 実際にはコミュニティ591に属していない

3. `induced_subgraph(graph, c("A", "B", "C"))`を実行
   - graphから`A`, `B`, `C`という名前のノードを探す
   - これらのノードは**異なるコミュニティに属している**
   - 互いに**エッジがない**

4. 結果: **エッジ数が0のサブグラフ** → **密度 = 0 / max_edges = 0**

### データテーブルでの比較結果

全1477個のコミュニティで検証：

```
community_id  n_edges_fixed  n_edges_orig  density_fixed  density_orig
591           1              0             1.000          0.000
592           1              0             1.000          0.000
593           1              0             1.000          0.000
...           ...            ...           ...            ...
(全コミュニティで n_edges_orig = 0)
```

## 解決方法

### 適用した修正

**pipeline/r_script/02_clustering.R 59-60行目**

```r
# 修正前
df$vertices <- merge(df$vertices, as.data.frame(test_wide),
                     by.x = "name", by.y = "to", all.x = TRUE)

# 修正後
df$vertices <- merge(df$vertices, as.data.frame(test_wide),
                     by.x = "name", by.y = "to", all.x = TRUE, sort = FALSE)
```

**変更点**: `sort = FALSE`を追加

**効果**:
- `df$vertices`の行順番を保持
- graphの頂点順番と一致したまま
- `cbind()`で正しく結合される

### 代替案（参考）

IBMseq_notparallel_process_data_ike.Rでは、`left_join()`を使用：

```r
df$vertices <- df$vertices %>%
  left_join(test_wide, by = c("name" = "to"))
```

`left_join()`は左側のテーブルの順番を保持するため、問題が発生しません。

## 修正後の検証結果

### ノード順番チェック
- ✅ **Identical order: TRUE** - 全3531ノードの順番が完全に一致
- ✅ **Community IDs match: TRUE** - 同位置でcommunity_idが一致
- ✅ **Community sets: 0 mismatches** - 全1477コミュニティのノード集合が一致

### 密度計算チェック
- ✅ **Mean density: 0.891** - 正常な値（元は0.000）
- ✅ **分布が正常**: 1049コミュニティが密度0.9-1.0（元は全て0）
- ✅ **全コミュニティでエッジが検出**: n_edges > 0（元は全て0）

## 影響範囲

この問題は、02_clustering.Rで処理された**全ての既存データ**に影響します：

### 影響を受ける分析
1. ✗ 密度計算（03_edge_density.R）
2. ✗ 直径計算（04_diameter.R） - コミュニティノード集合の取得に依存
3. ✗ Ego size計算（04_ego_size.R） - 同上
4. ✗ community_idを使用する全ての統計分析
5. ✗ 可視化（密度プロット、コミュニティサイズプロットなど）

### SMモードへの影響

SMモードそのものは問題の原因ではありません。
`merge()`のソート問題は、**SMモードの有無に関わらず発生**しています。

検証結果：
- `tmp/sm/` (SMモード): order_identical = FALSE
- `tmp/` (通常モード): order_identical = FALSE
- `tmp/test_no_rds/`: order_identical = FALSE

→ 全てのバージョンで同じ問題が発生

## 推奨事項

### 1. 即座に実施すべき対応

1. ✅ **02_clustering.Rを修正** → 完了
2. ⬜ **全ての既存データを再処理**
   - 全サンプルで02_clustering.Rを再実行
   - 続いて03以降のステップも再実行
3. ⬜ **結果の検証**
   - 密度値が正常範囲（0.5-1.0付近）になることを確認
   - 密度プロットが正常な分布を示すことを確認

### 2. 予防策

1. **単体テストの追加**
   - ノード順番の一致を検証するテストを追加
   - 密度計算の妥当性チェック（0が大量に出ないか）

2. **コードレビュー項目**
   - `merge()`使用時は`sort = FALSE`を確認
   - または`left_join()`の使用を推奨

3. **検証スクリプトの定期実行**
   - `verify_node_order.R`を各ステップ後に実行

## 提供ファイル

1. `node_order_analysis_report.md` - 詳細分析レポート
2. `verify_node_order.R` - ノード順番検証スクリプト
3. `verify_fixed_version.R` - 修正版検証スクリプト
4. `compare_density.R` - 密度比較スクリプト
5. `density_comparison.csv` - 全コミュニティの密度比較データ
6. `verify_node_order_summary.csv` - 検証結果サマリー

## まとめ

### 問題
- 密度計算が全て0になる
- 原因: `merge()`のソートによるノード順番の不一致

### 解決
- `sort = FALSE`を追加
- 全チェック合格

### 効果
- 密度: 0.000 → 0.891
- 分布: 全て0 → 正常（1049個が0.9-1.0）
- エッジ: 全コミュニティで0 → 正常に検出

### 次のステップ
1. 全データの再処理
2. 結果の検証
3. 論文・報告書の数値更新（必要に応じて）
