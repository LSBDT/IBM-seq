# IBMseq_process_data10.1.r vs IBMseq_notparallel_process_data_ike.R 差分まとめ

## 概要

| 項目 | IBMseq_process_data10.1.r | IBMseq_notparallel_process_data_ike.R |
|------|---------------------------|----------------------------------------|
| 並列処理 | あり（foreach + doParallel） | なし |
| 入力形式 | `.links`（ディレクトリ/name/name.links） | `.link.gz`（フラットファイル） |
| 対象サンプル数 | 複数（カンマ区切りリスト） | 単一サンプル |
| クラスタリング | グラフ全体にLouvain適用 | **連結成分ごとにLouvain適用（重要な修正）** |
| density計算 | あり | なし |
| 中間ファイル保存 | 最後に`save.image()`のみ | **各ステップでRDS保存（重要な修正）** |
| 作図 | あり（4種） | なし |

---

## 差分詳細

### 1. 並列処理

**10.1.r:**
```r
library(foreach)
library(doParallel)
library(parallel)
numCores <- detectCores()/8
cl <- makeCluster(numCores)
registerDoParallel(cl)
# ... %dopar% で並列処理
stopCluster(cl)
```

**ike.R:**
並列処理ライブラリなし。直接処理。

---

### 2. 入力ファイルパスの違い

**10.1.r:**
```r
links_file <- file.path(read_path, name, paste0(name, ".links"))
```
→ `read_path/name/name.links` という構造を期待

**ike.R:**
```r
links_file <- file.path(read_path, paste0(name, ".link.gz"))
```
→ `read_path/name.link.gz` というフラットな構造。gzip圧縮に対応。

---

### 3. クラスタリング方法（★最重要修正点）

**10.1.r:**
```r
cluster <- cluster_louvain(graph)
df_membership <- cbind(df$vertices, membership = cluster$membership)
df_membership$sum <- rowSums(df_membership[, 2:5], na.rm = TRUE)
```
→ グラフ全体に `cluster_louvain` を一括適用。非連結グラフだとLouvainが正しく動作しない。

**ike.R:**
```r
clusters <- components(graph)$membership
V(graph)$subgraph_id <- clusters

subgraph_list <- split(seq_along(clusters), clusters)
subgraphs <- lapply(subgraph_list, function(nodes) induced_subgraph(graph, nodes))
louvain_results <- lapply(subgraphs, cluster_louvain)
community_list <- lapply(louvain_results, membership)

# オフセット管理（連結成分間でcommunity_idが重複しないよう加算）
community_id <- rep(NA, vcount(graph))
offset <- 0
for (i in seq_along(community_list)) {
  nodes <- subgraph_list[[i]]
  comms <- community_list[[i]] + offset
  offset <- offset + max(community_list[[i]])
  community_id[nodes] <- comms
}
V(graph)$community_id <- community_id

df_membership <- cbind(df$vertices, subgraph_id = clusters, community_id = community_id)
```
→ **連結成分ごとに分割してLouvainを適用し、community_idをオフセットで一意に管理する**。非連結グラフを正しく扱える。

**この修正が「ikeには重要な修正点があった」の本体と考えられる。**

---

### 4. 中間ファイルの保存（★重要な修正点）

**10.1.r:**
```r
save.image(file.path(save_path, ".RData"))
```
→ 最後に全環境を一括保存するのみ。途中で失敗すると何も残らない。

**ike.R:**
```r
saveRDS(df_membership, file = file.path(save_path, paste0(name, "_data_membership.rds")))
saveRDS(graph,         file = file.path(save_path, paste0(name, "_data_graph.rds")))
saveRDS(louvain_results, file = file.path(save_path, paste0(name, "_data_community.rds")))
```
→ クラスタリング後に各オブジェクトを個別RDSとして保存。ステップ再実行が容易。

---

### 5. ログファイル名

**10.1.r:**
```r
cat(message, file = "process_log.txt", ...)
```
→ 固定名。複数サンプル実行時に上書きされる。

**ike.R:**
```r
cat(message, file = paste0(name, "_process_log.txt"), ...)
```
→ サンプル名ごとにログファイルが分かれる。

---

### 6. membershipデータの列構成

**10.1.r:**
- `df$vertices` の各列 + `membership`（Louvainの結果）+ `sum`（Target列の合計）

**ike.R:**
- `df$vertices` の各列 + `subgraph_id`（連結成分ID） + `community_id`（Louvain結果、オフセット済み）
- `sum` 列は削除されている（`#df_membership$sum <- ...` でコメントアウト）

---

### 7. グラフオブジェクトの更新方法

**10.1.r:**
```r
graph_updated <- graph_from_data_frame(df$edges, directed = FALSE, vertices = df_membership)
```
→ 新しいグラフオブジェクトを構築し直す

**ike.R:**
```r
V(graph)$subgraph_id <- clusters
V(graph)$community_id <- community_id
```
→ 既存グラフオブジェクトに頂点属性を直接追加

---

### 8. 後段の処理（10.1.r のみ）

以下の処理は `10.1.r` にのみ存在し、`ike.R` にはない（density計算を省いたため）:

1. **クラスター規模計算** (`calculate_total_per_cluster`): クラスターごとのノード数集計、>1000フィルタ、violin plot出力
2. **Edge Density計算** (`calculate_edge_density`): サブグラフのedge density計算、violin plot出力
3. **UMI/UEI数プロット** (`process_dataset`): ノード名のサフィックスパターン（.m1, .t2, .e1, .e2）でカウント、violin plot出力
4. **Ego Size計算** (`process_ego_size`): order=3のego size計算、density plot出力
5. **Diameter計算** (`calculate_diameter_parallel`): サブグラフの直径計算、violin plot出力

---

## 取り込むべき修正点（ike.R → 新スクリプト）

1. **連結成分ごとのLouvainクラスタリング**（非連結グラフの正しい処理）
2. **入力を `.link.gz` に対応**（gzip圧縮）
3. **ステップごとのRDS保存**（途中再実行対応）
4. **サンプル名付きログファイル**
5. **`subgraph_id`と`community_id`の分離**（連結成分とコミュニティを区別）

---

# IBMseq_process_data10.1.mix.r vs 現在のパイプライン実装 (00_main.R + 05_plot.R) 差分まとめ

## 概要

| 項目 | 10.1.mix.r | 現在の実装 (00_main.R + 05_plot.R) |
|------|------------|-------------------------------------|
| Mix データ対応 | あり（固定列範囲 2:13） | **あり（自動列数対応・改善済み）** |
| simplify() | なし（重複エッジ保持） | **オプション選択可能（--no-dup, --with-dup, --dup-then-dedup）** |
| クラスタリング | グラフ全体にLouvain | **連結成分ごとにLouvain（ike.R由来の修正）** |
| PDF出力ファイル名 | 固定名（例: `Total_numberOFnodesPERcluster.pdf`） | **サンプル名付き（例: `{name}_05_cluster_size.pdf`）** |
| ego_size PDF | 1ファイルに全サンプル（ページごと） | **サンプルごとに個別PDF** |
| 抗体別カウント | なし（sum列のみ） | **あり（抗体別集計・PDF・TSV出力）** |
| 中間ファイル保存 | 最後に`.RData`のみ | **各ステップでRDS保存** |

---

## 差分詳細

### 1. Mix データ対応（抗体列数）

**10.1.mix.r:**
```r
df_membership$sum <- rowSums(df_membership[, 2:13], na.rm = TRUE)  # 固定範囲
```
→ 5種類の抗体（10列 + UEI1/UEI2）を想定した固定範囲。抗体数が変わると修正が必要。

**現在の実装:**
```r
# 02_clustering.R: dcast で自動的に横持ち変換
test_wide <- dcast(test_dt, to ~ Target2, value.var = "Count", fill = 0L)
```
→ `data.table::dcast()` により抗体数に関わらず自動対応。4種類でも6種類でも変更不要。

**改善点**: 抗体数の柔軟性が向上し、コード修正なしで異なるデータセットに対応可能。

---

### 2. PDF出力形式

#### (a) ファイル名の違い

**10.1.mix.r:**
- 固定ファイル名:
  - `Total_numberOFnodesPERcluster.pdf`
  - `EdgeDensityPERcluster.pdf`
  - `Number_UMI_UEI_PERcluster.pdf`
  - `ego_size_plots.pdf`
  - `DiameterPERcluster.pdf`

**現在の実装:**
- サンプル名付きファイル名:
  - `{name}_05_cluster_size.pdf`
  - `{name}_05_edge_density.pdf`
  - `{name}_05_umi_uei.pdf`
  - `{name}_05_ego_size.pdf`
  - `{name}_05_diameter.pdf`
  - `{name}_05_antibody_counts.pdf` ← **新規追加**

**メリット**: 複数サンプルを処理しても上書きされず、ファイル名から内容が明確。

#### (b) ego_size の PDF 出力形式

**10.1.mix.r:**
```r
pdf(file.path(save_path, "ego_size_plots.pdf"))
for (dataset_name in x) {
  ego_size_df <- process_ego_size(dataset_name)
  p <- ggplot(ego_size_df, aes(x = value)) +
    geom_density(aes(color = L1), show.legend = FALSE) + ...
  print(p)
}
dev.off()
```
→ **1つのPDFファイルに全サンプルのプロットをページごとに出力**（マルチページPDF）

**現在の実装:**
```r
# 05_plot.R: サンプルごとに個別PDFファイル
pdf_out <- file.path(save_path, paste0(name, "_05_ego_size.pdf"))
pdf(pdf_out); print(p); dev.off()
```
→ **サンプルごとに個別のPDFファイル**

**元のスクリプトの利点**:
- 複数サンプルを1つのファイルで比較しやすい
- ファイル数が少なく管理しやすい

**現在の実装の利点**:
- サンプルごとに独立して管理可能
- 06_combine_plot.R で複数サンプル比較が可能

---

### 3. 抗体別カウント機能

**10.1.mix.r:**
```r
df_membership$sum <- rowSums(df_membership[, 2:13], na.rm = TRUE)
```
→ 抗体列の合計のみ計算（プロットなし）

**現在の実装 (05_plot.R):**
```r
# 抗体列を自動抽出
antibody_cols <- setdiff(all_cols, exclude_cols)
antibody_names <- unique(gsub("\\.(t1|t2|m1)$", "", antibody_cols))

# 各抗体ごとに集計
antibody_counts <- membership_dt[, .(count = sum(.SD, na.rm = TRUE)),
                                  by = community_id,
                                  .SDcols = ab_cols]
# violin + boxplot 出力
pdf(paste0(name, "_05_antibody_counts.pdf"))
```
→ **抗体別の集計・PDF出力・TSV出力を追加**（Mix データのみ）

**改善点**: 抗体ごとの分布を可視化できるようになった。

---

### 4. simplify() オプション

**10.1.mix.r:**
```r
graph <- graph.data.frame(d = links_data, directed = FALSE)
# simplify() なし
```
→ 重複エッジと自己ループを保持

**現在の実装:**
```bash
# オプションで選択可能
--no-dup          # グラフ構築直後に simplify()（推奨）
--with-dup        # simplify() なし（10.1.mix.r と同じ）
--dup-then-dedup  # クラスタリング後に simplify()
```

**改善点**: 重複エッジの扱いを柔軟に選択可能。Edge Density の正確性が向上。

---

### 5. クラスタリング方法

**10.1.mix.r:**
```r
cluster <- cluster_louvain(graph)
df_membership <- cbind(df$vertices, membership = cluster$membership)
```
→ グラフ全体に Louvain を一括適用（非連結グラフで問題）

**現在の実装 (02_clustering.R):**
```r
comp <- components(graph)
# 連結成分ごとに Louvain を適用し、オフセット管理
# （ike.R 由来の重要な修正）
```
→ **連結成分ごとに Louvain を適用**（正確）

---

## 取り込み済み・改善済みの点

1. ✅ **連結成分ごとのクラスタリング**（ike.R 由来）
2. ✅ **抗体数の自動対応**（dcast による柔軟性向上）
3. ✅ **抗体別カウント機能の追加**（新規実装）
4. ✅ **simplify() のオプション化**（重複エッジ処理の柔軟性）
5. ✅ **ステップごとのRDS保存**（再実行対応）
6. ✅ **サンプル名付きPDFファイル名**（上書き防止）

## 取り込み検討中の点（元スクリプトの良い部分）

1. ⚠️ **ego_size のマルチページPDF出力**:
   - 元スクリプト: 1ファイルに全サンプル（比較しやすい）
   - 現在: サンプルごとに個別PDF
   - **対応案**: 06_combine_plot.R で facet によるマルチサンプル比較を実装済み（別の形で実現）
