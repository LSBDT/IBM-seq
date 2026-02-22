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
