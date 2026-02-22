# IBMseq パイプライン

IBM-seq データからグラフを構築し、Louvain クラスタリングと各種指標（クラスターサイズ・Edge Density・UMI/UEI数・Ego Size・Diameter）を計算する R スクリプト群です。
このパッケージでは、IBMseqから生成される膨大なサブグラフをさらにコミュニティ分割を行うことで、IBMseq特有のグラフ構造をネットワーク特徴量から調査します。SMモードでは数万のネットワークを調べる際に、ノード名だけでも計算量の負荷があることから、コミュニティ分割のためのクラスタリングの段階で、ノード名のサフィックスを整数コードに変換し`node_type` 列として保存します。

![Version](https://img.shields.io/badge/version-v0.1.0-blue)
![R](https://img.shields.io/badge/R-%3E%3D4.0-informational)

Original scripts are developed by Keisuke Nimura (nimura@gunma-u.ac.jp)
Last update: 2026-02-22
Main modifier: Masaki Suimye Morioka (mmorioka@dbcls.rois.ac.jp)


---

## 目次

1. [必要なパッケージとインストール](#1-必要なパッケージとインストール)
2. [入力ファイル形式](#2-入力ファイル形式)
3. [スクリプト一覧](#3-スクリプト一覧)
4. [パイプライン全体の実行](#4-パイプライン全体の実行)
5. [個別ステップの実行](#5-個別ステップの実行)
6. [重複エッジの扱いについて（--no-dup / --with-dup / --dup-then-dedup）](#6-重複エッジの扱いについてno-dup--with-dup--dup-then-dedup)
7. [SM モード（Suffix Mode）](#7-sm-モードsuffix-mode)
8. [SM モード：検証・速度比較（sm_bench.R）](#8-sm-モード検証速度比較sm_benchr)
9. [3モード比較（compare_modes.R）](#9-3モード比較compare_modesr)
10. [テスト方法](#10-テスト方法)
11. [出力ファイル一覧](#11-出力ファイル一覧)
12. [よくある質問とトラブルシューティング](#12-よくある質問とトラブルシューティング)

---

## 1. 必要なパッケージとインストール

### 依存パッケージ

| パッケージ   | 用途                                    | 備考              |
|--------------|-----------------------------------------|-------------------|
| `data.table` | 高速なデータ操作（fread/fwrite含む）     | CRAN              |
| `igraph`     | グラフ構築・Louvain・ego_size・diameter | CRAN              |
| `ggplot2`    | 作図（violin/boxplot/density）          | CRAN              |
| `parallel`   | mclapply による並列処理                  | R 標準ライブラリ  |

### Requirement インストール方法

```bash
Rscript r_script/install_packages.R
```

手動でインストールする場合：

```r
install.packages(c("data.table", "igraph", "ggplot2"))
```

---

## 2. 入力ファイル形式

`.link.gz`：gzip 圧縮されたタブ区切りファイル（ヘッダーなし）

| 列番号 | 列名    | 説明                                      |
|--------|---------|-------------------------------------------|
| 1      | from    | ノード名（起点）                          |
| 2      | to      | ノード名（終点）                          |
| 3      | Target1 | from ノードのターゲット属性               |
| 4      | Target2 | to ノードのターゲット属性                 |
| 5      | count   | リード数                                  |

ノード名のサフィックス（suffix mode で使用）：

| サフィックス | ノードタイプ | 意味                  |
|--------------|--------------|-----------------------|
| `.m1`        | UMI1         | UMI 型ノード（side 1）|
| `.t2`        | UMI2         | UMI 型ノード（side 2）|
| `.e1`        | UEI1         | UEI 型ノード（side 1）|
| `.e2`        | UEI2         | UEI 型ノード（side 2）|

---

## 3. スクリプト一覧

```
r_script/
├── install_packages.R      # パッケージインストーラー
│
├── 00_main.R               # パイプライン実行（標準モード）
├── sm_00_main.R            # パイプライン実行（SM モード）
│
├── 01_load_graph.R         # Step 1: データ読み込み・グラフ構築
├── 02_clustering.R         # Step 2: Louvain クラスタリング（標準）
├── sm_02_clustering.R      # Step 2: Louvain + node_type エンコード（SM）
├── 03_density.R            # Step 3: クラスターサイズ・Edge Density（共通）
├── 04_features.R           # Step 4: UMI/UEI・Ego Size・Diameter（標準）
├── sm_04_features.R        # Step 4: 同上・UMI/UEI 高速版（SM）
├── 05_plot.R               # Step 5: 作図 PDF 出力（共通）
│
├── run_orig_metrics.R      # ike.R 互換モード（比較用）
├── compare_modes.R         # 3モード比較図の作成
└── sm_bench.R              # SM モード検証・速度比較
```

---

## 4. パイプライン全体の実行

### 基本構文

```bash
Rscript r_script/00_main.R <name> <read_path> <save_path> [オプション]
```

| 引数         | 説明                                          |
|--------------|-----------------------------------------------|
| `<name>`     | サンプル名（`.link.gz` のファイル名プレフィックス）|
| `<read_path>`| `.link.gz` が置かれているディレクトリ          |
| `<save_path>`| 出力ディレクトリ（存在しなければ自動作成）     |

### オプション一覧

| オプション          | デフォルト    | 説明                                                           |
|---------------------|---------------|----------------------------------------------------------------|
| `--no-dup`          | ✓（デフォルト）| 重複エッジ・自己ループを除去してからクラスタリング            |
| `--with-dup`        | —             | 重複エッジを保持してクラスタリング（ike.R 互換）              |
| `--dup-then-dedup`  | —             | 重複エッジを保持してクラスタリングし、**クラスタリング後**に除去 |
| `<整数>`            | 1000          | min_cluster_size：指標計算対象クラスターの最小ノード数        |
| `--cores=N`         | 1             | 並列計算コア数（72コア/512GBマシンなら `--cores=9` 程度が目安）|
| `--metrics=A,B,...` | 全指標        | 計算する指標（`edge_density`,`umi_uei`,`ego_size`,`diameter`）|
| `--from-tsv`        | —             | TSV から図だけ再作成（再計算しない）                          |
| `--no-rds`          | —             | Step 3/4 の指標 RDS を保存しない（TSV のみ出力）。自動的に `--from-tsv` も有効化。I/O 節約に使用 |

### 実行例

```bash
# 通常実行（全指標、重複エッジ除去、9コア）
Rscript r_script/00_main.R V5P2_24aB_CTCF_2 data output --no-dup 1000 --cores=9

# 一部の指標だけ計算（ego_size と diameter）
Rscript r_script/00_main.R V5P2_24aB_CTCF_2 data output --metrics=ego_size,diameter

# 計算済み TSV から図だけ作り直す
Rscript r_script/00_main.R V5P2_24aB_CTCF_2 data output --from-tsv

# RDS 中間ファイル不要・I/O を節約したい場合（TSV と PDF のみ出力）
Rscript r_script/00_main.R V5P2_24aB_CTCF_2 data output --no-rds 1000 --cores=9

# バックグラウンドで実行（ログをファイルに保存）
nohup Rscript r_script/00_main.R V5P2_24aB_CTCF_2 data output \
  --no-dup 1000 --cores=9 > output/run.log 2>&1 &

# 進捗確認
tail -f output/V5P2_24aB_CTCF_2_process.log
```

### SM モードでの全体実行

```bash
Rscript r_script/sm_00_main.R V5P2_24aB_CTCF_2 data output --no-dup 1000 --cores=9
```

オプションは `00_main.R` と同じです。UMI/UEI の計算がより高速になります（詳細は [セクション 7](#7-sm-モードsuffix-mode)）。

---

## 5. 個別ステップの実行

各ステップのスクリプトは `source()` での組み込みだけでなく、単独でも実行できます。
前のステップの出力（`{name}_{step}_*.rds`）が `save_path` に存在していることが前提です。

### Step 1: データ読み込み・グラフ構築

```bash
Rscript r_script/01_load_graph.R <name> <read_path> <save_path> [--no-dup|--with-dup]
```

- **入力**: `{read_path}/{name}.link.gz`
- **出力**: `{save_path}/{name}_01_graph.rds`, `{name}_01_summary.txt`

```bash
# 例
Rscript r_script/01_load_graph.R V5P2_24aB_CTCF_2 data output --no-dup
```

### Step 2: Louvain クラスタリング

```bash
# 標準モード
Rscript r_script/02_clustering.R <name> <save_path> [--cores=N] [--louvain-min=N]

# SM モード（node_type 列を付加）
Rscript r_script/sm_02_clustering.R <name> <save_path> [--cores=N] [--louvain-min=N]
```

| オプション       | デフォルト | 説明                                              |
|------------------|------------|---------------------------------------------------|
| `--cores=N`      | 1          | Louvain 並列コア数                                |
| `--louvain-min=N`| 3          | Louvain 適用の最小ノード数（それ未満は省略）       |

- **入力**: `{save_path}/{name}_01_graph.rds`
- **出力**: `{name}_02_graph.rds`, `{name}_02_membership.rds`, `{name}_02_community.rds`, `{name}_02_membership.tsv`, `{name}_02_summary.txt`

> **ポイント**: `--louvain-min=3`（デフォルト）により、ノードが 2 個以下の連結成分は Louvain をスキップし、連結成分 ID をそのままコミュニティ ID として使います。これにより数百万の trivial コンポーネントを持つ大規模データでも高速に処理できます。

### Step 3: クラスターサイズ・Edge Density 計算

```bash
Rscript r_script/03_density.R <name> <save_path> [min_cluster_size] [num_cores] [--no-edge-density]
```

- **入力**: `{name}_02_graph.rds`, `{name}_02_membership.rds`
- **出力**: `{name}_03_cluster_size.rds/.tsv`, `{name}_03_edge_density.rds/.tsv`, `{name}_03_density_summary.txt`

```bash
# 例：min_cluster_size=1000、4コア
Rscript r_script/03_density.R V5P2_24aB_CTCF_2 output 1000 4
```

### Step 4: 特徴量計算（UMI/UEI・Ego Size・Diameter）

```bash
# 標準モード
Rscript r_script/04_features.R <name> <save_path> [min_cluster_size] [num_cores] [--metrics=...]

# SM モード（UMI/UEI が高速）
Rscript r_script/sm_04_features.R <name> <save_path> [min_cluster_size] [--cores=N] [--metrics=...]
```

```bash
# 例：UMI/UEI だけ再計算
Rscript r_script/04_features.R V5P2_24aB_CTCF_2 output 1000 1 --metrics=umi_uei
```

- **出力**: `{name}_04_umi_uei.rds/.tsv`, `{name}_04_ego_size.rds/.tsv`, `{name}_04_diameter.rds/.tsv`

### Step 5: 作図

```bash
Rscript r_script/05_plot.R <name> <save_path> [--from-tsv]
```

`save_path` の中にある `{name}` プレフィックスを持つ5種類のファイルを自動で探し、それぞれの PDF を生成します。ファイルが存在しない場合はそのグラフをスキップして次へ進みます（エラーにはなりません）。

| 読み込むファイル              | 生成するPDF                    | グラフ種別                      |
|-------------------------------|-------------------------------|--------------------------------|
| `{name}_03_cluster_size.rds`  | `{name}_05_cluster_size.pdf`  | violin + boxplot（対数軸）      |
| `{name}_03_edge_density.rds`  | `{name}_05_edge_density.pdf`  | violin + boxplot                |
| `{name}_04_umi_uei.rds`       | `{name}_05_umi_uei.pdf`       | violin + boxplot（対数軸）      |
| `{name}_04_ego_size.rds`      | `{name}_05_ego_size.pdf`      | density plot（クラスター色分け）|
| `{name}_04_diameter.rds`      | `{name}_05_diameter.pdf`      | violin + boxplot                |

- `--from-tsv` を付けると `.rds` の代わりに `.tsv` を読み込む（計算をやり直さず図だけ再生成したいとき）
- 特定の指標だけ図を作りたい場合は、対象の `.rds`/`.tsv` だけ `save_path` に置いておけば、他はスキップされます

---

## 6. 重複エッジの扱いについて（--no-dup / --with-dup / --dup-then-dedup）

`.link.gz` ファイルには同じノードペアを結ぶエッジが複数含まれる場合があります（重複エッジ）。この扱いがパイプラインの結果に影響します。

### --no-dup（デフォルト・推奨）

```bash
Rscript r_script/00_main.R <name> <read_path> <save_path> --no-dup 1000
```

- `igraph::simplify()` を Step 1（グラフ構築直後）に適用し、**重複エッジと自己ループを除去**してからクラスタリング・指標計算を実施
- Edge Density の計算が理論的に正しくなる（重複エッジがあると分母に対して分子が水増しされる）
- **通常の解析はこちらを使用してください**

### --with-dup（ike.R 互換）

```bash
Rscript r_script/00_main.R <name> <read_path> <save_path> --with-dup 1000
```

- 重複エッジを保持したままグラフを構築（`simplify()` を適用しない）
- 旧スクリプト `ike.R` と同じ挙動
- **Edge Density の値が実際より大きくなる可能性あり**（重複エッジが分子に加算されるため）

### --dup-then-dedup（クラスタリング後に重複除去）

```bash
Rscript r_script/00_main.R <name> <read_path> <save_path> --dup-then-dedup 1000
```

- **Step 1**: 重複エッジを保持したままグラフを構築（`--with-dup` と同じ）
- **Step 2**: Louvain クラスタリングを重複エッジあり状態で実施（重複エッジがクラスタリング結果に影響）
- **Step 2 後半**: クラスタリング完了後に `igraph::simplify()` を適用して重複エッジを除去し、`_02_graph.rds` に保存
- **Step 3 以降**: 重複除去済みグラフで Edge Density・Ego Size・Diameter を計算

クラスタリングは重複エッジの情報を活用しつつ、指標計算では重複を除去した正確なグラフを用いたい場合に使用します。

### 3モードの比較

| モード              | Step1 simplify | クラスタリング | Step3以降 simplify | Edge Density |
|---------------------|:--------------:|:--------------:|:------------------:|:------------:|
| `--no-dup`          | ✓              | 重複なし       | —                  | 正確         |
| `--with-dup`        | —              | 重複あり       | —                  | 過大評価     |
| `--dup-then-dedup`  | —              | 重複あり       | ✓（Step2後）       | 正確         |

### 影響を受ける指標

| 指標             | --no-dup               | --with-dup             | --dup-then-dedup       |
|------------------|------------------------|------------------------|------------------------|
| クラスタリング結果 | 重複なしで分割         | 重複ありで分割         | 重複ありで分割         |
| Edge Density     | 正確                   | 過大評価の可能性あり   | 正確                   |
| UMI/UEI カウント | ほぼ同じ               | ほぼ同じ               | ほぼ同じ               |
| Ego Size         | 重複なしグラフで計算   | 重複ありグラフで計算   | 重複なしグラフで計算   |
| Diameter         | 重複なしグラフで計算   | 重複ありグラフで計算   | 重複なしグラフで計算   |

---

## 7. SM モード（Suffix Mode）

### 概要

SM モード（Suffix Mode）は、UMI/UEI カウントの計算を高速化したモードです。

**標準モードの問題点**：
`04_features.R` の `run_umi_uei()` は、ノード名のサフィックス（`.m1`, `.t2`, `.e1`, `.e2`）を `grepl()` で検索するループ処理を使います。大規模データ（クラスターが数十万個）ではこれが遅くなります。

**SM モードの解決策**：
クラスタリング（Step 2）の段階でノード名のサフィックスを整数コードに変換し、`node_type` 列として保存します。UMI/UEI 計算時は `data.table` の `group-by` 1回で済むため、大幅に高速化されます。

### node_type コード対応表

| 整数コード | サフィックス | ノードタイプ |
|------------|--------------|--------------|
| 1          | `.m1`        | UMI1         |
| 2          | `.t2`        | UMI2         |
| 3          | `.e1`        | UEI1         |
| 4          | `.e2`        | UEI2         |
| `NA`       | 上記以外     | —            |

### 実行方法

```bash
# SM モードで全パイプライン実行
Rscript r_script/sm_00_main.R <name> <read_path> <save_path> [オプション]

# 例：本番実行
Rscript r_script/sm_00_main.R V5P2_24aB_CTCF_2 data output --no-dup 1000 --cores=9
```

SM モードと標準モードのオプションは完全に同じです。

### パイプライン構成の違い

```
標準モード:
  00_main.R → 01_load_graph.R → 02_clustering.R      → 03_density.R → 04_features.R    → 05_plot.R

SM モード:
  sm_00_main.R → 01_load_graph.R → sm_02_clustering.R → 03_density.R → sm_04_features.R → 05_plot.R
```

共通スクリプト（`01_load_graph.R`, `03_density.R`, `05_plot.R`）は両モードで同じものを使います。

### 速度比較（実測値）

テストデータ（3,531ノード・93大クラスター）での計測：

| 実装                     | mean    | median  | 高速化倍率 |
|--------------------------|---------|---------|-----------|
| 標準（grepl ループ）     | 0.009 s | 0.009 s | —          |
| SM モード（group-by）    | 0.001 s | 0.001 s | **約 9x**  |

大規模データ（数十万クラスター）ではさらに大きな差が出ます。

### 出力の違い

SM モードでは `{name}_02_membership.rds` に `node_type` 整数列が追加されます：

```
標準モード: name | Target1 | ... | subgraph_id | community_id
SM モード:  name | Target1 | ... | subgraph_id | community_id | node_type
```

それ以外の出力ファイルは標準モードと同じ名前・形式です。

---

## 8. SM モード：検証・速度比較（sm_bench.R）

SM モードが標準モードと同じ結果を返すことを確認するベンチマークスクリプトです。

### 重要な設計方針

Louvain クラスタリングはランダム性を持つため、**異なるパイプライン実行で比較すると UMI/UEI の合計が必ず変わります**（バグではなく正常動作）。
`sm_bench.R` では **同一のクラスタリング結果（`_02_membership.rds`）** を両実装で共有し、正確な正確性検証を行います。

### 実行方法

```bash
Rscript r_script/sm_bench.R <name> <sm_save> [min_cluster_size] [n_reps] [out_pdf]
```

| 引数               | デフォルト                             | 説明                        |
|--------------------|----------------------------------------|-----------------------------|
| `<name>`           | —                                      | サンプル名                  |
| `<sm_save>`        | —                                      | SM パイプラインの出力ディレクトリ |
| `[min_cluster_size]`| 1000                                  | 大クラスター判定の閾値       |
| `[n_reps]`         | 5                                      | 速度計測の繰り返し回数       |
| `[out_pdf]`        | `{sm_save}/{name}_sm_bench.pdf`        | 出力 PDF のパス              |

```bash
# 例
Rscript r_script/sm_bench.R V5P2_24aB_CTCF_2_3000 output/sm 1000 10 results/sm_bench.pdf
```

### 出力（PDF 内容）

| ページ | 内容                                           |
|--------|------------------------------------------------|
| 表紙   | PASS/FAIL 結果・Speedup・パラメーター          |
| 1      | UMI/UEI 分布比較（orig vs sfx、violin + boxplot）|
| 1b     | UMI/UEI 合計カウント棒グラフ（完全一致確認）  |
| 2      | Cluster Size 分布（violin + boxplot）          |
| 3      | Edge Density 分布（violin + boxplot）          |
| 4      | Ego Size 密度プロット（クラスターごとに色分け）|
| 5      | Diameter 分布（violin + boxplot）              |

### 正確性チェックの判定基準

```
[PASS]: type ごとの UMI/UEI 合計が orig と sfx で完全一致
[FAIL]: 1つでも合計が異なる
```

---

## 9. 3モード比較（compare_modes.R）

original（ike.R 互換）・modified（00_main.R）・sm_modified（sm_00_main.R）の 3 モードの指標分布を比較します。

### ike.R 互換モードの実行

```bash
Rscript r_script/run_orig_metrics.R <name> <read_path> <save_path> [min_cluster_size] [num_cores]

# 例
Rscript r_script/run_orig_metrics.R V5P2_24aB_CTCF_2_3000 data output/orig 3 1
```

`run_orig_metrics.R` は旧スクリプト `ike.R` と同じ処理（`simplify()` なし・逐次 Louvain）を再現し、各指標の TSV を出力します。

### 3モード比較図の作成

```bash
Rscript r_script/compare_modes.R <name> <orig_dir> <mod_dir> <sm_dir> <out_pdf> [min_cluster_size]

# 例
Rscript r_script/compare_modes.R \
  V5P2_24aB_CTCF_2_3000 \
  output/orig \
  output/modified \
  output/sm \
  results/comparison.pdf \
  1000
```

| 引数          | 説明                             |
|---------------|----------------------------------|
| `<orig_dir>`  | run_orig_metrics.R の出力ディレクトリ |
| `<mod_dir>`   | 00_main.R の出力ディレクトリ     |
| `<sm_dir>`    | sm_00_main.R の出力ディレクトリ  |
| `<out_pdf>`   | 出力 PDF パス                     |

### 出力

- `{out_pdf}`: 5 指標の比較 violin/boxplot（PDF）
- `{out_pdf}.summary.tsv`: 中央値・平均・件数の数値サマリー

> **注意**: Louvain はランダム初期化のため、3モードの独立実行ではクラスター割り当てが異なります。比較は分布形状と合計カウントで行います。

---

## 10. テスト方法

本番データで実行する前に、小さいデータでパイプライン全体が正常に動作するか確認する方法を示します。

### テスト用小データの確認

```bash
ls data/
# V5P2_24aB_CTCF_2_3000.link.gz  （エッジ3,000件：テスト用）
# V5P2_24aB_CTCF_2_300.link.gz   （エッジ300件：超小テスト用）
# V5P2_24aB_CTCF_2.link.gz       （本番データ）
```

### Step 1: 標準モードのテスト

```bash
# テスト出力ディレクトリを作成
mkdir -p tmp/test_std

# min_cluster_size=3 で小データを実行（全指標）
Rscript r_script/00_main.R V5P2_24aB_CTCF_2_3000 data tmp/test_std --no-dup 3 --cores=1

# ログ確認
cat tmp/test_std/V5P2_24aB_CTCF_2_3000_process.log
```

期待される出力ファイル：

```
tmp/test_std/
├── V5P2_24aB_CTCF_2_3000_01_graph.rds
├── V5P2_24aB_CTCF_2_3000_02_membership.rds
├── V5P2_24aB_CTCF_2_3000_03_cluster_size.tsv
├── V5P2_24aB_CTCF_2_3000_03_edge_density.tsv
├── V5P2_24aB_CTCF_2_3000_04_umi_uei.tsv
├── V5P2_24aB_CTCF_2_3000_04_ego_size.tsv
├── V5P2_24aB_CTCF_2_3000_04_diameter.tsv
├── V5P2_24aB_CTCF_2_3000_05_*.pdf  （5つの PDF）
└── V5P2_24aB_CTCF_2_3000_process.log
```

### Step 2: SM モードのテスト

```bash
mkdir -p tmp/test_sm

Rscript r_script/sm_00_main.R V5P2_24aB_CTCF_2_3000 data tmp/test_sm --no-dup 3 --cores=1
```

SM モードでは `_02_membership.rds` に `node_type` 列が含まれることを確認：

```bash
Rscript -e "
  dt <- readRDS('tmp/test_sm/V5P2_24aB_CTCF_2_3000_02_membership.rds')
  cat('Columns:', paste(names(dt), collapse=', '), '\n')
  cat('node_type distribution:\n')
  print(table(dt\$node_type, useNA='ifany'))
"
```

### Step 3: SM モード検証（sm_bench.R）

```bash
Rscript r_script/sm_bench.R V5P2_24aB_CTCF_2_3000 tmp/test_sm 3 5 tmp/sm_bench.pdf
```

`[PASS]` が表示され、`tmp/sm_bench.pdf` が生成されることを確認します。

### Step 4: ike.R 互換モードの実行とモード比較

```bash
# ike.R 互換モード実行
mkdir -p tmp/test_orig
Rscript r_script/run_orig_metrics.R V5P2_24aB_CTCF_2_3000 data tmp/test_orig 3 1

# 3モード比較（事前に test_std と test_sm が完了していること）
Rscript r_script/compare_modes.R \
  V5P2_24aB_CTCF_2_3000 \
  tmp/test_orig \
  tmp/test_std \
  tmp/test_sm \
  tmp/test_comparison.pdf \
  3
```

### Step 5: 一括テストスクリプト

以下のシェルコマンドで上記テストを一括実行できます：

```bash
NAME="V5P2_24aB_CTCF_2_3000"
DATA="data"
MINCL=3

mkdir -p tmp/{test_std,test_sm,test_orig}

echo "=== [1/4] 標準モード ==="
Rscript r_script/00_main.R $NAME $DATA tmp/test_std --no-dup $MINCL --cores=1

echo "=== [2/4] SM モード ==="
Rscript r_script/sm_00_main.R $NAME $DATA tmp/test_sm --no-dup $MINCL --cores=1

echo "=== [3/4] SM bench ==="
Rscript r_script/sm_bench.R $NAME tmp/test_sm $MINCL 5 tmp/sm_bench.pdf

echo "=== [4/4] ike.R 互換 + 3モード比較 ==="
Rscript r_script/run_orig_metrics.R $NAME $DATA tmp/test_orig $MINCL 1
Rscript r_script/compare_modes.R $NAME tmp/test_orig tmp/test_std tmp/test_sm \
  tmp/comparison.pdf $MINCL

echo "=== テスト完了 ==="
ls tmp/sm_bench.pdf tmp/comparison.pdf
```

---

## 11. 出力ファイル一覧

| ファイル                                  | ステップ | 形式         | 説明                               |
|-------------------------------------------|----------|--------------|------------------------------------|
| `{name}_process.log`                      | 全       | テキスト     | 全ステップの実行ログ               |
| `{name}_01_graph.rds`                     | 1        | RDS (igraph) | 構築済みグラフオブジェクト         |
| `{name}_01_summary.txt`                   | 1        | テキスト     | V/E数・連結成分数                  |
| `{name}_02_graph.rds`                     | 2        | RDS (igraph) | community_id 属性付きグラフ        |
| `{name}_02_membership.rds`                | 2        | RDS (data.frame) | ノード属性テーブル（SM: +node_type）|
| `{name}_02_membership.tsv`                | 2        | TSV          | 同上（テキスト版）                  |
| `{name}_02_community.rds`                 | 2        | RDS          | クラスタリングメタデータ            |
| `{name}_02_summary.txt`                   | 2        | テキスト     | クラスタリング統計                  |
| `{name}_03_cluster_size.rds/.tsv`         | 3        | RDS/TSV      | 全クラスターのノード数              |
| `{name}_03_edge_density.rds/.tsv`         | 3        | RDS/TSV      | 大クラスターの Edge Density        |
| `{name}_03_density_summary.txt`           | 3        | テキスト     | Density 計算サマリー               |
| `{name}_04_umi_uei.rds/.tsv`              | 4        | RDS/TSV      | UMI/UEI カウント                   |
| `{name}_04_ego_size.rds/.tsv`             | 4        | RDS/TSV      | Ego Size（order=3）                |
| `{name}_04_diameter.rds/.tsv`             | 4        | RDS/TSV      | Diameter                           |
| `{name}_04_features_summary.txt`          | 4        | テキスト     | 特徴量計算サマリー                 |
| `{name}_05_cluster_size.pdf`              | 5        | PDF          | クラスターサイズ violin+boxplot     |
| `{name}_05_edge_density.pdf`              | 5        | PDF          | Edge Density violin+boxplot        |
| `{name}_05_umi_uei.pdf`                   | 5        | PDF          | UMI/UEI violin+boxplot（type別）   |
| `{name}_05_ego_size.pdf`                  | 5        | PDF          | Ego Size 密度プロット              |
| `{name}_05_diameter.pdf`                  | 5        | PDF          | Diameter violin+boxplot            |

---

## 12. よくある質問とトラブルシューティング

### Q1. Louvain を実行してもクラスター割り当てが毎回変わるのはなぜ？

Louvain アルゴリズムはランダム初期化を使うため、同じデータでも実行のたびに異なるクラスター割り当てになります。これは正常な動作です。異なるパイプライン実行間での UMI/UEI 合計カウントの違いはバグではありません。実装の正確性を検証したい場合は `sm_bench.R` を使い、同一クラスタリング結果上で比較してください。

### Q2. 大規模データで --cores=1 のまま実行したら非常に遅い

`--cores=N` を設定してください。Louvain クラスタリング（Step 2）と特徴量計算（Step 4）が `mclapply` で並列化されます。サーバー環境（72コア/512GB）では `--cores=9` 程度が推奨です。

### Q3. edge_density が全て 0 になっている

`--no-dup` を使った場合、重複エッジが除去されることで疎なグラフになります。テストデータ（3,000エッジ）では大クラスターが小さいため Edge Density が 0 になることがあります。本番データ（2百万エッジ）では正常な値になります。

### Q4. `Error: subscript out of bounds` が発生する

旧バージョンのスクリプトを使っている場合に発生することがあります（`lv_raw[[i]] <- NULL` の問題）。最新版では `lv_raw[[i]] <- integer(0)` に修正されています。スクリプトを最新版に更新してください。

### Q5. SM モードと標準モードで UMI/UEI 合計が違う

独立してパイプラインを実行した場合、Louvain のランダム性で大クラスターの構成が変わるため、UMI/UEI 合計が異なるのは正常です。同一クラスタリング結果での比較は `sm_bench.R` の `[PASS]/[FAIL]` で確認してください。

### Q7. RDS ファイルを保存せずにパイプラインを実行したい（ストレージ・I/O 節約）

`--no-rds` オプションを使います：

```bash
Rscript r_script/00_main.R V5P2_24aB_CTCF_2 data output --no-rds 1000 --cores=9
```

**保存されるファイル（変わらない）：**
- `_01_graph.rds`, `_02_graph.rds`, `_02_membership.rds`（Step 1/2 の構造 RDS）
- 全 TSV ファイル（`_03_*.tsv`, `_04_*.tsv`）
- 全 PDF ファイル（`_05_*.pdf`）

**省略されるファイル（5つ）：**
- `_03_cluster_size.rds`, `_03_edge_density.rds`
- `_04_umi_uei.rds`, `_04_ego_size.rds`, `_04_diameter.rds`

TSV さえ残っていれば後から `--from-tsv` で図の再作成が可能です。Step 4 を `--no-rds` の後に単独実行する場合、`_03_cluster_size.rds` がなくても自動的に `.tsv` にフォールバックします。

### Q6. `_02_membership.rds` に `node_type` 列がない

`02_clustering.R`（標準モード）を使った場合は `node_type` 列は含まれません。`sm_02_clustering.R` を使うか、`sm_00_main.R` でパイプライン全体を実行してください。
