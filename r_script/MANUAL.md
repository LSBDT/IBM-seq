# IBMseq パイプライン マニュアル

## 概要

IBMseq パイプラインは、単一細胞ゲノムネットワークデータを処理し、グラフ解析・クラスタリング・指標計算・作図を行うパイプラインです。

入力: `.link.gz` ファイル（バーコードペアのエッジリスト）
出力: クラスタリング結果（RDS/TSV）、各種指標（RDS/TSV）、図（PDF）

---

## ファイル構成

```
r_script/
├── 00_main.R        パイプライン全体を一括実行するメインスクリプト
├── 01_load_graph.R  データ読み込み・グラフ構築
├── 02_clustering.R  Louvainクラスタリング（連結成分ごと）
├── 03_density.R     クラスター規模・Edge Density計算
├── 04_features.R    UMI/UEI数・Ego Size・Diameter計算
├── 05_plot.R        作図（PDF出力、単一サンプル）
└── 06_combine_plot.R  複数サンプルの比較図を一括生成
```

各スクリプトは `00_main.R` から呼び出せるほか、単体でも実行できます。

---

## 一括実行（00_main.R）

### 書式

```
Rscript 00_main.R <name> <read_path> <save_path> [オプション ...]
```

### 必須引数

| 引数 | 説明 |
|------|------|
| `name` | サンプル名（例: `V5P2_24aB_CTCF_2_3000`）。出力ファイルのプレフィックスになる |
| `read_path` | `.link.gz` が置かれているディレクトリパス |
| `save_path` | 出力ディレクトリ（存在しない場合は自動作成） |

### オプション（順不同で指定可）

| オプション | デフォルト | 説明 |
|------------|-----------|------|
| `--no-dup` | **デフォルト** | 重複エッジを除去してからクラスタリング |
| `--with-dup` | — | 重複エッジを保持してクラスタリング |
| `<整数>` | `1000` | `min_cluster_size`（この値を超えるクラスターのみ指標を計算） |
| `--cores=N` | `1` | 並列計算コア数（72コア/512GBマシンなら `9` 程度が目安） |
| `--metrics=A,B,...` | 全指標 | 計算する指標をカンマ区切りで指定（下記参照） |
| `--from-tsv` | — | 作図ステップで RDS の代わりに TSV から読み込む |

#### `--metrics` で指定できる値

| 値 | 内容 | 担当スクリプト |
|----|------|---------------|
| `edge_density` | クラスター内 Edge Density | 03_density.R |
| `umi_uei` | UMI/UEI 数カウント | 04_features.R |
| `ego_size` | Ego Size（order=3） | 04_features.R |
| `diameter` | グラフ直径 | 04_features.R |

### 使用例

```bash
# 全指標を計算（通常実行）
Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output --no-dup 1000 --cores=9

# ego_size と diameter だけ計算（edge_density と umi_uei はスキップ）
Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output --metrics=ego_size,diameter

# edge_density だけ計算
Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output --metrics=edge_density

# 計算は再実行せず、既存の TSV から図だけ再作成
Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output --from-tsv

# テスト（min_cluster_size=3、2コア）
Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output --no-dup 3 --cores=2

# バックグラウンド実行
nohup Rscript 00_main.R V5P2_24aB_CTCF_2_3000 /data /output \
  --no-dup 1000 --cores=9 > /output/run.log 2>&1 &
```

---

## 個別スクリプトの実行

各スクリプトはパイプラインの中間出力（RDS）が揃っていれば、単独で実行できます。
前のステップが完了していることが前提です（下記の「依存関係」参照）。

### 01_load_graph.R — データ読み込み・グラフ構築

```
Rscript 01_load_graph.R <name> <read_path> <save_path> [--no-dup | --with-dup]
```

| 入力 | `<read_path>/<name>.link.gz` |
|------|-------------------------------|
| 出力 | `<name>_01_graph.rds`, `<name>_01_summary.txt` |

```bash
Rscript 01_load_graph.R V5P2_24aB_CTCF_2_3000 /data /output --no-dup
```

---

### 02_clustering.R — Louvainクラスタリング

```
Rscript 02_clustering.R <name> <save_path>
```

| 入力 | `<name>_01_graph.rds` |
|------|-----------------------|
| 出力 | `<name>_02_graph.rds`, `<name>_02_membership.rds`, `<name>_02_community.rds`, `<name>_02_membership.tsv`, `<name>_02_summary.txt` |

```bash
Rscript 02_clustering.R V5P2_24aB_CTCF_2_3000 /output
```

---

### 03_density.R — クラスター規模・Edge Density計算

```
Rscript 03_density.R <name> <save_path> [min_cluster_size] [--cores=N] [--no-edge-density]
```

| 入力 | `<name>_02_graph.rds`, `<name>_02_membership.rds` |
|------|---------------------------------------------------|
| 出力 | `<name>_03_cluster_size.rds/.tsv`, `<name>_03_cluster_size_large.tsv`, `<name>_03_edge_density.rds/.tsv`, `<name>_03_density_summary.txt` |

| オプション | 説明 |
|------------|------|
| `min_cluster_size` | 整数。デフォルト `1000` |
| `--cores=N` | 並列コア数。デフォルト `1` |
| `--no-edge-density` | Edge Density の計算をスキップ |

```bash
# Edge Density を計算する（デフォルト）
Rscript 03_density.R V5P2_24aB_CTCF_2_3000 /output 1000 --cores=4

# クラスターサイズだけ集計し Edge Density はスキップ
Rscript 03_density.R V5P2_24aB_CTCF_2_3000 /output 1000 --no-edge-density
```

---

### 04_features.R — UMI/UEI数・Ego Size・Diameter計算

```
Rscript 04_features.R <name> <save_path> [min_cluster_size] [--cores=N] [--metrics=A,B,...]
```

| 入力 | `<name>_02_graph.rds`, `<name>_02_membership.rds`, `<name>_03_cluster_size.rds` |
|------|----------------------------------------------------------------------------------|
| 出力 | `<name>_04_umi_uei.rds/.tsv`, `<name>_04_ego_size.rds/.tsv`, `<name>_04_diameter.rds/.tsv`, `<name>_04_features_summary.txt` |

| オプション | 説明 |
|------------|------|
| `min_cluster_size` | 整数。デフォルト `1000` |
| `--cores=N` | 並列コア数。デフォルト `1` |
| `--metrics=A,B,...` | 計算する指標（`umi_uei`, `ego_size`, `diameter`）。デフォルト全指標 |

```bash
# 全指標を計算
Rscript 04_features.R V5P2_24aB_CTCF_2_3000 /output 1000 --cores=4

# ego_size だけ計算
Rscript 04_features.R V5P2_24aB_CTCF_2_3000 /output 1000 --cores=4 --metrics=ego_size

# ego_size と diameter を計算
Rscript 04_features.R V5P2_24aB_CTCF_2_3000 /output 1000 --cores=4 --metrics=ego_size,diameter
```

---

### 05_plot.R — 作図

```
Rscript 05_plot.R <name> <save_path> [--from-tsv]
```

| 入力 (RDS) | `<name>_03_cluster_size.rds`, `<name>_03_edge_density.rds`, `<name>_04_umi_uei.rds`, `<name>_04_ego_size.rds`, `<name>_04_diameter.rds` |
|-----------|-------------------------------------------------------------------------------------------------------------------------------------------|
| 入力 (TSV, `--from-tsv` 時) | 上記と同名の `.tsv` ファイル |
| 出力 | `<name>_05_cluster_size.pdf`, `<name>_05_edge_density.pdf`, `<name>_05_umi_uei.pdf`, `<name>_05_ego_size.pdf`, `<name>_05_diameter.pdf` |

```bash
# RDS から図を作成（デフォルト）
Rscript 05_plot.R V5P2_24aB_CTCF_2_3000 /output

# TSV から図を再作成（計算は再実行しない）
Rscript 05_plot.R V5P2_24aB_CTCF_2_3000 /output --from-tsv
```

`--from-tsv` は、計算済みの TSV を手動で編集した後に図だけ再作成したい場合などに使用します。
対応する TSV ファイルが存在しない指標はスキップされます（エラーにはなりません）。

---

### 06_combine_plot.R — 複数サンプル比較図

各サンプルを `00_main.R` で個別に処理した後、生成された RDS/TSV を一括で読み込んで
サンプル間を並べた比較 violin/density plot を生成します。

```
# 同一ディレクトリ
Rscript 06_combine_plot.R <rds_dir> <name1,name2,...> [--out=dir] [--prefix=str] [--from-tsv] [--min-size=N]

# 異なるディレクトリ
Rscript 06_combine_plot.R --out=<out_dir> <dir1>:<name1> [<dir2>:<name2> ...] [--prefix=str] [--from-tsv]
```

| 入力 (RDS) | `<name>_03_cluster_size.rds`, `<name>_03_edge_density.rds`, `<name>_04_umi_uei.rds`, `<name>_04_ego_size.rds`, `<name>_04_diameter.rds` |
|-----------|-------------------------------------------------------------------------------------------------------------------------------------------|
| 出力 | `{prefix}_06_cluster_size.pdf`, `{prefix}_06_edge_density.pdf`, `{prefix}_06_umi_uei.pdf`, `{prefix}_06_ego_size.pdf`, `{prefix}_06_diameter.pdf` |

| オプション | 説明 |
|------------|------|
| `--out=path` | 出力ディレクトリ（省略時は `rds_dir`） |
| `--prefix=str` | 出力ファイルのプレフィックス（デフォルト: `combined`） |
| `--from-tsv` | RDS の代わりに TSV ファイルから読み込む |
| `--min-size=N` | cluster_size プロットで使用する最小クラスターサイズ（デフォルト: 0 = 全クラスター）。旧スクリプトの `total > 1000` に相当するには `--min-size=1000` を指定 |

```bash
# 同じディレクトリに 3 サンプル分の RDS がある場合
Rscript 06_combine_plot.R /output sampleA,sampleB,sampleC

# 出力先を別ディレクトリ(/output/figs)にし、クラスターサイズの閾値を設定
Rscript 06_combine_plot.R /output sampleA,sampleB,sampleC \
  --out=/output/figs --prefix=exp1 --min-size=1000

# 各サンプルが別ディレクトリの場合
Rscript 06_combine_plot.R --out=/output/combined \
  /output/rB:V5P2_rB_S2P_1_S1 \
  /output/6aB:V5P2_6aB_S2P_1_S8 \
  /output/24aB:V5P2_24aB_S2P_1_S15

# TSV から再作成（計算は再実行しない）
Rscript 06_combine_plot.R /output sampleA,sampleB,sampleC --from-tsv
```

#### 各プロットの仕様

| プロット | X 軸 | Y 軸 | 備考 |
|---------|------|------|------|
| cluster_size | サンプル名 | ノード数（log10） | `--min-size` で小クラスターを除外可 |
| edge_density | サンプル名 | Edge Density | 大クラスターのみ（03_density.R の時点で計算済み） |
| umi_uei | `{サンプル名}_{種類}` | カウント（log10） | x 軸ラベルがサンプル名+種類（UMI1等）になる |
| ego_size | Ego Size 値（log10） | density | サンプルごとに `facet_wrap` で分割 |
| diameter | サンプル名 | 直径 | 大クラスターのみ |

---

## 出力ファイル一覧

| ファイル | 内容 |
|---------|------|
| `<name>_01_graph.rds` | Step1 グラフオブジェクト |
| `<name>_01_summary.txt` | Step1 サマリー |
| `<name>_02_graph.rds` | Step2 グラフ（頂点属性付き） |
| `<name>_02_membership.rds/.tsv` | ノードごとのクラスター割り当て |
| `<name>_02_community.rds` | Louvain コミュニティオブジェクト |
| `<name>_02_summary.txt` | Step2 サマリー |
| `<name>_03_cluster_size.rds/.tsv` | 全クラスターのノード数 |
| `<name>_03_cluster_size_large.tsv` | `min_cluster_size` 超のクラスターのみ |
| `<name>_03_edge_density.rds/.tsv` | クラスターごとの Edge Density |
| `<name>_03_density_summary.txt` | Step3 サマリー |
| `<name>_04_umi_uei.rds/.tsv` | クラスターごとの UMI/UEI カウント |
| `<name>_04_ego_size.rds/.tsv` | ノードごとの Ego Size（order=3） |
| `<name>_04_diameter.rds/.tsv` | クラスターごとの直径 |
| `<name>_04_features_summary.txt` | Step4 サマリー |
| `<name>_05_cluster_size.pdf` | クラスター規模の violin + boxplot |
| `<name>_05_edge_density.pdf` | Edge Density の violin + boxplot |
| `<name>_05_umi_uei.pdf` | UMI/UEI カウントの violin + boxplot |
| `<name>_05_ego_size.pdf` | Ego Size の density plot（クラスター別） |
| `<name>_05_diameter.pdf` | 直径の violin + boxplot |
| `<name>_process.log` | 全ステップの処理ログ |
| `{prefix}_06_cluster_size.pdf` | 複数サンプルのクラスター規模比較 |
| `{prefix}_06_edge_density.pdf` | 複数サンプルの Edge Density 比較 |
| `{prefix}_06_umi_uei.pdf` | 複数サンプルの UMI/UEI カウント比較 |
| `{prefix}_06_ego_size.pdf` | 複数サンプルの Ego Size 比較（facet） |
| `{prefix}_06_diameter.pdf` | 複数サンプルの直径比較 |
| `{prefix}_06_combine.log` | 06_combine_plot.R の処理ログ |

---

## ステップ間の依存関係

```
01_load_graph.R
    └─> 02_clustering.R
            └─> 03_density.R
                    └─> 04_features.R
                            └─> 05_plot.R
```

個別スクリプトを実行する場合は、前のステップの出力（RDS）が存在している必要があります。
`--from-tsv` で `05_plot.R` を実行する場合は TSV ファイルが存在していれば十分です。

---

## 必要な R パッケージ

```r
install.packages(c("data.table", "igraph", "ggplot2", "parallel"))
```

`parallel` は R 標準ライブラリに含まれているため、通常は別途インストール不要です。

---

## 並列化の目安

`--cores=N`（または `03_density.R`/`04_features.R` の `--cores=N`）で指定します。

- クラスターごとの Edge Density 計算（Step3）
- Ego Size 計算（Step4）
- Diameter 計算（Step4）

に `mclapply` による並列化が適用されます。`N` はマシンのコア数・メモリ量・クラスター数に応じて調整してください。
72コア/512GB 環境では `--cores=9` 程度が目安です。

> 注意: `mclapply` は Linux/macOS のフォーク並列化を使用します。Windows では `num_cores=1` として動作します。

---

## ノード名のサフィックスについて（UMI/UEI カウント）

`04_features.R` の `run_umi_uei()` は、ノード名のサフィックスでバーコードの種類を判定します。

| サフィックス | 種類 |
|-------------|------|
| `.m1` | UMI1 |
| `.t2` | UMI2 |
| `.e1` | UEI1 |
| `.e2` | UEI2 |

データの前処理でサフィックスが異なる場合は、`04_features.R` 内の `suffixes` ベクトルを修正してください。
