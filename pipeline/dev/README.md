# 開発・検証用ファイル（dev/）

このディレクトリには、パイプライン開発時に使用した検証スクリプト、参考スクリプト、ドキュメントを格納しています。本番の解析実行には直接必要ありませんが、パイプラインの動作確認・性能比較・設計判断の根拠として保管しています。

---

## ディレクトリ構成

```
dev/
├── validation/                      # 検証・比較スクリプト
│   ├── test_compare_dup_modes.R     # --no-dup vs --dup-then-dedup の比較
│   ├── compare_modes.R              # 3モード比較図の作成
│   └── sm_bench.R                   # SM モード検証・速度比較
├── old_script/                      # 参考用：元スクリプト
│   ├── IBMseq_process_data10.1.r
│   ├── IBMseq_process_data10.1.mix.r
│   └── IBMseq_notparallel_process_data_ike.R
├── difference.md                    # 元スクリプトとの差分まとめ
└── specification_IBMseq_mix.md      # Mix データ仕様書
```

---

## validation/ - 検証スクリプト

### 1. test_compare_dup_modes.R

`--no-dup` と `--dup-then-dedup` のクラスター数・サイズ分布を比較する検証スクリプトです。

#### 重要性

- **重複エッジの扱いは Louvain クラスタリングの結果に影響を与えます**
- `--no-dup` と `--dup-then-dedup` では検出されるクラスター数が異なる場合があります
- 特に重複エッジが多いデータでは、この差が顕著になります

#### 実行方法

```bash
Rscript dev/validation/test_compare_dup_modes.R <name> <data_dir> <out_base> [min_cluster_size]
```

| 引数               | 説明                                     |
|--------------------|------------------------------------------|
| `<name>`           | サンプル名（例: V5P2_24aB_CTCF_2_3000） |
| `<data_dir>`       | `.link.gz` が置かれているディレクトリ    |
| `<out_base>`       | 出力ベースディレクトリ                   |
| `[min_cluster_size]` | クラスターサイズ閾値（デフォルト: 3）  |

#### スクリプトが実行する処理

1. 同じデータに対して `--no-dup` と `--dup-then-dedup` の両方を実行
2. クラスター数・クラスターサイズ分布を比較
3. 5種類のグラフと数値サマリーを出力：
   - クラスター総数の棒グラフ
   - 閾値以上のクラスター数の棒グラフ
   - クラスターサイズ分布（violin + boxplot）
   - クラスターサイズの累積分布（ECDF）
   - サイズビンごとのクラスター数ヒストグラム

#### 出力ファイル

```
{out_base}/
├── nodup/                           # --no-dup モードの中間ファイル
├── dup/                             # --dup-then-dedup モードの中間ファイル
├── compare_dup_modes.pdf            # 比較グラフ（5ページ）
└── compare_dup_modes_summary.tsv    # 数値サマリー
```

#### 数値サマリーの例

```
mode              n_clusters  n_clusters_large  median_size  mean_size  max_size  total_nodes
--no-dup          2422        32                1            1.5        7         3718
--dup-then-dedup  2450        35                1            1.5        8         3718
```

#### 実行例

```bash
Rscript dev/validation/test_compare_dup_modes.R \
  V5P2_24aB_CTCF_2_3000 \
  data \
  data/output_compare_dup \
  3
```

#### 注意事項

- Louvain アルゴリズムは確率的なため、実行ごとにクラスター数は若干変動します
- 重要なのは個々の数値ではなく、**分布全体の傾向の違い**です
- 重複エッジが多いほど、`--dup-then-dedup` の方がクラスター数が多くなる傾向があります

---

### 2. sm_bench.R

SM モードが標準モードと同じ結果を返すことを確認するベンチマークスクリプトです。

#### 重要な設計方針

Louvain クラスタリングはランダム性を持つため、**異なるパイプライン実行で比較すると UMI/UEI の合計が必ず変わります**（バグではなく正常動作）。
`sm_bench.R` では **同一のクラスタリング結果（`_02_membership.rds`）** を両実装で共有し、正確な正確性検証を行います。

#### 実行方法

```bash
Rscript dev/validation/sm_bench.R <name> <sm_save> [min_cluster_size] [n_reps] [out_pdf]
```

| 引数               | デフォルト                             | 説明                        |
|--------------------|----------------------------------------|-----------------------------|
| `<name>`           | —                                      | サンプル名                  |
| `<sm_save>`        | —                                      | SM パイプラインの出力ディレクトリ |
| `[min_cluster_size]`| 1000                                  | 大クラスター判定の閾値       |
| `[n_reps]`         | 5                                      | 速度計測の繰り返し回数       |
| `[out_pdf]`        | `{sm_save}/{name}_sm_bench.pdf`        | 出力 PDF のパス              |

#### 実行例

```bash
Rscript dev/validation/sm_bench.R V5P2_24aB_CTCF_2_3000 output/sm 1000 10 results/sm_bench.pdf
```

#### 出力（PDF 内容）

| ページ | 内容                                           |
|--------|------------------------------------------------|
| 表紙   | PASS/FAIL 結果・Speedup・パラメーター          |
| 1      | UMI/UEI 分布比較（orig vs sfx、violin + boxplot）|
| 1b     | UMI/UEI 合計カウント棒グラフ（完全一致確認）  |
| 2      | Cluster Size 分布（violin + boxplot）          |
| 3      | Edge Density 分布（violin + boxplot）          |
| 4      | Ego Size 密度プロット（クラスターごとに色分け）|
| 5      | Diameter 分布（violin + boxplot）              |

#### 正確性チェックの判定基準

```
[PASS]: type ごとの UMI/UEI 合計が orig と sfx で完全一致
[FAIL]: 1つでも合計が異なる
```

---

### 3. compare_modes.R

original（ike.R 互換）・modified（00_main.R）・sm_modified（sm_00_main.R）の 3 モードの指標分布を比較します。

#### ike.R 互換モードの実行

```bash
Rscript r_script/run_orig_metrics.R <name> <read_path> <save_path> [min_cluster_size] [num_cores]

# 例
Rscript r_script/run_orig_metrics.R V5P2_24aB_CTCF_2_3000 data output/orig 3 1
```

`run_orig_metrics.R` は旧スクリプト `ike.R` と同じ処理（`simplify()` なし・逐次 Louvain）を再現し、各指標の TSV を出力します。

#### 3モード比較図の作成

```bash
Rscript dev/validation/compare_modes.R <name> <orig_dir> <mod_dir> <sm_dir> <out_pdf> [min_cluster_size]

# 例
Rscript dev/validation/compare_modes.R \
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

#### 出力

- `{out_pdf}`: 5 指標の比較 violin/boxplot（PDF）
- `{out_pdf}.summary.tsv`: 中央値・平均・件数の数値サマリー

> **注意**: Louvain はランダム初期化のため、3モードの独立実行ではクラスター割り当てが異なります。比較は分布形状と合計カウントで行います。

---

## old_script/ - 参考用元スクリプト

パイプライン開発の元となった3つのスクリプトを保管しています：

| ファイル名                                | 説明                                                    |
|-------------------------------------------|---------------------------------------------------------|
| `IBMseq_process_data10.1.r`               | 元スクリプト（並列処理あり・Single データ用）          |
| `IBMseq_process_data10.1.mix.r`           | 元スクリプト（並列処理あり・Mix データ用）             |
| `IBMseq_notparallel_process_data_ike.R`   | ike.R（重要な修正版：連結成分ごとの Louvain）          |

詳細な差分については `difference.md` を参照してください。

---

## その他のファイル

### difference.md

元スクリプト（10.1.r, 10.1.mix.r, ike.R）と現在のパイプライン実装との差分をまとめたドキュメントです。

**主な内容：**
- クラスタリング方法の違い（グラフ全体 Louvain vs 連結成分ごと Louvain）
- 入力形式の違い（`.links` vs `.link.gz`）
- 並列処理の有無
- 中間ファイル保存方式の違い
- Mix データ対応の改善点

### specification_IBMseq_mix.md

Mix データ（複数抗体同時解析）の仕様書です。パイプライン実装時の要件定義として使用しました。

---

## まとめ

- **validation/** : パイプラインの動作検証・性能比較に使用するスクリプト
- **old_script/** : 開発の参考にした元スクリプト
- **difference.md** : 元スクリプトとの差分まとめ
- **specification_IBMseq_mix.md** : Mix データ仕様書

これらのファイルは開発・検証時に重要な役割を果たしましたが、本番の解析実行には `r_script/` 内のスクリプトを使用してください。
