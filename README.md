# IBM-seq リポジトリ

IBM-seq（Integration of Barcode-based Multi-omic single-cell sequencing）データの解析ツールとスクリプトを収録したリポジトリです。

![Version](https://img.shields.io/badge/version-v0.1.0-blue)
![License](https://img.shields.io/badge/license-MIT-green)

## リポジトリ構成

このリポジトリは、IBM-seq データの処理パイプラインと、その他のデータ解析スクリプトを整理して管理しています。

```
IBM-seq/
├── pipeline/          # IBM-seq データ処理パイプライン
├── data_analysis/     # データ解析スクリプト（追加予定）
├── LICENSE            # ライセンス情報
├── VERSION            # バージョン情報
└── README.md          # このファイル
```

### 📁 pipeline/

IBM-seq データからグラフを構築し、Louvain クラスタリングと各種ネットワーク指標を計算するパイプラインです。

**主な機能:**
- 単一細胞ゲノムネットワークのグラフ構築
- **Single データ（単一抗体）と Mix データ（複数抗体）の両方に対応**
- Louvain クラスタリングによるコミュニティ検出
- ネットワーク特徴量の計算（Edge Density、UMI/UEI カウント、Ego Size、Diameter など）
- 標準モードと高速SM モード（Suffix Mode）
- 複数サンプル比較図の作成

詳細は [pipeline/README.md](pipeline/README.md) をご覧ください。

### 📁 data_analysis/

今後、IBM-seq データの追加解析スクリプトをこちらに配置予定です。

## クイックスタート

### パイプラインの実行

```bash
# パッケージインストール
Rscript pipeline/r_script/install_packages.R

# パイプライン実行（標準モード）
Rscript pipeline/r_script/00_main.R <sample_name> <data_dir> <output_dir> --no-dup 1000 --cores=9

# パイプライン実行（SM モード：高速）
Rscript pipeline/r_script/sm_00_main.R <sample_name> <data_dir> <output_dir> --no-dup 1000 --cores=9
```

詳細な使い方は [pipeline/README.md](pipeline/README.md) を参照してください。

## 必要な環境

- R (>= 4.0)
- 必要なパッケージ: `data.table`, `igraph`, `ggplot2`, `parallel`

## Contributor

- **Original scripts**: Keisuke Nimura (nimura@gunma-u.ac.jp)
- **Main Modifier**: Masaki Suimye Morioka (mmorioka@dbcls.rois.ac.jp)
- **Modifier**: Kanako Ikeuchi (kanako.ikeuchi@riken.jp) 
Last update: 2026-03-06

## ライセンス

このプロジェクトは MIT ライセンスのもとで公開されています。詳細は [LICENSE](LICENSE) をご覧ください。

## 関連リンク

- パイプライン詳細ドキュメント: [pipeline/README.md](pipeline/README.md)
- マニュアル: [pipeline/r_script/MANUAL.md](pipeline/r_script/MANUAL.md)
