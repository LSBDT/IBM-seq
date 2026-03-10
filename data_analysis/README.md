# IBMseq解析用script

update 2026/03/10

IBMseqパイプライン実行後の出力スクリプトを使用した解析用のRスクリプト、shellスクリプトです。

## 目次

1. [必要なパッケージ](#1-必要なパッケージ)
2. [スクリプト一覧](#2-スクリプト一覧)
3. [スクリプト説明と使用例](#3-スクリプト説明と使用例)

## 1. 必要なパッケージ

依存パッケージ

`ggplot2`, `igraph`, `dplyr`

## 2. スクリプト一覧

`script/`

| スクリプト名                     | 用途                                                         |
| -------------------------------- | :----------------------------------------------------------- |
| `IBMseq_analysis.R`              | クラスター内ノード数のカウントや閾値を設定した際のcluster数をカウントする関数など様々なfunctionを格納 |
| `IBMseq_downsamp.sh`             | mat fileからread数が15M,10M,5MにDown samplingしてlink fileを作成し、これらを10回繰り返す。 |
| `MIX_simulate_mono_from_link.sh` | MIXのlink fileからそれぞれの抗体を取り除いたlink fileを作成する。 |

## 3. スクリプト説明と使用例

## IBMseq_analysis.R

`IBMseq_analysis.R`にはIBMseqの解析用の以下のRで使える関数が含まれています。

これらの関数を使用する際は以下を実行する。

```R
source("IBMseq_analysis.R")
```

### get_*

入力

IBMseqパイプラインにかけた際に出力される"{name}_02_membership.rds”を使用する。

関数

| 関数                         | 説明                                                         |
| ---------------------------- | ------------------------------------------------------------ |
| `get_cut_subgraph`           | subgraph内ノード数が4より多いサブグラフとクラスター情報の抽出する関数 |
| `get_community_node_count`   | クラスター内ノード数をカウントする関数                       |
| `get_community_per_subgraph` | 各サブグラフごとのクラスター数をカウントする関数             |
| `get_total_subgraph`:        | サブグラフの数をカウントする関数                             |

使用例

```R
source("ibmseq_analysis.R")
#test
test<-readRDS("{name}_02_membership.rds")
test_cut_subgraph<-get_cut_subgraph(test)
test_community_node_count<-get_community_node_count(test_cut_subgraph)
test_community_per_subgraph<-get_community_per_subgraph(test_community_node_count)
aB24_total_subgraph<-get_total_subgraph(test)
```



### *by_threshold

入力

get_community_node_countの出力

関数

| 関数                             | 説明                                     |
| -------------------------------- | ---------------------------------------- |
| `count_communities_by_threshold` | 閾値を変化させた際のcluster数をカウント  |
| `count_subgraphs_by_threshold`   | 閾値を変化させた際のsubgraph数をカウント |

使用例

```R
#使用例
test<-readRDS("{name}_02_membership.rds")
test_community_node_count<-get_community_node_count(test)
count_communities_by_threshold(test,0,2000,100)
```



### cycle_rank_per_community

入力

IBMseqパイプラインにかけた際に出力される

```
{name}_02_membership.rds
{name}_02_graph.rds
```

説明

各クラスターについてcycle rankを計算する関数。

cycle rankはグラフ内の独立なサイクル数を表す指標[1]であり、以下で定義される。

 $\mathrm{cycle\,rank}= E-N+C$

- $E$：エッジ数
- $N$：ノード数
- $C$：連結成分数

[1] Harary, F. (1969). *Graph Theory.*p32-41

出力

各クラスターについて以下の情報を含むデータフレーム

-  subgraph_id 
- community_id 
- n_nodes$N$
- edge_count $E$
- n_components $C$
- cycle_rank 

使用例

```R
mem<-readRDS("{name}_02_membership.rds")
graph<-readRDS("{name}_02_graph.rds")
cycle_rank_per_community(graph=graph, membership=mem, threshold=1000)
```



### tree_likeness_per_community

入力

IBMseqパイプラインにかけた際に出力される

```
{name}_02_membership.rds
{name}_02_graph.rds
```

説明

各クラスターについてtree like構造の程度を評価する指標を計算する。

tree構造の場合のエッジ数は

$E_{tree}=N-C$

であるため、観察されたエッジ数を比較して以下の指標を定義する。

 $\mathrm{tree\,likeness}=\frac{E_{tree}}{E_{obs}}=\frac{N-C}{E}$

- 値が 1 に近いほど tree構造に近い
- 値が小さいほどサイクルを多く含む構造

出力

各クラスターについて以下の情報を含むデータフレーム

- subgraph_id
-  community_id 
- n_nodes $N$
- edge_count $E$
- n_components $C$
- tree_likeness_ratio 

使用例

```R
#使用例
mem<-readRDS("{name}_02_membership.rds")
graph<-readRDS("{name}_02_graph.rds")
tree_likeness_per_community(graph=graph, membership=mem, threshold=1000)
```

## IBMseq_downsamp.sh

入力

mat file

説明

mat fileからread数が15M,10M,5MにDown samplingしてlink fileを作成し、これらを10回繰り返す。出力されたlink fileはIBMseqパイプラインへ実行可能

使用例

```bash
#実行方法
$bash IBMseq_downsamp.sh {mat file 1} {mat file 2} {mat file 3} DATADIR OUTPUTDIR
##実行例
#IBMseq_downsamp.sh test1.mat.gz test2.mat.gz test3.mat.gz Test/data output
```



## MIX_simulate_mono_from_link.sh

入力

link file

説明

MIXのlink fileからそれぞれの抗体を取り除いたlink fileを作成する。出力されたlink fileはIBMseqパイプラインへ実行可能。

使用例

```bash
#実行方法
$bash MIX_simulate_mono_from_link.sh {antibody名} {antibody数} {link file} {link file保存場所} {結果保存場所}

##実行例
$bash MIX_simulate_mono_from_link.sh S2P 2843106 V5P2.link.gz data S2P/out
```

