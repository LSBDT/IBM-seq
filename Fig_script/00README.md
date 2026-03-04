# IBMseq解析用script

update 2026/03/04

## Down sampling

mat fileからread数が15M,10M,5MにDown samplingしてlink fileを作成し、これらを10回繰り返す。

出力ファイル

script

- IBMseq_downsamp.sh

```bash
#実行方法
$bash IBMseq_downsamp.sh {mat file 1} {mat file 2} {mat file 3} DATADIR OUTPUTDIR
##実行例
#IBMseq_downsamp.sh test1.mat.gz test2.mat.gz test3.mat.gz Test/data output
```

## IBMseq解析用function

IBMseqパイプラインにかけた際に出力されるファイル”{name}_02_membership.rds”を使用する。script内に入っているfunctionは、

- get_cut_subgraph:subgraph内ノード数が4より多いsubgraphとコミュニティ情報の抽出
- get_community_node_count:コミュニティ内ノード数をカウントする関数 
- get_community_per_subgraph:各サブグラフごとのコミュニティ数をカウントする関数
- get_total_subgraph:サブグラフの数をカウントする関数
- count_communities_by_threshold:閾値を設定した際のcluster数をカウントする関数
- count_subgraphs_by_threshold:閾値を設定した際のsubgraph数をカウントする関数

script

- ibmseq_analysis.R

```R
#使用方法例
source("ibmseq_analysis.R")
#test
test<-readRDS("{name}_02_membership.rds")
test_cut_subgraph<-get_cut_subgraph(test)
test_community_node_count<-get_community_node_count(test_cut_subgraph)
test_community_per_subgraph<-get_community_per_subgraph(test_community_node_count)
aB24_total_subgraph<-get_total_subgraph(test)

count_communities_by_threshold(test_community_node_count,0,2000,100)
```

## Cycle rank

IBMseqパイプラインにかけた際に出力されるファイル”{name}_02_graph.rds”と

"{name}_02_membership.rds”を使用する。

cycle rankを計算し、その結果を”{name}_cycle_rank.rds”で保存できる。

script

- cycle_rank.R

```bash
#実行方法
$Rscript cycle_rank.R {data1},{data2},{data3} threshold {data保存場所} {結果保存場所}

##実行例
$Rscript cycle_rank.R {name1},{name2},{name3} 1000 data data/out
```

## Tree-likenessのdensity plot

IBMseqパイプラインにかけた際に出力されるファイル”{name}_02_graph.rds”と

"{name}_02_membership.rds”を使用する。

cycle rankを計算し、その結果を”{name}_density_tree_likeness.rds”で保存できる。

scripe

- density_tree_likeness.R

```bash
#実行方法
$Rscript density_tree_likeness.R {data1},{data2},{data3} threshold {data保存場所} {結果保存場所}

##実行例
$Rscript density_tree_likeness.R {name1},{name2},{name3} 1000 data data/out
```

## MIX networkでのそれぞれの抗体の影響力

MIXのlink fileからそれぞれの抗体を取り除いたlink fileを作成する。

出力ファイル

script

- MIX_simulate_mono_from_link.sh

```bash
#実行方法
$bash MIX_simulate_mono_from_link.sh {antibody名} {antibody数} {link file 1:name.link.gz} {link file保存場所} {結果保存場所}

##実行例
$bash MIX_simulate_mono_from_link.sh S2P 2843106 V5P2.link.gz data S2P/out
```



