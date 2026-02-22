#########################
# 2024/07/08
# IBMseqのRでのデータ処理
# 使い方は一番下
# line 264: suffixesはm1かt1かfastqを処理するコードに合わせる必要あり。
# 9 coresで400Gくらいのメモリを使用していそう
#########################


# 必要なパッケージの読み込み
library(data.table)
library(dplyr)
library(tidyverse)
library(igraph)
library(ggplot2)
library(reshape2)
library(foreach)
library(doParallel)
library(parallel)

# テーマの設定
my_plot2 <- theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", linewidth = 1),
        axis.text.x = element_text(color = "black", size = 16, angle = 45, hjust = 1),
        axis.text.y = element_text(color = "black", size = 16),
        axis.title = element_text(color = "black", size = 20),
        legend.background = element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_blank())

# 並列処理のためのクラスタを設定
numCores <- detectCores()/8 #detectCores() - 1; /3だと落ちる /4でも落ちた。72 cores, 512G
cl <- makeCluster(numCores)
registerDoParallel(cl)

# メイン関数
process_graph_data_parallel <- function(x, read_path, save_path) {
  
  # ファイルにログを保存する関数
  write_log <- function(message) {
    cat(message, file = "process_log.txt", append = TRUE, sep = "\n") #名前
    }
  # データを処理する関数
  process_data <- function(name) {
    # データの読み込み
    links_file <- file.path(read_path, name, paste0(name, ".links"))
    if (!file.exists(links_file)) {
      stop(paste("File", links_file, "does not exist or is non-readable."))
    }
    
    links_data <- fread(links_file, header = FALSE)
    colnames(links_data) <- c("from", "to", "Target1", "Target2", "count")
    
    # グラフの作成
    graph <- graph.data.frame(d = links_data, directed = FALSE)
    
    # デバッグ: 読み込んだデータとグラフの情報を表示
    cat("Processing:", name, "\n")
    cat("Links data:\n")
    print(head(links_data))
    cat("Graph summary:\n")
    print(graph)

    # デバッグ: 読み込んだデータとグラフの情報を表示
    write_log(paste("Processing:", name))
    write_log("Links data:")
    write_log(capture.output(head(links_data)))
    write_log("Graph summary:")
    write_log(capture.output(graph))

    # データフレーム変換と処理
    df <- igraph::as_data_frame(graph, "both")
    df_list <- df$edges[, c(1, 3)]
    colnames(df_list) <- c("to", "Target2")
    df_list <- rbind(df_list, df$edges[, c(2, 4)])
    
    # 各頂点にテーブルを追加
    test <- setDT(df_list)[, .(Count = .N), by = .(to, Target2)]
    
    # データを横持ちに変換
    test_wide <- test %>% pivot_wider(names_from = "Target2", values_from = "Count")
    test_wide[is.na(test_wide)] <- 0
    
    # データフレームを更新
    df$vertices <- df$vertices %>%
      left_join(test_wide, by = c("name" = "to"))
    
    # クラスタリング
    cluster <- cluster_louvain(graph)
    df_membership <- cbind(df$vertices, membership = cluster$membership)
    df_membership$sum <- rowSums(df_membership[, 2:5], na.rm = TRUE) #抗体の数で変わる。6種類x6種類なら2:13
    
    # デバッグ: クラスタリング結果の表示
    print(head(df_membership))
    write_log("Membership data:")
    write_log(capture.output(head(df_membership)))

    # データフレーム -> グラフデータに戻す
    graph_updated <- graph_from_data_frame(df$edges, directed = FALSE, vertices = df_membership)
    
    # 不要な中間ファイルを削除
    rm(links_data, graph, df, df_list, test, test_wide)

    # 結果をリストとして返す
    list(
      graph = graph_updated,
      membership = df_membership,
      cluster = cluster
    )
  }
  
  # 各データフレームを並列処理
  results <- foreach(name = x, .packages = c("data.table", "dplyr", "tidyverse", "igraph", "ggplot2", "reshape2")) %dopar% {
    process_data(name)
  }
  
  # 結果をグローバル環境に保存
  for (i in seq_along(x)) {
    name <- x[i]
    assign(paste0(name, "_graph"), results[[i]]$graph, envir = .GlobalEnv)
    assign(paste0(name, "_membership"), results[[i]]$membership, envir = .GlobalEnv)
    assign(paste0(name, "_cluster"), results[[i]]$cluster, envir = .GlobalEnv)

  # デバッグ: グローバル環境に保存されたデータを確認
  write_log(paste("Saved membership data for", name, ":\n"))
  write_log(capture.output(head(get(paste0(name, "_membership")))))
  }


  
  write_log(paste("GlobalEnv"))
  write_log(capture.output(ls(envir=.GlobalEnv)))
  write_log(paste("parent"))
  write_log(capture.output(ls(parent.frame())))
  write_log(paste("current"))
  write_log(capture.output(ls()))


  # クラスタの総数を計算する関数
  calculate_total_per_cluster <- function(name) {
    membership_df <- get(paste0(name, "_membership"))
    write_log(paste("Calculating total per cluster for:", name)) #1行
    write_log(capture.output(head(membership_df)))

    membership_sum <- membership_df %>%
      group_by(membership) %>%
      summarise(total = n(), .groups = 'drop')
    tmp <- membership_sum[membership_sum$total > 1000, ]
    tmp$library <- name
    
    print(tmp)
    write_log(paste("Calculated total per cluster for in loop:", name))
    write_log(capture.output(tmp))
    write_log(paste("Membership sum:", name))
    write_log(capture.output(head(membership_sum)))

    # 結果をリストとして返す
    list(membership_sum_1000 = tmp)
  }
  
  # 結果を保存するリスト
  tmp_list <- foreach(name = x, .export=ls(envir = .GlobalEnv), .packages = c("dplyr","tidyverse")) %dopar% {
    calculate_total_per_cluster(name)
  }
  
  # デバッグ用の出力
  write_log("Calculated total per cluster for out loop:")
  write_log(capture.output(head(tmp_list)))
  
  # 全てのデータフレームを結合
  tmp_combined <- do.call(rbind, lapply(tmp_list, `[[`, "membership_sum_1000"))
  write_log("Check tmp_combined:")
  write_log(capture.output(head(tmp_combined)))

  colnames(tmp_combined) <- c("membership", "total", "library")
  tmp_combined$library <- factor(tmp_combined$library, levels = x)
  
  # デバッグ用出力
  write_log("Calculated total per cluster in tmp_combined:")
  write_log(capture.output(head(tmp_combined)))
  write_log(capture.output(tail(tmp_combined)))

  # 結果をグローバル環境に保存
  for (i in seq_along(x)) {
    name <- x[i]
    assign(paste0(name, "_membership_sum"), tmp_list[[i]]$membership_sum_1000, envir = .GlobalEnv)
  }

  # 結果をグローバル環境に保存
  assign("combined_membership_sum", tmp_combined, envir = .GlobalEnv)

  # グラフ描画
  p <- ggplot(tmp_combined, aes(x = library, y = total)) +
    geom_violin() + 
    geom_boxplot(width = 0.1, notch = TRUE) +
    labs(x = "Library", y = "Total Number of Nodes per Cluster") + 
    scale_y_log10(limits = c(900, 100000), breaks = 10^(3:5)) +
    my_plot2
  
  # PDFファイルに保存
  pdf(file.path(save_path, "Total_numberOFnodesPERcluster.pdf"))
  print(p)
  dev.off()

  write_log("Finish calculating number")


  # Edge Density を計算する関数
  calculate_edge_density <- function(name) {
    df_membership_sum <- get(paste0(name, "_membership_sum"))
    membership_df <- get(paste0(name, "_membership"))
    graph_obj <- get(paste0(name, "_graph"))
    membership_df_filter <- membership_df %>% filter(membership %in% df_membership_sum$membership[df_membership_sum$total > 1000])
    cluster_list <- split(membership_df_filter$name, membership_df_filter$membership) 

    edge_density_vec <- sapply(cluster_list, function(cluster) {
      edge_density(induced_subgraph(graph_obj, cluster))
    })
    edge_density_df <- data.frame(
      ED = edge_density_vec[!is.na(edge_density_vec)],
      clusterNumber = which(!is.na(edge_density_vec)),
      library = name
    )
    return(edge_density_df)
  }
  
  # 結果を保存するリスト
  edge_density_list <- foreach(name = x, .export=ls(envir = .GlobalEnv), .packages = c("igraph", "dplyr")) %dopar% {
    calculate_edge_density(name)
  }
  
  # デバッグ用の出力
  write_log("Calculated edge density:")
  write_log(capture.output(head(edge_density_list)))
  
  # 全てのデータフレームを結合
  combined_edge_density <- do.call(rbind, edge_density_list)
  combined_edge_density$library <- factor(combined_edge_density$library, levels = x)

  # 結果をグローバル環境に保存
  assign("combined_edge_density", combined_edge_density, envir = .GlobalEnv)
  
  # violin plot
  p <- ggplot(combined_edge_density, aes(x = library, y = ED)) +
    geom_violin() + 
    geom_boxplot(width = 0.1, notch = TRUE) +
    labs(x = "Library", y = "Edge Density") + 
    my_plot2
  
  # PDFファイルに保存
  pdf(file.path(save_path, "EdgeDensityPERcluster.pdf"))
  print(p)
  dev.off()

  write_log("Finish ED")


# UMI/UEI の数をプロットする関数
process_dataset <- function(dataset_name) {
  membership_df <- get(paste0(dataset_name, "_membership"))
  df_membership_sum <- get(paste0(dataset_name, "_membership_sum"))
  membership_df_filter <- membership_df %>% filter(membership %in% df_membership_sum$membership[df_membership_sum$total > 1000])
  cluster_list <- split(membership_df_filter$name, membership_df_filter$membership) 
  suffixes <- c("UMI1" = "\\.m1$", "UMI2" = "\\.t2$", "UEI1" = "\\.e1$", "UEI2" = "\\.e2$")
  results_df <- data.frame(clusternumber = integer(), variation = integer(), name = character(), stringsAsFactors = FALSE)

  # プレフィックスとサフィックスごとに処理を行う
  for (suffix_name in names(suffixes)) {
    pattern <- suffixes[[suffix_name]]

    # apply関数を使用して各クラスター内で処理を行う
    cluster_results <- lapply(names(cluster_list), function(cluster) {
      names_in_cluster <- cluster_list[[cluster]]
      match_count <- sum(grepl(pattern, names_in_cluster))
      
      if (match_count > 0) {
        data.frame(
          clusternumber = as.integer(cluster),
          variation = match_count,
          name = paste(dataset_name, suffix_name, sep = "_"),
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })
    
    # NULLでない結果をデータフレームにバインド
    cluster_results <- do.call(rbind, cluster_results)
    if (!is.null(cluster_results)) {
      results_df <- bind_rows(results_df, cluster_results)
    }
  }
    write_log("results_df")
    write_log(capture.output(head(results_df)))

  return(results_df)
}

# 結果を保存するリスト
  results <- foreach(dataset_name = x, .combine = rbind, .export = ls(envir = .GlobalEnv), .packages = c("dplyr", "tidyr")) %dopar% {
    process_dataset(dataset_name)
  }

  write_log("results")
  write_log(capture.output(head(results)))
  write_log(capture.output(typeof(results)))

# 名前のレベルを動的に生成
name_levels <- unlist(lapply(x, function(prefix) {
  paste(prefix, c("UMI1", "UMI2", "UEI1", "UEI2"), sep = "_")
}))
results$name <- factor(results$name, levels = name_levels)

# 結果をグローバル環境に保存
assign("number_rB_6aB_24aB", results, envir = .GlobalEnv)

# プロット作成
p <- ggplot(results, aes(x = name, y = variation)) +
  geom_violin() +
  geom_boxplot(width = 0.1, notch = TRUE) +
  labs(x = "number per cluster > 1000") +
  scale_y_log10() +
  my_plot2

  # PDFファイルに保存
  pdf(file.path(save_path, "Number_UMI_UEI_PERcluster.pdf"))
  print(p)
  dev.off()


  
# ego size を計算する関数
  process_ego_size <- function(name) {
    df_membership_sum <- get(paste0(name, "_membership_sum"))
    membership_df <- get(paste0(name, "_membership"))
    graph_obj <- get(paste0(name, "_graph"))
    membership_df_filter <- membership_df %>% filter(membership %in% df_membership_sum$membership[df_membership_sum$total > 1000])
    cluster_list <- split(membership_df_filter$name, membership_df_filter$membership) 

    # 並列処理で ego_size を計算
    # ego_size_list <- foreach(cluster = cluster_list, .export=ls(envir = .GlobalEnv), .combine = rbind) %dopar% {
    #   ego_size_results <- lapply(cluster, function(cluster_names) {
    #       list(ego_size(induced_subgraph(graph_obj, cluster_names), order = 3))
    #   })
    #   names(ego_size_results) <- c(1:length(ego_size_results))

    #   ego_size_df <- do.call(rbind, lapply(ego_size_results[!sapply(ego_size_results, is.null)], melt))
    #   return(ego_size_df)
    # }
  ego_size_list <- foreach(cluster_name = names(cluster_list), .export=ls(envir = .GlobalEnv), .combine = rbind) %dopar% {
    cluster <- cluster_list[[cluster_name]]
    if (length(cluster) > 0) {
      subgraph <- induced_subgraph(graph_obj, cluster)
      ego_size_results <- ego_size(subgraph, order = 3)
      
      ego_size_df <- data.frame(
        value = ego_size_results,
        L1 = cluster_name
      )
      
      return(ego_size_df)
    } else {
      return(data.frame(value = numeric(0), L1 = character(0)))
    }
  }

    # 結果のデバッグ出力
    write_log("ego_size_list")
    write_log(capture.output(head(ego_size_list)))

    return(ego_size_list)
  }

  # PDF出力を開始
  pdf(file.path(save_path, "ego_size_plots.pdf"))
  
  # 各データセットを処理
  for (dataset_name in x) {
    ego_size_df <- process_ego_size(dataset_name)
    assign(paste0(dataset_name, "_ego_size"), ego_size_df, envir = .GlobalEnv)
  
  # 結果のデバッグ出力
  write_log("ego_size_df")
  write_log(capture.output(head(ego_size_df)))

    p <- ggplot(ego_size_df, aes(x = value)) +
      geom_density(aes(color = L1), show.legend = FALSE) +
      labs(x = paste0(dataset_name, "_egoSize")) +
      scale_x_log10(limits = c(4, 2000)) +
      ylim(c(0.0, 2.0)) +
      my_plot2
    print(p)
  }
  
  # PDFファイルを閉じる
  dev.off()
  

  # diameter を計算する関数
  calculate_diameter_parallel <- function(name) {
    df_membership_sum <- get(paste0(name, "_membership_sum"))
    membership_df <- get(paste0(name, "_membership"))
    graph_obj <- get(paste0(name, "_graph"))
    membership_df_filter <- membership_df %>% filter(membership %in% df_membership_sum$membership[df_membership_sum$total > 1000])
    cluster_list <- split(membership_df_filter$name, membership_df_filter$membership) 
  
    # 並列処理で diameter を計算
    diameter_list <- mclapply(cluster_list, function(cluster) {
      diameter(induced_subgraph(graph_obj, cluster), weights = NA)
      })
    
    names(diameter_list) <- c(1:length(diameter_list))
  
    # 結果のデータフレームを作成
    diameter_df <- data.frame(
      diameter = unlist(diameter_list, use.names = FALSE),
      #clusterNumber = unlist(sapply(diameter_list, function(x) which(!is.na(x))), use.names = FALSE),
      library = name
    )
  
    return(diameter_df)
  }
  
  # 結果を保存するリスト
  diameter_list <- lapply(x, calculate_diameter_parallel)
  
  # 結果のデバッグ出力
  write_log("diameter_list")
  write_log(capture.output(head(diameter_list)))

  # 全てのデータフレームを結合
  combined_diameter <- do.call(rbind, diameter_list)
  combined_diameter$library <- factor(combined_diameter$library, levels = x)

  # 結果をグローバル環境に保存
  assign("combined_diameter", combined_diameter, envir = .GlobalEnv)
  
  # violin plot
  p <- ggplot(combined_diameter, aes(x = library, y = diameter)) +
    geom_violin() + 
    geom_boxplot(width = 0.1, notch = TRUE) +
    labs(x = "Library", y = "Diameter") +
    my_plot2
  
  # PDFファイルに保存
  pdf(file.path(save_path, "DiameterPERcluster.pdf"))
  print(p)
  dev.off()
  
  # 作業スペースを保存
  save.image(file.path(save_path, ".RData"))
}



# process_graph_data_parallel を呼び出す
args <- commandArgs(trailingOnly = TRUE)
x <- unlist(strsplit(args[1], ","))
read_path <- args[2]
save_path <- args[3]

process_graph_data_parallel(x, read_path, save_path)

# クラスターを停止する
stopCluster(cl)





####ここまで
#2024/07/08

# Rscript process_data10.R "V5P2_rB_S2P_1_S1,V5P2_6aB_S2P_1_S8,V5P2_24aB_S2P_1_S15" "/data/nimura_data/BIKEN20240315" "/data/nimura_data/BIKEN20240315/V5P2_S2P"
# nohup Rscript process_data10.R "V5P2_rB__CTCF_1_S3,V5P2_6aB_CTCF_1_S10,V5P2_24aB_CTCF_1_S17" "/user/med/nimura/BIKEN20240315" "/user/med/nimura/BIKEN20240315/V5P2_CTCF" &
# nohup Rscript process_data10.27ac.R "V5P2_rB_H3K27ac_1_S2,V5P2_6aB_H3K27ac_1_S9,V5P2_24aB_H3K27ac_1_S16" "/user/med/nimura/BIKEN20240315" "/user/med/nimura/BIKEN20240315/V5P2_H3K27ac" &
# nohup Rscript process_data10.27me3.R "V5P2_rB_H3K27me3_1_S4,V5P2_6aB_H3K27me3_1_S11,V5P2_24aB_H3K27me3_1_S18" "/user/med/nimura/BIKEN20240315" "/user/med/nimura/BIKEN20240315/V5P2_H3K27me3" &
# nohup Rscript process_data10.4me1.R "V5P2_rB_H3K4me1_1_S5,V5P2_6aB_H3K4me1_1_S12,V5P2_24aB_H3K4me1_1_S19" "/user/med/nimura/BIKEN20240315" "/user/med/nimura/BIKEN20240315/V5P2_H3K4me1" &


 Membership data:
                                             name S2P.t1 UEI1 UEI2 S2P.t2
AAAAAAAAACCATAATTCGCC.m1 AAAAAAAAACCATAATTCGCC.m1      1    0    0      0
GGTGGGTTATGTTACCGA.e1       GGTGGGTTATGTTACCGA.e1      0    2    0      0
TAGTTACAATATCCCTTT.e2       TAGTTACAATATCCCTTT.e2      0    0    2      0
AAAAAAAAACCCGTATTGGTC.m1 AAAAAAAAACCCGTATTGGTC.m1      1    0    0      0
GCGCCATAATGACTGGGT.e1       GCGCCATAATGACTGGGT.e1      0    2    0      0
TCGATGTCATGTAATCCA.e2       TCGATGTCATGTAATCCA.e2      0    0    2      0

                                             name S2P.t1 UEI1 UEI2 S2P.t2
AAAAAAAAACACATTTACGTC.m1 AAAAAAAAACACATTTACGTC.m1      1    0    0      0
TAACCTGCATCGCGCCTG.e1       TAACCTGCATCGCGCCTG.e1      0    2    0      0
CTAGCGTAATTATGCAAC.e2       CTAGCGTAATTATGCAAC.e2      0    0    2      0
AAAAAAAAACACGAAATGATC.m1 AAAAAAAAACACGAAATGATC.m1      1    0    0      0
TAGGACTAATTGAGCAGA.e1       TAGGACTAATTGAGCAGA.e1      0    2    0      0
CCGTAGCAATCTCCGTAT.e2       CCGTAGCAATCTCCGTAT.e2      0    0    2      0
                         membership sum
AAAAAAAAACACATTTACGTC.m1          1   1
TAACCTGCATCGCGCCTG.e1             1   2
CTAGCGTAATTATGCAAC.e2             1   2
AAAAAAAAACACGAAATGATC.m1          2   1
TAGGACTAATTGAGCAGA.e1             2   2
CCGTAGCAATCTCCGTAT.e2             2   2

Calculated total per cluster for in loop: V5P2_rB_S2P_1_S1
# A tibble: 1 × 3
  membership    total library         
       <dbl>    <int> <chr>           
1          3 15479776 V5P2_rB_S2P_1_S1


Calculated total per cluster in tmp_combined:
# A tibble: 3 × 3
  membership    total library            
       <dbl>    <int> <fct>              
1          3 15479776 V5P2_rB_S2P_1_S1   
2          2 15228853 V5P2_6aB_S2P_1_S8  
3          5  5066902 V5P2_24aB_S2P_1_S15

 ### デモデータ
 # Rscript process_data8.4.R "V5P2_rB_tmp_1_S1,V5P2_6aB_tmp_1_S8,V5P2_24aB_tmp_1_S15" "/data/nimura_data/BIKEN20240315" "/data/nimura_data/BIKEN20240315/tmp"
