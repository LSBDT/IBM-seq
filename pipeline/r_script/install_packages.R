#!/usr/bin/env Rscript
# =============================================================================
# install_packages.R
# IBMseq パイプラインに必要な R パッケージをインストールする
#
# 使い方:
#   Rscript install_packages.R
#
# CRAN パッケージ: data.table, igraph, ggplot2
# parallel は R 標準ライブラリに含まれるためインストール不要
# =============================================================================

cat("=== IBMseq パッケージインストール ===\n\n")
cat("R バージョン: "); cat(R.version.string); cat("\n")
cat("ライブラリパス:\n"); cat(paste(" ", .libPaths(), collapse="\n")); cat("\n\n")

required_pkgs <- c("data.table", "igraph", "ggplot2")

# ---- 既存インストール確認 ----
cat("--- インストール状況確認 ---\n")
already_installed <- vapply(required_pkgs, function(pkg) {
  installed <- requireNamespace(pkg, quietly = TRUE)
  cat(sprintf("  %-12s : %s\n", pkg, if (installed) "OK (installed)" else "NOT FOUND"))
  installed
}, logical(1))

needs_install <- required_pkgs[!already_installed]

if (length(needs_install) == 0) {
  cat("\nすべてのパッケージがインストール済みです。\n")
} else {
  cat(paste0("\n以下をインストールします: ", paste(needs_install, collapse = ", "), "\n\n"))

  # CRAN ミラーを指定（日本）
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  for (pkg in needs_install) {
    cat(paste0("--- Installing: ", pkg, " ---\n"))
    tryCatch({
      install.packages(pkg, dependencies = TRUE)
      cat(paste0("  OK: ", pkg, "\n"))
    }, error = function(e) {
      cat(paste0("  ERROR: ", pkg, " - ", conditionMessage(e), "\n"))
    })
  }
}

# ---- 最終確認 ----
cat("\n--- インストール後の確認 ---\n")
all_ok <- TRUE
for (pkg in required_pkgs) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  cat(sprintf("  %-12s : %s\n", pkg, if (ok) "OK" else "FAILED"))
  if (!ok) all_ok <- FALSE
}

# parallel は標準ライブラリ確認
parallel_ok <- requireNamespace("parallel", quietly = TRUE)
cat(sprintf("  %-12s : %s (標準ライブラリ)\n", "parallel", if (parallel_ok) "OK" else "FAILED"))

cat("\n")
if (all_ok && parallel_ok) {
  cat("=== すべてのパッケージが利用可能です ===\n")
} else {
  cat("=== 一部のパッケージに問題があります。上記のエラーを確認してください ===\n")
  quit(status = 1)
}
