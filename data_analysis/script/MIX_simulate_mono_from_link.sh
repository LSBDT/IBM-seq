#!/bin/bash
# simulation for MIX mono from link file
# update 2026/03/04 ike

Antibody=$1
Number=$2
LINKFILE=$3
LINKDIR=$4
OUTDIR=$5

inpath="$LINKDIR/$LINKFILE"
base="${LINKFILE%.link.gz}"

mkdir -p "$OUTDIR"

process_sample() {
  local infile="$1"
  local base="$2"
  local n_take="$3"
  local ratio="$4"

  local idfile
  idfile="$(mktemp)"
  trap 'rm -f "$idfile"' RETURN

  # 先に対象抗体(Antibody.t1 / Antibody.t2)のIDを集めてユニーク → サンプリング
  zcat "$infile" \
    | awk -F'\t' '{print $1 "\t" $3; print $2 "\t" $4}' \
    | sort -u \
    | awk -F'\t' -v A="$Antibody" '$2 ~ ("^" A "\\.t[12]$") {print $1}' \
    | shuf -n "$n_take" > "$idfile"

  local got
  got="$(wc -l < "$idfile" | tr -d ' ')"

  echo "ratio $ratio -> requested=$n_take, sampled=$got"

  # IDリストに含まれるノードを持つエッジを除外（mono化）
  awk -v OFS="\t" '
    NR==FNR { id[$1]=1; next }
    !($1 in id || $2 in id)
  ' "$idfile" <(zcat "$infile") \
    | gzip > "$OUTDIR/${base}_${Antibody}_${ratio}_mono.link.gz"
}

# 例: 複数比率にしたいならここを変える
for ratio in 100; do
  num=$(( Number * ratio / 100 ))
  echo "=== ratio $ratio -> number=$num ==="
  process_sample "$inpath" "$base" "$num" "$ratio"
done