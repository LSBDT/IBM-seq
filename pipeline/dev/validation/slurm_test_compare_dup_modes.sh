#!/bin/bash
#SBATCH --job-name=test_compare_dup_modes
#SBATCH --output=logs/test_compare_dup_modes_%j.out
#SBATCH --error=logs/test_compare_dup_modes_%j.err
#SBATCH --time=24:00:00
#SBATCH --mem=128G
#SBATCH --cpus-per-task=16
#SBATCH --partition=standard

# =============================================================================
# Slurm script for test_compare_dup_modes.R
#
# Usage:
#   sbatch slurm_test_compare_dup_modes.sh
#
# Options:
#   sbatch --partition=PARTITION slurm_test_compare_dup_modes.sh
#   sbatch --cpus-per-task=32 --mem=256G slurm_test_compare_dup_modes.sh
#   sbatch --partition=high_mem --cpus-per-task=32 slurm_test_compare_dup_modes.sh
#
# Edit the variables below before submitting:
# =============================================================================

# ---- Configuration ----
NAME="V5P2_24aB_CTCF_2_3000"
DATA_DIR="data"
OUT_BASE="data/output_compare_dup_modes"
MIN_CLUSTER_SIZE=3
NUM_CORES=${SLURM_CPUS_PER_TASK}  # Use all allocated CPUs

# ---- Load R module (if needed) ----
# module load R/4.3.0  # Uncomment and adjust if using module system

# ---- Create log directory ----
mkdir -p logs

# ---- Run test_compare_dup_modes.R ----
echo "=== Job started at $(date) ==="
echo "Job ID: $SLURM_JOB_ID"
echo "Running on: $(hostname)"
echo "CPUs allocated: $SLURM_CPUS_PER_TASK"
echo "Memory allocated: $SLURM_MEM_PER_NODE MB"
echo ""

Rscript pipeline/dev/validation/test_compare_dup_modes.R \
  "${NAME}" \
  "${DATA_DIR}" \
  "${OUT_BASE}" \
  ${MIN_CLUSTER_SIZE} \
  ${NUM_CORES}

echo ""
echo "=== Job finished at $(date) ==="
