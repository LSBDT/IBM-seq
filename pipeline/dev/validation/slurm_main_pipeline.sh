#!/bin/bash
#SBATCH --job-name=ibmseq_pipeline
#SBATCH --output=logs/ibmseq_pipeline_%j.out
#SBATCH --error=logs/ibmseq_pipeline_%j.err
#SBATCH --time=48:00:00
#SBATCH --mem=256G
#SBATCH --cpus-per-task=16
#SBATCH --partition=standard

# =============================================================================
# Slurm script for IBMseq main pipeline (00_main.R)
#
# Usage:
#   sbatch slurm_main_pipeline.sh
#
# Options:
#   sbatch --partition=PARTITION slurm_main_pipeline.sh
#   sbatch --cpus-per-task=32 --mem=512G slurm_main_pipeline.sh
#   sbatch --partition=high_mem --cpus-per-task=32 slurm_main_pipeline.sh
#
# Edit the variables below before submitting:
# =============================================================================

# ---- Configuration ----
NAME="V5P2_24aB_CTCF_2_3000"
READ_PATH="data"
SAVE_PATH="data/output"
DUP_MODE="--no-dup"              # Options: --no-dup, --with-dup, --dup-then-dedup
MIN_CLUSTER_SIZE=1000
NUM_CORES=${SLURM_CPUS_PER_TASK}  # Use all allocated CPUs
METRICS="edge_density,umi_uei,ego_size,diameter"  # Or specify subset

# ---- Load R module (if needed) ----
# module load R/4.3.0  # Uncomment and adjust if using module system

# ---- Create log directory ----
mkdir -p logs

# ---- Run main pipeline ----
echo "=== Job started at $(date) ==="
echo "Job ID: $SLURM_JOB_ID"
echo "Running on: $(hostname)"
echo "CPUs allocated: $SLURM_CPUS_PER_TASK"
echo "Memory allocated: $SLURM_MEM_PER_NODE MB"
echo ""

Rscript pipeline/r_script/00_main.R \
  "${NAME}" \
  "${READ_PATH}" \
  "${SAVE_PATH}" \
  ${DUP_MODE} \
  ${MIN_CLUSTER_SIZE} \
  --cores=${NUM_CORES} \
  --metrics=${METRICS}

echo ""
echo "=== Job finished at $(date) ==="
