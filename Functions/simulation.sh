#!/bin/bash
#SBATCH --array=1-16
#SBATCH --time=00:10:00
#SBATCH --mem=1G
#SBATCH --error=Logs/rep_%A_%a.log
#SBATCH --output=Logs/rep_%A_%a.log

module load StdEnv/2020  gcc/9.3.0  r/4.1.2

Rscript --vanilla runSimulation.R $SLURM_ARRAY_TASK_ID &> Logs/rep_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.log