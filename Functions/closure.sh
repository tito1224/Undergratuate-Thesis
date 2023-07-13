#!/bin/bash
#SBATCH --array=1-100
#SBATCH --time=00:55:00
#SBATCH --mem=35G
#SBATCH --error=Logs/rep_%A_%a.log
#SBATCH --output=Logs/rep_%A_%a.log
#SBATCH --mail-user=oadebajo@uwo.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020  gcc/9.3.0  r/4.1.2  
setrpaths.sh --path /home/oadebajo/bin/mark
Rscript --vanilla testclosure.R  ${SLURM_ARRAY_TASK_ID} &> Logs/rep_${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}.log