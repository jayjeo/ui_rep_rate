#!/bin/bash
#SBATCH --job-name=bootsrap
#SBATCH --account=pi-ganong
#SBATCH --time=15:00:00
#SBATCH --partition=broadwl
#SBATCH --ntasks-per-node=10
#SBATCH --ntasks=10
#SBATCH --mail-type=ALL
#SBATCH --mem-per-cpu=5G
module load R

Rscript --quiet --no-restore --no-save analysis/source/run_bootstraps.R
