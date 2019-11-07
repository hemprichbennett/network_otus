#!/bin/sh
#$ -cwd     # Use current working directory
#$ -V     # Verbose
#$ -j y     # Maximum output, inc errors
#$ -r y     # Condense error files
#$ -pe smp 1     # Request CPU cores
#$ -l h_rt=10:00:00     # Request runtime (up to 240 hours)
#$ -l h_vmem=3G     # Request RAM per core
#$ -t 1-252



module load R


Rscript /data/scratch/btw863/network_OTUs_paper/scripts/data_generation/modularity.R ${SGE_TASK_ID}
