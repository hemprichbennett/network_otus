#!/bin/sh
#$ -cwd     # Use current working directory
#$ -V     # Verbose
#$ -j y     # Maximum output, inc errors
#$ -r y     # Condense error files
#$ -pe smp 1     # Request CPU cores
#$ -l h_rt=240:00:00     # Request runtime (up to 240 hours)
#$ -l h_vmem=15G     # Request RAM per core
#$ -t 1-48



module load R/3.4.3


Rscript /data/scratch/btw863/network_OTUs_paper/scripts/data_generation/fine_network_netlevel.R ${SGE_TASK_ID}
