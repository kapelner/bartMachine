#!/bin/bash

#$ -j y
#$ -N bhd_bakeoff_sims
#$ -t 6
#$ -q intel

echo "starting R for task # $SGE_TASK_ID"
export _JAVA_OPTIONS="-Xms128m -Xmx5300m"

R --no-save --args iter_num=$SGE_TASK_ID < r_scripts/missing_data/bhd_sims.R