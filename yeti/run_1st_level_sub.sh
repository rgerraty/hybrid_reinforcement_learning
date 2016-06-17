#!/bin/sh

# Directives
#PBS -N 1st_level_GLM
#PBS -W group_list=yetipsych 
#PBS -l nodes=2,walltime=75:00:00,mem=2048mb
#PBS -M rtg2116@columbia.edu
#PBS -m abe
#PBS -V

# Set output and error directories
#PBS -o localhost:/vega/psych/users/rtg2116/outputs
#PBS -e localhost:/vega/psych/users/rtg2116/errors

export FSLPARALLEL=1

bash /u/6/r/rtg2116/GitHub/hybrid_reinforcement_learning/yeti/run_1st_level.sh $arg1 $arg2

