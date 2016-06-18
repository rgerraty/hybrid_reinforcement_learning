#!/bin/sh

# Directives
#PBS -N image preprocessing
#PBS -W group_list=yetipsych 
#PBS -l nodes=2,walltime=75:00:00,mem=4000mb
#PBS -M rtg2116@columbia.edu
#PBS -m abe
#PBS -V

# Set output and error directories
#PBS -o localhost:/vega/psych/users/rtg2116/
#PBS -e localhost:/vega/psych/users/rtg2116/


export FSLPARALLEL=1

/u/6/r/rtg2116/GitHub/hybrid_reinforcement_learning/yeti/run_preproc.sh $arg1 $arg2 $arg3

