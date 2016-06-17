#!/bin/sh

# Directives
#PBS -N bedpost
#PBS -W group_list=yetipsych 
#PBS -l nodes=2,walltime=40:00:00,mem=1024mb
#PBS -M rtg2116@columbia.edu
#PBS -m abe
#PBS -V

# Set output and error directories
#PBS -o localhost:/vega/psych/users/rtg2116/outputs
#PBS -e localhost:/vega/psych/users/rtg2116/errors

# Print "Hello World"
export FSLPARALLEL=1

/u/6/r/rtg2116/GitHub/hybrid_reinforcement_learning/yeti/run_1st_level.sh $arg1 $arg2

# Sleep for 10 seconds


# Print date and time

# End of script
