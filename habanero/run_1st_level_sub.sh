#!/bin/bash

# Directives
#SBATCH --account=psych
#SBATCH --job-name=1st_level_GLM
#SBATCH -c 1
#SBATCH --time=00:100:00
#SBATCH --mem-per-cpu=8gb

bash /rigel/home/rtg2116/GitHub/hybrid_reinforcement_learning/habanero/run_1st_level.sh $arg1 $arg2 $arg3 $arg4

