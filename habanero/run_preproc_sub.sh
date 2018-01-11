#!/bin/bash

# Directives
#SBATCH --jobname=image_preprocessing
#SBATCH --account=rtg2116
#SBATCH -c 1
#SBATCH --time=00:75:00
#SBATCH --mem-per-cpu=8gb


bash /rigel/home/rtg2116/GitHub/hybrid_reinforcement_learning/habanero/run_preproc.sh $arg1 $arg2 $arg3

