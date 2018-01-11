#!/bin/bash

# Directives
#SBATCH --account=habapsych
#SBATCH --job-name=image_preprocessing
#SBATCH -c 1
#SBATCH --time=00:75:00
#SBATCH --mem-per-cpu=8gb


bash /rigel/home/rtg2116/GitHub/hybrid_reinforcement_learning/habanero/run_preproc.sh $arg1 $arg2 $arg3

