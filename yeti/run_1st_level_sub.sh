#!/bin/sh

# Directives
#PBS -N bedpost
#PBS -W group_list=yetipsych 
#PBS -l nodes=2,walltime=40:00:00,mem=1024mb
#PBS -M rtg2116@columbia.edu
#PBS -m abe
#PBS -V

# Set output and error directories
#PBS -o localhost:/vega/psych/users/rtg2116/
#PBS -e localhost:/vega/psych/users/rtg2116/

# Print "Hello World"
export FSLPARALLEL=1

/vega/psych/app/fsl/bin/bedpostx $arg1

# Sleep for 10 seconds


# Print date and time

# End of script
