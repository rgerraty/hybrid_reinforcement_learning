#!/bin/bash

input=$(readlink -e $1)
fsf=$(readlink -e $2)
sub=$3
run=$4

if [ ! -e $input ];
	then
	echo $input does not exist!

elif [ -z $run ];
	then
	echo Usage\:
	echo run_1st_level.sh \<input nifti\> \<template fsf\> \<subject number\> \<run number\>

else
	output=$(dirname $input)/../$(basename $fsf .fsf).feat

		if [ -e $output/stats/res4d.nii.gz ];
			then 
			echo $output completed already
		else
			if [ -d $output ];
				then
				echo Something Wrong\. Tried and failed to run $output. Trying again, but check\!
			fi
			nvols=$(fslinfo $input | grep dim4 | grep -v pix | awk '{ print $2 }');
			
			sed -e 's:XXSUBXX:'$sub':g' -e 's:XXRUNXX:'$run':g' -e 's:XXVOLSXX:'$nvols':g' -e 's:XXINXX:'$input':g' $fsf >$(dirname $input)/$sub$run\_tmp.fsf;

			feat $(dirname $input)/$sub$run\_tmp.fsf;
			rm -rf $(dirname $input)/$sub$run\_tmp.fsf;
		fi
fi
