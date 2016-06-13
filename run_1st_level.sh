#!/bin/bash

input=$1
fsf=$2

if [ ! -e $input ];
	then
	echo $input does not exist!
else
	output=$(dirname $1)/../$(basename $2 .fsf).feat
	
	sub=$(echo $input | cut -c 39-40 )
	run=$(echo $input | cut -c 50)
	

		if [ -e $output/stats/res4d.nii.gz ];
			then 
			echo $output completed already
		else
			if [ -d $output ];
				then
				echo Something Wrong\. Tried and failed to run $output. Trying again, but check\!
			fi
			nvols=$(fslinfo $input | grep dim4 | grep -v pix | awk '{ print $2 }');
			
			sed -e 's:XXSUBXX:'$sub':g' -e 's:XXRUNXX:'$run':g' -e 's:XXVOLSXX:'$nvols':g'-e 's:XXINXX:'$input':g'</home/rgerraty/GitHub/hybrid_reinforcement_learning/qchose_epval_pe.fsf  >tmp.fsf;

			feat tmp.fsf;
			rm -rf tmp.fsf;
		fi
fi