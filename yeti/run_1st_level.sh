#!/bin/bash

input=$(readlink -e $1)
fsf=$(readlink -e $2)

if [ ! -e $input ];
	then
	echo $input does not exist!
else
	output=$(dirname $1)/../$(basename $2 .fsf).feat
	
	sub=$(echo $input | cut -c 43-44 )
	run=$(echo $input | cut -c 54)
	

		if [ -e $output/stats/res4d.nii.gz ];
			then 
			echo $output completed already
		else
			if [ -d $output ];
				then
				echo Something Wrong\. Tried and failed to run $output. Trying again, but check\!
			fi
			nvols=$(fslinfo $input | grep dim4 | grep -v pix | awk '{ print $2 }');
			
			sed -e 's:XXSUBXX:'$sub':g' -e 's:XXRUNXX:'$run':g' -e 's:XXVOLSXX:'$nvols':g' -e 's:XXINXX:'$input':g'</u/6/r/rtg2116/GitHub/hybrid_reinforcement_learning/yeti/qchose_epval_pe.fsf  >$(dirname $input)/tmp.fsf;

			feat $(dirname $input)/tmp.fsf;
			rm -rf $(dirname $input)/tmp.fsf;
		fi
fi