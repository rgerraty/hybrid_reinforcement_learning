#!/bin/bash

######EDITABLE PARAMETERS#######
path=/data/engine/rgerraty/hybrid_mri/ #absolute path to subject folders
subjectlist=$(ls -d $path/TCST*) #list of subject folders
feat=qchose_epval_pe
copes=(1 1 1 1 1)  #if excluding any - code as 0
numcopes=`echo ${#copes[*]}`

#################################

for i in $subjectlist
    do 
    subject=`basename $i`
    runlist=$(ls -d $i/*/$feat\.feat)
    numsess=$(ls -d $runlist | wc -l)

    if [[ ! -z $runlist ]]
	then
#check for previously attempted analyses

    	if [ -a $i/$feat\_fe_combined.gfeat/cope1.feat/stats/res4d.nii.gz ];
    	    then echo fixed-effects already run $subject\'s $feat runs
    	else

	# clear the variables before defining them
    		unset cope
    		unset inputs 
    		unset EVs 
   		unset groups 

#make sure it output right

    		for ((k=1; k<=$numcopes; k++))
        		do
        		cope=""$cope"\\ \nset fmri(copeinput."$k")  `echo ${copes[$k-1]}`"
    		done
     
    		j=1;
    		for l in $runlist
        		do
        		inputs=""$inputs"\\ \nset feat_files("$j") \""$l"\""
        		EVs=""$EVs"\\ \nset fmri(evg"$j".1) 1"
        		groups=""$groups"\\ \nset fmri(groupmem."$j") 1"
			j=$((j+1));
    		done
		output=$(echo $i/$feat\.gfeat)

		    sed -e 's:XXOUTPUTXX:'$output':g' -e 's:XXNUMSESSXX:'$numsess':g' -e 's:XXNUMCOPESXX:'$numcopes':g' -e "s:XXCOPESXX:$cope:g" -e "s:XXINPUTSXX:$inputs:g" -e "s:XXEVsXX:$EVs:g" -e "s:XXGROUPSXX:$groups:g"</home/rgerraty/GitHub/hybrid_reinforcement_learning/qchose_epval_pe_sub_level.fsf>tmp.fsf

		    feat tmp.fsf; rm -rf tmp.fsf
	fi
    fi
done

