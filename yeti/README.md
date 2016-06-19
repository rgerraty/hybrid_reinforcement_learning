# Yeti Scripts
Slightly modified code for running fMRI analyses from the above directory on the yeti supercomputing cluster, in order to submit parallel jobs. Hopefully someday the scripts will be made general enough that this is unnecessary, but for now the following code will run with the directory structure present on Yeti. 


### Sync Yeti with local server
```{.bash}
rsync -az --exclude "TCST0*/*/dicoms/" --exclude "raw_comp" rgerraty@lovelace.psych.columbia.edu:/data/engine/rgerraty/hybrid_mri/ /vega/psych/users/rtg2116/hybrid_mri/
```

###Run preprocessing (need to generate template .fsf file first)
```{.bash}
for i in /vega/psych/users/rtg2116/hybrid_mri/TCST0*/{hybrid_r?,rest*}/*unwarp.nii.gz
do
	qsub -v arg1=$i, arg2=/u/6/r/rtg2116/GitHub/hybrid_reinforcement_learning/yeti/preproc_6mm_6del_100s_mc.fsf, arg3=$(dirname $i)/../structural/bravo.anat/T1_biascorr_brain.nii.gz
	/u/6/r/rtg2116/GitHub/hybrid_reinforcement_learning/yeti/run_preproc_sub.sh 
done
```

###Run 1st-Level GLM with Q-value, episodic value, and prediction error
```{.bash}
for i in /vega/psych/users/rtg2116/hybrid_mri/TCST0*/hybrid_r?/preproc_6mm_6del_100s_mc.feat/filtered_func_data.nii.gz; 
	do 
	s=$(echo $i | cut -c43-44); 
	r=$(echo $i | cut -c 54);
	if [ -e /vega/psych/users/rtg2116/hybrid_mri/behavior/"$s"_output/EV_files/FB_pe_run"$r".txt ];
		then 
		qsub -v arg1=$i,arg2=/u/6/r/rtg2116/GitHub/hybrid_reinforcement_learning/yeti/qchose_epval_pe.fsf /u/6/r/rtg2116/GitHub/hybrid_reinforcement_learning/yeti/run_1st_level_sub.sh
	else 
		echo no RL behavioral files for $i ;
	fi;
done
```
###Update 1st-level directories with registation files
```{.bash}
for s in /vega/psych/users/rtg2116/hybrid_mri/TCST0*/;
	do
	if [ -d $s/hybrid_r1/qchose_epval_pe.feat/ ]
	then
	for r in $s/hybrid_r?/preproc_6mm_6del_100s_mc.feat/reg/; 
		do 
		if [ ! -e $r/../../qchose_epval_pe.feat/reg/example_func2standard.mat ]
			then
			echo $r
			cp -R $r $r/../../qchose_epval_pe.feat/;
			cp $s/structural/bravo.anat/T1_to_MNI_lin.mat $r/../../qchose_epval_pe.feat/reg/highres2standard.mat
			cp $s/structural/bravo.anat/T1_to_MNI_nonlin_field.nii.gz $r/../../qchose_epval_pe.feat/reg/highres2standard_warp.nii.gz
			cp $FSLDIR/data/standard/MNI152_T1_2mm_brain.nii.gz $r/../../qchose_epval_pe.feat/reg/standard.nii.gz
			updatefeatreg $r/../../qchose_epval_pe.feat/
		fi
	done
	fi
done
```

### Sync local server with yeti
```{.bash}
rsync -az --exclude "TCST0*/*/dicoms/" --exclude "raw_comp" /vega/psych/users/rtg2116/hybrid_mri/ rgerraty@lovelace.psych.columbia.edu:/data/engine/rgerraty/hybrid_mri/
```


