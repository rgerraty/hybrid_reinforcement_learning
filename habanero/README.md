# Habanero Scripts
Slightly modified code for running fMRI analyses from the above directory on the habanero supercomputing cluster, in order to submit parallel jobs. Hopefully someday the scripts will be made general enough that this is unnecessary, but for now the following code will run with the directory structure present on Habanero. 


### Sync local server with Habanero
```.bash
rsync -azv --exclude "pilot/" --exclude group_analyses/ --exclude "TCST0*/*/dicoms*" --exclude "raw_comp.nii.gz" --include "TCST*/hybrid_r?/preproc*feat/" --exclude "TCST*/hybrid_r?/*feat" --include "*/" /data/engine/engram/rgerraty/hybrid_mri/ rtg2116@habanero.rcs.columbia.edu:/rigel/psych/users/rtg2116/hybrid_mri/
```

### Run preprocessing (need to generate template .fsf file first)
```.bash
for i in /rigel/psych/users/rtg2116/hybrid_mri/TCST0*/{hybrid_r?,rest*}/*unwarp.nii.gz
do
	sbatch --export=arg1=$i,arg2=/rigel/home/rtg2116/GitHub/hybrid_reinforcement_learning/habanero/preproc_6mm_6del_100s_mc.fsf,arg3=$(dirname $i)/../structural/bravo.anat/T1_biascorr_brain.nii.gz /rigel/home/rtg2116/GitHub/hybrid_reinforcement_learning/habanero/run_preproc_sub.sh 
done
```

### Run 1st-Level GLM with Q-value, episodic value, and prediction error
```.bash
fsf=qdiff_ep_val_ch_fb_pe_oldt.fsf
for i in /rigel/psych/users/rtg2116/hybrid_mri/TCST0*/hybrid_r?/preproc_6mm_6del_100s_mc.feat/filtered_func_data.nii.gz; 
	do 
	s=$(echo $i | cut -c44-45); 
	r=$(echo $i | cut -c 55);
	if [ -e /rigel/psych/users/rtg2116/hybrid_mri/behavior/"$s"_output/EV_files/FB_pe_run"$r".txt ];
		then 
		sbatch --export=arg1=$i,arg2=/rigel/home/rtg2116/GitHub/hybrid_reinforcement_learning/habanero/$fsf,arg3=$s,arg4=$r /rigel/home/rtg2116/GitHub/hybrid_reinforcement_learning/habanero/run_1st_level_sub.sh
	else 
		echo no RL behavioral files for $i ;
	fi;
done
```
### Update 1st-level directories with registation files
```.bash
feat=inc_ep_lik.feat
for s in /rigel/psych/users/rtg2116/hybrid_mri/TCST0*/;
	do
	if [ -d $s/hybrid_r1/$feat/ ]
	then
	for r in $s/hybrid_r?/preproc_6mm_6del_100s_mc.feat/reg/; 
		do 
		if [ ! -e $r/../../$feat/reg/example_func2standard.mat ]
			then
			if [ -d $r/../../$feat/ ]
				then
					echo $r
					cp -R $r $r/../../$feat;
					cp $s/structural/bravo.anat/T1_to_MNI_lin.mat $r/../../$feat/reg/highres2standard.mat
					cp $s/structural/bravo.anat/T1_to_MNI_nonlin_field.nii.gz $r/../../$feat/reg/highres2standard_warp.nii.gz
					cp $FSLDIR/data/standard/MNI152_T1_2mm_brain.nii.gz $r/../../$feat/reg/standard.nii.gz
					updatefeatreg $r/../../$feat/
			fi
		fi
	done
	fi
done
```

### Sync local server with habanero
```.bash
rsync -az /rigel/psych/users/rtg2116/hybrid_mri/ rgerraty@lovelace.psych.columbia.edu:/data/engine/engram/rgerraty/hybrid_mri/
```


