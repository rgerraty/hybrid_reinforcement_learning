# hybrid_reinforcement_learning


###Fit reinforcement learning models with episodic value
The code in hybrid_data_stan.R prepares data and fits heirarchical bayesian models of reinforcement learning in Stan. The model with episodic value is written in hybrid1_rl.stan, with an example of a standard RL model in standard_rl.stan. Code in extract_RL_pars.R extracts statistics for alpha and Beta parameters, as well as Q values and prediction error timecourses for fMRI analysis.


### Generate field map for B0 correction
```{.bash}
for i in /data/engine/rgerraty/hybrid_mri/TCST0*/B0_map;  
do 
	bo=$(ls $i/2*nii.gz);
	echo $bo; 
	bash /home/rgerraty/GitHub/NETPD/reversal_learning_pd/analysis/B0_unwarp.sh $bo; 
done
```

### Run anatomical preprocessing
```{.bash}
for i in /data/engine/rgerraty/hybrid_mri/TCST0*/structural/;
do 
	if [ -d $i/bravo.anat ]
		then
		echo fsl_anat already run for $i
	else
		if [ ! -e $i/bravo.nii.gz ]
			then
			bravo=$(ls $i/co*nii.gz | head -n1)
			mv $bravo $i/bravo.nii.gz
		fi
		fsl_anat -i $i/bravo.nii.gz
	fi
done
```

### B0 field correction for EPI scans
```{.bash}
for i in /data/engine/rgerraty/hybrid_mri/TCST0*/{hybrid_r?,rest*}/
do

	unwarp=$(ls $i/*_unwarp.nii.gz 2>/dev/null)
	epi=$(ls $i/*nii.gz | grep -v unwarp)

	if [[ -z $epi ]]
		then 
		echo no niftis in $i\!
	elif [[ ! -z $unwarp ]]
		then
		echo B0 field already generated \in $i
		echo delete before proceeding
	else
		dwell=$(echo $(dicom_hdr $i/dicoms/$(ls $i/dicoms/ | 
			head -n 1) | 
			grep 0043\ 102c | 
			awk 'BEGIN{ FS="//" }; { print $3 }') /1000000 | 
			bc -l) 

		fmap=$(ls $i/../B0_map/fieldmap_rads.nii.gz)

		fugue -i $epi --dwell=$dwell \
		--loadfmap=$fmap \
		-u $(dirname $epi)/$(basename $epi .nii.gz)_unwarp.nii.gz
	fi
done
```
### Get partially saturated first volume from every 4D epi volume as reference image
```{.bash}
for i in /data/engine/rgerraty/hybrid_mri/TCST0*/{hybrid_r?,rest*}/*unwarp.nii.gz
do
	if [ ! -e $(dirname $i)/example_func.nii.gz ]
		then
		fslroi $i $(dirname $i)/example_func.nii.gz 0 1
		bet $(dirname $i)/example_func.nii.gz $(dirname $i)/example_func.nii.gz 
	else
		echo example_func.nii.gz already exists in $(dirname $i)
	fi
done
```

###Run preprocessing (need to generate template .fsf file first)
```{.bash}
for i in /data/engine/rgerraty/hybrid_mri/TCST0*/{hybrid_r?,rest*}/*unwarp.nii.gz
do
	/home/rgerraty/GitHub/hybrid_reinforcement_learning/run_preproc.sh $i \
	/home/rgerraty/GitHub/hybrid_reinforcement_learning/preproc_6mm_6del_100s_mc.fsf \
	$(dirname $i)/../structural/bravo.anat/T1_biascorr_brain.nii.gz
done
```

###Make 3 column EV files for GLM from raw behavioral output
```{.bash}
for i in /data/engine/rgerraty/hybrid_mri/behavior/*output;
	do
	cd $i
	pwd
	matlab -nosplash -nojvm -r "addpath /home/rgerraty/GitHub/hybrid_reinforcement_learning/;make_EV(12);quit"
done
```

###Generated extended confounds from motion paramaters
```{.bash}
for i in /data/engine/rgerraty/hybrid_mri/TCST0*/hybrid_r?/preproc*feat/mc/; 
	do 
	cd $i; 
	pwd
	mp_diffpow.sh prefiltered_func_data_mcf.par diff; 
	paste prefiltered_func_data_mcf.par diff.dat>extended_confs_24par.txt;
done
```

###Run 1st-Level GLM with Q-value, episodic value, and prediction error
```{.bash}
fsf=epvalue_newold.fsf
for i in /data/engine/rgerraty/hybrid_mri/TCST0*/hybrid_r?/preproc_6mm_6del_100s_mc.feat/filtered_func_data.nii.gz; 
	do 
	s=$(echo $i | cut -c39-40); 
	r=$(echo $i | cut -c 50);
	if [ -e /data/engine/rgerraty/hybrid_mri/behavior/"$s"_output/EV_files/FB_pe_run"$r".txt ];
		then 
		bash /home/rgerraty/GitHub/hybrid_reinforcement_learning/run_1st_level.sh $i /home/rgerraty/GitHub/hybrid_reinforcement_learning/$fsf;
	else 
		echo no RL behavioral files for $i ;
	fi;
done
```

###Update 1st-level directories with registation files
```{.bash}
for s in /data/engine/rgerraty/hybrid_mri/TCST0*/;
	do
	if [ -d $s/hybrid_r1/qchose_epval_pe.feat/ ]
	then
	for r in $s/hybrid_r?/preproc_6mm_6del_100s_mc.feat/reg/; 
		do 
		if [ ! -e $r/../../qchose_epval_pe.feat/reg/example_func2standard.mat ]
			then
			if [ -d $r/../../qchose_epval_pe.feat/ ]
				then
					echo $r
					cp -R $r $r/../../qchose_epval_pe.feat/;
					cp $s/structural/bravo.anat/T1_to_MNI_lin.mat $r/../../qchose_epval_pe.feat/reg/highres2standard.mat
					cp $s/structural/bravo.anat/T1_to_MNI_nonlin_field.nii.gz $r/../../qchose_epval_pe.feat/reg/highres2standard_warp.nii.gz
					cp $FSLDIR/data/standard/MNI152_T1_2mm_brain.nii.gz $r/../../qchose_epval_pe.feat/reg/standard.nii.gz
					updatefeatreg $r/../../qchose_epval_pe.feat/
			fi
		fi
	done
	fi
done
```

###Set up group design matrix for subject-level fixed-effects estimates
```{.matlab}
[status,subs]=system('ls -d /data/engine/rgerraty/hybrid_mri/TCST*');
subs=strread(subs,'%s');
subs2=[];

j=1;
i=1;
for s=1:size(subs,1)

	[status,runs]=system(['ls -d ',subs{s},'/hybrid_r?/qchose_epval_pe.feat']);
	runs=strread(runs,'%s');
	if status==0
		for r=1:size(runs,1)
			design_mat(i,j)=1;
			i=i+1;
		end
		subs2=[subs2;runs];
		j=j+1;
	end
end
con_mat=eye(size(design_mat,2));
dlmwrite('/data/engine/rgerraty/hybrid_mri/group_analyses/n31_fe_design.mat',design_mat, ' ')
dlmwrite('/data/engine/rgerraty/hybrid_mri/group_analyses/n31_fe_con.mat',con_mat, ' ')
dlmwrite('/data/engine/rgerraty/hybrid_mri/group_analyses/n31_subs.txt',subs2,'')   
```