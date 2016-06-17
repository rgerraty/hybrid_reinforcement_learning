# Yeti Scripts
Slightly modified code for running fMRI analyses from the above directory on the yeti supercomputing cluster, in order to submit parallel jobs. Hopefully someday the scripts will be made general enough that this is unnecessary, but for now the following code will run with the directory structure present on Yeti. 


### Sync Yeti with local server
```{.bash}
rsync -az --exclude "TCST0*/*/dicoms/" --exclude "raw_comp" rgerraty@lovelace.psych.columbia.edu:/data/engine/rgerraty/hybrid_mri/ /vega/psych/users/rtg2116/hybrid_mri/
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
	pwd
	matlab -nosplash -nojvm -r "addpath /home/rgerraty/GitHub/hybrid_reinforcement_learning/;make_EV(12);quit"
end
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
for i in /data/engine/rgerraty/hybrid_mri/TCST0*/hybrid_r?/preproc_6mm_6del_100s_mc.feat/filtered_func_data.nii.gz; 
	do 
	s=$(echo $i | cut -c39-40); 
	r=$(echo $i | cut -c 50);
	if [ -e /data/engine/rgerraty/hybrid_mri/behavior/"$s"_output/EV_files/FB_pe_run"$r".txt ];
		then 
		bash /home/rgerraty/GitHub/hybrid_reinforcement_learning/run_1st_level.sh $i /home/rgerraty/GitHub/hybrid_reinforcement_learning/qchose_epval_pe.fsf;
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