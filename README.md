# hybrid_reinforcement_learning


###Fit reinforcement learning models with episodic value
The code in hybrid_data_stan.R prepares data and fits heirarchical bayesian models of reinforcement learning in Stan. The model with episodic value is written in hybrid1_rl.stan, with an example of a standard RL model in standard_rl.stan. Code in extract_RL_pars.R extracts statistics for alpha and Beta parameters, as well as Q values and prediction error timecourses for fMRI analysis.

###Make 3 column EV files for GLM from raw behavioral output
```{.bash}
for i in /data/engine/rgerraty/hybrid_mri/behavior/*output;
	do
	pwd
	matlab -nosplash -nojvm -r "addpath /home/rgerraty/GitHub/hybrid_reinforcement_learning/;make_EV(12);quit"
end
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

###Set up design matrix sub-level fixed-effects estimates
```{.matlab}
[status,subs]=system('ls -d /data/engine/rgerraty/hybrid_mri/TCST*');
subs=strread(subs,'%s');

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
dlmwrite('/data/engine/rgerraty/hybrid_mri/group_analyses/n31_subs2.txt',subs2,'')   
```