if [ -z $3 ]
	then
	echo script \for running preprocessing analysis \in feat
	echo need template .fsf file, nifti, and preprocessed structural image
	echo usage\:
	echo run_preproc.sh \<nifti\> \<fsf template\> \<structural\>
	echo e.g.
	echo run_preproc nifti.nii.gz ~/Github/reversal_pd/analysis/preproc.fsf ../../T1/T1_brain.nii.gz
	exit 1
fi

fullpath=$(readlink -f $1)
nifti=$(basename $fullpath)
dir_name=$(dirname $fullpath)

feat=$(basename $2 .fsf)
fsf=$(readlink -f $2)

struc=$(readlink -f $3)

if [ ! -e $fullpath ]
	then
	echo $fullpath does not exist
elif [ ! -e $fsf ]
	then
	echo $fsf does not exist
elif [ ! -e $struc ]
	then
	echo $struc does not exist
elif [ -e $dir_name/$feat\.feat/filtered_func_data.nii.gz ]
	then 
	echo $dir_name/$feat\.feat already completed
elif [ -d $dir_name/$feat\.feat ]
	then 
	echo $dir_name/$feat\.feat exists but did not finish
	echo please delete and try again
else
	TR=$(fslinfo $fullpath | grep pixdim4 | awk '{ print $2}')
	NVOLS=$(fslinfo $fullpath | grep ^dim4 | awk '{ print $2}')

	sed -e 's:xxTRxx:'$TR':g' -e 's:xxNVOLSxx:'$NVOLS':g' -e \
	's:xxDIRxx:'$dir_name':g' -e 's:xxNIFTIxx:'$nifti':g' -e \
	's:xxSTRUCxx:'$struc':g'<$fsf>tmp.fsf
	feat tmp.fsf #run temp file
	rm -rf tmp.fsf
fi



