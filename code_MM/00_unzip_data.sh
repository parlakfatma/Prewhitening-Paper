#!/bin/bash
#run from /N/project/BayesGLM_Validation/HCP

subjects_list="visit2_data/subjects.txt"

subjects=$(cat "$subjects_list")
zip_location="/N/dcwan/projects/hcp/retest"

tasks=( "SOCIAL" "RELATIONAL" "MOTOR" "LANGUAGE" "GAMBLING" "EMOTION" )

for task in "${tasks[@]}"
do
	echo $task
	zipto_location="HCP_"$task"_tmp"

	cd $zipto_location
	for subject in $subjects; do

	  zipname="$zip_location/"$subject"_3T_tfMRI_"$task"_preproc.zip"
	  zdir=$subject/MNINonLinear/Results/tfMRI_"$task"_LR
	  unzip $zipname $zdir/tfMRI_"$task"_LR_Atlas.dtseries.nii $zdir/Movement_Regressors.txt
	  zdir=$subject/MNINonLinear/Results/tfMRI_"$task"_LR/EVs
	  unzip $zipname $zdir/*.txt 
	  zdir=$subject/MNINonLinear/Results/tfMRI_"$task"_RL
	  unzip $zipname $zdir/tfMRI_"$task"_RL_Atlas.dtseries.nii $zdir/Movement_Regressors.txt
	  zdir=$subject/MNINonLinear/Results/tfMRI_"$task"_RL/EVs
	  unzip $zipname $zdir/*.txt 
	done

	cd ..

done