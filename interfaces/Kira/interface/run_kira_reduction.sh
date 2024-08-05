#args:
#$(-2):KiraCommand
#$(-1):NeatIBPOutputPath

#settings:
#-s: use shortened IBP (in spanning cuts mode)

################ read args #####################

KiraNeatIBPInterfacePath=$(dirname $( realpath ${BASH_SOURCE}))

if [ $# -lt 3 ] 
then
	if [ $# -lt 2 ]
	then
		echo "run_kira_reduction: Not enough arguments. At least 2."
		exit 1
	else
		KiraCommand=$1
		NeatIBPOutputPath=$2
	fi
else
	KiraCommand=${@:$#-1:1} #$(-2)
	NeatIBPOutputPath=${@:$#:1} #$(-1)
fi

IBPInputMode="normal"
IBPInput="IBP_all.txt"
KiraUDSSubFolder="userSystem/"
modSettingS=""
forceMode=false
while getopts "sf" opt
do
	case $opt in
		s)
			IBPInputMode="shortened"
			IBPInput="IBP_all_shortened.txt"
			KiraUDSSubFolder="userSystemShortened/"
			modSettingS="-s "
			# -s with a space in the end, to auto separate when using it
			;;
		f)
			# if kira results already exist in KiraIO folder
			# defaultly keep the old results and terminate the programm
			# if -f is  declared
			# the old kira results will be erased and programm continues
			forceMode=true
			;;
		?)
			echo "run_kira_reduction.sh: There is uncategorized parameter"
			exit 1
			;;
	esac
done



if [ ${KiraNeatIBPInterfacePath: -1} != / ]; then
	KiraNeatIBPInterfacePath=$KiraNeatIBPInterfacePath/
fi

NEATIBPPATH=$KiraNeatIBPInterfacePath../../../

MathematicaCommand=`cat $NEATIBPPATH/preload/MathematicaCommand.txt`
ShellProcessor=`cat $NEATIBPPATH/preload/ShellProcessor.txt`

#if [ $# -eq 0 ]
#then
#	echo "***Must specify NeatIBPOutputPath in the first argument."
#	echo "Failed." 
#	exit 1
#fi



if [ ${NeatIBPOutputPath: -1} != / ] 
then
	NeatIBPOutputPath=$NeatIBPOutputPath/
fi
NeatIBPResultPath=$NeatIBPOutputPath"results/"

			

############## check kira old results ##################
KiraIOPath=$NeatIBPOutputPath"KiraIO/"
if $forceMode
then
	dirName="firefly_saves/"
	if [ -e $KiraIOPath$dirName ]
	then
		echo "run_kira_reduction.sh: "$KiraIOPath$dirName" exists. -f is on. Removing it."
		rm -r $KiraIOPath$dirName
		if [ $? -eq 0 ]
		then
			echo "Removed. Continuing."
		else
			echo "Failed to remove "$KiraIOPath$dirName". Exiting."
			exit 1
		fi
	fi
	dirName="results/"
	if [ -e $KiraIOPath$dirName ]
	then
		echo "run_kira_reduction.sh: "$KiraIOPath$dirName" exists. -f is on. Removing it."
		rm -r $KiraIOPath$dirName
		if [ $? -eq 0 ]
		then
			echo "Removed. Continuing."
		else
			echo "Failed to remove "$KiraIOPath$dirName". Exiting."
			exit 1
		fi
	fi
	dirName="sectormappings/"
	if [ -e $KiraIOPath$dirName ]
	then
		echo "run_kira_reduction.sh: "$KiraIOPath$dirName" exists. -f is on. Removing it."
		rm -r $KiraIOPath$dirName
		if [ $? -eq 0 ]
		then
			echo "Removed. Continuing."
		else
			echo "Failed to remove "$KiraIOPath$dirName". Exiting."
			exit 1
		fi
	fi
	dirName="tmp/"
	if [ -e $KiraIOPath$dirName ]
	then
		echo "run_kira_reduction.sh: "$KiraIOPath$dirName" exists. -f is on. Removing it."
		rm -r $KiraIOPath$dirName
		if [ $? -eq 0 ]
		then
			echo "Removed. Continuing."
		else
			echo "Failed to remove "$KiraIOPath$dirName". Exiting."
			exit 1
		fi
	fi
else
#if not force mode
	if [ -e $KiraIOPath"firefly_saves/" -o -e $KiraIOPath"results/" -o -e $KiraIOPath"sectormappings/" -o -e $KiraIOPath"tmp/" ]
	then
		echo "***run_kira_reduction.sh: Old kira results exist."
		echo "Please delete all following sub folders:"
		echo "  1. firefly_saves/"
		echo "  2. results/"
		echo "  3. sectormappings/"
		echo "  4. tmp/"
		echo "in folder"$KiraIOPath", then run again. Or use the -f setting."
		echo "Exiting."
		exit 1
	fi
fi


	


############### NeatIBP to Kira #######################

if
	[ -e $NeatIBPResultPath$IBPInput -a -e $NeatIBPResultPath"MI_all.txt" -a -e $NeatIBPResultPath"OrderedIntegrals.txt" ]
then
	$MathematicaCommand -script $KiraNeatIBPInterfacePath"NeatIBPToKira.wl" $modSettingS$NeatIBPOutputPath
else
	echo "***NeatIBP result files are not complete. We need："
	echo "  1. "$IBPInput
	echo "  2. MI_all.txt"
	echo "  3. OrderedIntegrals.txt"
	echo "in folder "$NeatIBPResultPath
	echo "Giving up converting to kira form."
	echo "Failed." 
	exit 1
fi

############### Create jobs.yaml #####################
$MathematicaCommand -script $KiraNeatIBPInterfacePath"CreateJobsYaml.wl" $modSettingS$NeatIBPOutputPath


############### Run kira #####################

if
	[ -e $KiraIOPath"basis" -a -e $KiraIOPath"list" -a -e $KiraIOPath"jobs.yaml" -a -e $KiraIOPath$KiraUDSSubFolder"userdefinedsystem.kira" ]
then
	cd $KiraIOPath
	$KiraCommand $KiraIOPath"jobs.yaml"
	cd -
	#echo $KiraCommand" "$KiraIOPath"jobs.yaml"
else
	echo "***Kira input files are not complete. We need:"
	echo "  1. "$KiraUDSSubFolder"userdefinedsystem.kira"
	echo "  2. list"
	echo "  3. basis"
	echo "  4. jobs.yaml"
	echo "in folder "$KiraIOPath
	echo "Giving up running kira."
	echo "Failed." 
	exit 1
fi

############### Read kira results #####################
$MathematicaCommand -script $KiraNeatIBPInterfacePath"ReadKiraReductionResults.wl" $NeatIBPOutputPath

