#args:
#$1:KiraCommand
#$2:NeatIBPOutputPath

KiraCommand=$1
NeatIBPOutputPath=$2
KiraNeatIBPInterfacePath=$(dirname $( realpath ${BASH_SOURCE}))

if [ ${KiraNeatIBPInterfacePath: -1} != / ]; then
	KiraNeatIBPInterfacePath=$KiraNeatIBPInterfacePath/
fi

NEATIBPPATH=$KiraNeatIBPInterfacePath../../../

MathematicaCommand=`cat $NEATIBPPATH/preload/MathematicaCommand.txt`
ShellProcessor=`cat $NEATIBPPATH/preload/ShellProcessor.txt`

if [ $# -eq 0 ]
then
	echo "***Must specify NeatIBPOutputPath in the first argument."
	echo "Failed." 
	exit 1
fi



if [ ${NeatIBPOutputPath: -1} != / ] 
then
	NeatIBPOutputPath=$NeatIBPOutputPath/
fi
NeatIBPResultPath=$NeatIBPOutputPath"results/"

if
	[ -e $NeatIBPResultPath"IBP_all.txt" -a -e $NeatIBPResultPath"MI_all.txt" -a -e $NeatIBPResultPath"OrderedIntegrals.txt" ]
then
	$MathematicaCommand -script $KiraNeatIBPInterfacePath"NeatIBPToKira.wl" $NeatIBPOutputPath
else
	echo "***NeatIBP result files are not complete. Giving up converting to kira form."
	echo "Failed." 
	exit 1
fi
$MathematicaCommand -script $KiraNeatIBPInterfacePath"CreateJobsYaml.wl" $NeatIBPOutputPath


KiraIOPath=$NeatIBPOutputPath"KiraIO/"
if
	[ -e $KiraIOPath"basis" -a -e $KiraIOPath"list" -a -e $KiraIOPath"jobs.yaml" -a -e $KiraIOPath"userSystem/userdefinedsystem.kira" ]
then
	$ShellProcessor $KiraCommand $KiraIOPath"jobs.yaml"
else
	echo "***Kira input files are not complete. Giving up running kira."
	echo "Failed." 
	exit 1
fi

$MathematicaCommand -script $KiraNeatIBPInterfacePath"ReadKiraReductionResults.wl" $NeatIBPOutputPath

