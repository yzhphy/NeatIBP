KiraNeatIBPInterfacePath=$(dirname $( realpath ${BASH_SOURCE}))
MathematicaCommand=`cat $NEATIBPPATH/preload/MathematicaCommand.txt`
#ShellProcessor=`cat $NEATIBPPATH/preload/ShellProcessor.txt`
$MathematicaCommand -script $KiraNeatIBPInterfacePath/KiraToNeatIBP.wl $@
if [ -e ./config.txt -a -e ./kinematics.txt -a -e ./targetIntegrals.txt ]
then
	$NEATIBPPATH/run.sh
else
	echo "NeatIBP input files are not complete. Giving up running NeatIBP."
fi
NeatIBPOutputFolder=./outputs/kira/
NeatIBPResultFolder=./outputs/kira/results/
if
	[ -e $NeatIBPResultFolder/IBP_all.txt -a -e $NeatIBPResultFolder/MI_all.txt -a -e $NeatIBPResultFolder/OrderedIntegrals.txt ]
then
	$MathematicaCommand -script $KiraNeatIBPInterfacePath/NeatIBPToKira.wl $NeatIBPOutputFolder
else
	echo "NeatIBP result files are not complete. Giving up converting to kira form."
fi

#This interface folder can be moved to other location and it still works.
