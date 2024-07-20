packagePath=$(dirname $( realpath ${BASH_SOURCE}))
MathematicaCommand=`cat $packagePath/preload/MathematicaCommand.txt`
ShellProcessor=`cat $packagePath/preload/ShellProcessor.txt`

if [ $# -eq 0 ]
then
	AbsMissionInput=$( realpath ./config.txt)
else
	if [ -f $1 ]
	then
		AbsMissionInput=$( realpath $1)
	else
		echo $1" does not exist or is not a valid file."
		AbsMissionInput=$( realpath ./config.txt)
		echo "reseting mission input as "$AbsMissionInput
	fi
fi

$MathematicaCommand -script $packagePath/PrepareForContinueComputation.wl $AbsMissionInput
$MathematicaCommand -script $packagePath/AllMissionCompleteQ.wl $AbsMissionInput | $ShellProcessor
$MathematicaCommand -script $packagePath/Summary.wl $AbsMissionInput

#$MathematicaCommand -script $packagePath/PrepareForContinueComputation.wl config.txt
#$MathematicaCommand -script $packagePath/AllMissionCompleteQ.wl config.txt | $ShellProcessor
#$MathematicaCommand -script $packagePath/Summary.wl config.txt


$MathematicaCommand -script $packagePath/AssignIBPReduction.wl $AbsMissionInput
$MathematicaCommand -script $packagePath/LaunchIBPReduction.wl $AbsMissionInput | $ShellProcessor
