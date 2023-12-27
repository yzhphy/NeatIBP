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

$MathematicaCommand -script $packagePath/MissionStatusMonitor.wl $AbsMissionInput

#$MathematicaCommand -script $packagePath/MissionStatusMonitor.wl config.txt
