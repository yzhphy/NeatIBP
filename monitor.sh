packagePath=$(dirname $( realpath ${BASH_SOURCE}))
MathematicaCommand=`cat $packagePath/preload/MathematicaCommand.txt`
ShellProcessor=`cat $packagePath/preload/ShellProcessor.txt`
$MathematicaCommand -script $packagePath/MissionStatusMonitor.wl config.txt
