packagePath=$(dirname $( realpath ${BASH_SOURCE}))
MathematicaCommand=`cat $packagePath/preload/MathematicaCommand.txt`
ShellProcessor=`cat $packagePath/preload/ShellProcessor.txt`
$MathematicaCommand -script $packagePath/PrepareForContinueComputation.wl config.txt
$MathematicaCommand -script $packagePath/AllMissionCompleteQ.wl config.txt | $ShellProcessor
$MathematicaCommand -script $packagePath/Summary.wl config.txt
