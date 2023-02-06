packagePath=$(dirname $( realpath ${BASH_SOURCE}))
math -script $packagePath/PrepareForContinueComputation.wl config.txt
math -script $packagePath/AllMissionCompleteQ.wl config.txt | sh
math -script $packagePath/Summary.wl config.txt
