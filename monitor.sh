packagePath=$(dirname $( realpath ${BASH_SOURCE}))
math -script $packagePath/MissionStatusMonitor.wl config.txt
