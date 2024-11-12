#$1:pause tag file name
#$2:command
#$3:message

sleepTime=5

while [ -f $1 ]
do
	echo $3"Waiting for "$sleepTime" second(s)."
	sleep $sleepTime
done

$2

