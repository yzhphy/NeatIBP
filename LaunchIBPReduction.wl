(* ::Package:: *)

commandLineMode=True





If[commandLineMode,
	
	(*workingPath=$CommandLine[[-1]];(*we can specify working path in command line*)
	If[workingPath==="-script",workingPath=Directory[]<>"/"];
	If[!DirectoryQ[workingPath],
		Print["Warning: the argument working path ",workingPath," does not exist."];
		workingPath=Directory[]<>"/";
		Print["\t\t redefining working path as current working folder: ",workingPath,"."];
	];*)
	packagePath=DirectoryName[$InputFileName];
	AbsMissionInput=$CommandLine[[-1]];
	workingPath=DirectoryName[AbsMissionInput];(*is this really used?*)
	missionInput=FileNameSplit[AbsMissionInput][[-1]];
	
	
	MathematicaCommand=Import[packagePath<>"/preload/MathematicaCommand.txt"];
	ShellProcessor=Import[packagePath<>"/preload/ShellProcessor.txt"];
	EnvVarSetter=Import[packagePath<>"/preload/EnvVarSetter.txt"];
	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";
	outputPath="";
	
]








(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]


If[Get[workingPath<>missionInput]===$Failed,Print[
		"echo \"Unable to open config file "<>workingPath<>missionInput<>". Exiting.\""	
	];
	Exit[]
]





If[CutIndices==="spanning cuts",
	(*PrintAndLog[
		"!!![Notice]: the config setting CutIndices=\"spanning cuts\" is an out-of-date gramma since v1.1.0.0.\n",
		"It is still supported, but it is recommended to use the equivalent, new gramma: \n";
		"\tCutIndices={};\n",
		"\tSpanningCutsMode=True;"
	];*)(*not print in this .wl*)
	CutIndices={};
	SpanningCutsMode=True;
]


If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	
]
If[Intersection[StringSplit[outputPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["echo \"Path "<>outputPath<>" is illegal. Exiting.\""];
	Exit[0];

]
If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]





If[!FileExistsQ[outputPath<>"results/summary.txt"],
	Exit[0];(*summary failed*)
]


SetDirectory[workingPath]


TimeString[]:=Module[{at},at=FromAbsoluteTime[AbsoluteTime[]];StringRiffle[#,"_"]&[(ToString[Floor[#]]&/@at[[1,1;;6]])]<>"_"<>ToString[Floor[1000*(#-Floor[#])&[ at[[1,6]]]]]]


If[PerformIBPReduction=!=True,
	finishedTagFile="results/NeatIBP_finished.tag";
	script="";
	script=script<>"sleep 1.5"<>"\n";
	(*
		so that when the HQ sees NeatIBP_finished.tag
		the math kernel is really vacant
	*)
	script=script<>" >> "<>outputPath<>finishedTagFile<>"\n";
,
	script=Import[outputPath<>"tmp/assigned_reduction_script.sh","Text"];
]





Print[script]
