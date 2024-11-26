(* ::Package:: *)

(*
how to use in shell:
math -script ReadKiraReductionResults.wl [outputPath]

*)





commandLineMode=True





If[commandLineMode,
	(*packagePath=DirectoryName[$InputFileName];*)
	workingPath=Directory[]<>"/";
	outputPath=$CommandLine[[-1]];

	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	(*packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";*)
	outputPath=""
	
]








(*
This function appears in many codes
see in SyzygyRed.wl for where they are
If you want to modifie this code, remember to modify all of them!
*)
PrintAndLog[x___]:=Module[{string,originalString},
	If[LogFile=!="",
		string=StringRiffle[ToString/@{x},""];
		(*Run["echo \""<>string<>"\" >> "<>LogFile]*)
		If[FileExistsQ[LogFile],
			originalString=Import[LogFile]<>"\n"
		,
			originalString=""
		];
		Export[LogFile,originalString<>string]
	];
	Print[x]
]



If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


LogPath=outputPath<>"tmp/log_files/"
If[!DirectoryQ[LogPath],CreateDirectory[LogPath]];
LogFile=LogPath<>"ReadKiraReductionResults.txt"


PrintAndLog["Reading Kira reduction results..."]
timer=AbsoluteTime[];





kiraList=Get[outputPath<>"KiraIO/results/Tuserweight/kira_list.m"]


If[kiraList===$Failed,
	PrintAndLog["Failed to read kira reduction results in ",outputPath<>"KiraIO/results/Tuserweight/kira_list.m.  Exiting."];
	Exit[];
]


integrals=Get[outputPath<>"results/OrderedIntegrals.txt"];


If[integrals===$Failed,
	PrintAndLog["Failed to read integrals in ",outputPath<>"results/OrderedIntegrals.txt.  Exiting."];
	Exit[];
]


targetListStr=Import[outputPath<>"KiraIO/list","Text"]
If[targetListStr===$Failed,
	PrintAndLog["Failed to read targets in ",outputPath<>"KiraIO/list.  Exiting."];
	Exit[];
]
targetList=ToExpression/@DeleteCases[
	StringSplit[StringReplace[targetListStr," "->""],"\n"],
	""
]
targetList=Tuserweight/@targetList
	



kiraListG=kiraList/.Tuserweight[x_]:>integrals[[-x]]
targetListG=targetList/.Tuserweight[x_]:>integrals[[-x]]


IBPTable=#->(#/.kiraListG)&/@targetListG


reductionResultsFolder=outputPath<>"results/Kira_reduction_results/"
If[!DirectoryQ[reductionResultsFolder],CreateDirectory[reductionResultsFolder]]
Export[reductionResultsFolder<>"reduced_IBP_Table.txt",IBPTable//InputForm//ToString]


PrintAndLog["\tFinished. Time used: ",Round[AbsoluteTime[]-timer]," second(s)."]
