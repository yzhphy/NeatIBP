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
	outputPath="/home/zihao/NeatIBP/examples_private/kira_interface/examples/dbox-demo/outputs/kira/"
	
]








(*
This function appears in many codes
1. SyzygyRed.wl
2. Several or all .wl codes in interfaces/Kira/interface/
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



LogFile=outputPath<>"ReadKiraReductionResults_log.txt"


PrintAndLog["Reading Kira reduction results..."]
timer=AbsoluteTime[];


If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


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


kiraListG=kiraList/.Tuserweight[x_]:>integrals[[-x]]


reductionResultsFolder=outputPath<>"results/Kira_reduction_results/"
If[!DirectoryQ[reductionResultsFolder],CreateDirectory[reductionResultsFolder]]
Export[reductionResultsFolder<>"reduced_IBP_Table.txt",kiraListG//InputForm//ToString]


PrintAndLog["\tFinished. Time used: ",Round[AbsoluteTime[]-timer]," second(s)."]
