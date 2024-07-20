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








Print["Reading Kira reduction results..."]
timer=AbsoluteTime[];


If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


kiraList=Get[outputPath<>"KiraIO/results/Tuserweight/kira_list.m"]


If[kiraList===$Failed,
	Print["Failed to read kira reduction results in ",outputPath<>"KiraIO/results/Tuserweight/kira_list.m.  Exiting."];
	Exit[];
]


integrals=Get[outputPath<>"results/OrderedIntegrals.txt"];


If[integrals===$Failed,
	Print["Failed to read integrals in ",outputPath<>"results/OrderedIntegrals.txt.  Exiting."];
	Exit[];
]


kiraListG=kiraList/.Tuserweight[x_]:>integrals[[-x]]


reductionResultsFolder=outputPath<>"results/Kira_reduction_results/"
If[!DirectoryQ[reductionResultsFolder],CreateDirectory[reductionResultsFolder]]
Export[reductionResultsFolder<>"reduced_IBP_Table.txt",kiraListG//InputForm//ToString]


Print["\tFinished. Time used: ",Round[AbsoluteTime[]-timer]," second(s)."]
