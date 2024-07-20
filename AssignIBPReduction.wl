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


If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting.";Exit[]]]


If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	
]
If[Intersection[StringSplit[outputPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["echo \"Path "<>outputPath<>" is illegal. Exiting.\""];
	Exit[0];

]
If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


SetDirectory[workingPath]


TimeString[]:=Module[{at},at=FromAbsoluteTime[AbsoluteTime[]];StringRiffle[#,"_"]&[(ToString[Floor[#]]&/@at[[1,1;;6]])]<>"_"<>ToString[Floor[1000*(#-Floor[#])&[ at[[1,6]]]]]]


If[PerformIBPReduction=!=True,
	Exit[];
]





Switch[IBPReductionMethod,
"None",
	script="echo \"IBPReductionMethod=None, will not perform IBP reduction.\"\n";
,
"Kira",
	KiraCommandRefined=StringRiffle[
		DeleteCases[
			StringSplit[KiraCommand," "]
		,""]
	," "];
	If[!FileExistsQ[FermatPath],
		Print["***File at ",FermatPath," does not exist. Exiting."];
		Exit[];
	];
	If[FileNameSplit[FermatPath][[-1]]=!="fer64",
		Print["***FermatPath ",FermatPath," should be specified to \"fer64\". Exiting."];
		Exit[];
	];
	If[Count[
		FileExistsQ/@Select[
			StringSplit[KiraCommandRefined," "],
			FileNameSplit[#][[-1]]==="kira"&
		]
	,True]<1,
		Print["***KiraCommand \"",KiraCommand,"\" is not an correct kira command. Please check. Exiting."];
		Exit[];
	];(*kira command may include some settings*)
	Switch[EnvVarSetter,
	"export",
		script="export FERMATPATH=\""<>FermatPath<>"\"";
		(*script=script<>"\n"<>"export KiraCommand=\""<>KiraCommand<>"\"";*)
	,
	"setenv",
		script="setenv FERMATPATH \""<>FermatPath<>"\"";
		(*script=script<>"\n"<>"setenv KiraCommand \""<>KiraCommand<>"\"";*)
	,
	_,
		Print[" ***EnvVarSetter \"",EnvVarSetter,"\" is not supported yet."];
		Print["Valid values:"];
		Print["\t1. export"];
		Print["\t2. setenv"];
		Print["Please check the spelling or contact the developers of NeatIBP."];
		Print["Exiting."];
		Exit[];
	];
	script=script<>"\n"<>ShellProcessor<>" "<>packagePath<>
		"interfaces/Kira/interface/run_kira_reduction.sh \""<>KiraCommandRefined<>"\" "<>outputPath<>"\n";
,
"FFNumerical",
	script=MathematicaCommand<>" "<>packagePath<>"FFSolveIBP.wl "<>outputPath<>"\n";
,
_,
	(*this should have been excluded by initialization, but... better double checked*)
	Print["***Err: Unkown IBPReductionMethod ",IBPReductionMethod];(*we'd better use PrintAndLog*)
	Exit[];
]


Export[outputPath<>"tmp/assigned_reduction_script.sh",script,"Text"]
