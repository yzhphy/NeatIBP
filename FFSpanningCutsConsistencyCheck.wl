(* ::Package:: *)

commandLineMode=True





If[commandLineMode,
	packagePath=DirectoryName[$InputFileName];
	(*workingPath=$CommandLine[[-1]];(*we can specify working path in command line*)
	If[workingPath==="-script",workingPath=Directory[]<>"/"];
	If[!DirectoryQ[workingPath],
		Print["Warning: the argument working path ",workingPath," does not exist."];
		workingPath=Directory[]<>"/";
		Print["\t\t redefining working path as current working folder: ",workingPath,"."];
	];*)
	workingPath=Directory[]<>"/";(*is this really used?*)
	checkPath=$CommandLine[[-1]];
	MathematicaCommand=Import[packagePath<>"/preload/MathematicaCommand.txt"];
	ShellProcessor=Import[packagePath<>"/preload/ShellProcessor.txt"];
	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";
	checkPath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/Examples_in_the_paper/NP7-spanning_cuts_test/outputs/2l4pNP7_spanning_cuts_test-5"
	
]








SetDirectory[workingPath]


TimeString[]:=Module[{at},at=FromAbsoluteTime[AbsoluteTime[]];StringRiffle[#,"_"]&[(ToString[Floor[#]]&/@at[[1,1;;6]])]<>"_"<>ToString[Floor[1000*(#-Floor[#])&[ at[[1,6]]]]]]


If[StringSplit[checkPath,""][[-1]]=!="/",checkPath=checkPath<>"/"]


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



LogPath=checkPath<>"tmp/log_files/"
If[!DirectoryQ[LogPath],CreateDirectory[LogPath]];
LogFile=LogPath<>"FFSpanningCutsConsistencyCheck.txt"


If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
Get[checkPath<>"inputs/config.txt"]
(*If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	PrintAndLog["Output path has been set as "<>outputPath]
]*)
outputPath=checkPath


(*renaming the setting, because NeatIBP... actually, dose not perform "reduction" by default*)
If[ValueQ[ReductionOutputName],
	If[ReductionOutputName=!=OutputName,
		If[OutputName==="Untitled",
			ReductionOutputName=ReductionOutputName;
			(*use ReductionOutputName*)
		,
			ReductionOutputName=OutputName
		]
	]
,
	ReductionOutputName=OutputName
]
(*this is not needed here I guess, since there is outputPath=checkPath above ---2024.12.25*)


If[CutIndices==="spanning cuts",
	PrintAndLog[
		"!!![Notice]: the config setting CutIndices=\"spanning cuts\" is an out-of-date gramma since v1.1.0.0.\n",
		"It is still supported, but it is recommended to use the equivalent, new gramma: \n",
		"\tCutIndices={};\n",
		"\tSpanningCutsMode=True;"
	];
	CutIndices={};
	SpanningCutsMode=True;
]


TemporaryDirectory=outputPath<>"tmp"

(*Get[packagePath<>"SyzygyRed.wl"]*)

Get[packagePath<>"SparseRREF/SparseRREF.m"]



SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse






PrintAndLog["=================================================="];
PrintAndLog["Checking at ",checkPath]
PrintAndLog["-------------------------------------------------"]


targetFileName=FileNameSplit[targetIntegralsFile][[-1]]
kinematicsFileName=FileNameSplit[kinematicsFile][[-1]]


spanningCuts=Get[outputPath<>"tmp/spanningCuts.txt"]



If[ConsistencyCheckParallelization,
	For[i=1,i<=Length[spanningCuts],i++,
		cut=spanningCuts[[i]];
		solveFolder=outputPath<>"results/results_spanning_cuts/cut_"<>
			StringRiffle[ToString/@cut,"_"]<>"/";
		Run[MathematicaCommand<>" -script "<>packagePath<>"FFSolveIBP.wl "<>solveFolder];
	];
,
	jobNumber=Min[
		MathKernelLimit-1,
		ThreadUsedLimit,
		ConsistencyCheckParallelJobNumber
	];
	parallelCheckScript="";
	Switch[ConsistencyCheckParallelizationMethod,
	"Naive",
		For[i=1,i<=Length[spanningCuts],i++,
			cut=spanningCuts[[i]];
			solveFolder=outputPath<>"results/results_spanning_cuts/cut_"<>
				StringRiffle[ToString/@cut,"_"]<>"/";
			parallelCheckScript=parallelCheckScript<>
				MathematicaCommand<>" -script "<>packagePath<>"FFSolveIBP.wl "<>solveFolder<>" &\n";
			If[jobNumber=!=Infinity,
				If[Mod[i,jobNumber]===0,
					parallelCheckScript=parallelCheckScript<>"wait\n";
				];
			];
		];
		If[jobNumber===Infinity,
			parallelCheckScript=parallelCheckScript<>"wait\n";
		,
			If[Mod[Length[spanningCuts],jobNumber]=!=0,
				parallelCheckScript=parallelCheckScript<>"wait\n";
			]
		];
		Print[parallelCheckScript];
		Run[parallelCheckScript];
	,
	"GNUParallel",
		If[UseGNUParallel=!=True,
			PrintAndLog["**** Err: Must set UseGNUParallel=True if you want to use GNU parallel. Exit."];
			Exit[1];
		];
		For[i=1,i<=Length[spanningCuts],i++,
			cut=spanningCuts[[i]];
			solveFolder=outputPath<>"results/results_spanning_cuts/cut_"<>
				StringRiffle[ToString/@cut,"_"]<>"/";
			parallelCheckScript=parallelCheckScript<>"\n"<>
				MathematicaCommand<>" -script "<>packagePath<>"FFSolveIBP.wl "<>solveFolder<>"\n";
		];
		Export[outputPath<>"tmp/GNU_parallel_consistency_check_script.txt",parallelCheckScript,"Text"];
		If[jobNumber===Infinity,
			Run["cat "<>outputPath<>"tmp/GNU_parallel_consistency_check_script.txt | "<>GNUParallelCommand<>" --ungroup"<>"\n"];
		,
			Run["cat "<>outputPath<>"tmp/GNU_parallel_consistency_check_script.txt | "<>GNUParallelCommand<>" --ungroup -j "<>ToString[jobNumber]<>"\n"];
		];
		
	,
	_,
		PrintAndLog["**** Err: ConsistencyCheckParallelizationMethod must be one of the following: \"Naive\" or \"GNUParallel\". Exiting."];
		Exit[1];
	];		
]	


For[i=1,i<=Length[spanningCuts],i++,
	cut=spanningCuts[[i]];
	solveFolder=outputPath<>"results/results_spanning_cuts/cut_"<>
		StringRiffle[ToString/@cut,"_"]<>"/";
	cutRules[cut]=Get[solveFolder<>"rules.txt"];
	cutMIs[cut]=Get[solveFolder<>"results/MI_all.txt"];
]


allMIs=Union[Flatten[cutMIs/@spanningCuts]];


TargetReducedOnCut[target_,cut_]:=Module[{cutMIList,rules,targetReduced,MI,i,result},
	cutMIList=cutMIs[cut];
	rules=cutRules[cut];
	targetReduced=target/.rules;
	result={};
	For[i=1,i<=Length[allMIs],i++,
		MI=allMIs[[i]];
		If[
			Intersection[
				Sign/@((List@@MI)[[cut]]),
				{0,-1}
			]=!={}
		,
			result=Append[result,"cut"]
		,
			result=Append[result,D[targetReduced,MI]]
		]
	];
	result
]


timer=AbsoluteTime[];

PrintAndLog["\n=================================================="];
PrintAndLog["Reducing targets..."];
targets=Get[outputPath<>"inputs/"<>targetFileName];
Get[outputPath<>"inputs/"<>kinematicsFileName];

SDim=Length[Cases[Variables[{targets,allMIs}],_G][[1]]/.G->List];

targetReducedTable=Table[TargetReducedOnCut[targets[[i]],#]&/@spanningCuts,{i,Length[targets]}];
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];






timer=AbsoluteTime[]
PrintAndLog["Analyzing reduced table"];
consistencyChecks=Table[targetReducedTable[[i,All,k]],{i,Length[targets]},{k,Length[allMIs]}];
checkConclusions=Table[
	conclusion[
		targets[[i]],
		allMIs[[k]],
		consistencyChecks[[i,k]],
		spanningCuts[[Select[Range[Length[spanningCuts]],consistencyChecks[[i,k]][[#]]=!="cut"&]]],
		Length[Union[DeleteCases[consistencyChecks[[i,k]],"cut"]]]<=1
	],
	{i,Length[targets]},
	{k,Length[allMIs]}
]//Flatten
inconsistency=Select[checkConclusions,!(#[[5]])&];
inconsistencySummary=(inconsistency/.conclusion->List)[[All,{1,2,4}]];
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];





PrintAndLog["==========Check Report============"]
Export[outputPath<>"inconsistencySummary.txt",inconsistencySummary//InputForm//ToString];
If[inconsistencySummary==={},
	PrintAndLog["Consistency check passed."];
	(*create a check-pass certificate*)
	(*for SpanningCutsIBPShorten.wl *)
	(*only when it sees this certificate, it runs*)
	(*I use MD5 code to avoid the mistake by copying this certificate to some other path*)
	md5code=Hash[Import[checkPath<>"results/summary.txt","Text"],"MD5"];
	Export[checkPath<>"results/spanning_cuts_consistency_check_passed_certificate.txt",md5code];
,
	For[i=1,i<=Length[spanningCuts],i++,
		cut=spanningCuts[[i]];
		cutFolder=outputPath<>"results/results_spanning_cuts/cut_"<>
			StringRiffle[ToString/@cut,"_"]<>"/";
		Export[cutFolder<>"IBP_reduction_forbidden.tag","Spanning cuts consistency check **NOT** passed.","Text"];
	];
	displayLimit=6;
	PrintAndLog["Consistency check **NOT** passed. Inconsistencies:"];
	For[i=1,i<=Length[inconsistencySummary],i++,
		PrintAndLog[
			ToString[InputForm[inconsistencySummary[[i,1]]]]<>" reducing to "<>
			ToString[InputForm[inconsistencySummary[[i,2]]]]<>":"
		];
		mismatchCuts=inconsistencySummary[[i,3]];
		For[j=1,j<=Length[mismatchCuts],j++,
			PrintAndLog["\tcut "<>ToString[InputForm[mismatchCuts[[j]]]]]
		];
		
		If[i>=displayLimit,
			PrintAndLog[" ... and "<>ToString[Length[inconsistencySummary]-displayLimit]<>" more "];
			PrintAndLog["see "<>outputPath<>" inconsistencySummary.txt"];
			Break[]
		]
	]
]









