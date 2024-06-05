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


If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
Get[checkPath<>"inputs/config.txt"]
(*If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	Print["Output path has been set as "<>outputPath]
]*)
outputPath=checkPath


TemporaryDirectory=outputPath<>"tmp"

(*Get[packagePath<>"SyzygyRed.wl"]*)

Get[packagePath<>"SparseRREF/SparseRREF.m"]



SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse






Print["=================================================="];
Print["Checking at ",checkPath]
Print["-------------------------------------------------"]


targetFileName=FileNameSplit[targetIntegralsFile][[-1]]
kinematicsFileName=FileNameSplit[kinematicsFile][[-1]]


spanningCuts=Get[outputPath<>"tmp/spanningCuts.txt"]



For[i=1,i<=Length[spanningCuts],i++,
	cut=spanningCuts[[i]];
	solveFolder=outputPath<>"results/results_spanning_cuts/cut_"<>
		StringRiffle[ToString/@cut,"_"]<>"/";
	Run[MathematicaCommand<>" -script "<>packagePath<>"FFSolveIBP.wl "<>solveFolder];
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

Print["\n=================================================="];
Print["Reducing targets..."];
targets=Get[outputPath<>"inputs/"<>targetFileName];
Get[outputPath<>"inputs/"<>kinematicsFileName];

SDim=Length[Cases[Variables[{targets,allMIs}],_G][[1]]/.G->List];

targetReducedTable=Table[TargetReducedOnCut[targets[[i]],#]&/@spanningCuts,{i,Length[targets]}];
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];






timer=AbsoluteTime[]
Print["Analyzing reduced table"];
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
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];





Print["==========Check Report============"]
Export[outputPath<>"inconsistencySummary.txt",inconsistencySummary//InputForm//ToString];
If[inconsistencySummary==={},
	Print["Consistency check passed."];
	(*create a check-pass certificate*)
	(*for SpanningCutsIBPShorten.wl *)
	(*only when it sees this certificate, it runs*)
	(*I use MD5 code to avoid the mistake by copying this certificate to some other path*)
	md5code=Hash[Import[checkPath<>"results/summary.txt","Text"],"MD5"];
	Export[checkPath<>"results/spanning_cuts_consistency_check_passed_certificate.txt",md5code];
,
	displayLimit=6;
	Print["Consistency check **NOT** passed. Inconsistencies:"];
	For[i=1,i<=Length[inconsistencySummary],i++,
		Print[
			ToString[InputForm[inconsistencySummary[[i,1]]]]<>" reducing to "<>
			ToString[InputForm[inconsistencySummary[[i,2]]]]<>":"
		];
		mismatchCuts=inconsistencySummary[[i,3]];
		For[j=1,j<=Length[mismatchCuts],j++,
			Print["\tcut "<>ToString[InputForm[mismatchCuts[[j]]]]]
		];
		
		If[i>=displayLimit,
			Print[" ... and "<>ToString[Length[inconsistencySummary]-displayLimit]<>" more "];
			Print["see "<>outputPath<>" inconsistencySummary.txt"];
			Break[]
		]
	]
]









