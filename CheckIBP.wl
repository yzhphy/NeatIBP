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
	checkPath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/Examples_in_the_paper/2l4p_top/lxb3/outputs/lxb3"
	
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


timer=AbsoluteTime[];
Print["Reading Results..."];
MIs=Get[outputPath<>"results/MI_all.txt"];
IBPs=Get[outputPath<>"results/IBP_all.txt"];
integrals=Get[outputPath<>"results/OrderedIntegrals.txt"];
targets=Get[outputPath<>"inputs/"<>targetFileName];
Get[outputPath<>"inputs/"<>kinematicsFileName];

targets=Complement[targets,MIs];

SDim=Length[Cases[Variables[IBPs],_G][[1]]/.G->List];
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];

vars=GenericPoint[[All,1]];

(*random check points*)
numerics=#->RandomPrime[50*Length[vars]^2]/RandomPrime[150*Length[vars]^2]&/@Join[vars,{d}];







timer=AbsoluteTime[];
Print["Building Coefficient Matrix..."];
ca=CoefficientArrays[IBPs/.numerics,integrals]
If[Union[ArrayRules[ca[[1]]][[All,2]]]=!={0},Print["IBP relations involve integrals not listed in OrderedIntegrals.txt!"];Exit[0]]
M=ca[[2]]
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]




timer=AbsoluteTime[];
Print["RowReducing..."];
RM=SRSparseRowReduce[M,Modulus->FiniteFieldModulus]
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


timer=AbsoluteTime[];
Print["Analyzing reduction results (step 1/3)..."];

(*targetPositions=Flatten[Position[integrals,#]&/@targets]*)
entries=SortBy[Sort/@GatherBy[Select[ArrayRules[RM][[All,1]],#=!={_,_}&],#[[1]]&],#[[1]]&]
IntegralsReducedTowards[integral_]:=Module[{prePosition,position,row},
	prePosition=Flatten[Position[integrals,integral]];
	If[Length[prePosition]=!=1,Return[$Failed]];
	position=Flatten[prePosition][[1]];
	row=Select[entries,#[[1,2]]===position&];
	integrals[[row[[1,2;;-1,2]]]]
]
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


timer=AbsoluteTime[];
Print["Analyzing reduction results (step 2/3)..."];
LaunchKernels[]
targetTowards=ParallelTable[IntegralsReducedTowards[targets[[i]]],{i,Length[targets]},Method->"FinestGrained"];
CloseKernels[]
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


timer=AbsoluteTime[];
Print["Analyzing reduction results (step 3/3)..."];
strangeIntegrals=Complement[Union[Flatten[targetTowards]],MIs]

Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


Print["==========Check Report============"]
Print["check point: "<>ToString[numerics//InputForm]];
If[strangeIntegrals==={},
	Print["All targets are reduced to MIs."]
,
	Print["Targets are NOT reduced to MIs. "<>ToString[Length[strangeIntegrals]]<>" ADDITIONAL integrals appearead in the results. They are:"];
	If[Length[strangeIntegrals]<=10,
		Print[strangeIntegrals//InputForm//ToString]
	,
		Print[StringReplace[strangeIntegrals[[1;;10]]//InputForm//ToString,"}"->",...}"]]
	]
]






