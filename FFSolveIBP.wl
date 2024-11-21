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
LogFile=LogPath<>"FFSolveIBP.txt"


If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
Get[checkPath<>"inputs/config.txt"]
(*If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	PrintAndLog["Output path has been set as "<>outputPath]
]*)
outputPath=checkPath


If[CutIndices==="spanning cuts",
	(*PrintAndLog[
		"!!![Notice]: the config setting CutIndices=\"spanning cuts\" is an out-of-date gramma since v1.1.0.0.\n",
		"It is still supported, but it is recommended to use the equivalent, new gramma: \n",
		"\tCutIndices={};\n",
		"\tSpanningCutsMode=True;"
	];*)
	CutIndices={};
	SpanningCutsMode=True;
]


TemporaryDirectory=outputPath<>"tmp"

(*Get[packagePath<>"SyzygyRed.wl"]*)

Get[packagePath<>"SparseRREF/SparseRREF.m"]



SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse






PrintAndLog["=================================================="];
PrintAndLog["Solving IBPs on FF at ",checkPath]
PrintAndLog["-------------------------------------------------"]


targetFileName=FileNameSplit[targetIntegralsFile][[-1]]
kinematicsFileName=FileNameSplit[kinematicsFile][[-1]]


timer=AbsoluteTime[];
PrintAndLog["Reading Results..."];
MIs=Get[outputPath<>"results/MI_all.txt"];
IBPs=Get[outputPath<>"results/IBP_all.txt"];
integrals=Get[outputPath<>"results/OrderedIntegrals.txt"];
targets=Get[outputPath<>"inputs/"<>targetFileName];
Get[outputPath<>"inputs/"<>kinematicsFileName];

targets0=targets
targets=Complement[targets,MIs];


SDim=Length[Cases[Variables[IBPs],_G][[1]]/.G->List];
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];

vars=GenericPoint[[All,1]];

(*random check points*)
(*numerics=#->RandomPrime[50*Length[vars]^2]/RandomPrime[150*Length[vars]^2]&/@Join[vars,{d}];*)

numerics=Join[GenericPoint,GenericD]





timer=AbsoluteTime[];
PrintAndLog["Building Coefficient Matrix..."];
ca=CoefficientArrays[IBPs/.numerics,integrals]
If[Union[ArrayRules[ca[[1]]][[All,2]]]=!={0},PrintAndLog["IBP relations involve integrals not listed in OrderedIntegrals.txt!"];Exit[0]]
M=ca[[2]]
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]




timer=AbsoluteTime[];
PrintAndLog["RowReducing..."];
RM=SRSparseRowReduce[M,Modulus->FiniteFieldModulus]
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]






timer=AbsoluteTime[];
PrintAndLog["Analyzing reduction results (step 1/3)..."];
ar=Select[ArrayRules[RM],#[[1]]=!={_,_}&];
(*targetPositions=Flatten[Position[integrals,#]&/@targets]*)
entriesAndValues=GatherBy[ar,#[[1,1]]&](*un sorted*)
RowToRule[row_]:=Module[{columns,pivotColumn,rhsEntries,rule},
	columns=row[[All,1,2]];
	pivotColumn=Min@@columns;
	rhsEntries=Select[row,#[[1,2]]=!=pivotColumn&];
	rule=Rule[
		integrals[[pivotColumn]],
		Sum[-rhsEntries[[i,2]]*integrals[[rhsEntries[[i,1,2]]]],{i,Length[rhsEntries]}]
	];
	rule
]

(*IntegralsReducedAs[integral_]:=Module[{prePosition,position,row,rowEnrties},
	prePosition=Flatten[Position[integrals,integral]];
	If[Length[prePosition]=!=1,Return[$Failed]];
	position=Flatten[prePosition][[1]];
	row=Select[entries,#[[1,2]]===position&];
	rowEnrties=row[[1,2;;-1]];
	-integrals[[rowEnrties[[All,2]]]].((#/.ar)&/@rowEnrties)
]*)
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


timer=AbsoluteTime[];
PrintAndLog["Analyzing reduction results (step 2/3)..."];
(*LaunchKernels[];
rules=ParallelTable[RowToRule[entriesAndValues[[i]]],{i,Length[entriesAndValues]},Method->"FinestGrained"];
CloseKernels[]*)
rules=Table[RowToRule[entriesAndValues[[i]]],{i,Length[entriesAndValues]}]
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


timer=AbsoluteTime[];
PrintAndLog["Analyzing reduction results (step 3/3)..."];
(*sol=Select[rules,MemberQ[targets,#[[1]]]&]*)
sol=#->(#/.rules)&/@targets0;
(*sol2=Solve[RM.integrals==0,integrals//Reverse]
Export[checkPath<>"/sol2.txt",sol2//InputForm//ToString]
*)
Export[checkPath<>"/sol.txt",sol//InputForm//ToString]
Export[checkPath<>"/rules.txt",rules//InputForm//ToString]
Export[checkPath<>"/numerics.txt",numerics//InputForm//ToString]
Export[checkPath<>"/FiniteFieldModulus.txt",FiniteFieldModulus//InputForm//ToString]
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]
