(* ::Package:: *)

OptionSimplification=12;


commandLineMode=True


If[commandLineMode,
	workingPath=DirectoryName[$InputFileName];
	missionInput=$CommandLine[[-1]];
(*	AppendTo[$Path,workingPath];
	Get[missionInput]*)
	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	missionInput="example.txt"
	(*LoopMomenta={l1,l2};
	ExternalMomenta={k1,k2,k4};
	Propagators={l1^2,(l1-k1)^2,(l1-k1-k2)^2,(l2+k1+k2)^2,(l2-k4)^2,l2^2,(l1+l2)^2,(l1+k4)^2,(l2+k1)^2};
	Kinematics={k1^2->0,k2^2->0,k4^2->0,k1 k2->s/2,k1 k4->t/2,k2 k4->(-s/2-t/2)};
	GenericPoint={s->-1,t->-3}; 
	TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]}*)
	
]


AppendTo[$Path,workingPath];
If[Get[missionInput]===$Failed,Print["Unable to open "<>missionInput<>". Exiting.";Exit[]]]


outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/"


TemporaryDirectory = outputPath<>"tmp/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]
SingularDirectory = "/usr/bin/Singular"
Get[SyzygyRedPackageFile]


(*IntegralOrder="Global";
Prepare[];*)


missionStatusFolder=TemporaryDirectory<>"mission_status/"


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse


TimeString[]:=StringRiffle[#[[1;;3]],"."]<>" "<>StringRiffle[#[[4;;6]],":"]&[(ToString[Round[#]]&/@FromAbsoluteTime[AbsoluteTime[]][[1,1;;6]])]
ReprotString[list_,maxNum_]:=If[list==={},"",": "]<>If[Length[list]>maxNum,StringRiffle[ToString/@(list[[1;;maxNum]]),","]<>"...",StringRiffle[ToString/@(list),","]<>"."]
PrintStatus[]:=Module[{maxNum=6,missionWaitingSupersectors,missionComputationFinished,missionComputing,missionReadyToCompute},
	Print["----------------------------------------------"];
	Print[TimeString[]];
	missionWaitingSupersectors=(
			SortBy[Select[missionStatus,#[[2]]==="WaitingSupersectors"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
	missionReadyToCompute=(
			SortBy[Select[missionStatus,#[[2]]==="ReadyToCompute"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
	missionComputing=(
			SortBy[Select[missionStatus,#[[2]]==="Computing"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
	missionComputationFinished=(
			SortBy[Select[missionStatus,#[[2]]==="ComputationFinished"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
	
	Print[Length[missionWaitingSupersectors]," sector(s) waiting super sector(s)",ReprotString[SectorNumber/@missionWaitingSupersectors,maxNum]];
	Print[Length[missionReadyToCompute]," sector(s) ready to compute",ReprotString[SectorNumber/@missionReadyToCompute,maxNum]];
	Print[Length[missionComputing]," sector(s) computing",ReprotString[SectorNumber/@missionComputing,maxNum]];
	Print[Length[missionComputationFinished]," sector(s) finished",ReprotString[SectorNumber/@missionComputationFinished,maxNum]];
]


ActuallyRunningMissions[]:=Module[{ps},
	ps=Select[StringSplit[RunProcess[StringSplit["ps -ef"]]["StandardOutput"],"\n"],StringContainsQ[#,"Analyze_Sector.wl"]&];
	ToExpression[StringSplit[#," "][[-1]]]&/@ps
	(*But what if the user runs 2 different diagrams at a time?*)
]


While[True,
	missionStatus={ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex,Get[#]}&/@FileNames[All,missionStatusFolder];
	PrintStatus[];
	If[DeleteCases[missionStatus[[All,2]],"ComputationFinished"]==={}&&missionStatus=!={},Print["==============================================\nAll sectors finished."];Break[]];
	Pause[1]
]












