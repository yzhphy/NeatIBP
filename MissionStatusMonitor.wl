(* ::Package:: *)

commandLineMode=True


If[commandLineMode,
	packagePath=DirectoryName[$InputFileName];
	workingPath=Directory[]<>"/";
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


(*AppendTo[$Path,workingPath];*)
If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting.";Exit[]]]
If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting.";Exit[]]]
(*TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting.";Exit[]]]*)


If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	Print["Output path has been set as "<>outputPath]
]
If[Intersection[StringSplit[outputPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["Path "<>outputPath<>" is illegal. Exiting."];
	Exit[0];

]
If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


tmpPath=outputPath<>"tmp/"


TemporaryDirectory = outputPath<>"tmp/"
(*If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]*)
Get[packagePath<>"SyzygyRed.wl"]





missionStatusFolder=TemporaryDirectory<>"mission_status/"


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse


InitializationStatus[]:=If[FileExistsQ[tmpPath<>"initialization_failed.txt"],
	"failed",
	If[FileExistsQ[tmpPath<>"initialized.txt"],
		"finished"
	,
		"in progress"
	]
]


TimeString[]:=StringRiffle[#[[1;;3]],"."]<>" "<>StringRiffle[#[[4;;6]],":"]&[(ToString[Floor[#]]&/@FromAbsoluteTime[AbsoluteTime[]][[1,1;;6]])]
ReprotString[list_,maxNum_]:=If[list==={},"",": "]<>If[Length[list]>maxNum,StringRiffle[ToString/@(list[[1;;maxNum]]),","]<>"...",StringRiffle[ToString/@(list),","]<>"."]
PrintStatus[]:=Module[
{maxNum=6,missionWaitingSupersectors,missionComputationFinished,missionComputing,missionReadyToCompute,
missionLost,runningMissionUnregistered,actuallyRunningMissions},
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
	actuallyRunningMissions=ActuallyRunningMissions[];
	missionLost=Complement[SectorNumber/@missionComputing,actuallyRunningMissions];
	runningMissionUnregistered=Complement[actuallyRunningMissions,SectorNumber/@missionComputing];
	If[Length[missionLost]>0,
		Print["******** \nWarning:\n",Length[missionLost],"sector(s) lost"<>ReprotString[missionLost,maxNum]];
		Print["The corresponding process(es) lost. Maybe they terminated unexpectedly."]
	];
	If[Length[runningMissionUnregistered]>0,
		Print["******** \nError:\n",Length[runningMissionUnregistered],"computing sector(s) unregistered"<>ReprotString[runningMissionUnregistered,maxNum]];
		Print["This error is unexpected. Please make sure you are not running 2 NeatIBP with the same outputPath."]
	]
]
PrintWaitInitialization[]:=Module[{},
	Print["----------------------------------------------"];
	Print[TimeString[]];
	Print["Waiting initialization..."]
]


ActuallyRunningMissions[]:=Module[{ps,ASps,currentMissions},
	ps=Select[StringSplit[RunProcess[StringSplit["ps -ef"]]["StandardOutput"],"\n"],StringContainsQ[#,"Analyze_Sector.wl"]&];
	ASps=StringSplit[#," "][[-2;;-1]]&/@ps;
	currentMissions=Select[ASps,#[[2]]===outputPath&];
	ToExpression/@(currentMissions[[All,1]])
	(*Maybe the user runs 2 different diagrams at a time... *)
]


ActuallyRunningMissions[]


While[True,
	If[!DirectoryQ[outputPath//ToString],Print["outputPath "<>ToString[outputPath]<>" does not exist."];Break[]];
	missionStatus={ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex,Get[#]}&/@FileNames[All,missionStatusFolder];
	initializationStatus=InitializationStatus[];
	If[initializationStatus==="finished",PrintStatus[]];
	If[initializationStatus==="in progress",PrintWaitInitialization[]];
	If[initializationStatus==="failed",Print["==============================================\nInitialization Failed."];Break[]];
	If[DeleteCases[missionStatus[[All,2]],"ComputationFinished"]==={}&&missionStatus=!={}&&initializationStatus==="finished",Print["==============================================\nAll sectors finished."];Break[]];
	Pause[1]
]












