(* ::Package:: *)

commandLineMode=True


If[commandLineMode,
	packagePath=DirectoryName[$InputFileName];
	AbsMissionInput=$CommandLine[[-1]];
	workingPath=DirectoryName[AbsMissionInput];
	missionInput=FileNameSplit[AbsMissionInput][[-1]];
	(*workingPath=Directory[]<>"/";
	missionInput=$CommandLine[[-1]];*)
	MathematicaCommand=Import[packagePath<>"/preload/MathematicaCommand.txt"];
	ShellProcessor=Import[packagePath<>"/preload/ShellProcessor.txt"];
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
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
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
StringJoinLined[a_,b_]:=If[a==="",b,a<>"\n"<>b]
oldRunningMissionMismatchMessage="";
PrintStatus[]:=Module[
{maxNum=6,missionWaitingSupersectors,missionComputationFinished,missionComputing,missionReadyToCompute,
missionLost,runningMissionUnregistered,actuallyRunningMissions,runningMissionMismatchMessage},
	actuallyRunningMissions=ActuallyRunningMissions[];
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
	
	missionLost=Complement[SectorNumber/@missionComputing,actuallyRunningMissions];
	runningMissionUnregistered=Complement[actuallyRunningMissions,SectorNumber/@missionComputing];
	runningMissionMismatchMessage="";
	If[Length[missionLost]>0,
		runningMissionMismatchMessage=StringJoinLined[
			runningMissionMismatchMessage,
			"******** \nWarning:\n"<>ToString[Length[missionLost]]<>" sector(s) lost"<>ReprotString[missionLost,maxNum]
		];
		runningMissionMismatchMessage=StringJoinLined[
			runningMissionMismatchMessage,
			"The corresponding process(es) cannot be detected. Maybe they terminated unexpectedly."
		];
	];
	If[Length[runningMissionUnregistered]>0,
		runningMissionMismatchMessage=StringJoinLined[
			runningMissionMismatchMessage,
			"******** \nError:\n"<>ToString[Length[runningMissionUnregistered]]<>" computing sector(s) unregistered"<>ReprotString[runningMissionUnregistered,maxNum]
		];
		runningMissionMismatchMessage=StringJoinLined[
			runningMissionMismatchMessage,
			"This error is unexpected. Please make sure you are not running 2 NeatIBP with the same outputPath."
		];
	];
	If[And[runningMissionMismatchMessage===oldRunningMissionMismatchMessage,runningMissionMismatchMessage=!=""],Print[runningMissionMismatchMessage]];
	oldRunningMissionMismatchMessage=runningMissionMismatchMessage;
]
PrintWaitInitialization[]:=Module[{},
	Print["----------------------------------------------"];
	Print[TimeString[]];
	Print["Waiting initialization..."]
]


(*ActuallyRunningMissions[]:=Module[{ps,ASps,currentMissions},
	ps=Select[StringSplit[RunProcess[StringSplit["ps -ef"]]["StandardOutput"],"\n"],StringContainsQ[#,"Analyze_Sector.wl"]&];
	ASps=StringSplit[#," "][[-2;;-1]]&/@ps;
	currentMissions=Select[ASps,#[[2]]===outputPath&];
	ToExpression/@(currentMissions[[All,1]])
	(*Maybe the user runs 2 different diagrams at a time... *)
]*)
ActuallyRunningMissions[path_]:=Module[{ps,ASps,currentMissions},
	ps=Select[StringSplit[RunProcess[StringSplit["ps -ef"]]["StandardOutput"],"\n"],StringContainsQ[#,"Analyze_Sector.wl"]&];
	ASps=StringSplit[#," "][[-2;;-1]]&/@ps;
	currentMissions=Select[ASps,#[[2]]===path&];
	ToExpression/@(currentMissions[[All,1]])
	(*Maybe the user runs 2 different diagrams at a time... *)
]
ActuallyRunningMissions[]:=ActuallyRunningMissions[outputPath]





(* ::Section:: *)
(*Spanning Cuts monitor*)


MissionStatusInfo[path_]:=Module[
{missionStatusPath,missionStatus,missionWaitingSupersectors,missionReadyToCompute,
missionComputing,missionComputationFinished,actuallyRunningMissions,missionLost,
runningMissionUnregistered},
	actuallyRunningMissions=ActuallyRunningMissions[path];
	missionStatusPath=path<>"/tmp/mission_status/";
	missionStatus={ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex,Get[#]}&/@FileNames[All,missionStatusPath];
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
	missionLost=Complement[SectorNumber/@missionComputing,actuallyRunningMissions];
	runningMissionUnregistered=Complement[actuallyRunningMissions,SectorNumber/@missionComputing];
	
	MapThread[#1->#2&,{
		{
			"waiting",
			"computable",
			"computing",
			"finished",
			"*lost",
			"*unlabelled"
		},
		Length/@{
			missionWaitingSupersectors,
			missionReadyToCompute,
			missionComputing,
			missionComputationFinished,
			missionLost,
			runningMissionUnregistered
		}
	}]
	
]



CutMissionInfo[cut_]:=Module[{cutString,path,info},
	cutString=StringRiffle[ToString/@cut,"_"];
	path=outputPath<>"tmp/spanning_cuts_missions/cut_"<>cutString<>"/outputs/"<>ReductionOutputName<>"/";
	info=MissionStatusInfo[path];
	Join[{"cut"->cut},info]
]
CutMissionInfoExtended[cut_]:=Module[{cutString,path,info,status,progress,totalSectors},
	cutString=StringRiffle[ToString/@cut,"_"];
	path=outputPath<>"tmp/spanning_cuts_missions/cut_"<>cutString<>"/outputs/"<>ReductionOutputName<>"/";
	info=MissionStatusInfo[path];
	Switch[#===0&/@({"*lost","*unlabelled"}/.info),
	{True,True},
		Switch[#===0&/@({
			"waiting",
			"computable",
			"computing",
			"finished"
		}/.info),
		{True,True,True,True},
			status="not started";
		,
		{True,True,True,False},
			status="finished";
		,
		_,
			status="in progress";
		];
	,
	{True,False},
		status="**sector unlabelled";
	,
	{False,True},
		status="**sector lost";
	,
	{False,False},
		status="**sector lost/unlabbelled"
	];
	If[status==="not started",
		progress="-";
	,
		totalSectors=Total[info[[All,2]]];
		progress=Round[1000*("finished"/.info)/totalSectors]*0.1;
		progress=ToString[progress]<>"%"
	];
	Join[{"cut"->cut,"status"->status,"progress"->progress},info]
]


CutStringToCutIndex[cutString_]:=ToExpression/@StringSplit[StringReplace[cutString,"cut_"->""],"_"]


ReadCuts[]:=Module[{dirs,cutStrings},
	(*dirs=Select[FileNames[All,outputName<>"/tmp/spanning_cuts_missions/"],DirectoryQ];*)
	dirs=Select[FileNames[All,outputPath<>"/tmp/spanning_cuts_missions/"],DirectoryQ];
	cutStrings=FileNameSplit[#][[-1]]&/@dirs;
	CutStringToCutIndex/@cutStrings
]


CutMissionsInfoTable[]:=Module[{cuts,infos,heads},
	cuts=ReadCuts[];
	infos=CutMissionInfo/@cuts;
	heads={
		"cut",
		"waiting",
		"computable",
		"computing",
		"finished",
		"*lost",
		"*unlabelled"
	};
	(ToString/@(heads/.#))&/@Join[{{}},infos]
]
CutMissionsInfoTableExtended[]:=Module[{cuts,infos,heads},
	cuts=ReadCuts[];
	infos=CutMissionInfoExtended/@cuts;
	heads={
		"cut",
		"status",
		"progress",
		"waiting",
		"computable",
		"computing",
		"finished",
		"*lost",
		"*unlabelled"
	};
	
	(ToString/@(heads/.#))&/@Join[{{}},infos]
]


CompensateSpace[string_,length_]:=string<>StringRiffle[Table[" ",length],""]





FineTable[table_]:=Module[{rows,columns,r,c,maxLengths,fineTable,entry,finalString},
	fineTable=table;
	{rows,columns}=Dimensions[table];
	maxLengths=(Max@@(StringLength/@#))&/@Transpose[table];(*max string length of each column*)
	
	For[r=1,r<=rows,r++,
		For[c=1,c<=columns,c++,
		
			entry=table[[r,c]];
			fineTable[[r,c]]=CompensateSpace[entry,maxLengths[[c]]-StringLength[entry]]
		];
	];
	finalString=StringRiffle[StringRiffle[#,"\t"]&/@fineTable,"\n"];
	finalString
]


PrintCutsMissions[]:=Module[{displaystring},
	displaystring="----------------------------------------------";
	displaystring=displaystring<>"\n"<>TimeString[]<>"\n"<>displaystring;
	(*displaystring=displaystring<>"\n"<>FineTable[CutMissionsInfoTable[]];*)
	displaystring=displaystring<>"\n"<>FineTable[CutMissionsInfoTableExtended[]];
	Print[displaystring]
]


CutFinishedQ[cut_]:=Module[{info},
	info=CutMissionInfo[cut];
	
	And[
		({
			"waiting",
			"computable",
			"computing",
			"*lost",
			"*unlabelled"
		}/.info)===((*ToString/@*){0,0,0,0,0}),
		ToExpression["finished"/.info]>0
	]
]


AllCutsFinishedQ[]:=Module[{cuts},
	cuts=ReadCuts[];
	
	And@@(CutFinishedQ/@cuts)
]


(* ::Section:: *)
(*Run monitor*)


mode="normal"
While[True,
	If[FileExistsQ[outputPath<>"tmp/spanning_cuts_mode.txt"]&&FileExistsQ[outputPath<>"tmp/run_all_cuts.sh"]&&mode=="normal",
		mode="spanning cuts";
		Print["----------------------------------------------"];
		Print[TimeString[]];
		Print["Entering spanning cuts mode"];
		Pause[3]
	];
	If[mode==="normal",
		If[!DirectoryQ[outputPath//ToString],Print["outputPath "<>ToString[outputPath]<>" does not exist."];Break[]];
		missionStatus={ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex,Get[#]}&/@FileNames[All,missionStatusFolder];
		initializationStatus=InitializationStatus[];
		If[initializationStatus==="finished",PrintStatus[]];
		If[initializationStatus==="in progress",PrintWaitInitialization[]];
		If[initializationStatus==="failed",Print["==============================================\nInitialization Failed."];Break[]];
		If[DeleteCases[missionStatus[[All,2]],"ComputationFinished"]==={}&&missionStatus=!={}&&initializationStatus==="finished",Print["==============================================\nAll sectors finished."];Break[]];
		Pause[1]
	];
	If[mode==="spanning cuts",
		
		If[!DirectoryQ[outputPath//ToString],Print["outputPath "<>ToString[outputPath]<>" does not exist."];Break[]];
		initializationStatus=InitializationStatus[];
		terminateQSpanningCutsMode=False;
		If[initializationStatus==="finished",
			If[AllCutsFinishedQ[],terminateQSpanningCutsMode=True];
			PrintCutsMissions[];
		];
		If[initializationStatus==="in progress",PrintWaitInitialization[]];
		If[initializationStatus==="failed",Print["==============================================\nInitialization Failed."];Break[]];
		(*If[AllCutsFinishedQ[]&&initializationStatus==="finished",Print["==============================================\nAll cuts finished."];Break[]];
		*)
		If[terminateQSpanningCutsMode,Print["==============================================\nAll cuts finished."];Break[]];
		Pause[1]
	];
	
]












