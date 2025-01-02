(* ::Package:: *)

MemoryUsedLimit=Infinity
ThreadUsedLimit=Infinity
(*why?*)


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
	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	missionInput="example.txt"
	
]





(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting."];Exit[]]
If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting."];Exit[]]
(*TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting."];Exit[]]*)


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


If[CutIndices==="spanning cuts",
	(*PrintAndLog[
		"!!![Notice]: the config setting CutIndices=\"spanning cuts\" is an out-of-date gramma since v1.1.0.0.\n",
		"It is still supported, but it is recommended to use the equivalent, new gramma: \n",
		"\tCutIndices={};\n",
		"\tSpanningCutsMode=True;"
	];*)(*not print in this wl*)
	CutIndices={};
	SpanningCutsMode=True;
]


If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";

]
If[Intersection[StringSplit[outputPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["Path "<>outputPath<>" is illegal. Exiting."];
	Exit[0];

]
If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


missionStatusFolder=outputPath<>"tmp/mission_status/"


SectorNumberToSectorIndex//ClearAll
SectorNumber//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse
SectorNumber[sec_]:=FromDigits[sec//Reverse,2];










SectorOrdering//ClearAll
SectorOrdering[sector_]:={Total[sector],SectorNumber[sector]}


(*Add1[sector_,position_]:=Module[{result=sector,i},
	For[i=1,i<=Length[position],i++,
		result[[position[[i]]]]=1;
	];
	result
]
SuperSectors[sector_]:=Module[{addingPositions,possibleSupersectors},
	addingPositions=DeleteCases[Subsets[Flatten[Position[sector,0]]],{}];
	possibleSupersectors=Add1[sector,#]&/@addingPositions;
	SortBy[Intersection[NonZeroSectors,possibleSupersectors],SectorOrdering]
]*)
superOrSourceSectors=Get[outputPath<>"tmp/superOrSourceSectors.txt"];
SuperSectors[sector_]:=sector/.superOrSourceSectors


(*To get rid of some abnormal large values in a list to make sure all values are within [0, abnormalBound*current mean value]*)
PoissonWinzorize[list_,abnormalBound_]:=Module[{oldList,newList},
	oldList=list;
	While[True,
		newList=Select[oldList,#<=abnormalBound*Mean[oldList]&];
		If[Length[newList]==Length[oldList],Break[];];
		oldList=newList;
	];
	newList
]

MissionLimitByMemory[number_,maxUsage_]:=Module[{MUH,AMUH},
	(*MUH:memory usage history*)
	If[!FileExistsQ[outputPath<>"tmp/log.txt"],Return[Infinity]];
	MUH=ToExpression[StringSplit[#," "][[-2]]]&/@StringSplit[Import[outputPath<>"tmp/log.txt"],"\n"];
	If[MUH==={},Return[Infinity]];
	MUH=PoissonWinzorize[MUH,3];(*The value "3" is put in by hand, can be modified*)
	If[Length[MUH]<number,MUH=Join[MUH,Table[Round[N[Total[MUH]/Length[MUH]]],number-Length[MUH]]]];
	AMUH=MUH//Sort//Reverse//Accumulate;
	Length[Select[AMUH,#<=maxUsage&]]
]


(*missionStatus=((ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex)->Get[#])&/@FileNames[All,missionStatusFolder]



missionReadyToCompute=(
			SortBy[Select[missionStatus,#[[2]]==="ReadyToCompute"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
MissionLimitByMemory[Length[missionReadyToCompute],Round[MemoryAvailable[]/(1024^2)]-MinMemoryReserved]*)


TimeString[]:=StringRiffle[#[[1;;3]],"."]<>" "<>StringRiffle[#[[4;;6]],":"]&[(ToString[Round[#]]&/@FromAbsoluteTime[AbsoluteTime[]][[1,1;;6]])]


ProcessRunningQ[commandLine_]:=Module[{ps},
	ps=Select[
		StringSplit[RunProcess[StringSplit["ps -ef"]]["StandardOutput"],"\n"],
		StringContainsQ[#,commandLine]&
	];
	ps=!={}
]





If[MathKernelLimit<Infinity&&IsASpanningCutsSubMission,
	WKFolder=outputPath<>"tmp/worker_kernels/";
	SubmittingKernelsFolder=WKFolder<>"submitting_kernels/";
	OccupiedKernelsFolder=WKFolder<>"occupied_kernels/";
	RecievedKernelsFolder=WKFolder<>"recieved_kernels/";
]



reportClock=0
While[True,
	Pause[0.5];
	missionStatus=(
		(
			ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex
		)->Get[#]
	)&/@FileNames[All,missionStatusFolder];
	NonZeroSectors=missionStatus[[All,1]];
	If[Complement[Union[missionStatus[[All,2]]],{"ComputationFinished"(*,"Computing"*)}]==={},
		script="echo \"finished!\"\n";
		Break[];
	];
	
	
	missionReportingFinished=(
		SortBy[
			Select[
				missionStatus,
				StringSplit[#[[2]],"\n"][[1]]==="ReportingFinished"&
			],
			SectorOrdering[#[[1]]]&
		]//Reverse
	)[[All,1]];
	While[True,
		If[missionReportingFinished==={},Break[]];
		actuallyFinishedMissions=Select[
			missionReportingFinished,
			Not[ProcessRunningQ[StringSplit[#/.missionStatus,"\n"][[2]]]]&
		];
		missionReportingFinished=Complement[missionReportingFinished,actuallyFinishedMissions];
		Export[
			missionStatusFolder<>ToString[#//SectorNumber]<>".txt",
			"ComputationFinished"//InputForm//ToString
		]&/@actuallyFinishedMissions;
		If[MathKernelLimit<Infinity&&IsASpanningCutsSubMission,
			occupiedKernels=FileNames[All,OccupiedKernelsFolder];
			If[Length[occupiedKernels]<Length[actuallyFinishedMissions],
				Print["echo \"MissionStatusChecker: ERROR20241112 at "<>outputPath<>"\""];
				Run["echo \""<>"MissionStatusChecker: ERROR20241112 at "<>outputPath<>"\" >> "<>outputPath<>"tmp/log3.txt"];
				Exit[1];
				(*I once met a case that we, suspeciously, reached here... at 2024.11.24*)
			];
			freeKernels=occupiedKernels[[;;Length[actuallyFinishedMissions]]];
			Run["mv "<>#<>" "<>SubmittingKernelsFolder]&/@freeKernels
		]
	];
	missionStatus=(
		(
			ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex
		)->Get[#]
	)&/@FileNames[All,missionStatusFolder];
	missionWaitingSupersectors=(
			SortBy[Select[missionStatus,#[[2]]==="WaitingSupersectors"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
	newReadySectors=Select[missionWaitingSupersectors,DeleteCases[Union[SuperSectors[#]/.missionStatus],"ComputationFinished"]==={}&];
	newReadySectorNumbers=SectorNumber/@newReadySectors;
	Export[missionStatusFolder<>ToString[#]<>".txt","ReadyToCompute"//InputForm//ToString]&/@newReadySectorNumbers;
	
(*	Run["echo \""<>
			TimeString[]<>"\t Mission status checked and possibly changed"<>
			"\" >> tmp/log4.txt"
	];*)
	
	missionReadyToCompute=(
			SortBy[Select[missionStatus,#[[2]]==="ReadyToCompute"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
	
	missionComputing=(
			SortBy[Select[missionStatus,#[[2]]==="Computing"||#[[2]]==="AboutToCompute"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
	numOfNewComputingMissions=Min[
		Length[missionReadyToCompute],
		ThreadUsedLimit-Length[missionComputing],
		MathKernelLimit-1-Length[missionComputing],
		Max[MissionLimitByMemory[Length[missionReadyToCompute]+Length[missionComputing],MemoryUsedLimit]-Length[missionComputing],0]
		(*Max[MissionLimitByMemory[Length[missionReadyToCompute],Round[MemoryAvailable[]/(1024^2)]-MinMemoryReserved]-Length[missionComputing],0]*)
	];
	
	If[reportClock==0&&(DebugMode===True),
		Run["echo \""<>
			TimeString[]<>"\t"<>
			ToString[InputForm[{
				Length[missionReadyToCompute],
				ThreadUsedLimit-Length[missionComputing],
				MissionLimitByMemory[Length[missionReadyToCompute]+Length[missionComputing],MemoryUsedLimit]-Length[missionComputing]
				(*Max[MissionLimitByMemory[Length[missionReadyToCompute],Round[MemoryAvailable[]/(1024^2)]-MinMemoryReserved]-Length[missionComputing],0]*)
			}]]<>
			"\" >> "<>outputPath<>"tmp/log4.txt"
		]
	];
	reportClock=Mod[reportClock+1,1000];
	
	If[MathKernelLimit<Infinity&&IsASpanningCutsSubMission,
		recievedKernels=FileNames[All,RecievedKernelsFolder];
		
		If[Length[recievedKernels]<numOfNewComputingMissions,
			requestedKernels=numOfNewComputingMissions-Length[recievedKernels];
			newOccupiedKernels=recievedKernels;
			numOfNewComputingMissions=Length[recievedKernels];
		,
			requestedKernels=0;
			newOccupiedKernels=recievedKernels[[;;numOfNewComputingMissions]];
		];
		uselessKernels=Complement[recievedKernels,newOccupiedKernels];
		Run["mv "<>#<>" "<>OccupiedKernelsFolder]&/@newOccupiedKernels;
		Run["mv "<>#<>" "<>SubmittingKernelsFolder]&/@uselessKernels;
		Export[
			WKFolder<>"requested_kernels.txt",
			ToString[requestedKernels]
		];
	];
	scriptModifyStatus="";
	If[numOfNewComputingMissions>0,
		newComputingSectors=missionReadyToCompute[[1;;numOfNewComputingMissions]];
		newComputingSectorNumbers=SectorNumber/@newComputingSectors;
		Export[missionStatusFolder<>ToString[#]<>".txt","AboutToCompute"//InputForm//ToString]&/@newComputingSectorNumbers;
		scriptModifyStatus=StringRiffle[
			"echo \\\"Computing\\\" > "<>missionStatusFolder<>ToString[#]<>".txt"&/@newComputingSectorNumbers,
			"\n"
		];
		script=""<>
			StringRiffle[MathematicaCommand<>" -script "<>packagePath<>"Analyze_Sector.wl "<>AbsMissionInput<>" "<>ToString[#]<>" "<>outputPath<>" &\n"&/@newComputingSectorNumbers,""]<>
			MathematicaCommand<>" -script "<>packagePath<>"AllMissionCompleteQ.wl "<>AbsMissionInput<>" | "<>ShellProcessor<>" &\n"<>
			"wait\n";
			(*script=StringRiffle[MathematicaCommand<>" -script Analyze_Sector.wl "<>missionInput<>" "<>ToString[#]<>"\n"&/@newReadySectorNumbers,""];*)
		Break[];
	];
]




script="sleep 1\n"<>scriptModifyStatus<>"\n"<>script<>"\n" (*sleep: to makesure that when script runs, this wl really ends.*)
Run["echo \""<>script<>"\" >> "<>outputPath<>"tmp/log3.txt"]
Print[script]
