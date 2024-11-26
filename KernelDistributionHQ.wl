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
	packagePath=NotebookDirectory[];
	AbsMissionInput="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/dbox/config.txt";
	workingPath=DirectoryName[AbsMissionInput];
	missionInput=FileNameSplit[AbsMissionInput][[-1]];
	(*workingPath=Directory[]<>"/";
	missionInput=$CommandLine[[-1]];*)
	MathematicaCommand=Import[packagePath<>"/preload/MathematicaCommand.txt"];
	ShellProcessor=Import[packagePath<>"/preload/ShellProcessor.txt"];
	
]





(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting."];Exit[]]
If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting."];Exit[]]
(*TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting."];Exit[]]*)


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


spanningCuts=Get[outputPath<>"tmp/spanningCuts.txt"];


CutFolder[cut_]:=outputPath<>"tmp/spanning_cuts_missions/cut_"<>StringRiffle[ToString/@cut,"_"]<>"/"


StartCut[cut_]:=DeleteFile[CutFolder[cut]<>"pause.tag"]


CutOutputFolder[cut_]:=CutFolder[cut]<>"outputs/"<>ReductionOutputName<>"/"


finishedTagFile="results/NeatIBP_finished.tag"
CutFinishedQ[cut_]:=FileExistsQ[CutOutputFolder[cut]<>finishedTagFile]


CutWKFolder[cut_]:=CutOutputFolder[cut]<>"tmp/worker_kernels/"


HQFolder=outputPath<>"tmp/kernels_at_HQ/"
If[!DirectoryQ[#],CreateDirectory[#]]&[HQFolder]
If[!DirectoryQ[#],CreateDirectory[#]]&[HQFolder<>"vacant_kernels/"]
If[!DirectoryQ[#],CreateDirectory[#]]&[HQFolder<>"manager_kernels/"]



ClearFolderFiles[folder_]:=Module[{files},
	If[DirectoryQ[folder],
		files=FileNames[All,folder];
		files=Select[files,!DirectoryQ[#]&];
		DeleteFile/@files;
	]
]


tmpPath=outputPath<>"tmp/"
spcBehavior=Import[tmpPath<>"spanning_cuts_behaviour.tag","Text"];
If[!MemberQ[{"run","continue"},spcBehavior],
	PrintAndLog["Unidentified spcBehavior: "<>spcBehavior<>". Exiting."];
	Exit[];
	(*this is actually unexpected because if so, AllMissionCompleteQ.wl will report this bug earlier and we would not enter this wl file*)
]


If[spcBehavior==="continue",
	ClearFolderFiles[HQFolder];
	ClearFolderFiles[HQFolder<>"vacant_kernels/"];
	ClearFolderFiles[HQFolder<>"manager_kernels/"];
	Module[{cut,i},
		For[i=1,i<=Length[spanningCuts],i++,
			cut=spanningCuts[[i]];
			ClearFolderFiles[CutWKFolder[cut]];
			ClearFolderFiles[CutWKFolder[cut]<>"recieved_kernels/"];
			ClearFolderFiles[CutWKFolder[cut]<>"occupied_kernels/"];
			ClearFolderFiles[CutWKFolder[cut]<>"submitting_kernels/"];			
		]
	]
]


Export[HQFolder<>"vacant_kernels/kernel_"<>ToString[#],"","Text"]&/@Range[MathKernelLimit-1](*HQ kernel is ommited*)


CollectCutSubmittingKernels[cut_]:=Module[{cutSubmittingKernelsFolder,submittingKernels},
	cutSubmittingKernelsFolder=CutWKFolder[cut]<>"submitting_kernels/";
	If[!DirectoryQ[cutSubmittingKernelsFolder],Return[](*no submitting kernel in this cut, do nothing*)];
	submittingKernels=FileNames[All,cutSubmittingKernelsFolder];
	Run["mv "<>#<>" "<>HQFolder<>"vacant_kernels/"]&/@submittingKernels
]
CollectAllSubmittingKernels[]:=CollectCutSubmittingKernels/@spanningCuts


CountCutRecievedKernels[cut_]:=Module[{cutRecievedKernelsFolder,recievedKernels},
	cutRecievedKernelsFolder=CutWKFolder[cut]<>"recieved_kernels/";
	If[!DirectoryQ[cutRecievedKernelsFolder],Return[0](*no occupied kernel in this cut, return 0*)];
	recievedKernels=FileNames[All,cutRecievedKernelsFolder];
	recievedKernels//Length
]
HQVacantKernels[]:=FileNames[All,HQFolder<>"vacant_kernels/"]
HQManagerKernels[]:=FileNames[All,HQFolder<>"manager_kernels/"]


ReadCutRequestedKernels[cut_]:=Module[{cutRequestedKernelsFile,occupiedKernels},
	cutRequestedKernelsFile=CutWKFolder[cut]<>"requested_kernels.txt";
	(*Print[cutRequestedKernelsFile];*)
	If[!FileExistsQ[cutRequestedKernelsFile],Return[0]];
	Get[cutRequestedKernelsFile]
]


Module[
{CutMissionStatus,cut,i,status,vacantKernelsHQ,managerKernelsHQ,requestedKernels,grantingKernels},
	CutMissionStatus=Table["NotStarted",Length[spanningCuts]];
	While[True,
		CollectAllSubmittingKernels[];
		For[i=1,i<=Length[spanningCuts],i++,
			cut=spanningCuts[[i]];
			status=CutMissionStatus[[i]];
			
			Switch[status,
			"NotStarted",
				vacantKernelsHQ=HQVacantKernels[];
				(*Print["vacantKernelsHQ: ", vacantKernelsHQ];
				Print["vacantKernelsHQ: ", vacantKernelsHQ];*)
				If[Count[CutMissionStatus,"Running"]<MaxManagers&&Length[vacantKernelsHQ]>1,
					CutMissionStatus[[i]]="Running";
					Run["mv "<>vacantKernelsHQ[[1]]<>" "<>HQFolder<>"manager_kernels/"];
					(*actually we do not care about which manager kernel is for which cut
					we only care about their total number
					*)
					StartCut[cut];
				]
			,
			"Running",
				If[CutFinishedQ[cut],
					CutMissionStatus[[i]]="Finished";
					managerKernelsHQ=HQManagerKernels[];
					If[Length[managerKernelsHQ]<1,
						While[True,
							Print["*******KernelDistributionHQ: Unexpected Error. Please terminate NeatIBP Manually."];
							Pause[0.1]
						]
					];
					Run["mv "<>managerKernelsHQ[[1]]<>" "<>HQFolder<>"vacant_kernels/"];
				,
					(*if not finished*)
					requestedKernels=ReadCutRequestedKernels[cut]-CountCutRecievedKernels[cut];
					(*Print["cut: ", cut];
					Print["requestedKernels:",requestedKernels];*)
					vacantKernelsHQ=HQVacantKernels[];
					If[Length[vacantKernelsHQ]>=requestedKernels,
						grantingKernels=vacantKernelsHQ[[;;requestedKernels]];
					,
						grantingKernels=vacantKernelsHQ
					];
					Run["mv "<>#<>" "<>CutWKFolder[cut]<>"recieved_kernels/"]&/@grantingKernels;
					
				
				]
			]
		];
		If[DeleteCases[CutMissionStatus,"Finished"]==={},Break[]];
		Pause[0.3]
	]
	
	
]



