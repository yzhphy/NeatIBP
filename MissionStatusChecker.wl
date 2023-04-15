(* ::Package:: *)

MemoryUsedLimit=Infinity
ThreadUsedLimit=Infinity
(*why?*)


commandLineMode=True


If[commandLineMode,
	packagePath=DirectoryName[$InputFileName];
	workingPath=Directory[]<>"/";
	missionInput=$CommandLine[[-1]];

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


reportClock=0
While[True,
	Pause[0.5];
	missionStatus=((ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex)->Get[#])&/@FileNames[All,missionStatusFolder];
	NonZeroSectors=missionStatus[[All,1]];
	If[Complement[Union[missionStatus[[All,2]]],{"ComputationFinished"(*,"Computing"*)}]==={},
		script="echo \"finished!\"\n";
		Break[];
	];
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
			SortBy[Select[missionStatus,#[[2]]==="Computing"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
	numOfNewComputingMissions=Min[
		Length[missionReadyToCompute],
		ThreadUsedLimit-Length[missionComputing],
		Max[MissionLimitByMemory[Length[missionReadyToCompute]+Length[missionComputing],MemoryUsedLimit]-Length[missionComputing],0]
		(*Max[MissionLimitByMemory[Length[missionReadyToCompute],Round[MemoryAvailable[]/(1024^2)]-MinMemoryReserved]-Length[missionComputing],0]*)
	];
	If[reportClock==0,
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
	If[numOfNewComputingMissions>0,
		newComputingSectors=missionReadyToCompute[[1;;numOfNewComputingMissions]];
		newComputingSectorNumbers=SectorNumber/@newComputingSectors;
		Export[missionStatusFolder<>ToString[#]<>".txt","Computing"//InputForm//ToString]&/@newComputingSectorNumbers;
		script=""<>
			StringRiffle["math -script "<>packagePath<>"Analyze_Sector.wl "<>missionInput<>" "<>ToString[#]<>" "<>outputPath<>" &\n"&/@newComputingSectorNumbers,""]<>
			"math -script "<>packagePath<>"AllMissionCompleteQ.wl "<>missionInput<>" | sh &\n"<>
			"wait\n";
			(*script=StringRiffle["math -script Analyze_Sector.wl "<>missionInput<>" "<>ToString[#]<>"\n"&/@newReadySectorNumbers,""];*)
		Break[];
	];
]


Print[script]
Run["echo \""<>script<>"\" >> "<>outputPath<>"tmp/log3.txt"]
