(* ::Package:: *)

(*moduleIntersectionMethod="Linear"
If[moduleIntersectionMethod==="Linear",
	InputString["===========Warning===================\n moduleIntersectionMethod is Linear! input anything to continue."];
	Get["/home/zihao/projects/SyzygyRed/LinearSyz_release/LinearSyz.wl"]
]*)


commandLineMode=True


If[commandLineMode,
	
	(*workingPath=Directory[]<>"/";
	missionInput=$CommandLine[[-3]];*)
	
	
	packagePath=DirectoryName[$InputFileName];
	AbsMissionInput=$CommandLine[[-3]];
	workingPath=DirectoryName[AbsMissionInput];
	missionInput=FileNameSplit[AbsMissionInput][[-1]];
	
	
	sectorID=$CommandLine[[-2]]//ToExpression;
	(*outputPath=$CommandLine[[-1]];*)(*This is for MissionStatusMonitor to read, not needed here*)
	MathematicaCommand=Import[packagePath<>"/preload/MathematicaCommand.txt"];
	ShellProcessor=Import[packagePath<>"/preload/ShellProcessor.txt"];
	,
	Print["WARNING: program is not running in command line mode!"];
	
	packagePath=NotebookDirectory[];
	(*LoopMomenta={l1,l2};
	ExternalMomenta={k1,k2,k4};
	Propagators={l1^2,(l1-k1)^2,(l1-k1-k2)^2,(l2+k1+k2)^2,(l2-k4)^2,l2^2,(l1+l2)^2,(l1+k4)^2,(l2+k1)^2};
	Kinematics={k1^2->0,k2^2->0,k4^2->0,k1 k2->s/2,k1 k4->t/2,k2 k4->(-s/2-t/2)};
	GenericPoint={s->-1,t->-3}; 
	TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]};*)
	sectorID=127;
	missionInput="config.txt";
	workingPath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/dbox/";
]








(*
This function appears in many codes
see in SyzygyRed.wl for where they are
If you want to modifie this code, remember to modify all of them!
*)
LogFile="";
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



TimeString[]:=StringRiffle[#[[1;;3]],"."]<>" "<>StringRiffle[#[[4;;6]],":"]&[(ToString[Floor[#]]&/@FromAbsoluteTime[AbsoluteTime[]][[1,1;;6]])]


timer=AbsoluteTime[];





(*If[sectorID=!=-1,
	If[Get[missionStatusFolder<>ToString[sectorID]<>".txt"]=!="ReadyToCompute",
		
		
		
		Exit[];
	]
]*)


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse








(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
If[Get[workingPath<>missionInput]===$Failed,
	PrintAndLog["Unable to open "<>workingPath<>missionInput<>". Exiting."];
	(*Run["echo "<>ToString[sectorID]<>":\tExit[Unable to open "<>missionInput<>"]\t"<>ToString[InputForm[FromUnixTime[UnixTime[]]]]<>" >> tmp/log2.txt"];*)
	Exit[]
];
If[SectorwiseSettings=!={},
	
	SectorwiseSettingListForCurrentSector=Flatten[Select[SectorwiseSettings,#[[1]]===sectorID&][[All,2]]];
	
	ToExpression/@SectorwiseSettingListForCurrentSector;
	
]


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
	];*)
	CutIndices={};
	SpanningCutsMode=True;
]


If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting."];Exit[]]
(*TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting."];Exit[]]
TargetIntegrals//Clear;*)
(*PrintAndLog[sectorID,missionInput,Propagators//InputForm//ToString]
PrintAndLog[Head/@{sectorID,missionInput,Propagators//InputForm}]
PrintAndLog[SDim]*)





If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	Print["Output path has been set as "<>outputPath]
]
If[Intersection[StringSplit[outputPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["Path "<>outputPath<>" is illegal. Exiting."];
	Exit[0];

]
If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]



missionStatusFolder=outputPath<>"tmp/mission_status/"


(*Export[missionStatusFolder<>ToString[sectorID]<>".txt","Computing"//InputForm//ToString]*)
(* modify the status to "Computing" using MissionStatusChecker printed script*)


LogPath=outputPath<>"tmp/log_files/"
If[sectorID=!=-1,
	LogFile=LogPath<>ToString[sectorID]<>".txt";
]
If[!DirectoryQ[#],Run["mkdir "<>#]]&[LogPath]
PrintAndLog["===================================="]
If[sectorID=!=-1,PrintAndLog["Sector ",sectorID," starting at ",TimeString[],"."]]





revalantIntegralsFolder=outputPath<>"tmp/relavant_integrals/"
If[!DirectoryQ[#],Run["mkdir -p "<>#]]&[revalantIntegralsFolder]


Run["echo "<>ToString[sectorID]<>":\tStart\t"<>ToString[InputForm[FromUnixTime[UnixTime[]]]]<>" >> "<>outputPath<>"tmp/log2.txt"]


If[Get[packagePath<>"SparseRREF/SparseRREF.m"]===$Failed,
	PrintAndLog["Unable to Get SparseRREF. Exiting."];
	Run["echo "<>ToString[sectorID]<>":\tExit[Unable to Get SparseRREF]\t"<>ToString[InputForm[FromUnixTime[UnixTime[]]]]<>" >> "<>outputPath<>"tmp/log2.txt"];
	Exit[]
]


If[!DirectoryQ[#],Run["mkdir "<>#]]&[outputPath<>"tmp/"]
TemporaryDirectory = outputPath<>"tmp/singular_temp/singular_temp_"<>ToString[sectorID]<>"/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]






If[MemberQ[{"Linear","Singular+Linear"},moduleIntersectionMethod],
	Get[packagePath<>"LinearSyz/LinearSyz.wl"]
]
Get[packagePath<>"Pak_Algorithm/Pak_Algorithm.wl"]
Get[packagePath<>"SyzygyRed.wl"]
If[sectorID=!=-1,
	LogFile=LogPath<>ToString[sectorID]<>".txt";
]
reductionTasksFolder=outputPath<>"tmp/reduction_tasks/"



PrintAndLog["Preparing"]
Prepare[];
PrintAndLog["Prepared"]


TargetIntegrals=SortBy[Union[Flatten[Get/@FileNames[All,reductionTasksFolder<>ToString[sectorID]<>"/"]]],IntegralOrdering];


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
]

*)






If[!commandLineMode,probeTheFunctions=True];


CuttedQ[integral_,cut_]:=MemberQ[Union[Sign/@((List@@integral[[cut]])-1)],-1](* index that are cut <1 then return True*)


If[sectorID=!=-1,
	(*LogFile=LogPath<>ToString[sectorID]<>".txt";*)
	
	sectorMaps=Get[outputPath<>"tmp/sectorMaps.txt"];
	
	PrintAndLog["Analyzing sector "<>ToString[InputForm[SectorNumberToSectorIndex[sectorID]]]<>"."];
	(*Sectors=SortBy[Union[Sector/@TargetIntegrals],SectorOrdering]//Reverse;*)(*This is a hourse-butt-effect bug*)
	Sectors={SectorNumberToSectorIndex[sectorID]};
	
	
	(*RelavantSectors=SubsectorAllFinder[Sectors];*)
	
	If[NeedSymmetry===False,
		RelavantSectors=SubsectorAllFinder[Sectors]
	,
		RelavantSectors=MappedAndSubSectorsAllFinder[sectorMaps,Sectors]
	];
	
	(*assuming all targets are in the current sector, which is not a cutted sector*)
	If[CutIndices=!={},
		RelavantSectors=Select[RelavantSectors,!CuttedQ[#,CutIndices]&]
	];
	
	ZeroSectors=Select[RelavantSectors,ZeroSectorQ];
	NonZeroSectors=SortBy[Complement[RelavantSectors,Global`ZeroSectors],SectorOrdering]//Reverse;
	PrintAndLog[Length[NonZeroSectors]," non-zero sector(s) are found."];
	ZeroTargets=Select[Global`TargetIntegrals,MemberQ[Global`ZeroSectors,Sector[#]]&];
	ReductionTargets=Complement[Global`TargetIntegrals,Global`ZeroTargets];
	ReductionTasks=Association[Table[NonZeroSectors[[i]]->Select[ReductionTargets,NonZeroSectors[[i]]==Sector[#]&],{i,1,Length[NonZeroSectors]}]];
	
	
	
	ZeroSectorRemoval=SectorElimination/@ZeroSectors;
	IBPList=Association[Table[NonZeroSectors[[i]]->{},{i,1,Length[NonZeroSectors]}]];
	MIList=Association[Table[NonZeroSectors[[i]]->{},{i,1,Length[NonZeroSectors]}]];
	RelavantIntegrals=Association[Table[NonZeroSectors[[i]]->{},{i,1,Length[NonZeroSectors]}]];
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	
	If[Not[MIFromAzuritino===True],
		SectorAnalyze[SectorNumberToSectorIndex[sectorID],
			SeedingMethod->"Zurich",
			Verbosity->1,
			ModuleIntersectionMethod->moduleIntersectionMethod,
			SectorMappingRules->sectorMaps,
			Cut->CutIndices
		];
	,
		SectorAnalyze[SectorNumberToSectorIndex[sectorID],
			SeedingMethod->"Zurich",
			Verbosity->1,
			ModuleIntersectionMethod->moduleIntersectionMethod,
			SectorMappingRules->sectorMaps,
			Cut->CutIndices,
			ZurichInitialSteps->1
		];
	];
	(*end of max memory used*)];
	PrintAndLog["Analyze sector finished in sector ", sectorID,". Time used:", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ];
	
	resultFolder=outputPath<>"results/";
	If[!DirectoryQ[#],Run["mkdir "<>#]]&[resultFolder];
	resultMIFolder=resultFolder<>"MI/";
	If[!DirectoryQ[#],Run["mkdir "<>#]]&[resultMIFolder];
	Export[resultMIFolder<>ToString[sectorID]<>".txt",MIList[SectorNumberToSectorIndex[sectorID]]//InputForm//ToString];
	resultIBPFolder=resultFolder<>"IBP/";
	If[!DirectoryQ[#],Run["mkdir "<>#]]&[resultIBPFolder];
	Export[resultIBPFolder<>ToString[sectorID]<>".txt",IBPList[SectorNumberToSectorIndex[sectorID]]//InputForm//ToString];
	
	Export[revalantIntegralsFolder<>ToString[sectorID]<>".txt",RelavantIntegrals[SectorNumberToSectorIndex[sectorID]]//InputForm//ToString];
	
	For[i=1,i<=Length[NonZeroSectors],i++,
		currentSecNum=SectorNumber[NonZeroSectors[[i]]];
		If[currentSecNum===sectorID,Continue[]];
		reductionTasksFolderForSector[currentSecNum]=reductionTasksFolder<>ToString[currentSecNum]<>"/";
		If[!DirectoryQ[#],Run["mkdir "<>#]]&[reductionTasksFolderForSector[currentSecNum]];
		
		Export[
			reductionTasksFolderForSector[currentSecNum]<>ToString[sectorID]<>".txt",
			ReductionTasks[NonZeroSectors[[i]]]//InputForm//ToString
		]
	];
	
(*	AllMissionSectors=SortBy[(
		(ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex)
		&/@FileNames[All,missionStatusFolder]
	),SectorOrdering]//Reverse;*)
(*	missionStatus=(
		(ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex)->Get[#]
		&/@FileNames[All,missionStatusFolder]
	);
	missionWaitingSupersectors=(
		SortBy[Select[missionStatus,#[[2]]==="WaitingSupersectors"&],SectorOrdering[#[[1]]]&]//Reverse
	)[[All,1]];
	newReadySectors=Select[missionWaitingSupersectors,DeleteCases[Union[SuperSectors[#]/.missionStatus],"ComputationFinished"]==={}&];
	PrintAndLog["Computation finished. New subsectors ready to compute:\n",newReadySectors];
	Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","ReadyToCompute"//InputForm//ToString]&/@newReadySectors*)
]
	
	
	






missionStatus=(
	(ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex)->Get[#]
	&/@FileNames[All,missionStatusFolder]
);
(*missionWaitingSupersectors=(
	SortBy[Select[missionStatus,#[[2]]==="WaitingSupersectors"&],SectorOrdering[#[[1]]]&]//Reverse
)[[All,1]];
newReadySectors=Select[missionWaitingSupersectors,Union[SuperSectors[#]/.missionStatus]==={"ComputationFinished"}&]
For[i=1,i\[LessEqual]Length[newReadySectors],i++,
	Export[missionStatusFolder<>ToString[sectorID]<>".txt","ReadyToCompute"//InputForm//ToString]
]*)





missionReadyToCompute=SortBy[Select[missionStatus,#[[2]]==="ReadyToCompute"&],SectorOrdering[#[[1]]]&]//Reverse


(*runningScriptFolder=outputPath<>"tmp/running_scripts/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[runningScriptFolder]
Export[
	runningScriptFolder<>""<>ToString[sectorID]<>".txt",
	StringRiffle["bash analyze_sector.sh "<>missionInput<>" "<>ToString[SectorNumber[#[[1]]]]<>" &\n"&/@missionReadyToCompute,""]<>"wait\n"
]
	

*)



If[sectorID=!=-1,PrintAndLog["Sector ",sectorID," finished at ",TimeString[],"."]]


Run["echo "<>ToString[sectorID]<>":\tFinished\t"<>ToString[InputForm[FromUnixTime[UnixTime[]]]]<>" >> "<>outputPath<>"tmp/log2.txt"]


reportString=
"#"<>ToString[sectorID]<>" finished. Time used: "<>ToString[InputForm[Round[AbsoluteTime[]-timer]]]<>" s. Max memory used: "<>
ToString[InputForm[Round[MaxMemoryUsed[]/(1024^2)]]]<>" MB."


Run["echo \""<>reportString<>"\" >> "<>outputPath<>"tmp/log.txt"]


If[sectorID=!=-1,
	report="ReportingFinished\n"<>StringRiffle[$CommandLine," "]<>"";
	(*Export[missionStatusFolder<>ToString[sectorID]<>".txt","ComputationFinished"//InputForm//ToString]*)
	Export[missionStatusFolder<>ToString[sectorID]<>".txt",report//InputForm//ToString];
	
]
(*
2024.11.5
here, we make this change because:
	we will not let Analyze_Sectors.wl to write "ComputationFinished" into status.
	This work will done by the MissionStatusChecker.wl in its main While loop, 
	to detect that the process (recorded as command line command) really ends.
	Thus, we can prevent unlabelled sectors. 
	"ComputationFinished" will be labelled after Analyze_Sectors.wl really finished.
	The reason is, unlabelled sector is harmful to WorkerAllocationSystem. 
	It will make the system UNDERESTIMATE the workers in use 
	and grant workers for a manager more than MathKernelLimit
	Then everything is breaking down.
	So we are here to prevent the above.

*)
