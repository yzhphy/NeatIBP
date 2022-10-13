(* ::Package:: *)

OptionSimplification=12;
moduleIntersectionMethod="Singular"


commandLineMode=True


If[commandLineMode,
	workingPath=DirectoryName[$InputFileName];
	missionInput=$CommandLine[[-2]];
	sectorID=$CommandLine[[-1]]//ToExpression;
	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	(*LoopMomenta={l1,l2};
	ExternalMomenta={k1,k2,k4};
	Propagators={l1^2,(l1-k1)^2,(l1-k1-k2)^2,(l2+k1+k2)^2,(l2-k4)^2,l2^2,(l1+l2)^2,(l1+k4)^2,(l2+k1)^2};
	Kinematics={k1^2->0,k2^2->0,k4^2->0,k1 k2->s/2,k1 k4->t/2,k2 k4->(-s/2-t/2)};
	GenericPoint={s->-1,t->-3}; 
	TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]};*)
	sectorID=879;
	missionInput="example.txt";
	
]

















LogFile="";
PrintAndLog[x___]:=Module[{string},
	If[LogFile=!="",
		string=StringRiffle[ToString/@{x},""];
		Run["echo \""<>string<>"\" >> "<>LogFile]
	];
	Print[x]
]


timer=AbsoluteTime[];





(*If[sectorID=!=-1,
	If[Get[missionStatusFolder<>ToString[sectorID]<>".txt"]=!="ReadyToCompute",
		
		
		
		Exit[];
	]
]*)


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse








AppendTo[$Path,workingPath];
If[Get[missionInput]===$Failed,
	PrintAndLog["Unable to open "<>missionInput<>". Exiting."];
	(*Run["echo "<>ToString[sectorID]<>":\tExit[Unable to open "<>missionInput<>"]\t"<>ToString[InputForm[FromUnixTime[UnixTime[]]]]<>" >> log2.txt"];*)
	Exit[]
];
TargetIntegrals//Clear;
(*PrintAndLog[sectorID,missionInput,Propagators//InputForm//ToString]
PrintAndLog[Head/@{sectorID,missionInput,Propagators//InputForm}]
PrintAndLog[SDim]*)



outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/"


missionStatusFolder=outputPath<>"tmp/mission_status/"


revalantIntegralsFolder=outputPath<>"tmp/relavant_integrals/"
If[!DirectoryQ[#],Run["mkdir -p "<>#]]&[revalantIntegralsFolder]


Run["echo "<>ToString[sectorID]<>":\tStart\t"<>ToString[InputForm[FromUnixTime[UnixTime[]]]]<>" >> "<>outputPath<>"log2.txt"]


If[Get[workingPath<>"SparseRREF/SparseRREF.m"]===$Failed,
	PrintAndLog["Unable to Get SparseRREF. Exiting."];
	Run["echo "<>ToString[sectorID]<>":\tExit[Unable to Get SparseRREF]\t"<>ToString[InputForm[FromUnixTime[UnixTime[]]]]<>" >> "<>outputPath<>"log2.txt"];
	Exit[]
]


If[!DirectoryQ[#],Run["mkdir "<>#]]&[outputPath<>"tmp/"]
TemporaryDirectory = outputPath<>"tmp/singular_temp/singular_temp_"<>ToString[sectorID]<>"/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]
SingularDirectory = "/usr/bin/Singular"

Get[workingPath<>"Pak_Algorithm/Pak_Algorithm.wl"]
Get[SyzygyRedPackageFile]

reductionTasksFolder=outputPath<>"tmp/reduction_tasks/"

LogPath=outputPath<>"tmp/log_files/"

If[!DirectoryQ[#],Run["mkdir "<>#]]&[LogPath]




IntegralOrder="Global";
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


If[sectorID=!=-1,
	LogFile=LogPath<>ToString[sectorID]<>".txt";
	
	sectorMaps=Get[outputPath<>"sectorMaps.txt"];
	
	PrintAndLog["Analyzing sector "<>ToString[InputForm[SectorNumberToSectorIndex[sectorID]]]<>"."];
	Sectors=SortBy[Union[Sector/@TargetIntegrals],SectorOrdering]//Reverse;
	(*RelavantSectors=SubsectorAllFinder[Sectors];*)
	
	If[NeedSymmetry===False,
		RelavantSectors=SubsectorAllFinder[Sectors]
	,
		RelavantSectors=MappedAndSubSectorsAllFinder[sectorMaps,Sectors]
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
	
		
	SectorAnalyze[SectorNumberToSectorIndex[sectorID],SeedingMethod->"Zurich",Verbosity->1,ModuleIntersectionMethod->moduleIntersectionMethod,SectorMappingRules->sectorMaps];
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
	Export[missionStatusFolder<>ToString[sectorID]<>".txt","ComputationFinished"//InputForm//ToString]
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


runningScriptFolder=outputPath<>"tmp/running_scripts/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[runningScriptFolder]
Export[
	runningScriptFolder<>""<>ToString[sectorID]<>".txt",
	StringRiffle["bash analyze_sector.sh "<>missionInput<>" "<>ToString[SectorNumber[#[[1]]]]<>" &\n"&/@missionReadyToCompute,""]<>"wait\n"
]
	





If[sectorID=!=-1,PrintAndLog["Sector ",sectorID," finished."]]


Run["echo "<>ToString[sectorID]<>":\tFinished\t"<>ToString[InputForm[FromUnixTime[UnixTime[]]]]<>" >> "<>outputPath<>"log2.txt"]


reportString=
"#"<>ToString[sectorID]<>" finished. Time used: "<>ToString[InputForm[Round[AbsoluteTime[]-timer]]]<>" s. Max memory used: "<>
ToString[InputForm[Round[MaxMemoryUsed[]/(1024^2)]]]<>" MB."


Run["echo \""<>reportString<>"\" >> "<>outputPath<>"log.txt"]
