(* ::Package:: *)

OptionSimplification=12;


commandLineMode=True





If[commandLineMode,
	workingPath=DirectoryName[$InputFileName];
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


Print["Initializing."]


If[MemberQ[StringSplit[workingPath,""]," "],
	Print["Character \"space\" in working path, aborting."];
	Exit[0]
]


If[FileExistsQ[workingPath<>#],Run["rm -f "<>workingPath<>#]]&/@
{"log.txt","log1.txt","log2.txt","log3.txt","log4.txt"}


AppendTo[$Path,workingPath];
If[Get[missionInput]===$Failed,Print["Unable to open "<>missionInput<>". Exiting.";Exit[]]]



If[!DirectoryQ[#],Run["mkdir "<>#]]&[workingPath<>"outputs"]
outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/"
If[DirectoryQ[outputPath],
	continueQ=InputString["Reduction outputs \""<>ReductionOutputName<>"\" already exists. Do you want to overwrite them? Type Y or y to continue. Type others to abort.\n"];
	
	If[Or[continueQ=="y",continueQ=="Y"],
		Print["Removing "<>outputPath];
		Run["rm -rf "<>outputPath];
		Print["done."]
		,
		Print["Aborted."];
		Print["You can modify \"inputs_and_config.txt\" to change ReductionOutputName"];
		Exit[0]
	]
]
Run["mkdir "<>outputPath]
Run["cp "<>workingPath<>missionInput<>" "outputPath<>missionInput]


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse


TemporaryDirectory = outputPath<>"tmp/"
Run["rm -rf "<>TemporaryDirectory];
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]
TemporaryDirectorySingular = TemporaryDirectory<>"singular_temp/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectorySingular]
SingularDirectory = "/usr/bin/Singular"
Get[workingPath<>"Pak_Algorithm/Pak_Algorithm.wl"]
Get[SyzygyRedPackageFile](*I think the place of this .wl file can be fixed*)
runningScriptFolder=outputPath<>"tmp/running_scripts/"



IntegralOrder="Global";
Prepare[];


missionStatusFolder=TemporaryDirectory<>"mission_status/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[missionStatusFolder]
reductionTasksFolder=TemporaryDirectory<>"reduction_tasks/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[reductionTasksFolder]


Print["Finding nonzero sectors..."]
timer=AbsoluteTime[];
Sectors=SortBy[Union[Sector/@TargetIntegrals],SectorOrdering]//Reverse;
RelavantSectors=SubsectorAllFinder[Sectors];
ZeroSectors=Select[RelavantSectors,ZeroSectorQ];
NonZeroSectors=SortBy[Complement[RelavantSectors,Global`ZeroSectors],SectorOrdering]//Reverse;
Print[Length[NonZeroSectors]," non-zero sector(s) are found."];
ZeroTargets=Select[Global`TargetIntegrals,MemberQ[Global`ZeroSectors,Sector[#]]&];
ReductionTargets=Complement[Global`TargetIntegrals,Global`ZeroTargets];
ReductionTasks=Association[Table[NonZeroSectors[[i]]->Select[ReductionTargets,NonZeroSectors[[i]]==Sector[#]&],{i,1,Length[NonZeroSectors]}]];
ZeroSectorRemoval=SectorElimination/@ZeroSectors;
IBPList=Association[Table[NonZeroSectors[[i]]->{},{i,1,Length[NonZeroSectors]}]];
MIList=Association[Table[NonZeroSectors[[i]]->{},{i,1,Length[NonZeroSectors]}]];
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]



resultFolder=outputPath<>"results/";
If[!DirectoryQ[#],Run["mkdir "<>#]]&[resultFolder];
resultMIFolder=resultFolder<>"MI/";
If[!DirectoryQ[#],Run["mkdir "<>#]]&[resultMIFolder];
resultIBPFolder=resultFolder<>"IBP/";
If[!DirectoryQ[#],Run["mkdir "<>#]]&[resultIBPFolder];

Print["Exporting zero targets..."];
timer=AbsoluteTime[];
ZeroTargetsGatheredBySector=GatherBy[ZeroTargets,Sector]
For[i=1,i<=Length[ZeroTargetsGatheredBySector],i++,
	currentZeroTargets=ZeroTargetsGatheredBySector[[i]];
	currentZeroSector=Sector[currentZeroTargets[[1]]];
	currentZeroSectorID=SectorNumber[currentZeroSector];
	Export[resultMIFolder<>ToString[currentZeroSectorID]<>".txt",{}//InputForm//ToString];
	Export[resultIBPFolder<>ToString[currentZeroSectorID]<>".txt",currentZeroTargets//InputForm//ToString]
]
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


Print["Finding symmetry mappings between sectors..."];
timer=AbsoluteTime[];
If[NeedSymmetry===False,
	{uniqueSectors,mappedSectors,sectorMaps}={NonZeroSectors,{},{}}
,
	{uniqueSectors,mappedSectors,sectorMaps}=SectorMaps[NonZeroSectors];
	
]
Export[outputPath<>"sectorMaps.txt",sectorMaps//InputForm//ToString]
Print[Length[mappedSectors]," mapped sector(s) found."];
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]



Print["Exporting mapped targets..."];
timer=AbsoluteTime[];

newReductionTargets=ReductionTargets;

For[i=1,i<=Length[mappedSectors],i++,
	mappedSector=mappedSectors[[i]];
	mappedTargets=Select[ReductionTargets,Sector[#]===mappedSector&];
	mappedSectorID=SectorNumber[mappedSector];
	mapIBPs=#-(SymmetryMap[sectorMaps,#])&/@mappedTargets;
	Export[resultMIFolder<>ToString[mappedSectorID]<>".txt",{}//InputForm//ToString];
	Export[resultIBPFolder<>ToString[mappedSectorID]<>".txt",mapIBPs//InputForm//ToString];
	newReductionTargets=Union[newReductionTargets,IntegralList[mapIBPs]]
];
ReductionTargets=Select[newReductionTargets,MemberQ[uniqueSectors,Sector[#]]&]



Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]











Print["Constructing trees of missions..."];
timer=AbsoluteTime[];
mapAndSubRelationMatrix=Table[0,Length[uniqueSectors],Length[uniqueSectors]]



For[indexI=1,indexI<=Length[uniqueSectors],indexI++,
	mappedAndSubSectorsAll=MappedAndSubSectorsAllFinder[sectorMaps,{uniqueSectors[[indexI]]}];
	
	For[indexK=1,indexK<=Length[uniqueSectors],indexK++,
		If[MemberQ[mappedAndSubSectorsAll,uniqueSectors[[indexK]]]&&indexI=!=indexK,
			mapAndSubRelationMatrix[[indexI,indexK]]=1
		]
	]
]

(*Print["ysudgfasljkdfkasfdg;laslk;dsfgjkasjkedfghak;lfsdgl"];*)

superOrSourceSectors={}
For[indexK=1,indexK<=Length[uniqueSectors],indexK++,
	superOrSourceSectorsForCurrentSector={};
	For[indexI=1,indexI<=Length[uniqueSectors],indexI++,
		If[mapAndSubRelationMatrix[[indexI,indexK]]===1,
			superOrSourceSectorsForCurrentSector=Join[
				superOrSourceSectorsForCurrentSector,
				{uniqueSectors[[indexI]]}
			]
		]
	];
	superOrSourceSectorsForCurrentSector=SortBy[superOrSourceSectorsForCurrentSector,SectorOrdering];
	superOrSourceSectors=Join[
		superOrSourceSectors,
		{uniqueSectors[[indexK]]->superOrSourceSectorsForCurrentSector}
	]
	
]
Export[outputPath<>"superOrSourceSectors.txt",superOrSourceSectors//InputForm//ToString]
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


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
MasterMissions[id_]:=SectorNumber/@SuperSectors[SectorNumberToSectorIndex[id]]*)


(*allMissions=SectorNumber/@NonZeroSectors*)


(*masterMissions={#}->MasterMissions[#]&/@allMissions*)


(*topSectors=SectorNumberToSectorIndex/@Select[Flatten[masterMissions[[All,1]]],({#}/.(masterMissions))==={}&]*)


Print["Creating reduction tasks."]
timer=AbsoluteTime[];
For[i=1,i<=Length[uniqueSectors],i++,
	currentSecNum=SectorNumber[uniqueSectors[[i]]];
	reductionTasksFolderForSector[currentSecNum]=reductionTasksFolder<>ToString[currentSecNum]<>"/";
	Run["rm -rf "<>reductionTasksFolderForSector[currentSecNum]];
	If[!DirectoryQ[#],Run["mkdir "<>#]]&[reductionTasksFolderForSector[currentSecNum]];
	Export[reductionTasksFolderForSector[currentSecNum]<>"-1.txt",Select[ReductionTargets,Sector[#]===uniqueSectors[[i]]&]//InputForm//ToString]
]
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


Print["Initializing mission status..."];
timer=AbsoluteTime[];
Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","WaitingSupersectors"//InputForm//ToString]&/@uniqueSectors;
Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","ComputationFinished"//InputForm//ToString]&/@mappedSectors;
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]



(*Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","ReadyToCompute"//InputForm//ToString]&/@topSectors;*)
(*Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","ComputationFinished"//InputForm//ToString]&/@NonZeroSectors;*)


(*TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]};
SimpleIBP[Verbosity->1,SeedingMethod->"Direct"]//AbsoluteTiming*)





Print["Finished."]
