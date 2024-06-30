(* ::Package:: *)

commandLineMode=True








If[commandLineMode,
	(*workingPath=DirectoryName[$InputFileName];*)
	
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
	workingPath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples/dbox/";
	missionInput="config.txt"
	(*
	LoopMomenta={l1,l2};
	ExternalMomenta={k1,k2,k4};
	Propagators={l1^2,(l1-k1)^2,(l1-k1-k2)^2,(l2+k1+k2)^2,(l2-k4)^2,l2^2,(l1+l2)^2,(l1+k4)^2,(l2+k1)^2};
	Kinematics={k1^2->0,k2^2->0,k4^2->0,k1 k2->s/2,k1 k4->t/2,k2 k4->(-s/2-t/2)};
	GenericPoint={s->-1,t->-3}; 
	TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]}*)
	
]





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



readmeTextLines=Import[packagePath<>"README.md"];
versionNumber=DeleteCases[StringSplit[StringSplit[readmeTextLines,"## Version"][[2]],"\n"],""][[1]];
Print["================================================================
NeatIBP version "<>versionNumber<>"
by: Janko Boehm, Rourou Ma, Hefeng Xu, Zihao Wu and Yang Zhang.
================================================================"];
If[debugMode===True,Print["*** debug mode is on ***"]]



Print["Initializing."]


(*If[MemberQ[StringSplit[workingPath,""]," "],
	Print["Character \"space\" in working path, aborting."];
	Exit[0]
]
If[MemberQ[StringSplit[packagePath,""]," "],
	Print["Character \"space\" in package path, aborting."];
	Exit[0]
]*)


If[Intersection[StringSplit[packagePath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["****  packagePath "<>packagePath<>" is illegal. Exiting."];
	Exit[0];

]


If[Intersection[StringSplit[workingPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["****  workingPath "<>workingPath<>" is illegal. Exiting."];
	Exit[0];

]





(*If[FileExistsQ[workingPath<>#],Run["rm -f "<>workingPath<>#]]&/@
{"log.txt","log1.txt","log2.txt","log3.txt","log4.txt"}*)
(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
If[Get[workingPath<>missionInput]===$Failed,Print["****  Unable to open config file "<>workingPath<>missionInput<>". Exiting.";Exit[]]]
If[Get[kinematicsFile]===$Failed,Print["****  Unable to open kinematics file "<>kinematicsFile<>". Exiting.";Exit[]]]
TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["****  Unable to open target intergals file "<>targetIntegralsFile<>". Exiting.";Exit[]]]





(* ::Section:: *)
(*Setting outputPath*)


If[outputPath===Automatic,
	automaticOutputPath=True;
	If[!DirectoryQ[#],Run["mkdir "<>#]]&[workingPath<>"outputs"];
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	Print["Output path has been set as "<>outputPath]
	,
	automaticOutputPath=False;
]
If[Intersection[StringSplit[outputPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["****  Path "<>outputPath<>" is illegal. Exiting."];
	Exit[0];

]
If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]

(*If[FileExistsQ[#],Run["rm "<>#]]&[outputPath<>"tmp/"<>"initialized.txt"];*)
If[FileExistsQ[outputPath<>"tmp/"<>"initialized.txt"],
	Export[outputPath<>"tmp/"<>"once_initialized.txt",""];
	Run["rm "<>outputPath<>"tmp/"<>"initialized.txt"]
];


FinishedOutputPathQ[outputPath_]:=Module[{subDirs,IBPallExistQs},
	If[FileExistsQ[outputPath<>"results/IBP_all.txt"],
		Return[True]
	,
		If[DirectoryQ[outputPath<>"results/results_spanning_cuts/"],
			subDirs=Select[FileNames[All,outputPath<>"results/results_spanning_cuts/"],DirectoryQ];
			subDirs=#<>"/"&/@subDirs;
			IBPallExistQs=FileExistsQ[#<>"IBP_all.txt"]&/@subDirs;
			If[MemberQ[IBPallExistQs,False],
				Return[False]
			,
				Return[True]
			]
		,
			Return[False]
		]
	];
	
]


If[And[DirectoryQ[outputPath],automaticOutputPath],
	
	continueQ=InputString["Output directory \""<>outputPath<>"\" already exists. Do you want to delete it? Type Y or y to continue. Type others to abort.\n"];
	
	If[Or[continueQ=="y",continueQ=="Y"],
		If[!FinishedOutputPathQ[outputPath],
			Print["Output directory "<>outputPath<>" is not a complete directory.\nIt could be the output directory of a running NeatIBP mission, or could be a unfinished NeatIBP mission in the past."];
			Print["It is highly recommended that you check to make sure that it is not the former case. Otherwise, there will be conflict and unknown errors will occur."];
			Print["Do you still wish to delete "<>outputPath<>" ?"];
			continueQ2=InputString["If so, type Y or y to continue. Type others to abort.\n"];
			If[!Or[continueQ2=="y",continueQ2=="Y"],
				If[FileExistsQ[#],Run["rm -f "<>#]]&[outputPath<>"tmp/initialized.txt"];
				Print["Aborted."];
				Exit[0]
			]
		];
		Print["Removing "<>outputPath];
		Run["rm -rf "<>outputPath];
		Print["done."]
		,
		Print["Aborted."];
		Print["You can modify input file(s) to change ReductionOutputName"];
		Exit[0]
	]
]
If[And[DirectoryQ[outputPath],!automaticOutputPath],
	continueQ=InputString["Output directory \""<>outputPath<>"\" already exists.
Since this is not an automatic output folder, I cannot delete it for you considering security.
Please reset outputPath in "<>AbsMissionInput<>", or delete "<>outputPath<>" manually. Then try again.
Type in anything to abort.
"];
	
	Exit[0];
]
Run["mkdir -p "<>outputPath]
inputBackupPath=outputPath<>"inputs/"
Run["mkdir -p "<>inputBackupPath]
Run["cp "<>workingPath<>missionInput<>" "inputBackupPath<>missionInput]
Run["cp "<>kinematicsFile<>" "inputBackupPath]
Run["cp "<>targetIntegralsFile<>" "inputBackupPath]


(* ::Section:: *)
(*Some file or folders*)


TemporaryDirectory = outputPath<>"tmp/"
(*Run["rm -rf "<>TemporaryDirectory];*)(*It seems to be useless*)
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]
TemporaryDirectorySingular = TemporaryDirectory<>"singular_temp/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectorySingular]


Get[packagePath<>"Pak_Algorithm/Pak_Algorithm.wl"]
Get[packagePath<>"SyzygyRed.wl"]
runningScriptFolder=outputPath<>"tmp/running_scripts/"


LogPath=outputPath<>"tmp/log_files/"
LogFile=LogPath<>"initialization"<>".txt";
If[!DirectoryQ[#],Run["mkdir "<>#]]&[LogPath]
PrintAndLog["----------------\nNeatIBP version: "<>versionNumber]
PrintAndLog["Start initialization steps."]

Export[TemporaryDirectory<>"start_abs_time.txt",AbsoluteTime[]//InputForm//ToString]


(* ::Section:: *)
(*Validating Inputs*)


(* ::Subsection:: *)
(*Variable Name Protection*)


ProtectedNames=
{m,x,y,z,G,d,n,L,zeroLoopMomenta,Momenta,ScalarVarRep,var,ScalarVar,BaikovMatrix,SDim,GramMatrix,LoopExternalScalars,ScalarTangentSet,
ScalarExtendedTangentSet,BaikovKernelScalar,BaikovRevRep,BaikovRep,BaikovKernel,Parameters,TangentSet,ExtendedTangentSet,
ForwardRep,BackwardRep,Scalar2sp,sp2Scalar,sp,PolynomialU,PolynomialF,PolynomialG,numericPolynomialG,gen,varOrder,ss,(*in SyzygyRed.wl, IntegerPartition function. I think this variable, ss, can be set as local*)
ZeroSectors,NonZeroSectors,ZeroTargets,ReductionTargets,ReductionTasks,ZeroSectorRemoval,IBPList,MIList,SectorAnalyzeTiming,IntegralR,FI,BasicRawIBPs,FI0,ZM0,secNum,SelfSymmetryR,ZM,RelavantIntegrals,
groupMomentumU,groupMomentumV,StdL,i,spanningCuts,bottomSectors,topSectors,spanningCutsMissionMainPath,TemporaryDirectory,
Prepare,SectorwiseSettingListForCurrentSector,inputParameters

}//DeleteDuplicates
CheckRange={"TargetIntegrals","LoopMomenta","ExternalMomenta","Propagators","Kinematics","GenericPoint","GenericD"
}
AppearableList={
G->{"TargetIntegrals"},
d->{"GenericD"}
}
CheckFree[name_]:=Module[{range},
	range=CheckRange;
	If[MemberQ[AppearableList[[All,1]],name],range=Complement[range,name/.AppearableList]];
	Select[range,!FreeQ[ToExpression[#],name]&]
]
allProtectedNamesDisappearQ=True
Module[{name,i,checkFreeResult},
	For[i=1,i<=Length[ProtectedNames],i++,
		name=ProtectedNames[[i]];
		checkFreeResult=CheckFree[name];
		If[Length[checkFreeResult]>0,
			PrintAndLog["****  Protected symbol \"",name,"\" appear in the following input(s):\n\t", checkFreeResult];
			allProtectedNamesDisappearQ=False;
		]
	]
]
If[allProtectedNamesDisappearQ===False,
	PrintAndLog["****  Please raname the above symbol(s) in your input(s). Exiting..."];
	Exit[0];
]


(* ::Subsection:: *)
(*Other Validations*)


inputParameters=Complement[Variables[{Propagators,Kinematics[[All,2]]}],LoopMomenta,ExternalMomenta];
If[!SubsetQ[GenericPoint[[All,1]],inputParameters],
	PrintAndLog["****  GenericPoint dose not cover all scalar variables. Exiting..."];
	Exit[0];
]
If[Variables[inputParameters/.GenericPoint]=!={},
	PrintAndLog["****  GenericPoint ",GenericPoint," is not properly defined. Exiting..."];
	Exit[0];
]
If[Variables[d/.GenericD]=!={},
	PrintAndLog["****  GenericD ",GenericD," is not defined or not properly defined. Exiting..."];
	Exit[0];
]
If[ParameterRepermute===True,
	If[Sort[ParameterRepermuteIndex]=!=Range[Length[inputParameters]],
		PrintAndLog["****  ParameterRepermuteIndex ",ParameterRepermuteIndex," is not a well-defined permutation with length ",Length[inputParameters],". Exiting..."];
		Exit[0];
	]
]


If[Variables[TensorProduct[ExternalMomenta,ExternalMomenta]/.Kinematics/.GenericPoint]=!={},
	PrintAndLog["****  Kinematics replacement ",Kinematics," is not enough for external momenta ",ExternalMomenta,". Exiting..."];
	Exit[0];
]


If[Head[CutIndices]=!=List&&!MemberQ[{"spanning cuts"},CutIndices],
	PrintAndLog["****  Unexpected cut indices. Exiting..."];
	Exit[0]
]


If[And[MemberQ[{"spanning cuts"},CutIndices],Not[debugMode===True]],
	PrintAndLog["****  In the current version, spanning cuts mode is under developement. There may lurks unknown bugs. Please set debugMode=True if you want to try. Exiting..."];
	Exit[0]
]
If[And[FlexibleNeatIBPIntersectionDegreeBound,Not[debugMode===True]],
	PrintAndLog["****  In current version, FlexibleNeatIBPIntersectionDegreeBound is under developement. There may lurks unknown bugs. Please set debugMode=True if you want to try. Exiting..."];
	Exit[0]
]

(*If[And[SimplifySyzygyVectorsByCut,Not[debugMode===True]],
	PrintAndLog["****  In current version, SimplifySyzygyVectorsByCut is under developement. There may lurks unknown bugs. Please set debugMode=True if you want to try. Exiting..."];
	Exit[0]
]
*)




If[CutIndices=!={}&&NeedSymmetry,
	PrintAndLog["****  Please turn off symmetry if there is any cut indices. Exiting..."];
	Exit[0]
]



If[(CutIndices==="spanning cuts")&&(automaticOutputPath===False),
	PrintAndLog["****  In spanning cuts mode, outputPath must be set to be Automatic. Exiting..."];(*I think this is unecessary, we can modify the config for the cut missions*)
	Exit[0]
]


If[IntegralOrder =!= "MultiplePropagatorElimination"&&MIFromAzuritino,
	PrintAndLog["****  IntegralOrder must be set as \"MultiplePropagatorElimination\" if MIFromArzuritino is enabled. Exiting..."];
	Exit[0]
]


If[FiniteFieldModulus>46337,
	PrintAndLog["****  FiniteFieldModulus must not be larger than 46337. Exiting..."];
	Exit[0]
]
If[!PrimeQ[FiniteFieldModulus],
	PrintAndLog["****  FiniteFieldModulus ",FiniteFieldModulus ," is not a prime number. Exiting..."];
	Exit[0]
]



If[!MemberQ[{"MultiplePropagatorElimination","ISPElimination","Global"},IntegralOrder],
	PrintAndLog["****  Invalid IntegralOrder \""<>ToString[IntegralOrder]<>"\". Exiting..."];
	Exit[0]
]
If[!MemberQ[{"MomentumSpace","FeynmanParameterization"},ExternalMomentaGroupingMethod],
	PrintAndLog["****  Invalid ExternalMomentaGroupingMethod \""<>ToString[ExternalMomentaGroupingMethod]<>"\". Exiting..."];
	Exit[0]
]
If[!MemberQ[{"Orthogonalization","DeltaPlaneProjection"},PreferedExternalExtendedRotationMethod],
	PrintAndLog["****  Invalid PreferedExternalExtendedRotationMethod \""<>ToString[PreferedExternalExtendedRotationMethod]<>"\". Exiting..."];
	Exit[0]
]


If[!MemberQ[{"Zurich","Zurich+FIBPGrouping"},NeatIBPSeedingMethod],
	PrintAndLog["****  Invalid IntegralOrder \""<>ToString[IntegralOrder]<>"\". Exiting..."];
	Exit[0]
]
If[FIBPGroupingStartRatio>1||FIBPGroupingStartRatio<0,
	PrintAndLog["****  FIBPGroupingStartRatio must be between 0 and 1. Exiting..."];
	Exit[0]
]
If[FIBPGroupingRemainingGroups<1||Head[FIBPGroupingRemainingGroups]=!=Integer,
	PrintAndLog["****  FIBPGroupingRemainingGroups must be a positive integer. Exiting..."];
	Exit[0]
]


(*If[NeedSymmetry&&MIFromAzuritino,
	PrintAndLog["****  Sorry, Azuritino dose not support symmetry in the current version.\n We are working on it and soon it will come.\n Exiting..."];
	Exit[0]
]*)


CutableQ[integral_,cut_]:=!MemberQ[Union[Sign/@((List@@integral[[cut]])-1)],1](* index that are cut must \[LessEqual] 1*)
CuttedQ[integral_,cut_]:=MemberQ[Union[Sign/@((List@@integral[[cut]])-1)],-1](* index that are cut <1 then return True*)


If[CutIndices=!="spanning cuts",
	If[MemberQ[CutableQ[#,CutIndices]&/@TargetIntegrals,False],
		PrintAndLog["****  Sorry, this version dose not support cutting indices larger than 1. Please remove corresponding target integrals with such multiple propagators.\nExiting..."];
		Exit[0];
	]
]








preparation=Prepare[];
If[preparation==="Propagators Length and SDim mismatch.",
	PrintAndLog["*** The length of the Propagators ",Propagators," ",Length[Propagators]," and SDim=L(L+1)/2+LE=",SDim," mismatch. Exiting."];
	Exit[0];
]
If[preparation==="Propagators is not a well-defined independent basis.",
	PrintAndLog["*** The Propagators ",Propagators," is not a well-defined independent basis. Exiting."];
	Exit[0];
]


If[Union[(Length[Propagators]===Length[#])&/@TargetIntegrals]=!={True},
	PrintAndLog["****  Length of Propagators and indices of TargetIntegrals mismatch. Exiting..."];
	Exit[0];
]


(* ::Section:: *)
(*Other file read and writes*)


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse





resultFolder=outputPath<>"results/";
If[!DirectoryQ[#],Run["mkdir "<>#]]&[resultFolder];
resultMIFolder=resultFolder<>"MI/";
If[!DirectoryQ[#],Run["mkdir "<>#]]&[resultMIFolder];
resultIBPFolder=resultFolder<>"IBP/";
If[!DirectoryQ[#],Run["mkdir "<>#]]&[resultIBPFolder];














missionStatusFolder=TemporaryDirectory<>"mission_status/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[missionStatusFolder]
reductionTasksFolder=TemporaryDirectory<>"reduction_tasks/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[reductionTasksFolder]


(* ::Section:: *)
(*The rest steps*)


If[CutIndices=!={}&&CutIndices=!="spanning cuts",
	
	PrintAndLog["Removing target integrals vanishing on cut "<>ToString[InputForm[CutIndices]]];
	timer=AbsoluteTime[];
	cuttedTargets=Select[TargetIntegrals,CuttedQ[#,CutIndices]&];
	TargetIntegrals=Complement[TargetIntegrals,cuttedTargets];
	gatheredCuttedTargets=GatherBy[cuttedTargets,Sector];
	For[i=1,i<=Length[gatheredCuttedTargets],i++,
		gatheredCuttedTargetsOnASector=gatheredCuttedTargets[[i]];
		cuttedSector=Sector[gatheredCuttedTargetsOnASector[[1]]];
		cuttedSectorID=SectorNumber[cuttedSector];
		Export[resultMIFolder<>ToString[cuttedSectorID]<>".txt",{}//InputForm//ToString];
		Export[resultIBPFolder<>ToString[cuttedSectorID]<>".txt",gatheredCuttedTargetsOnASector//InputForm//ToString]
	];
	PrintAndLog["\t"<>ToString[Length[cuttedTargets]]<>" vanishing targets removed. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]
]





PrintAndLog["Finding nonzero sectors..."]
timer=AbsoluteTime[];
Sectors=SortBy[Union[Sector/@TargetIntegrals],SectorOrdering]//Reverse;

RelavantSectors=SubsectorAllFinder[Sectors];

If[CutIndices=!={}&&CutIndices=!="spanning cuts",
	RelavantSectors=Select[RelavantSectors,!CuttedQ[#,CutIndices]&]
];


ZeroSectors=Select[RelavantSectors,ZeroSectorQ];
NonZeroSectors=SortBy[Complement[RelavantSectors,Global`ZeroSectors],SectorOrdering]//Reverse;
PrintAndLog["\t",Length[NonZeroSectors]," non-zero sector(s) are found."];
ZeroTargets=Select[Global`TargetIntegrals,MemberQ[Global`ZeroSectors,Sector[#]]&];
ReductionTargets=Complement[Global`TargetIntegrals,Global`ZeroTargets];
ReductionTasks=Association[Table[NonZeroSectors[[i]]->Select[ReductionTargets,NonZeroSectors[[i]]==Sector[#]&],{i,1,Length[NonZeroSectors]}]];
ZeroSectorRemoval=SectorElimination/@ZeroSectors;
IBPList=Association[Table[NonZeroSectors[[i]]->{},{i,1,Length[NonZeroSectors]}]];
MIList=Association[Table[NonZeroSectors[[i]]->{},{i,1,Length[NonZeroSectors]}]];
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]







PrintAndLog["Exporting zero targets..."];
timer=AbsoluteTime[];
ZeroTargetsGatheredBySector=GatherBy[ZeroTargets,Sector]
For[i=1,i<=Length[ZeroTargetsGatheredBySector],i++,
	currentZeroTargets=ZeroTargetsGatheredBySector[[i]];
	currentZeroSector=Sector[currentZeroTargets[[1]]];
	currentZeroSectorID=SectorNumber[currentZeroSector];
	Export[resultMIFolder<>ToString[currentZeroSectorID]<>".txt",{}//InputForm//ToString];
	Export[resultIBPFolder<>ToString[currentZeroSectorID]<>".txt",currentZeroTargets//InputForm//ToString]
]
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


PrintAndLog["Finding symmetry mappings between sectors..."];
timer=AbsoluteTime[];
If[NeedSymmetry===False,
	PrintAndLog["\tSymmetry is off, skip."];
	{uniqueSectors,mappedSectors,sectorMaps}={NonZeroSectors,{},{}}
,
	{uniqueSectors,mappedSectors,sectorMaps}=SectorMaps[NonZeroSectors];
	
]
Export[outputPath<>"tmp/sectorMaps.txt",sectorMaps//InputForm//ToString]
PrintAndLog["\t",Length[mappedSectors]," mapped sector(s) found."];
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]



PrintAndLog["Exporting mapped targets..."];
timer=AbsoluteTime[];
If[NeedSymmetry===False,PrintAndLog["\tSymmetry is off, skip."]]
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
(*
Here, some targets from symmetry are collected and will be labeled to be from sector "-1" (which means from user input.)
But they are not!
Although they will not cause error but may cause confusion while debugging.
No need to change, but better to change.
*)
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]














PrintAndLog["Constructing trees of missions..."];
timer=AbsoluteTime[];
mapAndSubRelationMatrix=Table[0,Length[uniqueSectors],Length[uniqueSectors]]



PrintAndLog["\t Building mapAndSubRelationMatrix..."];
For[indexI=1,indexI<=Length[uniqueSectors],indexI++,
	mappedAndSubSectorsAll=MappedAndSubSectorsAllFinder[sectorMaps,{uniqueSectors[[indexI]]}];
	
	For[indexK=1,indexK<=Length[uniqueSectors],indexK++,
		If[MemberQ[mappedAndSubSectorsAll,uniqueSectors[[indexK]]]&&indexI=!=indexK,
			mapAndSubRelationMatrix[[indexI,indexK]]=1
		]
	]
]


PrintAndLog["\t Finding top and bottom sectors..."];
topSectors=uniqueSectors[[
	Select[Range[Length[uniqueSectors]],Union[mapAndSubRelationMatrix[[All,#]]]==={0}&]
]]
bottomSectors=uniqueSectors[[
	Select[Range[Length[uniqueSectors]],Union[mapAndSubRelationMatrix[[#,All]]]==={0}&]
]]
topSectors=SortBy[topSectors,SectorOrdering]
bottomSectors=SortBy[bottomSectors,SectorOrdering]
Export[outputPath<>"tmp/topSectors.txt",topSectors//InputForm//ToString]
Export[outputPath<>"tmp/bottomSectors.txt",bottomSectors//InputForm//ToString]
spanningCuts=SectorIndex/@bottomSectors
Export[outputPath<>"tmp/spanningCuts.txt",spanningCuts//InputForm//ToString]


PrintAndLog["\t Building trees..."];

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
Export[outputPath<>"tmp/superOrSourceSectors.txt",superOrSourceSectors//InputForm//ToString]
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


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


PrintAndLog["Creating reduction tasks."]
timer=AbsoluteTime[];
For[i=1,i<=Length[uniqueSectors],i++,
	currentSecNum=SectorNumber[uniqueSectors[[i]]];
	reductionTasksFolderForSector[currentSecNum]=reductionTasksFolder<>ToString[currentSecNum]<>"/";
	(*Run["rm -rf "<>reductionTasksFolderForSector[currentSecNum]];*)
	If[!DirectoryQ[#],Run["mkdir "<>#]]&[reductionTasksFolderForSector[currentSecNum]];
	Export[reductionTasksFolderForSector[currentSecNum]<>"-1.txt",Select[ReductionTargets,Sector[#]===uniqueSectors[[i]]&]//InputForm//ToString]
]
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


PrintAndLog["Initializing mission status..."];
timer=AbsoluteTime[];
Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","WaitingSupersectors"//InputForm//ToString]&/@uniqueSectors;
Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","ComputationFinished"//InputForm//ToString]&/@mappedSectors;
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]



(*Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","ReadyToCompute"//InputForm//ToString]&/@topSectors;*)
(*Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","ComputationFinished"//InputForm//ToString]&/@NonZeroSectors;*)


(*TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]};
SimpleIBP[Verbosity->1,SeedingMethod->"Direct"]//AbsoluteTiming*)





tmpPath=outputPath<>"tmp/";
If[!DirectoryQ[#],Run["mkdir -p "<>#]]&[tmpPath];


If[CutIndices==="spanning cuts",Export[tmpPath<>"spanning_cuts_mode.txt",""]]

Export[tmpPath<>"initialized.txt",""]
PrintAndLog["Initialization Finished."]
