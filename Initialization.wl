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
by: Janko Boehm, Rourou Ma, Johann Usovitsch, Hefeng Xu, Yingxuan Xu, Zihao Wu and Yang Zhang.
================================================================"];
If[DeveloperMode===True,Print["*** developer mode is on ***"]]
If[DebugMode===True,Print["*** debug mode is on ***"]]



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
If[Get[workingPath<>missionInput]===$Failed,Print["****  Unable to open config file "<>workingPath<>missionInput<>". Exiting."];Exit[]]
If[Get[kinematicsFile]===$Failed,Print["****  Unable to open kinematics file "<>kinematicsFile<>". Exiting."];Exit[]]
TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["****  Unable to open target intergals file "<>targetIntegralsFile<>". Exiting."];Exit[]]





(*renaming the setting, because NeatIBP... actually, dose not perform "reduction" by default*)
If[ValueQ[ReductionOutputName],
	If[ReductionOutputName=!=OutputName,
		If[OutputName==="Untitled",
			ReductionOutputName=ReductionOutputName;
			(*use ReductionOutputName*)
		,
			PrintAndLog[
				"!![Notice]: It seems that you have set ReductionOutputName = \"",
				ReductionOutputName,"\" and OutputName = \"",OutputName,"\"\n",
				"The two settings are the same in NeatIBP v1.1.0.0 or later.\n",
				"Resetting ReductionOutputName = \"",OutputName,"\"."
			];
			Pause[3];
			ReductionOutputName=OutputName
		]
	]
,
	ReductionOutputName=OutputName
]


(* ::Section::Closed:: *)
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

If[FileExistsQ[#],DeleteFile[#]]&[outputPath<>"tmp/"<>"initialization_failed.txt"]
If[FileExistsQ[outputPath<>"tmp/"<>"initialized.txt"],
	Export[outputPath<>"tmp/"<>"once_initialized.txt",""];
	Run["rm "<>outputPath<>"tmp/"<>"initialized.txt"]
];(*here once means cengjing initialized in the past*)




finishedTagFile="results/NeatIBP_finished.tag"


(*FinishedOutputPathQ[outputPath_]:=Module[{subDirs,IBPallExistQs},
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
	
]*)
FinishedOutputPathQ[outputPath_]:=FileExistsQ[outputPath<>finishedTagFile](* this tag file will be created in AssignIBPReduction.wl after all finished.*)


If[And[DirectoryQ[outputPath],automaticOutputPath],
	
	continueQ=InputString["*****************************\nOutput directory \""<>outputPath<>"\" already exists. Do you want to delete it? Type Y or y to continue. Type others to abort.\n"];
	
	If[Or[continueQ=="y",continueQ=="Y"],
		If[!FinishedOutputPathQ[outputPath],
			Print["*****************************\nOutput directory "<>outputPath<>" is not a NeatIBP output directory with a finished tag.\nIt could be: "];
			Print["\t1. an output directory of a NeatIBP mission which is running right now, or"];
			Print["\t2. an output directory of an unfinished NeatIBP mission in the past, or"];
			Print["\t3. an output directory generated by NeatIBP v1.0.5.2 (at 14th Oct. 2024) or earlier versions."];
			Print["It is STRONGLLY recommended to make sure that it is not the first case. Otherwise, there will be conflict and unknown errors will occur."];
			Print["If it is not the first case, it is safe to delete "<>outputPath<>"."];
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


ErrorLine[]:=PrintAndLog["******************************************************************"];


(* ::Subsection:: *)
(*Variable Name Protection*)


ProtectedNames=
{m,x,y,z,G,d,n,L,zeroLoopMomenta,Momenta,ScalarVarRep,var,ScalarVar,BaikovMatrix,SDim,GramMatrix,LoopExternalScalars,ScalarTangentSet,
ScalarExtendedTangentSet,BaikovKernelScalar,BaikovRevRep,BaikovRep,BaikovKernel,Parameters,TangentSet,ExtendedTangentSet,
ForwardRep,BackwardRep,Scalar2sp,sp2Scalar,sp,PolynomialU,PolynomialF,PolynomialG,numericPolynomialG,gen,varOrder,ss,(*in SyzygyRed.wl, IntegerPartition function. I think this variable, ss, can be set as local*)
ZeroSectors,NonZeroSectors,ZeroTargets,ReductionTargets,ReductionTasks,ZeroSectorRemoval,IBPList,MIList,SectorAnalyzeTiming,IntegralR,FI,BasicRawIBPs,FI0,ZM0,secNum,SelfSymmetryR,ZM,RelavantIntegrals,
groupMomentumU,groupMomentumV,StdL,i,j,k,spanningCuts,bottomSectors,topSectors,spanningCutsMissionMainPath,TemporaryDirectory,
Prepare,SectorwiseSettingListForCurrentSector,inputParameters,FI,ZM,FI0,ZM0,FIHead,ZMHead,FI00,ZM00,pshead,processes,checkTimes,
spanningCuts,spanningCut,cut,outputPathsWithUnfinishedIni,ReductionOutputName,OutputName

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
			ErrorLine[];
			PrintAndLog["****  Protected symbol \"",name,"\" appear in the following input(s):\n\t", checkFreeResult];
			allProtectedNamesDisappearQ=False;
		]
	]
]
If[allProtectedNamesDisappearQ===False,
	ErrorLine[];
	PrintAndLog["****  Please raname the above symbol(s) in your input(s). Exiting..."];
	ErrorLine[];
	Exit[0];
]


(* ::Subsection:: *)
(*spanning cuts terminology*)


If[CutIndices==="spanning cuts",
	PrintAndLog[
		"!!![Notice]: the config setting CutIndices=\"spanning cuts\" is an out-of-date gramma since v1.1.0.0.\n",
		"It is still supported, but it is recommended to use the equivalent, new gramma: \n",
		"\tCutIndices={};\n",
		"\tSpanningCutsMode=True;"
	];
	CutIndices={};
	SpanningCutsMode=True;
	Pause[3];
]


(* ::Subsection:: *)
(*Other Validations*)


OptionCheck[settingStr_,optionList_]:=Module[{settingValue=ToExpression[settingStr]},
	If[!MemberQ[optionList,settingValue],
		ErrorLine[];
		PrintAndLog["****** Wrong setting ",settingStr,"=",settingValue//InputForm//ToString,". It should be one of the following: ",optionList//InputForm//ToString,"."];
		ErrorLine[];
		Exit[0]
	]
]


If[ValueQ[FurtherSyzygyVectorsSelectionStricty],
	ErrorLine[];
	PrintAndLog[
		"*** Sorry, it seem that you have set FurtherSyzygyVectorsSelectionStricty=",
		FurtherSyzygyVectorsSelectionStricty,
		". In the version v1.1.0.0 and later, this setting is renamed to FurtherSyzygyVectorsSelectionStrictness. ",
		"Please use FurtherSyzygyVectorsSelectionStrictness=",FurtherSyzygyVectorsSelectionStricty," instead. Apology for the inconvenience."
	];
	ErrorLine[];
	Exit[0];

]
If[ValueQ[NeatIBPIntersectionTimeConstrain],
	ErrorLine[];
	PrintAndLog[
		"*** Sorry, it seem that you have set NeatIBPIntersectionTimeConstrain=",
		NeatIBPIntersectionTimeConstrain,
		". In the version v1.1.0.1 and later, this setting is renamed to NeatIBPIntersectionTimeConstrainForFlexibleDegreeBound. ",
		"Please use NeatIBPIntersectionTimeConstrainForFlexibleDegreeBound=",NeatIBPIntersectionTimeConstrain," instead. Apology for the inconvenience."
	];
	ErrorLine[];
	Exit[0];

]
If[And[NeatIBPIntersectionDegreeBound<=0,FlexibleNeatIBPIntersectionDegreeBound],
	ErrorLine[];
	PrintAndLog[
		"Cannot set NeatIBPIntersectionDegreeBound=",NeatIBPIntersectionDegreeBound,
		" when FlexibleNeatIBPIntersectionDegreeBound=True. \nPlease set NeatIBPIntersectionDegreeBound to be a positive integer."
	];
	ErrorLine[];
	Exit[0];
]


If[ValueQ[LiftSelectionStricty],
	ErrorLine[];
	PrintAndLog[
		"*** Sorry, it seem that you have set LiftSelectionStricty=",
		LiftSelectionStricty,
		". In the version v1.1.0.0 and later, this setting is renamed to LiftSelectionStrictness. ",
		"Please use LiftSelectionStrictness=",LiftSelectionStricty," instead. Apology for the inconvenience."
	];
	ErrorLine[];
	Exit[0];

]


If[ToString[debugMode]=!="debugMode",
	ErrorLine[];
	PrintAndLog["*** It seems that you have set debugMode=",debugMode,". In the version v1.1.0.0 and later, please use DebugMode=",debugMode," (capital D) instead. Sorry for the inconvenience."];
	ErrorLine[];
	Exit[0];
]


inputParameters=Complement[Variables[{Propagators,Kinematics[[All,2]]}],LoopMomenta,ExternalMomenta];
If[!SubsetQ[GenericPoint[[All,1]],inputParameters],
	ErrorLine[];
	PrintAndLog[
		"****  GenericPoint dose not cover all scalar variables. Not covered variables: ",
		Complement[inputParameters,GenericPoint[[All,1]]],
		". Exiting..."
	];
	ErrorLine[];
	Exit[0];
]
If[Variables[inputParameters/.GenericPoint]=!={},
	ErrorLine[];
	PrintAndLog["****  GenericPoint ",GenericPoint," is not properly defined. Exiting..."];
	ErrorLine[];
	Exit[0];
]
If[Variables[d/.GenericD]=!={},
	ErrorLine[];
	PrintAndLog["****  GenericD ",GenericD," is not defined or not properly defined. Exiting..."];
	ErrorLine[];
	Exit[0];
]
If[ParameterRepermute===True,
	If[Sort[ParameterRepermuteIndex]=!=Range[Length[inputParameters]],
		ErrorLine[];
		PrintAndLog["****  ParameterRepermuteIndex ",ParameterRepermuteIndex," is not a well-defined permutation with length ",Length[inputParameters],". Exiting..."];
		ErrorLine[];
		Exit[0];
	]
]


If[Variables[TensorProduct[ExternalMomenta,ExternalMomenta]/.Kinematics/.GenericPoint]=!={},
	ErrorLine[];
	PrintAndLog["****  Kinematics replacement ",Kinematics," is not enough for external momenta ",ExternalMomenta,". Exiting..."];
	ErrorLine[];
	Exit[0];
]


If[Head[CutIndices]=!=List(*&&!MemberQ[{"spanning cuts"},CutIndices]*),
	ErrorLine[];
	PrintAndLog["****  Unexpected cut indices ",CutIndices,". Exiting..."];
	ErrorLine[];
	Exit[0]
]


(*If[And[MemberQ[{"spanning cuts"},CutIndices],Not[DeveloperMode===True]],
	PrintAndLog["****  In the current version, spanning cuts mode is under developement. There may lurks unknown bugs. Please set DeveloperMode=True if you want to try. Exiting..."];
	Exit[0]
]*)
(*If[And[FlexibleNeatIBPIntersectionDegreeBound,Not[DeveloperMode===True]],
	ErrorLine[];
	PrintAndLog["****  In current version, FlexibleNeatIBPIntersectionDegreeBound is under developement. There may lurks unknown bugs. Please set DeveloperMode=True if you want to try. Exiting..."];
	ErrorLine[];
	Exit[0]
]*)

If[And[Not[kinematicsFile===workingPath<>"kinematics.txt"],Not[DeveloperMode===True]],
	ErrorLine[];
	PrintAndLog["****  Sorry, in current version, we do not recommend non-default value of kinematicsFile. We recommend that you keep it as workingPath<>\"kinematics.txt\". "];
	PrintAndLog["****  If you really want to use this non-default file name, please set DeveloperMode=True, then run again."];
	PrintAndLog["****  But, we need to tell you that, this may bring disfunctioning of some of the modules of NeatIBP."];
	ErrorLine[];
	Exit[0]
]

If[And[Not[targetIntegralsFile===workingPath<>"targetIntegrals.txt"],Not[DeveloperMode===True]],
	ErrorLine[];
	PrintAndLog["****  Sorry, in current version, we do not recommend non-default value of targetIntegralsFile. We recommend that you keep it as workingPath<>\"targetIntegrals.txt\". "];
	PrintAndLog["****  If you really want to use this non-default file name, please set DeveloperMode=True, then run again."];
	PrintAndLog["****  But, we need to tell you that, this may bring disfunctioning of some of the modules of NeatIBP."];
	ErrorLine[];
	Exit[0]
]


(*If[And[SimplifySyzygyVectorsByCut,Not[DebugMode===True]],
	PrintAndLog["****  In current version, SimplifySyzygyVectorsByCut is under developement. There may lurks unknown bugs. Please set DebugMode=True if you want to try. Exiting..."];
	Exit[0]
]
*)




If[CutIndices=!={}&&NeedSymmetry,
	ErrorLine[];
	PrintAndLog["****  Please turn off symmetry if there is any cut indices. Exiting..."];
	ErrorLine[];
	Exit[0]
]
If[SpanningCutsMode===True&&NeedSymmetry,
	ErrorLine[];
	PrintAndLog["****  Please turn off symmetry in spanning cuts mode. Exiting..."];
	ErrorLine[];
	Exit[0]
]



If[
(*(CutIndices==="spanning cuts")*)(SpanningCutsMode===True)&&(automaticOutputPath===False),
	ErrorLine[];
	PrintAndLog["****  In spanning cuts mode, outputPath must be set to be Automatic. Exiting..."];(*I think this is unecessary, we can modify the config for the cut missions*)
	ErrorLine[];
	Exit[0]
]


If[IntegralOrder =!= "MultiplePropagatorElimination"&&MIFromAzuritino,
	ErrorLine[];
	PrintAndLog["****  IntegralOrder must be set as \"MultiplePropagatorElimination\" if MIFromArzuritino is enabled. Exiting..."];
	ErrorLine[];
	Exit[0]
]


If[FiniteFieldModulus>46337,
	ErrorLine[];
	PrintAndLog["****  FiniteFieldModulus must not be larger than 46337. Exiting..."];
	ErrorLine[];
	Exit[0]
]
If[!PrimeQ[FiniteFieldModulus],
	ErrorLine[];
	PrintAndLog["****  FiniteFieldModulus ",FiniteFieldModulus ," is not a prime number. Exiting..."];
	ErrorLine[];
	Exit[0]
]



(*If[!MemberQ[{"MultiplePropagatorElimination","ISPElimination","Global"},IntegralOrder],
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
If[!MemberQ[{"None","Kira","FFNumerical"},IBPReductionMethod],
	PrintAndLog["****  Invalid IBPReductionMethod \""<>ToString[IBPReductionMethod]<>"\". Exiting..."];
	Exit[0]
]*)
OptionCheck["IntegralOrder",{"MultiplePropagatorElimination","ISPElimination","Global"}]
OptionCheck["ExternalMomentaGroupingMethod",{"MomentumSpace","FeynmanParameterization"}]
OptionCheck["PreferedExternalExtendedRotationMethod",{"Orthogonalization","DeltaPlaneProjection"}]
OptionCheck["IBPReductionMethod",{"None","Kira","FFNumerical"}]


(*If[!MemberQ[{"Zurich","Zurich+FIBPGrouping"},NeatIBPSeedingMethod],
	PrintAndLog["****  Invalid IntegralOrder \""<>ToString[IntegralOrder]<>"\". Exiting..."];
	Exit[0]
]
]*)
OptionCheck["NeatIBPSeedingMethod",{"Zurich","Zurich+FIBPGrouping"}]



If[FIBPGroupingRemainingGroups<1||Head[FIBPGroupingRemainingGroups]=!=Integer,
	ErrorLine[];
	PrintAndLog["****  FIBPGroupingRemainingGroups must be a positive integer. Exiting..."];
	ErrorLine[];
	Exit[0];
]
If[FIBPGroupingStartRatio>1||FIBPGroupingStartRatio<0,
	ErrorLine[];
	PrintAndLog["****  FIBPGroupingStartRatio must be between 0 and 1. Exiting..."];
	ErrorLine[];
	Exit[0]
]


(*If[NeedSymmetry&&MIFromAzuritino,
	PrintAndLog["****  Sorry, Azuritino dose not support symmetry in the current version.\n We are working on it and soon it will come.\n Exiting..."];
	Exit[0]
]*)


CutableQ[integral_,cut_]:=!MemberQ[Union[Sign/@((List@@integral[[cut]])-1)],1](* index that are cut must \[LessEqual] 1*)
CuttedQ[integral_,cut_]:=MemberQ[Union[Sign/@((List@@integral[[cut]])-1)],-1](* index that are cut <1 then return True*)


If[(*CutIndices=!="spanning cuts"*)True,
(*this layer of If, I think, is just for making sure that we do not meet string[[i]],
just a gramma problem.
In new gramma of spc, this is not longer a problem.
---2024.10.26
  *)
	If[MemberQ[CutableQ[#,CutIndices]&/@TargetIntegrals,False],
		ErrorLine[];
		PrintAndLog["****  Sorry, this version dose not support cutting indices larger than 1. Please remove corresponding target integrals with such multiple propagators.\nExiting..."];
		ErrorLine[];
		Exit[0];
	]
]








preparation=Prepare[];
If[preparation==="Propagators Length and SDim mismatch.",
	ErrorLine[];
	PrintAndLog["*** The length of the Propagators ",Propagators," ",Length[Propagators]," and SDim=L(L+1)/2+LE=",SDim," mismatch. Exiting."];
	ErrorLine[];
	Exit[0];
]
If[preparation==="Propagators is not a well-defined independent basis.",
	ErrorLine[];
	PrintAndLog["*** The Propagators ",Propagators," is not a well-defined independent basis. Exiting."];
	ErrorLine[];
	Exit[0];
]


If[Union[(Length[Propagators]===Length[#])&/@TargetIntegrals]=!={True},
	ErrorLine[];
	PrintAndLog["****  Length of Propagators and indices of TargetIntegrals mismatch. Exiting..."];
	ErrorLine[];
	Exit[0];
]


If[SpanningCutsMode===True&&ShortenSpanningCutsIBPs===True&&SpanningCutsConsistencyCheck=!=True,
	ErrorLine[];
	PrintAndLog["****  Wrong setting: In spanning cuts mode, if you set ShortenSpanningCutsIBPs=True, you must also set SpanningCutsConsistencyCheck=True."];
	ErrorLine[];
	Exit[0];
]


(*If[WorkerAllocationSystem=!=And[MathKernelLimit<Infinity,SpanningCutsMode],
	PrintAndLog[
		"****  Wrong WorkerAllocationSystem: ",
		WorkerAllocationSystem,". Required: ",
		And[MathKernelLimit<Infinity,SpanningCutsMode],
		"\nBecause: MathKernelLimit="MathKernelLimit,"and SpanningCutsMode=",SpanningCutsMode
	];
	Exit[0];
]*)


If[MathKernelLimit<3,
	ErrorLine[];
	PrintAndLog["****  Too-few MathKernelLimit: ",MathKernelLimit, ". MathKernelLimit should be at least 3."];
	ErrorLine[];
	Exit[0];
]
If[And[!IntegerQ[MathKernelLimit],MathKernelLimit=!=Infinity],
	ErrorLine[];
	PrintAndLog["**** Wrong MathKernelLimit: ",MathKernelLimit, ". MathKernelLimit must be an integer or Infinity."];
	ErrorLine[];
	Exit[0];
]
If[MaxManagers<1,
	ErrorLine[];
	PrintAndLog["****  Too-few MaxManagers: ",MaxManagers, ". MaxManagers should be at least 1."];
	ErrorLine[];
	Exit[0];
]
If[MathKernelLimit<2*MaxManagers+1,
	ErrorLine[];
	PrintAndLog["****  Too-large MaxManagers: ",MaxManagers, ". MaxManagers should be no greater than (MathKernelLimit-1)/2=",(MathKernelLimit-1)/2];
	ErrorLine[];
	Exit[0];
]



If[And[!IntegerQ[SPCIBPReductionParallelJobNumber],SPCIBPReductionParallelJobNumber=!=Infinity]||SPCIBPReductionParallelJobNumber<=0,
	ErrorLine[];
	PrintAndLog["**** Wrong SPCIBPReductionParallelJobNumber: ",SPCIBPReductionParallelJobNumber, ". SPCIBPReductionParallelJobNumber must be a positive integer or Infinity."];
	ErrorLine[];
	Exit[0];
]
If[And[!IntegerQ[ConsistencyCheckParallelJobNumber],ConsistencyCheckParallelJobNumber=!=Infinity]||ConsistencyCheckParallelJobNumber<=0,
	ErrorLine[];
	PrintAndLog["**** Wrong ConsistencyCheckParallelJobNumber: ",ConsistencyCheckParallelJobNumber, ". ConsistencyCheckParallelJobNumber must be a positive integer or Infinity."];
	ErrorLine[];
	Exit[0];
]


If[Not[UseGNUParallel===True],
	If[ConsistencyCheckParallelizationMethod==="GNUParallel",
		ErrorLine[];
		PrintAndLog["**** You have not set UseGNUParallel=True, so you cannot set ConsistencyCheckParallelizationMethod=\"GUNParallel\";"];
		ErrorLine[];
		Exit[0];
	];
	If[SPCIBPReductionParallelizationMethod==="GNUParallel",
		ErrorLine[];
		PrintAndLog["**** You have not set UseGNUParallel=True, so you cannot set SPCIBPReductionParallelizationMethod=\"GUNParallel\";"];
		ErrorLine[];
		Exit[0];
	]
,
	If[RunProcess[{"which",GNUParallelCommand},"StandardOutput"]==="",
		ErrorLine[];
		PrintAndLog["**** The GNUParallelCommand ",GNUParallelCommand," not found using: which ",GNUParallelCommand];
		ErrorLine[];
		Exit[0];
	]
]



OptionCheck["SPCIBPReductionParallelizationMethod",{"Naive","GNUParallel"}]
OptionCheck["ConsistencyCheckParallelizationMethod",{"Naive","GNUParallel"}]



If[SimplifyByCutMethod==="LiftResubstitution"&&(!(DeveloperMode===True)),
	ErrorLine[];
	PrintAndLog["In this version, LiftResubstitution is no longer an option of SimplifyByCutMethod recommended for users. If you want to use it, please set DeveloperMode=True."];
	PrintAndLog["Developer tips: the consistency between LiftResubstitution and [AdditionalIBPs] is not yet developed, and possibly will not be developed. "];
	ErrorLine[];
	Exit[];
]




OptionCheck["SpanningCutsEvaluationMode",{"Sequential","Parallel"}]





OptionCheck["UseShortenedSpanningCutsIBPs",{True,False,Automatic}]
OptionCheck["ShortenSpanningCutsIBPs",{True,False}]
If[UseShortenedSpanningCutsIBPs===True&&ShortenSpanningCutsIBPs===False,
	ErrorLine[];
	PrintAndLog["**** You have set ShortenSpanningCutsIBPs=False, so you cannot set UseShortenedSpanningCutsIBPs=True. Exiting."];
	ErrorLine[];
	Exit[];
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


If[MathKernelLimit<Infinity&&IsASpanningCutsSubMission,
	If[!DirectoryQ[#],CreateDirectory[#]]&[TemporaryDirectory<>"worker_kernels/"];
	If[!DirectoryQ[#],CreateDirectory[#]]&[TemporaryDirectory<>"worker_kernels/recieved_kernels/"];
	If[!DirectoryQ[#],CreateDirectory[#]]&[TemporaryDirectory<>"worker_kernels/occupied_kernels/"];
	If[!DirectoryQ[#],CreateDirectory[#]]&[TemporaryDirectory<>"worker_kernels/submitting_kernels/"];
]


(* ::Section:: *)
(*The rest steps*)


If[CutIndices=!={}&&(*CutIndices=!="spanning cuts"*)(*(SpanningCutsMode=!=True)*)True,
(*
2024.10.29:
shall we ask SpanningCutsMode=!=True?
because we want IBPs such that all 'cutted' integrals=0 be distributed into results/ folder of each cuts, 
not the results/ folder root path of spc.
Oh but this is actually not troublesome, because in sub spc folders, the targetIntegrals are full target lists.
(which is to say, in spanning cuts mode, initialization only export the list of cuts (and some tags), other outputs are useless.)
in order to keep the same as the following codes, we remove this requirement.
*)
	
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

If[CutIndices=!={}&&(*CutIndices=!="spanning cuts"*)(*(SpanningCutsMode=!=True)*)True,
(*it seems that we should not ask SpanningCutsMode=!=True here
so I modified it as True
*)
	RelavantSectors=Select[RelavantSectors,!CuttedQ[#,CutIndices]&]
];


ZeroSectors=Select[RelavantSectors,ZeroSectorQ];
NonZeroSectors=SortBy[Complement[RelavantSectors,Global`ZeroSectors],SectorOrdering]//Reverse;
PrintAndLog["\t",Length[NonZeroSectors]," non-zero sector(s) are found."];
ZeroTargets=Select[Global`TargetIntegrals,MemberQ[Global`ZeroSectors,Sector[#]]&];
ReductionTargets=Complement[Global`TargetIntegrals,Global`ZeroTargets];
ReductionTasks=Association[Table[NonZeroSectors[[i]]->Select[ReductionTargets,NonZeroSectors[[i]]==Sector[#]&],{i,1,Length[NonZeroSectors]}]];
ZeroSectorRemoval=SectorElimination/@ZeroSectors;(*what for?*)
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
	(*If[MathKernelLimit<Infinity,];*)
	(*
	2024.11.11:
		It seems that we should worry about the parallelization in finding symmetries in spanning cuts mode,
		where there are many Ini.wl running and if 2 ini runs this part of code in the same time
		the MathKernelLimit will be exceeded.
		But actually we need not to worry, as long as NeedSymmetry is not supported in SpanningCutsMode
	*)
	
	{uniqueSectors,mappedSectors,sectorMaps}=SectorMaps[NonZeroSectors];
	
]
Export[outputPath<>"tmp/sectorMaps.txt",sectorMaps//InputForm//ToString]
PrintAndLog["\t",Length[mappedSectors]," mapped sector(s) found."];
PrintAndLog["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). "]



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


If[(*CutIndices==="spanning cuts"*)(SpanningCutsMode===True),
	PrintAndLog["Spanning cuts mode."];
	Export[tmpPath<>"spanning_cuts_mode.txt",""];
	Export[tmpPath<>"spanning_cuts_behaviour.tag","run","Text"];
]

Export[tmpPath<>"initialized.txt",""]
PrintAndLog["Initialization Finished."]
