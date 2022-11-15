(* ::Package:: *)

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
If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting.";Exit[]]]
If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting.";Exit[]]]
(*TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting.";Exit[]]]*)


If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	Print["Output path has been set as "<>outputPath]
]


Print["Preparering for continue computation."]


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse


TemporaryDirectory = outputPath<>"tmp/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]
TemporaryDirectorySingular = TemporaryDirectory<>"singular_temp/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectorySingular]

Get[packagePath<>"SyzygyRed.wl"]
runningScriptFolder=outputPath<>"tmp/running_scripts/"




Prepare[];


missionStatusFolder=TemporaryDirectory<>"mission_status/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[missionStatusFolder]
reductionTasksFolder=TemporaryDirectory<>"reduction_tasks/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[reductionTasksFolder]


SectorNumberToSectorIndex[1002]


missionStatus=((ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex)->Get[#])&/@FileNames[All,missionStatusFolder];
missionNotFinished=(SortBy[Select[missionStatus,#[[2]]=!="ComputationFinished"&][[All,1]],SectorOrdering]);


Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","WaitingSupersectors"//InputForm//ToString]&/@missionNotFinished;



(*TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]};
SimpleIBP[Verbosity->1,SeedingMethod->"Direct"]//AbsoluteTiming*)


Print["Finished."]
