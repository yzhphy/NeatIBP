(* ::Package:: *)

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


AppendTo[$Path,workingPath];
If[Get[missionInput]===$Failed,Print["echo \"Unable to open "<>missionInput<>". Exiting.\""];Exit[]]





getSparseRREF=True
getSparseRREF=<<SparseRREF`


If[Get[missionInput]===$Failed,Print["echo \"Unable to get SparseRREF. Exiting.\""];Exit[]]


outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/"


missionStatusFolder=outputPath<>"tmp/mission_status/"


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse


missionStatus={ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex,Get[#]}&/@FileNames[All,missionStatusFolder]


If[DeleteCases[Union[missionStatus[[All,2]]],"ComputationFinished"]==={},
	script="echo \"All mission finished!\"\n"
	,
	script="math -script MissionStatusChecker.wl "<>missionInput<>" | sh"
]

Print[script]
Run["echo \""<>script<>"\" >> "<>outputPath<>"log3.txt"]

