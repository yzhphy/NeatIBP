(* ::Package:: *)

commandLineMode=True


If[commandLineMode,
	
	
	(*workingPath=Directory[]<>"/";*)
	
	packagePath=DirectoryName[$InputFileName];
	AbsMissionInput=$CommandLine[[-1]];
	workingPath=DirectoryName[AbsMissionInput];
	missionInput=FileNameSplit[AbsMissionInput][[-1]];
	
	MathematicaCommand=Import[packagePath<>"/preload/MathematicaCommand.txt"];
	ShellProcessor=Import[packagePath<>"/preload/ShellProcessor.txt"];
	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[]<>"examples/dbox/";
	packagePath=NotebookDirectory[];
	missionInput="inputs_and_config.txt"
	(*LoopMomenta={l1,l2};
	ExternalMomenta={k1,k2,k4};
	Propagators={l1^2,(l1-k1)^2,(l1-k1-k2)^2,(l2+k1+k2)^2,(l2-k4)^2,l2^2,(l1+l2)^2,(l1+k4)^2,(l2+k1)^2};
	Kinematics={k1^2->0,k2^2->0,k4^2->0,k1 k2->s/2,k1 k4->t/2,k2 k4->(-s/2-t/2)};
	GenericPoint={s->-1,t->-3}; 
	TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]}*)
	
]


(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]


If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting."];Exit[]]
If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting."];Exit[]]
TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting."];Exit[]]


If[CutIndices==="spanning cuts",
	(*PrintAndLog[
		"!!![Notice]: the config setting CutIndices=\"spanning cuts\" is an out-of-date gramma since v1.0.5.4.\n",
		"It is still supported, but it is recommended to use the equivalent, new gramma: \n",
		"\tCutIndices={};\n",
		"\tSpanningCutsMode=True;"
	];*)(*not print in this wl*)
	CutIndices={};
	SpanningCutsMode=True;
]


(*getSparseRREF=True
getSparseRREF=<<SparseRREF`*)





If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	
]
If[Intersection[StringSplit[outputPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["echo \"Path "<>outputPath<>" is illegal. Exiting.\""];
	Exit[0];

]
If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


tmpPath=outputPath<>"tmp/"



If[!FileExistsQ[tmpPath<>"initialized.txt"],
	Export[tmpPath<>"initialization_failed.txt",""];
	Print["echo \"Initialization failed, cannot start missions.\""];
	Exit[0];
]




missionStatusFolder=outputPath<>"tmp/mission_status/"


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse


missionStatus={ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex,Get[#]}&/@FileNames[All,missionStatusFolder]





If[!FileExistsQ[tmpPath<>"spanning_cuts_mode.txt"],
	If[DeleteCases[Union[missionStatus[[All,2]]],"ComputationFinished"]==={},
		script="echo \"All mission finished!\"\n";
		,
		script=MathematicaCommand<>" -script "<>packagePath<>"MissionStatusChecker.wl "<>AbsMissionInput<>" | "<>ShellProcessor
	]
,
	script=MathematicaCommand<>" -script "<>packagePath<>"PrepareForSpanningCuts.wl "<>AbsMissionInput<>" \n"<>
			tmpPath<>"run_all_cuts.sh"
]

Print[script]
Run["echo \""<>script<>"\" >> "<>outputPath<>"tmp/log3.txt"]




