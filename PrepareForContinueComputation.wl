(* ::Package:: *)

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
	workingPath=NotebookDirectory[];
	missionInput="example.txt"
	
	
]





(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting."];Exit[]]
If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting."];Exit[]]
(*TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting."];Exit[]]*)


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
	Print[
		"!!![Notice]: the config setting CutIndices=\"spanning cuts\" is an out-of-date gramma since v1.1.0.0.\n",
		"It is still supported, but it is recommended to use the equivalent, new gramma: \n",
		"\tCutIndices={};\n",
		"\tSpanningCutsMode=True;"
	];
	CutIndices={};
	SpanningCutsMode=True;
]


If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	Print["Output path has been set as "<>outputPath]
]
If[Intersection[StringSplit[outputPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["Path "<>outputPath<>" is illegal. Exiting."];
	Exit[0];

]
If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


tmpPath=outputPath<>"tmp/"


If[FileExistsQ[#],DeleteFile[#]]&[tmpPath<>"continue_preparation_failed.tag"]


ReadPs[path_]:=Module[{ps},
	ps=StringSplit[RunProcess[{"ps","-ef"},"StandardOutput"],"\n"];
	(*Join[{ps[[1]]},Select[ps,StringContainsQ[#,path]&]]*)
	pshead=ps[[1]];
	ps=Select[ps,StringContainsQ[#,path]&];
	ps
]


checkTimes=3
While[True,
	For[i=1,i<=checkTimes,i++,
		Print["checking processes (",i,"/",checkTimes,")..."];
		processes=ReadPs[outputPath];
		If[Length[processes]>0,Break[]];
	];
	If[Length[processes]>0,
		Print["******************"];
		Print[pshead];
		Print[StringRiffle[processes,"\n"]];
		Print["******************"];
		Print["The above processes are detected which are relevant to the outputPath ",outputPath];
		Print["Continuing is highly unsecure for possible confilicts. We recommend you abort, kill them by hand, and try again."];
		continueQ=InputString[" Type \"continue\" to continue. Type \"r\" or \"R\" to detect processes again. Type others to abort.\n"];
		If[continueQ==="R",continueQ="r"];
		Switch[continueQ,
		"r",
			Continue[];(*Continue the While loop*)
		,
		"continue",
			
			Break[];(*Break the While loop*)
		,
		_,
			Export[tmpPath<>"continue_preparation_failed.tag","","Text"];
			Exit[0];
		]
	,
	(*else*)
		Print["check passed"];
		Break[];	
	]
]



If[FileExistsQ[tmpPath<>"once_initialized.txt"],
	Export[outputPath<>"tmp/"<>"initialized.txt",""];
]
If[!FileExistsQ[tmpPath<>"initialized.txt"],
	(*this part is not so good, because it differs from a spc sub mission and others, uncomfortable...
	but I donot have time to improve this ---- 2024.12.25
	*)
	If[IsASpanningCutsSubMission,
		If[DirectoryQ[outputPath],
			Print["Cannot continue a computation with an unfinished initialization... Please run this compuation from the beginning."];
			Export[tmpPath<>"continue_preparation_failed.tag","","Text"];
			Exit[0]
		,
			Run[MathematicaCommand<>" -script "<>packagePath<>"Initialization.wl "<>AbsMissionInput];
			Exit[0];
		]
	,
		Print["Cannot continue a computation without initialization... Please run this compuation from the beginning."];
		Export[tmpPath<>"continue_preparation_failed.tag","","Text"];
		Exit[0];
	];
	
]


Print["Preparering for continue computation."]


SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse


TemporaryDirectory = outputPath<>"tmp/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]
TemporaryDirectorySingular = TemporaryDirectory<>"singular_temp/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectorySingular]


If[FileExistsQ[#],Export[#,"0"]]&[TemporaryDirectory<>"start_abs_time.txt"]


Get[packagePath<>"SyzygyRed.wl"]
runningScriptFolder=outputPath<>"tmp/running_scripts/"




Prepare[];


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
(*I do not know if this block is needed, but I feel safer to add it---2024.11.26*)


tmpPath=TemporaryDirectory
If[!FileExistsQ[tmpPath<>"spanning_cuts_mode.txt"],
	missionStatus=((ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex)->Get[#])&/@FileNames[All,missionStatusFolder];
	missionNotFinished=(SortBy[Select[missionStatus,#[[2]]=!="ComputationFinished"&][[All,1]],SectorOrdering]);
	Export[missionStatusFolder<>ToString[SectorNumber[#]]<>".txt","WaitingSupersectors"//InputForm//ToString]&/@missionNotFinished;
,
	
	Export[tmpPath<>"spanning_cuts_behaviour.tag","continue","Text"];
]


Print["Finished."]
