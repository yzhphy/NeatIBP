(* ::Package:: *)

commandLineMode=True





If[commandLineMode,
	packagePath=DirectoryName[$InputFileName];
	If[StringSplit[packagePath,""][[-1]]=!="/",packagePath=packagePath<>"/"];
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
	(*LoopMomenta={l1,l2};
	ExternalMomenta={k1,k2,k4};
	Propagators={l1^2,(l1-k1)^2,(l1-k1-k2)^2,(l2+k1+k2)^2,(l2-k4)^2,l2^2,(l1+l2)^2,(l1+k4)^2,(l2+k1)^2};
	Kinematics={k1^2->0,k2^2->0,k4^2->0,k1 k2->s/2,k1 k4->t/2,k2 k4->(-s/2-t/2)};
	GenericPoint={s->-1,t->-3}; 
	TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]}*)
	
]





(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting.";]Exit[]]
If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting."];Exit[]]
TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting."];Exit[]]


If[CutIndices==="spanning cuts",
	(*Print[
		"!!![Notice]: the config setting CutIndices=\"spanning cuts\" is an out-of-date gramma since v1.0.5.4.\n",
		"It is still supported, but it is recommended to use the equivalent, new gramma: \n",
		"\tCutIndices={};\n",
		"\tSpanningCutsMode=True;"
	];*)(*do not lolososo, bblailai too many times is annoying....*)
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
If[FileExistsQ[tmpPath<>"once_initialized.txt"],
	Export[outputPath<>"tmp/"<>"initialized.txt",""];
]
If[!FileExistsQ[tmpPath<>"initialized.txt"],
	Print["Cannot perform this operation without finishing initialization... Please run this compuation from the beginning."];
	Exit[0];
]


LogPath=outputPath<>"tmp/log_files/"
LogFile=LogPath<>"prepare_for_spanning_cuts"<>".txt";


PrintAndLog["Spanning cuts mode is on.\nPreparering for spanning cuts missions."]
timer=AbsoluteTime[]





SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse


TemporaryDirectory = outputPath<>"tmp/"
(*If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]*)
TemporaryDirectorySingular = TemporaryDirectory<>"singular_temp/"
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectorySingular]





Get[packagePath<>"SyzygyRed.wl"]
(*runningScriptFolder=outputPath<>"tmp/running_scripts/"*)




Prepare[];


spanningCuts=Get[TemporaryDirectory<>"spanningCuts.txt"]
PrintAndLog["\tRead ",Length[spanningCuts]," spanning cuts."]


targetIndexLists=TargetIntegrals/.G->List;
uncuttableIndices=Select[Range[SDim],
	MemberQ[Sign/@(targetIndexLists[[All,#]]-1),1]&
]
If[Length[uncuttableIndices]>0,
	PrintAndLog["\tThe following indices are larger than 1 in certain targets:"];
	PrintAndLog["\t",uncuttableIndices];
	PrintAndLog["\tIn current version of NeatIBP, these indices are not cuttable."];
	PrintAndLog["\tRedefining spanning cuts..."];
	spanningCuts=Sort[Complement[#,uncuttableIndices]]&/@spanningCuts;
	spanningCuts=spanningCuts//DeleteDuplicates;
	newSpanningCuts=spanningCuts;
	For[i=1,i<=Length[spanningCuts],i++,
		For[j=1,j<=Length[spanningCuts],j++,
			If[i==j,Continue[]];
			If[SubsetQ[spanningCuts[[i]],spanningCuts[[j]]],
				newSpanningCuts=DeleteCases[newSpanningCuts,spanningCuts[[i]]]
			]
		]
	];
	spanningCuts=newSpanningCuts;
	Export[TemporaryDirectory<>"spanningCuts.txt",spanningCuts//InputForm//ToString];
	PrintAndLog["\tDone."];
]


ModifiedConfig[file_,cut_]:=Module[{string},
	string=Import[file];
	(*string=StringReplace[string,{"CutIndices="~~Shortest[x__]~~"\n"->"\n"}];*)(*no need and cause bug, see dev log for reason. 2024.10.30*)
	string=string<>"\n\n(*-----actual cuts-----*)\nCutIndices="<>ToString[InputForm[cut]]<>";\n";
	string=string<>"\n\n(*-----turn off spanning cuts mode in individual cuts-----*)\nSpanningCutsMode=False;\n";
	string=string<>"\n\n(*-----label as sub mission-----*)\nIsASpanningCutsSubMission=True;\n";
	string=string<>"\n\n(*-----turn off IBP reduction in individual cuts-----*)\nPerformIBPReduction=False;\n";
	(*actually we can also turn off symmetry here... but, currently not needed.---2024.10.28*)
	(*string//Print;*)
	string
]


(*Export[
	TemporaryDirectory<>"run_cut.sh",
"cd $1
"<>packagePath<>"run.sh
cd -",
	"Text"
]*)
Export[
	TemporaryDirectory<>"run_cut.sh",
""<>packagePath<>"run.sh $1"<>missionInput,
	"Text"
]
Run["chmod +x "<>TemporaryDirectory<>"run_cut.sh"]


(*we may need to recreate spanningCutsMissionMainPath by default, considering what if this is a 2nd time running? *)
Options[PrepareSPC]={KernelDistributionHQ->False}
PrepareSPC[]:=Module[{i,cut,stringTail,cutMissionPath,runAllCutsScript},
	PrintAndLog["Creating spanning cuts missions..."];
	spanningCutsMissionMainPath=TemporaryDirectory<>"spanning_cuts_missions/";
	If[OptionValue[KernelDistributionHQ],
		runAllCutsScript=MathematicaCommand<>" -script "<>packagePath<>"KernelDistributionHQ.wl "<>AbsMissionInput<>" &\n";
	,
		runAllCutsScript="";
	];
	
	If[!DirectoryQ[#],CreateDirectory[#]]&[spanningCutsMissionMainPath];
	For[i=1,i<=Length[spanningCuts],i++,
		cut=spanningCuts[[i]];
		stringTail=StringRiffle[ToString/@cut,"_"];
		cutMissionPath=spanningCutsMissionMainPath<>"cut_"<>stringTail<>"/";
		If[!DirectoryQ[#],CreateDirectory[#]]&[cutMissionPath];
		(*Run["cp "<>workingPath<>missionInput<>" "inputBackupPath<>missionInput];*)
		Export[cutMissionPath<>missionInput,ModifiedConfig[workingPath<>missionInput,cut]];
		Run["cp "<>kinematicsFile<>" "cutMissionPath];
		Run["cp "<>targetIntegralsFile<>" "cutMissionPath];
		If[OptionValue[KernelDistributionHQ],
			Export[cutMissionPath<>"pause.tag","","Text"];
		];
		Switch[SpanningCutsEvaluationMode,
		"Sequential",
			runAllCutsScript=runAllCutsScript<>TemporaryDirectory<>"run_cut.sh "<>cutMissionPath<>"\n";
		,
		"Parallel",
			If[OptionValue[KernelDistributionHQ],
				runAllCutsScript=runAllCutsScript<>packagePath<>"paused_command.sh "<>cutMissionPath<>"pause.tag "<>
					"\""<>TemporaryDirectory<>"run_cut.sh "<>cutMissionPath<>" \"cut mission "<>ToString[InputForm[cut]]<>":\" &\n";
			,
				runAllCutsScript=runAllCutsScript<>TemporaryDirectory<>"run_cut.sh "<>cutMissionPath<>" &\n";
			]
		,
		_,
			PrintAndLog["Unkown evaluation mode, using sequential. "];
			runAllCutsScript=runAllCutsScript<>TemporaryDirectory<>"/run_cut.sh "<>cutMissionPath<>"\n";
		];
	];
	If[SpanningCutsEvaluationMode==="Parallel",runAllCutsScript=runAllCutsScript<>"wait\n"];
	Export[TemporaryDirectory<>"run_all_cuts.sh",runAllCutsScript,"Text"];
	Run["chmod +x "<>TemporaryDirectory<>"run_all_cuts.sh"]
]




If[MathKernelLimit<Infinity&&SpanningCutsEvaluationMode==="Parallel",
	PrepareSPC[KernelDistributionHQ->True]
,
	PrepareSPC[]
]


PrintAndLog["Finished. Time used: ",Round[AbsoluteTime[]-timer]," second(s)."]
