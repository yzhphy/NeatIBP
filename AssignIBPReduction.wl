(* ::Package:: *)

commandLineMode=True





If[commandLineMode,
	
	(*workingPath=$CommandLine[[-1]];(*we can specify working path in command line*)
	If[workingPath==="-script",workingPath=Directory[]<>"/"];
	If[!DirectoryQ[workingPath],
		Print["Warning: the argument working path ",workingPath," does not exist."];
		workingPath=Directory[]<>"/";
		Print["\t\t redefining working path as current working folder: ",workingPath,"."];
	];*)
	packagePath=DirectoryName[$InputFileName];
	AbsMissionInput=$CommandLine[[-1]];
	workingPath=DirectoryName[AbsMissionInput];(*is this really used?*)
	missionInput=FileNameSplit[AbsMissionInput][[-1]];
	
	
	MathematicaCommand=Import[packagePath<>"/preload/MathematicaCommand.txt"];
	ShellProcessor=Import[packagePath<>"/preload/ShellProcessor.txt"];
	EnvVarSetter=Import[packagePath<>"/preload/EnvVarSetter.txt"];
	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";
	outputPath="";
	
]








(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]


If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting."];Exit[]]


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


If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	
]
If[Intersection[StringSplit[outputPath,""],{" ","\t","\n","?","@","#","$","*","&","(",")","\"","\'","|"}]=!={},
	Print["echo \"Path "<>outputPath<>" is illegal. Exiting.\""];
	Exit[0];

]
If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


SetDirectory[workingPath]


TimeString[]:=Module[{at},at=FromAbsoluteTime[AbsoluteTime[]];StringRiffle[#,"_"]&[(ToString[Floor[#]]&/@at[[1,1;;6]])]<>"_"<>ToString[Floor[1000*(#-Floor[#])&[ at[[1,6]]]]]]


(*
This function appears in many codes
see in SyzygyRed.wl for where they are
If you want to modifie this code, remember to modify all of them!
*)
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



LogPath=outputPath<>"tmp/log_files/"
If[!DirectoryQ[LogPath],CreateDirectory[LogPath]];
LogFile=LogPath<>"AssignIBPReduction.txt"


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


finishedTagFile="results/NeatIBP_finished.tag"
If[PerformIBPReduction=!=True,
	(*Export[outputPath<>finishedTagFile,"","Text"];*)
	(*moved this part to LaunchIBPReduction.wl*)
	Exit[];
]





(*If[FileExistsQ[outputPath<>"tmp/once_summarized.tag"],
	Export[outputPath<>"tmp/summarized.tag","","Text"]
]*)
If[!FileExistsQ[outputPath<>"tmp/summarized.tag"],
	Export[
		outputPath<>"tmp/assigned_reduction_script.sh",
		"echo \"Did not found "<>outputPath<>"tmp/summarized.tag"<>". Possibly NeatIBP did not finish properly. Give up performing IBP reduction.\"",
		"Text"
	];
	Exit[0];
]





(*If[!FileExistsQ[outputPath<>"tmp/IBP_reduction_permission.tag"],
	Export[
		outputPath<>"tmp/assigned_reduction_script.sh",
		"echo \"Did not found permission tag from Summary.wl. Possibly NeatIBP did not finish properly. Give up performing IBP reduction.\"",
		"Text"
	];
	Exit[];
]
*)


If[(*CutIndices==="spanning cuts"*)SpanningCutsMode===True,
	If[
		DirectoryQ[outputPath<>"results/results_spanning_cuts/"]&&
		FileExistsQ[outputPath<>"tmp/spanning_cuts_mode.txt"]
	,
		mode="spanning cuts";
	,
		PrintAndLog[outputPath," does not look like a spanning cuts folder. Failed."];
		Exit[];
	]
,
	mode="normal";
]


shortenFinishQScript="

if [ ! -e "<>outputPath<>"tmp/spanning_cuts_IBP_shorten_finished.txt ]
then
	echo \"*** Err: Spanning cuts shorten not finised in \"\""<>outputPath<>"\"
	echo \"Cannot run kira reduction on them.\"
	echo \"Exiting.\"
	exit 1
fi

"


Switch[mode,
"normal",
	Switch[IBPReductionMethod,
	"None",
		script="echo \"IBPReductionMethod=None, will not perform IBP reduction.\"\n";
	,
	"Kira",
	(*if we want to modify this part of code, do not forget to modify that in spanning cuts mode too!*)
		KiraCommandRefined=StringRiffle[
			DeleteCases[
				StringSplit[KiraCommand," "]
			,""]
		," "];
		If[!FileExistsQ[FermatPath],
			PrintAndLog["***File at ",FermatPath," does not exist. Exiting."];
			Exit[];
		];
		If[FileNameSplit[FermatPath][[-1]]=!="fer64",
			PrintAndLog["***FermatPath ",FermatPath," should be specified to \"fer64\". Exiting."];
			Exit[];
		];
		If[Count[
			FileExistsQ/@Select[
				StringSplit[KiraCommandRefined," "],
				FileNameSplit[#][[-1]]==="kira"&
			]
		,True]<1,
			PrintAndLog["***KiraCommand \"",KiraCommand,"\" is not an correct kira command. Please check. Exiting."];
			Exit[];
		];(*kira command may include some settings*)
		Switch[EnvVarSetter,
		"export",
			script="export FERMATPATH=\""<>FermatPath<>"\"\n";
			(*script=script<>"\n"<>"export KiraCommand=\""<>KiraCommand<>"\"";*)
		,
		"setenv",
			script="setenv FERMATPATH \""<>FermatPath<>"\"\n";
			(*script=script<>"\n"<>"setenv KiraCommand \""<>KiraCommand<>"\"";*)
		,
		_,
			PrintAndLog[" ***EnvVarSetter \"",EnvVarSetter,"\" is not supported yet."];
			PrintAndLog["Valid values:"];
			PrintAndLog["\t1. export"];
			PrintAndLog["\t2. setenv"];
			PrintAndLog["Please check the spelling or contact the developers of NeatIBP."];
			PrintAndLog["Exiting."];
			Exit[];
		];
		If[ForceRemoveExistingKiraResults===True,
			kiraScriptSetting=""
		,
			kiraScriptSetting="-f "
		];
		script=script<>"\n"<>ShellProcessor<>" "<>packagePath<>
			"interfaces/Kira/interface/run_kira_reduction.sh"<>" "<>
			kiraScriptSetting<>"\""<>KiraCommandRefined<>"\" "<>outputPath<>"\n";
	,
	"FFNumerical",
		script=MathematicaCommand<>" "<>packagePath<>"FFSolveIBP.wl "<>outputPath<>"\n";
	,
	_,
		(*this should have been excluded by initialization, but... better double checked*)
		PrintAndLog["***Err: Unkown IBPReductionMethod ",IBPReductionMethod];(*we'd better use PrintAndLog. 2024.10.26: yes, we used.*)
		Exit[];
	];
,
"spanning cuts",(*here "spanning cuts" is refering to mode, not CutIndices, so it is kept here --- 2024.10.26*)
	Switch[IBPReductionMethod,
	"None",
		script="echo \"IBPReductionMethod=None, will not perform IBP reduction.\"\n";
	,
	"Kira",
		(*if we want to modify this part of code, do not forget to modify that in normal mode too!*)
		KiraCommandRefined=StringRiffle[
			DeleteCases[
				StringSplit[KiraCommand," "]
			,""]
		," "];
		If[!FileExistsQ[FermatPath],
			PrintAndLog["***File at ",FermatPath," does not exist. Exiting."];
			Exit[];
		];
		If[FileNameSplit[FermatPath][[-1]]=!="fer64",
			PrintAndLog["***FermatPath ",FermatPath," should be specified to \"fer64\". Exiting."];
			Exit[];
		];
		If[Count[
			FileExistsQ/@Select[
				StringSplit[KiraCommandRefined," "],
				FileNameSplit[#][[-1]]==="kira"&
			]
		,True]<1,
			PrintAndLog["***KiraCommand \"",KiraCommand,"\" is not an correct kira command. Please check. Exiting."];
			Exit[];
		];(*kira command may include some settings*)
		Switch[EnvVarSetter,
		"export",
			script="export FERMATPATH=\""<>FermatPath<>"\"\n";
			(*script=script<>"\n"<>"export KiraCommand=\""<>KiraCommand<>"\"";*)
		,
		"setenv",
			script="setenv FERMATPATH \""<>FermatPath<>"\"\n";
			(*script=script<>"\n"<>"setenv KiraCommand \""<>KiraCommand<>"\"";*)
		,
		_,
			PrintAndLog[" ***EnvVarSetter \"",EnvVarSetter,"\" is not supported yet."];
			PrintAndLog["Valid values:"];
			PrintAndLog["\t1. export"];
			PrintAndLog["\t2. setenv"];
			PrintAndLog["Please check the spelling or contact the developers of NeatIBP."];
			PrintAndLog["Exiting."];
			Exit[];
		];
		If[ForceRemoveExistingKiraResults===True,
			kiraScriptSetting=""
		,
			kiraScriptSetting="-f "
		];
		If[SpanningCutsConsistencyCheck===True,
			script=script<>"\n"<>MathematicaCommand<>" -script "<>packagePath<>
				"FFSpanningCutsConsistencyCheck.wl "<>outputPath<>"\n";
		];

		If[Or[UseShortenedSpanningCutsIBPs===True,And[UseShortenedSpanningCutsIBPs===Automatic,ShortenSpanningCutsIBPs===True]],
			kiraScriptSetting=kiraScriptSetting<>"-s ";
			script=script<>MathematicaCommand<>" -script "<>packagePath<>
				"FFSpanningCutsIBPShorten.wl "<>outputPath<>"\n";
			script=script<>shortenFinishQScript<>"\n\n";

		];
		spanningCuts=Get[outputPath<>"tmp/spanningCuts.txt"];
		Switch[SPCIBPReductionParallelization,
		True,
			Switch[SPCIBPReductionParallelizationMethod,
			"Naive",
				For[i=1,i<=Length[spanningCuts],i++,
					cut=spanningCuts[[i]];
					cutString="cut_"<>StringRiffle[ToString/@cut,"_"];
					cutPath=outputPath<>"results/results_spanning_cuts/"<>cutString<>"/";
					script=script<>ShellProcessor<>" "<>packagePath<>
						"interfaces/Kira/interface/run_kira_reduction.sh"<>" "<>
						kiraScriptSetting<>"\""<>KiraCommandRefined<>"\" "<>cutPath<>" &\n";
					If[SPCIBPReductionParallelJobNumber=!=Infinity,
						If[Mod[i,SPCIBPReductionParallelJobNumber]===0,
							script=script<>"wait\n";
						];
					];
				];
				If[SPCIBPReductionParallelJobNumber===Infinity,
					script=script<>"wait\n";
				,
					If[Mod[Length[spanningCuts],SPCIBPReductionParallelJobNumber]=!=0,
						script=script<>"wait\n";
					]
				];
			,
			"GNUParallel",
				If[UseGNUParallel=!=True,
					PrintAndLog["**** Err: Must set UseGNUParallel=True if you want to use GNU parallel. Exit."];
					Exit[1];
				];
				GNUParallelScript="";
				For[i=1,i<=Length[spanningCuts],i++,
					cut=spanningCuts[[i]];
					cutString="cut_"<>StringRiffle[ToString/@cut,"_"];
					cutPath=outputPath<>"results/results_spanning_cuts/"<>cutString<>"/";
					GNUParallelScript=GNUParallelScript<>ShellProcessor<>" "<>packagePath<>
						"interfaces/Kira/interface/run_kira_reduction.sh"<>" "<>
						kiraScriptSetting<>"\""<>KiraCommandRefined<>"\" "<>cutPath<>"\n";
				];
				Export[outputPath<>"tmp/GNU_parallel_reduction_script.txt",GNUParallelScript,"Text"];
				If[SPCIBPReductionParallelJobNumber===Infinity,
					script=script<>"\ncat "<>outputPath<>"tmp/GNU_parallel_reduction_script.txt | "<>GNUParallelCommand<>" --ungroup"<>"\n";
				,
					script=script<>"\ncat "<>outputPath<>"tmp/GNU_parallel_reduction_script.txt | "<>GNUParallelCommand<>" --ungroup -j "<>ToString[SPCIBPReductionParallelJobNumber]<>"\n";
				];
				
			,
			_,
				PrintAndLog["**** Err: SPCIBPReductionParallelizationMethod must be one of the following: \"Naive\" or \"GNUParallel\". Exiting."];
				Exit[1];
			]	
		,
		False,
			For[i=1,i<=Length[spanningCuts],i++,
				cut=spanningCuts[[i]];
				cutString="cut_"<>StringRiffle[ToString/@cut,"_"];
				cutPath=outputPath<>"results/results_spanning_cuts/"<>cutString<>"/";
				script=script<>ShellProcessor<>" "<>packagePath<>
					"interfaces/Kira/interface/run_kira_reduction.sh"<>" "<>
					kiraScriptSetting<>"\""<>KiraCommandRefined<>"\" "<>cutPath<>"\n";
			];
		,
		_,
			PrintAndLog["**** Err: SPCIBPReductionParallelization must be True or False. Exiting."];
			Exit[1];
		];
		If[Or[UseShortenedSpanningCutsIBPs===True,And[UseShortenedSpanningCutsIBPs===Automatic,ShortenSpanningCutsIBPs===True]],
			IBPMergeSetting="-s "
		,
			IBPMergeSetting=""
		];
		script=script<>"\n"<>MathematicaCommand<>" -script "<>packagePath<>
			"ReducedSpanningCutsMerge.wl "<>IBPMergeSetting<>"-Kira "<>outputPath<>"\n";

		
		
	,
	"FFNumerical",
		script="echo \"Sorry, FFNumerical reduction in spanning cuts mode is in to-do list, not online yet.\"\n";
	,
	_,
		(*this should have been excluded by initialization, but... better double checked*)
		PrintAndLog["***Err: Unkown IBPReductionMethod ",IBPReductionMethod];(*we'd better use PrintAndLog. 2024.10.26: yes we used.*)
		Exit[];
	];
,
_,
	PrintAndLog["AssignIBPReduction.wl: unkown mode ", mode, ". Failed"];
	Exit[];
]


If[Not[SpanningCutsMode===True],
	script=script<>" >> "<>outputPath<>finishedTagFile
]




PrintAndLog[outputPath<>"tmp/assigned_reduction_script.sh"]


Export[outputPath<>"tmp/assigned_reduction_script.sh",script,"Text"]
