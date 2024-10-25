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


If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting.";Exit[]]]


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


finishedTagFile="results/NeatIBP_finished.tag"





If[!FileExistsQ[outputPath<>"results/summary.txt"],
	If[PerformIBPReduction===True,Print["Summary failed, cannot perform IBP reduction."]];
	Exit[0];
]


If[PerformIBPReduction=!=True,
	Export[outputPath<>finishedTagFile,"","Text"];
	Exit[];
]





If[CutIndices==="spanning cuts",
	If[
		DirectoryQ[outputPath<>"results/results_spanning_cuts/"]&&
		FileExistsQ[outputPath<>"tmp/spanning_cuts_mode.txt"]
	,
		mode="spanning cuts";
	,
		Print[outputPath," does not look like a spanning cuts folder. Failed."];
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
			Print["***File at ",FermatPath," does not exist. Exiting."];
			Exit[];
		];
		If[FileNameSplit[FermatPath][[-1]]=!="fer64",
			Print["***FermatPath ",FermatPath," should be specified to \"fer64\". Exiting."];
			Exit[];
		];
		If[Count[
			FileExistsQ/@Select[
				StringSplit[KiraCommandRefined," "],
				FileNameSplit[#][[-1]]==="kira"&
			]
		,True]<1,
			Print["***KiraCommand \"",KiraCommand,"\" is not an correct kira command. Please check. Exiting."];
			Exit[];
		];(*kira command may include some settings*)
		Switch[EnvVarSetter,
		"export",
			script="export FERMATPATH=\""<>FermatPath<>"\"";
			(*script=script<>"\n"<>"export KiraCommand=\""<>KiraCommand<>"\"";*)
		,
		"setenv",
			script="setenv FERMATPATH \""<>FermatPath<>"\"";
			(*script=script<>"\n"<>"setenv KiraCommand \""<>KiraCommand<>"\"";*)
		,
		_,
			Print[" ***EnvVarSetter \"",EnvVarSetter,"\" is not supported yet."];
			Print["Valid values:"];
			Print["\t1. export"];
			Print["\t2. setenv"];
			Print["Please check the spelling or contact the developers of NeatIBP."];
			Print["Exiting."];
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
		Print["***Err: Unkown IBPReductionMethod ",IBPReductionMethod];(*we'd better use PrintAndLog*)
		Exit[];
	];
,
"spanning cuts",
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
			Print["***File at ",FermatPath," does not exist. Exiting."];
			Exit[];
		];
		If[FileNameSplit[FermatPath][[-1]]=!="fer64",
			Print["***FermatPath ",FermatPath," should be specified to \"fer64\". Exiting."];
			Exit[];
		];
		If[Count[
			FileExistsQ/@Select[
				StringSplit[KiraCommandRefined," "],
				FileNameSplit[#][[-1]]==="kira"&
			]
		,True]<1,
			Print["***KiraCommand \"",KiraCommand,"\" is not an correct kira command. Please check. Exiting."];
			Exit[];
		];(*kira command may include some settings*)
		Switch[EnvVarSetter,
		"export",
			script="export FERMATPATH=\""<>FermatPath<>"\"";
			(*script=script<>"\n"<>"export KiraCommand=\""<>KiraCommand<>"\"";*)
		,
		"setenv",
			script="setenv FERMATPATH \""<>FermatPath<>"\"";
			(*script=script<>"\n"<>"setenv KiraCommand \""<>KiraCommand<>"\"";*)
		,
		_,
			Print[" ***EnvVarSetter \"",EnvVarSetter,"\" is not supported yet."];
			Print["Valid values:"];
			Print["\t1. export"];
			Print["\t2. setenv"];
			Print["Please check the spelling or contact the developers of NeatIBP."];
			Print["Exiting."];
			Exit[];
		];
		If[ForceRemoveExistingKiraResults===True,
			kiraScriptSetting=""
		,
			kiraScriptSetting="-f "
		];
		If[UseShortenedIBPForKira===True,
			kiraScriptSetting=kiraScriptSetting<>"-s ";
			script=script<>"\n"<>MathematicaCommand<>" -script "<>packagePath<>
				"FFSpanningCutsConsistencyCheck.wl "<>outputPath<>"\n";
			script=script<>MathematicaCommand<>" -script "<>packagePath<>
				"FFSpanningCutsIBPShorten.wl "<>outputPath<>"\n";
			script=script<>shortenFinishQScript<>"\n\n";

		];
		spanningCuts=Get[outputPath<>"tmp/spanningCuts.txt"];
		Switch[ParallelKiraReductionForSpanningCuts,
		True,
			For[i=1,i<=Length[spanningCuts],i++,
				cut=spanningCuts[[i]];
				cutString="cut_"<>StringRiffle[ToString/@cut,"_"];
				cutPath=outputPath<>"results/results_spanning_cuts/"<>cutString<>"/";
				script=script<>ShellProcessor<>" "<>packagePath<>
					"interfaces/Kira/interface/run_kira_reduction.sh"<>" "<>
					kiraScriptSetting<>"\""<>KiraCommandRefined<>"\" "<>cutPath<>" &\n";
				If[ParallelKiraJobNumber=!=Infinity,
					If[Mod[i,ParallelKiraJobNumber]===0,
						script=script<>"wait\n";
					];
				];
			];
			If[ParallelKiraJobNumber===Infinity,
				script=script<>"wait\n";
			,
				If[Mod[i,ParallelKiraJobNumber]=!=0,
					script=script<>"wait\n";
				]
			];
			
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
			Print["**** Err: ParallelKiraReductionForSpanningCuts must be True or False. Exiting."];
			Exit[1];
		];
		If[UseShortenedIBPForKira===True,
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
		Print["***Err: Unkown IBPReductionMethod ",IBPReductionMethod];(*we'd better use PrintAndLog*)
		Exit[];
	];
,
_,
	Print["AssignIBPReduction.wl: unkown mode ", mode, ". Failed"];
	Exit[];
]


script=script<>" >> "<>outputPath<>finishedTagFile;



Print[outputPath<>"tmp/assigned_reduction_script.sh"]


Export[outputPath<>"tmp/assigned_reduction_script.sh",script,"Text"]
