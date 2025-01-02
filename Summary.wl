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
	workingPath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/Examples_in_the_paper/2l4p_top/lxb3/";
	packagePath=NotebookDirectory[];
	missionInput="config.txt";
	
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



SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse



(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting."];Exit[]]
If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting."];Exit[]]
(*TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting."];Exit[]]

*)



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
	(*Print[
		"!!![Notice]: the config setting CutIndices=\"spanning cuts\" is an out-of-date gramma since v1.1.0.0.\n",
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


If[FileExistsQ[tmpPath<>"summarized.tag"],
	Export[tmpPath<>"once_summarized.tag","","Text"];(*once means ceng2 jing1*)
	DeleteFile[tmpPath<>"summarized.tag"]
];



TemporaryDirectory=outputPath<>"tmp"
(*Run["rm -rf "<>TemporaryDirectory];*)(*It seems to be useless*)
If[!DirectoryQ[#],Run["mkdir "<>#]]&[TemporaryDirectory]
Get[packagePath<>"Pak_Algorithm/Pak_Algorithm.wl"]
Get[packagePath<>"SyzygyRed.wl"]
Prepare[];


LogFile=outputPath<>"results/summary"<>".txt";
(*If[!DirectoryQ[#],Run["mkdir "<>#]]&[LogPath]*)
Print["Summarizing..."]






ReportTotalTimeUsed[]:=Module[{startAbsTime,timeUsedString,timeUsed,seconds,minutes,hours},
	PrintAndLog["=========================================="];
	If[FileExistsQ[tmpPath<>"start_abs_time.txt"],
		startAbsTime=Get[tmpPath<>"start_abs_time.txt"];
		If[startAbsTime===0,
			timeUsedString="not available since this is a continued running.";
		,
			timeUsed=Round[AbsoluteTime[]-startAbsTime];
			{minutes,seconds}=QuotientRemainder[timeUsed,60];
			{hours,minutes}=QuotientRemainder[minutes,60];
			timeUsedString=If[hours>0,ToString[hours]<>"h",""]<>If[minutes>0||hours>0,ToString[minutes]<>"m",""]<>ToString[seconds]<>"s.";
		];
		PrintAndLog["Total real time used: ",timeUsedString];
	]
]






If[!FileExistsQ[tmpPath<>"initialized.txt"],
	Print["*****Initialization failed, cannot summarize."];
	Exit[0];
]
If[FileExistsQ[tmpPath<>"continue_preparation_failed.tag"],
	Print["****Continue preparation failed, cannot summarize."];
	Exit[0];
]
CopyFile2[file1_,file2_]:=Module[{},
	If[!FileExistsQ[file1],
		PrintAndLog["Summary.wl: File ",file1," does not exist. Give up summarization."];
		Exit[];
	];
	CopyFile[file1,file2]
]
(*This is for the case that spanning cuts mission failed, to stop summarizing. ----2025.01.02*)

If[FileExistsQ[tmpPath<>"spanning_cuts_mode.txt"],
	PrintAndLog["Spanning cuts mode, collecting spanning cuts results."];
	Module[{spanningCutsMissionMainPath,spanningCutStrings,spanningCuts,
	spanningCutString,spanningCutResultFolder,spanningCutSummaryFolder,spanningCutInputsFolder},
		spanningCutsMissionMainPath=tmpPath<>"spanning_cuts_missions/";
		spanningCuts=Get[tmpPath<>"spanningCuts.txt"];
		spanningCutStrings=StringRiffle[ToString/@#,"_"]&/@spanningCuts;
		For[i=1,i<=Length[spanningCutStrings],i++,
			spanningCutString=spanningCutStrings[[i]];
			spanningCutSummaryFolder=outputPath<>"results/results_spanning_cuts/cut_"<>spanningCutString<>"/";
			spanningCutResultFolder=spanningCutsMissionMainPath<>"cut_"<>spanningCutString<>"/outputs/"<>ReductionOutputName<>"/results/";
			spanningCutInputsFolder=spanningCutsMissionMainPath<>"cut_"<>spanningCutString<>"/outputs/"<>ReductionOutputName<>"/inputs/";
			If[!DirectoryQ[#],CreateDirectory[#]]&[spanningCutSummaryFolder];(*automatically -p*)
			If[!DirectoryQ[#],CreateDirectory[#]]&[spanningCutSummaryFolder<>"results/"];
			If[!DirectoryQ[#],CreateDirectory[#]]&[spanningCutSummaryFolder<>"inputs/"];
			(
				CopyFile2[spanningCutResultFolder<>#<>".txt",spanningCutSummaryFolder<>"results/"<>#<>".txt"];
				(*PrintAndLog["\t copied ",spanningCutResultFolder<>#<>".txt"," to ",spanningCutSummaryFolder<>"results/"<>#<>".txt"];*)
			)&/@{"IBP_all","MI_all","OrderedIntegrals","summary"};
			(
				CopyFile2[spanningCutInputsFolder<>#,spanningCutSummaryFolder<>"inputs/"<>#];
				(*PrintAndLog["\t copied ",spanningCutInputsFolder<>#," to ",spanningCutSummaryFolder<>"inputs/"<>#];*)
			)&/@(FileNameSplit[#][[-1]]&/@FileNames[All,spanningCutInputsFolder]);
		]
	];
	PrintAndLog["\t Finished.\n All spanning cuts finished."];
	ReportTotalTimeUsed[];
	(*same code in the end of this wl file*)
	If[PerformIBPReduction===True,
		Export[outputPath<>"tmp/summarized.tag","","Text"];
	];
	Exit[0];
]



TemporaryDirectory=tmpPath


missionStatusFolder=tmpPath<>"mission_status/"
missionStatus={ToExpression[StringReplace[FileNameSplit[#][[-1]],".txt"->""]]//SectorNumberToSectorIndex,Get[#]}&/@FileNames[All,missionStatusFolder];
If[!SubsetQ[{"ComputationFinished"},Union[missionStatus[[All,2]]]],
	Print["Not all missions are finished, cannot summarize."];
	Exit[0];
]











If[MIFromAzuritino===True,azuritinoMIFolder=outputPath<>"tmp/azuritino_MIs/"]





fileNamesMI=FileNames[All,outputPath<>"results/MI/"];
sectorIDsMI=ToExpression[StringReplace[FileNameSplit[#][[-1]],{".txt"->""}]]&/@fileNamesMI;
MIs=Get/@fileNamesMI;
ordering=SortBy[Range[Length[sectorIDsMI]],{-Total[SectorNumberToSectorIndex[sectorIDsMI[[#]]]],-sectorIDsMI[[#]]}&];
(*Print[{Total[SectorNumberToSectorIndex[sectorIDsMI[[#]]]],sectorIDsMI[[#]]}&/@Range[Length[sectorIDsMI]]]*)
sectorIDsMI=sectorIDsMI[[ordering]];
MIs=MIs[[ordering]];


sectorMaps=Get[outputPath<>"tmp/sectorMaps.txt"];
mappedSectors=sectorMaps[[All,1]];
If[MIFromAzuritino===True,
	AzuritinoMIs=If[
		Or[
			MemberQ[mappedSectors,SectorNumberToSectorIndex[#]],
			ZeroSectorQ[SectorNumberToSectorIndex[#]]
		],
		{}
	,
		If[FileExistsQ[azuritinoMIFolder<>ToString[#]<>".txt"],
			Get[azuritinoMIFolder<>ToString[#]<>".txt"]
		,
			{}
		]
	]&/@sectorIDsMI
	
];



fileNamesIBP=FileNames[All,outputPath<>"results/IBP/"];
sectorIDsIBP=ToExpression[StringReplace[FileNameSplit[#][[-1]],{".txt"->""}]]&/@fileNamesIBP;
IBPs=Get/@fileNamesIBP;
ordering=SortBy[Range[Length[sectorIDsIBP]],{-Total[SectorNumberToSectorIndex[sectorIDsIBP[[#]]]],-sectorIDsIBP[[#]]}&];
sectorIDsIBP=sectorIDsIBP[[ordering]];
IBPs=IBPs[[ordering]];


If[And[NeedSymmetry===True,AdditionalMISymmetries===True],
	timer=AbsoluteTime[];
	PrintAndLog["Finding additional symmetry relations between master integrals"];
	MIsbackupPath=outputPath<>"tmp/MIs_before_AdditionalMISymmetries/";
	MIsbackup=MIs;
	For[i=1,i<=sectorIDsMI,i++,
		Export[MIsbackupPath<>ToString[sectorIDsMI[[i]]]<>".txt",MIs[[i]]//InputForm//ToString]
	];
	undeterminedMIs=SortBy[MIs//Flatten,IntegralOrdering]//Reverse//Reverse;(* the function tends to choose master integrals in the fronter of the list as unique integrals, 2 reverses for remindering! This order must agrees our convention!*)
	AdditionalMISymmetryRelations={};
	uniqueMIs={};
	mappedMIs={};
	While[True,
		If[undeterminedMIs==={},Break[]];
		(*Print["\t\t",Length[undeterminedMIs]," undetermined master intergal(s) left."];*)
		newUniqueMI=undeterminedMIs[[1]];
		uniqueMIs=Join[uniqueMIs,{newUniqueMI}];
		undeterminedMIs=undeterminedMIs[[2;;-1]];
		mappedUndeterminedMIIndices={};
		For[i=1,i<=Length[undeterminedMIs],i++,
			newTestingMI=undeterminedMIs[[i]];
			If[LPSymmetryQ[newTestingMI,newUniqueMI],
				mappedMIs=Join[mappedMIs,{newTestingMI}];
				mappedUndeterminedMIIndices=Join[mappedUndeterminedMIIndices,{{i}}];
				AdditionalMISymmetryRelations=Join[AdditionalMISymmetryRelations,{newTestingMI->newUniqueMI}]
			]
		];
		undeterminedMIs=Delete[undeterminedMIs,mappedUndeterminedMIIndices]
	];
	PrintAndLog["\tFinished. Found ",Length[AdditionalMISymmetryRelations]," additional symmetry relation(s):\n\t",AdditionalMISymmetryRelations];
	PrintAndLog["\tTime used: ",Round[AbsoluteTime[]-timer]," second(s)"];
	timer=AbsoluteTime[];
	PrintAndLog["Rearranging results... "];
	MIs=Complement[#,mappedMIs]&/@MIs;
	MINumberDecreased=-(Length[Flatten[MIs]]-Length[Flatten[MIsbackup]]);
	PrintAndLog[
		"\tRemoved ",
		MINumberDecreased,
		If[MINumberDecreased===Length[AdditionalMISymmetryRelations]," "," (*** abnormal!) "],
		"mapped master integral(s) in MIs from NeatIBP results."
	];
	If[MIFromAzuritino===True,
		AzuritinoMIsbackup=AzuritinoMIs;
		AzuritinoMIs=Complement[#,AzuritinoMIs]&/@MIs;
		AzuritinoMINumberDecreased=-(Length[Flatten[AzuritinoMIs]]-Length[Flatten[AzuritinoMIsbackup]]);
		PrintAndLog[
			"\tRemoved ",
			AzuritinoMINumberDecreased,
			If[AzuritinoMINumberDecreased===Length[AdditionalMISymmetryRelations]," "," "],
			"mapped master integral(s) in MIs from Azuritino results."
		];
	];
	IBPNumberIncreased=0;
	For[i=1,i<=Length[sectorIDsIBP],i++,
		sectorID=sectorIDsIBP[[i]];
		mappedMIsInCurrentSector=Select[mappedMIs,SectorNumber[Sector[#]]===sectorID&];
		IBPs[[i]]=Join[
			IBPs[[i]],
			(#-(#/.AdditionalMISymmetryRelations))&/@mappedMIsInCurrentSector
		];
		IBPNumberIncreased+=Length[mappedMIsInCurrentSector]
	];
	PrintAndLog[
		"\tAdded ",
		IBPNumberIncreased,
		If[IBPNumberIncreased===Length[AdditionalMISymmetryRelations]," "," (*** abnormal!) "],
		"relations(s) into IBPs."
	];
	For[i=1,i<=Length[sectorIDsMI],i++,
		Export[outputPath<>"results/MI/"<>ToString[sectorIDsMI[[i]]]<>".txt",MIs[[i]]//InputForm//ToString]
	];
	PrintAndLog["\tnew MIs from NeatIBP results saved."];
	For[i=1,i<=Length[sectorIDsIBP],i++,
		Export[outputPath<>"results/IBP/"<>ToString[sectorIDsIBP[[i]]]<>".txt",IBPs[[i]]//InputForm//ToString]
	];(*I really do not like overwriting this... maybe we will rewrite here in the future*)
	PrintAndLog["\tnew IBPs saved."];
	PrintAndLog["\tFinished. Time used: ",Round[AbsoluteTime[]-timer]," second(s)"];
]





(*SDim=Length[Cases[Variables[IBPs],_G][[1]]/.G->List]*)

timer=AbsoluteTime[];
relavantIntegrals=Get/@FileNames[All,outputPath<>"tmp/relavant_integrals/"];
(*integralList=IntegralList[relavantIntegrals]*)
integralList=IntegralList[Join[IBPs,MIs]];
Export[outputPath<>"results/OrderedIntegrals.txt",integralList//InputForm//ToString];
Export[outputPath<>"results/MI_all.txt",MIs//Flatten//InputForm//ToString];
Export[outputPath<>"results/IBP_all.txt",IBPs//Flatten//InputForm//ToString];

(*Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]*)






PrintAndLog["=================Summary==================="]


MIDifferenceReportString=""
If[MIFromAzuritino===True,
	MIDifference=Length[Flatten[MIs]]-Length[Flatten[AzuritinoMIs]];
	MIDifferenceReportString=Switch[Sign[MIDifference],
	1,
		" (+"<>ToString[MIDifference]<>" MI(s) than azuritino)",
	0,
		"",
	-1,
		" (-"<>ToString[-MIDifference]<>" MI(s) than azuritino)"
	]
]
PrintAndLog["Total MI number: ",Length[Flatten[MIs]],MIDifferenceReportString]
PrintAndLog["Total IBP number: ",Length[Flatten[IBPs]]]
PrintAndLog["Total integral number: ",Length[Flatten[integralList]]]
PrintAndLog["---------------MIs-------------------------"]
PrintAndLog[Flatten[MIs]//InputForm//ToString]
PrintAndLog["---------------MI numbers------------------"]
For[i=1,i<=Length[fileNamesMI],i++,
	sector=SectorNumberToSectorIndex[sectorIDsMI[[i]]];
	MI=MIs[[i]];
	If[MIFromAzuritino===True,AzuritinoMI=AzuritinoMIs[[i]]];
	MIDifferenceReportString="";
	If[MIFromAzuritino===True,
		MIDifference=Length[MI]-Length[AzuritinoMI];
		MIDifferenceReportString=Switch[Sign[MIDifference],
		1,
			" (+"<>ToString[MIDifference]<>" MI(s) than azuritino)",
		0,
			"",
		-1,
			" (-"<>ToString[-MIDifference]<>" MI(s) than azuritino)"
		]
	];
	If[Length[MI]>0,PrintAndLog["sector ",sector," :\t",Length[MI]," MI(s)",MIDifferenceReportString]]
]

PrintAndLog["---------------IBP numbers------------------"]
For[i=1,i<=Length[fileNamesIBP],i++,
	sector=SectorNumberToSectorIndex[sectorIDsIBP[[i]]];
	IBP=IBPs[[i]];
	If[Length[IBP]>0,PrintAndLog["sector ",sector," :\t",Length[IBP]," IBPs(s)"]]
]
ReportTotalTimeUsed[];








(*same code in spanning cuts part of this wl file*)
If[PerformIBPReduction===True,
	Export[outputPath<>"tmp/summarized.tag","","Text"];
]

