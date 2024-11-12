(* ::Package:: *)

(*
how to use in shell:
...

*)


commandLineMode=True





If[commandLineMode,
	(*packagePath=DirectoryName[$InputFileName];*)
	workingPath=Directory[]<>"/";
	commandLine=$CommandLine;
	If[MemberQ[commandLine,"-s"],
		shortenedMode=True;
		commandLine=DeleteCases[commandLine,"-s"];
	,
		shortenedMode=False;
	];
	outputPath=commandLine[[-1]];
	reducedIBPInputSetting=$CommandLine[[-2]];
	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	(*packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";*)
	(*resultsPath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/Examples_Kira+NeatIBP/config-tenniscourt/config/massless/massless-tenniscourt/massless-tenniscourt/spc/outputs/tenniscort-mslss-spc/results/";*)
	shortenedMode=True;
	reducedIBPInputSetting="-Kira";
	outputPath="/home/zihao/NeatIBP/examples_private/kira_interface/examples/dbox-demo/outputs/kira-spc/"
	
]








If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


resultsPath=outputPath<>"results/"
spcResultsPath=resultsPath<>"results_spanning_cuts/";


(*If[StringSplit[#][[-1]]=!="/",#=#<>"/"]&/@resultsPath*)


targets=Get[outputPath<>"inputs/targetIntegrals.txt"]
(*issue of the hard coded file name targetIntegrals.txt*)


Print["number of targets: ",targets//Length]


Print[reducedIBPInputSetting]


(* ::Section:: *)
(*Identifying IBP input*)


Switch[reducedIBPInputSetting,
"-Kira",
	reducedIBPInput="Kira";
,
"-FFNumerical",
	reducedIBPInput="FFNumerical";
,
_,
	Print["[Notice]: IBP input not specified or uncategorized. Reading config file for this info."];
	outputPath1=outputPath;
	Get[outputPath<>"inputs/config.txt"];
	reducedIBPInput=IBPReductionMethod;
	Print["IBP input: ",IBPReductionMethod];
	outputPath=outputPath1;
]


Switch[reducedIBPInput,
"Kira",
	furtherPath="results/Kira_reduction_results/reduced_IBP_Table.txt";
,
"FFNumerical",
	Print["ReducedSpanningCutsMerge: Sorry, FFNumerical spanning cuts is still in developement. Exiting."];
	Exit[];
,
_,
	Print["ReducedSpanningCutsMerge: Uncategorized IBP input: \"",reducedIBPInput,"\". Exiting. "];
	Exit[];
	
]


(* ::Section:: *)
(*Reading spc outputs*)


timer=AbsoluteTime[];
Print["Reading IBP reduction results in each spanning cuts..."];


GetReducedIBPs[cut_]:=Get[spcResultsPath<>"cut_"<>StringRiffle[ToString/@cut,"_"]<>"/"<>furtherPath]





spanningCutStrings=Select[FileNameSplit[#][[-1]]&/@FileNames[All,spcResultsPath],StringContainsQ["cut_"]]
(*need to be modified, we should read SpanningCuts.txt, but what if user deletes tmp folder? Maybe if so we can use this as a 2nd choice*)
spanningCuts=(ToExpression/@(StringSplit[#,"_"][[2;;-1]]))&/@spanningCutStrings;


(*not needed, code specially for kira*)
(*furtherPath="KiraInput/results/Tuserweight/"
GetOrderedIntegrals[cut_]:=Get[spcResultsPath<>"cut_"<>StringRiffle[ToString/@cut,"_"]<>"/results/OrderedIntegrals.txt"];
GetTuserweightToGRules[cut_]:=Module[{integralsReverse=Reverse[GetOrderedIntegrals[cut]]},
	Table[Tuserweight[i]->integralsReverse[[i]],{i,Length[integralsReverse]}]
]
GetReducedIBPs[cut_]:=Get[spcResultsPath<>"cut_"<>StringRiffle[ToString/@cut,"_"]<>"/"<>furtherPath<>"kira_list.m"]/.GetTuserweightToGRules[cut];
GetMI[cut_]:=Module[{string,lines,MIs},
	string=StringReplace[Import[spcResultsPath<>"cut_"<>StringRiffle[ToString/@cut,"_"]<>"/"<>furtherPath<>"masters","Text"]," "->""];
	lines=DeleteCases[StringSplit[string,"\n"],""];
	lines=StringSplit[#,"#"][[1]]&/@lines;(*decomment*)
	MIs=Tuserweight[ToExpression[#]]&/@lines;
	MIs/.GetTuserweightToGRules[cut]
]
GetMIFinal[cut_]:=Module[{string,lines,MIs},
	string=StringReplace[Import[spcResultsPath<>"cut_"<>StringRiffle[ToString/@cut,"_"]<>"/"<>furtherPath<>"masters.final","Text"]," "->""];
	lines=DeleteCases[StringSplit[string,"\n"],""];
	lines=StringSplit[#,"#"][[1]]&/@lines;(*decomment*)
	MIs=Tuserweight[ToExpression[#]]&/@lines;
	MIs/.GetTuserweightToGRules[cut]
]*)





FormulateReducedIBP//ClearAll
Options[FormulateReducedIBP]={IntegralHead->G}
FormulateReducedIBP[reducedIBP_,OptionsPattern[]]:=Module[{MIs,integralHead},
	integralHead=OptionValue[IntegralHead];
	MIs=Select[Variables[reducedIBP[[2]]],Head[#]===integralHead&];
	reducedIBP[[1]]->(#->D[reducedIBP[[2]],#]&/@MIs)
]





allMIs={};
formulatedReducedIBPs={};
Module[{i,j,cut,newFormulatedReducedIBP,gotReducedIBPs},
	For[i=1,i<=Length[spanningCuts],i++,
		Print["\t\treading spanning cuts (",i,"/",Length[spanningCuts],")"];
		cut=spanningCuts[[i]];
		gotReducedIBPs=GetReducedIBPs[cut];
		If[gotReducedIBPs===$Failed,
			Print["Failed to read reduced IBPs for cut " ,cut,". Giving up the rest steps. "];
			Exit[]
		];
		newFormulatedReducedIBP=FormulateReducedIBP/@gotReducedIBPs;
		formulatedReducedIBPs=Append[formulatedReducedIBPs,newFormulatedReducedIBP];
		allMIs=Append[allMIs,newFormulatedReducedIBP[[All,2,All,1]]];
	]
]
allMIs=allMIs//Flatten//Union;
Print["\tTotal MI number: ",allMIs//Length]
Print["\tFinished. Time used: ",Round[AbsoluteTime[]-timer]," second(s)."]


If[MemberQ[allMIs,$Failed],Print["Failed to read all MIs. Exit. "];Exit[]]


(* ::Subsection::Closed:: *)
(*bak*)
(**)


(*MergeFormulatedReducedIBP[targets_,IBPA_,IBPB_,cutsA_,cutsB_]:=Module[
{IBPC,target,i,targetRedA,targetRedB,MIs,j,MI},
	If[!SubsetQ[IBPA[[All,1]],targets],
		Print["***MergeFormulatedReducedIBP: Lacking target(s) in IBPA."];
		Return[$Failed]
	];
	If[!SubsetQ[IBPB[[All,1]],targets],
		Print["***MergeFormulatedReducedIBP: Lacking target(s) in IBPB."];
		Return[$Failed]
	];
	IBPC={};
	For[i=1,i\[LessEqual]Length[targets],i++,
		target=targets[[i]];
		targetRedA=target/.IBPA;
		targetRedB=target/.IBPB;
		MIs=Union[targetRedA[[All,1]],targetRedB[[All,1]]];
		For[j=1,j\[LessEqual]Length[MIs],j++,
			MI=MIs[[j]];
			Switch[IntegralCuttedQ[MI,]
		]
	]
	
]*)


(* ::Section:: *)
(*Merging IBPs*)


timer=AbsoluteTime[];
Print["Merging IBP results..."];


IntegralCuttedQ[int_,cut_]:=MemberQ[Sign/@((int/.G->List)[[cut]]-1/2),-1]


RandomNumericCheck[expr_]:=Module[{vars=Variables[expr]},Factor[expr/.(#->RandomPrime[Length[vars]*500]/RandomPrime[Length[vars]^2*1000]&/@vars)]]


MergeReducedRedults//ClearAll
Options[MergeReducedRedults]={ShortenedMode->False}
MergeReducedRedults[OptionsPattern[]]:=Module[
{i,j,k,coeff,newCoeff,reducedResult,target,MI,formulatedReducedIBP,
cut,targetReducedFormulated,targetReducedFormulatedMIs,oldK,reducedResults
},
	reducedResults={};
	For[i=1,i<=Length[targets],i++,
		target=targets[[i]];
		Print["\tMerging target ",target," (",i,"/",Length[targets],")..."];
		reducedResult={};
		For[j=1,j<=Length[allMIs],j++,
			MI=allMIs[[j]];
			coeff="ND";
			For[k=1,k<=Length[spanningCuts],k++,
			
				cut=spanningCuts[[k]];
				formulatedReducedIBP=formulatedReducedIBPs[[k]];
				targetReducedFormulated=target/.formulatedReducedIBP;
				targetReducedFormulatedMIs=targetReducedFormulated[[All,1]];
				If[MemberQ[targetReducedFormulatedMIs,MI],
					newCoeff=MI/.targetReducedFormulated
				,
					newCoeff=0
				];
				If[
					Or[
						IntegralCuttedQ[MI,cut],
						And[
							OptionValue[ShortenedMode],
							RandomNumericCheck[newCoeff]===0
						]
					]
				,
					If[RandomNumericCheck[newCoeff]=!=0,
						Print["\t***** Err: MI ",MI," should vanish on cut ",cut,"with nonzero coefficient, but it appears in the reduction result of ", target,"."];
						coeff=$Failed;
						Break[]
					,
						Continue[]
					]
				,
					If[Or[
						coeff==="ND",
						And[
							OptionValue[ShortenedMode],
							RandomNumericCheck[coeff]===0
						]
					],
						coeff=newCoeff;
						oldK=k;(*only used for err report*)
						
					,
						
						If[RandomNumericCheck[coeff-newCoeff]=!=0,
							Print["\t***** Err: Coefficient of ",target," reducing to MI ",MI," mismatches between cuts ",spanningCuts[[oldK]]," and ",spanningCuts[[k]],"."];
							coeff=$Failed;
							Break[]
						,
							coeff=newCoeff;
							oldK=k;
							
						]
					]
				];
				
			];
			If[coeff===$Failed,
				reducedResult=$Failed;
				Break[];
			];
			reducedResult=Append[reducedResult,MI->coeff];
		];
		
		reducedResults=Append[reducedResults,target->reducedResult]
	];
	reducedResults
]


mergedIBPFormulated=MergeReducedRedults[ShortenedMode->shortenedMode]


UnFormulateReducedIBP[formulatedIBP_]:=Rule[
	formulatedIBP[[1]],
	Sum[
		(formulatedIBP[[2,i,2]])*
		formulatedIBP[[2,i,1]]
	,
		{i,Length[formulatedIBP[[2]]]}]
]


mergedIBP=UnFormulateReducedIBP/@mergedIBPFormulated


(*GetCoeff[target_,MI_,cut_]:=Module[{reduced=target/.(FormulateReducedIBP/@GetReducedIBPs[cut])},
	If[MemberQ[reduced[[All,1]],MI],MI/.reduced,0]
]*)



exportPath=outputPath<>"results/reduced_IBP_spanning_cuts_merged/from_"<>reducedIBPInput<>"_reduction/"
If[!DirectoryQ[exportPath],CreateDirectory[exportPath]];
Export[exportPath<>"IBPTable.txt",mergedIBP//InputForm//ToString]





finishedTagFile="results/NeatIBP_finished.tag"
Export[outputPath<>finishedTagFile,"","Text"];


Print["\tFinished. Time used: ",Round[AbsoluteTime[]-timer]," second(s)."]
