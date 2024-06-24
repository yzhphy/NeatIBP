(* ::Package:: *)

(*
how to use in shell:
...

*)


commandLineMode=True





If[commandLineMode,
	(*packagePath=DirectoryName[$InputFileName];*)
	workingPath=Directory[]<>"/";
	spcResultsPath=$CommandLine[[-1]]<>"/";

	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	(*packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";*)
	(*resultsPath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/Examples_Kira+NeatIBP/config-tenniscourt/config/massless/massless-tenniscourt/massless-tenniscourt/spc/outputs/tenniscort-mslss-spc/results/";*)
	resultsPath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/Examples_Kira+NeatIBP/config-tenniscourt/config/massless/spc/outputs/tenniscort-mslss-spc/results_spanning_cuts-0612/results_spanning_cuts-0612/";
	targets=Get["/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/Examples_Kira+NeatIBP/config-tenniscourt/config/massless/massless-tenniscourt/massless-tenniscourt/spc/targetIntegrals.txt"];
	
]








If[StringSplit[#][[-1]]=!="/",#=#<>"/"]&/@resultsPath


Print["number of targets: ",targets//Length]


(* ::Section:: *)
(*Reading spc Kira outputs*)


timer=AbsoluteTime[];
Print["Reading spanning cuts Kira outputs..."];


spcResultsPath=resultsPath(*<>"results_spanning_cuts/";*)


spanningCutStrings=Select[FileNameSplit[#][[-1]]&/@FileNames[All,spcResultsPath],StringContainsQ["cut_"]]
(*need to be modified, we should read SpanningCuts.txt*)
spanningCuts=(ToExpression/@(StringSplit[#,"_"][[2;;-1]]))&/@spanningCutStrings;


furtherPath="KiraInput/results/Tuserweight/"
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
]


FormulateReducedIBP//ClearAll
Options[FormulateReducedIBP]={IntegralHead->G}
FormulateReducedIBP[reducedIBP_,OptionsPattern[]]:=Module[{MIs,integralHead},
	integralHead=OptionValue[IntegralHead];
	MIs=Select[Variables[reducedIBP[[2]]],Head[#]===integralHead&];
	reducedIBP[[1]]->(#->D[reducedIBP[[2]],#]&/@MIs)
]





allMIs={};
formulatedReducedIBPs={};
Module[{i,j,cut,newFormulatedReducedIBP},
	For[i=1,i<=Length[spanningCuts],i++,
		Print["\t\treading spanning cuts (",i,"/",Length[spanningCuts],")"];
		cut=spanningCuts[[i]];
		newFormulatedReducedIBP=FormulateReducedIBP/@GetReducedIBPs[cut];
		formulatedReducedIBPs=Append[formulatedReducedIBPs,newFormulatedReducedIBP];
		allMIs=Append[allMIs,newFormulatedReducedIBP[[All,2,All,1]]];
	]
]
allMIs=allMIs//Flatten//Union;
Print["\tTotal MI number: ",allMIs//Length]
Print["\tFinished. Time used: ",Round[AbsoluteTime[]-timer]," second(s)."]


(* ::Subsection::Closed:: *)
(*bak*)
(**)


(*CombineFormulatedReducedIBP[targets_,IBPA_,IBPB_,cutsA_,cutsB_]:=Module[
{IBPC,target,i,targetRedA,targetRedB,MIs,j,MI},
	If[!SubsetQ[IBPA[[All,1]],targets],
		Print["***CombineFormulatedReducedIBP: Lacking target(s) in IBPA."];
		Return[$Failed]
	];
	If[!SubsetQ[IBPB[[All,1]],targets],
		Print["***CombineFormulatedReducedIBP: Lacking target(s) in IBPB."];
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
(*Combining IBPs*)


timer=AbsoluteTime[];
Print["Combining IBP results..."];


IntegralCuttedQ[int_,cut_]:=MemberQ[Sign/@((int/.G->List)[[cut]]-1/2),-1]


RandomNumericCheck[expr_]:=Module[{vars=Variables[expr]},Factor[expr/.(#->RandomPrime[Length[vars]*500]/RandomPrime[Length[vars]^2*1000]&/@vars)]]


CombineReducedRedults//ClearAll
Options[CombineReducedRedults]={ShortenedMode->False}
CombineReducedRedults[OptionsPattern[]]:=Module[
{i,j,k,coeff,newCoeff,reducedResult,target,MI,formulatedReducedIBP,
cut,targetReducedFormulated,targetReducedFormulatedMIs,oldK,reducedResults
},
	reducedResults={};
	For[i=1,i<=Length[targets],i++,
		target=targets[[i]];
		Print["\tCombining target ",target," (",i,"/",Length[targets],")..."];
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


CombineReducedRedults[ShortenedMode->True]


GetCoeff[target_,MI_,cut_]:=Module[{reduced=target/.(FormulateReducedIBP/@GetReducedIBPs[cut])},
	If[MemberQ[reduced[[All,1]],MI],MI/.reduced,0]
]



Print["\tFinished. Time used: ",Round[AbsoluteTime[]-timer]," second(s)."]
