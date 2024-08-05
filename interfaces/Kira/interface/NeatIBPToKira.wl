(* ::Package:: *)

(*
how to use in shell:
math -script NeatIBPToKira.wl [NeatIBPOutputFolder]

*)


outputWithIntegralsLabelledByOrdering=True


commandLineMode=True





If[commandLineMode,
	(*packagePath=DirectoryName[$InputFileName];*)
	workingPath=Directory[]<>"/";
	commandLine=$CommandLine;
	convertPath=commandLine[[-1]];
	If[commandLine[[-2]]==="-s",
		mode="shortened";
	,
		mode="normal";	
	];
,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	(*packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";*)
	convertPath=workingPath<>"\\dbox\\outputs\\dbox\\";
	mode="normal"
	
]








If[StringSplit[convertPath,""][[-1]]=!="/",convertPath=convertPath<>"/"]


(* ::Section:: *)
(*Deal with mode*)


Switch[mode,
"normal",
	inputIBPFileName="IBP_all.txt";
	kiraUDSSubFolder="userSystem/";
,
"shortened",
	inputIBPFileName="IBP_all_shortened.txt";
	kiraUDSSubFolder="userSystemShortened/";
,
_,
	Print["***NeatIBPToKira.wl: Unkown mode ",mode, ". Exiting."];
	Exit[];
]


(* ::Section:: *)
(*Reading NeatIBP outputs*)


timer=AbsoluteTime[];
Print["Reading NeatIBP outputs... "];


NeatIBPOutputPath=convertPath<>"results/"


Print["\tIBPFile: ",NeatIBPOutputPath<>inputIBPFileName]
IBPs=Get[NeatIBPOutputPath<>inputIBPFileName]


mlist=Get[NeatIBPOutputPath<>"MI_all.txt"]


integrals=Get[NeatIBPOutputPath<>"OrderedIntegrals.txt"];


reducelist=Get[convertPath<>"/inputs/targetIntegrals.txt"];


kinevar=Select[Variables[IBPs],Head[#]=!=G&](*all the kinematics and d*)


Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


(* ::Section:: *)
(*Sorting IBPs using KIRA order*)


timer=AbsoluteTime[];
Print["Sorting IBPs using KIRA order..."];


PreKiraOrder[ibp_,integrals_]:=Module[{ar,bins},
	ar=ArrayRules[CoefficientArrays[ibp,integrals][[2]]];
	If[ar[[-1,2]]===0,ar=ar[[1;;-2]]];
	Sort[ar[[All,1,1]]]
]
SortIBPsForKira[ibps_,integrals_]:=Module[{preKiraOrders,maxLen,indices,kiraOrders},
	preKiraOrders=Table[PreKiraOrder[ibps[[i]],integrals],{i,Length[ibps]}];
	maxLen=Max[Length[#]&/@preKiraOrders];
	kiraOrders={
		#[[1]],
		-Length[#],
		PadRight[#,maxLen,Length[integrals]+1]
		(*Length[integrals]+1 is simpler than the most simple integral in integrals*)
	}&/@preKiraOrders;
	(*the larger the kiraOrders, the simpler the IBP is *)
	indices=SortBy[Range[Length[ibps]],kiraOrders[[#]]&]//Reverse;
	(*we put the simple functions in the front*)
	(*probe=kiraOrders[[indices]];*)
	ibps[[indices]]
]


userdefinedinput=SortIBPsForKira[IBPs,integrals]


Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]


(* ::Section:: *)
(*Exporting to KIRA input*)


timer=AbsoluteTime[];
Print["Exporting to KIRA input..."];


integralsReversed=integrals//Reverse;


IntegralLabel[int_]:=Flatten[Position[integralsReversed,int]][[1]]


IntegralLabelling[int_]:=If[outputWithIntegralsLabelledByOrdering,IntegralLabel[int],int]


(*Tokirastring[x_]:=Module[{length,varlist,coefflist,stringlist,stringlist1},
length=Length[x];
varlist=Table[Complement[Variables[x[[i]]],kinevar],{i,length}];
coefflist=Table[D[x[[i]],varlist[[i]][[j]]],{i,length},{j,Length[varlist[[i]]]}];
stringlist[i_]:=Append[Table[ToString[varlist[[i]][[j]]]<>"*("<>ToString[InputForm[coefflist[[i]][[j]]]]<>")",{j,Length[varlist[[i]]]}],"\n"];
stringlist1=Table[stringlist[i],{i,length}]//Flatten;
Return[stringlist1];];*)

OneIBPtoKiraString[ibp_]:=Module[{ar,relatedIntegrals},
	relatedIntegrals=Cases[Variables[ibp],_G];
	ar=ArrayRules[CoefficientArrays[ibp,relatedIntegrals][[2]]];
	If[ar[[-1,2]]===0,ar=ar[[1;;-2]]];
	StringRiffle[
		ToString[relatedIntegrals[[#[[1,1]]]]//IntegralLabelling//InputForm]<>"*("<>ToString[InputForm[#[[2]]]]<>")"&/@ar
	,"\n"]
]
IBPstoKiraString[ibps_]:=StringRiffle[Table[
	If[Mod[i,1000]===0,Print["\t\tconverting IBP to Kira expression... (",i,"/",Length[ibps],")"]];
	OneIBPtoKiraString[ibps[[i]]],
{i,Length[ibps]}],"\n\n"]





userdefinedsystem=StringReplace[IBPstoKiraString[userdefinedinput]," "->""];





mlist=IntegralLabelling/@mlist
reducelist=IntegralLabelling/@reducelist





basis=StringReplace[StringRiffle[ToString[InputForm[#]]&/@mlist,"\n\n"]," "->""]
list=StringReplace[StringRiffle[ToString[InputForm[#]]&/@reducelist,"\n"]," "->""]


kiraInputFolder=convertPath<>"KiraIO/"
If[!DirectoryQ[#],CreateDirectory[#]]&@kiraInputFolder
If[!DirectoryQ[#],CreateDirectory[#]]&@(kiraInputFolder<>kiraUDSSubFolder)


Export[
	kiraInputFolder<>kiraUDSSubFolder<>"userdefinedsystem.kira",
	userdefinedsystem,
	"Text"
]


Export[kiraInputFolder<>"basis",basis,"Text"]
Export[kiraInputFolder<>"list",list,"Text"]


Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]
