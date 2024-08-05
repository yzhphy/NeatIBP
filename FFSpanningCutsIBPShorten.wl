(* ::Package:: *)

(*
If in cut A there is a MI, then in cut B we can set that MI to 0
This requires that the spanning cuts pass consistency check.
*)


(*
2024.8.3 comment:
I don't know why this script is named "FF"SpaningCutsIBPShorten
Maybe this relies on "FF"SpanningCutsConsistencyCheck.wl
But this script, itself, dose not seem to rely on FF

*)


commandLineMode=True





If[commandLineMode,
	packagePath=DirectoryName[$InputFileName];
	(*workingPath=$CommandLine[[-1]];(*we can specify working path in command line*)
	If[workingPath==="-script",workingPath=Directory[]<>"/"];
	If[!DirectoryQ[workingPath],
		Print["Warning: the argument working path ",workingPath," does not exist."];
		workingPath=Directory[]<>"/";
		Print["\t\t redefining working path as current working folder: ",workingPath,"."];
	];*)
	workingPath=Directory[]<>"/";(*is this really used?*)
	checkPath=$CommandLine[[-1]];
	MathematicaCommand=Import[packagePath<>"/preload/MathematicaCommand.txt"];
	ShellProcessor=Import[packagePath<>"/preload/ShellProcessor.txt"];
	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";
	checkPath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/examples_private/Examples_in_the_paper/NP7-spanning_cuts_test/outputs/2l4pNP7_spanning_cuts_test-5"
	
]








If[StringSplit[checkPath,""][[-1]]=!="/",checkPath=checkPath<>"/"]


md5code=Get[checkPath<>"results/spanning_cuts_consistency_check_passed_certificate.txt"];
If[md5code=!=Hash[Import[checkPath<>"results/summary.txt","Text"],"MD5"],
	Print["*****The spanning cuts results in "<>checkPath<>" has not passed consistency check! Cannot shorthen IBP. \n*****Exiting..."];
	Exit[0];
]


SetDirectory[workingPath]


TimeString[]:=Module[{at},at=FromAbsoluteTime[AbsoluteTime[]];StringRiffle[#,"_"]&[(ToString[Floor[#]]&/@at[[1,1;;6]])]<>"_"<>ToString[Floor[1000*(#-Floor[#])&[ at[[1,6]]]]]]





If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
Get[checkPath<>"inputs/config.txt"]
(*If[outputPath===Automatic,
	outputPath=workingPath<>"outputs/"<>ReductionOutputName<>"/";
	Print["Output path has been set as "<>outputPath]
]*)
outputPath=checkPath


TemporaryDirectory=outputPath<>"tmp"

(*Get[packagePath<>"SyzygyRed.wl"]*)

Get[packagePath<>"SparseRREF/SparseRREF.m"]



SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse






Print["=================================================="];
Print["Shortening IBPs at ",checkPath]
Print["-------------------------------------------------"]


targetFileName=FileNameSplit[targetIntegralsFile][[-1]]
kinematicsFileName=FileNameSplit[kinematicsFile][[-1]]


spanningCuts=Get[outputPath<>"tmp/spanningCuts.txt"]






Print["Reading IBPs..."];
timer=AbsoluteTime[];
For[i=1,i<=Length[spanningCuts],i++,
	cut=spanningCuts[[i]];
	cutFolder=outputPath<>"results/results_spanning_cuts/cut_"<>
		StringRiffle[ToString/@cut,"_"]<>"/";
	cutIBPs[cut]=Get[cutFolder<>"results/IBP_all.txt"];
	cutMIs[cut]=Get[cutFolder<>"results/MI_all.txt"];
]
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];





timer=AbsoluteTime[];
Print["Sorting cuts..."];
CutOrdering[cut_]:={
	Length[cutIBPs[cut]],
	Length[Cases[Variables[cutIBPs[cut]],_G]],
	ByteCount[cutIBPs[cut]],
	LeafCount[cutIBPs[cut]]
}
spanningCutsSorted=SortBy[spanningCuts,CutOrdering];
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];

(*
targets=Get[outputPath<>"inputs/"<>targetFileName];
Get[outputPath<>"inputs/"<>kinematicsFileName];

SDim=Length[Cases[Variables[{targets,allMIs}],_G][[1]]/.G->List];

targetReducedTable=Table[TargetReducedOnCut[targets[[i]],#]&/@spanningCuts,{i,Length[targets]}];
Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];
*)






For[i=1,i<=Length[spanningCutsSorted],i++,
	timer=AbsoluteTime[];
	Print["Shortening IBPs on cut "<>ToString[InputForm[cut]]<>" (",i,"/",Length[spanningCutsSorted],")"];
	cut=spanningCutsSorted[[i]];
	formerCuts=spanningCutsSorted[[1;;i-1]];
	eliminatableMIs=Flatten[cutMIs/@formerCuts]//Union;
	currentCutIBPs=cutIBPs[cut];
	currentCutIBPsShortened=currentCutIBPs/.(#->0&/@eliminatableMIs);
	cutFolder=outputPath<>"results/results_spanning_cuts/cut_"<>
		StringRiffle[ToString/@cut,"_"]<>"/";
	Export[cutFolder<>"results/IBP_all_shortened.txt",currentCutIBPsShortened//InputForm//ToString];
	Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."];
]






Print["==========================\nspanning cuts IBP shorten finished."]
Export[checkPath<>"tmp/spanning_cuts_IBP_shorten_finished.txt",""]

