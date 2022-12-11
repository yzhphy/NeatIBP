(* ::Package:: *)

commandLineMode=True





If[commandLineMode,
	packagePath=DirectoryName[$InputFileName];
	workingPath=Directory[]<>"/";
	missionInput=$CommandLine[[-1]];

	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	missionInput="inputs_and_config.txt";
	LoopMomenta={l1,l2};
	ExternalMomenta={k1,k2,k4};
	Propagators={l1^2,(l1-k1)^2,(l1-k1-k2)^2,(l2+k1+k2)^2,(l2-k4)^2,l2^2,(l1+l2)^2,(l1+k4)^2,(l2+k1)^2};
	Kinematics={k1^2->0,k2^2->0,k4^2->0,k1 k2->s/2,k1 k4->t/2,k2 k4->(-s/2-t/2)};
	GenericPoint={s->-1,t->-3}; 
	TargetIntegrals={G[1,1,1,1,1,1,1,-5,0],G[1,1,1,1,1,1,1,-4,-1],G[1,1,1,1,1,1,1,-1,-4]}
	
]








(*AppendTo[$Path,workingPath];*)
If[Get[packagePath<>"default_settings.txt"]===$Failed,Exit[0]]
If[Get[workingPath<>missionInput]===$Failed,Print["Unable to open config file "<>workingPath<>missionInput<>". Exiting.";Exit[]]]
If[Get[kinematicsFile]===$Failed,Print["Unable to open kinematics file "<>kinematicsFile<>". Exiting.";Exit[]]]
(*TargetIntegrals=Get[targetIntegralsFile]
If[TargetIntegrals===$Failed,Print["Unable to open target intergals file "<>targetIntegralsFile<>". Exiting.";Exit[]]]

*)



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
If[!FileExistsQ[tmpPath<>"initialized.txt"],
	Print["Initialization failed, cannot summarize."];
	Exit[0];
]


TemporaryDirectory=outputPath<>"tmp"
Get[packagePath<>"SyzygyRed.wl"]






SectorNumberToSectorIndex//ClearAll
SectorNumberToSectorIndex[num_]:=IntegerDigits[num,2,Length[Propagators]]//Reverse






Print["Summarizing..."];

fileNamesMI=FileNames[All,outputPath<>"results/MI/"];
sectorIDsMI=ToExpression[StringReplace[FileNameSplit[#][[-1]],{".txt"->""}]]&/@fileNamesMI;
MIs=Get/@fileNamesMI
ordering=SortBy[Range[Length[sectorIDsMI]],{-Total[SectorNumberToSectorIndex[sectorIDsMI[[#]]]],-sectorIDsMI[[#]]}&]
(*Print[{Total[SectorNumberToSectorIndex[sectorIDsMI[[#]]]],sectorIDsMI[[#]]}&/@Range[Length[sectorIDsMI]]]*)
sectorIDsMI=sectorIDsMI[[ordering]]
MIs=MIs[[ordering]]

fileNamesIBP=FileNames[All,outputPath<>"results/IBP/"];
sectorIDsIBP=ToExpression[StringReplace[FileNameSplit[#][[-1]],{".txt"->""}]]&/@fileNamesIBP;
IBPs=Get/@fileNamesIBP
ordering=SortBy[Range[Length[sectorIDsIBP]],{-Total[SectorNumberToSectorIndex[sectorIDsIBP[[#]]]],-sectorIDsIBP[[#]]}&]
sectorIDsIBP=sectorIDsIBP[[ordering]]
IBPs=IBPs[[ordering]]


SDim=Length[Cases[Variables[IBPs],_G][[1]]/.G->List]

timer=AbsoluteTime[];
relavantIntegrals=Get/@FileNames[All,outputPath<>"tmp/relavant_integrals/"];
(*integralList=IntegralList[relavantIntegrals]*)
integralList=IntegralList[IBPs]
Export[outputPath<>"results/OrderedIntegrals.txt",integralList//InputForm//ToString];
Export[outputPath<>"results/MI_all.txt",MIs//Flatten//InputForm//ToString];
Export[outputPath<>"results/IBP_all.txt",IBPs//Flatten//InputForm//ToString];

Print["\tDone. Time Used: ", Round[AbsoluteTime[]-timer], " second(s)."]




Print["=================Summary==================="]


Print["Total MI number: ",Length[Flatten[MIs]]]
Print["Total IBP number: ",Length[Flatten[IBPs]]]
Print["Total integral number: ",Length[Flatten[integralList]]]
Print["---------------MIs-------------------------"]
Print[Flatten[MIs]//InputForm//ToString]
Print["---------------MI numbers------------------"]
For[i=1,i<=Length[fileNamesMI],i++,
	sector=SectorNumberToSectorIndex[sectorIDsMI[[i]]];
	MI=MIs[[i]];
	If[Length[MI]>0,Print["sector ",sector," :\t",Length[MI]," MI(s)"]]
]

Print["---------------IBP numbers------------------"]
For[i=1,i<=Length[fileNamesIBP],i++,
	sector=SectorNumberToSectorIndex[sectorIDsIBP[[i]]];
	IBP=IBPs[[i]];
	If[Length[IBP]>0,Print["sector ",sector," :\t",Length[IBP]," IBPs(s)"]]
]



