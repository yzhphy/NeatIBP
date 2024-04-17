(* ::Package:: *)

(*
how to use in shell:
math -script KiraToNeatIBP.wl [settings]

settings must include:
--kinematics_file=.... 
--integral_families_file=.... 
--integral_family_name=... 
--targets_file=...

it may also include:
--symmetry=Ture/False 
--degbound=... 
--modulus=...
--option_simplification=... 
--seeding_additional_degree=...
*)


(* ::Section::Closed:: *)
(*Read in informations*)


commandLineMode=True


If[commandLineMode,
	(*packagePath=DirectoryName[$InputFileName];*)
	workingPath=Directory[]<>"/";
	NeatIBPPackagePath=Environment["NEATIBPPATH"];
	SingularApp=Environment["SINGULARAPP"];
	SpaSMLib=Environment["SPASMLIB"];
	
	commandLine=$CommandLine;
	settingStrings=Select[commandLine,StringContainsQ[#,"--"]&&StringContainsQ[#,"="]&];
	settingPairs=StringSplit[StringReplace[#,"--"->""],"=",All]&/@settingStrings;
	(*Export[workingPath<>"1.txt",settingPairs//InputForm//ToString];*)
	settingKeys=settingPairs[[All,1]];
	Module[{i,key},
		For[i=1,i<=Length[settingKeys],i++,
			key=settingKeys[[i]];
			If[key==="",
				Print["Key not specified in argument ",settingStrings[[i]]\:ff0c". Abort."];
				Exit[];
			];
			
			If[settingPairs[[i,2]]==="",
				Print["Value not specified in argument ",settingStrings[[i]]\:ff0c". Abort."];
				Exit[];
			];
			If[settingPairs[[i,2]]==="Unidentified",
				Print["Word Unidentified cannot be used in argument ",settingStrings[[i]]\:ff0c". Abort."];
				Exit[];
			];
			
			If[Count[settingKeys,key]>1,
				Print["Key ",key\:ff0c" is specified by more than once. Abort."];
				Exit[];
			];
		];
	];
	settingRules=#[[1]]->#[[2]]&/@settingPairs;
,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	(*packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";*)
	NeatIBPPackagePath=$Failed;
	SingularApp=$Failed;
	SpaSMLib=$Failed;
	settingRules={};
]


Print["Generating NeatIBP input files from kira inputs..."]


(* ::Section:: *)
(*Check input and settings*)


If[NeatIBPPackagePath===$Failed,
	Print["NeatIBP package path $NEATIBPPATH not specified. Abort."];
	Exit[];
];
If[SingularApp===$Failed,
	Print["Singular app $SINGULARAPP not specified. Abort."];
	Exit[];
];
If[SpaSMLib===$Failed,
	Print["SpaSM lib package path $SPASMLIB not specified. Abort."];
	Exit[];
];
If[StringSplit[NeatIBPPackagePath,""][[-1]]=!="/",NeatIBPPackagePath=NeatIBPPackagePath<>"/"]


ReadSetting[key_]:=If[MemberQ[settingRules[[All,1]],key],
	Return[key/.settingRules]
,
	Return["Unidentified"]
]

ReadMandatorySetting[key_]:=Module[{result},
	result=ReadSetting[key];
	If[result==="Unidentified",Print["Missing value for ",key,". Abort."];Exit[];];
	result
]
kinematicsFile=ReadMandatorySetting["kinematics_file"];
familiesFile=ReadMandatorySetting["integral_families_file"];
familyName=ReadMandatorySetting["integral_family_name"];
(*we assume that the name in the command line is NOT queted with ""! different from yaml file!*)
targetFile=ReadMandatorySetting["targets_file"];

mandatoryKeys={
"kinematics_file",
"integral_families_file",
"integral_family_name",
"targets_file"
}


(* ::Section:: *)
(*ExtraSettings*)


(* ::Subsection:: *)
(*RecursiveToExpression*)


RecursiveToExpression[str_]:=Module[{i,result},
	result=str;
	For[i=1,i<=200,i++,
		If[Head[result]===String,
			result=ToExpression[result]
		,
			Break[]
		]
	];
	If[i>=100,Print["Warning in RecursiveToExpression: possible abnormal expression ",str]];
	result
]





(* ::Subsection:: *)
(*translation*)


translationToNeatIBPSettings={
	(*"cut"\[Rule] "CutIndices",*)(*this is expected to read from yaml file*)
	"symmetry"->"NeedSymmetry",
	"degbound"->"NeatIBPIntersectionDegreeBound",
	"modulus"->"FiniteFieldModulus",
	"option_simplification"->"OptionSimplification",
	"seeding_additional_degree"->"SeedingAdditionalDegree"
}





extraSettingKeys=translationToNeatIBPSettings[[All,1]]
irrKeys=Complement[settingKeys(*from command line*),extraSettingKeys,mandatoryKeys]
If[irrKeys=!={},
	Print["KiraToNeatIBP warning: irrelavant key(s) ", irrKeys, " appeared. Is this a typo?"]
];
specifiedSettings=Select[translationToNeatIBPSettings[[All,1]],ReadSetting[#]=!="Unidentified"&]


extraSettingsString=StringRiffle[
	(#/.translationToNeatIBPSettings)<>"="<>ReadSetting[#]<>";"&/@specifiedSettings,
	"\n"
]





(* ::Section:: *)
(*Yaml Analyze*)


(* ::Subsection:: *)
(*YamlFileAnalyze*)


(* ::Subsubsection::Closed:: *)
(*NodeAdd*)


(*add a node at tree with some index as its sub node*)
NodeAdd::NodeNotFound="The node with `1` not found in layer `2`. Returning the original tree.";
NodeAdd[tree_,index_,content_]:=Module[{result,index2,i,position},
	result=tree;
	index2={2};
	For[i=1,i<=Length[index],i++,
		position=index[[i]];
		If[Or[
			position>Length[result[[index2/.List->Sequence]]],
			position===0,
			position<-Length[result[[index2/.List->Sequence]]]
		],
			Message[NodeAdd::NodeNotFound,index[[1;;i]],i];
			Return[tree];
		];
		index2=Join[index2,{position,2}]
	];
	result[[index2/.List->Sequence]]=Append[result[[index2/.List->Sequence]],Node[content,{}]];
	result
]


(* ::Subsubsection:: *)
(*YamlStringAnalyze*)


CountIndentSpaces[line_]:=Module[{chars,i},
	chars=StringSplit[line,""];
	i=1;
	While[True,
		If[chars[[i]]===" ",i+=1,Break[]];
	];
	i-1
]


YamlStringAnalyze//ClearAll
YamlStringAnalyze::UnexpectedIndent="Unexpected Indent at line \"`1`\". Failed analyzing.";
Options[YamlStringAnalyze]={KeepSpace->False}
YamlStringAnalyze[inputStr_,OptionsPattern[]]:=Module[
{str,lines,lineIndentSpaces,indentSpacesSet,lineLayers,dataTree,i,
layer,content
},
	str=inputStr;
	str=StringReplace[str,{"#"~~Shortest[___]~~EndOfLine->""}];(*decomment*)
	lines=StringSplit[str,"\n"];
	lines=Select[lines,Complement[StringSplit[#,""],{" "}]=!={}&];(*remove space lines*)
	lineIndentSpaces=CountIndentSpaces/@lines;
	indentSpacesSet=Sort[lineIndentSpaces//Union];
	lineLayers=Position[indentSpacesSet,#]&/@lineIndentSpaces//Flatten;
	(*Print[lineIndentSpaces];*)
	(*start to reed data into tree structure*)
	dataTree=Node[Null,{}];
	layer=0;
	For[i=1,i<=Length[lines],i++,
		If[lineLayers[[i]]-layer>1,
			Message[YamlStringAnalyze::UnexpectedIndent,lines[[i]]];
			Return[$Failed];
		];
		If[OptionValue[KeepSpace],content=lines[[i]],content=StringReplace[lines[[i]]," "->""]];
		dataTree=NodeAdd[dataTree,Table[-1,lineLayers[[i]]-1],content];
		layer=lineLayers[[i]];
	];
	dataTree
]


(* ::Subsubsection::Closed:: *)
(*SquareBracketToList*)


(*cannot deal with pure string, must start at [, and many other assumptions, no warning if input is not suitable*)
(*can be improved, but may be later*)
SquareBracketToList::ColonInList="This version dose not accept colon(s) in list expression in `1`. Returning the original string."
SquareBracketToList[inputStr_]:=Module[{str,tree,layer,word,mode,charList,i,char},
	str=StringReplace[inputStr," "->""];(*despace, forced*)
	If[StringContainsQ[str,":"],Message[SquareBracketToList::ColonInList,inputStr];Return[$Failed]];
	charList=StringSplit[str,""];
	tree=Node[Null,{}];
	layer=0;
	word="";
	For[i=1,i<=Length[charList],i++,
		char=charList[[i]];
		Switch[char,
		"[",
			tree=NodeAdd[tree,Table[-1,layer],List];
			layer+=1;
			word="";
		,
		",",
			If[!MemberQ[{"[",",","]"},charList[[i-1]]],
				tree=NodeAdd[tree,Table[-1,layer],word]
			];
			word="";
		,
		"]",
			If[!MemberQ[{"[",",","]"},charList[[i-1]]],
				tree=NodeAdd[tree,Table[-1,layer],word]
			];
			word="";
			layer-=1;
		,
		_,
			word=word<>char;
		];
		(*Print[word]*)
	];
	(*Print[tree];*)
	tree/.Node[Null,{x___}]:>x//.Node[List,x___]:>x/.Node[x___,{}]:>x
];
StringHandleForSquareBrackets[str_]:=If[
	StringContainsQ[str,"]"]&&StringContainsQ[str,"["],
	SquareBracketToList[str],
	str
]


(* ::Subsubsection:: *)
(*YamlNodeAnalyze*)


YamlNodeAnalyze::UnexpectedGramma="Unexpected `1` in string `2`. Failed analyzing.";
YamlNodeAnalyze::UnidentifiedLineType="Unidentified line type in `1`. Failed analyzing.";
YamlNodeAnalyze::NoKey="No key specified in line `1`. Failed analyzing.";
(*we assume we do not accept colons in brackets, if so, no warning, unknown error occurs *)
YamlNodeAnalyze[content_,subnodes_]:=Module[{strSeg,key,value,listEntry},
	If[content===Null,Return[subnodes]];
	Switch[
	{
		StringContainsQ[content,":"],
		StringSplit[StringReplace[content," "->""],""][[1]]==="-"
		(*the first minus sign in a line will be interpreted as the beginning of a list entry!*)
	}
	,
	{True,False},(*Rules mode*)
		strSeg=StringSplit[content,":",All];
		If[Length[strSeg]=!=2,
			Message[YamlNodeAnalyze::UnexpectedGramma,"colons",content];
			Return[$Failed];
		];
		{key,value}=strSeg;
		If[StringReplace[key," "->""]==="",
			Message[YamlNodeAnalyze::NoKey,content];
			Return[$Failed];
		];
		Switch[
		{
			StringReplace[value," "->""]==="",
			subnodes==={}
		}
		,
		{True,False},
			Return[key->subnodes];
		,
		{False,True},
			Return[key->StringHandleForSquareBrackets[value]];
		,
		{True,True},
			Return[key->""];
		,
		{False,False},
			Message[YamlNodeAnalyze::UnexpectedGramma,"structure",content<>StringRiffle[subnodes[[All,1]],"\n"]];
			Return[$Failed]
		];
	,
	{False,True},(*list mode*)
		Switch[
		{
			StringReplace[content," "->""]==="-",(*is this a list of list?*)
			subnodes==={}
		}
		,
		{True,False},
			Return[subnodes];
		,
		{False,True},
			listEntry=StringRiffle[StringSplit[content,"-",All][[2;;-1]],"-"];
			(*this is my first time to use this StringSplit[...,All], be carefull*)
			Return[StringHandleForSquareBrackets[listEntry]];
		,
		{True,True},
			Return[""];
		,
		{False,False},
			listEntry=StringRiffle[StringSplit[content,"-",All][[2;;-1]],"-"];
			(*this is my first time to use this StringSplit[...,All], be carefull*)
			Return[Join[{StringHandleForSquareBrackets[listEntry]},subnodes]];
			(*sub nodes of a list element will be interpreted as in a same list of the current line
			i.e.
				- A
				  B
			is same as
				-
				  A
				  B
			*)
		];
	,
	{True,True},(*rule in list*)
		listEntry=StringRiffle[StringSplit[content,"-",All][[2;;-1]],"-"];
		(*this is my first time to use this StringSplit[...,All], be carefull*)
		strSeg=StringSplit[listEntry,":",All];
		If[Length[strSeg]=!=2,
			Message[YamlNodeAnalyze::UnexpectedGramma,"colons",content];
			Return[$Failed];
		];
		{key,value}=strSeg;
		If[StringReplace[key," "->""]==="",
			Message[YamlNodeAnalyze::NoKey,content];
			Return[$Failed];
		];
		Switch[
		{
			StringReplace[value," "->""]==="",
			subnodes==={}
		}
		,
		{True,False},(*a single rule with value as a list or a list of rule ("ordered dictionary")*)
			Return[key->subnodes];
		,
		{False,True},(*a single rule poiting to some value written in a line*)
			Return[key->StringHandleForSquareBrackets[value]];
		,
		{True,True},
			Return[key->""];
		,
		{False,False},
		(*if so, the list element itself is a list, with its first element a rule (corresponding to the current line)
			i.e.
				- A:B
				  C
			is same as
				-
				  A:B
				  C
		*)
			Return[Join[
				{key->StringHandleForSquareBrackets[value]},
				subnodes
			]]
		];
	,
	{False,False},
		Message[YamlNodeAnalyze::UnidentifiedLineType,content];
		Return[$Failed];
	];
]


(* ::Subsubsection:: *)
(*YamlFileAnalyze*)


YamlDataTreeAnalyze[dataTree_]:=dataTree/.Node->YamlNodeAnalyze
YamlFileAnalyze::BraceOccurred="This version dose not support brace bracket `1` in yaml files. Failed.";
YamlFileAnalyze[file_]:=Module[{str},
	str=Import[file,"Text"];
	If[str===$Failed,Return[$Failed]];
	If[StringContainsQ[str,"{"],Message[YamlFileAnalyze::BraceOccurred,"{"];Return[$Failed]];
	If[StringContainsQ[str,"}"],Message[YamlFileAnalyze::BraceOccurred,"}"];Return[$Failed]];
	str//YamlStringAnalyze//YamlDataTreeAnalyze
]



(* ::Section:: *)
(*Read Kinematics*)


(* ::Subsection::Closed:: *)
(*Readers*)


(*to be solved, thus not a rule*)
ScalarproducRuleReader[list_]:={
	Times@@(ToExpression/@(list[[1]])),
	list[[2]]//ToExpression
}


PropagatorReader[list_,momenta_]:=Module[{momentumTerm,massTerm,i,a},
	massTerm=RecursiveToExpression[list[[2]]];
	momentumTerm=RecursiveToExpression[list[[1]]];
	If[Exponent[momentumTerm/.(#-># a&/@momenta),a]===1,
		momentumTerm=momentumTerm^2
	];
	momentumTerm-massTerm
	
]


(* ::Subsubsection::Closed:: *)
(*SolveReplacements*)


SolveReplacements[replacements_,independentMomenta_,momentaRep_]:=Module[{equations,sol,sp,sps,spRep,spInvRep,result},
	equations=(#[[1]]-#[[2]]&/@replacements)/.momentaRep;
	
	sps=Table[Table[sp[independentMomenta[[j]],independentMomenta[[i]]],{j,1,i}],{i,1,Length[independentMomenta]}]//Flatten;
	spRep=Table[sps[[i]]->(sps[[i]]/.(sp->Times)),{i,Length[sps]}];
	spInvRep=Reverse/@spRep;
	
	equations=Expand[equations]/.spInvRep;
	
	(*Print[equations];
	Print[sps];*)
	sol=Solve[#==0&/@equations,sps];
	If[Length[sol]===0,
		Print["No solution in SolveReplacements" ];
		Return[$Failed]
	];
	If[Length[sol]>1,
		Print["Multiple (",Length[sol],") solution in SolveReplacements" ];
		Return[$Failed]
	];
	sol=sol[[1]];
	result=(#/.spRep)->(#/.sol/.spRep)&/@sps;
	If[Intersection[Variables[result[[All,2]]],independentMomenta]=!={},
		Print["SolveReplacements warning: the replacemented scalar products still contains momenta expression, possible due to lacking conditions or incorrect independent momenta."];
	];
	result
	
	
]


(* ::Subsection:: *)
(*MakeKinematicsFromYamlFiles*)


Options[MakeKinematicsFromYamlFiles]={ReplaceAVariableByOne->False};
MakeKinematicsFromYamlFiles[kinematicsFile_,familiesFile_,familyName_,OptionsPattern[]]:=Module[
{kinematicsData,familiesData,familyData,
incomingMomenta,outgoingMomenta,momentumConservation,kinematicInvariants,scalarproductRules,
loopMomenta,propagators,cutPropagators,
preExternalMomenta,dependentExtMomentum,externalMomenta,externalMomentaRep,sol,spConstrains,allMomenta,symbolRelacedByOne,kinematicVariables
},
	kinematicsData=YamlFileAnalyze[kinematicsFile];
	familiesData=YamlFileAnalyze[familiesFile];
	If[!FreeQ[{familiesData,kinematicsData},$Failed],Return[$Failed]];
	kinematicsData="kinematics"/.kinematicsData;
	familiesData="integralfamilies"/.familiesData;
	If[kinematicsData==="kinematics",Print["MakeKinematicsFromYamlFiles: unable to read kinematics data."]];
	If[familiesData==="integralfamilies",Print["MakeKinematicsFromYamlFiles: unable to read integral families data."]];
	
	familyData=Select[familiesData,ToExpression[("name"/.#)]===familyName&];
	(*we assume that the name in the yaml file is queted with "", thus after ToExpression, it is still a string.*)
	
	If[familyData==={},
		Print["MakeKinematicsFromYamlFiles: No family matches name "<>familyName];
		Return[$Failed]
	];
	
	If[Length[familyData]>1,
		Print["MakeKinematicsFromYamlFiles: Multiple ("<>Length[familyData]<>") family matches name "<>familyName];
		Return[$Failed]
	];
	familyData=familyData[[1]];
	
	(*-----------------------*)
	(*making external momenta*)
	(*-----------------------*)
	incomingMomenta=ToExpression/@("incoming_momenta"/.kinematicsData);
	outgoingMomenta=ToExpression/@("outgoing_momenta"/.kinematicsData);
	momentumConservation=ToExpression/@("momentum_conservation"/.kinematicsData);
	preExternalMomenta=Join[incomingMomenta,outgoingMomenta];
	If[MemberQ[AtomQ/@preExternalMomenta,False],
		Print["MakeKinematicsFromYamlFiles: the external momenta list contains non-atomic expression."];
		Return[$Failed]
	];
	If[Length[DeleteDuplicates[preExternalMomenta]]=!=Length[preExternalMomenta],
		Print["MakeKinematicsFromYamlFiles: the external momenta list contains duplicated elements."];
		Return[$Failed]
	];
	If[Or[Head[momentumConservation]=!=List,Length[momentumConservation]=!=2],
		Print["MakeKinematicsFromYamlFiles: momentum conservation should be a list with length 2."];
		Return[$Failed]
	];
	If[Length[Intersection[Variables[momentumConservation[[1]]],preExternalMomenta]]===1,
		dependentExtMomentum=Intersection[Variables[momentumConservation[[1]]],preExternalMomenta][[1]];
	,
		dependentExtMomentum=preExternalMomenta[[-1]];
	];
	externalMomenta=DeleteCases[preExternalMomenta,dependentExtMomentum];
	sol=Quiet[
		Solve[(momentumConservation.{1,-1})==0,Join[externalMomenta,{dependentExtMomentum}]],
		{Solve::svars}
	];
	If[sol==={},
		Print["MakeKinematicsFromYamlFiles: No solution for momentum conservation ",momentumConservation];
		Return[$Failed]
	];
	externalMomentaRep=sol[[1]];
	
	(*-----------------------*)
	(*making replacements*)
	(*-----------------------*)
	spConstrains=ScalarproducRuleReader/@("scalarproduct_rules"/.kinematicsData);
	scalarproductRules=SolveReplacements[spConstrains,externalMomenta,externalMomentaRep];
	If[scalarproductRules===$Failed,Return[$Failed]];
	
	(*-----------------------*)
	(*making loop momenta*)
	(*-----------------------*)
	loopMomenta=ToExpression/@("loop_momenta"/.familyData);
	(*probe=familyData;*)
	If[MemberQ[AtomQ/@loopMomenta,False],
		Print["MakeKinematicsFromYamlFiles: the loop momenta list contains non-atomic expression."];
		Return[$Failed]
	];
	If[Length[DeleteDuplicates[loopMomenta]]=!=Length[loopMomenta],
		Print["MakeKinematicsFromYamlFiles: the loop momenta list contains duplicated elements."];
		Return[$Failed]
	];
	
	(*-----------------------*)
	(*making propagators*)
	(*-----------------------*)
	allMomenta=Join[loopMomenta,externalMomenta];
	propagators=PropagatorReader[#,allMomenta]&/@("propagators"/.familyData);
	
	propagators=propagators/.externalMomentaRep;
	
	
	(*-----------------------*)
	(*other readings*)
	(*-----------------------*)
	kinematicInvariants=Map[ToExpression,"kinematic_invariants"/.kinematicsData,{2}];
	kinematicVariables=kinematicInvariants[[All,1]];
	If[MemberQ[AtomQ/@kinematicVariables,False],
		Print["MakeKinematicsFromYamlFiles: the kinematic invariants list contains non-atomic expression."];
		Return[$Failed]
	];
	
	cutPropagators=ToExpression/@("cut_propagators"/.familyData);
	If[OptionValue[ReplaceAVariableByOne],
		symbolRelacedByOne=ToExpression["symbol_to_replace_by_one"/.kinematicsData];
		propagators=propagators/.symbolRelacedByOne->1;
		scalarproductRules=scalarproductRules/.symbolRelacedByOne->1;
		kinematicVariables=DeleteCases[kinematicVariables,symbolRelacedByOne]
	];
	
	
	
	
	{
		"LoopMomenta"->loopMomenta,
		"ExternalMomenta"->externalMomenta,
		"Propagators"->propagators,
		"Kinematics"->scalarproductRules,
		"KinematicVariables"->kinematicVariables,
		"CutIndices"->If[cutPropagators==="cut_propagators",{},cutPropagators]
	}

	
]



(* ::Section:: *)
(*ToNeatIBPInputFiles*)


basicConfigString="outputPath=Automatic;\n"<>
"kinematicsFile = workingPath<>\"kinematics.txt\";\n"<>
"targetIntegralsFile = workingPath<>\"targetIntegrals.txt\";\n"<>
"ReductionOutputName=\""<>"kira"<>"\";\n"<>
(*"ReductionOutputName=\""<>familyName<>"\";\n"<>*)
(*not supporting multiple families in a same folder yet...*)
(*if modified here, remember to modify the sh script!*)
"SingularApp="<>"\""<>SingularApp<>"\";\n"<>
"SparseRREF`SpaSMLibrary="<>"\""<>SpaSMLib<>"\";\n"


ToNeatIBPInputFiles[kinematicsFile_,familiesFile_,familyName_,outputFolder_]:=Module[
{data,kinematicsString,configString,numerics,vars},
	If[!DirectoryQ[outputFolder],Print["ToNeatIBPInputFiles: output folder ",outputFolder," does not exist."];Return[$Failed]];
	data=MakeKinematicsFromYamlFiles[kinematicsFile,familiesFile,familyName];
	If[data===$Failed,Return[$Failed]];
	kinematicsString="";
	configString=basicConfigString<>"\n"<>extraSettingsString<>"\n\n";
	
	kinematicsString=(kinematicsString<>#<>"="<>ToString[InputForm[#/.data]]<>";\n")&["LoopMomenta"];
	kinematicsString=(kinematicsString<>#<>"="<>ToString[InputForm[#/.data]]<>";\n")&["ExternalMomenta"];
	kinematicsString=(kinematicsString<>#<>"="<>ToString[InputForm[#/.data]]<>";\n")&["Propagators"];
	kinematicsString=(kinematicsString<>#<>"="<>ToString[InputForm[#/.data]]<>";\n")&["Kinematics"];
	
	vars="KinematicVariables"/.data;
	numerics=#->RandomPrime[50*Length[vars]^2]/RandomPrime[150*Length[vars]^2]&/@Join[vars,{}];
	kinematicsString=kinematicsString<>"GenericPoint="<>ToString[InputForm[numerics]]<>";\n";
	
	configString=configString<>"CutIndices="<>ToString[InputForm["CutIndices"/.data]]<>";\n";
	
	Export[outputFolder<>"kinematics.txt",kinematicsString];
	Export[outputFolder<>"config.txt",configString];
	{kinematicsString,configString}
]









ToNeatIBPInputFiles[kinematicsFile,familiesFile,familyName,workingPath]


(* ::Subsection:: *)
(*target files*)


targetString=Import[targetFile,"Text"];
(*we assume inside the file is a list of integral monomials*)
targets=ToExpression/@DeleteCases[StringSplit[StringReplace[targetString," "->""],"\n"],""]
familyNameHead=ToExpression[familyName]
(*this time ToExpression[familyName] is an expression, not a string*)
If[Union[Head/@targets]=!={familyNameHead},

	Print["target integrals with unexpected heads ",
		ToString[InputForm[DeleteCases[Union[Head/@targets],familyNameHead]]],
		". Abort."
	];
	Exit[];
]
targetsInNeatIBPFormat=targets/.familyNameHead->G
Export[
	workingPath<>"targetIntegrals.txt",
	"{\n"<>StringRiffle[
		ToString[InputForm[#]]&/@targetsInNeatIBPFormat,
		",\n"
	]<>"\n}"
]


Print["Finished."]


(* ::Section::Closed:: *)
(*Check protected Names*)


(*ProtectedNames=
{Node,CheckFree,allProtectedNamesDisappearQ
}//DeleteDuplicates
CheckRange={"TargetIntegrals","LoopMomenta","ExternalMomenta","Propagators","Kinematics","GenericPoint","GenericD"
}
AppearableList={
G->{"TargetIntegrals"},
d->{"GenericD"}
}
CheckFree[name_]:=Module[{range},
	range=CheckRange;
	If[MemberQ[AppearableList[[All,1]],name],range=Complement[range,name/.AppearableList]];
	Select[range,!FreeQ[ToExpression[#],name]&]
]
allProtectedNamesDisappearQ=True
Module[{name,i,checkFreeResult},
	For[i=1,i<=Length[ProtectedNames],i++,
		name=ProtectedNames[[i]];
		checkFreeResult=CheckFree[name];
		If[Length[checkFreeResult]>0,
			Print["****  Protected symbol \"",name,"\" appear in the following input(s):\n\t", checkFreeResult];
			allProtectedNamesDisappearQ=False;
		]
	]
]
If[allProtectedNamesDisappearQ===False,
	Print["****  Please raname the above symbol(s) in your input(s). Exiting..."];
	Exit[0];
]
*)
