(* ::Package:: *)

TimingReportOfRowReduce=True;
LogFile="";

RowReduceFunction=SRSparseRowReduce
debugModification20230314=True;
debugModification20231102=True;


ReportIndent[n_]:=StringRiffle[Table["\t",n],""]


ProbeIntermediateResult[name_,sec_,expr_]:=Module[{intermediateResultFolder},
	If[DebugMode===False,Return[]];
	intermediateResultFolder=outputPath<>"tmp/intermediate_results/"<>name<>"/";
	If[!DirectoryQ[#],Run["mkdir -p "<>#]]&[intermediateResultFolder];
	Export[intermediateResultFolder<>ToString[sec]<>".txt",expr//InputForm//ToString];
	If[SilentExport=!=True,
		PrintAndLog["probed intermediate result into ",intermediateResultFolder<>ToString[sec]<>".txt"];
	];
	
]


ConvenientExport[path_,contents_]:=Module[{folder},
	folder=DirectoryName[path];
	If[!DirectoryQ[folder],Run["mkdir -p "<>folder]];
	Export[path,contents];
	PrintAndLog["Exported contents to ",path];
]





(*
This function appears in many codes
1. SyzygyRed.wl
2. Several or all .wl codes in interfaces/Kira/interface/
3. FFSolveIBP.wl, FFSpanningCutsConsistencyCheck.wl, AssignIBPReduction.wl,SpanningCutsIBPShorten.wl
4. Analyze_Sectors.wl (but here, it also gets SyzygyRed.wl, thus there is overwitting)
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



LT[exp_,var_,order_]:=MonomialList[exp,var,order][[1]];





ListIntersect[set1_,set2_]:=If[MemberQ[set2,#],#,Nothing]&/@set1;   (* Intersection of list, which keeps the ordering in set1 *)


PolynomialDeg[poly_,var_]:=If[poly===0,-\[Infinity],Total[CoefficientRules[poly,var,DegreeReverseLexicographic][[1,1]]]];


monomialAnsatz[var1_,deg_]:=(Times@@MapThread[#1^#2&,{var1,Exponent[#,var1]}])&/@MonomialList[(1+Total[var1])^deg//Expand,var1,DegreeReverseLexicographic]


SimplificationRules::usage="";
ScriptFile::usage="";
ScriptOnly::usage="";
OutputFile::usage="";
HighestDegree::usage="";
zeroDeg::usage="";
WorkingDegree::usage="";
degBound::usage="";
MaxCut::usage="";
primeList::usage="";
TruncationFirst::usage="";
VariableOrder::usage="";
Cut::usage="";


(*pivots[matrix_]:=Module[{ARLonglist},
	ARLonglist=GatherBy[matrix//ArrayRules,#[[1,1]]&];
	Return[#[[1,1,2]]&/@(ARLonglist[[;;-2]])];
];*)


positivity[list_]:=If[Union[#>0&/@list]==Head[list][True],True,False];





(* ::Section:: *)
(*Baikov Representation*)


ScalarTangentVector[rowIndex1_,rowIndex2_]:=Module[{position,vector},
	position=Position[Global`ScalarVar,#][[1,1]]&/@(Global`BaikovMatrix[[rowIndex2]]);
	vector=SparseArray[Table[position[[i]]->If[i==rowIndex2,2,1]Global`BaikovMatrix[[rowIndex1,i]],{i,1,Length[position]}],SDim];
	Return[vector//Normal];
];


Prepare[]:=Module[{Formula,MatrixA,VectorB,ScalarC,BList,AList,Vectorb,lambda},
	ClearAll[m];
	n=Length[ExternalMomenta]+1;
	L=Length[LoopMomenta];
	
	zeroLoopMomenta=(#->0)&/@LoopMomenta;
	Momenta=Join[ExternalMomenta,LoopMomenta];
	SDim=L (L+1)/2+L (Length[ExternalMomenta]);  (* dimension for s_ij's *)
	
	ScalarVarRep=Join[Table[LoopMomenta[[i]] ExternalMomenta[[j]]->x[i,j],{i,1,Length[LoopMomenta]},{j,1,Length[ExternalMomenta]}]//Flatten,Table[LoopMomenta[[i]] LoopMomenta[[j]]->y[i,j],{i,1,Length[LoopMomenta]},{j,i,Length[LoopMomenta]}]//Flatten];
	
	ScalarVar=Join[Table[x[i,j],{i,1,Length[LoopMomenta]},{j,1,Length[ExternalMomenta]}]//Flatten,Table[y[i,j],{i,1,Length[LoopMomenta]},{j,i,Length[LoopMomenta]}]//Flatten];
	
	
	If[Length[Propagators]=!=SDim,
		
		Return["Propagators Length and SDim mismatch."]
	];
	(* Propagator search *)
	
	(*If[Length[Propagators]<SDim,
		SGr=GroebnerBasis[Expand[Propagators]/.Kinematics/.ScalarVarRep,ScalarVar//Reverse,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
		RSP=LT[#,ScalarVar//Reverse,DegreeReverseLexicographic]&/@SGr;
		RSP=Flatten[Intersection[Variables[#],ScalarVar]&/@RSP];
		ISP=Complement[ScalarVar,RSP];
		ScalarVarRepBack2=Join[Table[x[i,j]->(LoopMomenta[[i]]+ExternalMomenta[[j]])^2,{i,1,Length[LoopMomenta]},{j,1,Length[ExternalMomenta]}]//Flatten,Table[y[i,j]->(LoopMomenta[[i]]+LoopMomenta[[j]])^2,{i,1,Length[LoopMomenta]},{j,i,Length[LoopMomenta]}]//Flatten];
		ISP=ISP/.ScalarVarRepBack2;
		PrintAndLog["more propagators added ... ",ISP];
		Propagators=Join[Propagators,ISP];
	]; *)
	
	
	
	var=Table[z[i],{i,1,Length[Propagators]}];
	BList=Table[B[i],{i,1,Length[Propagators]}];
	AList=Table[A[i],{i,1,Length[Propagators]}];
	
	BaikovMatrix=Table[Momenta[[i]] Momenta[[j]]//Expand,{i,1,Length[Momenta]},{j,1,Length[Momenta]}]/.Kinematics/.ScalarVarRep;

	GramMatrix=BaikovMatrix[[1;;n-1,1;;n-1]];
	LoopExternalScalars=Table[x[i,j],{i,1,L},{j,1,n-1}];
	(* rowIndex2 must correspond to the loop component *) 
	ScalarTangentSet=Flatten[Table[ScalarTangentVector[i,j],{i,1,L+n-1},{j,n,L+n-1}],1];
	
	(* Extended tangent set, WITH cofactor*)
	ScalarExtendedTangentSet=Flatten[Table[If[i!=j,Append[ScalarTangentVector[i,j],0],Append[ScalarTangentVector[i,j],-2]],{i,1,L+n-1},{j,n,L+n-1}],1];
	
	
	
	
	BaikovKernelScalar=Det[BaikovMatrix];
	MatrixA=Coefficient[Expand[#]/.Kinematics/.ScalarVarRep,ScalarVar]&/@Propagators;
	Vectorb=(Expand[#]/.zeroLoopMomenta/.Kinematics)&/@Propagators;
	If[MatrixRank[MatrixA]=!=SDim,
		Return["Propagators is not a well-defined independent basis."]
	];
	(* z' s = MatrixA.ScalarVar + Vectorb, conversion between s_ij (x_ij here) to Baikov z's *)
	
	BaikovRevRep=MapThread[#1->#2&,{var,MatrixA . ScalarVar+Vectorb}];
	BaikovRep=MapThread[#1->#2&,{ScalarVar,Inverse[MatrixA] . (var-Vectorb)}];
	BaikovKernel=BaikovKernelScalar/.BaikovRep;
	
	Parameters=Select[Variables[BaikovMatrix/.BaikovRep],!Head[#]===z&];
	If[ParameterRepermute===True,
		If[Sort[ParameterRepermuteIndex]=!=Range[Length[Parameters]],
			PrintAndLog["***** Error, wrong permutation ",ParameterRepermuteIndex];
			Exit[0];
		,
			Parameters=Parameters[[ParameterRepermuteIndex]];
		]
	];
	
	TangentSet=MatrixA . #&/@ScalarTangentSet/.BaikovRep//Factor;
	ExtendedTangentSet=Transpose[Join[Transpose[TangentSet],{Transpose[ScalarExtendedTangentSet]//Last}]]; (* including the cofactors *)
	
	ForwardRep[1]=Join[
		Table[z[i]->ToExpression["z"<>ToString[i]],{i,1,SDim}],
		Table[Parameters[[i]]->ToExpression["c"<>ToString[i]],{i,1,Parameters//Length}],
		{AuxKinVar->c0}
	];
	BackwardRep[1]=Reverse/@ForwardRep[1];
	(*ForwardRep[2]=Join[Table[ScalarVar[[i]]->ToExpression["z"<>ToString[i]],{i,1,SDim}],Table[Parameters[[i]]->ToExpression["c"<>ToString[i]],{i,1,Parameters//Length}]];
	BackwardRep[2]=Reverse/@ForwardRep[2];*)

	Scalar2sp=#[[2]]->(#[[1]]/.{x_ y_->sp[x,y],x_^2->sp[x,x]})&/@ScalarVarRep; 
	sp2Scalar=Join[Reverse/@Scalar2sp,
		Table[sp[ExternalMomenta[[i]],ExternalMomenta[[j]] ]->(ExternalMomenta[[i]]*ExternalMomenta[[j]]/.Kinematics),{i,1,Length[ExternalMomenta]},{j,1,Length[ExternalMomenta]}]//Flatten
		];
	
	PropagatorScalar=Expand[Propagators]/.ScalarVarRep/.Kinematics;
	
	(* Feynman representation  *)
	
	Formula=(Propagators . var)/.(#->lambda #&/@LoopMomenta);
	MatrixA=D[SeriesCoefficient[Formula,{lambda,0,2}],{LoopMomenta},{LoopMomenta}]/2;	
	VectorB=Coefficient[SeriesCoefficient[Formula,{lambda,0,1}],LoopMomenta]/2;
	ScalarC=SeriesCoefficient[Formula,{lambda,0,0}];
	Global`PolynomialU=Det[MatrixA]//Factor;
	Global`PolynomialF=-PolynomialU (Expand[ScalarC]/.Kinematics)+ Cancel[PolynomialU (Expand[VectorB . Inverse[MatrixA] . VectorB]/.Kinematics)]//Expand;
	Global`PolynomialG=PolynomialU+PolynomialF;
	Global`numericPolynomialG=PolynomialG/.GenericPoint;
	PrintAndLog["L=",L," E=",Length[ExternalMomenta]];
	PrintAndLog["Parameters ",Parameters];
	PrintAndLog["Baikov variables: var=",var];
	
	
];


(* Get two tangent modules for the problem *)


TangentModules[propIndex_,cutIndex_]:=Module[{M1,M2,M1ext,cut,ISPcondition},	
		cut=z[#]->0&/@cutIndex;
		
		ISPcondition=#->1&/@(Complement[Global`var,z[#]&/@propIndex]);
		
		M1=Global`TangentSet/.cut;
		M1ext=Global`ExtendedTangentSet/.cut;
		M2=DiagonalMatrix[var]/.ISPcondition/.cut;
		Return[{M1,M1ext,M2}];

];
MatrixOutput[sector_]:=Module[{Modules},
	Modules=TangentModules[sector//SectorIndex,{}];
	Return[{Modules[[1]] . (gen/@Range[SDim]),Modules[[3]] . (gen/@Range[SDim])}];

]



(*Print["If you see an error message saying cannot open LinearSyzForLinearModule_FF_v2.wl, you can ignore it for the current version:"]
Get["/home/zihao/projects/SyzygyRed/LinearSyz/LinearSyzForLinearModule_FF_v2.wl"]*)


(* ::Section::Closed:: *)
(*Cut*)


SingularIdeal[propIndex_,cutIndex_]:=Module[{cut,FF1,SingularIdeal},
	 cut=z[#]->0&/@cutIndex;
	 FF1=Global`BaikovKernel/.cut;
	 SingularIdeal=Join[D[FF1,{Global`var}],{FF1}];
	 Return[SingularIdeal];
];


(* ::Section::Closed:: *)
(* Sector Tools*)


Sector[int_]:=Table[If[int[[j]]>0,1,0],{j,1,Length[int]}]
SectorIndex[int_]:=Table[If[int[[j]]>0,j,Nothing],{j,1,Length[int]}]
Index2Sector[propIndex_]:=Exponent[Times@@(z/@propIndex),var];


SectorHeight[int_]:=Count[int/. G->List,u_/;u>0]


SectorCut[sector_]:=G[x__]:>(If[Sector[G[x]]===sector,1,0])*G[x];


SectorCutQ[sec_,cut_]:=And@@(#>=0&/@(sec-cut));

SectorNumber[sec_]:=FromDigits[sec//Reverse,2];



Gcut[sectorIndex_]:=Table[If[sectorIndex[[i]]<=0,z[i]->0,Nothing],{i,1,SDim}];


ZeroSectorQ[sectorIndex_]:=Module[{G,scaling,linearEquation,coefficentlist,scalingResult,zlist,k,eqns},
	G=Global`numericPolynomialG/.Gcut[sectorIndex];
	zlist=Variables[var/.Gcut[sectorIndex]]//Sort;
	scaling=Table[k[i],{i,Length[zlist]}];
	linearEquation=(Sum[scaling[[i]] zlist[[i]]D[G,zlist[[i]]],{i,Length[zlist]}])-G;
	coefficentlist=CoefficientRules[linearEquation,zlist];
	eqns=#[[2]]&/@coefficentlist;
	scalingResult=GroebnerBasis[eqns,scaling,MonomialOrder->Lexicographic,CoefficientDomain->RationalFunctions];
	If[scalingResult==={1},
		Return[False],
		Return[True]];
]


SectorOrdering[sector_]:={SectorHeight[sector],SectorNumber[sector]};


(* Find the subsectors of one sector *)
SubsectorFinder[sector_]:=Module[{pos,n},
	pos=SectorIndex[sector];
	n=SectorHeight[sector];
	Table[SparseArray[MapThread[#1->#2&,{pos,IntegerDigits[i,2,n]}],SDim]//Normal,{i,0,2^n-1}]//Return;
];   
(* Find the subsectors of a list sectors *)
SubsectorAllFinder[sectorList_]:=Module[{list=sectorList,result={},DiamondSet,i},
	list=SortBy[Union[list],SectorOrdering]//Reverse;
	While[list=!={},
		DiamondSet=SubsectorFinder[list[[1]]];
		list=Select[list,!MemberQ[DiamondSet,#]&];
		list=SortBy[list,SectorOrdering]//Reverse;
		result=Union[result,DiamondSet];
	];
	Return[SortBy[result,SectorOrdering]//Reverse];
	
];

	
	



SectorElimination[sector_]:=(G@@Table[If[sector[[i]]>0,PatternTest[Pattern[ToExpression["m"<>ToString[i]]//Evaluate,Blank[]],Positive],PatternTest[Pattern[ToExpression["m"<>ToString[i]]//Evaluate,Blank[]],NonPositive]],{i,1,SDim}]):>0;








(* ::Section::Closed:: *)
(*Integral Ordering*)


Switch[IntegralOrder,
"MultiplePropagatorElimination",
	IntegralWeight[int_]:=Join[
		{IntegralPropagatorDegree[int]},
		IntegralPropagatorType[int][[
			SectorIndex[Sector[int]]
		]],
		{IntegralISPDegree[int]},
		-IntegralISPType[int][[
			Complement[
				Range[Length[int/.G->List]],
				SectorIndex[Sector[int]]
			]
		]]
	],
"ISPElimination",
	IntegralWeight[int_]:=Join[
		{IntegralISPDegree[int]},
		-IntegralISPType[int][[
			Complement[
				Range[Length[int/.G->List]],
				SectorIndex[Sector[int]]
			]
		]],
		{IntegralPropagatorDegree[int]},
		IntegralPropagatorType[int][[
			SectorIndex[Sector[int]]
		]]
	],
"Global",
	IntegralWeight[int_]:=Join[
		{IntegralAbsDegree[int]},
		Abs/@(int/.G->List)
	]
]





IntegralCut[cut_]:=G[x__]:>If[SectorCutQ[Sector[G[x]],cut],G[x],0];
IntegralDegree[int_]:=(int/.G->List)-Sector[int];
IntegralAbsDegree[int_]:=Abs/@((int/.G->List)-Sector[int]);
IntegralSectorHeight[int_]:=Count[int/.G->List,u_/;u>0];

IntegralPropagatorDegree[int_]:=Sum[If[int[[j]]>1,int[[j]]-1,0],{j,1,Length[int]}]; 
IntegralISPDegree[int_]:=-Total[Select[int/.G->List,#<0&]];
IntegralPropagatorType[int_]:=Table[If[int[[j]]>=1,int[[j]],0],{j,1,Length[int]}]; 
IntegralISPType[int_]:=Table[If[int[[j]]<=0,int[[j]],0],{j,1,Length[int]}]; 

FIntegralSectorISPDegree[fInt_,sector_]:=(IntegralRealization[fInt,Table[0,SDim]]/.G->List) . (sector-1)



IntegralOrdering[int_]:=Join[{IntegralSectorHeight[int],SectorNumber[int//Sector]},IntegralWeight[int]];   (* Key function *)


CollectG//ClearAll
Options[CollectG]={
	RelevantGs -> Automatic,
	CoefficientForm -> Identity
}
CollectG[exp_,OptionsPattern[]]:=Module[{Gs,coeffs,coeffForm},
	Gs=OptionValue[RelevantGs];
	If[Gs===Automatic,
		Gs=Select[Variables[exp],Head[#]==G&];
	];
	coeffs=Coefficient[exp,Gs];
	coeffForm=OptionValue[CoefficientForm];
	If[coeffForm=!=Identity,
		coeffs=coeffForm/@coeffs;
	];
	coeffs.Gs
]


(* ::Subsection::Closed:: *)
(*old or unneeded codes*)


(*IntegralList[IBP_]:=Select[Variables[IBP],Head[#]==G&];*)
(*FIntegralSectorISPDegree[fInt_,sector_]:=((fInt/.m[_]->0)/.G->List).(sector-1)*)
(*IntegralSectorOrder[int_]:=SectorWeightMatrix1[Sector[int]].IntegralAbsDegree[int];*)
(*IntegralReal[indices_]:=Dispatch[Table[m[i]->indices[[i]],{i,1,SDim}]];*)
(*IBPWeight[IBP_]:=Max[Total[IntegralAbsDegree[#]]&/@IntegralList[IBP]];*)
(*IBPWeight[IBP_]:=Max[Total[IntegralAbsDegree[#]]&/@IntegralList[IBP,SortTheIntegrals->False]];*)


(*BlockMatrix[m_,n_]:=Module[{matrix},
	matrix=SparseArray[{},{m+n,m+n}]//Normal;
	Do[matrix[[1,j]]=1,{j,1,m}];
	Do[matrix[[j+1,j]]=1,{j,1,m-1}];
	Do[matrix[[m+1,j+m]]=1,{j,1,n}];
	Do[matrix[[j+m+1,j+m]]=1,{j,1,n-1}];
	Return[matrix];
];(*block degreed lexicographic*)
SectorWeightMatrix[sec_]:=Module[{propIndex,ISPIndex,matrix,i,ip,blockM},
	propIndex=Position[sec,1]//Flatten//Reverse;
	ISPIndex=Position[sec,0]//Flatten//Reverse; (* !!! *)
	matrix=SparseArray[{},{SDim,SDim}]//Normal;
	Switch[Global`IntegralOrder,
			"MultiplePropagatorElimination",
			blockM=BlockMatrix[propIndex//Length,ISPIndex//Length];
			For[i=1,i<=Length[propIndex],i++,
				matrix[[All,propIndex[[i]]]]=blockM[[All,i]];
			];
			For[i=1,i<=Length[ISPIndex],i++,
				matrix[[All,ISPIndex[[i]]]]=blockM[[All,i+Length[propIndex]]];
			];
			,
			"ISPElimination",
			blockM=BlockMatrix[ISPIndex//Length,propIndex//Length];
			For[i=1,i<=Length[ISPIndex],i++,
				matrix[[All,ISPIndex[[i]]]]=blockM[[All,i]];
			];
			For[i=1,i<=Length[propIndex],i++,
				matrix[[All,propIndex[[i]]]]=blockM[[All,i+Length[ISPIndex]]];
			];
			,
			"Global",
			blockM=BlockMatrix[SDim,0];
			For[i=1,i<=Length[propIndex],i++,
				matrix[[All,propIndex[[i]]]]=blockM[[All,i]];
			];
			For[i=1,i<=Length[ISPIndex],i++,
				matrix[[All,ISPIndex[[i]]]]=blockM[[All,i+Length[propIndex]]];
			];
	
	];

	Return[matrix];

];*)


(* ::Section:: *)
(*Singular Interface*)


(* ::Subsection::Closed:: *)
(*orderings*)


Options[SingularOrderingString]={WpOrderingWeight->Automatic}
(*Automatic means, in current version, wp(1,...,1),wp(1,...,1),...,wp(1,...,1),wp(0,...,0)*)
(*comment 2025.1.13:
this is because the last block is treated as parameters.
*)
SingularOrderingString[lens__,OptionsPattern[]]:=Module[{lengthList={lens},result,weight,inputWeight},
	If[SingularMonomialOrdering==="wp"||SingularMonomialOrdering==="Wp",
		result=SingularWpOrderingString[lengthList,WpOrderingWeight->OptionValue[WpOrderingWeight]]
	,
		result=StringRiffle[(SingularMonomialOrdering<>"("<>ToString[#]<>")")&/@DeleteCases[lengthList,0],","]
	];
	result
]
Options[SingularWpOrderingString]={WpOrderingWeight->Automatic}
SingularWpOrderingString[lengthList_,OptionsPattern[]]:=Module[{inputWeight,weight,result},
	inputWeight=OptionValue[WpOrderingWeight];
	If[inputWeight===Automatic,
		weight=Table[Table[If[i===Length[lengthList],0,1],lengthList[[i]]],{i,Length[lengthList]}];
		weight=DeleteCases[weight,{}];
	,
		(*currently not used, maybe useful in the future.*)
		If[Head[inputWeight]=!=List,
			PrintAndLog["***Error: WpOrderingWeight ",inputWeight," is not a well defined weight list. Exiting."];
			Exit[0];
		];
		If[Union[Head/@inputWeight]=!={List},
			PrintAndLog["***Error: WpOrderingWeight ",inputWeight," is not a well defined weight list. Exiting."];
			Exit[0];
		];
		If[(Length/@inputWeight)=!=lengthList,
			PrintAndLog["***Error: WpOrderingWeight ",inputWeight," and length of input ",lengthList," mismatch. Exiting."];
			Exit[0];
		];
		weight=inputWeight;
	];
	
	result=StringRiffle[
		(SingularMonomialOrdering<>"("<>StringRiffle[ToString/@#,","]<>")")&/@weight
	,","];
	result
]







(* ::Subsection::Closed:: *)
(*Singular GB*)


SingularGBText="LIB \"matrix.lib\";
ring R = (MODULUS), (VAR, PARAMETERS), (ORDERINGSTRING);
option(prot);
degBound=DEGBOUND;
module m = MODULE;
module gb=std(m);
write(\"OUTPUTFILE\",string(gb));
exit;
"


Options[SingularGBMaker]={Modulus->0,SimplificationRules->Global`OptionSimplification,ScriptFile->TemporaryDirectory<>"GB.sing",
OutputFile->TemporaryDirectory<>"GB_result.txt",ScriptOnly->False,degBound->0,Silence->True,BlockPrioryVars->{}
};
SingularGBMaker[Minput_,varRedundant_,parameterRedundant_,OptionsPattern[]]:=Module[{M,SingularScript,forwardRep,backwardRep,var,parameters,varpara,len1,len2,
varString,parameterString,varBlockPriory,varBlockNotPriory,orderString},
	
	
	If[FileExistsQ[OptionValue[OutputFile]],DeleteFile[OptionValue[OutputFile]]];
	(*M=ModuleSlash[Minput];*)
	M=Minput;
	varpara=Variables[M];
	var=ListIntersect[varRedundant,varpara];  (* Delete the variables not in the modules *)
	parameters=ListIntersect[parameterRedundant,varpara];   (* Delete the parameters not in the modules *)
	(*If[parameters=={},parameters=parameterRedundant[[{1}]]];   (*  If there is no parameter, to fit in the Singular code, pick up one parameter *)*)
	If[parameters==={},parameters={AuxKinVar}];(*modified at 2025.3.18: to fix the case that parameterRedundant itself is {}*)
	If[OptionValue[BlockPrioryVars]=!={},
		varBlockPriory=Intersection[var,OptionValue[BlockPrioryVars]]//Sort;
		varBlockNotPriory=Complement[var,OptionValue[BlockPrioryVars]]//Sort;
		var=Join[varBlockPriory,varBlockNotPriory];
		orderString=SingularOrderingString[Length[varBlockPriory],Length[varBlockNotPriory],Length[parameters]];
	,
		orderString=SingularOrderingString[Length[var],Length[parameters]]
	];
	varString=StringReplace[ToString[var/.ForwardRep[1]],{"{"->"","}"->""}];
	parameterString=StringReplace[ToString[parameters/.ForwardRep[1]],{"{"->"","}"->""}];
	
	SingularScript=StringReplace[SingularGBText,{"VAR"->varString,"PARAMETERS"->parameterString}];
	SingularScript=StringReplace[SingularScript,{"MODULUS"->ToString[Modulus//OptionValue],"ORDERINGSTRING"->orderString}];
	SingularScript=StringReplace[SingularScript,{"MODULE"->Module2SingularForm[M,1]}];
	SingularScript=StringReplace[SingularScript,"OUTPUTFILE"->OptionValue[OutputFile]];
	SingularScript=StringReplace[SingularScript,{"SimplificationStrategy"->ToString[OptionValue[SimplificationRules]],"DEGBOUND"->ToString[OptionValue[degBound]]}];
	If[OptionValue[Silence]===True,
		SingularScript=StringReplace[SingularScript,"option(prot);\n"->""];
	];
	If[OptionValue[Silence]=!=True,
		PrintAndLog[OptionValue[ScriptFile]]
	];
	
	Export[OptionValue[ScriptFile],SingularScript,"Text"];
	If[OptionValue[ScriptOnly], Return[]];
];


Options[GBRead]=Options[SingularGBMaker];
GBRead[dim_,OptionsPattern[]]:=Module[{string,vectors},
	
	If[!FileExistsQ[OptionValue[OutputFile]],
		(*Return[]*)
		PrintAndLog["#",secNum,"  Singular result file ",OptionValue[OutputFile]," not found in GBRead."];
		(*PrintAndLog["******** sector ",secNum," failed."];
		Quit[];*)
		Return[$Failed];
	];
	string=StringReplace["{"<>Import[OptionValue[OutputFile]]<>"}","gen("~~Shortest[x__]~~")":>"gen["~~x~~"]"];
	vectors=ToExpression[string];
	
	(*yan-er-dao-ling!*)
	If[Count[vectors,0]>0,PrintAndLog["#",secNum,"  zero(s) in results encountored. They are deleted."];vectors=DeleteCases[vectors,0]];
	
	vectors=vectors/.gen[index_]:>UnitVector[dim,index]/.BackwardRep[1];
	Return[vectors];
];


Options[SingularGB]={Modulus->0,SimplificationRules->Global`OptionSimplification,ScriptFile->TemporaryDirectory<>"GB.sing",
OutputFile->TemporaryDirectory<>"GB_result.txt",TrashFile->TemporaryDirectory<>"GB_trash.txt",(*TestOnly->False,*)ScriptOnly->False,degBound->0,
SingularTimeUsedLimit->Infinity,
VariableOrder->var,(*Cut->{},ShortenTheModules->False,*)
NumericMode->False,PrintDegBound->False,DeleteResultFiles->DeleteSingularResultFiles,DeleteScriptFiles->DeleteSingularScriptFiles,Silence->True};
SingularGB[vectorList_,vars_,cutIndex_,OptionsPattern[]]:=Module[{M,cut,varsCutted,SingularCommand,timer,gb},
	cut=Table[vars[[i]]->0,{i,cutIndex}];
	M=vectorList/.cut;
	If[OptionValue[NumericMode]===True,
		M=M/.GenericPoint/.GenericD;
	];
	
	(*varOrder=Join[Complement[var,Variables[M]]//Sort,Intersection[var,Variables[M]]//Sort];*)
	(*varOrder=Intersection[vars,Variables[M]]//Sort;*)
	(* PrintAndLog[varOrder]; *)
	varsCutted=DeleteCases[vars/.cut,0];
	SingularGBMaker[M,varsCutted,
		Parameters,ScriptFile->OptionValue[ScriptFile],
		OutputFile->OptionValue[OutputFile],
		Modulus->OptionValue[Modulus],
		SimplificationRules->OptionValue[SimplificationRules],
		degBound->OptionValue[degBound],
		Silence->OptionValue[Silence]
	];
	If[OptionValue[Silence]===True,
		SingularCommand=SingularApp<>" --no-out "<>OptionValue[ScriptFile]<>" >> "<>OptionValue[TrashFile];
	,
		SingularCommand=SingularApp<>" "<>OptionValue[ScriptFile];
	];
	If[OptionValue[SingularTimeUsedLimit]=!=Infinity,
		If[OptionValue[SingularTimeUsedLimit]<0,
			PrintAndLog["SingularGB: SingularTimeUsedLimit must be positive!"];
			Exit[1];
		];
		SingularCommand="timeout "<>ToString[OptionValue[SingularTimeUsedLimit]]<>" "<>SingularCommand;
	];
	timer=AbsoluteTime[];
	Run[SingularCommand];
	If[OptionValue[Silence]=!=True,
		PrintAndLog["Singular running time ... ",AbsoluteTime[]-timer];
	];
	gb=GBRead[Length[M[[1]]],OutputFile->OptionValue[OutputFile]];
	
	If[OptionValue[DeleteScriptFiles]&&FileExistsQ[OptionValue[ScriptFile]],
		Run["rm "<>OptionValue[ScriptFile]];
		If[(!OptionValue[Silence]),Print[OptionValue[ScriptFile]<>" deleted."]];
	];
	If[OptionValue[DeleteResultFiles]&&FileExistsQ[OptionValue[OutputFile]],
		Run["rm "<>OptionValue[OutputFile]];
		If[(!OptionValue[Silence]),Print[OptionValue[OutputFile]<>" deleted."]];
	];
	If[OptionValue[PrintDegBound]&&(!OptionValue[Silence]),PrintAndLog["Degree bound:",OptionValue[degBound]]];
	If[FileExistsQ[OptionValue[TrashFile]],Run["rm "<>OptionValue[TrashFile]]];
	Return[gb];
];


(* ::Subsection:: *)
(*Singular intersection*)


(*IdString[x___]:=ToString[Round[AbsoluteTime[]]]<>ToString[Hash[ToString[InputForm[{x}]],"MD5"]]<>ToString[RandomInteger[ByteCount[{x}]]]*)


ModuleSlash[m_]:=Table[If[Union[m[[j]]]==={0},Nothing,m[[j]]],{j,1,Length[m]}];


Vector2gen[vec_,mode_]:=vec . Table[gen[j],{j,1,Length[vec]}]/.ForwardRep[mode];
Vector2SingularForm[vec_,mode_]:=StringReplace[ToString[InputForm[Vector2gen[vec,mode]]],{"gen["~~Shortest[x__]~~"]":>"gen("<>x<>")","{"->"[","}"->"]"}];
Module2SingularForm[m_,mode_]:=StringReplace[ToString[Vector2SingularForm[#,mode]&/@m],{"{"->"","}"->""}];


SingularIntersectionText="LIB \"matrix.lib\";
ring R = (MODULUS), (VAR, PARAMETERS), (ORDERINGSTRING);
option(prot);
degBound=DEGBOUND;
module m1 = MODULE1;
module m2 = MODULE2;
module m=m1,m2;
module m1ext = M1ext;
module a=syz(m);
matrix matrixa=a;
matrix matrixa1=submat(matrixa,1..M1SIZE,1..size(a));
matrix matrixm1ext=m1ext;
module m12old=module(matrixm1ext*matrixa1);
ring R2 = (MODULUS,PARAMETERS), (VAR), (ORDERING_STRING_2);
module m12=imap(R,m12old);
module mInt=simplify(m12,SimplificationStrategy);
write(\"OUTPUTFILE\",string(mInt));
exit;
";
SingularIntersectionTextForSyzOnlyMode="LIB \"matrix.lib\";
ring R = (MODULUS), (VAR, PARAMETERS), (ORDERINGSTRING);
option(prot);
degBound=DEGBOUND;
module m1 = MODULE1;
module m2 = MODULE2;
module m=m1,m2;
module a=syz(m);
module asim=simplify(a,SimplificationStrategy);
write(\"OUTPUTFILE\",string(asim));
exit;
";
(*SingularIntersectionText=(*debug2023*)"LIB \"matrix.lib\";
ring R = (MODULUS,PARAMETERS), (VAR), dp;
option(prot);
degBound=DEGBOUND;
module m1 = MODULE1;
module m2 = MODULE2;
module m=m1,m2;
module m1ext = M1ext;
module a=syz(m);
matrix matrixa=a;
matrix matrixa1=submat(matrixa,1..M1SIZE,1..size(a));
matrix matrixm1ext=m1ext;
module m12old=module(matrixm1ext*matrixa1);
ring R2 = (MODULUS,PARAMETERS), (VAR), (ORDERING_STRING_2);
module m12=imap(R,m12old);
module mInt=simplify(m12,SimplificationStrategy);
write(\"OUTPUTFILE\",string(mInt));
exit;
";*)


SingularSyzText="LIB \"matrix.lib\";
ring R = (MODULUS), (VAR, PARAMETERS), (ORDERINGSTRING);
option(prot);
degBound=DEGBOUND;
module m1 = MODULE1;
module a=syz(m1);
ring R2 = (MODULUS,PARAMETERS), (VAR), (ORDERING_STRING_2);
module a2=imap(R,a);
module a3=simplify(a2,SimplificationStrategy);
write(\"OUTPUTFILE\",string(a3));
exit;
";


(*this code is added by Hefeng. But sorryly... not actually used in the main codes. *)
SingularIntersectionTextForFlexibleDegBound="LIB \"matrix.lib\";
LIB \"tasks.lib\";
timer = 0; //initialization of timer
system(\"--ticks-per-sec\",1000); 
ring R = (MODULUS), (VAR, PARAMETERS), (ORDERINGSTRING);
option(prot);
degBound=DEGBOUND;
module m1 = MODULE1;
module m2 = MODULE2;
module m=m1,m2;
module m1ext = M1ext;
module a;

int tStart;
int flag_Overtime = 0;
int time_limit = 3600000; //defined by user, default 1h


tStart = timer; // Record start time.
task ta1; // initialization of task type;
ta1 = \"syz\", list(I);
startTasks(ta1);
waitAllTasks(ta1);

while(timer - tStart < time_limit){
if(getState(ta1)==\"started\"){
flag_Overtime = 0;
}
else{
//getResult(ta1);
print(\"break\");
break;
}
}
if(getState(ta1)==\"started\"){
flag_Overtime = 1;
killTask(ta1);
print(\"Syzygy computation timeout\");
write(\"ERRORLOG\",\"Syzygy computation timeout, may need to downsize degBound\");
exit;
}
else{
a = getResult(ta1);
print(\"Syzygy successfully computed\");
}

matrix matrixa=a;
matrix matrixa1=submat(matrixa,1..M1SIZE,1..size(a));
matrix matrixm1ext=m1ext;
module m12old=module(matrixm1ext*matrixa1);
ring R2 = (MODULUS,PARAMETERS), (VAR), (ORDERING_STRING_2);
module m12=imap(R,m12old);
module mInt=simplify(m12,SimplificationStrategy);
write(\"OUTPUTFILE\",string(mInt));
exit;
";


Options[SingularIntersectionMaker]={Modulus->0,SimplificationRules->Global`OptionSimplification,ScriptFile->TemporaryDirectory<>"intersection.sing",
OutputFile->TemporaryDirectory<>"intersection_result.txt",ScriptOnly->False,degBound->0,BlockPrioryVars->{},SyzOnlyMode->False
};
SingularIntersectionMaker[M1input_,M1extinput_,M2input_,varRedundant_,parameterRedundant_,OptionsPattern[]]:=Module[{M1,M1ext,M2,SingularScript,forwardRep,backwardRep,var,parameters,varpara,len1,len2,
varString,parameterString,varBlockPriory,varBlockNotPriory,orderString,orderString2},
	
	
	If[FileExistsQ[OptionValue[OutputFile]],DeleteFile[OptionValue[OutputFile]]];
	If[OptionValue[SyzOnlyMode]=!=True,
	(*I think this may be not necessary. If we do, it may bring incorrect correspondence in SyzOnlyMode*)
		M1=ModuleSlash[M1input];
		M1ext=ModuleSlash[M1extinput];
		M2=ModuleSlash[M2input];
	,
		M1=Identity[M1input];
		M1ext=Identity[M1extinput];
		M2=Identity[M2input];
	];
	
	varpara=Variables[Join[M1ext,M2]];
	var=ListIntersect[varRedundant,varpara];  (* Delete the variables not in the modules *)
	parameters=ListIntersect[parameterRedundant,varpara];   (* Delete the parameters not in the modules *)
	(*If[parameters=={},parameters=parameterRedundant[[{1}]]];   (*  If there is no parameter, to fit in the Singular code, pick up one parameter *)*)
	If[parameters==={},parameters={AuxKinVar}];(*modified at 2025.3.18: to fix the case that parameterRedundant itself is {}*)
	If[OptionValue[BlockPrioryVars]=!={},
		varBlockPriory=Intersection[var,OptionValue[BlockPrioryVars]]//Sort;
		varBlockNotPriory=Complement[var,OptionValue[BlockPrioryVars]]//Sort;
		var=Join[varBlockPriory,varBlockNotPriory];
		orderString=SingularOrderingString[Length[varBlockPriory],Length[varBlockNotPriory],Length[parameters]];
		orderString2=SingularOrderingString[Length[varBlockPriory],Length[varBlockNotPriory]];
	,
		orderString=SingularOrderingString[Length[var],Length[parameters]];
		orderString2=SingularOrderingString[Length[var]]
	];
	varString=StringReplace[ToString[var/.ForwardRep[1]],{"{"->"","}"->""}];
	parameterString=StringReplace[ToString[parameters/.ForwardRep[1]],{"{"->"","}"->""}];
	If[OptionValue[SyzOnlyMode],
		SingularScript=SingularIntersectionTextForSyzOnlyMode;
	,
		SingularScript=SingularIntersectionText;
	];
	SingularScript=StringReplace[SingularScript,{"VAR"->varString,"PARAMETERS"->parameterString}];
	SingularScript=StringReplace[SingularScript,{"MODULUS"->ToString[Modulus//OptionValue],"ORDERINGSTRING"->orderString,"ORDERING_STRING_2"->orderString2}];
	SingularScript=StringReplace[SingularScript,{"MODULE1"->Module2SingularForm[M1,1],"M1ext"->Module2SingularForm[M1ext,1],"MODULE2"->Module2SingularForm[M2,1],"M1SIZE"->ToString[Length[M1]]}];
	SingularScript=StringReplace[SingularScript,"OUTPUTFILE"->OptionValue[OutputFile]];
	SingularScript=StringReplace[SingularScript,{"SimplificationStrategy"->ToString[OptionValue[SimplificationRules]],"DEGBOUND"->ToString[OptionValue[degBound]]}];
	PrintAndLog[OptionValue[ScriptFile]];
	Export[OptionValue[ScriptFile],SingularScript,"Text"];
	If[OptionValue[ScriptOnly], Return[]];
];


Options[IntersectionRead]=Join[Options[SingularIntersectionMaker],{Dim->0}];
IntersectionRead[OptionsPattern[]]:=Module[{dim,string,vectors,object},
	If[!OptionValue[SyzOnlyMode],
		dim=Global`SDim+1;
		object="VectorList";
	,
		dim=OptionValue[Dim];
		If[dim===0,
			PrintAndLog["#",secNum," Unexpected dimension 0 in IntersectionRead."];
			Return[$Failed]
		];
		object="syzygy result";
	];
	
	(*If[!FileExistsQ[OptionValue[OutputFile]],Return[]];*)
	If[!FileExistsQ[OptionValue[OutputFile]],
		(*Return[]*)
		PrintAndLog["#",secNum,"  Singular result file ",OptionValue[OutputFile]," not found in IntersectionRead."];
		(*
		PrintAndLog["******** sector ",secNum," failed."];
		Quit[];
		*)
		Return[$Failed];
		
	];
	string=StringReplace["{"<>Import[OptionValue[OutputFile]]<>"}","gen("~~Shortest[x__]~~")":>"gen["~~x~~"]"];
	vectors=ToExpression[string];
	
	(*yan-er-dao-ling!  (emm...? WHY?2025.7.1)*)
	If[Count[vectors,0]>0,PrintAndLog["#",secNum,"  zero(s) in "<>object<>" Encountored. They are deleted."];vectors=DeleteCases[vectors,0]];
	
	vectors=vectors/.gen[index_]:>UnitVector[dim,index]/.BackwardRep[1];
	Return[vectors];
];


ShortenedModule[M1ext_,restrictedIndices_]:=Module[{shortenedM1,shortenedM2},
	shortenedM1=M1ext[[All,restrictedIndices]];
	If[restrictedIndices=={},Return[shortenedM1]];
	shortenedM2=DiagonalMatrix[z/@restrictedIndices];
	Return[{shortenedM1,shortenedM2}]
]


Options[SingularIntersection]={Modulus->0,SimplificationRules->Global`OptionSimplification,ScriptFile->TemporaryDirectory<>"intersection.sing",
OutputFile->TemporaryDirectory<>"intersection_result.txt",TestOnly->False,ScriptOnly->False,degBound->0,VariableOrder->var,Cut->{},ShortenTheModules->False,
SingularTimeUsedLimit->Infinity,SyzOnlyMode->False,
NumericMode->False,PrintDegBound->False,DeleteResultFiles->DeleteSingularResultFiles,DeleteScriptFiles->DeleteSingularScriptFiles};
SingularIntersection[resIndex_,OptionsPattern[]]:=Module[
{M1,M1ext,M2,SingularCommand,timer,vectors,cutIndex,blockPrioryVars,outputFile},
	cutIndex=OptionValue[Cut];
	If[!SubsetQ[resIndex,cutIndex],PrintAndLog["Sorry... This version does not support the case with a cut propagator index UNrestricted ..."]; Return[$Failed];];
	{M1,M1ext,M2}=TangentModules[resIndex,cutIndex];
	If[OptionValue[NumericMode]===True,
		M1=M1/.GenericPoint/.GenericD;
		M1ext=M1ext/.GenericPoint/.GenericD;
		M2=M2/.GenericPoint/.GenericD;
	];
	If[OptionValue[ShortenTheModules]===True,
		If[OptionValue[SyzOnlyMode],
			PrintAndLog["SingularIntersection: Sorry, ShortenTheModules is not yet supported in SyzOnlyMode."];
			(*because I am lazy*)
			Return[$Failed]
		];
		{M1,M2}=ShortenedModule[M1ext,resIndex];
		If[M2==={},Return[M1ext]](*No restrictions on any of the indices*)
	];
	If[OptionValue[TestOnly],PrintAndLog[resIndex];Return[{M1,M2}]];
	Switch[SingularBaikovVariableOrdering,
	"DenominatorFirst",
		
		varOrder=Join[Intersection[var,Variables[M2]]//Sort,Complement[var,Variables[M2]]//Sort];
		If[SingularBaikovVariableBlockOrdering===True,
			blockPrioryVars=Intersection[var,Variables[M2]]//Sort
		,
			blockPrioryVars={}
		];
	,
	_,
		If[SingularBaikovVariableOrdering=!="NumeratorFirst",PrintAndLog["SingularIntersection: unkown SingularBaikovVariableOrdering ",SingularBaikovVariableOrdering,", switching to NumeratorFirst"]];
		varOrder=Join[Complement[var,Variables[M2]]//Sort,Intersection[var,Variables[M2]]//Sort];
		If[SingularBaikovVariableBlockOrdering===True,
			blockPrioryVars=Complement[var,Variables[M2]]//Sort
		,
			blockPrioryVars={}
		];
		
	];
	
	(* PrintAndLog[varOrder]; *)
	If[OptionValue[SyzOnlyMode],
		outputFile=TemporaryDirectory<>"intersection_result_syz_only.txt";
		(*the codes are not unified... never mind, since there is no freedom for OptionValue[OutputFile]
		user cannot set this value. 
		*)
	,
		outputFile=OptionValue[OutputFile];
	];
	SingularIntersectionMaker[M1,M1ext,M2,varOrder,Parameters,
		ScriptFile->OptionValue[ScriptFile],
		OutputFile->outputFile,
		Modulus->OptionValue[Modulus],
		SimplificationRules->OptionValue[SimplificationRules],
		degBound->OptionValue[degBound],
		BlockPrioryVars->blockPrioryVars,
		SyzOnlyMode->OptionValue[SyzOnlyMode]
	];
	SingularCommand=SingularApp<>" "<>OptionValue[ScriptFile];
	If[OptionValue[SingularTimeUsedLimit]=!=Infinity,
		If[OptionValue[SingularTimeUsedLimit]<0,
			PrintAndLog["SingularIntersection: SingularTimeUsedLimit must be positive!"];
			Exit[1];
		];
		SingularCommand="timeout "<>ToString[OptionValue[SingularTimeUsedLimit]]<>" "<>SingularCommand;
	];
	timer=AbsoluteTime[];
	Run[SingularCommand];
	PrintAndLog["Singular running time ... ",AbsoluteTime[]-timer];
	
	vectors=IntersectionRead[
		OutputFile->outputFile,
		SyzOnlyMode->OptionValue[SyzOnlyMode],
		Dim->Length[M1]+Length[M2](*this only works in SyzOnlyMode!*)
	];
	
	
	If[OptionValue[DeleteScriptFiles]&&FileExistsQ[OptionValue[ScriptFile]],
		Run["rm "<>OptionValue[ScriptFile]];
		Print[OptionValue[ScriptFile]<>" deleted."];
	];
	If[OptionValue[DeleteResultFiles]&&FileExistsQ[OptionValue[OutputFile]],
		Run["rm "<>outputFile];
		Print[outputFile<>" deleted."];
	];
	If[OptionValue[PrintDegBound],PrintAndLog["Degree bound:",OptionValue[degBound]]];
	Return[vectors];
];


(* ::Subsection:: *)
(*Singular lift to GB*)


(*SingularLiftToGBText="LIB \"matrix.lib\";
ring R = (MODULUS), (VAR, PARAMETERS), (ORDERINGSTRING);
option(prot);
degBound=DEGBOUND;
module m = MODULE;
module gb=std(m);
module l=lift(m,gb);
module s=syz(m);
module reducedL=NF(l,s);
module mResult=simplify(reducedL,SimplificationStrategy);
write(\"OUTPUTFILE\",string(mResult));
exit;
";*)
SingularLiftToGBText="LIB \"matrix.lib\";
ring R = (MODULUS), (VAR, PARAMETERS), (ORDERINGSTRING);
option(prot);
degBound=DEGBOUND;
module m = MODULE;
module gb=std(m);
module l=lift(m,gb);
module mResult=simplify(l,SimplificationStrategy);
write(\"OUTPUTFILE\",string(mResult));
exit;
";



Options[SingularLiftToGBMaker]={Modulus->0,SimplificationRules->Global`OptionSimplification,ScriptFile->TemporaryDirectory<>"lift_to_GB.sing",
OutputFile->TemporaryDirectory<>"lift_to_GB_result.txt",ScriptOnly->False,degBound->0,BlockPrioryVars->{}
};
SingularLiftToGBMaker[Minput_,varRedundant_,parameterRedundant_,OptionsPattern[]]:=Module[{M,SingularScript,forwardRep,backwardRep,var,parameters,varpara,len1,len2,
varString,parameterString,varBlockPriory,varBlockNotPriory,orderString},
	
	
	If[FileExistsQ[OptionValue[OutputFile]],DeleteFile[OptionValue[OutputFile]]];
	(*M=ModuleSlash[Minput];*)
	M=Minput;
	varpara=Variables[M];
	var=ListIntersect[varRedundant,varpara];  (* Delete the variables not in the modules *)
	parameters=ListIntersect[parameterRedundant,varpara];   (* Delete the parameters not in the modules *)
	(*If[parameters=={},parameters=parameterRedundant[[{1}]]];   (*  If there is no parameter, to fit in the Singular code, pick up one parameter *)*)
	If[parameters==={},parameters={AuxKinVar}];(*modified at 2025.3.18: to fix the case that parameterRedundant itself is {}*)
	If[OptionValue[BlockPrioryVars]=!={},
		varBlockPriory=Intersection[var,OptionValue[BlockPrioryVars]]//Sort;
		varBlockNotPriory=Complement[var,OptionValue[BlockPrioryVars]]//Sort;
		var=Join[varBlockPriory,varBlockNotPriory];
		orderString=SingularOrderingString[Length[varBlockPriory],Length[varBlockNotPriory],Length[parameters]];
	,
		orderString=SingularOrderingString[Length[var],Length[parameters]]
	];
	varString=StringReplace[ToString[var/.ForwardRep[1]],{"{"->"","}"->""}];
	parameterString=StringReplace[ToString[parameters/.ForwardRep[1]],{"{"->"","}"->""}];
	
	SingularScript=StringReplace[SingularLiftToGBText,{"VAR"->varString,"PARAMETERS"->parameterString}];
	SingularScript=StringReplace[SingularScript,{"MODULUS"->ToString[Modulus//OptionValue],"ORDERINGSTRING"->orderString}];
	SingularScript=StringReplace[SingularScript,{"MODULE"->Module2SingularForm[M,1]}];
	SingularScript=StringReplace[SingularScript,"OUTPUTFILE"->OptionValue[OutputFile]];
	SingularScript=StringReplace[SingularScript,{"SimplificationStrategy"->ToString[OptionValue[SimplificationRules]],"DEGBOUND"->ToString[OptionValue[degBound]]}];
	PrintAndLog[OptionValue[ScriptFile]];
	Export[OptionValue[ScriptFile],SingularScript,"Text"];
	If[OptionValue[ScriptOnly], Return[]];
];


Options[LiftToGBRead]=Options[SingularLiftToGBMaker];
LiftToGBRead[dim_,OptionsPattern[]]:=Module[{string,vectors},
	
	If[!FileExistsQ[OptionValue[OutputFile]],
		(*Return[]*)
		PrintAndLog["#",secNum,"  Singular result file ",OptionValue[OutputFile]," not found in LiftToGBRead."];
		(*
		PrintAndLog["******** sector ",secNum," failed."];
		Quit[];
		*)
		Return[$Failed];
	];
	string=StringReplace["{"<>Import[OptionValue[OutputFile]]<>"}","gen("~~Shortest[x__]~~")":>"gen["~~x~~"]"];
	vectors=ToExpression[string];
	
	(*yan-er-dao-ling!*)
	If[Count[vectors,0]>0,PrintAndLog["#",secNum,"  zero(s) in results encountored. They are deleted."];vectors=DeleteCases[vectors,0]];
	
	vectors=vectors/.gen[index_]:>UnitVector[dim,index]/.BackwardRep[1];
	Return[vectors];
];


Options[SingularLiftToGB]={Modulus->0,SimplificationRules->Global`OptionSimplification,ScriptFile->TemporaryDirectory<>"lift_to_GB.sing",
OutputFile->TemporaryDirectory<>"lift_to_GB_result.txt",(*TestOnly->False,*)ScriptOnly->False,degBound->0,VariableOrder->var,(*Cut->{},ShortenTheModules->False,*)
NumericMode->False,PrintDegBound->False,DeleteResultFiles->DeleteSingularResultFiles,DeleteScriptFiles->DeleteSingularScriptFiles,SingularTimeUsedLimit->Infinity};
SingularLiftToGB[vectorList_,vars_,cutIndex_,OptionsPattern[]]:=Module[{M,cut,varsCutted,SingularCommand,timer,liftMatrix},
	cut=Table[vars[[i]]->0,{i,cutIndex}];
	M=vectorList/.cut;
	If[OptionValue[NumericMode]===True,
		M=M/.GenericPoint/.GenericD;
	];
	
	(*varOrder=Join[Complement[var,Variables[M]]//Sort,Intersection[var,Variables[M]]//Sort];*)
	(*varOrder=Intersection[vars,Variables[M]]//Sort;*)
	(* PrintAndLog[varOrder]; *)
	varsCutted=DeleteCases[vars/.cut,0];
	SingularLiftToGBMaker[M,varsCutted,Parameters,ScriptFile->OptionValue[ScriptFile],OutputFile->OptionValue[OutputFile],Modulus->OptionValue[Modulus],SimplificationRules->OptionValue[SimplificationRules],degBound->OptionValue[degBound]];
	SingularCommand=SingularApp<>" "<>OptionValue[ScriptFile];
	If[OptionValue[SingularTimeUsedLimit]=!=Infinity,
		If[OptionValue[SingularTimeUsedLimit]<0,
			PrintAndLog["SingularLiftToGB: SingularTimeUsedLimit must be positive!"];
			Exit[1];
		];
		SingularCommand="timeout "<>ToString[OptionValue[SingularTimeUsedLimit]]<>" "<>SingularCommand;
	];
	timer=AbsoluteTime[];
	Run[SingularCommand];
	PrintAndLog["Singular running time ... ",AbsoluteTime[]-timer];
	
	liftMatrix=LiftToGBRead[Length[M],OutputFile->OptionValue[OutputFile]];
	
	If[OptionValue[DeleteScriptFiles]&&FileExistsQ[OptionValue[ScriptFile]],
		Run["rm "<>OptionValue[ScriptFile]];
		Print[OptionValue[ScriptFile]<>" deleted."];
	];
	If[OptionValue[DeleteResultFiles]&&FileExistsQ[OptionValue[OutputFile]],
		Run["rm "<>OptionValue[OutputFile]];
		Print[OptionValue[OutputFile]<>" deleted."];
	];
	If[OptionValue[PrintDegBound],PrintAndLog["Degree bound:",OptionValue[degBound]]];
	Return[liftMatrix];
];


(* ::Subsection::Closed:: *)
(*SimplifyByCut*)


Options[SimplifyByCut]={sectorNumber->0,ReportLayer->2,DCornerOnly->False,SimplifyMethod->SimplifyByCutMethod,FurtherSelection->FurtherSyzygyVectorsSelection,Modulus->0,SkipLift->SkipLiftSelection};
SimplifyByCut[vectorsInput_,cutIndices_,OptionsPattern[]]:=Module[
{vectors,vectorsCutted,sortedVectorIndices,vectorsSorted,vectorsCuttedSorted,vectorsInputSorted,i,
tmp,syzygyVectorAbsDegrees,syzygyVectorISPDegrees,syzygyVectorPropDegrees,FurtherSelectionETC,FurtherSelectionTimer,
memoryUsed,timer,selectedIndices,selectedIndices2,selectedIndices2New,vectorsCuttedSortedSelectedGB2,
vectorsCuttedSortedSelectedGB,vectorsCuttedSortedSelected,recoveredIndices,
secNo,reportLayer,ind1,ind2,result,cutInd,entry,liftMatrix,timer2,memoryUsed2,timer3,memoryUsed3,timeForAGBComputation},
	secNo=OptionValue[sectorNumber];
	reportLayer=OptionValue[ReportLayer];
	
	timer=AbsoluteTime[];
	PrintAndLog["#",secNo,ReportIndent[reportLayer],"Reforming vector lists..."];
	memoryUsed=MaxMemoryUsed[
	vectors=vectorsInput;
	If[OptionValue[DCornerOnly],
		PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"DCornerOnly mode, skip."]
	,
		For[ind1=1,ind1<=Length[vectors],ind1++,
			For[ind2=1,ind2<=Length[cutIndices],ind2++,
				cutInd=cutIndices[[ind2]];
				entry=Cancel[vectors[[ind1,cutInd]]/z[cutInd]];
				If[Expand[(Denominator[entry]/.z[cutInd]->0)]===0,
					PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Warning in SimplifyByCut, cannot cancel z[",cutInd," in vectors[[",ind1,cutInd "]]. Giving up simplification."];
					Return[vectorsInput];
				];
				vectors[[ind1,cutInd]]=entry;
			]
		];
	];
	
	(*end of MaxMemoryUsed*)];
	PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Finished. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ];
	
	
	timer=AbsoluteTime[];
	PrintAndLog["#",secNo,ReportIndent[reportLayer],"Cutting vector lists..."];
	memoryUsed=MaxMemoryUsed[
	vectorsCutted=vectors/.Table[z[i]->0,{i,cutIndices}];
	(*end of MaxMemoryUsed*)];
	PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Finished. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ];
	
	timer=AbsoluteTime[];
	PrintAndLog["#",secNo,ReportIndent[reportLayer],"Deriving syzygy vector degrees..."];
	memoryUsed=MaxMemoryUsed[
	syzygyVectorPropDegrees=Table[
		Exponent[vectors[[i]]/.GenericD/.GenericPoint/.Table[z[j]->tmp,{j,cutIndices}],tmp],
		{i,Length[vectors]}
	];
	syzygyVectorISPDegrees=Table[
		Exponent[vectors[[i]]/.GenericD/.GenericPoint/.Table[z[j]->tmp,{j,Complement[Range[SDim],cutIndices]}],tmp],
		{i,Length[vectors]}
	];
	syzygyVectorAbsDegrees=Table[
		Exponent[vectors[[i]]/.GenericD/.GenericPoint/.Table[z[j]->tmp,{j,Range[SDim]}],tmp],
		{i,Length[vectors]}
	];
	(*end of MaxMemoryUsed*)];
	PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Finished. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ];
	
	timer=AbsoluteTime[];
	PrintAndLog["#",secNo,ReportIndent[reportLayer],"Sorting vector lists..."];
	memoryUsed=MaxMemoryUsed[
	sortedVectorIndices=SortBy[Range[Length[vectors]],{
		syzygyVectorISPDegrees[[#]],
		syzygyVectorAbsDegrees[[#]],
		syzygyVectorPropDegrees[[#]],
		LeafCount[vectors[[#]]],
		ByteCount[vectors[[#]]]
		
	}&];
	vectorsSorted=vectors[[sortedVectorIndices]];
	vectorsCuttedSorted=vectorsCutted[[sortedVectorIndices]];
	vectorsInputSorted=vectorsInput[[sortedVectorIndices]];
	(*end of MaxMemoryUsed*)];
	PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Finished. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ];
	
	
	
	
	
	timer=AbsoluteTime[];
	PrintAndLog["#",secNo,ReportIndent[reportLayer],"Making simplified vector list..."];
	memoryUsed=MaxMemoryUsed[
	Switch[OptionValue[SimplifyMethod]
	,"LiftResubstitution",
		timer=AbsoluteTime[];
		PrintAndLog["#",secNo,ReportIndent[reportLayer],"Computing lift matrix..."];
		memoryUsed=MaxMemoryUsed[
		liftMatrix=SingularLiftToGB[vectorsCuttedSorted,var,cutIndices,Modulus->OptionValue[Modulus],SingularTimeUsedLimit->LiftResubstitutionSingularTimeConstraint];
		(*end of MaxMemoryUsed*)];
		PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Finished. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ];
		If[liftMatrix===$Failed,
			PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Warning in SimplifyByCut, Singular returns $Failed (A possible reason: time used exceeds the user-set limit). Giving up simplification."];
			Return[vectorsInput];
		];
		
		
		timer2=AbsoluteTime[];
		PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Lift resubstituting..."];
		memoryUsed2=MaxMemoryUsed[
		result=(#.vectorsInputSorted)&/@liftMatrix;
		(*end of MaxMemoryUsed*)];
		PrintAndLog["#",secNo,ReportIndent[reportLayer+2],"Finished. Time Used: ", Round[AbsoluteTime[]-timer2], " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB." ];
		
	,_,
		If[SimplifyByCutMethod=!="LiftSelection",PrintAndLog["#",secNo,"\t","SimplifyByCut: Unknown SimplifyByCutMethod, switching to LiftSelection"]];
		If[(!OptionValue[SkipLift])===True,
			timer=AbsoluteTime[];
			PrintAndLog["#",secNo,ReportIndent[reportLayer],"Computing lift matrix..."];
			memoryUsed=MaxMemoryUsed[
			liftMatrix=SingularLiftToGB[vectorsCuttedSorted,var,cutIndices,Modulus->OptionValue[Modulus],SingularTimeUsedLimit->LiftSelectionSingularTimeConstraint];
			(*end of MaxMemoryUsed*)];
			PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Finished. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ];
			If[liftMatrix===$Failed,
				PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Warning in SimplifyByCut, Singular returns $Failed (A possible reason: time used exceeds the user-set limit). Keeping result=vectorsInputSorted in this step."];
				selectedIndices=Range[Length[vectorsInputSorted]];
				result=vectorsInputSorted[[selectedIndices]];
			,
				timer2=AbsoluteTime[];
				PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Lift selecting... [stricty=",Max[Min[LiftSelectionStrictness,1],0],"]"];
				memoryUsed2=MaxMemoryUsed[
				selectedIndices=Select[Range[Length[vectorsInputSorted]],Union[liftMatrix[[All,#]]]=!={0}&];
				PrintAndLog["#",secNo,ReportIndent[reportLayer+2],"Selected vectors: ",Length[selectedIndices],"."];
				If[LiftSelectionStrictness<1,
					recoveredIndices={};
					For[i=1,i<=Length[vectorsInputSorted],i++,
						If[!MemberQ[selectedIndices,i],
							If[RandomReal[]>LiftSelectionStrictness,
								recoveredIndices=Join[recoveredIndices,{i}]
							]
						]
					];
					PrintAndLog["#",secNo,ReportIndent[reportLayer+2],"Recovered unselected vectors: ",Length[recoveredIndices],"."];
					selectedIndices=Join[selectedIndices,recoveredIndices]//Union//Sort;
				];
				PrintAndLog["#",secNo,ReportIndent[reportLayer+2],"Vectors: ",Length[selectedIndices],"."];
				result=vectorsInputSorted[[selectedIndices]];
				(*end of MaxMemoryUsed*)];
				PrintAndLog["#",secNo,ReportIndent[reportLayer+2],"Finished. Time Used: ", Round[AbsoluteTime[]-timer2], " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB." ];
			];	
		,
			PrintAndLog["#",secNo,ReportIndent[reportLayer+3],"SkipLiftSelection is True, skipping the selection using lift matrix. "];
			selectedIndices=Range[Length[vectorsInputSorted]];
			result=vectorsInputSorted[[selectedIndices]];
		];
		
		
		
		If[OptionValue[FurtherSelection]===True,
			timer2=AbsoluteTime[];
			PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Further selecting syzygy vectors..."];
			memoryUsed2=MaxMemoryUsed[
			vectorsCuttedSortedSelected=vectorsCuttedSorted[[selectedIndices]];
			timer3=AbsoluteTime[];
			PrintAndLog["#",secNo,ReportIndent[reportLayer+2],"Computing cut GB..."];
			memoryUsed3=MaxMemoryUsed[
			vectorsCuttedSortedSelectedGB=SingularGB[
				vectorsCuttedSortedSelected,var,{},
				Modulus->OptionValue[Modulus],
				SingularTimeUsedLimit->FurtherSelectionSingularTimeConstraint
			];
			(*end of MaxMemoryUsed*)];
			timeForAGBComputation=AbsoluteTime[]-timer3;
			PrintAndLog["#",secNo,ReportIndent[reportLayer+3],"Finished. Time Used: ",Round[timeForAGBComputation], " second(s). Memory used: ",Round[memoryUsed3/(1024^2)]," MB." ];
			If[vectorsCuttedSortedSelectedGB===$Failed,
				PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Warning in SimplifyByCut, Singular returns $Failed. Skipping further selection."];
				Return[result];
			];
			timer3=AbsoluteTime[];
			FurtherSelectionETC=Max[Min[FurtherSyzygyVectorsSelectionStrictness,1],0]*timeForAGBComputation*Length[vectorsCuttedSortedSelected];
			PrintAndLog["#",secNo,ReportIndent[reportLayer+2],"Testing each vector [stricty=",Max[Min[FurtherSyzygyVectorsSelectionStrictness,1],0],
				"]. Estimated time cost: ",
				Round[FurtherSelectionETC],
				" second(s)."];
			memoryUsed3=MaxMemoryUsed[
			selectedIndices2=Range[Length[vectorsCuttedSortedSelected]];
			FurtherSelectionTimer=AbsoluteTime[];
			For[i=Length[selectedIndices2],i>=1,i--,
				If[AbsoluteTime[]-FurtherSelectionTimer>FurtherSelectionTimeUsedLimit,
					PrintAndLog["#",secNo,ReportIndent[reportLayer+3],"The further selection time used exceeds the limit ",FurtherSelectionTimeUsedLimit," second(s)."];
					PrintAndLog["#",secNo,ReportIndent[reportLayer+3],"Skiping the rest ",i," further selections. ( total: ",Length[vectorsCuttedSortedSelected],")."];
					Break[];
				];
				selectedIndices2New=DeleteCases[selectedIndices2,i];
				If[RandomReal[]<FurtherSyzygyVectorsSelectionStrictness,
					vectorsCuttedSortedSelectedGB2=SingularGB[
						vectorsCuttedSortedSelected[[selectedIndices2New]],var,{},
						Modulus->OptionValue[Modulus],
						SingularTimeUsedLimit->FurtherSelectionSingularTimeConstraint
					];
					If[And[
							vectorsCuttedSortedSelectedGB===vectorsCuttedSortedSelectedGB2,
							vectorsCuttedSortedSelectedGB2=!=$Failed
						]
					,
						selectedIndices2=selectedIndices2New;
					,
						If[vectorsCuttedSortedSelectedGB2===$Failed,
							PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Warning in SimplifyByCut, Singular returns $Failed. Keeping the current vector (No.",i,") as selected."];
						];
						If[ReportFurtherSelectedSyzygyVectors===True,PrintAndLog["#",secNo,ReportIndent[reportLayer+3],"vector No.",i," selected."]]
					]
				,
					If[ReportFurtherSelectedSyzygyVectors===True,PrintAndLog["#",secNo,ReportIndent[reportLayer+3],"vector No.",i," selected (without testing)."]]
				];
			];
			(*end of MaxMemoryUsed*)];
			PrintAndLog["#",secNo,ReportIndent[reportLayer+3],"Finished. Time Used: ",Round[AbsoluteTime[]-timer3], " second(s). Memory used: ",Round[memoryUsed3/(1024^2)]," MB." ];
			
			result=result[[selectedIndices2]];
			(*end of MaxMemoryUsed*)];
			PrintAndLog["#",secNo,ReportIndent[reportLayer+2],"Finished. Time Used: ", Round[AbsoluteTime[]-timer2], " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB." ];
		
		]
	];
	(*end of MaxMemoryUsed*)];
	PrintAndLog["#",secNo,ReportIndent[reportLayer+1],"Finished. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ];
	PrintAndLog["#",secNo,ReportIndent[reportLayer],"Length of the simplified vector list: ",Length[result]];
	PrintAndLog["#",secNo,ReportIndent[reportLayer],"LeafCount of the simplified vector list: ",LeafCount[result]];
	PrintAndLog["#",secNo,ReportIndent[reportLayer],"ByteCount of the simplified vector list: ",ByteCount[result]];
	result
	
]



(* ::Section:: *)
(*IBP generator*)


Std[f_,ref_]:=Total[(G@@(ref-#[[1]]))*(#[[2]])&/@CoefficientRules[f,var,DegreeReverseLexicographic]];
(*If[seedingViaFIBPFunction,
	ClearAll[Std];
	PrintAndLog["seedingViaFIBPFunction is on, redefining Std."];
	Std[f_,ref_]:=Module[{cr=CoefficientRules[f,var,DegreeReverseLexicographic]},
		Sum[(G@@(ref-cr[[i,1]]))*cr[[i,2]],{i,Length[cr]}]
	]
]*)


Options[IBPGenerator]:={Cut->{}};
IBPGenerator[vector_,RestrictedPropIndex_,OptionsPattern[]]:=Module[{i,b,ref,reflocal,term1,term2=0,h,f,bb,cutIndex},
	cutIndex=OptionValue[Cut];
	If[!SubsetQ[RestrictedPropIndex,cutIndex],PrintAndLog["Sorry... This version does not support the case with a cut propagator index UNrestricted ..."]; Return[];];
	If[cutIndex=!={},Return[IBPCutGenerator[vector,RestrictedPropIndex,cutIndex]];];
	b=vector[[-1]];
	h=L+(n-1)+1;
	ref=Table[m[i],{i,1,SDim}];
	term1=Std[-((d-h)/2)b,ref];
	
	For[i=1,i<=SDim,i++,
		If[MemberQ[RestrictedPropIndex,i],
			bb=Cancel[vector[[i]]/z[i]];
			f=(-m[i]+1)bb+z[i]*D[bb,z[i]];
			term2+=Std[f,ref];
			,
			reflocal=ref/.m[i]->m[i]+1;
			f=-m[i] vector[[i]]+z[i]*D[vector[[i]],z[i]];
			term2+=Std[f,reflocal];
		];
		
	];
	Return[term1+term2];
	
];

(*If[seedingViaFIBPFunction,
ClearAll[IBPGenerator];
PrintAndLog["seedingViaFIBPFunction is on, redefining IBPGenerator."];
Options[IBPGenerator]:={Cut->{}};
IBPGenerator[vector_,RestrictedPropIndex_,OptionsPattern[]]:=Module[{i,b,ref,reflocal,term1,term2=0,h,f,bb,cutIndex},
	cutIndex=OptionValue[Cut];
	If[!SubsetQ[RestrictedPropIndex,cutIndex],PrintAndLog["Sorry... This version does not support the case with a cut propagator index UNrestricted ..."]; Return[];];
	If[cutIndex=!={},Return[IBPCutGenerator[vector,RestrictedPropIndex,cutIndex]];];
	b=vector[[-1]];
	h=L+(n-1)+1;
	ref=Table[Slot[i],{i,1,SDim}];
	term1=Std[-((d-h)/2)b,ref];
	
	For[i=1,i<=SDim,i++,
		If[MemberQ[RestrictedPropIndex,i],
			bb=Cancel[vector[[i]]/z[i]];
			f=(-Slot[i]+1)bb+z[i]*D[bb,z[i]];
			term2+=Std[f,ref];
			,
			reflocal=ref/.Slot[i]->Slot[i]+1;
			f=-Slot[i] vector[[i]]+z[i]*D[vector[[i]],z[i]];
			term2+=Std[f,reflocal];
		];
		
	];
	Return[Evaluate[term1+term2]&]
	
	
];

]*)


IBPCutGenerator[vector_,RestrictedPropIndex_,cutIndex_]:=Module[{i,b,ref,reflocal,term1,term2=0,h,f,bb,cutNormalization},
	b=vector[[-1]];
	h=L+(n-1)+1;
	cutNormalization=m[#]->1&/@cutIndex;
	(* PrintAndLog[cutNormalization]; *)
	ref=Table[m[i],{i,1,SDim}]/.cutNormalization;
	term1=Std[-((d-h)/2)b,ref];
	For[i=1,i<=SDim,i++,
		If[MemberQ[cutIndex,i],Continue[];];
		If[MemberQ[RestrictedPropIndex,i],
			bb=Cancel[vector[[i]]/z[i]];
			f=(-m[i]+1)bb+z[i]*D[bb,z[i]];
			term2+=Std[f,ref];
			,
			reflocal=ref/.m[i]->m[i]+1;
			f=-m[i] vector[[i]]+z[i]*D[vector[[i]],z[i]];
			term2+=Std[f,reflocal];
		];
	];
	Return[term1+term2];
];

(*If[seedingViaFIBPFunction,
ClearAll[IBPCutGenerator];
PrintAndLog["seedingViaFIBPFunction is on, redefining IBPCutGenerator."];
IBPCutGenerator[vector_,RestrictedPropIndex_,cutIndex_]:=Module[{i,b,ref,reflocal,term1,term2=0,h,f,bb,cutNormalization},
	b=vector[[-1]];
	h=L+(n-1)+1;
	cutNormalization=Slot[#]->1&/@cutIndex;
	(* PrintAndLog[cutNormalization]; *)
	ref=Table[Slot[i],{i,1,SDim}]/.cutNormalization;
	term1=Std[-((d-h)/2)b,ref];
	For[i=1,i<=SDim,i++,
		If[MemberQ[cutIndex,i],Continue[];];
		If[MemberQ[RestrictedPropIndex,i],
			bb=Cancel[vector[[i]]/z[i]];
			f=(-Slot[i]+1)bb+z[i]*D[bb,z[i]];
			term2+=Std[f,ref];
			,
			reflocal=ref/.Slot[i]->Slot[i]+1;
			f=-Slot[i] vector[[i]]+z[i]*D[vector[[i]],z[i]];
			term2+=Std[f,reflocal];
		];
	];
	Return[Evaluate[term1+term2]&];
	
];

]*)


(* ::Section::Closed:: *)
(*IBP sector Tools*)


Options[IntegralList]={SortTheIntegrals->True}
IntegralList[IBP_,OptionsPattern[]]:=If[OptionValue[SortTheIntegrals]===True,
	SortBy[Select[Variables[IBP],Head[#]==G&],IntegralOrdering]//Reverse
,
	Select[Variables[IBP],Head[#]==G&]
]

LeadingIntegral[IBP_]:=Block[{list},
	list=IntegralList[IBP];
	If[list=!={},Return[list[[1]]],Return[{}]];
];





(* Functions for the complexity of IBPs *)
(*IBPSectorHeight[IBP_]:=Block[{LT},LT=LeadingIntegral[IBP]; If[LT==={},Return[-1],Return[SectorHeight[Int]]];];*)
IBPSubSectorDegree[IBP_,sector_]:=Max[Total[IntegralAbsDegree[#]]&/@(Select[IntegralList[IBP,SortTheIntegrals->False],Sector[#]=!=sector&])];
IBPSubSectorDenominatorDegree[IBP_,sector_]:=Max[IntegralPropagatorDegree[#]&/@(Select[IntegralList[IBP,SortTheIntegrals->False],Sector[#]=!=sector&])];
IBPSubSectorNumeratorDegree[IBP_,sector_]:=Max[IntegralISPDegree[#]&/@(Select[IntegralList[IBP,SortTheIntegrals->False],Sector[#]=!=sector&])];
IBPISPSectorDegree[IBP_,sector_]:=Max[(IntegralISPDegree/@Select[IntegralList[IBP,SortTheIntegrals->False],Sector[#]==sector&])]; (* "IntegralISPDegree" vs "IntegralAbsDegree" *)
FIBPSectorISPDegree[FIBP_,sector_]:=Max[FIntegralSectorISPDegree[#,sector]&/@(Cases[Variables[FIBP],_G])]


IntegerPartition[n_,len_]:=Module[{cc,clist,conditions},
	clist=Table[cc[i],{i,1,len}];
	conditions=Table[cc[i]>=0,{i,1,len}];
	
	AppendTo[conditions,Sum[cc[i],{i,1,len}]==n];
	ss=Solve[conditions,clist,Integers];
	Return[clist/.ss];
];
positionArrange[positions_,values_,len_]:=SparseArray[MapThread[#1->#2&,{positions,values}],len]//Normal;
NumeratorShifts[sector_,n_]:=-positionArrange[Complement[Range[SDim],sector//SectorIndex],#,SDim]&/@IntegerPartition[n,SDim-SectorHeight[sector]];
SeedMerge[numshifts_,denshifts_]:=Flatten[Table[numshifts[[i]]+denshifts[[j]],{i,1,Length[numshifts]},{j,1,Length[denshifts]}],1];




IntegralRealization[FundamentalIBP_,degree_]:=FundamentalIBP/.Dispatch[Table[m[i]->degree[[i]],{i,1,SDim}]];
(*If[seedingViaFIBPFunction,
	ClearAll[IntegralRealization];
	PrintAndLog["seedingViaFIBPFunction is on, redefining IntegralRealization."];
	IntegralRealization[FundamentalIBP_,degree_]:=FundamentalIBP@@degree;
]*)


pivots[matrix_]:=Module[{ARLonglist},
	ARLonglist=GatherBy[matrix//ArrayRules,#[[1,1]]&];
	Return[#[[1,1,2]]&/@(ARLonglist[[;;-2]])];
];


(* ::Section::Closed:: *)
(*Symmetry *)


(* ::Text:: *)
(*These codes depend on Pak.wl. But we need to include it outside this .wl file, some place where this .wl file is included.*)


(* ::Subsection:: *)
(*SectorMaps*)


ZSymmetry[momentumMap_]:=Module[{zs},
	zs=Cases[Variables[PolynomialG],_z];
	Table[
		z[i]->(Expand[Propagators[[i]]/.momentumMap]/.Kinematics/.ScalarVarRep/.BaikovRep//Factor)
	,
		{i,Length[zs]}
	]
]
SymmetryRules[zPerm_]:=Module[{prop1,prop2,momentumMaps},
	prop1=zPerm[[All,1]]/.z[i_]:>Propagators[[i]];
	prop2=zPerm[[All,2]]/.z[i_]:>Propagators[[i]];
	(*momentumMaps=TimeConstrained[
		MomentumMap[LoopMomenta,ExternalMomenta,prop1,prop2,Kinematics,FullEMsConstrain->True],
		MomentumMapTimeConstraint,
		Export[outputPath<>"tmp/MomentumMapFailureInput.txt",{LoopMomenta,ExternalMomenta,prop1,prop2,Kinematics}//InputForm//ToString];
		PrintAndLog["** Failure: MomentumMap computation timed out.\n zPerm:",zPerm];
		If[ParallelInFindingSectorMaps,CloseKernels[]];
		Quit[];
	];*)
	momentumMaps=MomentumMap[LoopMomenta,ExternalMomenta,prop1,prop2,Kinematics,FullEMsConstrain->True];
	ZSymmetry/@momentumMaps
]
FindSymmetry[sec1_,sec2_]:=Module[{zs,G1,G2,zs1,zs2,zPerms,result},
	(*If[Union[Flatten[sec1]]=!=Union[{0,1}],Print["sector ",sec1," is an unexpected input."];Return[$Failed]];
	If[Union[Flatten[sec2]]=!=Union[{0,1}],Print["sector ",sec2," is an unexpected input."];Return[$Failed]];*)
	
	
	If[!SubsetQ[{0,1},Union[Flatten[sec1]]],Print["sector ",sec1," is an unexpected input."];Return[$Failed]];
	If[!SubsetQ[{0,1},Union[Flatten[sec2]]],Print["sector ",sec2," is an unexpected input."];Return[$Failed]];
	If[Total[sec1]=!=Total[sec2],Return[{}]];(*no need to find symmetry between sectors of different heights*)
	zs=Sort[Cases[Variables[PolynomialG],_z]];
	G1=PolynomialG/.Table[z[i]->z[i]*sec1[[i]],{i,Length[zs]}];
	G2=PolynomialG/.Table[z[i]->z[i]*sec2[[i]],{i,Length[zs]}];
	zs1=Cases[Variables[G1],_z];
	zs2=Cases[Variables[G2],_z];
	zPerms=FullPakCompare[G1,G2,zs1,zs2];
	result=SymmetryRules/@zPerms;
	(*probe=zPerms;*)
	result=DeleteDuplicates[Flatten[result,1]];
	(*Print[#->#&/@zs];*)
	(*DeleteCases[result,#->#&/@zs]*)
	result
]
FindSymmetry[sec_]:=FindSymmetry[sec,sec]


Options[SectorMaps]={MaxKernelNumber->Infinity}
SectorMaps[sectors_,OptionsPattern[]]:=Module[
{
	undeterminedSectors,uniqueSectors,mappedSectors,sectorMaps,newUniqueSector,mappedUndeterminedSectorIndices,
	i,maps,selectedMap,newTestingSector,listOfMaps
},
	undeterminedSectors=Reverse[sectors];(*without reversing, the function tends to choose sectors in the fronter of the list as unique sectors, this is the reversion of our convention*)
	uniqueSectors={};
	mappedSectors={};
	sectorMaps={};
	If[ParallelInFindingSectorMaps,
		Print["\tLaunching kernels..."];
		LaunchKernels[Min[
			OptionValue[MaxKernelNumber],
			MathKernelLimit-1,
			$ProcessorCount,
			ThreadUsedLimit
		]];
		Print["\tDone."]
	];
	While[True,
		If[undeterminedSectors==={},Break[]];
		Print["\t\t",Length[undeterminedSectors]," undetermined sector(s) left."];
		newUniqueSector=undeterminedSectors[[1]];
		uniqueSectors=Join[uniqueSectors,{newUniqueSector}];
		undeterminedSectors=undeterminedSectors[[2;;-1]];
		mappedUndeterminedSectorIndices={};
		If[Not[ParallelInFindingSectorMaps],
			For[i=1,i<=Length[undeterminedSectors],i++,
				newTestingSector=undeterminedSectors[[i]];
				(*PrintAndLog["finding symmetries between ",newTestingSector,newUniqueSector];*)
				maps=FindSymmetry[newTestingSector,newUniqueSector];
				(*PrintAndLog["found ",Length[maps],"maps."];*)
				If[Length[maps]>0,
					selectedMap=SortBy[maps,{LeafCount[#],ByteCount[#]}&][[1]];
					mappedSectors=Join[mappedSectors,{newTestingSector}];
					mappedUndeterminedSectorIndices=Join[mappedUndeterminedSectorIndices,{{i}}];
					sectorMaps=Join[sectorMaps,{newTestingSector->{newUniqueSector,selectedMap}}]
				]
			];
		,
			
			listOfMaps=ParallelTable[
				FindSymmetry[undeterminedSectors[[i]],newUniqueSector],
				{i,Length[undeterminedSectors]},
				Method->"FinestGrained"
			];
			For[i=1,i<=Length[undeterminedSectors],i++,
				newTestingSector=undeterminedSectors[[i]];
				(*maps=FindSymmetry[newTestingSector,newUniqueSector];*)
				maps=listOfMaps[[i]];
				If[Length[maps]>0,
					selectedMap=SortBy[maps,{LeafCount[#],ByteCount[#]}&][[1]];
					mappedSectors=Join[mappedSectors,{newTestingSector}];
					mappedUndeterminedSectorIndices=Join[mappedUndeterminedSectorIndices,{{i}}];
					sectorMaps=Join[sectorMaps,{newTestingSector->{newUniqueSector,selectedMap}}]
				]
			];
		];
		undeterminedSectors=Delete[undeterminedSectors,mappedUndeterminedSectorIndices]
		
	];
	If[ParallelInFindingSectorMaps,
		Print["\tClosing kernels..."];
		CloseKernels[];
		Print["\tDone."];
	];
	{uniqueSectors,mappedSectors,sectorMaps}
]


(* ::Subsection:: *)
(*Integral Maps*)


MappedIntegral[zMap_,indices_]:=Module[{zs,sector,ispq,test,numeratorInds,denominatorInds,numerator,denominator,crn,crd,di,df,Gn},
	sector=Boole[#>0]&/@indices;
	ispq=1-sector;
	zs=Sort[zMap[[All,1]]];
	test=Product[zs[[i]]^-sector[[i]],{i,Length[zs]}]/.zMap//Expand;
	If[Length[MonomialList[test]]>1,PrintAndLog["MappedIntegral: sector ",sector," inconsistent with map ",zMap];Return[$Failed]];
	numeratorInds=indices . DiagonalMatrix[ispq];
	denominatorInds=indices . DiagonalMatrix[sector];
	numerator=Product[zs[[i]]^-numeratorInds[[i]],{i,Length[zs]}]/.zMap//Expand;
	denominator=Product[zs[[i]]^denominatorInds[[i]],{i,Length[zs]}]/.zMap//Expand;
	crn=CoefficientRules[numerator,zs];
	crd=CoefficientRules[denominator,zs];
	If[Length[crd]>1,PrintAndLog["MappedIntegral: sector ",sector," inconsistent with map ",zMap];Return[$Failed]];
	di=crd[[1,1]];
	df=crd[[1,2]];
	Gn=Total[(G@@(#[[1]]))*(#[[2]])&/@crn];
	Gn/.G[x__]:>((G@@(di-{x}))/df)
	
]



GZeroSectorCut[x__]:=If[MemberQ[Global`ZeroSectors,Sector[G[x]]],0,G[x]]


GMapped[sectorMaps_,indices_]:=Module[{sector,mappedSectors,map,preResult,result},
	sector=Boole[#>0]&/@indices;
	mappedSectors=sectorMaps[[All,1]];
	If[MemberQ[mappedSectors,sector],
		map=(sector/.sectorMaps)[[2]];
		preResult=MappedIntegral[map,indices];
		result=preResult/.G->GZeroSectorCut;
		Return[result]
	,
		Return[G@@indices]
	]
]


(* ::Subsection::Closed:: *)
(*Map a expression*)


(*main map function*)
Options[SymmetryMap]={ReplacementDirection->"Foreward"}(*"Backward" ReplacementDirection is not fully tested*)
SymmetryMap[sectorMaps_,expr_,OptionsPattern[]]:=Module[
{replacementChain,mappedSectors,expressionContainingPossibleSourceIntegrals,sourceIntegrals,newReplacements,result,i,replacements
,debugLabel
},
	
	replacementChain={};
	expressionContainingPossibleSourceIntegrals=expr;
	mappedSectors=sectorMaps[[All,1]];
	(*
	Explain why we need such a While True:
	A mapped sector A can be mapped to unique sector B, but an integral in A, with some numerator, can be mapped as combination of integrals not only in B, but also in subsectors of B. 
	However, a subsector of B is not necessarilly a unique sector, it could be mapped to another unique sector C, so we need to map again.
	*)
	
	While[True,
		
		sourceIntegrals=Select[
			IntegralList[
				expressionContainingPossibleSourceIntegrals,
				SortTheIntegrals->False
			],
			MemberQ[mappedSectors,Sector[#]]&
		];
		If[sourceIntegrals==={},Break[]];
		newReplacements=Table[
			sourceIntegrals[[i]]->(sourceIntegrals[[i]]/.G[x__]:>GMapped[sectorMaps,{x}]),
			{i,Length[sourceIntegrals]}
		];
		If[OptionValue[ReplacementDirection]==="Backward",
			While[True,
				If[
					Select[
						IntegralList[
							newReplacements[[All,2]],
							SortTheIntegrals->False
						],
						MemberQ[newReplacements[[All,1]],#]&
					]==={},
					Break[]
				];
				newReplacements=#[[1]]->(#[[2]]/.newReplacements)&/@newReplacements
			];
		];
		replacementChain=Join[replacementChain,{newReplacements}];
		expressionContainingPossibleSourceIntegrals=newReplacements[[All,2]];
	];
	result=expr;
	
	Switch[OptionValue[ReplacementDirection],
	"Backward",
		If[Length[replacementChain]>0,
			For[i=1,i<=Length[replacementChain],i++,
				If[i===1,
					replacements=replacementChain[[-i]];
				,
					replacements=replacementChain[[-i]]/.replacements;
				];
				
			];
			result=result/.replacements;
			
		];
	,
	"Foreward",
		For[i=1,i<=Length[replacementChain],i++,
			replacements=replacementChain[[i]];
			result=result/.replacements
		]
	,
	_,
		PrintAndLog["Unknown ReplacementDirection in SymmetryMap"];
		Return[$Failed]
	];
	
	result
]


(* ::Subsubsection:: *)
(*old version code*)


(*(*main map function*)
Options[SymmetryMap]={CheckByNumerics->True}
SymmetryMap[sectorMaps_,expr_,OptionsPattern[]]:=Module[{exprExpanded,newExprExpanded,check},
	exprExpanded=Expand[expr];
	(*
	Explain why we need such a While True:
	A mapped sector A can be mapped to unique sector B, but an integral in A, with some numerator, can be mapped as combination of integrals not only in B, but also in subsectors of B. 
	However, a subsector of B is not necessarilly a unique sector, it could be mapped to another unique sector C, so we need to map again.
	*)
	While[True,
		newExprExpanded=CollectG[exprExpanded/.G[x__]:>GMapped[sectorMaps,{x}]];
		check=newExprExpanded-exprExpanded;
		If[OptionValue[CheckByNumerics],check=check/.GenericPoint/.GenericD];
		If[Expand[check]===0,Return[newExprExpanded]];
		exprExpanded=newExprExpanded
	]
]*)


(* ::Subsection:: *)
(*MappedAndSubSectorsAllFinder*)


(*
Explain what this function is:
say, f(s) is the sub sector of s
g(s) is the map of s
f and g can operate on s in any order, such as ffgfgfgfgf(s)...
This function gives all unique sectors that can be got, starting from s, using f and g , in any ways.

*)

MappedAndSubSectorsAllFinder[sectorMaps_,sectors_]:=Module[{mappingOfSectors,oldSectors,newSectors,mappedSectors},
	mappingOfSectors=(#[[1]]->#[[2,1]])&/@sectorMaps;
	mappedSectors=sectorMaps[[All,1]];
	oldSectors=Join[#,#/.mappingOfSectors]&[SubsectorAllFinder[sectors]];
	oldSectors=SortBy[oldSectors,SectorOrdering]//Reverse;
	While[True,
		newSectors=Join[#,#/.mappingOfSectors]&[SubsectorAllFinder[oldSectors]];
		newSectors=SortBy[newSectors,SectorOrdering]//Reverse;
		If[newSectors===oldSectors,Break[]];
		oldSectors=newSectors;
	];
	Complement[newSectors,mappedSectors]
]


(* ::Subsection:: *)
(*Sector self symmetries*)


SelfSymmetryRealization[zMap_,indices_]:=Expand[MappedIntegral[zMap,indices]-(G@@indices)](*well this function does not map the produced subsec integrals*)


(* ::Subsection::Closed:: *)
(*LPSymmetryQ*)


LPIntegrand[indices_]:=Module[
{i,zeroIndices,positiveIndices,negativeIndices,Gpol,index,k,GD
},
	zeroIndices=Select[Range[SDim],indices[[#]]===0&];
	positiveIndices=Select[Range[SDim],indices[[#]]>0&];
	negativeIndices=Select[Range[SDim],indices[[#]]<0&];
	Gpol=Global`PolynomialG/.Table[z[k]->0,{k,zeroIndices}];
	GD=Gpol^(-d/2);
	For[k=1,k<=Length[negativeIndices],k++,
		i=negativeIndices[[k]];
		index=indices[[i]];
		GD=D[GD,{z[i],-index}](-1)^(-index)/.z[i]->0
	];
	For[k=1,k<=Length[positiveIndices],k++,
		i=positiveIndices[[k]];
		index=indices[[i]];
		GD=GD*z[i]^(index-1)/Gamma[index]
	];
	GD*(-1)^Total[indices]*Gamma[d/2]/Gamma[(L+1)d/2-Total[indices]]
]


LPSymmetryQ[integral1_,integral2_]:=Module[
{sec1,sec2,indices1,indices2,zs,G1,G2,zs1,zs2,zPerms,i,zPerm,GIntegrand1,GIntegrand2},
	If[integral1===integral2,Return[True]];
	indices1=integral1/.G->List;
	indices2=integral2/.G->List;
	If[Total[indices1]=!=Total[indices2],Return[False]];(*LP Integrand containds a Gamma[(L+1)d/2-Total[indices]], if the sum of indices dismatch, they must not have LP symmetry*)
	sec1=Sector[integral1];
	sec2=Sector[integral2];
	If[Total[sec1]=!=Total[sec2],Return[False]];(*integrals with different sector height are considered to be without symmetry between each other*)
	zs=Sort[Cases[Variables[PolynomialG],_z]];
	G1=PolynomialG/. Table[z[i]->z[i] sec1[[i]],{i,Length[zs]}];
	G2=PolynomialG/. Table[z[i]->z[i] sec2[[i]],{i,Length[zs]}];
	zs1=Cases[Variables[G1],_z];zs2=Cases[Variables[G2],_z];
	zPerms=FullPakCompare[G1,G2,zs1,zs2];
	If[zPerms==={},Return[False]];(*no symmetry*)

	For[i=1,i<=Length[zPerms],i++,
		zPerm=zPerms[[i]];
		GIntegrand1=integral1/.G[x__]:>LPIntegrand[{x}];
		GIntegrand2=integral2/.G[x__]:>LPIntegrand[{x}];
		
		If[Factor[(GIntegrand1/.zPerm)-GIntegrand2]===0,Return[True]]
	];
	Return[False]

]


(* ::Section:: *)
(*Azuritino*)


(* ::Subsection:: *)
(*Critical point*)


QuotientRingSize[ideal1_,varList1_,OptionsPattern[]]:=Module[{ideal=ideal1,var=varList1,LTList,i,n,condition,ss,cclist,cc},
    LTList=Exponent[LT[#,var,DegreeReverseLexicographic],var]&/@ideal;
	n=Length[var];
	cclist=cc/@Range[n];

	condition=Table[MapThread[#1<#2&,{cclist,LTList[[i]]}]/.List->Or,{i,1,Length[LTList]}];
	
	
	AppendTo[condition,Table[cc[i]>=0,{i,1,n}]/.List->And];
	
	ss=Solve[condition,cclist,Integers];
	If[Length[ss]>0&&Union[NumberQ/@Flatten[cclist/.ss]]=!={True},
		Return[99999];
	];
	
	Return[ss//Length];
];

Options[LeeCriticalPoints]={Modulus->FiniteFieldModulus};
LeeCriticalPoints[sector_,OptionsPattern[]]:=Module[{P,Indices,vlist,ideal,GB,w},
	Indices=SectorIndex[sector];
	(* P=Together[BaikovKernel/.(z[#]->0&/@Indices)/.GenericPoint];
	vlist=z/@Complement[Range[SDim],Indices]//Sort; *)
	
	P=Together[(PolynomialF+PolynomialU)/.(z[#]->0&/@Complement[Range[SDim],Indices])/.GenericPoint];
	vlist=z/@Indices;
	ideal=Join[D[P,{vlist}],{w P-1}];
	
	GB=GroebnerBasis[ideal,Join[vlist,{w}],Modulus->OptionValue[Modulus],MonomialOrder->DegreeReverseLexicographic];
	Return[QuotientRingSize[GB,Join[vlist,{w}]]];
]



(* ::Subsection:: *)
(*AzuritinoMIFind*)


AzuritePoolToSeed[PoolMember_,ISPIndices_]:=Module[{i,result=Table[1,SDim]},
	For[i=1,i<=Length[PoolMember],i++,
		result[[ISPIndices[[i]]]]=PoolMember[[i]]
	];
	result
]





Options[AzuritinoMIFind]={degBound->0,AzuritinoGenericD->GenericD,MaxISPDegree->AzuritinoDefaultMaxDegree,MinISPDegreeForAnalysis->AzuritinoDefaultStartDegree,Modulus->FiniteFieldModulus,CriticalPointCounting->False,
selfSymmetryZMaps->{}
};
AzuritinoMIFind[sector_,OptionsPattern[]]:=Module[{P,secNo,Indices,ISPIndices,ISPlen,timer=AbsoluteTime[],tt=AbsoluteTime[],vectorList,FIBPs,FIBPISPdegree,IBPFunctions
,Pool,IBPs,i,j,NewIBPs,IntList,M,MI,irreducibleInts,LeeCounting,MaxISPD=OptionValue[MaxISPDegree],sectorCut,
MinISPD=OptionValue[MinISPDegreeForAnalysis],pivotList,zMaps,newSelfSymmetries,LeeExpectedMICandidates,oldMI,MIKeepSameStepsCount},
		secNo=SectorNumber[sector];
		Indices=SectorIndex[sector];
		sectorCut=SectorCut[sector];	
		ISPIndices=Complement[Range[SDim],SectorIndex[sector]];
		ISPlen=ISPIndices//Length;
		If[ISPlen==0,(*in case that there are no ISPs, P dependends on no zi`s*)
			P=Together[BaikovKernel/.z[a_]:>0];
			If[P===0,
				Return[{}],
				Return[{G@@sector}]
			];
		];
		LeeExpectedMICandidates="not defined";
		If[OptionValue[CriticalPointCounting],
			LeeCounting=LeeCriticalPoints[sector];
			PrintAndLog["#",secNo,"\t\t","Lee's MI counting ... ",LeeCounting];
			If[LeeCounting<1000,
				(* If the output number does not seem weird ...*)
				i=0;
				While[Binomial[ISPlen+i+1-1,ISPlen+1-1]<=LeeCounting,
					i++;
				];
				MinISPD=i+1;
				If[debugModification20231102===True,
					MaxISPD=MinISPD*2+3;
				,
					MaxISPD=MinISPD+2;
				];
				LeeExpectedMICandidates=Flatten[SeedMerge[NumeratorShifts[sector,#],{sector}]&/@Range[0,MinISPD],1];
				LeeExpectedMICandidates=(G@@#)&/@LeeExpectedMICandidates;
			]
		];
		(*If[MaxISPD<MinISPD+2,MaxISPD=MinISPD+2];*)
		PrintAndLog["#",secNo,"\t\t","start to find MIs from IBPs with numerator starting degree ",MinISPD," to max degree ",MaxISPD];
		
		
		
		vectorList=SingularIntersection[Indices,
			Cut->Indices,
			degBound->OptionValue[degBound],
			NumericMode->True,
			ScriptFile->TemporaryDirectory<>"azuritino_intersection.sing",
			OutputFile->TemporaryDirectory<>"azuritino_intersection_result.txt"
		];
		
		If[vectorList===$Failed,
			PrintAndLog["#",secNo,"\t\t","***** Singular running returns $Failed. "];
			PrintAndLog["***** sector",secNo," failed. Exiting."];
			Exit[1];
		];
		
		PrintAndLog["#",secNo,"\t\t","Azuritino: number of syzygy generators: ",Length[vectorList]];
		tt=AbsoluteTime[];
		FIBPs=(CollectG[IBPGenerator[#,Indices,Cut->Indices]]&/@vectorList)/.OptionValue[AzuritinoGenericD];  (* CollectG may be slow *)
		(* Global`TestFIBPs=FIBPs; *) (* BackDoor *)
		FIBPISPdegree=FIBPSectorISPDegree[#,sector]&/@(FIBPs);
		FIBPISPdegree=FIBPISPdegree/.(-\[Infinity]->0);  (* To be improved; YZ *)
		
		IBPFunctions=Table[Function@@{FIBPs[[i]]}/. Table[m[ISPIndices[[k]]]->Slot[k],{k,1,SDim-SectorHeight[sector]}],{i,1,Length[FIBPs]}];
	
		
		PrintAndLog["#",secNo,"\t\t","Azuritino: formal max-cut IBPs generated... ",AbsoluteTime[]-tt];
		(* Zurich-type Seeding *)
		tt=AbsoluteTime[];
		
		IBPs={};
		For[i=0,i<=MaxISPD,i++,
			Pool[i]=(-1)*IntegerPartition[i,ISPlen];
			
			NewIBPs=Reap[
				For[j=1,j<=Length[FIBPs],j++,
					If[FIBPISPdegree[[j]]<=i,Sow[Map[IBPFunctions[[j]][#/.List->Sequence]&,Pool[i-FIBPISPdegree[[j]]],1]];];
					(*FIBPFunction is a function with #, is it still safe to use # again here?*)
				];
			][[2]]//Flatten;
			PrintAndLog["#",secNo,"\t\t","Azuritino: Step "<>ToString[i]<>" : "<>ToString[Length[NewIBPs]]<>" test IBPs generated... ",AbsoluteTime[]-tt];
			tt=AbsoluteTime[];
			IBPs=Join[IBPs,NewIBPs];
			
			(* adding symmetry relation to IBPs here *)
			If[Not[NeedSymmetry===False],
				tt=AbsoluteTime[];
				zMaps=OptionValue[selfSymmetryZMaps];
				newSelfSymmetries=Table[
					SelfSymmetryRealization[
						(zMaps[[k]]/.GenericPoint),
						AzuritePoolToSeed[Pool[i][[l]],ISPIndices]
					]/.sectorCut,
					{k,Length[zMaps]},
					{l,Length[Pool[i]]}
				]//Flatten;
				IBPs=Join[IBPs,newSelfSymmetries];
				
				PrintAndLog["#",secNo,"\t\t","Azuritino: Step "<>ToString[i]<>" : "<>ToString[Length[newSelfSymmetries]]<>" symmetry relations generated... ",AbsoluteTime[]-tt]
			];
			
			IntList=IntegralList[IBPs];
			If[LeeExpectedMICandidates=!="not defined",
				If[!SubsetQ[IntList,LeeExpectedMICandidates]&&i==MinISPD,
					PrintAndLog["#",secNo,"\t\t","[Azuritino Notice]: Seeding in step "<>ToString[i]<>" dose not cover all MI candidates in estimated by critical point." ," Rearranging seeding range from degree ",MinISPD+1," to degree ",MaxISPD+1,"." ];
					MinISPD+=1;
					If[debugModification20231102===True,
						MaxISPD+=2;
					,
						MaxISPD+=1;
					];
				]				
			];
			If[i>=MinISPD,
				M=SRSparseRowReduce[CoefficientArrays[IBPs,IntList][[2]],Modulus->OptionValue[Modulus]];
				pivotList=pivots[M];
				
				irreducibleInts=IntList[[Complement[Range[Length[IntList]],pivotList]]];
				
				If[i==MinISPD,
					MI=irreducibleInts;
					MIKeepSameStepsCount=0;
					,
					oldMI=MI;
					MI=Intersection[MI,irreducibleInts];
					If[Sort[MI]===Sort[oldMI],MIKeepSameStepsCount+=1,MIKeepSameStepsCount=0];
					
				];
				PrintAndLog["#",secNo,"\t\t","Azuritino: Step "<>ToString[i]<>" :        test IBPs reduced over the finite field ... ",AbsoluteTime[]-tt];
				PrintAndLog["#",secNo,"\t\t","Azuritino: Step "<>ToString[i]<>" :        MI length:",Length[MI]];
				If[debugModification20231102===True,
						If[MIKeepSameStepsCount===2&&i<MaxISPD,
							PrintAndLog["#",secNo,"\t\t","Azuritino: MIs has been unchanging for continuous 2 steps. Skiping the rest steps."];
							Break[];
						];
				]
			];
			tt=AbsoluteTime[];
		];
		(*Global`TestIBPs=IBPs;
		Global`TestRedIBPs=M . IntList;*)
		PrintAndLog["#",secNo,"\t\t","Azuritino:  Sector "<>ToString[sector]<>":  master integrals found ... ",AbsoluteTime[]-tt];
		Return[MI];
	
]



(* ::Section:: *)
(*Main *)


(* ::Subsection::Closed:: *)
(*SimpleIBP*)


(*what for?*)
Options[SimpleIBP]:={Verbosity->0,TestOnly->False,SeedingMethod->"Zurich"};
SimpleIBP[OptionsPattern[]]:=Module[{RelavantSectors,i,Sectors,timeUsed},
	If[Head[Global`TargetIntegrals]=!=List,PrintAndLog["The variable 'TargetIntegrals' is not set."]; Return[];];
	
	Sectors=SortBy[Union[Sector/@TargetIntegrals],SectorOrdering]//Reverse;
	RelavantSectors=SubsectorAllFinder[Sectors];
	
	Global`ZeroSectors=Select[RelavantSectors,ZeroSectorQ];
	Global`NonZeroSectors=SortBy[Complement[RelavantSectors,Global`ZeroSectors],SectorOrdering]//Reverse;
	PrintAndLog[Length[Global`NonZeroSectors]," non-zero sector(s) are found."];
	Global`ZeroTargets=Select[Global`TargetIntegrals,MemberQ[Global`ZeroSectors,Sector[#]]&];
	Global`ReductionTargets=Complement[Global`TargetIntegrals,Global`ZeroTargets];
	Global`ReductionTasks=Association[Table[Global`NonZeroSectors[[i]]->Select[Global`ReductionTargets,Global`NonZeroSectors[[i]]==Sector[#]&],{i,1,Length[Global`NonZeroSectors]}]];

	Global`ZeroSectorRemoval=SectorElimination/@Global`ZeroSectors;
	Global`IBPList=Association[Table[Global`NonZeroSectors[[i]]->{},{i,1,Length[Global`NonZeroSectors]}]];
	Global`MIList=Association[Table[Global`NonZeroSectors[[i]]->{},{i,1,Length[Global`NonZeroSectors]}]];
	
	Global`SectorAnalyzeTiming={};
	For[i=1,i<=Length[Global`NonZeroSectors],i++,
		(*PrintAndLog[SectorAnalyze[Global`NonZeroSectors[[i]],Verbosity->OptionValue[Verbosity],SeedingMethod->OptionValue[SeedingMethod]]//AbsoluteTiming];*)
		timeUsed=AbsoluteTiming[SectorAnalyze[Global`NonZeroSectors[[i]],Verbosity->OptionValue[Verbosity],SeedingMethod->OptionValue[SeedingMethod]]][[1]];
		AppendTo[Global`SectorAnalyzeTiming,"#"<>ToString[SectorNumber[Global`NonZeroSectors[[i]]]]->Round[timeUsed]];
		PrintAndLog["#"<>ToString[SectorNumber[Global`NonZeroSectors[[i]]]],"  Total Time Used: ",Round[timeUsed]," second(s)."]
	];
	
];


(* ::Subsection::Closed:: *)
(*DenominatorTypeTools*)


DenominatorTypesFromMaxDenominatorDegrees[maxes_]:=Module[{helperHeadForDenominatorTypesFromMaxDenominatorDegrees,h,poly,i,cr},
	h=Table[helperHeadForDenominatorTypesFromMaxDenominatorDegrees[i],{i,Length[maxes]}];
	poly=1;
	For[i=1,i<=Length[maxes],i++,
		If[maxes[[i]]=!=0,
			poly=poly*(1+h[[i]])^(maxes[[i]]-1)*h[[i]]
		];
	];
	poly=Expand[poly];
	cr=CoefficientRules[poly,h];
	cr[[All,1]]
]


Options[DenominatorTypeCompleting]={Strategy->"CompleteForEach"}
DenominatorTypeCompleting[DenominatorTypes_,OptionsPattern[]]:=Module[{maxes,result,dt,i},
	Switch[OptionValue[Strategy],
	"CompleteForAll",
		maxes=Max/@Transpose[DenominatorTypes];
		result=maxes//DenominatorTypesFromMaxDenominatorDegrees,
	"CompleteForEach",
		result={};
		For[i=1,i<=Length[DenominatorTypes],i++,
			dt=DenominatorTypes[[i]];
			result=Join[result,DenominatorTypesFromMaxDenominatorDegrees[dt]];
		];
		result=result//Union,
	_,
		result="** DenominatorTypeCompleting Err: Unknown Strategy."
	];
	result
]


DenominatorTypeLift[DenominatorTypes_,liftIndexLists_,sector_]:=Module[{i,j,lift,result},
	result=DenominatorTypes;
	For[i=1,i<=Length[DenominatorTypes],i++,
		For[j=1,j<=Length[liftIndexLists],j++,
			lift=Table[sector[[k]]*If[MemberQ[liftIndexLists[[j]],k],1,0],{k,SDim}];
			AppendTo[result,lift+DenominatorTypes[[i]]]
		]
	];
	result//Union
]


DenominatorLiftingShifts[sector_,liftDegree_]:=Module[{cs,c,constrain1,constrain2,constrain3,constrains,sol},
	cs=c/@Range[Length[sector]];
	constrain1=c[#]==0&/@Select[Range[Length[sector]],sector[[#]]===0&];
	constrain2=c[#]>=0&/@Select[Range[Length[sector]],sector[[#]]===1&];
	constrain3={sector . cs==liftDegree};
	constrains=Join[constrain1,constrain2,constrain3];
	sol=Solve[constrains,cs,Integers];
	SortBy[cs/.sol,{Count[#,0],-#}&]
]


(* ::Subsection::Closed:: *)
(*ZurichSeeding*)


ZurichSeeding[sector_,nFIBPs_,IBPISPdegreeList_,CurrentDeg_,DenominatorTypes_,OptionsPattern[]]:=Module[{nn,i,j,seeds,RawIBPs={},nIBPs={}},
	nn=Length[nFIBPs];
	If[Not[SowAndReap],
		For[i=1,i<=nn,i++,
			If[CurrentDeg<IBPISPdegreeList[[i]],Continue[]];
			seeds=SeedMerge[NumeratorShifts[sector,CurrentDeg-IBPISPdegreeList[[i]]],DenominatorTypes];
			RawIBPs=Join[RawIBPs,IntegralR[FI[i],#]&/@seeds];  (* Abstract notation FI[i] for FIBPs[[i]] *) 
			nIBPs=Join[nIBPs,IntegralRealization[nFIBPs[[i]],#]&/@seeds];
		];
	,
		nIBPs=Reap[
			For[i=1,i<=nn,i++,
				If[CurrentDeg<IBPISPdegreeList[[i]],Continue[]];
				seeds=SeedMerge[NumeratorShifts[sector,CurrentDeg-IBPISPdegreeList[[i]]],DenominatorTypes];
				RawIBPs=Join[RawIBPs,IntegralR[FI[i],#]&/@seeds];  (* Abstract notation FI[i] for FIBPs[[i]] *) 
				(*nIBPs=Join[nIBPs,IntegralRealization[nFIBPs[[i]],#]&/@seeds];*)
				For[j=1,j<=Length[seeds],j++,
					Sow[IntegralRealization[nFIBPs[[i]],seeds[[j]]]]
				]
			];
		][[2]];
		If[Length[nIBPs]>0,nIBPs=nIBPs[[1]]];
	];
	Return[{RawIBPs,nIBPs}];
];
ZurichSeedingVianFIBPFunctions[sector_,nFIBPFunctions_,IBPISPdegreeList_,CurrentDeg_,DenominatorTypes_,OptionsPattern[]]:=Module[
{nn,i,j,seeds,RawIBPs={},nIBPs={}},
	nn=Length[nFIBPFunctions];
	If[Not[SowAndReap],
		For[i=1,i<=nn,i++,
			If[CurrentDeg<IBPISPdegreeList[[i]],Continue[]];
			seeds=SeedMerge[NumeratorShifts[sector,CurrentDeg-IBPISPdegreeList[[i]]],DenominatorTypes];
			RawIBPs=Join[RawIBPs,IntegralR[FI[i],#]&/@seeds];  (* Abstract notation FI[i] for FIBPs[[i]] *) 
			nIBPs=Join[nIBPs,(nFIBPFunctions[[i]]@@#)&/@seeds];
		];
	,
		nIBPs=Reap[
			
			For[i=1,i<=nn,i++,
				If[CurrentDeg<IBPISPdegreeList[[i]],Continue[]];
				seeds=SeedMerge[NumeratorShifts[sector,CurrentDeg-IBPISPdegreeList[[i]]],DenominatorTypes];
				RawIBPs=Join[RawIBPs,IntegralR[FI[i],#]&/@seeds];  (* Abstract notation FI[i] for FIBPs[[i]] *) 
				(*nIBPs=Join[nIBPs,(nFIBPFunctions[[i]]@@#)&/@seeds];*)
				For[j=1,j<=Length[seeds],j++,
					Sow[(nFIBPFunctions[[i]])@@(seeds[[j]])]
				];
				
			];
		][[2]];
		If[Length[nIBPs]>0,nIBPs=nIBPs[[1]]];
	];
	Return[{RawIBPs,nIBPs}];
];


(* ::Subsubsection:: *)
(*FineGrainedZurichSeeding*)


(*FGZurichSeeding[sector_,nFIBPs_,IBPISPdegreeList_,degreeStructures_,OptionsPattern[]]:=Module[{i,DenominatorTypes,CurrentDeg,RawIBPs,nIBPs,RawIBPsAll,nIBPsAll},
	RawIBPsAll={};
	nIBPsAll={};
	For[i=1,i<=Length[degreeStructures],i++,
		{CurrentDeg,DenominatorTypes}=degreeStructures[[i]];
		If[debugModification20230314,DenominatorTypes=DenominatorTypeCompleting[DenominatorTypes]];
		{RawIBPs,nIBPs}=ZurichSeeding[sector,nFIBPs,IBPISPdegreeList,CurrentDeg,DenominatorTypes];
		
	]
]*)


(* ::Subsection::Closed:: *)
(*FindReducedIntegrals and ReduceTowards*)


FindReducedIntegrals[rIBPs_,MIs_]:=Module[{result,tempInts,i},
			result=Reap[
					For[i=1,i<=Length[rIBPs],i++,
						tempInts=Complement[Variables[rIBPs[[i]]],MIs];
						
						If[Length[tempInts]==1,
							Sow[tempInts[[1]]];
						];
				];
			][[2]]//Flatten;
			Return[result];
]

ReduceTowards[rIBPs_,targets_,irredIntegrals_]:=Module[
{result,rowInts,i,rowTargetSet,irredTargets,reducedTargets,irrelavantTargets,relavantTargets,relavantIntegrals
},
	relavantIntegrals=IntegralList[rIBPs,SortTheIntegrals->False];
	relavantTargets=Intersection[relavantIntegrals,targets];
	irrelavantTargets=Complement[targets,relavantTargets];
	irredTargets=Intersection[relavantTargets,irredIntegrals];
	reducedTargets=Complement[relavantTargets,irredTargets];
	result=Reap[
				For[i=1,i<=Length[rIBPs],i++,
					rowInts=Variables[rIBPs[[i]]];
					rowTargetSet=Intersection[rowInts,reducedTargets];
					If[Length[rowTargetSet]>0,
					
						Sow[Complement[rowInts,reducedTargets]];
					];
			];
			][[2]]//Flatten//Union;
	result=Join[irrelavantTargets,irredTargets,result]//Flatten//Union;
	Return[result];
]



(* ::Subsection::Closed:: *)
(*FindIBPs (to seed by more denominator degrees)*)


Options[FindIBPs]={
	Verbosity->1,DenominatorLift->1,AdditionalDegree->4,SelfSymmetryZMaps->{},FunctionTitle->"[AdditionalIBPs]:",
	FIHead->FI0,ZMHead->ZM0
};
FindIBPs[sector_,targets0_,MIs_,(*basicnIBPs_,basicSectorIntegrals_,*)FIBPs_,DenominatorTypes0_,OptionsPattern[]]:=Module[
{timer,memoryUsed,nFIBPFunctions,nFIBPs,secNo,IBPISPdegrees,sectorCut,targets,numeratorMaxDeg,DenominatorTypes1,DenominatorTypes,NewDenominatorTypes,nIBPs,rawIBPs,
denLift,numDeg,denShifts,i,denShift,NewrawIBPs,NewnIBPs,seeds,newSymmetryRelations,zMaps,SectorIntegrals,result,breakQ,redIndex,irredIndex,rIBPs,irredIntegrals,
WellAddressedIntegrals,targetsReduced,allSkipQ,timer2,memoryUsed2,IBPDenominatorDegreeList,IBPNumeratorDegreeList,leafCounts,byteCounts,IBPIndex,newMIs,liftedDenominatorTypes,
NewDenominatorTypesList,j,NewSectorIntegrals,nIBPsUnCut,NewnIBPsUnCut,FIhead,ZMhead
},
	
	targets=Complement[targets0,MIs];
	secNo=SectorNumber[sector];
	sectorCut=SectorCut[sector];
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"  Generating numerical FIBPs0..."]];
	nFIBPs=(FIBPs/.GenericPoint/.GenericD);
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t  nFIBPs0 generated. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	If[seedingViaFIBPFunction,
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"  Generating nFIBPFunctions0..."]];
		nFIBPFunctions=Table[Evaluate[nFIBPs[[k]]/.Table[m[i]->Slot[i],{i,1,SDim}]]&,{k,Length[nFIBPs]}];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t  nFIBPFunctions0 generated. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	];
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t  ","Deriving IBPISPdegrees0..."];];
	IBPISPdegrees=FIBPSectorISPDegree[#,sector]&/@FIBPs;
	IBPISPdegrees=\!\(\*
TagBox["IBPISPdegrees",
FullForm]\)/.\!\(\*
TagBox[
StyleBox[
RowBox[{"Rule", "[", 
RowBox[{
RowBox[{"DirectedInfinity", "[", 
RowBox[{"-", "1"}], "]"}], ",", "0"}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);(*?*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t\t  ","IBPISPdegrees derived. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	numeratorMaxDeg=(Max@@(IntegralISPDegree/@targets))+OptionValue[AdditionalDegree];
	PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"  ","numeratorMaxDeg=",numeratorMaxDeg];
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t  ","Preparing the searching steps..."];];
	(*DenominatorTypes=DenominatorTypes0;
	nIBPs=basicnIBPs/.sectorCut;   (*in FindIBPs, all nIBPs should be on-cut. I think this might be good to also be applied to the main function... let`s see in the future*)
	nIBPsUnCut=basicnIBPs;
	SectorIntegrals=basicSectorIntegrals;*)
	DenominatorTypes={};
	nIBPs={};
	nIBPsUnCut={};
	SectorIntegrals={};
	rawIBPs=BasicRawIBPs/@Range[Length[nIBPs]];
	breakQ=False;
	allSkipQ=True;
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	For[denLift=0,denLift<=OptionValue[DenominatorLift],denLift++,
		denShifts=DenominatorLiftingShifts[sector,denLift];
		NewDenominatorTypesList={};
		For[j=1,j<=Length[denShifts],j++,
			denShift=denShifts[[j]];
			liftedDenominatorTypes=#+denShift&/@DenominatorTypes0;
			DenominatorTypes1=DenominatorTypeCompleting[Join[DenominatorTypes,liftedDenominatorTypes]];
			NewDenominatorTypes=Complement[DenominatorTypes1,DenominatorTypes];
			NewDenominatorTypesList=Join[NewDenominatorTypesList,{NewDenominatorTypes}];
			
		];
		DenominatorTypes=Join[DenominatorTypes,Join@@NewDenominatorTypesList]//Union;
		
		For[numDeg=0,numDeg<=numeratorMaxDeg,numDeg++,
			For[i=1,i<=Length[NewDenominatorTypesList],i++,
				NewDenominatorTypes=NewDenominatorTypesList[[i]];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"","Starting a new step:  ","denLift=",denLift,", ","numDeg=",numDeg,", ","denShift=",denShifts[[i]]];];
				
				If[NewDenominatorTypes==={},
					PrintAndLog["#",secNo,"",""<>OptionValue[FunctionTitle]<>"\tNo new DenominatorTypes in this step. Skipping the rest operations in this step."];
					Continue[]
				];
				timer=AbsoluteTime[];
				memoryUsed=MaxMemoryUsed[
				
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t  ","Zurich seeding. "];];
				If[seedingViaFIBPFunction,
					{NewrawIBPs,NewnIBPs}=ZurichSeedingVianFIBPFunctions[sector,nFIBPFunctions,IBPISPdegrees,numDeg,NewDenominatorTypes];
				,
					{NewrawIBPs,NewnIBPs}=ZurichSeeding[sector,nFIBPs,IBPISPdegrees,numDeg,NewDenominatorTypes];
				];
				
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t\t  ","Zurich seeding finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
				If[Not[NeedSymmetry===False],
					zMaps=OptionValue[SelfSymmetryZMaps];
					timer=AbsoluteTime[];
					memoryUsed=MaxMemoryUsed[
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t","    Appending self-symmetries at current step..."];];
					seeds=Flatten[SeedMerge[NumeratorShifts[sector,#],NewDenominatorTypes]&/@{numDeg},1];
					NewrawIBPs=Join[
						NewrawIBPs,
						Table[SelfSymmetryR[ZM[i],seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten
					];
					newSymmetryRelations=Table[SelfSymmetryRealization[zMaps[[i]]/.GenericPoint,seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten;
					NewnIBPs=Join[
						NewnIBPs,
						newSymmetryRelations
					];
					(*end of MaxMemoryUsed*)];
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"",""<>OptionValue[FunctionTitle]<>"\t\tAppending self-symmetries finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"",""<>OptionValue[FunctionTitle]<>"\t\tNumber of self-symmety relations appended: ",Length[newSymmetryRelations]," ."]];
				];
				If[Length[NewrawIBPs]===0||Length[NewnIBPs]===0,
					PrintAndLog["#",secNo,"",""<>OptionValue[FunctionTitle]<>"\tNo new IBPs found in this step. Skipping the rest operations in this step."];
					Continue[];
				];
				NewnIBPsUnCut=NewnIBPs;
				(*NewnIBPs=NewnIBPs/.sectorCut;*)
				NewnIBPs=(#/.sectorCut)&/@NewnIBPs;
				nIBPs=Join[nIBPs,NewnIBPs];
				rawIBPs=Join[rawIBPs,NewrawIBPs];
				nIBPsUnCut=Join[nIBPsUnCut,NewnIBPsUnCut];
				timer=AbsoluteTime[];
				memoryUsed=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t  ","Deriving SectorIntegrals..."];];
				NewSectorIntegrals=Select[IntegralList[NewnIBPs,SortTheIntegrals->False],Sector[#]==sector&];
				SectorIntegrals=Join[SectorIntegrals,Complement[NewSectorIntegrals,SectorIntegrals]]//DeleteDuplicates
				
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t\t  ","SectorIntegrals derived. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
				If[!SubsetQ[SectorIntegrals,Union[Join[targets,MIs]]],
					PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t\t  ","SectorIntegrals does not cover all targets and MIs. Skipping the rest operations in this step."];
					Continue[];
				];
				allSkipQ=False;
				
				timer=AbsoluteTime[];
				memoryUsed=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t  ","Sorting SectorIntegrals..."];];
				SectorIntegrals=SortBy[SectorIntegrals,IntegralOrdering]//Reverse;
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t\t  ","SectorIntegrals sorted. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
				
				timer=AbsoluteTime[];
				memoryUsed=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t  ","Performing IBPAnalyze..."];];
				{redIndex,irredIndex,rIBPs}=IBPAnalyze[nIBPs,SectorIntegrals];
				irredIntegrals=SectorIntegrals[[irredIndex]];
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t\t  ","IBPAnalyze finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
				WellAddressedIntegrals=FindReducedIntegrals[rIBPs,MIs];
				If[SubsetQ[WellAddressedIntegrals,targets],PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t Found enough IBPs"];breakQ=True];
				If[breakQ===True,Break[]];
			];
			
			If[breakQ===True,Break[]];
		];
		If[breakQ===True,Break[]];
	];
	If[!breakQ,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t Not found enough IBPs."]];
	If[allSkipQ,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t Nothing new happend in FindIBPs."];Return[$Failed]];
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t  ","Rendering the result..."];];
	targetsReduced=Intersection[WellAddressedIntegrals,targets];
	newMIs=ReduceTowards[rIBPs,targets,irredIntegrals];
	IBPIndex=Select[Range[Length[rawIBPs]],FreeQ[rawIBPs[[#]],BasicRawIBPs]&];
	rawIBPs=rawIBPs[[IBPIndex]];
	nIBPs=nIBPs[[IBPIndex]];
	nIBPsUnCut=nIBPsUnCut[[IBPIndex]];
	
	(*rawIBPs=rawIBPs/.FI->FI0/.ZM->ZM0;*)(*ZM0 is same as ZM, just for labelling. FI0 is different from FI*)
	FIhead=OptionValue[FIHead];
	ZMhead=OptionValue[ZMHead];
	rawIBPs=rawIBPs/.FI->FIhead/.ZM->ZMhead;(*allow using other labels*)
	result={rawIBPs,nIBPsUnCut,nIBPs(*this is cutted*),targetsReduced,newMIs};
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	PrintAndLog["#",secNo,""<>OptionValue[FunctionTitle]<>"\t Found ",Length[IBPIndex]," IBPs."];
	result
]


















































































(* ::Subsection:: *)
(*SectorAnalyze (main)*)


Options[SectorAnalyze]:={SeedingMethod->"Zurich",Verbosity->0,AdditionalDegree->SeedingAdditionalDegree,DirectInitialSteps->2,TestOnly->False,
ZurichInitialSteps->3,ModuleIntersectionMethod->"Singular",SectorMappingRules->{},Cut->{},KillNCornerSubsecFIBPs->FIBPsNCornerKill
};
SectorAnalyze[sector_,OptionsPattern[]]:=Module[{secheight,secindex,VectorList,timer,FIBPs,numshifts,r,s,LocalTargets,DenominatorTypes,
i,sectorCut,FIBPs1,CornerIBP,baseIBP,propLocus,ISPLocus,BaikovCut,rawIBPs={},nIBPs={},MIs={},step,newIBPs,seeds,integrals,SectorIntegrals,redIndex,irredIndex,rIBPs,
nFIBPs,WellAddressedIntegrals,secNo,degRep,rr,IBPDegreeList,IBPIndex,ReducedIntegrals,UsedIndex,subsector,SubsectorInts,tempInts,
IBPISPdegrees,NewrawIBPs,NewnIBPs,timer2,M1,M1ext,M2,sectorMaps,mappedSectors,tailSectors,leafCounts,byteCounts,maxDenominatorIndices,formalIntegrals,
zs,zMaps,newNIBPs,FIBPCurrentSectorIntegrals,memoryUsed,memoryUsed2,nFIBPFunctions,newSymmetryRelations,VectorList1,irredIntegrals,FIBPs0,FIBPISPDegree0,
redundantMIs,nFIBPs0,azuritinoMIFolder,nFIBPFunctions0,redundantMIsReduced,newMIs,FindIBPResult,redundantMIsToBeReduced,CompensationIBPDenominatorDegrees,
reduceTowards,remainedFIBPlabels,usedVectors,usedFIBPs,additionalResultFolder,memoryUsed3,timer3,nIBPsCutted,NewnIBPsCutted,newSymmetryRelationsCutted,NewSectorIntegrals,
NeatIBPIntersectionDegreeBoundDecreased,VectorListSimplifiedByCut,VectorListSimplifiedByCutWithDCornerOnly,VectorList00,FIBPs00,syzygies,syzygiesForM1
},
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["--------------------------------------------------------------------\nInitializing new sector ",sector," ..."]];
	
	
	secheight=SectorHeight[sector];
	secindex=SectorIndex[sector];
	secNo=SectorNumber[sector];
	
	secNum=secNo;
	
	If[!MemberQ[Global`NonZeroSectors,sector],PrintAndLog["This is an irrelavant sector or zero sector."]; Return[]];
	
	
	
	FIBPs00=None;
	LocalTargets=ReductionTasks[sector];
	PrintAndLog["Target integrals: ",LocalTargets//Length];
	
	
	(*sectorMaps=OptionValue[SectorMappingRules];
	mappedSectors=sectorMaps[[All,1]];*)
	(*If[MemberQ[mappedSectors,sector]&&(Not[NeedSymmetry===False]),
		PrintAndLog["This is an mapped sector."];
		Return[];
		rawIBPs=#-(#/.G[x__]:>GMapped[sectorMaps,{x}])&/@LocalTargets
		(* but it seems impossible to have new targets amerge in mapped sectors *)
	];*)
	
	
	
	propLocus=Position[sector,1]//Flatten;
	ISPLocus=Position[sector,0]//Flatten;
	
	sectorCut=SectorCut[sector];	
	BaikovCut=Table[z[propLocus[[i]]]->0,{i,1,Length[propLocus]}];
	
	r=Max[IntegralPropagatorDegree/@LocalTargets];
	s=Max[IntegralISPDegree/@LocalTargets];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Initialized. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ]];
	If[LocalTargets==={},
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  No target integrals on this sector, skipping the rest steps. "];];
		Return[]
	];  (* Nothing to reduce *)
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Driving DenominatorTypes..."]];
	DenominatorTypes=Union[Append[IntegralPropagatorType/@LocalTargets,sector]];
	If[debugModification20230314,DenominatorTypes=DenominatorTypeCompleting[DenominatorTypes]];
	If[DenominatorTypeLiftIndexLists=!={},
		DenominatorTypes=DenominatorTypeLift[DenominatorTypes,DenominatorTypeLiftIndexLists,sector];
	];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  DenominatorTypes derived. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	If[OptionValue[Verbosity]==1,
		PrintAndLog["#",secNo,"\tTarget Info:  MaxNumDeg=",s,";  DenominatorTypes=", DenominatorTypes];
	];
	
	
	If[Not[NeedSymmetry===False],
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Finding z maps of sector self-symmetries..."]];
		zs=z/@Range[Length[sector]];
		zMaps=DeleteCases[FindSymmetry[sector],#->#&/@zs];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Z maps of sector self-symmetries found. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"   ",Length[zMaps]," zMap(s) found."];]
	];
	
	If[MIFromAzuritino===True,
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Finding MIs using Azuritino..."]];
		MIs=AzuritinoMIFind[sector,
			CriticalPointCounting->CriticalPointInAzuritino,
			selfSymmetryZMaps->zMaps,
			degBound->AzuritinoIntersectionDegreeBound
		];
		azuritinoMIFolder=outputPath<>"tmp/azuritino_MIs/";
		If[!DirectoryQ[#],Run["mkdir -p "<>#]]&[azuritinoMIFolder];
		Export[azuritinoMIFolder<>ToString[secNo]<>".txt",MIs//InputForm//ToString];
		(*end of MaxMemoryUsed*)];
	
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  MIs found. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"   ",Length[MIs]," MI(s) : ",MIs];];
		If[SubsetQ[MIs,LocalTargets],
			(* No need to generate IBPs for this sector *)
			Global`MIList[sector]=MIs;
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  All target integrals are MIs, skipping the rest steps. "];];
			Return[];
		];
		
	];
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  numerator degree: ",s," propagator degree: ",r];];
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Solving module intersections..."]];
	Switch[OptionValue[ModuleIntersectionMethod],
	"Singular",
		If[FlexibleNeatIBPIntersectionDegreeBound=!=True,
			VectorList=SingularIntersection[secindex,
				degBound->NeatIBPIntersectionDegreeBound,
				VariableOrder->(var//Reverse),
				Cut->OptionValue[Cut],
				PrintDegBound->True,
				NumericMode->NumericIBP,
				SyzOnlyMode->VectorListFromSyzInSingularIntersection
			];
			
		,
			For[
				NeatIBPIntersectionDegreeBoundDecreased=0,
				NeatIBPIntersectionDegreeBoundDecreased<=NeatIBPIntersectionDegreeBoundDecreaseLimit,
				NeatIBPIntersectionDegreeBoundDecreased++,
				
				VectorList=TimeConstrained[
					SingularIntersection[secindex,
						degBound->NeatIBPIntersectionDegreeBound-NeatIBPIntersectionDegreeBoundDecreased,
						VariableOrder->(var//Reverse),
						Cut->OptionValue[Cut],
						PrintDegBound->True,
						NumericMode->NumericIBP,
						SingularTimeUsedLimit->NeatIBPIntersectionTimeConstraintForFlexibleDegreeBound+10,
						(*10 more seconds to make sure. This number is just a time to kill Singular zombie, so it does not matter to +10*)
						SyzOnlyMode->VectorListFromSyzInSingularIntersection
					],
					NeatIBPIntersectionTimeConstraintForFlexibleDegreeBound,
					$Failed
				];
				If[VectorList===$Failed,
					PrintAndLog[
						"#",secNo,
						"  Singular intersection returns $Failed at time limit (",
						ToString[NeatIBPIntersectionTimeConstraintForFlexibleDegreeBound],
						" second(s)) with degbound=",
						ToString[NeatIBPIntersectionDegreeBound-NeatIBPIntersectionDegreeBoundDecreased]
					];
					If[NeatIBPIntersectionDegreeBoundDecreased===NeatIBPIntersectionDegreeBoundDecreaseLimit,
						PrintAndLog[
							"#",secNo,"   *****still cannot calculate Singular intersection at degbound=",
							ToString[NeatIBPIntersectionDegreeBound-NeatIBPIntersectionDegreeBoundDecreased]
						];
						PrintAndLog["********** Sector ",secNo," Failed."];
						Quit[];
					,
						PrintAndLog[
							"#",secNo,"   Trying Singular intersection at degbound=",
							ToString[NeatIBPIntersectionDegreeBound-NeatIBPIntersectionDegreeBoundDecreased-1]
						]
					]
				,
					Break[];
				]
			];
		];
		If[VectorList===$Failed,
			PrintAndLog["#",secNo,"\t\t","***** Singular running returns $Failed. "];
			PrintAndLog["***** sector",secNo," failed. Exiting."];
			Exit[1];
		];
		If[VectorListFromSyzInSingularIntersection===True,
			{M1,M1ext,M2}=TangentModules[secindex,{}];
			syzygies=VectorList;(*name VectorList is borrowed temply in the mode where VectorListFromSyzInSingularIntersection=True*)
			syzygiesForM1=syzygies[[All,1;;Length[M1]]];
			VectorList=Sum[#[[i]]*M1ext[[i]],{i,Length[M1ext]}]&/@syzygiesForM1;
		]
	,
	"Linear",
		{M1,M1ext,M2}=TangentModules[secindex,{}];
		VectorList=SolveDegreedIntersection[M1ext,secindex,LinearSyzDegree],
	"Singular+Linear",
		If[FlexibleNeatIBPIntersectionDegreeBound===True,PrintAndLog["#",secNo,"Sorry, Singular+Linear method dose not yet support flexible degbound. Ignoring this option..."]];
		VectorList=SingularIntersection[secindex,
			degBound->NeatIBPIntersectionDegreeBound,
			VariableOrder->(var//Reverse),
			Cut->OptionValue[Cut],
			PrintDegBound->True,
			NumericMode->NumericIBP
		];
		If[VectorList===$Failed,
			PrintAndLog["#",secNo,"\t\t","***** Singular running returns $Failed. "];
			PrintAndLog["***** sector",secNo," failed. Exiting."];
			Exit[1];
		];
		{M1,M1ext,M2}=TangentModules[secindex,{}];
		VectorList1=SolveDegreedIntersection[M1ext,secindex,LinearSyzDegree];
		VectorList=Join[VectorList1,VectorList];
	];
	(*ProbeIntermediateResult["vectors",secNo,VectorList];*)
	(*end of MaxMemoryUsed*)];
	
	If[ExportTheModules===True,
		{M1,M1ext,M2}=TangentModules[secindex,{}];
		ConvenientExport[outputPath<>"results/additional_outputs/M1/"<>ToString[secNo]<>".txt",M1//InputForm//ToString];
		ConvenientExport[outputPath<>"results/additional_outputs/M1ext/"<>ToString[secNo]<>".txt",M1ext//InputForm//ToString];
		ConvenientExport[outputPath<>"results/additional_outputs/M2/"<>ToString[secNo]<>".txt",M2//InputForm//ToString];
	];
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Module intersections solved. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  VectorList Length: ", Length[VectorList]]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  VectorList ByteCount: ", ByteCount[VectorList]]];
	(*If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Module intersection ",AbsoluteTime[]-timer];];*)
	
	If[SimplifySyzygyVectorsByCut===True,
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Simplifying vector list by cut..."]];
		VectorListSimplifiedByCut=SimplifyByCut[VectorList,secindex,
			DCornerOnly->And[(DenominatorTypes==={sector}),AllowingDCornerOnlyModeInSimplifyByCut],
			sectorNumber->secNo,
			Modulus->FiniteFieldModulus2
		];
		If[!StrictDenominatorPowerIndices,VectorList00=VectorList];
		If[ResultInRestoredLaportaIBPs,VectorList00=VectorList];
		
		(*Terminology (very easy to be confused, so I write it here)
			simplify by cut: SBC, kill N corner:KNC
			1.SBC yes, KNC yes, VecListfromSingular---->FIBPs00---(SBC)--\[Rule] FIBPs0 ---(KNC)--\[Rule] FIBPs 
			2.SBC no , KNC yes, VecListfromSingular----------------------\[Rule] FIBPs0 ---(KNC)--\[Rule] FIBPs 
			3.SBC yes, KNC no , VecListfromSingular---->FIBPs00---(SBC)----FIBPs0----(do nothing)---\[Rule] FIBPs 
			4.SBC no , KNC no , VecListfromSingular-----------------------\[Rule]FIBPs0----(do nothing)---\[Rule] FIBPs 
			This is just the logical relation, in codes, FIBPs0 is generated by VectorList after SBC
			
		*)
		VectorList=VectorListSimplifiedByCut;
		
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  "," Finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	];
	
	If[ExportAllSyzygyVectors,
		ConvenientExport[outputPath<>"results/additional_outputs/all_syzygy_vectors/"<>ToString[secNo]<>".txt",VectorList//InputForm//ToString];
	];
	If[ExportAllSyzygyVectorsBeforeSimplification,
		ConvenientExport[
			outputPath<>"results/additional_outputs/all_syzygy_vectors_before_simplification/"<>ToString[secNo]<>".txt",
			VectorList00//InputForm//ToString
		];
	];
	
	(*ProbeIntermediateResult["VectorList",secNo,VectorList];(*debug2023*)*)
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Generating Formal IBPs..."]];
	FIBPs=IBPGenerator[#,secindex,Cut->OptionValue[Cut]]&/@VectorList;
	If[!StrictDenominatorPowerIndices,FIBPs0=FIBPs];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[FIBPs]," Formal IBPs generated. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	If[ExportAllFIBPs,
		ConvenientExport[outputPath<>"results/additional_outputs/all_FIBPs/"<>ToString[secNo]<>".txt",FIBPs//InputForm//ToString];
	];
	
	(*ProbeIntermediateResult["FIBPs",secNo,FIBPs];*)
	
	
	(* Remove IBPs for lower sectors *)
	(* But this will wrongly kill some IBPs that generates IBPs not for lower sectors at non-corner seed*)
	(* I (zihao) suggest we turn off this step, the subsec IBPs will be automatically removed in the next steps so it is not needed here to do so*)
	(* In the new versions, this step is modified and on.*)
	(* According to dev_log the above comments seem to be updated at 2023.4.6*)
	
	If[OptionValue[KillNCornerSubsecFIBPs],
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Removing FIBPs for lower sectors seeding with DenominatorTypes..."]];
		FIBPs1={};
		CornerIBP={};
		remainedFIBPlabels={};
		For[i=1,i<=Length[FIBPs],i++,
			(*baseIBP=IntegralRealization[FIBPs[[i]],sector];*)
			
			(*If[(baseIBP/.sectorCut)===0&&(Union[VectorList[[i,propLocus]]/.BaikovCut]==={0}),
				Continue[];  (* This IBP corresponds to a lower sector *)
			]; *)
			
			maxDenominatorIndices=Max/@Transpose[DenominatorTypes];
			(*WARNING: why use max here? what if there is a term (m1-2)G[...], then m1=2\[Rule]subsecIBP but m1=1 not \[Rule] subsecIBP
			2025.1.3: well... the coefficients does not matter because we are looking into integrals themselves
			  *)
			formalIntegrals=Cases[Variables[FIBPs[[i]]],_G];
			
			FIBPCurrentSectorIntegrals=Union[
				Expand[
					(IntegralRealization[#,maxDenominatorIndices]/.sectorCut)&/@formalIntegrals
				]
			];
			
			If[DeleteCases[FIBPCurrentSectorIntegrals,0]==={},
				Continue[];  (* This FIBP seeds to a lower sector in DenominatorTypes*)
			]; 
			AppendTo[remainedFIBPlabels,i];
			AppendTo[FIBPs1,FIBPs[[i]]];
			AppendTo[CornerIBP,baseIBP];
		];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[FIBPs]," Formal IBPs are generated; ",Length[FIBPs1]," Formal IBPs are used. ",Length[FIBPs]-Length[FIBPs1]," Formal IBPs are removed. "(*,AbsoluteTime[]-timer*)];];
		FIBPs=FIBPs1;
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  IBPs for lower sectors removed. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	];
	If[ExportNCornerKilledFIBPs,
		ConvenientExport[outputPath<>"results/additional_outputs/n_corner_killed_FIBPs/"<>ToString[secNo]<>".txt",FIBPs//InputForm//ToString];
	];
	(*ProbeIntermediateResult["FIBPs",secNo,FIBPs];(*debug2023*)*)
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Generating numerical FIBPs..."]];
	
	(*nFIBPs=CollectG/@(FIBPs/.GenericPoint/.GenericD);*)
	nFIBPs=(FIBPs/.GenericPoint/.GenericD);
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  nFIBPs generated. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	If[seedingViaFIBPFunction,
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Generating nFIBPFunctions..."]];
		nFIBPFunctions=Table[Evaluate[nFIBPs[[k]]/.Table[m[i]->Slot[i],{i,1,SDim}]]&,{k,Length[nFIBPs]}];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  nFIBPFunctions generated. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	];
	
	
	
	
(*	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Collecting Gs..."]];
	
	nFIBPs=CollectG/@nFIBPs;
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"Collecting Gs Finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	timer=AbsoluteTime[];*)

	(* Seeding *)
	
	Switch[OptionValue[SeedingMethod],
		(*
		"Direct",(*stopped updating since 2023.4.12*)
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[seedingViaFIBPFunction,
			PrintAndLog["WARNING: This version does not support seeding via FIBP Functions in Method \"Direct\". We are seeding via index replacement."];
		];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Seeding in step 0..."]];
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		(* Step 0 *)
		step=0;
		seeds=Flatten[SeedMerge[NumeratorShifts[sector,#],DenominatorTypes]&/@Range[0,OptionValue[DirectInitialSteps]],1];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["\t\t seeds created. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		(* rawIBPs=Table[IntegralR[FIBPs[[i]],seeds[[j]]],{i,1,Length[FIBPs]},{j,1,Length[seeds]}]//Flatten; *)
		rawIBPs=Table[IntegralR[FI[i],seeds[[j]]],{i,1,Length[FIBPs]},{j,1,Length[seeds]}]//Flatten;
		
		If[NeedSymmetry===False,
		(*timing and memory report omitted here, and, not updating*)
			nIBPs=rawIBPs/.FI[i_]:>nFIBPs[[i]]/.IntegralR->IntegralRealization/.sectorCut
		,
			rawIBPs=Join[
				rawIBPs,
				Table[SelfSymmetryR[ZM[i],seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten
			];
			nIBPs=rawIBPs;
			nIBPs=nIBPs/.FI[i_]:>nFIBPs[[i]]/.IntegralR->IntegralRealization;
			nIBPs=nIBPs/.ZM[i_]:>(zMaps[[i]]/.GenericPoint)/.SelfSymmetryR->SelfSymmetryRealization;
			nIBPs=nIBPs/.sectorCut;
		];
		
		
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["\t\t nIBPs created. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		SectorIntegrals=IntegralList[nIBPs];
		{redIndex,irredIndex,rIBPs}=IBPAnalyze[nIBPs,SectorIntegrals];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["\t\t IBPAnalyze finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		If[Not[MIFromAzuritino===True],
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			MIs=SectorIntegrals[[irredIndex]];    (* This is not the final MI *)
			MIs=Select[MIs,IntegralISPDegree[#]<=OptionValue[DirectInitialSteps]&];  (* Assume the high degree master integrals are not MIs *)
			(*end of MaxMemoryUsed*)];
		];
		If[OptionValue[Verbosity]==1,PrintAndLog["\t\t MIs created in step 0. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["\tFinished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
		
		
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","s=",OptionValue[DirectInitialSteps],", ",Length[rawIBPs]," IBPs are generated"];];
		
		For[step=OptionValue[DirectInitialSteps]+1,step<=s+OptionValue[AdditionalDegree],step++,
			timer=AbsoluteTime[];
			memoryUsed=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Seeding in step ",step,"..."]];
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			seeds=SeedMerge[NumeratorShifts[sector,step],DenominatorTypes];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["\t\t seeds created. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			newIBPs=Table[IntegralR[FI[i],seeds[[j]]],{i,1,Length[FIBPs]},{j,1,Length[seeds]}]//Flatten;
			
			If[NeedSymmetry===False,
			(*timing and memory report omitted here, and, not updating*)
				rawIBPs=Join[rawIBPs,newIBPs];
				nIBPs=Join[nIBPs,newIBPs/.FI[i_]:>nFIBPs[[i]]/.IntegralR->IntegralRealization/.sectorCut];
			,
				newIBPs=Join[
					newIBPs,
					Table[SelfSymmetryR[ZM[i],seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten
				];
				rawIBPs=Join[rawIBPs,newIBPs];
				newNIBPs=newIBPs;
				newNIBPs=newNIBPs/.FI[i_]:>nFIBPs[[i]]/.IntegralR->IntegralRealization;
				newNIBPs=newNIBPs/.ZM[i_]:>(zMaps[[i]]/.GenericPoint)/.SelfSymmetryR->SelfSymmetryRealization;
				newNIBPs=newNIBPs/.sectorCut;
				nIBPs=Join[nIBPs,newNIBPs]
			];
			
			
		
			
			
			
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["\t\t nIBPs created. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			SectorIntegrals=IntegralList[nIBPs];
		
			{redIndex,irredIndex,rIBPs}=IBPAnalyze[nIBPs,SectorIntegrals];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["\t\t IBPAnalyze finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			WellAddressedIntegrals=FindReducedIntegrals[rIBPs,MIs];
			
			WellAddressedIntegrals=Join[WellAddressedIntegrals,MIs];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["\t\t WellAddressedIntegrals finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["\tFinished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","s=",step,", ",Length[newIBPs]," more IBPs are generated"];];
			
			If[SubsetQ[WellAddressedIntegrals,LocalTargets],Break[]];
			
		];
		
		
		,
		*)
		"Zurich",
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Seeding in step 0..."]];
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Deriving IBPISPdegrees..."];];
		
		
		(*IBPISPdegrees=IBPISPSectorDegree[#,sector]&/@(CornerIBP/.GenericD//Expand);*)
		
		(*IBPISPdegrees=IBPISPSectorDegree[
			Collect[
				#/.GenericD/.GenericPoint,
				Cases[Variables[#],_G]
			]
			,sector
		]&/@(CornerIBP);*)
		(*do not use corner IBP to estimate FIBP degree, it may cause error by accident.*)
		
		IBPISPdegrees=FIBPSectorISPDegree[#,sector]&/@FIBPs;
		IBPISPdegrees=\!\(\*
TagBox["IBPISPdegrees",
FullForm]\)/.\!\(\*
TagBox[
StyleBox[
RowBox[{"Rule", "[", 
RowBox[{
RowBox[{"DirectedInfinity", "[", 
RowBox[{"-", "1"}], "]"}], ",", "0"}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\);(*?*)
		(*ProbeIntermediateResult["IBPISPdegrees",secNo,IBPISPdegrees];*)
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","IBPISPdegrees derived. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
		
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  \t","Zurich seeding..."];];
		If[seedingViaFIBPFunction,
			{rawIBPs,nIBPs}=Flatten/@(ZurichSeedingVianFIBPFunctions[sector,nFIBPFunctions,IBPISPdegrees,#,DenominatorTypes]&/@Range[0,OptionValue[ZurichInitialSteps],1]//Transpose);
			
		,
			{rawIBPs,nIBPs}=Flatten/@(ZurichSeeding[sector,nFIBPs,IBPISPdegrees,#,DenominatorTypes]&/@Range[0,OptionValue[ZurichInitialSteps],1]//Transpose);
			
		];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","\t\tZurich seeding finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  \t","Cutting nIBPs..."];];
		nIBPsCutted=(#/.sectorCut)&/@nIBPs;(*compared to nIBPs/.sectorCut, not faster, but much less memory used *)
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","\t\tFinished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
		
		If[Not[NeedSymmetry===False],
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  \t","Appending self-symmetries at current step..."];];
			
			seeds=Flatten[SeedMerge[NumeratorShifts[sector,#],DenominatorTypes]&/@Range[0,OptionValue[ZurichInitialSteps]],1];
			
			rawIBPs=Join[
				rawIBPs,
				Table[SelfSymmetryR[ZM[i],seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten
			];
			newSymmetryRelations=Table[SelfSymmetryRealization[zMaps[[i]]/.GenericPoint,seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten;
			newSymmetryRelationsCutted=(#/.sectorCut)&/@newSymmetryRelations;
			nIBPs=Join[
				nIBPs,
				newSymmetryRelations
			];
			nIBPsCutted=Join[
				nIBPsCutted,
				newSymmetryRelationsCutted
			];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","\t\tAppending self-symmetries finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","\t\tNumber of self-symmety relations appended: ",Length[newSymmetryRelations]," ."]];
		];
		
		
		
		
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Deriving SectorIntegrals..."];];
		SectorIntegrals=Select[IntegralList[(*nIBPs/.sectorCut*)nIBPsCutted,SortTheIntegrals->False],Sector[#]==sector&];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","SectorIntegrals Derived. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Sorting SectorIntegrals..."];];
		SectorIntegrals=SortBy[SectorIntegrals,IntegralOrdering]//Reverse;
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","SectorIntegrals sorted. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		(*ProbeIntermediateResult["nIBPs_and_SectorIntegrals",secNo,{nIBPs,SectorIntegrals}];*)
		If[SectorIntegrals=!={},
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Performing IBPAnalyze..."];];
			{redIndex,irredIndex,rIBPs}=IBPAnalyze[(*nIBPs/.sectorCut*)nIBPsCutted,SectorIntegrals];
			(*SectorIntegrals=Select[IntegralList[nIBPs],Sector[#]==sector&];*)(*commented out at 2023.4.15*)
			If[Not[MIFromAzuritino===True],
				MIs=SectorIntegrals[[irredIndex]];    (* This is not the final MI *)
			];
			(*end of MaxMemoryUsed*)];
		,
			If[MIFromAzuritino=!=True,
				PrintAndLog["#",secNo,"\t\t  ","No sector integrals found in the initial step.\n Try larger the initial step degree (currently ",OptionValue[ZurichInitialSteps],") or turn on MIFromAzuritino.\n****** Sector ",secNo," Failed. ******"];
				Quit[];
			,
				PrintAndLog["#",secNo,"\t\t  ","Note: there is no sector integrals found in the initial step. Discarding the IBPs generated in theses steps."];
				rawIBPs={};
				nIBPs={};
				nIBPsCutted={};
			];
			
		];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","IBPAnalyze finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Seeding in step 0 finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
		
		For[step=OptionValue[ZurichInitialSteps]+1,step<=s+OptionValue[AdditionalDegree],step++,
			timer=AbsoluteTime[];
			memoryUsed=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Seeding in step ",step,"..."]];
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Zurich seeding..."];];
			
			
			If[seedingViaFIBPFunction,
				{NewrawIBPs,NewnIBPs}=ZurichSeedingVianFIBPFunctions[sector,nFIBPFunctions,IBPISPdegrees,step,DenominatorTypes];
				
			,
				{NewrawIBPs,NewnIBPs}=ZurichSeeding[sector,nFIBPs,IBPISPdegrees,step,DenominatorTypes];
			];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Zurich seeding finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  \t","Cutting new nIBPs..."];];
			NewnIBPsCutted=(#/.sectorCut)&/@NewnIBPs;
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","\t\tFinished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
			
			If[Not[NeedSymmetry===False],
				timer2=AbsoluteTime[];
				memoryUsed2=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  \t","Appending self-symmetries at current step..."];];
				seeds=Flatten[SeedMerge[NumeratorShifts[sector,#],DenominatorTypes]&/@{step},1];
				NewrawIBPs=Join[
					NewrawIBPs,
					Table[SelfSymmetryR[ZM[i],seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten
				];
				newSymmetryRelations=Table[SelfSymmetryRealization[zMaps[[i]]/.GenericPoint,seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten;
				newSymmetryRelationsCutted=(#/.sectorCut)&/@newSymmetryRelations;
				NewnIBPs=Join[
					NewnIBPs,
					newSymmetryRelations
				];
				NewnIBPsCutted=Join[
					NewnIBPsCutted,
					newSymmetryRelationsCutted
				];
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","\t\tAppending self-symmetries finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","\t\tNumber of self-symmety relations appended: ",Length[newSymmetryRelations]," ."]];
			];
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Joining results of the steps..."];];
			rawIBPs=Join[rawIBPs,NewrawIBPs];
			nIBPs=Join[nIBPs,NewnIBPs];
			nIBPsCutted=Join[nIBPsCutted,NewnIBPsCutted];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Joining finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Deriving SectorIntegrals..."];];
				(*timer3=AbsoluteTime[];
				memoryUsed3=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Cutting nIBPs..."];];
				nIBPsCutted=nIBPs/.sectorCut;
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer3],  " second(s). Memory used: ",Round[memoryUsed3/(1024^2)]," MB."]];
				*)
				timer3=AbsoluteTime[];
				memoryUsed3=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Getting integral list from new NewnIBPsCutted..."];];
				NewSectorIntegrals=IntegralList[NewnIBPsCutted,SortTheIntegrals->False]
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer3],  " second(s). Memory used: ",Round[memoryUsed3/(1024^2)]," MB."]];
				
				timer3=AbsoluteTime[];
				memoryUsed3=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Unioning SectorIntegrals..."];];
				SectorIntegrals=Join[SectorIntegrals,NewSectorIntegrals]//DeleteDuplicates;
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer3],  " second(s). Memory used: ",Round[memoryUsed3/(1024^2)]," MB."]];
				
				
				timer3=AbsoluteTime[];
				memoryUsed3=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Correcting integral list..."];];
				SectorIntegrals=Select[SectorIntegrals,Sector[#]==sector&]
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer3],  " second(s). Memory used: ",Round[memoryUsed3/(1024^2)]," MB."]];
				
				
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","SectorIntegrals derived. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Sorting SectorIntegrals..."];];
			SectorIntegrals=SortBy[SectorIntegrals,IntegralOrdering]//Reverse;
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","SectorIntegrals sorted. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			
			
			
			If[SectorIntegrals==={},
				If[MIFromAzuritino=!=True,
					PrintAndLog["#",secNo,"\t\t  ","No sector integrals found in step ",step,".\n This should be an unexpected error.\n****** Sector ",secNo," Failed. ******"];
					Quit[];
				,
					PrintAndLog["#",secNo,"\t\t  ","Note: there is no sector integrals found in step ",step,". Discarding the IBPs generated and skipping the rest of this step."];
					rawIBPs={};
					nIBPs={};
					nIBPsCutted={};
					WellAddressedIntegrals={};
				];
			,
				timer2=AbsoluteTime[];
				memoryUsed2=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Performing IBPAnalyze..."];];
				{redIndex,irredIndex,rIBPs}=IBPAnalyze[(*nIBPs/.sectorCut*)nIBPsCutted,SectorIntegrals];
				irredIntegrals=SectorIntegrals[[irredIndex]];
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","IBPAnalyze finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
				
				timer2=AbsoluteTime[];
				memoryUsed2=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Ending this step..."];];
				If[Not[MIFromAzuritino===True],
					MIs=Intersection[MIs,SectorIntegrals[[irredIndex]]];
				];
				WellAddressedIntegrals=FindReducedIntegrals[rIBPs,MIs];
			];
			WellAddressedIntegrals=Join[WellAddressedIntegrals,MIs];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","s=",step,", ",Length[NewnIBPs]," more IBPs are generated"];];
			(*end of MaxMemoryUsed*)];
			
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Step",step," finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Seeding in step ",step," finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
			If[SubsetQ[WellAddressedIntegrals,LocalTargets],
				Break[]
			];
			If[step==s+OptionValue[AdditionalDegree],
				If[And[StrictDenominatorPowerIndices===False,AllowedDenominatorPowerLift>0],
					PrintAndLog["#",secNo,"\t","[Notice]: Targets are still not reduced to MIs in the maximum step ",
						step,". Trying to find more IBPs by seeds with denominator powers lifted."];
					
					(*FIBPs0 is the original FIBPs without killing n corners*)
					redundantMIs=ReduceTowards[rIBPs,LocalTargets,irredIntegrals];
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ",Length[redundantMIs]," redundant MI(s) : ",redundantMIs];];
					redundantMIsToBeReduced=Complement[redundantMIs,MIs];
					
					
					If[SimplifySyzygyVectorsByCut,
						(*
							unfortunately, if SimplifySyzygyVectorsByCut has been used, we cannot use FIBPs0,
							because it is still from simplified vector list,
							the simplification, is based on an assumption:
								sub-layer IBPs are deletable. (1)
							(if we used DCornerOnly, it dose not change the statement, because it is based on:
								sub-sector IBPs are deletable. (2)
							but (2) is stronger than statement (1), so, (1) is (2)'s necessary condition,
							so whatever we used DCornerOnly or not, we say (1)  must holds.
							)
							But, the reality is, (1) does not hold here. Otherwise, we will not reach this part of codes, would we? (possibly because we used degbound in Singular)
							So, we cannot use simplified vector list here.
						*)
						timer=AbsoluteTime[];
						memoryUsed=MaxMemoryUsed[
						If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  We cannot use FIBPs0 since it is from simplified vector list."]];
						If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  We use FIBPs00 to take place FIBPs0."]];
						If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Generating FIBPs00 from VectorList00..."]];
						FIBPs00=IBPGenerator[#,secindex,Cut->OptionValue[Cut]]&/@VectorList00;
						(*end of MaxMemoryUsed*)];
						If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","FIBPs00 generated. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
						
						memoryUsed=MaxMemoryUsed[
						FindIBPResult=FindIBPs[
							sector,redundantMIsToBeReduced,MIs,(*nIBPs,SectorIntegrals,*)FIBPs00,DenominatorTypes,
							DenominatorLift->AllowedDenominatorPowerLift,AdditionalDegree->OptionValue[AdditionalDegree],SelfSymmetryZMaps->zMaps,
							FIHead->FI00,ZMHead->ZM00
						]
						(*end of MaxMemoryUsed*)];
						If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","AdditionalIBPs finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
					,
						timer=AbsoluteTime[];
						memoryUsed=MaxMemoryUsed[
						FindIBPResult=FindIBPs[
							sector,redundantMIsToBeReduced,MIs,(*nIBPs,SectorIntegrals,*)FIBPs0,DenominatorTypes,
							DenominatorLift->AllowedDenominatorPowerLift,AdditionalDegree->OptionValue[AdditionalDegree],SelfSymmetryZMaps->zMaps
						]
						(*end of MaxMemoryUsed*)];
						If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","AdditionalIBPs finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
					];
					
					
				
					If[FindIBPResult===$Failed,
						PrintAndLog["#",secNo,"\t","*** Failed to find more IBPs by seeds with denominator powers lifted."];
						{NewrawIBPs,NewnIBPs,NewnIBPsCutted,redundantMIsReduced,newMIs}={{},{},{},{},redundantMIs}
					,
						{NewrawIBPs,NewnIBPs,NewnIBPsCutted,redundantMIsReduced,newMIs}=FindIBPResult;
					];
					If[Complement[redundantMIsToBeReduced,redundantMIsReduced]=!={},
						If[StrictMI===True,
							PrintAndLog["#",secNo,"\t","*** Targets are still not reduced to MIs Even after seeding with denominator powers lifted. Failed.\n****** Sector ",secNo," Failed. ******"];
							Quit[];
						,
							PrintAndLog["#",secNo,"\t","*** Targets are still not reduced to MIs Even after seeding with denominator powers lifted. Redefining master integrals."];
							MIs=newMIs;
						]
					];
					
					nIBPs=Join[nIBPs,NewnIBPs];
					nIBPsCutted=Join[nIBPsCutted,NewnIBPsCutted];
					rawIBPs=Join[rawIBPs,NewrawIBPs];
					
					timer=AbsoluteTime[];
					memoryUsed=MaxMemoryUsed[
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Checking the result from AdditionalIBPs..."];];
					
					
					timer2=AbsoluteTime[];
					memoryUsed2=MaxMemoryUsed[
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Deriving SectorIntegrals..."];];
					SectorIntegrals=Select[IntegralList[(*nIBPs/.sectorCut*)nIBPsCutted,SortTheIntegrals->False],Sector[#]==sector&];
					(*this is unimproved SectorIntegrals generation way, but ... lets see if it is slow, if not , leave it unimproved is ok*)
					(*end of MaxMemoryUsed*)];
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","SectorIntegrals Derived. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
					timer2=AbsoluteTime[];
					memoryUsed2=MaxMemoryUsed[
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Sorting SectorIntegrals..."];];
					SectorIntegrals=SortBy[SectorIntegrals,IntegralOrdering]//Reverse;
					(*end of MaxMemoryUsed*)];
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","SectorIntegrals sorted. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
					
					timer2=AbsoluteTime[];
					memoryUsed2=MaxMemoryUsed[
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Performing IBPAnalyze..."];];
					{redIndex,irredIndex,rIBPs}=IBPAnalyze[(*nIBPs/.sectorCut*)nIBPsCutted,SectorIntegrals];
					(*end of MaxMemoryUsed*)];
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","IBPAnalyze finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
					timer2=AbsoluteTime[];
					memoryUsed2=MaxMemoryUsed[
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Performing ReduceTowards..."];];
					reduceTowards=ReduceTowards[rIBPs,LocalTargets,SectorIntegrals[[irredIndex]]];
					(*end of MaxMemoryUsed*)];
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
				
					
					
					
					If[SubsetQ[MIs,reduceTowards],
						PrintAndLog["#",secNo,"\t\t nIBPs is enough to reduce all LocalTargets to MIs."];
					,
						PrintAndLog["#",secNo,"\t\t **** nIBPs is NOT ENOUGH to reduce all LocalTargets to MIs."];
					];
					(*end of MaxMemoryUsed*)];
					If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
					
					
				,
					If[StrictMI===True,
						PrintAndLog["#",secNo,"\t","*** Targets are still not reduced to MIs in the maximum step ",step,". Failed.\n****** Sector ",secNo," Failed. ******"];
						Quit[];
					,
						PrintAndLog["#",secNo,"\t","***Warning: Targets are still not reduced to MIs in the maximum step ",step,". Redefining master integrals."];
						MIs=ReduceTowards[rIBPs,LocalTargets,irredIntegrals];
						(*ProbeIntermediateResult["rIBPs",secNo,rIBPs];*)
					];
				];
			]
		];
		
		
		
	];
	
	(*ProbeIntermediateResult["original_nIBPs",secNo,nIBPs];(*debug20230421*)*)
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ",Length[MIs]," MI(s) : ",MIs];];
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Current IBP number: ",Length[nIBPs]];];
	
	If[ExportCompleteRawIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/complete_raw_IBPs/"<>ToString[secNo]<>".txt",
			rawIBPs//InputForm//ToString
		];
	];
	If[ExportCompleteRawIBPsInFI0,
		ConvenientExport[
			outputPath<>"results/additional_outputs/complete_raw_IBPs_in_FI0/"<>ToString[secNo]<>".txt",
			rawIBPs/.FI[x_]:>FI0[remainedFIBPlabels[[x]]]//InputForm//ToString
		];(*Safe with FI00*)
	];
	If[ExportCompleteNIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/complete_nIBPs/"<>ToString[secNo]<>".txt",
			nIBPs//InputForm//ToString
		];
	];
	If[ExportCompleteNIBPsCutted,
		ConvenientExport[
			outputPath<>"results/additional_outputs/complete_nIBPs_cutted/"<>ToString[secNo]<>".txt",
			nIBPsCutted//InputForm//ToString
		];
	];
	
	If[SubsetQ[MIs,LocalTargets],
		(* No need to generate IBPs for this sector *)
		Global`MIList[sector]=MIs;
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  All target integrals are MIs, skipping the rest steps. "];];
		Return[];
	];
	
	(*|||||||||*)(*ProbeIntermediateResult["nIBPs_original",secNo,nIBPs];*)
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Removing subsector IBPs..."]];
	IBPIndex=Select[Range[Length[nIBPs]],CollectG[nIBPsCutted[[#]]]=!=0&];(*CollectG may be slow... and seems to be unnecessary*)
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[nIBPs]-Length[IBPIndex]," IBPs removed, ",Length[IBPIndex]," IBPs remained."]];
	rawIBPs=rawIBPs[[IBPIndex]];
	nIBPs=nIBPs[[IBPIndex]];
	nIBPsCutted=nIBPsCutted[[IBPIndex]];
	(*|||||||||*)(*ProbeIntermediateResult["nIBPs_subsecIBPsRemoved",secNo,nIBPs];*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Subsector IBPs removed. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	If[ExportSubsecRemovedRawIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/subsec_removed_raw_IBPs/"<>ToString[secNo]<>".txt",
			rawIBPs//InputForm//ToString
		];
	];
	If[ExportSubsecRemovedRawIBPsInFI0,
		ConvenientExport[
			outputPath<>"results/additional_outputs/subsec_removed_raw_IBPs_in_FI0/"<>ToString[secNo]<>".txt",
			rawIBPs/.FI[x_]:>FI0[remainedFIBPlabels[[x]]]//InputForm//ToString
		];
		(*Safe with FI00*)
	];
	If[ExportSubsecRemovedNIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/subsec_removed_nIBPs/"<>ToString[secNo]<>".txt",
			nIBPs//InputForm//ToString
		];
	];
	If[ExportSubsecRemovedNIBPsCutted,
		ConvenientExport[
			outputPath<>"results/additional_outputs/subsec_removed_nIBPs_cutted/"<>ToString[secNo]<>".txt",
			nIBPsCutted//InputForm//ToString
		];
	];
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Sorting nFIBPs..."]];
	(* Sort the IBPs and find the independent Ones*)
	(* degRep=Dispatch[#->rr^Total[IntegralAbsDegree[#]]&/@IntegralList[nIBPs]];
	IBPDegreeList=Exponent[#,rr]&/@(nIBPs/.degRep); *)
	(*ProbeIntermediateResult["nIBPs_rawIBPs",secNo,{nIBPs,rawIBPs}];*)
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Determining CompensationIBPDenominatorDegrees..."]];(*CompensationIBP: IBP from seeding with denominator lifted. If not a CompensationIBP,  this value is -1 *)
	timer2=AbsoluteTime[];
	memoryUsed2=MaxMemoryUsed[
	CompensationIBPDenominatorDegrees=Table[-1,Length[rawIBPs]];
	For[i=1,i<=Length[rawIBPs],i++,
		If[And[
			FreeQ[rawIBPs[[i]],FI0],
			FreeQ[rawIBPs[[i]],ZM0],
			FreeQ[rawIBPs[[i]],FI00],
			FreeQ[rawIBPs[[i]],ZM00]
		],
			Continue[];
		];
		CompensationIBPDenominatorDegrees[[i]]=IBPSubSectorDenominatorDegree[nIBPs[[i]]]
	];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Calculating IBPDegreeList..."]];
	
	timer2=AbsoluteTime[];
	memoryUsed2=MaxMemoryUsed[
	IBPDegreeList=IBPSubSectorDegree[#,sector]&/@nIBPs;
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  Calculating leafCounts..."]];
	timer2=AbsoluteTime[];
	memoryUsed2=MaxMemoryUsed[
	leafCounts=LeafCount/@nIBPs;
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  Calculating byteCounts..."]];
	timer2=AbsoluteTime[];
	memoryUsed2=MaxMemoryUsed[
	byteCounts=ByteCount/@nIBPs;
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  Sorting..."]];
	
	timer2=AbsoluteTime[];
	memoryUsed2=MaxMemoryUsed[
	IBPIndex=SortBy[Range[Length[nIBPs]],{CompensationIBPDenominatorDegrees[[#]],IBPDegreeList[[#]],leafCounts[[#]],byteCounts[[#]]}&];
	rawIBPs=rawIBPs[[IBPIndex]];
	nIBPs=nIBPs[[IBPIndex]];
	nIBPsCutted=nIBPsCutted[[IBPIndex]];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	(*|||||||||*)(*ProbeIntermediateResult["nIBPs_sorted",secNo,nIBPs];*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  nFIBPs sorted. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	If[ExportSortedRawIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/sorted_raw_IBPs/"<>ToString[secNo]<>".txt",
			rawIBPs//InputForm//ToString
		];
	];
	If[ExportSortedRawIBPsInFI0,
		ConvenientExport[
			outputPath<>"results/additional_outputs/sorted_raw_IBPs_in_FI0/"<>ToString[secNo]<>".txt",
			rawIBPs/.FI[x_]:>FI0[remainedFIBPlabels[[x]]]//InputForm//ToString
		];
		(*Safe with FI00*)
	];
	If[ExportSortedNIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/sorted_nIBPs/"<>ToString[secNo]<>".txt",
			nIBPs//InputForm//ToString
		];
	];
	If[ExportSortedNIBPsCutted,
		ConvenientExport[
			outputPath<>"results/additional_outputs/sorted_nIBPs_cutted/"<>ToString[secNo]<>".txt",
			nIBPsCutted//InputForm//ToString
		];
	];
	
	
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Selecting independent IBPs..."]];
	

	
	IBPIndex=IndepedentSet[nIBPsCutted,SectorIntegrals];
	rawIBPs=rawIBPs[[IBPIndex]];
	
	(*I put the following integral realization codes after oerlikon algorithm*)
	(*rawIBPs=rawIBPs/.FI[i_]:>FIBPs[[i]]/.IntegralR->IntegralRealization;   (* Only at this step, we obtain the analytic IBPs *)(*Why not /.sectorCut?*)
	If[Not[NeedSymmetry===False],
		rawIBPs=rawIBPs/.ZM[i_]:>(zMaps[[i]])/.SelfSymmetryR->SelfSymmetryRealization;
	];*)
	
	nIBPs=nIBPs[[IBPIndex]];
	nIBPsCutted=nIBPsCutted[[IBPIndex]];
	
	(*|||||||||*)(*ProbeIntermediateResult["nIBPs_independent",secNo,nIBPs];*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Independent IBPs selected. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[rawIBPs]," IBPs are selected with ",Length[IntegralList[nIBPs/.SectorCut[sector],SortTheIntegrals->False]]," integrals in current sector."]];
	
	(*ProbeIntermediateResult["nIBPs2",secNo,nIBPs];*)
	
	
	If[ExportIndependentRawIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/independent_raw_IBPs/"<>ToString[secNo]<>".txt",
			rawIBPs//InputForm//ToString
		];
	];
	If[ExportIndependentRawIBPsInFI0,
		ConvenientExport[
			outputPath<>"results/additional_outputs/independent_raw_IBPs_in_FI0/"<>ToString[secNo]<>".txt",
			rawIBPs/.FI[x_]:>FI0[remainedFIBPlabels[[x]]]//InputForm//ToString
		];
		(*Safe with FI00*)
	];
	If[ExportIndependentNIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/independent_nIBPs/"<>ToString[secNo]<>".txt",
			nIBPs//InputForm//ToString
		];
	];
	If[ExportIndependentNIBPsCutted,
		ConvenientExport[
			outputPath<>"results/additional_outputs/independent_nIBPs_cutted/"<>ToString[secNo]<>".txt",
			nIBPsCutted//InputForm//ToString
		];
	];
	
	(* Remove the unneeded IBP , Oerlikon algorithm *)
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Removing the unneeded IBPs..."]];
	
	(*SectorIntegrals=IntegralList[nIBPs];*)
	ReducedIntegrals=Complement[LocalTargets,MIs];
	
	
	UsedIndex=UsedRelations[nIBPsCutted,ReducedIntegrals,MIs];
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ",Length[nIBPs]-Length[UsedIndex]," IBP relations are removed. ", Length[UsedIndex]," left."];];
	
	
	rawIBPs=rawIBPs[[UsedIndex]];
	nIBPs=nIBPs[[UsedIndex]];(*although not needed, we keep this to debug*)
	nIBPsCutted=nIBPsCutted[[UsedIndex]];
	
	(*ProbeIntermediateResult["rawIBPsFrom",secNo,rawIBPs[[All,1]]//Union];(*debug2023*)*)
	(*we say this is the final rawIBP in the sence of concept. 
	The same symbol is used for another concept afterwards in this function*)
	If[ExportFinalRawIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/final_raw_IBPs/"<>ToString[secNo]<>".txt",
			rawIBPs//InputForm//ToString
		];
	];
	
	If[ExportFinalRawIBPsInFI0,
		ConvenientExport[
			outputPath<>"results/additional_outputs/final_raw_IBPs_in_FI0/"<>ToString[secNo]<>".txt",
			rawIBPs/.FI[x_]:>FI0[remainedFIBPlabels[[x]]]//InputForm//ToString
		];
		(*Safe with FI00*)
	];
	If[ExportFinalNIBPs,
		ConvenientExport[
			outputPath<>"results/additional_outputs/final_nIBPs/"<>ToString[secNo]<>".txt",
			nIBPs//InputForm//ToString
		];
	];
	If[ExportFinalNIBPsCutted,
		ConvenientExport[
			outputPath<>"results/additional_outputs/final_nIBPs_cutted/"<>ToString[secNo]<>".txt",
			nIBPsCutted//InputForm//ToString
		];
	];
	If[ExportUsedSyzygyVectors===True,
		usedVectors=Union[
			Select[
				rawIBPs[[All,1]]/.FI[x_]:>FI0[remainedFIBPlabels[[x]]],
				Or[Head[#]===FI0,Head[#]===FI00]&
			]
		]/.FI0[x_]:>VectorList[[x]];
		If[FIBPs00=!=None,
			usedVectors=usedVectors/.FI00[x_]:>VectorList00[[x]]
		];
		ConvenientExport[outputPath<>"results/additional_outputs/used_syzygy_vectors/"<>ToString[secNo]<>".txt",usedVectors//InputForm//ToString];
	];
	If[ExportUsedFIBPs===True,
		usedFIBPs=Union[
			Select[
				rawIBPs[[All,1]],
				Head[#]===FI||Head[#]===FI0||Head===FI00&
			]
		];
		If[MemberQ[Head/@usedFIBPs,FI0],
			usedFIBPs=Union[usedFIBPs/.FI[x_]:>FI0[remainedFIBPlabels[[x]]]]
		];
		usedFIBPs=usedFIBPs/.{FI0[x_]:>FIBPs0[[x]],FI[x_]:>FIBPs[[x]]};
		(*well, we assume if FI0 appears, FIBPs0 must be defined, if not , we cannot code so here*)
		If[FIBPs00=!=None,
			usedFIBPs=usedFIBPs/.FI00[x_]:>FIBPs00[[x]]
		];
		ConvenientExport[outputPath<>"results/additional_outputs/used_FIBPs/"<>ToString[secNo]<>".txt",usedFIBPs//InputForm//ToString];
	];

	
	
	(*|||||||||*)(*ProbeIntermediateResult["nIBPs_used",secNo,nIBPs];*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Unneeded IBPs removed. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[rawIBPs]," IBPs remaining with ",Length[IntegralList[nIBPs/.SectorCut[sector],SortTheIntegrals->False]]," integrals in current sector."]];
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Realizing raw IBPs..."]];
	rawIBPs=rawIBPs/.FI0[i_]:>FIBPs0[[i]]/.FI[i_]:>FIBPs[[i]];
	If[FIBPs00=!=None,rawIBPs=rawIBPs/.FI00[i_]:>FIBPs00[[i]]];
	rawIBPs=rawIBPs/.IntegralR->IntegralRealization;   (* Only at this step, we obtain the analytic IBPs *)
	If[Not[NeedSymmetry===False],
		rawIBPs=rawIBPs/.ZM00->ZM/.ZM0->ZM/.ZM[i_]:>(zMaps[[i]])/.SelfSymmetryR->SelfSymmetryRealization;
	];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Raw IBP realized. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	
	
	(*	 Mapping integrals *)
	(*
		we map the integrals here because:
		1. It is easy
		2. We do not care about symmetries between sectors in the steps above, 
			since the symmetries only concern sub sectors of current sector, and the steps above don`t care about these subsectors
	*)
	
	If[Not[NeedSymmetry===False],
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		sectorMaps=OptionValue[SectorMappingRules];
		mappedSectors=sectorMaps[[All,1]];
		(*PrintAndLog["#",secNo,"\t",Length[mappedSectors]];*)
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Performing sector maps..."]];
		If[mappedSectors=!={},
			(*rawIBPs=rawIBPs/.G[x__]:>GMapped[sectorMaps,{x}];*)
			(*rawIBPs=SymmetryMap[sectorMaps,#]&/@rawIBPs;*)
			rawIBPs=SymmetryMap[sectorMaps,rawIBPs];
		];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Sector mapping finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
		
	];
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	(*	 Remove zero-sector integrals *)
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Removing zero-sector integrals..."]];
	timer2=AbsoluteTime[];
	memoryUsed2=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Deriving integral list..."]];
    integrals=Select[IntegralList[rawIBPs,SortTheIntegrals->False],!MemberQ[Global`ZeroSectors,Sector[#]]&];
    (*end of MaxMemoryUsed*)];
    If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	timer2=AbsoluteTime[];
	memoryUsed2=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  removing zero integrals..."]];
	(*rawIBPs=IBPCoefficientForm[CoefficientArrays[rawIBPs,integrals][[2]]] . integrals; *)
	(*Do I need to factor these coefficients?...OH yes, very important
	(2023.11.08)well, but, Factor is really too slow for some hard problems,
	I would like to make it an option, defaultly Expand
	2024.05.31: Expand does not work on SparseArray.............. 
	Thus, I modified the above line by CollectG, as follows:
	*)
	rawIBPs=CollectG[#,CoefficientForm->IBPCoefficientForm,RelevantGs->integrals]&/@rawIBPs;
	(*must specify relevant Gs here to dezerosectorintegrals*)
	 (*end of MaxMemoryUsed*)];
    If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  Finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	
	
	(*end of MaxMemoryUsed*)];
	
	
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Zero-sector integrals removed. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[rawIBPs]," IBPs remaining with ",Length[IntegralList[rawIBPs/.SectorCut[sector],SortTheIntegrals->False]]," integrals in current sector."]];
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Saving results in current sector..."]];
	
	If[!OptionValue[TestOnly],
		Global`IBPList[sector]=rawIBPs;
		Global`MIList[sector]=MIs;
		Global`RelavantIntegrals[sector]=integrals;
		SubsectorInts=Select[IntegralList[rawIBPs],Sector[#]!=sector&];
		tailSectors=DeleteDuplicates[Sector/@SubsectorInts];
		For[i=1,i<=Length[tailSectors],i++,
			subsector=tailSectors[[i]];
			Global`ReductionTasks[subsector]=Union[Global`ReductionTasks[subsector],Select[SubsectorInts,Sector[#]==subsector&]];
		];
		
	];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Results saved for current sector. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
];





























































(* ::Subsection:: *)
(*Row Reduce Modules*)


(*NonzeroEntriesString[matrix_]:=ToString[InputForm[N[Round[1000*((Length[ArrayRules[matrix]]-1)/(Times@@Dimensions[matrix]))]*0.1]]]<>"%"*)



Options[IBPAnalyze]:={Modulus->FiniteFieldModulus};
IBPAnalyze[IBPs_,Ints_,OptionsPattern[]]:=Module[{M,RM,redIndex,irredIndex,timer,memoryUsed},
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	(*ProbeIntermediateResult["M_IBPAnalyze",secNum,M];*)
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  Deriving coefficient matrix in IBPAnalyze. "]];
	M=CoefficientArrays[IBPs,Ints][[2]];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  Deriving coefficient matrix in IBPAnalyze finished. "," Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	(*ProbeIntermediateResult["M_IBPAnalyze",secNum,M];*)
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  RowReduce in IBPAnalyze started. Matrix dimension: ",Dimensions[M]]];
	RM=RowReduceFunction[M,Modulus->OptionValue[Modulus]];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  RowReduce in IBPAnalyze finished. Matrix dimension: ",Dimensions[M],". Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
(*	timer=AbsoluteTime[];
	RM=FFRowReduce[M];
	If[TimingReportOfRowReduce===True,PrintAndLog["\t\t\tFFRowReduce in IBPAnalyze finished. Matrix dimension: ",Dimensions[M],". Time used: ",AbsoluteTime[]-timer," s."]];*)
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  Deriving pivots in IBPAnalyze..."]];
	redIndex=pivots[RM];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  Pivots derived in IBPAnalyze. Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	irredIndex=Complement[Range[Length[Ints]],redIndex];
	If[ReportIrreducibleIntegralsAfterIBPAnalyze,
		PrintAndLog["#",secNum,"\t\t",Length[irredIndex]," irreducible integrals: ",Ints[[irredIndex]]]
	];
	Return[{redIndex,irredIndex,RM . Ints}];
];


Options[IndepedentSet]:={Modulus->FiniteFieldModulus};
IndepedentSet[IBPs_,Ints_,OptionsPattern[]]:=Module[{M,RM,redIndex,indepIndex,timer,memoryUsed},
	
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	(*ProbeIntermediateResult["M_IBPAnalyze",secNum,M];*)
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  Deriving coefficient matrix in IndepedentSet. "]];
	M=CoefficientArrays[IBPs,Ints][[2]];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  Deriving coefficient matrix in IndepedentSet finished. "," Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  RowReduce in IndepedentSet started. Matrix dimension: ",Dimensions[M//Transpose]]];
	
	RM=RowReduceFunction[M//Transpose,Modulus->OptionValue[Modulus]];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  RowReduce in IndepedentSet finished. Matrix dimension: ",Dimensions[M//Transpose],". Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
(*	timer=AbsoluteTime[];
	RM=FFRowReduce[M//Transpose];
	If[TimingReportOfRowReduce===True,PrintAndLog["\t\t\tFFRowReduce in IndepedentSet finished. Matrix dimension: ",Dimensions[M//Transpose],". Time used: ",AbsoluteTime[]-timer," s."]];*)
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  Deriving pivots in IndepedentSet..."]];
	indepIndex=pivots[RM];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  Pivots derived in IndepedentSet. Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	Return[indepIndex];
];
If[UseSRFindPivots===True,ClearAll[IndepedentSet];
Options[IndepedentSet]:={Modulus->FiniteFieldModulus};
IndepedentSet[IBPs_,Ints_,OptionsPattern[]]:=Module[{M,redIndex,indepIndex,timer,memoryUsed},
	M=CoefficientArrays[IBPs,Ints][[2]];
	(*ProbeIntermediateResult["M_IndepedentSet",secNum,M];(*debug20231227*)*)
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  RREF-pivots finding in IndepedentSet started. Matrix dimension: ",Dimensions[M//Transpose]]];
	indepIndex=SRFindPivots[M//Transpose,Modulus->OptionValue[Modulus]];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  RREF-pivots finding in IndepedentSet finished. Matrix dimension: ",Dimensions[M//Transpose],". Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	Return[indepIndex];
];
(*End of if*)]


SparseIdentityMatrix[n_]:=SparseArray[Table[{k,k}->1,{k,n}]]


Options[UsedRelations]:={Modulus->FiniteFieldModulus};
UsedRelations[IBPs_,ReducedIntegrals_,MIs_,OptionsPattern[]]:=Module[{Ints,M,Mext,ReducedIntegralColumns,RM,i,j,columnIndex,rowIndex,MatrixL,tempList,result,timer,memoryUsed},
	
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	(*ProbeIntermediateResult["M_IBPAnalyze",secNum,M];*)
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  Deriving coefficient matrix and integral list in UsedRelations. "]];
	Ints=IntegralList[IBPs];
	M=CoefficientArrays[IBPs,Ints][[2]];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  Deriving coefficient matrix and integral list in UsedRelations finished. ","Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	
	ReducedIntegralColumns=Flatten[Position[Ints,#]&/@ReducedIntegrals];
	tempList=Table[Null,{i,1,Length[ReducedIntegralColumns]}];
	(*Mext=Join[Transpose[M],IdentityMatrix[M//Length]]//Transpose;*)
	Mext=Join[Transpose[M],SparseIdentityMatrix[M//Length]]//Transpose;
	
	(*Export[Global`workingPath<>"UR.txt",Mext//InputForm//ToString];*)
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  RowReduce in UsedRelations started. Matrix dimension: ",Dimensions[Mext]]];
	RM=RowReduceFunction[Mext,Modulus->OptionValue[Modulus]];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  RowReduce in UsedRelations finished. Matrix dimension: ",Dimensions[Mext],". Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	(*timer=AbsoluteTime[];
	RM=FFRowReduce[Mext];
	If[TimingReportOfRowReduce===True,PrintAndLog["\t\t\tFFRowReduce in UsedRelations finished. Matrix dimension: ",Dimensions[Mext],". Time used: ",AbsoluteTime[]-timer," s."]];*)
	MatrixL=RM[[All,Length[Ints]+1;;]];
	
	(* To find the corresponding rows for the ReducedIntegrals *)
	For[i=1,i<=Length[ReducedIntegralColumns],i++,
			columnIndex=ReducedIntegralColumns[[i]];
			rowIndex=ArrayRules[RM[[All,columnIndex]]][[1,1,1]];
			tempList[[i]]=Complement[Keys[ArrayRules[MatrixL[[rowIndex]]]]//Flatten,{_}];
	];  
	
	result=Flatten[tempList]//Union//Sort;
	
	Return[result];
	

];


(* ::Subsection::Closed:: *)
(*not used functions*)


(*Options[IBPtest]:={Modulus->FiniteFieldModulus};
IBPtest[IBPs_,sector_,OptionsPattern[]]:=Module[{M,RM,Ints,redIndex,irredIndex,timer,memoryUsed},
	Ints=Select[IntegralList[IBPs],Sector[#]==sector&]//IntegralList;
	M=CoefficientArrays[IBPs/.GenericPoint/.GenericD,Ints][[2]];
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  RowReduce in IBPtest started. Matrix dimension: ",Dimensions[M]]];
	RM=RowReduceFunction[M,Modulus->OptionValue[Modulus]];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  RowReduce in IBPtest finished. Matrix dimension: ",Dimensions[M],". Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
(*	timer=AbsoluteTime[];
	RM=FFRowReduce[M];
	If[TimingReportOfRowReduce===True,PrintAndLog["\t\t\tFFRowReduce in IBPtest finished. Matrix dimension: ",Dimensions[M],". Time used: ",AbsoluteTime[]-timer," s."]];*)
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  Deriving pivots in IBPtest..."]];
	redIndex=pivots[RM];
	(*end of MaxMemoryUsed*)];
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t\t  Pivots derived in IBPtest. Time used: ",Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	irredIndex=Complement[Range[Length[Ints]],redIndex];
	PrintAndLog["Test: reduced integrals ",Ints[[redIndex]]];
];*)





(* ::Section::Closed:: *)
(*FFRowReduce*)


(*FFRowReduce[matrix_]:=Module[{dimensions,c,coefficients,equations,solution,reducedEquations,ca,ar},
	dimensions=Dimensions[matrix];
	coefficients=c/@Range[dimensions[[2]]];
	equations=DeleteCases[matrix.coefficients,0];
	solution=FFSparseSolve[#==0&/@equations,coefficients,"MaxPrimes"->400];
	reducedEquations=#[[1]]-#[[2]]&/@SortBy[solution,#[[1]]&];
	ca=CoefficientArrays[reducedEquations,coefficients][[2]];
	ar=Delete[ArrayRules[ca],-1];
	Return[SparseArray[ar,dimensions]];
]*)
