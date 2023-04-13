(* ::Package:: *)

TimingReportOfRowReduce=True;
LogFile="";
probeTheFunctions=False;
RowReduceFunction=SRSparseRowReduce
debugModification20230314=True;



ProbeIntermediateResult[name_,sec_,expr_]:=Module[{intermediateResultFolder},
	intermediateResultFolder=outputPath<>"tmp/intermediate_results/"<>name<>"/";
	If[!DirectoryQ[#],Run["mkdir -p "<>#]]&[intermediateResultFolder];
	Export[intermediateResultFolder<>ToString[sec]<>".txt",expr//InputForm//ToString]
]


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





(* ::Section::Closed:: *)
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
	
	
	If[Length[Propagators]<SDim,Return[]];
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
	
	(* z' s = MatrixA.ScalarVar + Vectorb, conversion between s_ij (x_ij here) to Baikov z's *)
	
	BaikovRevRep=MapThread[#1->#2&,{var,MatrixA.ScalarVar+Vectorb}];
	BaikovRep=MapThread[#1->#2&,{ScalarVar,Inverse[MatrixA].(var-Vectorb)}];
	BaikovKernel=BaikovKernelScalar/.BaikovRep;
	
	Parameters=Select[Variables[BaikovMatrix/.BaikovRep],!Head[#]===z&];
	TangentSet=MatrixA.#&/@ScalarTangentSet/.BaikovRep//Factor;
	ExtendedTangentSet=Transpose[Join[Transpose[TangentSet],{Transpose[ScalarExtendedTangentSet]//Last}]]; (* including the cofactors *)
	
	ForwardRep[1]=Join[Table[z[i]->ToExpression["z"<>ToString[i]],{i,1,SDim}],Table[Parameters[[i]]->ToExpression["c"<>ToString[i]],{i,1,Parameters//Length}]];
	BackwardRep[1]=Reverse/@ForwardRep[1];
	ForwardRep[2]=Join[Table[ScalarVar[[i]]->ToExpression["z"<>ToString[i]],{i,1,SDim}],Table[Parameters[[i]]->ToExpression["c"<>ToString[i]],{i,1,Parameters//Length}]];
	BackwardRep[2]=Reverse/@ForwardRep[2];

	Scalar2sp=#[[2]]->(#[[1]]/.{x_ y_->sp[x,y],x_^2->sp[x,x]})&/@ScalarVarRep; 
	sp2Scalar=Join[Reverse/@Scalar2sp,
		Table[sp[ExternalMomenta[[i]],ExternalMomenta[[j]] ]->(ExternalMomenta[[i]]*ExternalMomenta[[j]]/.Kinematics),{i,1,Length[ExternalMomenta]},{j,1,Length[ExternalMomenta]}]//Flatten
		];
	
	PropagatorScalar=Expand[Propagators]/.ScalarVarRep/.Kinematics;
	
	(* Feynman representation  *)
	
	Formula=(Propagators.var)/.(#->lambda #&/@LoopMomenta);
	MatrixA=D[SeriesCoefficient[Formula,{lambda,0,2}],{LoopMomenta},{LoopMomenta}]/2;	
	VectorB=Coefficient[SeriesCoefficient[Formula,{lambda,0,1}],LoopMomenta]/2;
	ScalarC=SeriesCoefficient[Formula,{lambda,0,0}];
	Global`PolynomialU=Det[MatrixA]//Factor;
	Global`PolynomialF=-PolynomialU (Expand[ScalarC]/.Kinematics)+ Cancel[PolynomialU (Expand[VectorB.Inverse[MatrixA].VectorB]/.Kinematics)]//Expand;
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
	Return[{Modules[[1]].(gen/@Range[SDim]),Modules[[3]].(gen/@Range[SDim])}];

]



(*Print["If you see an error message saying cannot open LinearSyzForLinearModule_FF_v2.wl, you can ignore it for the current version:"]
Get["/home/zihao/projects/SyzygyRed/LinearSyz/LinearSyzForLinearModule_FF_v2.wl"]*)


(* ::Section:: *)
(*Cut*)


SingularIdeal[propIndex_,cutIndex_]:=Module[{cut,FF1,SingularIdeal},
	 cut=z[#]->0&/@cutIndex;
	 FF1=Global`BaikovKernel/.cut;
	 SingularIdeal=Join[D[FF1,{Global`var}],{FF1}];
	 Return[SingularIdeal];
];


(* ::Section::Closed:: *)
(* Sector Tools*)


BlockMatrix[m_,n_]:=Module[{matrix},
	matrix=SparseArray[{},{m+n,m+n}]//Normal;
	Do[matrix[[1,j]]=1,{j,1,m}];
	Do[matrix[[j+1,j]]=1,{j,1,m-1}];
	Do[matrix[[m+1,j+m]]=1,{j,1,n}];
	Do[matrix[[j+m+1,j+m]]=1,{j,1,n-1}];
	Return[matrix];
];(*block degreed lexicographic*)


Sector[int_]:=Table[If[int[[j]]>0,1,0],{j,1,Length[int]}]
SectorIndex[int_]:=Table[If[int[[j]]>0,j,Nothing],{j,1,Length[int]}]
Index2Sector[propIndex_]:=Exponent[Times@@(z/@propIndex),var];


SectorHeight[int_]:=Count[int/. G->List,u_/;u>0]


SectorCut[sector_]:=G[x__]:>(If[Sector[G[x]]==sector,1,0])*G[x];


SectorCutQ[sec_,cut_]:=And@@(#>=0&/@(sec-cut));

SectorNumber[sec_]:=FromDigits[sec//Reverse,2];
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

];


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





(* ::Section:: *)
(*Integral Ordering*)


IntegralWeight[int_]:=SectorWeightMatrix[int//Sector].IntegralAbsDegree[int];
IntegralCut[cut_]:=G[x__]:>If[SectorCutQ[Sector[G[x]],cut],G[x],0];
IntegralDegree[int_]:=(int/.G->List)-Sector[int];
IntegralAbsDegree[int_]:=Abs/@((int/.G->List)-Sector[int]);
IntegralSectorHeight[int_]:=Count[int/.G->List,u_/;u>0];
IntegralSectorOrder[int_]:=SectorWeightMatrix1[Sector[int]].IntegralAbsDegree[int];
(*IntegralReal[indices_]:=Dispatch[Table[m[i]->indices[[i]],{i,1,SDim}]];*)
IntegralPropagatorDegree[int_]:=Sum[If[int[[j]]>1,int[[j]]-1,0],{j,1,Length[int]}]; 
IntegralISPDegree[int_]:=-Total[Select[int/.G->List,#<0&]];
IntegralPropagatorType[int_]:=Table[If[int[[j]]>=1,int[[j]],0],{j,1,Length[int]}]; 
(*FIntegralSectorISPDegree[fInt_,sector_]:=((fInt/.m[_]->0)/.G->List).(sector-1)*)
FIntegralSectorISPDegree[fInt_,sector_]:=(IntegralRealization[fInt,Table[0,SDim]]/.G->List).(sector-1)



IntegralOrdering[int_]:=Join[{IntegralSectorHeight[int],SectorNumber[int//Sector]},IntegralWeight[int]];   (* Key function *)


(*IntegralList[IBP_]:=Select[Variables[IBP],Head[#]==G&];*)

IBPWeight[IBP_]:=Max[Total[IntegralAbsDegree[#]]&/@IntegralList[IBP,SortTheIntegrals->False]];


CollectG[exp_]:=Coefficient[exp,Select[Variables[exp],Head[#]==G&]].Select[Variables[exp],Head[#]==G&];


(* ::Section:: *)
(*Singular Interface*)


ModuleSlash[m_]:=Table[If[Union[m[[j]]]==={0},Nothing,m[[j]]],{j,1,Length[m]}];


Vector2gen[vec_,mode_]:=vec.Table[gen[j],{j,1,Length[vec]}]/.ForwardRep[mode];
Vector2SingularForm[vec_,mode_]:=StringReplace[ToString[InputForm[Vector2gen[vec,mode]]],{"gen["~~Shortest[x__]~~"]":>"gen("<>x<>")","{"->"[","}"->"]"}];
Module2SingularForm[m_,mode_]:=StringReplace[ToString[Vector2SingularForm[#,mode]&/@m],{"{"->"","}"->""}];


SingularIntersectionText="LIB \"matrix.lib\";
ring R = (MODULUS), (VAR, PARAMETERS), (dp(LEN1),dp(LEN2));
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
ring R2 = (MODULUS,PARAMETERS), (VAR), dp(LEN1);
module m12=imap(R,m12old);
module mInt=simplify(m12,SimplificationStrategy);
write(\"OUTPUTFILE\",string(mInt));
exit;
";


SingularSyzText="LIB \"matrix.lib\";
ring R = (MODULUS), (VAR, PARAMETERS), (dp(LEN1),dp(LEN2));
option(prot);
degBound=DEGBOUND;
module m1 = MODULE1;
module a=syz(m1);
ring R2 = (MODULUS,PARAMETERS), (VAR), dp(LEN1);
module a2=imap(R,a);
module a3=simplify(a2,SimplificationStrategy);
write(\"OUTPUTFILE\",string(a3));
exit;
";


Options[SingularIntersectionMaker]={Modulus->0,SimplificationRules->Global`OptionSimplification,ScriptFile->TemporaryDirectory<>"intersection.sing",
OutputFile->TemporaryDirectory<>"intersection_result.txt",ScriptOnly->False,degBound->0
};
SingularIntersectionMaker[M1input_,M1extinput_,M2input_,varRedundant_,parameterRedundant_,OptionsPattern[]]:=Module[{M1,M1ext,M2,SingularScript,forwardRep,backwardRep,var,parameters,varpara,len1,len2,
varString,parameterString},
	
	
	If[FileExistsQ[OptionValue[OutputFile]],DeleteFile[OptionValue[OutputFile]]];
	M1=ModuleSlash[M1input];
	M1ext=ModuleSlash[M1extinput];
	M2=ModuleSlash[M2input];
	varpara=Variables[Join[M1ext,M2]];
	var=ListIntersect[varRedundant,varpara];  (* Delete the variables not in the modules *)
	parameters=ListIntersect[parameterRedundant,varpara];   (* Delete the parameters not in the modules *)
	If[parameters=={},parameters=parameterRedundant[[1]]];   (*  If there is no parameter, to fit in the Singular code, pick up one parameter *)
	
	varString=StringReplace[ToString[var/.ForwardRep[1]],{"{"->"","}"->""}];
	parameterString=StringReplace[ToString[parameters/.ForwardRep[1]],{"{"->"","}"->""}];
	
	SingularScript=StringReplace[SingularIntersectionText,{"VAR"->varString,"PARAMETERS"->parameterString}];
	SingularScript=StringReplace[SingularScript,{"MODULUS"->ToString[Modulus//OptionValue],"LEN1"->ToString[Length[var]],"LEN2"->ToString[Length[parameters]]}];
	SingularScript=StringReplace[SingularScript,{"MODULE1"->Module2SingularForm[M1,1],"M1ext"->Module2SingularForm[M1ext,1],"MODULE2"->Module2SingularForm[M2,1],"M1SIZE"->ToString[Length[M1]]}];
	SingularScript=StringReplace[SingularScript,"OUTPUTFILE"->OptionValue[OutputFile]];
	SingularScript=StringReplace[SingularScript,{"SimplificationStrategy"->ToString[OptionValue[SimplificationRules]],"DEGBOUND"->ToString[OptionValue[degBound]]}];
	PrintAndLog[OptionValue[ScriptFile]];
	Export[OptionValue[ScriptFile],SingularScript,"Text"];
	If[OptionValue[ScriptOnly], Return[]];
];


Options[IntersectionRead]=Options[SingularIntersectionMaker];
IntersectionRead[OptionsPattern[]]:=Module[{dim,string,vectors},
	dim=Global`SDim+1;
	If[!FileExistsQ[OptionValue[OutputFile]],Return[]];
	string=StringReplace["{"<>Import[OptionValue[OutputFile]]<>"}","gen("~~Shortest[x__]~~")":>"gen["~~x~~"]"];
	vectors=ToExpression[string];
	
	(*yan-er-dao-ling!*)
	If[Count[vectors,0]>0,PrintAndLog["#",secNum,"  ZEROs in VectorList Encountored!!They are deleted."];vectors=DeleteCases[vectors,0]];
	
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
NumericMode->False};
SingularIntersection[resIndex_,OptionsPattern[]]:=Module[{M1,M1ext,M2,SingularCommand,timer,vectors,cutIndex},
	cutIndex=OptionValue[Cut];
	If[!SubsetQ[resIndex,cutIndex],PrintAndLog["Sorry... This version does not support the case with a cut propagator index UNrestricted ..."]; Return[];];
	{M1,M1ext,M2}=TangentModules[resIndex,cutIndex];
	If[OptionValue[NumericMode]===True,
		M1=M1/.GenericPoint/.GenericD;
		M1ext=M1ext/.GenericPoint/.GenericD;
		M2=M2/.GenericPoint/.GenericD;
	];
	If[OptionValue[ShortenTheModules]===True,
		{M1,M2}=ShortenedModule[M1ext,resIndex];
		If[M2==={},Return[M1ext]](*No restrictions on any of the indices*)
	];
	If[OptionValue[TestOnly],PrintAndLog[resIndex];Return[{M1,M2}]];
	varOrder=Join[Complement[var,Variables[M2]]//Sort,Intersection[var,Variables[M2]]//Sort];
	(* PrintAndLog[varOrder]; *)
	SingularIntersectionMaker[M1,M1ext,M2,varOrder,Parameters,ScriptFile->OptionValue[ScriptFile],OutputFile->OptionValue[OutputFile],Modulus->OptionValue[Modulus],SimplificationRules->OptionValue[SimplificationRules],degBound->OptionValue[degBound]];
	SingularCommand=SingularApp<>" "<>OptionValue[ScriptFile];
	timer=AbsoluteTime[];
	Run[SingularCommand];
	PrintAndLog["Singular running time ... ",AbsoluteTime[]-timer];
	vectors=IntersectionRead[OutputFile->OptionValue[OutputFile]];
	Return[vectors];
];


(* ::Section::Closed:: *)
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
IBPSectorHeight[IBP_]:=Block[{LT},LT=LeadingIntegral[IBP]; If[LT==={},Return[-1],Return[SectorHeight[Int]]];];
IBPWeight[IBP_]:=Max[Total[IntegralAbsDegree[#]]&/@IntegralList[IBP]];
IBPSubSectorDegree[IBP_,sector_]:=Max[Total[IntegralAbsDegree[#]]&/@(Select[IntegralList[IBP,SortTheIntegrals->False],Sector[#]!=sector&])];
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
	momentumMaps=MomentumMap[LoopMomenta,ExternalMomenta,prop1,prop2,Kinematics,FullEMsConstrain->True];
	ZSymmetry/@momentumMaps
]
FindSymmetry[sec1_,sec2_]:=Module[{zs,G1,G2,zs1,zs2,zPerms,result},
	If[Union[Flatten[sec1]]=!=Union[{0,1}],Print["sector ",sec1," is an unexpected input."];Return[$Failed]];
	If[Union[Flatten[sec2]]=!=Union[{0,1}],Print["sector ",sec2," is an unexpected input."];Return[$Failed]];
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


SectorMaps[sectors_]:=Module[
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
		LaunchKernels[Min[$ProcessorCount,ThreadUsedLimit]];
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
				maps=FindSymmetry[newTestingSector,newUniqueSector];
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


MappedIntegral[zMap_,indices_]:=Module[{zs,sector,ispq,test,numeratorInds,denominatorInds,numerator,denominator,crn,crd,di,df,Gn},
	sector=Boole[#>0]&/@indices;
	ispq=1-sector;
	zs=Sort[zMap[[All,1]]];
	test=Product[zs[[i]]^-sector[[i]],{i,Length[zs]}]/.zMap//Expand;
	If[Length[MonomialList[test]]>1,PrintAndLog["MappedIntegral: sector ",sector," inconsistent with map ",zMap];Return[$Failed]];
	numeratorInds=indices.DiagonalMatrix[ispq];
	denominatorInds=indices.DiagonalMatrix[sector];
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


(*main map function*)
SymmetryMap[sectorMaps_,expr_]:=Module[{exprExpanded,newExprExpanded},
	exprExpanded=Expand[expr];
	(*
	Explain why we need such a While True:
	A mapped sector A can be mapped to unique sector B, but an integral in A, with some numerator, can be mapped as combination of integrals not only in B, but also in subsectors of B. 
	However, a subsector of B is not necessarilly a unique sector, it could be mapped to another unique sector C, so we need to map again.
	*)
	While[True,
		newExprExpanded=Expand[exprExpanded/.G[x__]:>GMapped[sectorMaps,{x}]];
		If[Expand[newExprExpanded-exprExpanded]===0,Return[newExprExpanded]];
		exprExpanded=newExprExpanded
	]
]


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


SelfSymmetryRealization[zMap_,indices_]:=Expand[MappedIntegral[zMap,indices]-(G@@indices)]


(* ::Section:: *)
(*Azuritino*)


(* ::Subsection:: *)
(*Critical point*)


QuotientRingSize[ideal1_,varList1_,OptionsPattern[]]:=Module[{ideal=ideal1,var=varList1,LTList,i,n,condition,ss,cclist},
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

Options[LeeCriticalPoints]={Modulus->42013};
LeeCriticalPoints[sector_,OptionsPattern[]]:=Module[{P,Indices,vlist,ideal,GB},
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


Options[AzuritinoMIFind]={degBound->0,AzuritinoGenericD->GenericD,MaxISPDegree->4,MinISPDegreeForAnalysis->1,Modulus->42013,CriticalPointCounting->False,
selfSymmetryZMaps->{}
};
AzuritinoMIFind[sector_,OptionsPattern[]]:=Module[{P,secNo,Indices,ISPIndices,ISPlen,timer=AbsoluteTime[],tt=AbsoluteTime[],vectorList,FIBPs,FIBPISPdegree,IBPFunctions
,Pool,IBPs,i,j,NewIBPs,IntList,M,MI,irreducibleInts,LeeCounting,MaxISPD=OptionValue[MaxISPDegree],sectorCut,
MinISPD=OptionValue[MinISPDegreeForAnalysis],pivotList,zMaps,newSelfSymmetries},
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
				MaxISPD=MinISPD+2;
				PrintAndLog["#",secNo,"\t\t","start to find MIs from IBPs with numerator degree  ...",MinISPD]		
			]
		];
		
		
		
		
		vectorList=SingularIntersection[Indices,
			Cut->Indices,
			degBound->OptionValue[degBound],
			NumericMode->True,
			ScriptFile->TemporaryDirectory<>"azuritino_intersection.sing",
			OutputFile->TemporaryDirectory<>"azuritino_intersection_result.txt"
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
			
			If[i>=MinISPD,
				IntList=IntegralList[IBPs];
				M=SRSparseRowReduce[CoefficientArrays[IBPs,IntList][[2]],Modulus->OptionValue[Modulus]];
				pivotList=pivots[M];
				
				irreducibleInts=IntList[[Complement[Range[Length[IntList]],pivotList]]];
				
				If[i==MinISPD,
					MI=irreducibleInts;
					,
					MI=Intersection[MI,irreducibleInts];
				];
				PrintAndLog["#",secNo,"\t\t","Azuritino: Step "<>ToString[i]<>" :        test IBPs reduced over the finite field ... ",AbsoluteTime[]-tt];
				
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


DenominatorTypeCompleting[DenominatorTypes_]:=Module[{maxes},
	maxes=Max/@Transpose[DenominatorTypes];
	maxes//DenominatorTypesFromMaxDenominatorDegrees

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


(* ::Subsection::Closed:: *)
(*FindReducedIntegrals*)


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






(* ::Subsection:: *)
(*SectorAnalyze (main)*)


Options[SectorAnalyze]:={SeedingMethod->"Zurich",Verbosity->0,AdditionalDegree->3,DirectInitialSteps->2,TestOnly->False,
ZurichInitialSteps->3,ModuleIntersectionMethod->"Singular",SectorMappingRules->{},Cut->{},KillCornerSubsecFIBPs->True
};
SectorAnalyze[sector_,OptionsPattern[]]:=Module[{secheight,secindex,VectorList,timer,FIBPs,numshifts,r,s,LocalTargets,DenominatorTypes,
i,sectorCut,FIBPs1,CornerIBP,baseIBP,propLocus,ISPLocus,BaikovCut,rawIBPs={},nIBPs={},MIs={},step,newIBPs,seeds,integrals,SectorIntegrals,redIndex,irredIndex,rIBPs,
nFIBPs,WellAddressedIntegrals,secNo,degRep,rr,IBPDegreeList,IBPIndex,ReducedIntegrals,UsedIndex,subsector,SubsectorInts,tempInts,
IBPISPdegrees,NewrawIBPs,NewnIBPs,timer2,M1,M1ext,M2,sectorMaps,mappedSectors,tailSectors,leafCounts,byteCounts,maxDenominatorIndices,formalIntegrals,
zs,zMaps,newNIBPs,FIBPCurrentSectorIntegrals,memoryUsed,memoryUsed2,nFIBPFunctions
},
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["--------------------------------------------------------------------\nInitializing new sector ",sector," ..."]];
	If[!MemberQ[Global`NonZeroSectors,sector],PrintAndLog["This is an irrelavant sector or zero sector."]; Return[]];
	
	
	
	
	LocalTargets=ReductionTasks[sector];
	PrintAndLog["Target integrals: ",LocalTargets//Length];
	If[LocalTargets=={},
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  No target integrals on this sector, skipping the rest steps. "];];
		Return[]
	];  (* Nothing to reduce *)
	
	(*sectorMaps=OptionValue[SectorMappingRules];
	mappedSectors=sectorMaps[[All,1]];*)
	(*If[MemberQ[mappedSectors,sector]&&(Not[NeedSymmetry===False]),
		PrintAndLog["This is an mapped sector."];
		Return[];
		rawIBPs=#-(#/.G[x__]:>GMapped[sectorMaps,{x}])&/@LocalTargets
		(* but it seems impossible to have new targets amerge in mapped sectors *)
	];*)
	
	secheight=SectorHeight[sector];
	secindex=SectorIndex[sector];
	secNo=SectorNumber[sector];
	
	secNum=secNo;
	
	propLocus=Position[sector,1]//Flatten;
	ISPLocus=Position[sector,0]//Flatten;
	
	sectorCut=SectorCut[sector];	
	BaikovCut=Table[z[propLocus[[i]]]->0,{i,1,Length[propLocus]}];
	
	r=Max[IntegralPropagatorDegree/@LocalTargets];
	s=Max[IntegralISPDegree/@LocalTargets];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Initialized. Time Used: ", Round[AbsoluteTime[]-timer], " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB." ]];
	
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
		MIs=AzuritinoMIFind[sector,CriticalPointCounting->CriticalPointInAzuritino,selfSymmetryZMaps->zMaps];
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
		VectorList=SingularIntersection[secindex,degBound->5,VariableOrder->(var//Reverse),Cut->OptionValue[Cut]]
		,
		"Linear",
		{M1,M1ext,M2}=TangentModules[secindex,{}];
		VectorList=SolveDegreedIntersection[M1ext,secindex,4]
	];
	(*ProbeIntermediateResult["vectors",secNo,VectorList];*)
	(*end of MaxMemoryUsed*)];
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Module intersections solved. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  VectorList Length: ", Length[VectorList]]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  VectorList ByteCount: ", ByteCount[VectorList]]];
	(*If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Module intersection ",AbsoluteTime[]-timer];];*)
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Generating Formal IBPs..."]];
	If[probeTheFunctions===True,Print["{secindex,VectorList} probed"];probe["{secindex,VectorList}",secNum]={secindex,VectorList}];
	
	
	FIBPs=IBPGenerator[#,secindex,Cut->OptionValue[Cut]]&/@VectorList;
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[FIBPs]," Formal IBPs generated. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	(*ProbeIntermediateResult["FIBPs",secNo,FIBPs];*)
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Driving DenominatorTypes..."]];
	DenominatorTypes=Union[Append[IntegralPropagatorType/@LocalTargets,sector]];
	If[debugModification20230314,DenominatorTypes=DenominatorTypeCompleting[DenominatorTypes]];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  DenominatorTypes derived. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	
	(* Remove IBPs for lower sectors *)
	(* But this will wrongly kill some IBPs that generates IBPs not for lower sectors at non-corner seed*)
	(* I (zihao) suggest we turn off this step, the subsec IBPs will be automatically removed in the next steps so it is not needed here to do so*)
	If[OptionValue[KillCornerSubsecFIBPs],
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Removing FIBPs for lower sectors seeding with DenominatorTypes..."]];
		FIBPs1={};
		CornerIBP={};
		
		For[i=1,i<=Length[FIBPs],i++,
			(*baseIBP=IntegralRealization[FIBPs[[i]],sector];*)
			
			(*If[(baseIBP/.sectorCut)===0&&(Union[VectorList[[i,propLocus]]/.BaikovCut]==={0}),
				Continue[];  (* This IBP corresponds to a lower sector *)
			]; *)
			
			maxDenominatorIndices=Max/@Transpose[DenominatorTypes];
			formalIntegrals=Cases[Variables[FIBPs[[i]]],_G];
			
			FIBPCurrentSectorIntegrals=Union[
				Expand[
					(IntegralRealization[#,maxDenominatorIndices]/.sectorCut)&/@formalIntegrals
				]
			];
			
			If[FIBPCurrentSectorIntegrals==={0},
				Continue[];  (* This FIBP seeds to a lower sector in DenominatorTypes*)
			]; 
			
			AppendTo[FIBPs1,FIBPs[[i]]];
			AppendTo[CornerIBP,baseIBP];
		];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[FIBPs]," Formal IBPs are generated; ",Length[FIBPs1]," Formal IBPs are used. ",Length[FIBPs]-Length[FIBPs1]," Formal IBPs are removed. "(*,AbsoluteTime[]-timer*)];];
		FIBPs=FIBPs1;
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  IBPs for lower sectors removed. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed/(1024^2)]," MB."]];
	];
	
	
	
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
		
		"Direct",
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
		(*timing and memory report omitted here*)
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
		
		
		
		If[probeTheFunctions===True,Print["{rawIBPs,nFIBPs,sectorCut} probed"];probe["{rawIBPs,nFIBPs,sectorCut}",secNum]={rawIBPs,nFIBPs,sectorCut}];
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
			(*timing and memory report omitted here*)
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
			
			
		
			
			
			If[probeTheFunctions===True,Print["{nIBPs,newIBPs,nFIBPs,sectorCut} probed"];probe["{nIBPs,newIBPs,nFIBPs,sectorCut}",secNum]={nIBPs,newIBPs,nFIBPs,sectorCut}];
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
		
		"Zurich",
		timer=AbsoluteTime[];
		memoryUsed=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Seeding in step 0..."]];
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Deriving IBPISPdegrees..."];];
		
		If[probeTheFunctions===True,Print["{sector,CornerIBP,VectorList,FIBPs} probed"];probe["{sector,CornerIBP,VectorList,FIBPs}",secNum]={sector,CornerIBP,VectorList,FIBPs}];
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
		
		
		If[Not[NeedSymmetry===False],
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  \t","Appending self-symmetries at current step..."];];
			
			seeds=Flatten[SeedMerge[NumeratorShifts[sector,#],DenominatorTypes]&/@Range[0,OptionValue[ZurichInitialSteps]],1];
			
			rawIBPs=Join[
				rawIBPs,
				Table[SelfSymmetryR[ZM[i],seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten
			];
			
			nIBPs=Join[
				nIBPs,
				Table[SelfSymmetryRealization[zMaps[[i]]/.GenericPoint,seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten
			];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","\t\tAppending self-symmetries finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			
		];
		
		
		
		
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Deriving SectorIntegrals..."];];
		SectorIntegrals=Select[IntegralList[nIBPs],Sector[#]==sector&];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","SectorIntegrals Derived. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		(*ProbeIntermediateResult["nIBPs_and_SectorIntegrals",secNo,{nIBPs,SectorIntegrals}];*)
		timer2=AbsoluteTime[];
		memoryUsed2=MaxMemoryUsed[
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Performing IBPAnalyze..."];];
		{redIndex,irredIndex,rIBPs}=IBPAnalyze[nIBPs,SectorIntegrals];
		SectorIntegrals=Select[IntegralList[nIBPs],Sector[#]==sector&];
		If[Not[MIFromAzuritino===True],
			MIs=SectorIntegrals[[irredIndex]];    (* This is not the final MI *)
		];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","IBPAnalyze finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Seeding in step 0 finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
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
			If[Not[NeedSymmetry===False],
				timer2=AbsoluteTime[];
				memoryUsed2=MaxMemoryUsed[
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  \t","Appending self-symmetries at current step..."];];
				seeds=Flatten[SeedMerge[NumeratorShifts[sector,#],DenominatorTypes]&/@Range[0,OptionValue[ZurichInitialSteps]],1];
				NewrawIBPs=Join[
					NewrawIBPs,
					Table[SelfSymmetryR[ZM[i],seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten
				];
				NewnIBPs=Join[
					NewnIBPs,
					Table[SelfSymmetryRealization[zMaps[[i]]/.GenericPoint,seeds[[j]]],{i,1,Length[zMaps]},{j,1,Length[seeds]}]//Flatten
				];
				(*end of MaxMemoryUsed*)];
				If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ","\t\tAppending self-symmetries finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			];
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Joining results of the steps..."];];
			rawIBPs=Join[rawIBPs,NewrawIBPs];
			nIBPs=Join[nIBPs,NewnIBPs];
			SectorIntegrals=Select[IntegralList[nIBPs],Sector[#]==sector&];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Joining finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Performing IBPAnalyze..."];];
			{redIndex,irredIndex,rIBPs}=IBPAnalyze[nIBPs,SectorIntegrals];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","IBPAnalyze finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			
			timer2=AbsoluteTime[];
			memoryUsed2=MaxMemoryUsed[
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Ending this step..."];];
			If[Not[MIFromAzuritino===True],
				MIs=Intersection[MIs,SectorIntegrals[[irredIndex]]];
			];
			WellAddressedIntegrals=FindReducedIntegrals[rIBPs,MIs];
			WellAddressedIntegrals=Join[WellAddressedIntegrals,MIs];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","Step",step," finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t\t  ","s=",step,", ",Length[NewnIBPs]," more IBPs are generated"];];
			(*end of MaxMemoryUsed*)];
			If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Seeding in step ",step," finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
			If[SubsetQ[WellAddressedIntegrals,LocalTargets],Break[]];
		];
		
		
		
	];
	If[Not[MIFromAzuritino===True],
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ",Length[MIs]," MI(s) : ",MIs];];
	];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Current IBP number: ",Length[nIBPs]];];
	
	
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
	IBPIndex=Select[Range[Length[nIBPs]],CollectG[nIBPs[[#]]/.sectorCut]=!=0&];(*CollectG may be slow... and seems to be unnecessary*)
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[nIBPs]-Length[IBPIndex]," IBPs removed, ",Length[IBPIndex]," IBPs remained."]];
	rawIBPs=rawIBPs[[IBPIndex]];
	nIBPs=nIBPs[[IBPIndex]];
	(*|||||||||*)(*ProbeIntermediateResult["nIBPs_subsecIBPsRemoved",secNo,nIBPs];*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Subsector IBPs removed. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Sorting nFIBPs..."]];
	(* Sort the IBPs and find the independent Ones*)
	(* degRep=Dispatch[#->rr^Total[IntegralAbsDegree[#]]&/@IntegralList[nIBPs]];
	IBPDegreeList=Exponent[#,rr]&/@(nIBPs/.degRep); *)
	
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
	IBPIndex=SortBy[Range[Length[nIBPs]],{IBPDegreeList[[#]],leafCounts[[#]],byteCounts[[#]]}&];
	rawIBPs=rawIBPs[[IBPIndex]];
	nIBPs=nIBPs[[IBPIndex]];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ","Finished. Time Used: ", Round[AbsoluteTime[]-timer2],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	(*|||||||||*)(*ProbeIntermediateResult["nIBPs_sorted",secNo,nIBPs];*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  nFIBPs sorted. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Selecting independent IBPs..."]];
	

	
	IBPIndex=IndepedentSet[nIBPs,SectorIntegrals];
	rawIBPs=rawIBPs[[IBPIndex]];
	
	(*I put the following integral realization codes after oerlikon algorithm*)
	(*rawIBPs=rawIBPs/.FI[i_]:>FIBPs[[i]]/.IntegralR->IntegralRealization;   (* Only at this step, we obtain the analytic IBPs *)(*Why not /.sectorCut?*)
	If[Not[NeedSymmetry===False],
		rawIBPs=rawIBPs/.ZM[i_]:>(zMaps[[i]])/.SelfSymmetryR->SelfSymmetryRealization;
	];*)
	
	nIBPs=nIBPs[[IBPIndex]];
	
	(*|||||||||*)(*ProbeIntermediateResult["nIBPs_independent",secNo,nIBPs];*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Independent IBPs selected. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[rawIBPs]," IBPs are selected with ",Length[IntegralList[nIBPs/.SectorCut[sector],SortTheIntegrals->False]]," integrals in current sector."]];
	
	(*ProbeIntermediateResult["nIBPs2",secNo,nIBPs];*)
	
	(* Remove the unneeded IBP , Oerlikon algorithm *)
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Removing the unneeded IBPs..."]];
	
	(*SectorIntegrals=IntegralList[nIBPs];*)
	ReducedIntegrals=Complement[LocalTargets,MIs];
	
	
	UsedIndex=UsedRelations[nIBPs,ReducedIntegrals,MIs];
	
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  ",Length[nIBPs]-Length[UsedIndex]," IBP relations are removed. ", Length[UsedIndex]," left."];];
	
	
	rawIBPs=rawIBPs[[UsedIndex]];
	nIBPs=nIBPs[[UsedIndex]];(*although not needed, we keep this to debug*)
	
	(*|||||||||*)(*ProbeIntermediateResult["nIBPs_used",secNo,nIBPs];*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Uneeded IBPs removed. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  ",Length[rawIBPs]," IBPs remaining with ",Length[IntegralList[nIBPs/.SectorCut[sector],SortTheIntegrals->False]]," integrals in current sector."]];
	
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Realizing raw IBPs..."]];
	rawIBPs=rawIBPs/.FI[i_]:>FIBPs[[i]]/.IntegralR->IntegralRealization;   (* Only at this step, we obtain the analytic IBPs *)
	If[Not[NeedSymmetry===False],
		rawIBPs=rawIBPs/.ZM[i_]:>(zMaps[[i]])/.SelfSymmetryR->SelfSymmetryRealization;
	];
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Raw IBP realized. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
	
	
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
			rawIBPs=SymmetryMap[sectorMaps,#]&/@rawIBPs
		];
		(*end of MaxMemoryUsed*)];
		If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Sector mapping finished. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
		
	];
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	(*	 Remove zero-sector integrals *)
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"  Removing zero-sector integrals..."]];
	
    integrals=Select[IntegralList[rawIBPs],!MemberQ[Global`ZeroSectors,Sector[#]]&];
	rawIBPs=Factor[CoefficientArrays[rawIBPs,integrals][[2]]].integrals; (*Do I need to factor these coefficients?...OH yes, very important*)
	(*end of MaxMemoryUsed*)];
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Zero-sector integrals removed. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
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
	If[OptionValue[Verbosity]==1,PrintAndLog["#",secNo,"\t  Results saved for current sector. Time Used: ", Round[AbsoluteTime[]-timer],  " second(s). Memory used: ",Round[memoryUsed2/(1024^2)]," MB."]];
	
];









(* ::Subsection::Closed:: *)
(*Row Reduce Modules*)


(*NonzeroEntriesString[matrix_]:=ToString[InputForm[N[Round[1000*((Length[ArrayRules[matrix]]-1)/(Times@@Dimensions[matrix]))]*0.1]]]<>"%"*)



Options[IBPAnalyze]:={Modulus->42013};
IBPAnalyze[IBPs_,Ints_,OptionsPattern[]]:=Module[{M,RM,redIndex,irredIndex,timer,memoryUsed},
	M=CoefficientArrays[IBPs,Ints][[2]];
	If[probeTheFunctions===True,Print["Matrix in IBPAnalyze probed"];probe["IBPAnalyze",secNum]=M];
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
	Return[{redIndex,irredIndex,RM.Ints}];
];


Options[IndepedentSet]:={Modulus->42013};
IndepedentSet[IBPs_,Ints_,OptionsPattern[]]:=Module[{M,RM,redIndex,indepIndex,timer,memoryUsed},
	M=CoefficientArrays[IBPs,Ints][[2]];
	If[probeTheFunctions===True,Print["Matrix in IndepedentSet probed"];probe["IndepedentSet",secNum]=M//Transpose];
	timer=AbsoluteTime[];
	memoryUsed=MaxMemoryUsed[
	If[TimingReportOfRowReduce===True,PrintAndLog["#",secNum,"\t\t  RowReduce in IndepedentSet started. Matrix dimension: ",Dimensions[M//Transpose]]];
	(*If[probeTheFunctions===True,Print["Special pause for 150 seconds!"];Pause[150]];*)
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


SparseIdentityMatrix[n_]:=SparseArray[Table[{k,k}->1,{k,n}]]


Options[UsedRelations]:={Modulus->42013};
UsedRelations[IBPs_,ReducedIntegrals_,MIs_,OptionsPattern[]]:=Module[{Ints,M,Mext,ReducedIntegralColumns,RM,i,j,columnIndex,rowIndex,MatrixL,tempList,result,timer,memoryUsed},
	Ints=IntegralList[IBPs];
	M=CoefficientArrays[IBPs,Ints][[2]];
	
	ReducedIntegralColumns=Flatten[Position[Ints,#]&/@ReducedIntegrals];
	tempList=Table[Null,{i,1,Length[ReducedIntegralColumns]}];
	(*Mext=Join[Transpose[M],IdentityMatrix[M//Length]]//Transpose;*)
	Mext=Join[Transpose[M],SparseIdentityMatrix[M//Length]]//Transpose;
	
	(*Export[Global`workingPath<>"UR.txt",Mext//InputForm//ToString];*)
	
	If[probeTheFunctions===True,Print["Matrix in UsedRelations probed"];probe["UsedRelations",secNum]=Mext];
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


Options[IBPtest]:={Modulus->42013};
IBPtest[IBPs_,sector_,OptionsPattern[]]:=Module[{M,RM,Ints,redIndex,irredIndex,timer,memoryUsed},
	Ints=Select[IntegralList[IBPs],Sector[#]==sector&]//IntegralList;
	M=CoefficientArrays[IBPs/.GenericPoint/.GenericD,Ints][[2]];
	If[probeTheFunctions===True,Print["Matrix in IBPtest probed"];probe["IBPtest",secNum]=M];
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
];


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
