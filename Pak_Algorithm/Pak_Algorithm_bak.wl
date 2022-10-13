(* ::Package:: *)

Print["Pak algorithm implemented by Yang Zhang. 03.18.2022"];


(* ::Section:: *)
(*PAK*)


Options[Polynomial2Matrix]:={MonomialOrder->DegreeReverseLexicographic};
Polynomial2Matrix[poly_,var_,OptionsPattern[]]:=Module[{mlist,trivialization,Matrix,n,ExtM},
	trivialization=Dispatch[#->1&/@var];
	n=Length[var];
	mlist=MonomialList[poly,var,OptionValue[MonomialOrder]];
	Matrix=Table[0,{i,1,n+1},{j,1,Length[mlist]}];
	Matrix[[1]]=mlist/.trivialization;
	Do[Matrix[[i+1]]=Exponent[mlist,var[[i]]],{i,1,n}];
	Matrix=Matrix//Transpose;
	ExtM=Join[Matrix,{Join[{0},var]}];
	Return[ExtM];
];


MatrixColumnSwap[M1_,i_,j_]:=Module[{ColumnVector,M=M1},
	ColumnVector=M[[All,i]];
	M[[All,i]]=M[[All,j]];
	M[[All,j]]=ColumnVector;
	Return[M];
];
MatrixRowSort[M1_,FirstColumnNumber_]:=Module[{AuxMatrix,M},
	AuxMatrix={M1[[-1]]};   (* Last row is auxilliary *)
	M=M1[[1;;-2]];
    M=SortBy[M,#[[1;;FirstColumnNumber]]&];
    Return[Join[M,AuxMatrix]];
];
InverseTrans[rep_]:=Sort[Reverse/@rep];



(* StandardPermutation[f_,var_]:=Module[{ip,MatrixCopies,m,n,j,pos,matrix=M1,MatrixRep,MaxIndex},
	matrix=Polynomial2Matrix[f,var];
	{m,n}= Dimensions[matrix]-{1,1};   (* The last row is Auxilliary. n is the number of variables *)
	
	
	For[ip=2,ip<=(n+1),ip++,
		MatrixCopies=Table[MatrixColumnSwap[matrix,ip,j],{j,ip,n+1}];
		MatrixRep=Table[Null,{j,ip,n+1}];
		(* On each copy *)
		
		For[j=ip,j<=(n+1),j++,
			(* n+2-ip copies *)
			pos=j-ip+1;
			MatrixCopies[[pos]]=MatrixRowSort[MatrixCopies[[pos]],ip];
			
			MatrixRep[[pos]]=MatrixCopies[[pos]][[1;;-2,ip]];
		];
		
		(* Find the maximized copy *)
		MaxIndex=SortBy[Range[n+2-ip],MatrixRep[[#]]&][[-1]];
		
		matrix=MatrixCopies[[MaxIndex]];
		Print[matrix//MatrixForm];
		
	];	
	Return[Table[matrix[[-1,1+i]]->var[[i]],{i,1,n}]//Sort];   (* Reversed permutation, to checked further *)



]; *)


FullStandardPermutation[f_,var_]:=Module[{matrix,ip,MatrixCopies,m,n,j,pos,MatrixRep,MaxIndex,initialmatrix,matrixpool,nn,index,groups
,MaximumVector},
	initialmatrix=Polynomial2Matrix[f,var];
	{m,n}= Dimensions[initialmatrix]-{1,1};   (* The last row is Auxilliary. n is the number of variables *)
	
	matrixpool={initialmatrix};
	
	
	
	For[ip=2,ip<=(n+1),ip++,
			MatrixCopies=Flatten[Table[MatrixColumnSwap[matrixpool[[nn]],ip,j],{nn,1,Length[matrixpool]},{j,ip,n+1}],1];
			MatrixCopies=Table[MatrixRowSort[MatrixCopies[[j]],ip],{j,1,MatrixCopies//Length}];
			
			MatrixRep=Table[MatrixCopies[[j]][[1;;-2,ip]],{j,1,MatrixCopies//Length}];
			
			(* Find the maximum vector *)
			MaximumVector=Sort[MatrixRep][[-1]];
			index=Table[If[MatrixRep[[j]]===MaximumVector,j,Nothing],{j,1,MatrixRep//Length}];
			matrixpool=MatrixCopies[[index]];
			(* Print[MatrixForm/@matrixpool]; *)
	];
		
		
		
	
		
		
		
	Return[Table[matrixpool[[nn]][[-1,1+i]]->var[[i]],{nn,1,Length[matrixpool]},{i,1,n}]//Sort];   (* Reversed permutation, to checked further *)



];
PolynomialSymmetry[poly_,var_]:=Module[{reps},
	reps=FullStandardPermutation[poly,var];
	Return[Table[Sort[#[[1]]->(#[[2]]/.InverseTrans[reps[[1]]])&/@reps[[i]]      ],{i,1,Length[reps]}]];

];


PakStandard[f1_,var_]:=f1/.FullStandardPermutation[f1,var][[1]];
PakCompare[f1_,f2_,var_]:=Module[{StandardPolynomial1,StandardPolynomial2,rep1,rep2,rep},
	rep1=FullStandardPermutation[f1,var][[1]];
	StandardPolynomial1=f1/.rep1;
	rep2=FullStandardPermutation[f2,var][[1]];
	StandardPolynomial2=f2/.rep2;
	If[!Expand[StandardPolynomial1-StandardPolynomial2]===0,Return[False]];
	rep=Table[var[[i]]->(var[[i]]/.rep1/.(Reverse/@rep2)),{i,1,Length[var]}]//Sort;
	Return[rep];    
];





(* F1=3x1+x2-x1 x2+x3^2+10 x4
F2=3x1+x2-x1 x2+x3^2+10 x4/.{x1\[Rule]z2,x2\[Rule]z4,x3\[Rule]z3,x4\[Rule]z1} *)


(* FullPakCompare[F1,F2,{x1,x2,x3,x4},{z1,z2,z3,z4}]
F1/.%//Union *)


(* ff=RandomPolynomial[x/@Range[10],3,20];
gg=ff/.{x[2]\[Rule]x[5],x[5]\[Rule]x[6],x[6]\[Rule]x[2]};
PakCompare[ff,gg,x/@Range[10]] *)


(* ::Section:: *)
(*Pak Subgraph *)


(* To see if f2 can be written f1/. some variable \[Rule] 0 + variable changes*)
PakSubCompare[f1_,f2_,var1_,var2_]:=Module[{len1,len2,i,subvars,NameChange,f1reduced,rep,reduction,CR1,CR2},
	len1=Length[var1];
	len2=Length[var2];
	If[len1<len2, Return[False]];
	CR2=(#[[2]]&/@CoefficientRules[f2,var2])//Sort;
	subvars=Subsets[var1,{len2}];
	For[i=1,i<=Length[subvars],i++,
		reduction=(#->0&/@Complement[var1,subvars[[i]] ]);
		f1reduced=f1/.reduction;
		CR1=(#[[2]]&/@CoefficientRules[f1reduced,subvars[[i]]])//Sort;
		If[!CR1===CR2,Continue[]];
		NameChange=MapThread[#1->#2&,{var2,subvars[[i]]}];

		rep=PakCompare[f2/.NameChange,f1reduced,subvars[[i]]];
		If[Length[rep]>0,Return[{reduction,Table[var2[[j]]->(var2[[j]]/.NameChange/.rep),{j,1,len2}]}]];
	];
	Return[False];

];

(* To find all the ways if f2 can be written f1/. some variable \[Rule] 0 + f2's variable changes*)
PakSubCompareDeep[f1_,f2_,var1_,var2_]:=Module[{len1,len2,i,subvars,NameChange,f1reduced,rep,reduction,CR1,CR2,result={}},
	len1=Length[var1];
	len2=Length[var2];
	If[len1<len2, Return[False]];
	CR2=(#[[2]]&/@CoefficientRules[f2,var2])//Sort;
	subvars=Subsets[var1,{len2}];
	For[i=1,i<=Length[subvars],i++,
		reduction=(#->0&/@Complement[var1,subvars[[i]] ]);
		f1reduced=f1/.reduction;
		CR1=(#[[2]]&/@CoefficientRules[f1reduced,subvars[[i]]])//Sort;
		If[!CR1===CR2,Continue[]];
	(* 	NameChange=MapThread[#1->#2&,{var2,subvars[[i]]}]; *)
		
		rep=FullPakCompare[f2,f1reduced,var2,subvars[[i]]];
		If[Length[rep]>0,
			result=Join[result,{reduction,#}&/@rep];
		];
	];
	
	If[result=={},Return[False]];
	Return[result];
];



(* ::Section:: *)
(*Feynman Integral Related*)


Options[SymanzikPolynomials]:={EuclideanIntegral->False};
SymanzikPolynomials[Internal_,External_,Propagators_,Kinematics_,Sector_,OptionsPattern[]]:=Module[{Formula,MatrixA,VectorB,ScalarC,Vectorb,
zeroLoopMomenta,Momenta,LoopExternalScalars,ScalarTangentSet,ScalarTangentVector,n,L,SDim,var,FeynmanU,FeynmanF},	
	n=Length[External]+1;
	L=Length[Internal];
	
	Momenta=Join[External,Internal];
	
	(* Propagator search *)
	
	
	
	
	var=Table[z[i],{i,1,Length[Propagators]}];
	
	
	
	
	(* Feynman representation *)
	
	Formula=(Propagators.DiagonalMatrix[Sector].var)/.(#->\[Lambda] #&/@Internal);
	MatrixA=D[SeriesCoefficient[Formula,{\[Lambda],0,2}],{Internal},{Internal}]/2;	
	VectorB=Coefficient[SeriesCoefficient[Formula,{\[Lambda],0,1}],Internal]/2;
	ScalarC=SeriesCoefficient[Formula,{\[Lambda],0,0}];
	FeynmanU=Det[MatrixA]//Factor;
	FeynmanF=-FeynmanU (Expand[ScalarC]/.Kinematics)+ Cancel[FeynmanU (Expand[VectorB.Inverse[MatrixA].VectorB]/.Kinematics)];
	If[OptionValue[EuclideanIntegral],FeynmanF*=-1];
	Return[{FeynmanU,FeynmanF}];
	
];

Options[FeynmanParameterization]:={EuclideanIntegral->False,SplitOutput->False};
FeynmanParameterization[Internal_,External_,Propagators_,Kinematics_,int_,OptionsPattern[]]:=Module[{U,F,alpha,overallFactor,integrand,PositiveIndices,
xlist,index,Sector,var,L,multiplePropagatorFactor},
	index=List@@int;
	Sector=If[#>0,1,0]&/@index;
	var=Table[z[i],{i,1,Length[Propagators]}];
	L=Length[Internal];
	{U,F}=SymanzikPolynomials[Internal,External,Propagators,Kinematics,Sector];
	alpha=index//Total;
	PositiveIndices=List@@Select[index,#>0&];
	(* xlist=Table[ToExpression["x"<>ToString[i]],{i,1,Length[PositiveIndices]}]; *)
	overallFactor=Gamma[alpha-L d/2] /Times@@(Gamma/@PositiveIndices);
	multiplePropagatorFactor=Product[If[index[[i]]>0,var[[i]]^(index[[i]]-1) ,1],{i,1,Length[var]}];
	If[!OptionValue[EuclideanIntegral],overallFactor*=(-1)^alpha];
	If[!OptionValue[SplitOutput],
		integrand=F^(L d/2-alpha)/U^((L+1)d/2-alpha)*multiplePropagatorFactor;
		Return[{overallFactor,integrand}];
		,
		Return[{overallFactor,U,(L+1)d/2-alpha,F,(L d/2-alpha),multiplePropagatorFactor}];
	];
];


SectorCompare[SectorStructure1_,SectorStructure2_]:=Module[{Internal1,External1,Propagators1,Replacements1,SectorIndex1,
Internal2,External2,Propagators2,Replacements2,SectorIndex2,result={},
G1,G2,dim1,dim2,vlist1,vlist2,rep,i,ref1,Int1,rule},
	If[Length[SectorStructure1]!=5||Length[SectorStructure2]!=5, Print["Incorrect sector info ..."]; Return[{}]];
	{Internal1,External1,Propagators1,Replacements1,SectorIndex1}=SectorStructure1;
	{Internal2,External2,Propagators2,Replacements2,SectorIndex2}=SectorStructure2;
	If[Sort[SectorIndex1]!=Sort[SectorIndex2],Return[{}]];
	G1=SymanzikPolynomials[Internal1,External1,Propagators1,Replacements1,SectorIndex1]//Total//Expand;
	G2=SymanzikPolynomials[Internal2,External2,Propagators2,Replacements2,SectorIndex2]//Total//Expand;
	
	
	
	vlist1=Variables[G1];
	vlist2=Variables[G2];
	rep=FullPakCompare[G1,G2,vlist1,vlist2];
	If[rep=={},
		Return[rep];
		,
		dim1=Length[Propagators1];
		dim2=Length[Propagators2];
		ref1=Product[z[i]^n[i],{i,1,dim1}]/.(#->1&/@Complement[z/@Range[dim1],vlist1])/.n[j_]:>ToExpression["n"<>ToString[j]];
		
		Int1=G[AA,If[#===0,0,PatternTest[Pattern[#,Blank[]],Positive]]&/@(Exponent[ref1,z/@Range[dim2]])];
		
		For[i=1,i<=Length[rep],i++,
			rule=(Int1:>Evaluate[(G[BB,Exponent[ref1/.rep[[i]],z/@Range[dim2]]])]);
			AppendTo[result,rule];
		];
		Return[result];
	
	];
];


(* ::Section:: *)
(*Momentum Space*)


(*
zihao 2022.09.20
1. restricted the coeffs a[...], b[...], and c[...] to be numbers, rather than rational functions of s,t,...
2. some sub diagrams donot depend on t for example, so no need to require t\[Rule]t. The former version was too strict, so that some symmetries were lost. The new version fixed this.


*)
GenerateUF[propagators_,loops_,kineticConditions_]:=Module[{xs,x,U,F,A,b,c,coefficients},
	xs=Table[x[i],{i,Length[propagators]}];
	coefficients=CoefficientArrays[xs.propagators,loops];
	If[Length[coefficients]!=3,Print["GenerateUF Err: x.D is not quadratic."];Return[$Failed]];
	{c,b,A}=coefficients;
	A=Normal[A];
	A=Expand[(A+Transpose[A])/2];
	b=Normal[b/2];
	c=Normal[c];
	U=(Det[A]//Expand)/.kineticConditions;
	F=((-c U+Factor[U (Inverse[A].b).b])//Expand)/.kineticConditions;
	Return[{U,F,xs}//Expand]
]
KinematicConstrains[internal_,external_,props_,kinematics_,head_]:=Module[{T,c,rep1,eqns,U,F,G,G1,G2,xs,clist,otherVars,kinematics2},
	T=Table[c[i,j],{i,1,external//Length},{j,1,external//Length}];
	If[Length[external]===0,Return[{}]];
	rep1=MapThread[#1->#2&,{external,T.external}];
	{U,F,xs}=GenerateUF[props,internal,{}];
	G=U+F;
	G1=Expand[G/.kinematics];
	kinematics2=Table[kinematics[[i,1]]->(Expand[kinematics[[i,1]]/.rep1]/.kinematics),{i,Length[kinematics]}];
	G2=Expand[G/.kinematics2];
	
	eqns={Expand[G1-G2]};
	clist=Select[Variables[eqns],Head[#]==c&];
	otherVars=Complement[Variables[eqns],clist];
	eqns=#[[2]]&/@Flatten[CoefficientRules[#,otherVars]&/@eqns];
	eqns/.c->head
]






(* ::Section:: *)
(*Needed Codes*)


Options[MomentumMap]={FreeEMs->False,FullEMsConstrain->False};
MomentumMap[internal_,external_,prop1_,prop2_,kinematics_,OptionsPattern[]]:=Module[{allvectors,a,b,c,A,B,T,rep1,rep2,rep,prop1trans,prop2expand,eqns,formula,clist,GB,sol,i,
result={},DM,leadingCoef,kvars,kEqns},
	allvectors=Join[internal,external];
	A=Table[a[i,j],{i,1,internal//Length},{j,1,internal//Length}];
	B=Table[b[i,j],{i,1,internal//Length},{j,1,external//Length}];
	T=Table[c[i,j],{i,1,external//Length},{j,1,external//Length}];
	If[Length[external]>0,
		rep1=MapThread[#1->#2&,{external,T.external}],
		rep1={};
	];
	
	
	rep2=MapThread[#1->#2&,{internal,A.internal+B.external}];
	rep=Join[rep1,rep2];
	prop1trans=Expand[prop1/.rep]/.kinematics;
	prop2expand=Expand[prop2]/.kinematics;
	
	
	eqns=#[[2]]&/@Flatten[CoefficientRules[#,allvectors]&/@(prop1trans-prop2expand)];	
	If[external=!={}&&(OptionValue[FreeEMs]===False),
		If[OptionValue[FullEMsConstrain]===True,
			kEqns=Table[(Expand[kinematics[[i,1]]/.rep]/.kinematics)-kinematics[[i,2]],{i,1,kinematics//Length}]
		,
			kEqns=KinematicConstrains[internal,external,prop2(*prop1 or prop2 here both ok*),kinematics,c]
		]
	,
		kEqns={}
		
	];
	
	eqns=Join[eqns,kEqns];

	clist=Select[Variables[eqns],Head[#]==a||Head[#]==b||Head[#]==c&];
	
	
	(*Following 2 lines added by zihao, 2022.09.20*)
	kvars=Complement[Variables[eqns],clist];
	eqns=#[[2]]&/@Flatten[CoefficientRules[#,kvars]&/@eqns];
	(************)
	
	GB=GroebnerBasis[eqns,clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
	
	
	If[GB=={1},(*Print["momentum transformation not found ..."];*)Return[{}]];

	sol=Solve[GB==0,clist];
	

	If[sol=={},Return[{}]];
	
	For[i=1,i<=Length[sol],i++,
		DM=D[internal/.rep/.sol[[i]],{internal}];
		
		If[Det[DM]^2!=1,Continue[]];   (* Wrong Jacobi for internal momenta *)
		AppendTo[result,sol[[i]]];
	(*	leadingCoef=SortBy[ArrayRules[DM],#[[1]]&][[1,2]]; *)
	(* 	Print[Det[DM]," ",leadingCoef]; *)
		(* If[leadingCoef>0,AppendTo[result,sol[[i]]]];  *)  (* Gauge choice to remove unwanted ALL-MINUS transformations *)
	];

	Return[rep/.result];
];



FullPakCompare[f1_,f2_,vlist1_,vlist2_]:=Module[{F1,F2,reName,StandardPolynomial1,StandardPolynomial2,rep1List,rep2,repList,i,reNameInv},
	If[Length[vlist1]!=Length[vlist2],Return[{}]];
	F1=f1;
	reName=Table[vlist2[[i]]->vlist1[[i]],{i,1,Length[vlist1]}];
	reNameInv=Table[vlist1[[i]]->vlist2[[i]],{i,1,Length[vlist1]}];
	F2=f2/.reName;
	
	rep1List=FullStandardPermutation[F1,vlist1];
	StandardPolynomial1=F1/.rep1List[[1]];
	
	rep2=FullStandardPermutation[F2,vlist1][[1]];
	StandardPolynomial2=F2/.rep2;
	If[!Expand[StandardPolynomial1-StandardPolynomial2]===0,Return[{}]];
	repList=Reap[For[i=1,i<=Length[rep1List],i++,
		Sow[Table[vlist1[[j]]->(vlist1[[j]]/.rep1List[[i]]/.(Reverse/@rep2)/.reNameInv),{j,1,Length[vlist1]}]//Sort];
	]][[2,1]];
	Return[repList];
];


(* ::Section:: *)
(*Needed Codes*)


MatrixRowSort[M1_,FirstColumnNumber_]:=Module[{AuxMatrix,M},
	AuxMatrix={M1[[-1]]};   (* Last row is auxilliary *)
	M=M1[[1;;-2]];
    M=SortBy[M,#[[1;;FirstColumnNumber]]&];
    Return[Join[M,AuxMatrix]];
];


MatrixColumnSwap[M1_,i_,j_]:=Module[{ColumnVector,M=M1},
	ColumnVector=M[[All,i]];
	M[[All,i]]=M[[All,j]];
	M[[All,j]]=ColumnVector;
	Return[M];
];


Options[Polynomial2Matrix]:={MonomialOrder->DegreeReverseLexicographic};
Polynomial2Matrix[poly_,var_,OptionsPattern[]]:=Module[{mlist,trivialization,Matrix,n,ExtM},
	trivialization=Dispatch[#->1&/@var];
	n=Length[var];
	mlist=MonomialList[poly,var,OptionValue[MonomialOrder]];
	Matrix=Table[0,{i,1,n+1},{j,1,Length[mlist]}];
	Matrix[[1]]=mlist/.trivialization;
	Do[Matrix[[i+1]]=Exponent[mlist,var[[i]]],{i,1,n}];
	Matrix=Matrix//Transpose;
	ExtM=Join[Matrix,{Join[{0},var]}];
	Return[ExtM];
];


FullStandardPermutation[f_,var_]:=Module[{matrix,ip,MatrixCopies,m,n,j,pos,MatrixRep,MaxIndex,initialmatrix,matrixpool,nn,index,groups
,MaximumVector},
	initialmatrix=Polynomial2Matrix[f,var];
	{m,n}= Dimensions[initialmatrix]-{1,1};   (* The last row is Auxilliary. n is the number of variables *)
	
	matrixpool={initialmatrix};
	
	
	
	For[ip=2,ip<=(n+1),ip++,
			MatrixCopies=Flatten[Table[MatrixColumnSwap[matrixpool[[nn]],ip,j],{nn,1,Length[matrixpool]},{j,ip,n+1}],1];
			MatrixCopies=Table[MatrixRowSort[MatrixCopies[[j]],ip],{j,1,MatrixCopies//Length}];
			
			MatrixRep=Table[MatrixCopies[[j]][[1;;-2,ip]],{j,1,MatrixCopies//Length}];
			
			(* Find the maximum vector *)
			MaximumVector=Sort[MatrixRep][[-1]];
			index=Table[If[MatrixRep[[j]]===MaximumVector,j,Nothing],{j,1,MatrixRep//Length}];
			matrixpool=MatrixCopies[[index]];
			(* Print[MatrixForm/@matrixpool]; *)
	];
		
		
		
	
		
		
		
	Return[Table[matrixpool[[nn]][[-1,1+i]]->var[[i]],{nn,1,Length[matrixpool]},{i,1,n}]//Sort];   (* Reversed permutation, to checked further *)



];


FullPakCompare[f1_,f2_,vlist1_,vlist2_]:=Module[{F1,F2,reName,StandardPolynomial1,StandardPolynomial2,rep1List,rep2,repList,i,reNameInv},
	If[Length[vlist1]!=Length[vlist2],Return[{}]];
	F1=f1;
	reName=Table[vlist2[[i]]->vlist1[[i]],{i,1,Length[vlist1]}];
	reNameInv=Table[vlist1[[i]]->vlist2[[i]],{i,1,Length[vlist1]}];
	F2=f2/.reName;
	
	rep1List=FullStandardPermutation[F1,vlist1];
	StandardPolynomial1=F1/.rep1List[[1]];
	
	rep2=FullStandardPermutation[F2,vlist1][[1]];
	StandardPolynomial2=F2/.rep2;
	If[!Expand[StandardPolynomial1-StandardPolynomial2]===0,Return[{}]];
	repList=Reap[For[i=1,i<=Length[rep1List],i++,
		Sow[Table[vlist1[[j]]->(vlist1[[j]]/.rep1List[[i]]/.(Reverse/@rep2)/.reNameInv),{j,1,Length[vlist1]}]//Sort];
	]][[2,1]];
	Return[repList];
];


GenerateUF[propagators_,loops_,kineticConditions_]:=Module[{xs,x,U,F,A,b,c,coefficients},
	xs=Table[x[i],{i,Length[propagators]}];
	coefficients=CoefficientArrays[xs.propagators,loops];
	If[Length[coefficients]!=3,Print["GenerateUF Err: x.D is not quadratic."];Return[$Failed]];
	{c,b,A}=coefficients;
	A=Normal[A];
	A=Expand[(A+Transpose[A])/2];
	b=Normal[b/2];
	c=Normal[c];
	U=(Det[A]//Expand)/.kineticConditions;
	F=((-c U+Factor[U (Inverse[A].b).b])//Expand)/.kineticConditions;
	Return[{U,F,xs}//Expand]
]




KinematicConstrains[internal_,external_,props_,kinematics_,head_]:=Module[{T,c,rep1,eqns,U,F,G,G1,G2,xs,clist,otherVars,kinematics2},
	T=Table[c[i,j],{i,1,external//Length},{j,1,external//Length}];
	If[Length[external]===0,Return[{}]];
	rep1=MapThread[#1->#2&,{external,T.external}];
	{U,F,xs}=GenerateUF[props,internal,{}];
	G=U+F;
	G1=Expand[G/.kinematics];
	kinematics2=Table[kinematics[[i,1]]->(Expand[kinematics[[i,1]]/.rep1]/.kinematics),{i,Length[kinematics]}];
	G2=Expand[G/.kinematics2];
	
	eqns={Expand[G1-G2]};
	clist=Select[Variables[eqns],Head[#]==c&];
	otherVars=Complement[Variables[eqns],clist];
	eqns=#[[2]]&/@Flatten[CoefficientRules[#,otherVars]&/@eqns];
	eqns/.c->head
]



Options[MomentumMap]={FreeEMs->False,FullEMsConstrain->False};
MomentumMap[internal_,external_,prop1_,prop2_,kinematics_,OptionsPattern[]]:=Module[{allvectors,a,b,c,A,B,T,rep1,rep2,rep,prop1trans,prop2expand,eqns,formula,clist,GB,sol,i,
result={},DM,leadingCoef,kvars,kEqns},
	allvectors=Join[internal,external];
	A=Table[a[i,j],{i,1,internal//Length},{j,1,internal//Length}];
	B=Table[b[i,j],{i,1,internal//Length},{j,1,external//Length}];
	T=Table[c[i,j],{i,1,external//Length},{j,1,external//Length}];
	If[Length[external]>0,
		rep1=MapThread[#1->#2&,{external,T.external}],
		rep1={};
	];
	
	
	rep2=MapThread[#1->#2&,{internal,A.internal+B.external}];
	rep=Join[rep1,rep2];
	prop1trans=Expand[prop1/.rep]/.kinematics;
	prop2expand=Expand[prop2]/.kinematics;
	
	
	eqns=#[[2]]&/@Flatten[CoefficientRules[#,allvectors]&/@(prop1trans-prop2expand)];	
	If[external=!={}&&(OptionValue[FreeEMs]===False),
		If[OptionValue[FullEMsConstrain]===True,
			kEqns=Table[(Expand[kinematics[[i,1]]/.rep]/.kinematics)-kinematics[[i,2]],{i,1,kinematics//Length}]
		,
			kEqns=KinematicConstrains[internal,external,prop2(*prop1 or prop2 here both ok*),kinematics,c]
		]
	,
		kEqns={}
		
	];
	
	eqns=Join[eqns,kEqns];

	clist=Select[Variables[eqns],Head[#]==a||Head[#]==b||Head[#]==c&];
	
	
	(*Following 2 lines added by zihao, 2022.09.20*)
	kvars=Complement[Variables[eqns],clist];
	eqns=#[[2]]&/@Flatten[CoefficientRules[#,kvars]&/@eqns];
	(************)
	
	GB=GroebnerBasis[eqns,clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
	
	
	If[GB=={1},(*Print["momentum transformation not found ..."];*)Return[{}]];

	sol=Solve[GB==0,clist];
	

	If[sol=={},Return[{}]];
	
	For[i=1,i<=Length[sol],i++,
		DM=D[internal/.rep/.sol[[i]],{internal}];
		
		If[Det[DM]^2!=1,Continue[]];   (* Wrong Jacobi for internal momenta *)
		AppendTo[result,sol[[i]]];
	(*	leadingCoef=SortBy[ArrayRules[DM],#[[1]]&][[1,2]]; *)
	(* 	Print[Det[DM]," ",leadingCoef]; *)
		(* If[leadingCoef>0,AppendTo[result,sol[[i]]]];  *)  (* Gauge choice to remove unwanted ALL-MINUS transformations *)
	];

	Return[rep/.result];
];

