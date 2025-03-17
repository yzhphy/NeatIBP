(* ::Package:: *)

(*Print["Pak algorithm implemented by Yang Zhang. 03.18.2022"];*)


Print["Pak algorithm included."]


WQPrintAndLog[x___]:=If[ReportWarningInMomentumMap,PrintAndLog[x]]


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



(* ::Section:: *)
(*MomentumMap*)


(* ::Subsection:: *)
(*MomentumMap*)


Options[MomentumMap]={FreeEMs->False,FullEMsConstrain->True};
MomentumMap[internal_,external_,prop1_,prop2_,kinematics_,OptionsPattern[]]:=Module[{allvectors,a,b,c,A,B,T,rep1,rep2,rep,prop1trans,prop2expand,eqns,formula,clist,GB,sol,i,
result={},DM,leadingCoef,kvars,kEqns,kinematicVar,Exteqns,ExtGB,instance,additionalConditions},
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
	
	
	(*(*Following 2 lines added by zihao, 2022.09.20*)
	kvars=Complement[Variables[eqns],clist];
	eqns=#[[2]]&/@Flatten[CoefficientRules[#,kvars]&/@eqns];
	(************)*)
	
	kinematicVar=Complement[Variables[eqns],clist];
	Exteqns=#[[2]]&/@Flatten[CoefficientRules[#,kinematicVar]&/@Complement[eqns,{0}]];
	ExtGB=GroebnerBasis[Exteqns,clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
    If[ExtGB==={1},
		If[UseNicePropagatorsInMomentumMap,
			Return[DeeperMomentumMap[internal,external,prop1,prop2,kinematics]]
		,
			Return[DeepMomentumMap[internal,external,prop1,prop2,kinematics]]
		];
		
	];
	
	If[QuotientRingSize[ExtGB,clist]<99999,
		sol=Solve[ExtGB==0,clist];
	,
		instance=FindInstance[ExtGB==0/.GenericPoint,clist,Rationals];
		If[instance=={},
			WQPrintAndLog[
				"** Warning: contradiction between ExtGB and its instance solutions while finding MomentumMaps between the following propagators:\n",
				prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
				"** No instance found. Giving up finding the corresponding symmetries."
			];
			Return[{}];
		];
		If[MemberQ[(Element[#,Rationals]===True)&/@Factor[instance[[1,All,2]]],False],
			instance=FindInstance[ExtGB==0/.GenericPoint,clist,Rationals];
		];
		If[instance=={},
			WQPrintAndLog[
				"** Warning: contradiction between ExtGB and its instance solutions while finding MomentumMaps between the following propagators:","\n",
				prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
				"** No rational instance found. Giving up finding the corresponding symmetries."
			];
			Return[{}];
		];
		instance=instance[[1]](*only 1 solution, take it*);
		additionalConditions=(#[[1]]-#[[2]])&/@Select[instance,MemberQ[{1,0,-1},#[[2]]]&];(*c are scaleless, we do not worry about some GenricPoint having s\[Rule]1 or similar*)
		ExtGB=GroebnerBasis[Join[ExtGB,additionalConditions],clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
		Switch[QuotientRingSize[ExtGB,clist],
		99999,
			WQPrintAndLog[
				"** Warning: contradiction between ExtGB and its instance solutions while finding MomentumMaps between the following propagators:","\n",
				prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
				"** The momentum map is not fixed by the instance searching. Giving up finding the corresponding symmetries."
			];
			Return[{}];
		,
		0,
			WQPrintAndLog[
				"** Warning: contradiction between ExtGB and its instance solutions while finding MomentumMaps between the following propagators:","\n",
				prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
				"** The momentum map is not consistent with the instance searching. Giving up finding the corresponding symmetries."
			];
			Return[{}];
		,
		_,
			sol=Solve[ExtGB==0,clist];
		]
	];
	
	If[sol=={},Return[{}]];
	For[i=1,i<=Length[sol],i++,
		DM=D[internal/.rep/.sol[[i]],{internal}];
		
		If[Factor[Det[DM]]^2=!=1,Continue[]];   (* Wrong Jacobi for internal momenta *)
		AppendTo[result,sol[[i]]];
	(*	leadingCoef=SortBy[ArrayRules[DM],#[[1]]&][[1,2]]; *)
	(* 	Print[Det[DM]," ",leadingCoef]; *)
		(* If[leadingCoef>0,AppendTo[result,sol[[i]]]];  *)  (* Gauge choice to remove unwanted ALL-MINUS transformations *)
	];
	(*Return[rep/.result];*)
	
	SortBy[rep/.result,ByteCount][[{1}]]
	
	(*{1} is to keep data structure same as before*)
	
];



(* ::Subsection:: *)
(*DeepMomentumMap*)


(* ::Subsubsection:: *)
(*reliaments*)


(*IntegralMomentaGroup[Internal_, External_, Propagators_]:=Module[{result,yList,yRep,y,i,ScalarPropagator,M={},indices,ComplementIndices,rep,backrep},
	yRep=Table[Internal[[i]]External[[j]]->y[i,j],{i,1,Length[Internal]},{j,1,Length[External]}]//Flatten;
	ScalarPropagator=Expand[Propagators]/.yRep;
	M=Reap[For[i=1,i<=Length[Internal],i++,
		Sow[D[ScalarPropagator,{Table[y[i,j],{j,1,Length[External]}]}]];
	
	];][[2]];
	M=RowReduce[Flatten[M,2]];
	ComplementIndices=Complement[Range[Length[External]],pivots[M]];
	result={Complement[M . External,{0}],External[[ComplementIndices]]};
	rep=Solve[Flatten[result]==groupMomentumU/@Range[Length[External]],External][[1]];
	backrep=MapThread[#1->#2&,{groupMomentumU/@Range[Length[External]],Flatten[result]}];
	Return[Join[result,{rep,backrep}]];
]

*)



IntegralMomentaGroup//ClearAll
(*Options[IntegralMomentaGroup]={Method->"MomentumSpace"}*)
Options[IntegralMomentaGroup]={Method->ExternalMomentaGroupingMethod}

IntegralMomentaGroup[Internal_, External_, Propagators_,OptionsPattern[]]:=Module[{result,yList,yRep,y,i,ScalarPropagator,M={},indices,ComplementIndices,rep,backrep},
	If[OptionValue[Method]==="FeynmanParameterization",
		Return[IntegralMomentaGroupFeynPar[Internal,External,Propagators]];
	,
		If[OptionValue[Method]=!="MomentumSpace",
			PrintAndLog["IntegralMomentaGroup: Unkown Method \""<>OptionValue[Method]<>"\". "];
			Return[$Failed];
		]
	];
	yRep=Table[Internal[[i]]External[[j]]->y[i,j],{i,1,Length[Internal]},{j,1,Length[External]}]//Flatten;
	ScalarPropagator=Expand[Propagators]/.yRep;
	M=Reap[For[i=1,i<=Length[Internal],i++,
		Sow[D[ScalarPropagator,{Table[y[i,j],{j,1,Length[External]}]}]];
	
	];][[2]];
	M=RowReduce[Flatten[M,2]];
	ComplementIndices=Complement[Range[Length[External]],pivots[M]];
	result={Complement[M . External,{0}],External[[ComplementIndices]]};
	rep=Solve[Flatten[result]==groupMomentumU/@Range[Length[External]],External][[1]];
	backrep=MapThread[#1->#2&,{groupMomentumU/@Range[Length[External]],Flatten[result]}];
	Return[Join[result,{rep,backrep}]];
]




devsetting20240624="ExtRREF";
(*
"ExtRREF"
or
"NS^2"(NS=nullspace)
the two are in principle equivalent
just in case.
*)
IntegralMomentaGroupFeynPar[Internal_, External_, Propagators_]:=Module[
{U,F,Gpol,xs,cr,coeffs,M,pos,complementIndices,RM,
groupNum,groupCoeffs,groups,rep,backrep,result,MNull
},
	{U,F,xs}=GenerateUF[Propagators,Internal,{}];
	Gpol=U+F;
	cr=CoefficientRules[Gpol,xs];
	coeffs=cr[[All,2]];
	M=Table[D[coeffs[[i]],External[[j]]],{i,Length[coeffs]},{j,Length[External]}];
	Switch[devsetting20240624,
	"NS^2",
		groupNum=MatrixRank[M];
		complementIndices=Complement[Range[Length[External]],pivots[RowReduce[M]]];
		MNull=M//NullSpace;
		(*find the basis that is ortho to M's null space*)
		If[MNull==={},
			groupCoeffs=IdentityMatrix[Length[External]];
		,
			groupCoeffs=MNull//NullSpace//RowReduce;
		];
		groups=#.External&/@groupCoeffs;
	,
	"ExtRREF",
		(*
		coordinatize the external momenta w.r.t. external momenta basis
		taken that the mandeston vars are quadratic
		*)
		M=Transpose[M];
		M=Flatten[
			Table[
				D[#[[i]],External[[j]]],
				{i,Length[#]},
				{j,Length[External]}
			]
		]&/@M;
		M=Transpose[M];
		RM=RowReduce[M];
		groups=DeleteCases[Expand[#.External]&/@RM,0];
		complementIndices=Complement[Range[Length[External]],pivots[RM]];
		groupNum=Length[groups];(*just a horse butt*)
	,
	_,
		PrintAndLog["IntegralMomentaGroupFeynPar: wrong devsetting20240624: ",devsetting20240624,"."];
		PrintAndLog["This is a bug. Please contact depeloper to fix this. Exiting..."];
		Exit[0];
	];
	If[Length[groups]=!=groupNum,
		PrintAndLog["IntegralMomentaGroupFeynPar: The groups found ",groups," mismatchs matrix rank ",groupNum];
		Return[$Failed]
	];
	result={groups,External[[complementIndices]]};
	If[Length[result//Flatten]=!=Length[External],
		PrintAndLog["IntegralMomentaGroupFeynPar: The number groups+complements ",result," mismatchs the number of external momenta ",Length[External]];
		Return[$Failed]
	];
	rep=Solve[Flatten[result]==groupMomentumU/@Range[Length[External]],External][[1]];
	backrep=MapThread[#1->#2&,{groupMomentumU/@Range[Length[External]],Flatten[result]}];
	Return[Join[result,{rep,backrep}]]
]


Options[ExtendedRotationByOrthogonalization]={BackupMethod->"None",ReportNotice->ReportNoticeInDeepMomentumMap};
ExtendedRotationByOrthogonalization[external_,vectors_,vectorsImage_,kinematics_,OptionsPattern[]]:=Module[
{d,n,vectorsC,vectorsImageC,basis1,basis2,MatrixB1,MatrixB2,MatrixS,gramdet1,gramdet2,GlobalMatrixT,GlobalETrans},
	d=Length[external];
	n=Length[vectors];
	(*If[vectors===vectorsImage,Return[IdentityMatrix[d]]];
	If[vectors===-vectorsImage,Return[-IdentityMatrix[d]]];*)
	
	If[vectors===vectorsImage,Return[#->#&/@external]];
	If[vectors===-vectorsImage,Return[#->-#&/@external]];
	
	gramdet1=Det[Expand[KroneckerProduct[vectors,vectors]]/.kinematics]//Factor;
	gramdet2=Det[Expand[KroneckerProduct[vectorsImage,vectorsImage]]/.kinematics]//Factor;
	
	If[Or[gramdet1===0,gramdet2===0],
		Switch[OptionValue[BackupMethod],
		"None",
			WQPrintAndLog[
				"** Warning: failed to find a extended rotation by orthogonalization between vectors:","\n",
				vectors//InputForm//ToString,"\n",vectorsImage//InputForm//ToString,"\n",
				"** Vanishing local Gram determinat. Giving up finding the corresponding symmetries."
			];
			Return[{}],
		"DeltaPlaneProjection",
			If[OptionValue[ReportNotice],
				PrintAndLog[
					"\t\t\t[Notice]: vanishing local Gram determinat in extending rotation by orthogonalization between vectors:","\n",
					"\t\t\t",vectors//InputForm//ToString,"\n","\t\t\t",vectorsImage//InputForm//ToString,"\n",
					"\t\t\t\t Using backup method DeltaPlaneProjection."
				]
			];
			Return[ExtendedRotationByDeltaPlaneProjection[external,vectors,vectorsImage,kinematics]]
		]
	];
	
	vectorsC=LinearComplement[external,vectors];
	vectorsImageC=LinearComplement[external,vectorsImage];
	basis1=OrthogonalComplement[external,vectors,vectorsC,kinematics];
	basis2=OrthogonalComplement[external,vectorsImage,vectorsImageC,kinematics];
	
	MatrixB1=CoefficientArrays[basis1,external][[2]];
	MatrixB2=CoefficientArrays[basis2,external][[2]];
	MatrixS=IdentityMatrix[d];
	
	Do[MatrixS[[i,i]]=Sqrt[Expand[basis1[[i]]^2]/Expand[basis2[[i]]^2]/.kinematics//Together],{i,n+1,d}];
	(*Return[Inverse[MatrixB1] . MatrixS . MatrixB2//Together];*)
	GlobalMatrixT=Inverse[MatrixB1] . MatrixS . MatrixB2//Together;
	GlobalETrans=MapThread[#1->#2&,{external,GlobalMatrixT . external}];
	(*PrintAndLog[GlobalETrans,external];(*debug20240626*)*)
	Collect[GlobalETrans,external,Factor]
	
];
LinearComplement[external_,vectors_]:=Module[{M},
	If[vectors=={},Return[external]];  
	M=RowReduce[CoefficientArrays[vectors,external][[2]]];
	Return[external[[Complement[Range[Length[external]],pivots[M]]]]];

];
LinearProjection[external_,vectors_,targetvector_,kinematics_]:=Module[{M,LocalGram},
	LocalGram=Expand[KroneckerProduct[vectors,vectors]]/.kinematics;  
	Return[(Expand[targetvector vectors]/.kinematics) . Inverse[LocalGram] . vectors]; 
];
Options[OrthogonalComplement]={Normalization->False};
OrthogonalComplement[external_,vectors_,vectorsComplement_,kinematics_,OptionsPattern[]]:=Module[{i,n,d,result,newVector},
	If[vectors=={},Return[external]]; (* In this case, do NOT try to find an orthogonal basis!*)
	n=Length[vectors];
	d=Length[external];
	result=vectors;
	For[i=n+1,i<=d,i++,
		newVector=vectorsComplement[[i-n]]-LinearProjection[external,result,vectorsComplement[[i-n]],kinematics]//Simplify;
		If[OptionValue[Normalization],newVector=newVector/Sqrt[Expand[newVector^2]/.kinematics//Factor]];
		AppendTo[result,newVector];
	];
	Return[result];
]



Options[ExtendedRotationByDeltaPlaneProjection]={BackupMethod->"None",ReportNotice->ReportNoticeInDeepMomentumMap};
ExtendedRotationByDeltaPlaneProjection[external_,vectors_,vectorsImage_,kinematics_,OptionsPattern[]]:=Module[
{vectorsDiff,vectorsDiffMatrix,indepIndices,ws,subGram,invSubGram,selectedVectors,projectionMatrixAPrime,transformMatrixB,
projectionMatrixA,rep,ind,p,pProjectionCoordinates,pImage
},
	vectorsDiff=vectorsImage-vectors;
	If[DeleteCases[Expand[vectorsDiff],0]==={},Return[#->#&/@external]];
	vectorsDiffMatrix=CoefficientArrays[vectorsDiff,external][[2]];
	indepIndices=pivots[vectorsDiffMatrix//Transpose//RowReduce];
	ws=vectorsDiff[[indepIndices]];
	selectedVectors=vectors[[indepIndices]];
	subGram=Factor[Expand[Table[ws[[i]]ws[[j]],{i,Length[ws]},{j,Length[ws]}]]/.kinematics];
	If[Factor[Det[subGram]]===0,
		Switch[OptionValue[BackupMethod],
		"None",
			WQPrintAndLog[
				"** Warning: failed to find a extended rotation by delta plane projection between vectors:","\n",
				vectors//InputForm//ToString,"\n",vectorsImage//InputForm//ToString,"\n",
				"** Vanishing local Gram determinat. Giving up finding the corresponding symmetries."
			];
			Return[{}],
		"Orthogonalization",
			If[OptionValue[ReportNotice],
				PrintAndLog[
					"\t\t\t[Notice]: vanishing local Gram determinat in extending rotation by delta plane projection between vectors:","\n",
					"\t\t\t",vectors//InputForm//ToString,"\n","\t\t\t",vectorsImage//InputForm//ToString,"\n",
					"\t\t\t\tUsing backup method Orthogonalization."
				]
			];
			Return[ExtendedRotationByOrthogonalization[external,vectors,vectorsImage,kinematics]]
		]
	];
	
	invSubGram=Factor[Inverse[subGram]];
	projectionMatrixA=Table[Expand[selectedVectors[[i]]ws[[j]]]/.kinematics,{i,Length[ws]},{j,Length[ws]}].invSubGram;
	projectionMatrixAPrime=projectionMatrixA+IdentityMatrix[Length[ws]];
	transformMatrixB=Factor[projectionMatrixAPrime.Inverse[projectionMatrixA]];
	rep=Table[0,Length[external]];
	For[ind=1,ind<=Length[external],ind++,
		p=external[[ind]];
		pProjectionCoordinates=Table[Expand[p ws[[i]]]/.kinematics,{i,Length[ws]}].invSubGram;
		pImage=p-pProjectionCoordinates.ws+pProjectionCoordinates.transformMatrixB.ws;
		rep[[ind]]=p->Collect[pImage,external,Factor]
	];
	If[Union[Expand[vectors/.rep]-vectorsImage]=!={0},
		Switch[OptionValue[BackupMethod],
		"None",
			WQPrintAndLog[
				"** Warning: failed to find a extended rotation by delta plane projection.","\n",
				"** Inconsistent delta plane transformation ", rep," between vectors:\n",
				vectors//InputForm//ToString,"\n",vectorsImage//InputForm//ToString,
				"Giving up finding the corresponding symmetries."
			];
			Return[{}],
		"Orthogonalization",
			If[OptionValue[ReportNotice],
				PrintAndLog[
					"\t\t\t[Notice]: Inconsistent delta plane transformation ", rep," resulted when finding extended rotation by delta plane projection between vectors:\n",
					"\t\t\t",vectors//InputForm//ToString,"\n","\t\t\t",vectorsImage//InputForm//ToString,"\n",
					"\t\t\t\tUsing backup method Orthogonalization."
				]
			];
			Return[ExtendedRotationByOrthogonalization[external,vectors,vectorsImage,kinematics]]
		]
	];
	rep
	(*Table[D[(external[[i]]/.rep),external[[j]]],{i,Length[external]},{j,Length[external]}]//Factor*)
]


(* ::Subsubsection:: *)
(*DeepMomentumMap*)


Options[DeepMomentumMap]={ReportNotice->ReportNoticeInDeepMomentumMap}
DeepMomentumMap[internal_,external_,prop1_,prop2_,kinematics_,OptionsPattern[]]:=Module[{allvectors,group1,group1C,group2,group2C,rep1,backrep1,rep2,backrep2,LocalGram1,
LocalGram2,MatrixT,localE,MatrixA,MatrixB,Localprop1,Localprop2,LocalETrans,LocalLTrans,Trans,PropEqns,LocalKinematics1,LocalKinematics2,LocalGramEqns,
LocalGB,clist,sol,GlobalMatrixT,GlobalETrans,a,b,c,i,DM,sol1,instance,additionalConditions,
LocalEqns,nLocalGB,result
},

(*   Find the symmetry using the actual momenta*)
	(*Print["DeepMomentumMap: ",prop1," ",prop2];*)
	{group1,group1C,rep1,backrep1}=IntegralMomentaGroup[internal, external, prop1];
	{group2,group2C,rep2,backrep2}=IntegralMomentaGroup[internal, external, prop2]/.groupMomentumU->groupMomentumV;
	If[Length[group1]=!=Length[group2],
		Return[{}];(*this may have problems in massive tadepole, we expect these tadepoles will not need DeepMomentumMap... but not guaranteed. 2023.05.08: we have endeed encountered this in lxb3 {1,4,6}\[Rule]{1,5,6}, and MomentumMap does not find such a symmetry.*)
	];
	localE=group1//Length;
	Localprop1=prop1/.rep1//Together;
	Localprop2=prop2/.rep2//Together;
	
	
	LocalGram1=Expand[KroneckerProduct[group1,group1]]/.kinematics;
	LocalGram2=Expand[KroneckerProduct[group2,group2]]/.kinematics;
	LocalKinematics1=Table[groupMomentumU[i]groupMomentumU[j]->LocalGram1[[i,j]],{i,1,localE},{j,1,localE}]//Flatten;
	LocalKinematics2=Table[groupMomentumV[i]groupMomentumV[j]->LocalGram2[[i,j]],{i,1,localE},{j,1,localE}]//Flatten;
	
	
	MatrixA=Table[a[i,j],{i,1,internal//Length},{j,1,internal//Length}];
	MatrixB=Table[b[i,j],{i,1,internal//Length},{j,1,localE}];
	MatrixT=Table[c[i,j],{i,1,Length[group1]},{j,1,Length[group1]}];
	LocalETrans=MapThread[#1->#2&,{groupMomentumU/@Range[localE],MatrixT . (groupMomentumV/@Range[localE])}];
	LocalLTrans=MapThread[#1->#2&,{internal, MatrixA . internal+MatrixB . (groupMomentumV/@Range[localE])}];
	Trans=Join[LocalETrans,LocalLTrans];
	
	PropEqns=Expand[(Localprop1/.Trans)-Localprop2]/.LocalKinematics2;
	PropEqns=#[[2]]&/@Flatten[CoefficientRules[PropEqns,Join[internal,(groupMomentumV/@Range[localE])]]];
	LocalGramEqns=Flatten[LocalGram1-MatrixT . LocalGram2 . Transpose[MatrixT]]; 
	clist=Variables[{MatrixA,MatrixB,MatrixT}];
	LocalEqns=Join[PropEqns,LocalGramEqns];
	
	nLocalGB=GroebnerBasis[LocalEqns/.GenericPoint,clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
	
	Switch[QuotientRingSize[nLocalGB,clist],
	99999,
		TimeConstrained[
			instance=FindInstance[nLocalGB==0/.GenericPoint,clist];
			If[instance=={},
				WQPrintAndLog[
					"** Warning: continuous solution found in DeepMomentumMaps between the following propagators:","\n",
					prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
					"** Momenta groups:","\n",
					group1//InputForm//ToString,"\n",group2//InputForm//ToString,"\n",
					"** No instance found.","\n",
					"** Giving up finding the corresponding symmetries."
				];
				Return[{}];
			];
			If[MemberQ[(Element[#,Rationals]===True)&/@Factor[instance[[1,All,2]]],False],
				instance=FindInstance[nLocalGB==0/.GenericPoint,clist,Rationals];
			];
			If[instance=={},
				WQPrintAndLog[
					"** Warning: continuous solution found in DeepMomentumMaps between the following propagators:","\n",
					prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
					"** Momenta groups:","\n",
					group1//InputForm//ToString,"\n",group2//InputForm//ToString,"\n",
					"** No rational instance found.","\n",
					"** Giving up finding the corresponding symmetries."
				];
				Return[{}];
			];
			instance=instance[[1]](*only 1 solution, take it*);
			
			
			additionalConditions=(#[[1]]-#[[2]])&/@Select[instance,MemberQ[{1,0,-1},#[[2]]]&];(*c are scaleless, we do not worry about some GenricPoint having s\[Rule]1 or similar*)
			
			(*probe2023=LocalGB;(*debug2023*)*)
			LocalGB=GroebnerBasis[Join[LocalEqns,additionalConditions],clist,Sort->True,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
			LocalGB=GroebnerBasis[LocalGB,clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
			Switch[QuotientRingSize[LocalGB,clist],
			99999,
				WQPrintAndLog[
					"** Warning: continuous solution found in DeepMomentumMaps between the following propagators:","\n",
					prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
					"** Momenta groups:","\n",
					group1//InputForm//ToString,"\n",group2//InputForm//ToString,"\n",
					"** The momentum map is not fixed by the instance solution.","\n",
					"** Giving up finding the corresponding symmetries."
				];
				Return[{}];
			,
			0,
				WQPrintAndLog[
					"** Warning: continuous solution found in DeepMomentumMaps between the following propagators:","\n",
					prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
					"** Momenta groups:","\n",
					group1//InputForm//ToString,"\n",group2//InputForm//ToString,"\n",
					"** The momentum map is not consistent with the instance solution.","\n",
					"** Giving up finding the corresponding symmetries."
				];
				(*Export["instance.txt",{probe2023,additionalConditions,instance}//InputForm//ToString//ToString];(*debug2023*)*)
				Return[{}];
			,
			_,
				sol=Solve[LocalGB==0,clist];
			]
		,
			MomentumMapTimeConstraint
		,
			WQPrintAndLog[
				"** Warning: continuous solution found in DeepMomentumMaps between the following propagators:","\n",
				prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
				"** Momenta groups:","\n",
				group1//InputForm//ToString,"\n",group2//InputForm//ToString,"\n",
				"** Evaluation using FindInstance timed out.","\n",
				"** Giving up finding the corresponding symmetries."
			];
			Return[{}];
		]
	,
	0,
		WQPrintAndLog[
			"** Warning: no solution found in DeepMomentumMaps between the following propagators:","\n",
			prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
			"** Momenta groups:","\n",
			group1//InputForm//ToString,"\n",group2//InputForm//ToString,"\n",
			"** Giving up finding the corresponding symmetries."
		];
		Return[{}];
	,
	_,
		
		TimeConstrained[
			LocalGB=GroebnerBasis[LocalEqns,clist,Sort->True,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
			LocalGB=GroebnerBasis[LocalGB,clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
			sol=Solve[LocalGB==0,clist];
		,
			MomentumMapTimeConstraint
		,
			WQPrintAndLog[
				"** Warning: solving equations timed out in DeepMomentumMaps between the following propagators:","\n",
				prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
				"** Momenta groups:","\n",
				group1//InputForm//ToString,"\n",group2//InputForm//ToString,"\n",
				"** Giving up finding the corresponding symmetries."
			];
			Return[{}];
		]
	];
	
	If[sol=={},Return[{}]];
	sol1={};
	For[i=1,i<=Length[sol],i++,
		DM=D[internal/.LocalLTrans/.sol[[i]]/.backrep2,{internal}];
		If[Factor[Det[DM]]^2=!=1,Continue[]];   (* Wrong Jacobi for internal momenta *)
		AppendTo[sol1,sol[[i]]];
	];
	sol=sol1;
	
	If[sol=={},Return[{}]];
	sol=SortBy[sol,ByteCount[Expand[Trans/.#]]&][[1]];
	
	(* Print[group1,(groupMomentumU/@Range[localE])/.LocalETrans/.sol/.backrep2]; *);
	(*GlobalMatrixT=ExtendedRotationByOrthogonalization[external,group1,(groupMomentumU/@Range[localE])/.LocalETrans/.sol/.backrep2,kinematics];
	GlobalETrans=MapThread[#1->#2&,{external,GlobalMatrixT . external}];*)
	
	Switch[PreferedExternalExtendedRotationMethod,
	"Orthogonalization",
		GlobalETrans=ExtendedRotationByOrthogonalization[external,group1,(groupMomentumU/@Range[localE])/.LocalETrans/.sol/.backrep2,kinematics,BackupMethod->"DeltaPlaneProjection"],
	"DeltaPlaneProjection",
		GlobalETrans=ExtendedRotationByDeltaPlaneProjection[external,group1,(groupMomentumU/@Range[localE])/.LocalETrans/.sol/.backrep2,kinematics,BackupMethod->"Orthogonalization"]
	];
	
	
	
	
	If[GlobalETrans==={},Return[{}]];
	result={Join[GlobalETrans,LocalLTrans/.sol/.backrep2]};(*{Join[...]} is to keep data structure same as before*)
	
	If[(result=!={})&&OptionValue[ReportNotice],
		PrintAndLog[
			"\t\t\t[Notice]: Symmetry found using DeepMomentumMaps between the following propagators","\n",
			"\t\t\t",prop1//InputForm//ToString,"\n\t\t\t",prop2//InputForm//ToString
		];
	];
	
	Return[result];
	
]


(* ::Subsection:: *)
(*DeeperMomentumMap*)


(* Only for quadratic propagators *)
NicePropagators[internal_,external_,prop1_,kinematics_]:=Module[{quadraticProp,\[Lambda],LoopFlow,LoopFlowMatrix,ll,SimpleIndepLoopMomenta,InterL,
rep1,rep2={},backrep1,backrep2={},L,Flow,InterProp,i,LocalInterProp,LocalFlow,LocalExternalFlow,shortestLocalFlow,shortestLocalFlows},
	quadraticProp=SeriesCoefficient[prop1/.MapThread[#1->#2&,{internal,\[Lambda] internal}],{\[Lambda],0,2}]//Factor;
	L=Length[internal];
	LoopFlow=quadraticProp/.Power[x_,2]:>x;(*wrong but work if propagator is defined like -(l+p)^2+m^2*)
	LoopFlowMatrix=CoefficientArrays[LoopFlow,internal][[2]]//Normal;
	LoopFlowMatrix=SortBy[LoopFlowMatrix,{Norm[#],Reverse[#]}&];
	SimpleIndepLoopMomenta=LoopFlowMatrix[[pivots[RowReduce[Transpose[LoopFlowMatrix]]]]] . internal;
	If[Length[SimpleIndepLoopMomenta]<L,Return["Failed"];];
	backrep1=MapThread[#1->#2&,{InterL/@Range[L],SimpleIndepLoopMomenta}];
	rep1=Solve[(InterL/@Range[L])-SimpleIndepLoopMomenta==0,internal][[1]];
	(*Flow=(SeriesCoefficient[prop1/.MapThread[#1->#2&,{Join[internal,external],\[Lambda] Join[internal,external]}],{\[Lambda],0,2}]//Factor)/.Power[x_,2]:>x;*)
	InterProp=prop1/.rep1;
	Flow=(SeriesCoefficient[InterProp/.MapThread[#1->#2&,{Join[InterL/@Range[L],external],\[Lambda] Join[InterL/@Range[L],external]}],{\[Lambda],0,2}]//Factor)/.Power[x_,2]:>x;
	shortestLocalFlows={};
	For[i=1,i<=L,i++,
		(*LocalInterProp=Select[InterProp,Intersection[Variables[#],InterL/@Range[L]]=={InterL[i]}&];
		If[LocalInterProp==={},Return["Failed"]];
		LocalExternalFlow=SortBy[D[LocalInterProp,InterL[i]]/2/.InterL[i]->0,ByteCount];
		AppendTo[rep2,InterL[i]->StdL[i]-LocalExternalFlow[[1]]];
		AppendTo[backrep2,StdL[i]->InterL[i]+LocalExternalFlow[[1]]];*)
		LocalFlow=Select[Flow,Intersection[Variables[#],InterL/@Range[L]]=={InterL[i]}&];
		If[LocalFlow==={},Return["Failed"]];
		shortestLocalFlow=SortBy[LocalFlow,ByteCount[#/.InterL[i]->0]&][[1]];
		AppendTo[shortestLocalFlows,shortestLocalFlow]
	];
	backrep2=Table[StdL[i]->shortestLocalFlows[[i]],{i,Length[shortestLocalFlows]}];
	rep2=Solve[
		Table[StdL[i]==shortestLocalFlows[[i]],{i,Length[shortestLocalFlows]}]
	,InterL/@Range[L]][[1]];
	
	Return[{rep1/.rep2,backrep2/.backrep1}//Expand];
]


Options[DeeperMomentumMap]={ReportNotice->ReportNoticeInDeepMomentumMap}
DeeperMomentumMap[internal_,external_,prop1_,prop2_,kinematics_,OptionsPattern[]]:=Module[{nice1,nice2,rep1,backrep1,rep2,backrep2,L,trans,CheckPoint1,CheckPoint2,result},
	nice1=NicePropagators[internal,external,prop1,kinematics];
	nice2=NicePropagators[internal,external,prop2,kinematics];
	If[nice1=="Failed"||nice2=="Failed",
		WQPrintAndLog[
			"** Warning: nice form of the propagators not found in DeeperMomentumMaps between the following propagators:","\n",
			prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
			"** Giving up finding the corresponding symmetries."
		];
		Return[{}];
	];
	L=Length[internal];
	{rep1,backrep1}=nice1//Expand;
	{rep2,backrep2}=nice2//Expand;
	(*Print[prop1/.rep1//Factor,prop2/.rep2//Factor];*)
	
	trans=DeepMomentumMap[StdL/@Range[L],ExternalMomenta,prop1/.rep1//Factor,prop2/.rep2//Factor,Kinematics,ReportNotice->False];
	If[trans=={},Return[{}]];
	trans=trans[[1]];(*data structure difference form original codes from YZ*)
	
	CheckPoint1=prop2-(prop1/.rep1/.trans/.backrep2)//Factor;
	
	If[Union[CheckPoint1]=!={0},
		WQPrintAndLog[
			"** Warning: wrong NicePropagator transformation in DeeperMomentumMaps between the following propagators:","\n",
			prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
			"** Giving up finding the corresponding symmetries."
		];
		Print[trans];
		Return[{}];
	];
	
	result=Collect[MapThread[#1->#2&,{Join[internal,external],Join[internal,external]/.rep1/.trans/.backrep2}],Join[internal,external],Factor];
	CheckPoint2=D[internal/.result,{internal}]//Det//Factor;

	If[Abs[CheckPoint2]=!=1,
		WQPrintAndLog[
			"** Warning: resulting transformation in DeeperMomentumMaps is of Jacobian unequal to 1 or -1 between the following propagators:","\n",
			prop1//InputForm//ToString,"\n",prop2//InputForm//ToString,"\n",
			"** Giving up finding the corresponding symmetries."
		];
		Return[{}];
	];
	If[(result=!={})&&OptionValue[ReportNotice],
		PrintAndLog[
			"\t\t\t[Notice]: Symmetry found using DeeperMomentumMaps between the following propagators","\n",
			"\t\t\t",prop1//InputForm//ToString,"\n\t\t\t",prop2//InputForm//ToString
		];
	];
	result={result};(*data structure difference form original codes from YZ*)
	Return[result];
]


(* ::Subsection:: *)
(*backups *)


(* MomentumMap just find one solution; YZ, 4.16.2023*)
(* already added to the function above*)
(*Print["******Err: MomentumMap Redefined**********"]
Quit[];
MomentumMap[internal_,external_,prop1_,prop2_,kinematics_]:=Module[{allvectors,a,b,c,A,B,T,rep1,rep2,rep,prop1trans,prop2expand,eqns,formula,
clist,GB,sol,instance,additionalConditions,kinematicVar,Exteqns,ExtGB},
     allvectors=Join[internal,external];
     A=Table[a[i,j],{i,1,internal//Length},{j,1,internal//Length}];
     B=Table[b[i,j],{i,1,internal//Length},{j,1,external//Length}];
     T=Table[c[i,j],{i,1,external//Length},{j,1,external//Length}];
     rep1=MapThread[#1->#2&,{external,T . external}];
     rep2=MapThread[#1->#2&,{internal,A . internal+B . external}];
     rep=Join[rep1,rep2];
     prop1trans=Expand[prop1/.rep]/.kinematics;
     prop2expand=Expand[prop2]/.kinematics;

  eqns=#[[2]]&/@Flatten[CoefficientRules[#,allvectors]&/@(prop1trans-prop2expand)];
  eqns=Join[eqns,Table[(Expand[kinematics[[i,1]]/.rep1]/.kinematics)-kinematics[[i,2]],{i,1,kinematics//Length}]];
  clist=Select[Variables[eqns],Head[#]==a||Head[#]==b||Head[#]==c&];
     kinematicVar=Complement[Variables[eqns],clist];
  Exteqns=#[[2]]&/@Flatten[CoefficientRules[#,kinematicVar]&/@Complement[eqns,{0}]]; (* First to test if there is a kinematic var independent solution *)

  ExtGB=GroebnerBasis[Exteqns,clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
     If[ExtGB=!={1},
         sol=Solve[ExtGB==0,clist];
         Return[SortBy[rep/.sol,ByteCount][[1]]];
     ];

  GB=GroebnerBasis[eqns,clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
     If[GB=={1},Print["momentum transformation not found ..."];Return[{}]];
     If[QuotientRingSize[GB,clist]<99999,
         sol=Solve[GB==0,clist];
         Return[SortBy[rep/.sol,ByteCount][[1]]];
         ,

  instance=FindInstance[GB==0/.GenericPoint,clist,Rationals];
         (* 
If[instance=={},instance=FindInstance[GB==0/.GenericPoint,clist,Rationals];]; 
*)
         If[instance=={},Return[];];
         instance=instance[[1]];
additionalConditions=(#[[1]])&/@Select[instance,#[[2]]===0&];
GB=GroebnerBasis[Join[GB,additionalConditions],clist,MonomialOrder->DegreeReverseLexicographic,CoefficientDomain->RationalFunctions];
         Switch[QuotientRingSize[GB,clist],
             99999,
             Print["The momentum map is not fixed by the instance 
searching ..."];
             Return[{}];,
             0,
             Print["The momentum map is not consistent with the instance 
..."];
             Return[{}];,
             _,
             sol=Solve[GB==0,clist];
             Return[SortBy[rep/.sol,ByteCount][[1]]];
             ];
     ];


];*)
