(* ::Package:: *)

(*Print["Pak algorithm implemented by Yang Zhang. 03.18.2022"];*)


Print["Pak algorithm included."]


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

