(* ::Package:: *)

(*
This is a package to generate syzygy for modules that is linear in all its variables
version2 is going to support monomial orderring
added smart mode
*)
LinearSyz`SortCoefficients=True




LinearSyz::usage="test text"


(* ::Subsection::Closed:: *)
(*control pad*)


(*nothing here*)


(* ::Subsection:: *)
(*code*)


CoefficientOddering[c_]:=Module[{position=c[[1]],term=c[[2]]},
	Return[{term,position}]
]


<<FiniteFlow`


SPrint[x___]:=If[!silence,PrintAndLog[x]]


silence=!True;


LinearQ[expr_,variables_]:=Module[{scale},Expand[expr*scale-(expr/.(#->scale*#&/@variables))]===0]






CoefficientEncode[position_,indices_]:=c[ToString[position]<>"p"<>StringRiffle[ToString/@indices,"p"]]
CoefficientDecode[string_]:=Module[{ss=StringSplit[string,"p"]},c[ToExpression[ss[[1]]],ToExpression/@(ss[[2;;-1]])]]
(*we assume the indices are non-negative!*)


PolynomialDegree::inhomogeneous="The polynomial `1` is not homogeneous for variables `2`.";
PolynomialDegree[polynomial_,variables_]:=Module[{scale,degree,scaledPolynomial},
	scaledPolynomial=polynomial/.(#->scale*#&/@variables);
	degree=Exponent[scaledPolynomial,scale];
	If[Expand[scaledPolynomial-If[degree==-Infinity,0,polynomial*scale^degree]]=!=0,Message[PolynomialDegree::inhomogeneous,polynomial,variables];Return[$Failed]];
	Return[degree]
	
]


(*This function is for function DegreedMonomials*)
(*monomial order is degreed lexicographic. Keep this in the whole package!*)
DistributePowerIndices[variableNumber_,degree_]:=Module[{seperatorPositions,seperatorPositionsWithBoundaries,result},
	seperatorPositions=Subsets[Range[variableNumber+degree-1],{variableNumber-1}];
	seperatorPositionsWithBoundaries=Join[{0},#,{variableNumber+degree}]&/@seperatorPositions;
	result=Table[#[[i+1]]-#[[i]]-1,{i,Length[#]-1}]&/@seperatorPositionsWithBoundaries;
	Return[result]
]
(*This function is for function DegreedMonomials*)
IndicesToMonomials[variables_,indicies_]:=Inner[Power,variables,indicies,Times]
MonomialToIndices[monomial_,variables_]:=Exponent[monomial,#]&/@variables(*This will ignore the const. factor!*)

DegreedMonomials[variables_,degree_]:=Module[{variableNumber,powerIndicies,result},
	(*If[degree===-Infinity,Return[{}]];*)
	variableNumber=Length[variables];
	powerIndicies=DistributePowerIndices[variableNumber,degree];
	result=IndicesToMonomials[variables,#]&/@powerIndicies;
	Return[result]
]
ListOfDegreedMonomials[variables_,degreeList_]:=Module[{listUnion,degreedMonomialMembers,positions,result},
	listUnion=Union[degreeList];
	degreedMonomialMembers=DegreedMonomials[variables,#]&/@listUnion;
	positions=Flatten[Position[listUnion,#]&/@degreeList];
	result=degreedMonomialMembers[[positions]];
	Return[result]
]
GenerateSyzAnsatzAndCoefficients[variables_,degreeList_]:=Module[{listUnion,variableNumber,powerIndiciesMembers,preAnsatzMembers,positions,powerIndicies,result,ansatz,coefficients},
	listUnion=Union[degreeList];
	variableNumber=Length[variables];
	powerIndiciesMembers=DistributePowerIndices[variableNumber,#]&/@listUnion;
	positions=Flatten[Position[listUnion,#]&/@degreeList];
	powerIndicies=powerIndiciesMembers[[positions]];
	ansatz=Table[Total[c[i,#]IndicesToMonomials[variables,#]&/@(powerIndicies[[i]])],{i,Length[degreeList]}];
	If[LinearSyz`SortCoefficients===True,
		coefficients=SortBy[Flatten[Table[c[i,#]&/@(powerIndicies[[i]]),{i,Length[degreeList]}]],CoefficientOddering];
		,
		coefficients=Flatten[Table[c[i,#]&/@(powerIndicies[[i]]),{i,Length[degreeList]}]];
	];
	(*Sorting costs time consideratelly!*)
	result={ansatz,coefficients};
	Return[result/.c->CoefficientEncode]
]


ClearAll[PolynomialCoefficients]
PolynomialCoefficients[poly_,degreedMonomials_]:=Module[{mo,polyHandled,result},
	If[degreedMonomials==={1},Return[{poly}]];
	polyHandled=Expand[poly]/.(#->mo[#]&/@degreedMonomials);
	result=Table[D[polyHandled,mo[degreedMonomials[[k]]]],{k,Length[degreedMonomials]}];
	Return[result]
](*this function may be slow!*)


GenerateSyzForHigerDegree[syz_,variables_,raisedDegree_]:=Module[{degreedMonomials,result},
	degreedMonomials=DegreedMonomials[variables,raisedDegree];
	result=Table[syz*degreedMonomials[[i]],{i,Length[degreedMonomials]}];
	Return[result]
]
PolynomialVectorDegrees[vector_,variables_]:=PolynomialDegree[#,variables]&/@vector
GenerateSyzForGivenDegree//ClearAll
GenerateSyzForGivenDegree[syz_,variables_,degree_,syzDegreeShift_]:=Module[{syzDegree,syzVectorDegrees,preSyzDegree},
	syzVectorDegrees=PolynomialVectorDegrees[syz,variables];
	preSyzDegree=DeleteCases[Union[syzVectorDegrees-syzDegreeShift],-Infinity];
	If[Length[preSyzDegree]!=1,Print["Err in GenerateSyzForGivenDegree: the syzygy is not uniform degreed or it is all 0."];Return[$Failed]];
	syzDegree=preSyzDegree[[1]];
	Return[GenerateSyzForHigerDegree[syz,variables,degree-syzDegree]]
]
(*the syzygy degree is defined as the maximum of degrees of the components OF THE CORRESPONDING SYZYGY ANSATZ*)
(*the module degree is defined as the minimum of degrees of the components*)


(*SyzVector[syz_]:=Flatten[syz/.PolyInfo->(#3&)]*)


Options[SimplifyLinearlyIndependency]={}
SimplifyLinearlyIndependency[matrix_,OptionsPattern[]]:=Module[{rowReducedMatrix,positions,headPositions,result},
	If[matrix==={},Return[{}]];
	rowReducedMatrix=RowReduceFunction[matrix];
	positions=DeleteCases[Position[#,1]&/@rowReducedMatrix,{}];
	headPositions=Flatten[#[[1]]&/@positions];
	result=Table[Boole[#==i],{i,Length[matrix[[1]]]}]&/@headPositions;
	Return[SparseArray[result]](*This part should be rewritten in the next versions! Only sparse array is allowed!*)
]


ListRegroup[list_,numbers_]:=Module[{summedNumbers,result},
	If[Total[numbers]!=Length[list],Print["warning in ListRegroup. The total number does not match the length of the list"]];
	summedNumbers=Join[{0},Accumulate[numbers]];
	result=Table[list[[summedNumbers[[i]]+1;;summedNumbers[[i+1]]]],{i,Length[numbers]}];
	Return[result]
]






ClearAll[ConstrainEquationsFromSyz]
ConstrainEquationsFromSyz[syz_,variables_]:=Module[{crs,monoListsIndices,reps,result},
	crs=CoefficientRules[#,variables]&/@syz;
	result=Sum[(crs[[i]][[All,2]]).(c[i,#]&/@crs[[i]][[All,1]]),{i,Length[crs]}];
(*	expandedSyz=Expand[syz];
	monoListsIndices=SortBy[Union[MonomialToIndices[#,variables]&/@DeleteCases[Flatten[MonomialList[expandedSyz]],0]],-Total];
	(*indices of mono. ever appeared :{{ind1},{ind2},...}*)
	If[MemberQ[monoListsIndices,Table[0,Length[variables]]],Print["ConstrainEquationsFromSyz Warning: constant monomial occurred in syz! It shouldn`t be here!"]];
	reps=Table[(IndicesToMonomials[variables,#]\[Rule]c[i,#])&/@monoListsIndices,{i,Length[syz]}];
	prob={reps,expandedSyz};
	result=Sum[expandedSyz[[i]]/.reps[[i]],{i,Length[expandedSyz]}];*)
	Return[result/.c->CoefficientEncode]
	
]


ClearAll[ReducedConstrainEquationsFromSLD]
ReducedConstrainEquationsFromSLD[SLD_,variables_,coefficients_]:=Module[{constrainEquations,(*constrainEquationsEncoded,coefficientsEncoded,*)solution,timer=AbsoluteTime[]},
	If[SLD==={},Return[{}]];
	constrainEquations=ConstrainEquationsFromSyz[#,variables]&/@SLD;
	
	SPrint["\t ReducedConstrainEquationsFromSLD timer1:",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];
(*	constrainEquationsEncoded=constrainEquations/.c\[Rule]CoefficientEncode;
	coefficientsEncoded=coefficients/.c\[Rule]CoefficientEncode;*)
	(*solution=FFSparseSolve[(#==0)&/@constrainEquations,Reverse[Variables[constrainEquations]]];*)
	(*We assume the coefficients from the outside are consistent with Variables[constrainEquations], otherwise there might be something wrong*)
	(*we also assume that the increasing of irrelevant coefficients will not make the computationm too slow*)
	(*probe={constrainEquations,coefficients};*)
	SPrint["\t Start FFSparseSolve"];
	SPrint["\t equations: ",Length[constrainEquations]];
	SPrint["\t bytecount: ",ByteCount[constrainEquations]];
	
	solution=FFSparseSolve[(#==0)&/@constrainEquations,Reverse[coefficients]];
	(*why reversing? 
	this is because that, while solving the null space we prefer that coefficients from the beginning  survive,
	but here, we are killing some coefficients, so we choose those victims from the end.
	*)
	SPrint["\t ReducedConstrainEquationsFromSLD timer2:",AbsoluteTime[]-timer];
	Return[solution[[All,1]]]
	
	
]


(*original idealVectorDegrees is replaced by moduleDegreeList*)
ClearAll[SolveDegreedSyz]
Options[SolveDegreedSyz]={}
SolveDegreedSyz[module_,moduleDegreeList_,variables_,degree_,lowerDegreeSyzSolutions_,OptionsPattern[]]:=Module[
{(*moduleDegreeList,*)moduleDegree,syzDegreeShift,syzDegrees,syzFromLowerDegrees,dmList,coefficients,syzAnsatz,SLDVectors,syzEqs,
coefficientEquations,coefficientSolution,regroupedNullSpace,syzSolutions,syzSolutionsList,freeCoefficients,
equationsFromSLD,timer,allEquations},
	
	timer=AbsoluteTime[];
	(*moduleDegreeList=ModuleDegrees[module,variables];*)
	
	moduleDegree=Min[moduleDegreeList];
	syzDegreeShift=moduleDegree-moduleDegreeList;
	syzDegrees=degree+syzDegreeShift;
	(*the module degree is defined as the maximum of degrees of the components*)
	
	SPrint["1\t",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];
	
	
	syzFromLowerDegrees=Flatten[GenerateSyzForGivenDegree[#,variables,degree,syzDegreeShift]&/@lowerDegreeSyzSolutions,1];(*SLD*)
	
	SPrint["2\t",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];	
	
(*	SLDVectors=SyzVector/@syzFromLowerDegrees;
	
	SPrint[AbsoluteTime[]-timer];
	timer=AbsoluteTime[];	*)
	

	(*dmList=ListOfDegreedMonomials[variables,syzDegrees];	
	coefficients=Flatten[Table[Table[c[i,k],{k,Length[dmList[[i]]]}],{i,Length[module]}]];
	syzAnsatz=Table[Sum[c[i,k] dmList[[i,k]],{k,Length[dmList[[i]]]}],{i,Length[module]}];*)
	
	{syzAnsatz,coefficients}=GenerateSyzAnsatzAndCoefficients[variables,syzDegrees];
	

	(*coefficients=Select[Variables[syzAnsatz],Head[#]\[Equal]c&];*)
	
	
	SPrint["3\t",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];
	

	
	syzEqs=syzAnsatz.module//Expand;
	
	SPrint["4\t",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];
	
	coefficientEquations=Flatten[CoefficientRules[#,variables][[All,2]]&/@syzEqs];
	(*coefficientMatrix=CoefficientArrays[Flatten[CoefficientRules[#,variables][[All,2]]&/@syzEqs],coefficients][[2]];*)
	SPrint["5\t",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];
	
	(*simplifiedLinearlyIndependency=SimplifyLinearlyIndependency[SLDVectors];*)
	equationsFromSLD=ReducedConstrainEquationsFromSLD[syzFromLowerDegrees,variables,coefficients];
	
	SPrint["6\t",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];
	
	allEquations=Join[equationsFromSLD,coefficientEquations];
	coefficientSolution=FFSparseSolve[(#==0)&/@allEquations,coefficients];
	(*c should be better encoded here,... I will do this soon...*)
	(*DO THIS SOON!!!THIS IS VERY IMPORTANT! OR IT WILL BE SLOW,VERY SLOW!*)
	
	SPrint["7\t",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];
	
	(*regroupedNullSpace=ListRegroup[#,Table[Length[dmList[[i]]],{i,Length[dmList]}]]&/@nullSpace;*)
	(*syzSolutions=Table[#[[i]].dmList[[i]],{i,Length[dmList]}]&/@regroupedNullSpace;*)
	(*syzSolutions=Table[PolyInfo[variables,syzDegrees[[i]],#[[i]]],{i,Length[syzDegrees]}]&/@regroupedNullSpace;*)
	syzSolutions=syzAnsatz/.Dispatch[coefficientSolution];
	
	freeCoefficients=Select[Variables[syzSolutions],Head[#]==c&];
	SPrint["8\t",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];
	
	(*syzSolutionsList=Table[syzSolutions/.Select[(#->0)&/@freeCoefficients,#[[1]]=!=freeCoefficients[[i]]&]/.(freeCoefficients[[i]]->1),{i,Length[freeCoefficients]}];*)
	
	syzSolutionsList=Table[D[syzSolutions,freeCoefficients[[i]]],{i,Length[freeCoefficients]}];
	SPrint["9\t",AbsoluteTime[]-timer];
	timer=AbsoluteTime[];
	
	Return[Join[lowerDegreeSyzSolutions,syzSolutionsList]]
]


ClearAll[LinearSyz]
Options[LinearSyz]={SyzNumberBound->Infinity}
LinearSyz[module_,moduleDegreeList_,variables_,degreeBound_,OptionsPattern[]]:=Module[{i,result,timer},
	result={};
	For[i=0,i<=degreeBound,i++,
		timer=AbsoluteTime[];
		result=SolveDegreedSyz[module,moduleDegreeList,variables,i,result];
		Print["Syz solved at degree ",i,". Found ",Length[result]," syzs in total. Time used: ",AbsoluteTime[]-timer,"s."];
		(*Print["Syz: ",result]*);
		If[Length[result]>=OptionValue[SyzNumberBound],Break[]]
	];

	Return[result]
]



(* ::Subsubsection:: *)
(*smart linear syz*)


HomogeneousModuleNullSpace[module_,variables_]:=Module[{moduleDegrees,zeroDegreedSyz,nullSpace,rowReducedNullSpace},
	moduleDegrees=Table[0,Length[module]];
	nullSpace=LinearSyz[module,moduleDegrees,variables,0,SilentMode->True];
	rowReducedNullSpace=RowReduce[nullSpace];
	Return[rowReducedNullSpace]
]


LinearlyRedundantModuleMemberPosition[rowReducedNullSpace_]:=Module[{positions,result},
	positions=DeleteCases[Position[#,1]&/@rowReducedNullSpace,{}];
	result=Flatten[#[[1]]&/@positions];
	Return[result]
]


(*LinearlyRedundantModuleMemberPosition[#,Variables[#]]&[{{x},{x},{0}}]*)


(*(*Get rid of module generators that are linearly redundant before computing syzygy*)
SmartLinearSyz[module_,moduleDegreeList_,variables_,degreeBound_]:=Module[{moduleDegreeListUnion,positionsOfDegrees,
groupOfHomogeneousModules},
	moduleDegreeListUnion=moduleDegreeList//Union;
	positionsOfDegrees=Flatten[Position[moduleDegreeList,#]]&/@moduleDegreeListUnion;
	groupOfHomogeneousModules=module[[#]]&/@positionsOfDegrees;
	correspondingNullSpaces
	linearlyRedundantPositions=LinearlyRedundantModuleMemberPosition[#,variables];
	Return[groupOfHomogeneousModules]
]*)


(*SmartLinearSyz[{{x},{x},{0}},{1,1,0},1,1]*)


(*Position[{1,1,2,2,1,3,2},1]//Flatten*)


(* ::Subsection:: *)
(*other usable functions*)


ClearAll[GenerateIdeal]
GenerateIdeal[externalMomenta_,loopMomenta_,propagators_,kineticReplacements_,restrictedIndices_]:=Module[
{BaikovVariables,kineticVariables,variables,allVectors,prod2v,scalarProducts,BaikovReplacement,BaikovMatrix,P,Ideal,zConstrains},
	BaikovVariables=Table[ToExpression["z"<>ToString[i]],{i,Length[propagators]}];
	kineticVariables=Variables[kineticReplacements[[All,2]]];
	variables=Join[kineticVariables,BaikovVariables];
	allVectors=Join[loopMomenta,externalMomenta];
	prod2v=Outer[#1 #2->v@@Sort[{#1,#2}]&,allVectors,allVectors]//Flatten//DeleteDuplicates;
	scalarProducts=Select[prod2v[[All,1]]/.kineticReplacements/.prod2v,Head[#]==v&];
	BaikovReplacement=Solve[(Expand[propagators]/.kineticReplacements/.prod2v)==BaikovVariables,scalarProducts][[1]];
	BaikovMatrix=Outer[v@@Sort[{#1,#2}]/.BaikovReplacement/.v->Times/.kineticReplacements&,allVectors,allVectors];
	P=Det[BaikovMatrix];
	zConstrains=Table[If[MemberQ[restrictedIndices,i],BaikovVariables[[i]],1],{i,Length[BaikovVariables]}];
	Ideal=Join[Table[D[P,BaikovVariables[[i]]]*zConstrains[[i]],{i,Length[BaikovVariables]}],{P}]//Expand;
	Return[{Ideal,variables}]
]


RestrictedModule[M1ext_,restrictedIndices_]:=Module[{restrictedM1,restrictedM2},
	restrictedM1=M1ext[[All,restrictedIndices]];
	If[restrictedIndices=={},Return[restrictedM1]];
	restrictedM2=DiagonalMatrix[z/@restrictedIndices];
	Return[Join[restrictedM1,restrictedM2]]
]


ClearAll[SolveDegreedIntersection]
Options[SolveDegreedIntersection]={SyzNumberBound->Infinity}
SolveDegreedIntersection[M1ext_,restrictedIndices_,requiredDegree_,OptionsPattern[]]:=Module[{module,variables,moduleDegrees,syz,intersection},
	module=RestrictedModule[M1ext,restrictedIndices];
	variables=Variables[module];
	moduleDegrees=Table[1,Length[module]];
	syz=LinearSyz[module,moduleDegrees,variables,requiredDegree,SyzNumberBound->OptionValue[SyzNumberBound]];
	If[syz==={},Return[{}]];
	intersection=(#[[1;;Length[M1ext]]]&/@syz).M1ext;
	Return[intersection]
]


Print["package loaded."]


(* ::Subsection::Closed:: *)
(*use*)


(*externalMomenta={p1};
loopMomenta={l1};
propagators={l1^2,l1 p1};
kineticReplacements={p1^2->s};
(*===================================================*)
{Ideal,variables}=GenerateIdeal[externalMomenta,loopMomenta,propagators,kineticReplacements,{}]*)


(*externalMomenta={p1,p2,p4};
loopMomenta={l1};
propagators={l1^2,(l1+p1)^2,(l1+p1+p2)^2,(l1-p4)^2};
kineticReplacements={p1^2->0,p2^2->0,p4^2->0,p1 p2->s/2,p1 p4->t/2,p2 p4->(-s-t)/2};
(*===================================================*)
{Ideal,variables}=GenerateIdeal[externalMomenta,loopMomenta,propagators,kineticReplacements,{1,2,3,4}]*)


(*externalMomenta={p1,p2,p4};
loopMomenta={l1,l2};
propagators={l1^2,(l1+p1)^2,(l1+p1+p2)^2,(l2-p1-p2)^2,(l2+p4)^2,l2^2,(l1+l2)^2,(l1-p4)^2,(l2-p1)^2};
kineticReplacements={p1^2->0,p2^2->0,p4^2->0,p1 p2->s/2,p1 p4->t/2,p2 p4->(-s-t)/2};
(*===================================================*)
{Ideal,variables}=GenerateIdeal[externalMomenta,loopMomenta,propagators,kineticReplacements,{1,2,3,4,5,6,7}];*)


(*module={Ideal}//Transpose*)


(*silence=!False;
LinearSyz[module,{4,4,4,4,4},variables,8]
%.Ideal//Expand
*)




(* ::Subsection::Closed:: *)
(*backup*)


(*PolyInfoToPolynomial[variables_,degree_,coefficients_]:=coefficients.DegreedMonomials[variables,degree]*)


(*PolynomialToPolyInfo::requiredDegreeNonPositive="requiredDegree `1` is not negative.";
PolynomialToPolyInfo::requiredDegreeNotConsistent="requiredDegree `1` is not consistent with the polynomial `2`, which is degreed `3`.";
PolynomialToPolyInfo[polynomial_,requiredDegree_,variables_]:=Module[{polynomialDegree,dm,coefficients},
	If[requiredDegree<0,Message[PolynomialToPolyInfo::requiredDegreeNonPositive,requiredDegree];Return[$Failed]];
	If[Expand[polynomial]===0,polynomialDegree=requiredDegree,polynomialDegree=PolynomialDegree[polynomial,variables]];
	If[polynomialDegree!=requiredDegree,Message[PolynomialToPolyInfo::requiredDegreeNotConsistent,requiredDegree,polynomial,polynomialDegree];Return[$Failed]];
	dm=DegreedMonomials[variables,requiredDegree];
	coefficients=PolynomialCoefficients[polynomial,dm];
	Return[PolyInfo[variables,requiredDegree,coefficients]]
]

*)



(*
(*ModuleDegrees is not used in this version*)
ModuleDegrees::nonHomo="the module element `1` is not homogeneous";
ModuleDegrees::zero="the module element `1` is 0";
ModuleDegrees[module_,variables_]:=Module[{result,degree,i,preDegree},
	result={};
	For[i=1,i<=Length[module],i++,
		preDegree=PolynomialVectorDegrees[module[[i]],variables];
		degree=Union[DeleteCases[preDegree,-Infinity]];
		
		If[Length[degree]==0,Message[ModuleDegrees::zero,module[[i]]];Return[$Failed]];
		If[Length[degree]>1,Message[ModuleDegrees::nonHomo,module[[i]]];Return[$Failed]];
		
		AppendTo[result,degree[[1]]];
		
	];
	Return[result]
]*)


(*(*
in new version(but no more the newest!), syz is formated as PolyInfo
one must take care that in this format, polynomial 0 is attached with a degree, which is nolonger -Infinity
maybe the clarification of degree of 0 can make the code more consistent.
*)
GenerateSyzForHigerDegree[syz_,raisedDegree_]:=Transpose[PolyInfoPowerRaiseBy[#,raisedDegree]&/@syz]
(*PolynomialVectorDegrees[vector_,variables_]:=Module[{scaling},Exponent[Expand[vector]/.Table[variables[[i]]->variables[[i]]*scaling,{i,Length[variables]}],scaling]]*)
GenerateSyzForGivenDegree//ClearAll
GenerateSyzForGivenDegree::zeroSyz="the syz `1` is 0.";
GenerateSyzForGivenDegree::nonUniformDegreedSyz="the syz `1` is not uniform degreed under degree shift `2`.";
GenerateSyzForGivenDegree[syz_,degree_,syzDegreeShift_]:=Module[{syzDegree,syzVectorDegrees,preSyzDegree},
	syzVectorDegrees=#[[2]]&/@syz;
	preSyzDegree=DeleteCases[Union[syzVectorDegrees-syzDegreeShift],-Infinity];
	If[Length[preSyzDegree]>1,Message[GenerateSyzForGivenDegree::nonUniformDegreedSyz,syz,syzDegreeShift];Return[$Failed]];
	If[Length[preSyzDegree]==0,Message[GenerateSyzForGivenDegree::zeroSyz,syz];Return[$Failed]];
	syzDegree=preSyzDegree[[1]];
	Return[GenerateSyzForHigerDegree[syz,degree-syzDegree]]
]
(*the syzygy degree is defined as the maximum of degrees of the components OF THE CORRESPONDING SYZYGY ANSATZ*)
(*the Ideal degree is defined as the minimum of degrees of the components*)

*)



(*(*gives the polyInfo that represents the polynomials,
 which are proportional to the polynomial represented by the input polyInfo.
 we expect that this function will speed up the calculation, let`s see*)
PolyInfoPowerRaiseBy::targetDegreeLessThanInput="the target degree `1` must not be less than input degree `2`.";
PolyInfoPowerRaiseBy[polyInfo_,raisedDegree_]:=Module[
{variables,targetDegree,inputDegree,coefficients,inputDM,inputDMIndices,raisedDMIndices,result,i,j,resultRow},
	variables=polyInfo[[1]];
	inputDegree=polyInfo[[2]];
	targetDegree=raisedDegree+inputDegree;
	If[raisedDegree<0,Message[PolyInfoPowerRaiseBy::targetDegreeLessThanInput,targetDegree,inputDegree];Return[$Failed]];
	coefficients=polyInfo[[3]];
	inputDMIndices=DistributePowerIndices[Length[variables],inputDegree];
	raisedDMIndices=DistributePowerIndices[Length[variables],raisedDegree];
	result={};
	(*For[i=1,i<=Length[raisedDMIndices],i++,
		For[j=1,j\[LessEqual]Length[inputDMIndices],j++,
			result[[i,PowerIndicesToPosition[inputDMIndices[[j]]+raisedDMIndices[[i]]]]]
			+=coefficients[[PowerIndicesToPosition[inputDMIndices[[j]]]]]
		]
	];*)
	For[i=1,i<=Length[raisedDMIndices],i++,
		resultRow=Table[0,Binomial[Length[variables]+targetDegree-1,Length[variables]-1]];
		For[j=1,j<=Length[inputDMIndices],j++,
			resultRow[[PowerIndicesToPosition[inputDMIndices[[j]]+raisedDMIndices[[i]]]]]
			+=coefficients[[PowerIndicesToPosition[inputDMIndices[[j]]]]]
		];
		AppendTo[result,PolyInfo[variables,targetDegree,resultRow]];
	];
	Return[result]
	(*Return[PolyInfo[variables,targetDegree,#]&/@result]*)
]*)


(*(*return the position of a power index list in corresponding DistributePowerIndices results*)
PowerIndicesToPosition[powerIndicies_]:=Module[{seperatorPositionsWithRightBoundary,seperatorPositions,rightBoundary,result},
	seperatorPositionsWithRightBoundary=Accumulate[powerIndicies+1];
	seperatorPositions=Delete[seperatorPositionsWithRightBoundary,-1];
	rightBoundary=seperatorPositionsWithRightBoundary[[-1]];
	result=Binomial[rightBoundary-1,Length[seperatorPositions]]-Sum[Binomial[rightBoundary-seperatorPositions[[-i]]-1,i],{i,Length[seperatorPositions]}];
	Return[result]
]*)
