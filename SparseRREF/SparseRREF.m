(* ::Package:: *)

BeginPackage["SparseRREF`"]

SRLoadLib::usage = "RWLoadLib[] (re)loads the library."
SRSparseRowReduce::usage = "Accept a SparseArray and return the reduced echelon form of it in SparseArray."
SRSparseProps::usage = "Get properties of a matrix."
SRSparseValues::usage = "Get explicit values of a matrix."
SRSparsePos::usage = "Get explicit positions of a matrix."
SRSparseDims::usage = "Get dimension of a matrix."
SRNonZeroValues::usage = "Get the number of the explicit value positions."
SRRREF::usage = "Actually perform Gauss Elimination over a sparse matrix."
(*SRTest::usage = "test"*)


SR::rrefnolib = "Cannot find the SRRREF library."
SR::spasmnolib = "Cannot find the spaSm library."

Begin["`Private`"]

SRSparseValues[a_SparseArray] :=  
        SRSparseProps["ExplicitValues",a];

SRSparsePos[b_SparseArray] :=  
        SRSparseProps["ExplicitPositions",b];

SRSparseDims[c_SparseArray] :=
        Dimensions[c];

SRNonZeroValues[d_SparseArray] :=
        Length[d["NonzeroValues"]];

SRSparseRowReduce[e_SparseArray, Modulus->f_Integer] :=Module[{normalized},
        normalized = Cancel[e,Modulus->f];
        SRRREF[SRSparsePos[normalized], SRSparseValues[normalized], SRSparseDims[normalized], SRNonZeroValues[normalized], f]];

srrreflib = $Failed;
sprreflib = $Failed;

SRLoadLib[] := Module[
    {},
    srrreflib = FileNameJoin[{DirectoryName[$InputFileName],"SparseRREF.so"}];
    sprreflib = "/usr/local/lib/libspasm.so"; 
     
    If[TrueQ[srrreflib == $Failed],
       Message[SR::nolib];,
       LibraryLoad[srrreflib];
       SRLoadLibObjects[]
    ];
    
    If[TrueQ[sprreflib == $Failed],
       Message[SP::nolib];,
       LibraryLoad[sprreflib];
    ];

];

SRLoadLibObjects[] := Module[
    {},
    SRSparseProps = LibraryFunctionLoad[srrreflib, 
   "sparse_properties", {"UTF8String", {LibraryDataType[SparseArray], "Constant"}}, {_,_}];
    SRRREF = LibraryFunctionLoad[srrreflib, "rowreduce", {{Integer, 2, "Shared"},{Integer, 1, "Shared"}, {Integer, 1, "Shared"}, Integer, Integer}, {LibraryDataType[SparseArray], "Shared"}];

]
SRLoadLib[];

End[] (* "`Private`" *)

EndPackage[] (* "SparseRREF`" *)
