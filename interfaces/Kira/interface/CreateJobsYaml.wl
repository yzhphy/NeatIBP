(* ::Package:: *)

(*
how to use in shell:
math -script CreateJobsYaml.wl [outputPath]

*)








commandLineMode=True





If[commandLineMode,
	(*packagePath=DirectoryName[$InputFileName];*)
	workingPath=Directory[]<>"/";
	commandLine=$CommandLine;
	outputPath=commandLine[[-1]];
	If[commandLine[[-2]]==="-s",
		mode="shortened";
	,
		mode="normal";
	];

	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	(*packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";*)
	outputPath="";
	mode="normal"
]








If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


jobYamlString="
jobs:
  - reduce_user_defined_system:
      input_system: {files: [\"userSystemSUFFIX\"], otf: true}
#      input_system: {files: [\"userSystemSUFFIX\"], otf: true} # works as well
#      input_system: {files: [\"userSystemSUFFIX\"]} # works as well
#      input_system: \"userSystemSUFFIX\" # works as well
      select_integrals:
        select_mandatory_list:
          - [list] 
#          - [Tuserweight,list] # works as well; topology Tuserweight is the default topology if weight bit notation is used in user defined files

#      iterative_reduction: masterwise
#      conditional: false
      
      run_initiate: true
#      run_triangular: true
#      run_back_substitution: true
      run_firefly: RUNFIREFLY
      run_triangular: RUNTRIANGULAR
      run_back_substitution: RUNBACKSUBSTITUTION
      preferred_masters: basis
  - kira2math:
      target:
       - [list]
#       - [Tuserweight,list] # works as well
"


Switch[mode,
"normal",
	jobYamlString=StringReplace[jobYamlString,"SUFFIX"->""];
,
"shortened",
	jobYamlString=StringReplace[jobYamlString,"SUFFIX"->"Shortened"];
,
_,
	Print["***CreateJobsYaml.wl: Unkown mode ",mode, ". Exiting."];
	Exit[];
]


If[Not[RunFireFlyInKira===False],
	jobYamlString=StringReplace[jobYamlString,{
		"RUNFIREFLY"->"true",
		"RUNTRIANGULAR"->"false",
		"RUNBACKSUBSTITUTION"->"false"
	}]
,
	jobYamlString=StringReplace[jobYamlString,{
		"RUNFIREFLY"->"false",
		"RUNTRIANGULAR"->"true",
		"RUNBACKSUBSTITUTION"->"true"
	}]

]


If[!DirectoryQ[outputPath<>"KiraIO/"],
	Print["***KiraIO folder ",outputPath<>"KiraIO/", " dose not exist. Exiting."];
	Exit[];
]


Export[outputPath<>"KiraIO/jobs.yaml",jobYamlString,"Text"]