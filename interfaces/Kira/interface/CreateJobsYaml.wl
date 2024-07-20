(* ::Package:: *)

(*
how to use in shell:
math -script CreateJobsYaml.wl [outputPath]

*)





commandLineMode=True





If[commandLineMode,
	(*packagePath=DirectoryName[$InputFileName];*)
	workingPath=Directory[]<>"/";
	outputPath=$CommandLine[[-1]];

	,
	Print["WARNING: program is not running in command line mode!"];
	workingPath=NotebookDirectory[];
	(*packagePath="/home/zihao/projects/SyzygyRed/Parallelization/github/NeatIBP/";*)
	outputPath=""
	
]








If[StringSplit[outputPath,""][[-1]]=!="/",outputPath=outputPath<>"/"]


jobYamlString="
jobs:
  - reduce_user_defined_system:
      input_system: {files: [\"userSystem\"], otf: true}
#      input_system: {files: [\"userSystem\"], otf: true} # works as well
#      input_system: {files: [\"userSystem\"]} # works as well
#      input_system: \"userSystem\" # works as well
      select_integrals:
        select_mandatory_list:
          - [list] 
#          - [Tuserweight,list] # works as well; topology Tuserweight is the default topology if weight bit notation is used in user defined files

#      iterative_reduction: masterwise
#      conditional: false
      
      run_initiate: true
#      run_triangular: true
#      run_back_substitution: true
      run_firefly: true
      preferred_masters: basis
  - kira2math:
      target:
       - [list]
#       - [Tuserweight,list] # works as well
"


If[!DirectoryQ[outputPath<>"KiraIO/"],
	Print["***KiraIO folder ",outputPath<>"KiraIO/", " dose not exist. Exiting."];
	Exit[];
]


Export[outputPath<>"KiraIO/jobs.yaml",jobYamlString,"Text"]
