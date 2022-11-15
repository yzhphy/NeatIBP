Dependencies:
Singular
https://www.singular.uni-kl.de/

SparseRREF by RWinter. 
url: https://github.com/RWUSTC/RW-s-Sparse-Matrix-RREF
Try to type
<<SparseRREF`
In a test mathematica notebook to test if this dependency is functioning. 


To use SyzygyRed_Parallel, edit "inputs_and_config.txt" first. Then run command
./run.sh
The reduction procedure will run in parallel automatically.

you can run 
./monitor.sh
in another terminal to see the status of missions

if the reduction process terminate unexpectedly or by hand, you can run
./continue.sh
to continue the reduction.
Or you can run 
./run.sh
to start reduction from the very beginning (existing results will be deleted)
