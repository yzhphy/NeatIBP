# NeatIBP
## Usage
NeatIBP is a mathematica package to generate small-sized integration-by-parts (IBP) relations for Feynman integral reduction.

## Dependencies
### Singular
Singular is a computer algebra system for polynomial computations. 
Link:
https://www.singular.uni-kl.de/

## Installation
1. Install dependencies
2. Install NeatIBP. You can install using
```
git clone https://github.com/yzhphy/NeatIBP.git
```

## Manual


 run command
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
