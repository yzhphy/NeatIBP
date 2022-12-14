# NeatIBP
## Usage
NeatIBP is a mathematica package to generate small-sized integration-by-parts (IBP) relations for Feynman integral reduction.

## Dependencies
### Singular
Singular is a computer algebra system for polynomial computations. 
Link:
https://www.singular.uni-kl.de/
### SpaSM
SpaSM is a C library developed to perform sparse gaussian elimination modulo a small prime p. <br/>
Be advised that this package can only be installed on **Debian/Ubuntu OS (DO NOT INSTALL ON 22.04 BECAUSE OF SOME UNKNOWN ISSUES, WHICH MAY CAUSE DESKTOP GUI ERROR)** currently. 
Download link:
https://github.com/cbouilla/spasm <br/>

**Installation manual:**
1. Download the SpaSM package, you may use  
```
git clone https://github.com/cbouilla/spasm.git
```
2. The installation procedure requires sudo permission. Check if you have noweb & libtool installed, if not, you may use
```
sudo apt-get install noweb
sudo apt-get install libtool
```
3. Install the package using the following commands
```
sudo autoreconf -i
sudo automake --add-missing
sudo automake
sudo ./configure
sudo make
sudo make check
sudo make install
```
4. Make sure that you have libspasm.so at **/usr/local/lib**.
5. It is ready to go.

## Installation of NeatIBP
After installing all the dependencies mentioned above, you can install NeatIBP by
```
git clone https://github.com/yzhphy/NeatIBP.git
```

## Manual
### Preparing inputs
To run NeatIBP, you need to create 3 input file files in your working folder.

1. A txt file named "kinematics.txt", containing the kinematic infomation of the Feynman diagram.

2. A txt file named "targetIntegrals.txt", containing the target integrals you are reducing.

3. A txt file named "config.txt" containing the necessary settings. Some settings you need to pay attention:

> 3a. SingularApp is the command to run the dependency Singular. For example, if your Singular is installed in /usr/bin/ (which is the usual case), this means you can run Singular using /usr/bin/Singular in terminal. Then you should set SingularApp="/usr/bin/Singular"

> 3b. It is recommended that you set outputPath=Automatic. Then, NeatIBP will set the output path as a sub sub directory of your working directory with a path related to the variable ReductionOutputName you set in the same config file.

You can copy the 3 files from the examples provided in the example directory and modify them.

### Running
After preparing the above 3 files, turn on a terminal in the current working directory. Assuming your NeatIBP is installed as /SomePath/NeatIBP/, then you can run NeatIBP in that terminal by 
```
/SomePath/NeatIBP/run.sh
```
You can also turn on another terminal in the current working directory and run
```
/SomePath/NeatIBP/monitor.sh
```
to see the status of the missions.
If your NeatIBP is ternimated by hand or unexpectedly, you can run the following command in the terminal to continue the mission
```
/SomePath/NeatIBP/continue.sh
```




