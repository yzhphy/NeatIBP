# NeatIBP
## Usage
NeatIBP is a mathematica package to generate small-sized integration-by-parts (IBP) relations for Feynman integral reduction.

## Version
1.0.1.0

## Dependencies
**Singular**

Singular is a computer algebra system for polynomial computations. Home page link:

https://www.singular.uni-kl.de/

**SpaSM**

SpaSM is a C library developed to perform sparse gaussian elimination modulo a small prime p. Github repo. link:

https://github.com/cbouilla/spasm


## Installation
This section shows how to install NeatIBP itself and its depencencies. You can use the following command to get NeatIBP itself installed on your computer:
```
git clone https://github.com/yzhphy/NeatIBP.git
```
Then, if you do not have **Singular** installed on your computer, please install this dependency following 

https://www.singular.uni-kl.de/

If you do not have **SpaSM** installed on your computer, you can install this dependency from 

https://github.com/cbouilla/spasm

However, considering some installation compatibility issues discovered in some operating systems (including Ubuntu 22.04 and CentOS), we recommend that you install **SpaSM** using the script provided in our repo. To do so, change to the **NeatIBP/SparseRREF** folder and run the script by the following command
```
sudo bash spasm_mod_script.sh
```
This script modifies the original SpaSM installation steps (we thank the authour of SpaSM for permitting us to do so). It will automatically install SpaSM for you.

After this, please check that the file **libspasm.so** exists in the folder **/usr/local/lib**. If so, SpaSM is ready to go.

## Manual of how to use NeatIBP
### Preparing inputs
To run NeatIBP, you need to create 3 input file files in your working folder.

1. A txt file named "kinematics.txt", containing the kinematic infomation of the Feynman diagram.

2. A txt file named "targetIntegrals.txt", containing the target integrals you are reducing.

3. A txt file named "config.txt" containing the necessary settings. Some settings you need to pay attention:

> 3a. **SingularApp** is the command to run the dependency Singular. For example, if your Singular is installed in /usr/bin/ (which is the usual case), this means you can run Singular using /usr/bin/Singular in terminal. Then you should set **SingularApp="/usr/bin/Singular"**

> 3b. **SparseRREF\`SpaSMLibrary** is the absolute path of SpaSM library. If you installed SpaSM using the script we provided, this library should be installed at /usr/local/lib. Thus, the default value of this setting should be **SparseRREF\`SpaSMLibrary = "/usr/local/lib/libspasm.so"** . If you have moved the Spasm packge elsewhere (though we do not recommend to do so), please change the value of this setting accordingly.

> 3c. It is recommended that you set **outputPath=Automatic**. Then, NeatIBP will set the output path as a sub sub directory of your working directory with a path related to the variable **ReductionOutputName** you set in the same config file.

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
In the examples, we have provided some convenient ways to comply the above operations. At first, modify the file "packagePath.txt" in the working folder containing the package path as "/SomePath/NeatIBP/". Then, turn on a terminal in the current working directory and use
```
./run.sh
```
to run NeatIBP, or use 
```
./monitor.sh
```
to turn on a monitor, or use
```
./continue.sh
```
to continue an unfinished mission.




### Checking completeness of the IBP system
Please keep in mind that, NeatIBP selects the IBP relations using numeric methods. Sometimes, NeatIBP will give a **WRONG** result because the numeric point (which is the variable **GenericPoint** set by the user in "kinematics.txt") happens to be a special bad point. Thus, it is highly recommended that you check the IBP system after the NeatIBP finished. To do so, turn on a terminal at the "/SomePath/NeatIBP/", and
```
math -script CheckIBP.wl [path]
```
where \[path\] is the output folder of the NeatIBP results. A complete output folder should contain subfolders as: "inputs", "results", and "tmp". 
> Notice: if your output folder contains 2 subfolders "results", and "tmp", as well as 3 additional txt files, this means that your output folder is created using some older version of NeatIBP. Please create a subfolder "input" and move the 3 additional txt files inside it. Otherwise you cannot use "CheckIBP.wl".


This step will automatically pick a random numeric check point and checks the IBP system at this point, to see if the target integrals are all reduced to master integrals. If not, most possibly, your **GenericPoint** is a bad point. But there is a little chance that the random check point is a bad point. We suggest you run the above command again to check at another random point.
