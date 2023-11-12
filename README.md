# NeatIBP
## Usage
NeatIBP is a mathematica package to generate small-sized integration-by-parts (IBP) relations for Feynman integral reduction.

## Version
1.0.2.7

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
Be advised, NeatIBP currently support only Mathematica 12.0 and up. If you are using some old version of Mathematica, please update to the new version.

Then, if you do not have **Singular** installed on your computer, please install this dependency from its [home page](https://www.singular.uni-kl.de/).

If you do not have **SpaSM** installed on your computer, you need to install this dependency. At first, you should check that if your system has the dependency package **libtool** installed. If not, one can run: (debian-based system, for example)
you can install this dependency from 

```
sudo apt-get install libtool
```

Then, to install **SpaSM** itself, considering some version compatibility issues, we strongly suggest that you install **SpaSM** using the script provided in our repo. To do so, change to the **NeatIBP/SparseRREF** folder and run the script by the following command
```
sudo bash spasm_mod_script.sh
```
This script modifies the original SpaSM installation steps (we thank the authour of SpaSM for permitting us to do so). It will automatically install SpaSM for you.

After this, please check that the file **libspasm.so** exists in the folder **/usr/local/lib**. If so, SpaSM is ready to go.

Besides, NeatIBP supports an alternative way of installation via spack. If the user prefer to do so, please follow the instructions in the [appendix](https://github.com/yzhphy/NeatIBP#installation-via-spack-alternative).

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

## Additional Statements
NeatIBP uses Mathematica in command line using command like
```
math -script script_name.wl
```
Please make sure that this works on your computer in order to use NeatIBP. This version of NeatIBP dose not support running Mathematica in command lines using other commands yet.

## How to cite
If you use NeatIBP in your research, we would appreciate it if you cite the related paper  [arxiv 2305.08783](https://arxiv.org/abs/2305.08783). Or you can cite with
```
@article{Wu:2023upw,
    author = "Wu, Zihao and Boehm, Janko and Ma, Rourou and Xu, Hefeng and Zhang, Yang",
    title = "{NeatIBP 1.0, A package generating small-size integration-by-parts relations for Feynman integrals}",
    eprint = "2305.08783",
    archivePrefix = "arXiv",
    primaryClass = "hep-ph",
    reportNumber = "USTC-ICTS/PCFT-23-15",
    month = "5",
    year = "2023"
}
```
or
```
%\cite{Wu:2023upw}
\bibitem{Wu:2023upw}
Z.~Wu, J.~Boehm, R.~Ma, H.~Xu and Y.~Zhang,
%``NeatIBP 1.0, A package generating small-size integration-by-parts relations for Feynman integrals,''
[arXiv:2305.08783 [hep-ph]].
```

## Appendix
### Installation via Spack (alternative)

If the user prefer to install NeatIBP using spack, please follow the steps in this section. If the user have installed NeatIBP and its dependencies using the manual in the previous section, please skip this section.

We will assume that the user has some directory path with read and
write access. In the following, we assume this path is set as the environment variable
`software_ROOT`, as detailed in the following example:

```bash
mkdir -p ~/singular-gpispace/neatibp-install
export software_ROOT=~/singular-gpispace
export install_ROOT=~/singular-gpispace/neatibp-install

```
Note, this needs to be set again if you open a new terminal session (preferably set it automatically by adding the line to your .profile file).

#### Install Spack
If Spack is not already present in the above directory, clone Spack from Github:
```bash
git clone https://github.com/spack/spack.git $software_ROOT/spack

```
We check out verison v0.19.0 of Spack (the current version):
```bash
cd $software_ROOT/spack
git checkout releases/v0.19
cd $software_ROOT

```
Spack requires a couple of standard system packages to be present. For example, on an Ubuntu machines they can be installed by the following commands (which typically require sudo privilege)

```bash
sudo apt update

```
```bash
sudo apt install build-essential ca-certificates coreutils curl environment-modules gfortran git gpg lsb-release python3 python3-distutils python3-venv unzip zip

```

To be able to use spack from the command line, run the setup script:
```bash
. $software_ROOT/spack/share/spack/setup-env.sh

```
Note, this script needs to be executed again if you open a new terminal session (preferably set it automatically by adding the line to your .profile file).

Finally, Spack needs to boostrap clingo.  This can be done by concretizing any
spec, for example
```bash
spack spec zlib

```

Note: If you experience connection timeouts due to a slow internet connection you can set in the following file the variable `connect_timeout` to a larger value.
```bash
vim $software_ROOT/spack/etc/spack/defaults/config.yaml

```

#### How to uninstall Spack
Note that Spack can be uninstalled by just deleting its directory and its configuration files. Be CAREFUL to do that, since it will delete your Spack setup. Typically you do NOT want to do that now, so the code is commented out. It can be useful if your Spack installation is broken:

```bash
#cd
#rm -rf $software_ROOT/spack/
#rm -rf .spack

```

#### Install NeatIBP

Once you have installed Spack, our package can be installed with just three lines of code.

Clone the NeatIBP package repository into this directory:
```bash
git clone https://github.com/singular-gpispace/spack-packages.git $software_ROOT/spack-packages

```

Add the NeatIBP package repository to the Spack installation:
```bash
spack repo add $software_ROOT/spack-packages

```

#### Installing packages

Now you are good to go to install specific packages of the repo. For example, you could do:

```bash
spack install neatibp

```

If you install your first package, this will take a bit of time since a lot of dependencies are installed, e.g. Singular and SpaSM.

To use the package you have to load it:

```bash
spack load neatibp

```
Then one should change to the default_settings.txt and modify the following variables to the installation of the dependencies. 

One can just run the script to do the substitution and delete the script by:


```bash

sh $install_ROOT/spack_sub_script.sh

```
Or manually do as the following:

```bash

cd $NEATIBP_INSTALL_DIR/NeatIBP

sed -i 's@/usr/local/lib@'"$SPASM_INSTALL_DIR/lib"'@' default_settings.txt

sed -i 's@/usr/bin/Singular@'"$SINGULAR_INSTALL_DIR/bin"'@' default_settings.txt

sed -i 's@$(dirname $( realpath ${BASH_SOURCE}))@'"$NEATIBP_INSTALL_DIR/NeatIBP"'@' run.sh

sed -i 's@$(dirname $( realpath ${BASH_SOURCE}))@'"$NEATIBP_INSTALL_DIR/NeatIBP"'@' monitor.sh

sed -i 's@$(dirname $( realpath ${BASH_SOURCE}))@'"$NEATIBP_INSTALL_DIR/NeatIBP"'@' continue.sh

```
One should copy the main files to some directory, for example, $install_ROOT/NeatIBP

```bash

cd $install_ROOT/NeatIBP

cp $NEATIBP_INSTALL_DIR/NeatIBP/continue.sh continue.sh

cp $NEATIBP_INSTALL_DIR/NeatIBP/run.sh run.sh

cp $NEATIBP_INSTALL_DIR/NeatIBP/monitor.sh monitor.sh

cp $NEATIBP_INSTALL_DIR/NeatIBP/default_settings.txt config.txt

mkdir outputs

```

Then it's ready to follow the manual to configure NeatIBP in workspace.
