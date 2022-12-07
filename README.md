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
2. The installation procedure requires sudo permission. Check if you have noweb installed, if not, you may use
```
sudo apt-get install noweb
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
