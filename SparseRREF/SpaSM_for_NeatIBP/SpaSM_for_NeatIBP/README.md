SpaSM (Sparse direct Solver Modulo _p_) for NeatIBP
=======================================

SpaSM is a software library devoted to sparse gaussian elimination modulo a small prime _p_. 
It is available under the General Public License Version 2 or later (GPLv2+).

This is a SpaSM version made for NeatIBP only.

To solve the problem of installation, we changed the file bench/Makefile.am and added bench/modules.c in this version.

All the changes will not do harm to the SpaSM package itself and it works well.

All the credits of SpaSM goes to Dr. Charles Bouillaguet, Sorbonne Université and we have the permission to include this package into NeatIBP.

The algorithms used in SpaSM are described in [CASC'16](http://www-almasty.lip6.fr/~bouillaguet/pub/CASC16.pdf) and [PASCO'17](http://www-almasty.lip6.fr/~bouillaguet/pub/PASCO17.pdf).

last change date: Jan.26, 2023

Features
--------

The core of the library is an implementation of the GPLU algorithm, heavily inspired by 
[Tim Davis](http://faculty.cse.tamu.edu/davis/)'s [CSparse](http://faculty.cse.tamu.edu/davis/publications_files/CSparse.zip), and 
adapted to the context of exact computation. On top of this, we designed new strategies to search for structural pivots. 
This allows several kind of useful operations on sparse matrices:
  * LU and PLUQ factorization
  * Rank computation
  * Solution of linear systems
  * Kernel basis
  * Permutation to block triangular form
  * Reduced Row-Echelon Form

Finally, the library does I/O of matrices in [SMS format](http://hpac.imag.fr/), which makes it 
somewhat compatible with [LinBox](http://linalg.org/).

A set of demonstration programs is provided (see the `bench` folder).

Installation
------------

In brief:
```./configure <options> && make && make check```

If you do not have the `configure` script, try:
```autoreconf -i```

You can also build out-of-tree using (for instance):
```mkdir build ; cd build ; ../configure && make && make check```

SpaSM does not rely on any third-party software, but is capable of using:
  * [METIS](http://glaros.dtc.umn.edu/gkhome/metis/metis/overview) to find row separators.
  * [FFLAS-FFPACK](https://github.com/linbox-team/fflas-ffpack) for dense rank computation.
  * [LinBox](https://github.com/linbox-team/linbox) for other rank algorithms.
  * [Lemon](https://lemon.cs.elte.hu/trac/lemon) to find maximum matchings on non-bipartite graphs.

SpaSM uses OpenMP to exploit multicore machines.

The most commonly used option include:
- `--with-metis=<path>` : build the METIS interface
- `--with-fflas-ffpack=<path>` : enable the tools relying on dense rank computation
- `--with-linbox=<path>` : build the linbox wrappers (for comparison purpose)
- `--with-lemon=<path>` : build the lemon matching tool

Demonstration scripts
---------------------

All SpaSM demonstration scripts read a matrix in [SMS format](http://hpac.imag.fr/) on the standard input.

For instance, these commands (run inside the `bench/` folder) will compute the rank of several large matrices in a few seconds:
```
curl http://hpac.imag.fr/Matrices/Margulies/kneser_10_4_1.sms.gz | gunzip - | ./rank_hybrid
curl http://hpac.imag.fr/Matrices/Homology/mk13.b5.135135x270270.sms.gz | gunzip - | ./rank_hybrid
curl http://hpac.imag.fr/Matrices/G5/IG5-17.sms.gz | gunzip - | ./rank_hybrid
```

It would be necessary to disable greedy pivot search for this one:
```
curl http://hpac.imag.fr/Matrices/Mgn/M0,6.data/M0,6-D9.sms.gz | gunzip - | ./rank_hybrid
```

When matrices have many empty rows/columns, they can/have to be removed with the `stack` utility:
```
curl http://hpac.imag.fr/Matrices/Relat/relat8.sms.gz | gunzip - | ./stack | ./rank_hybrid
curl http://hpac.imag.fr/Matrices/Relat/relat9.sms.gz | gunzip - | ./stack | ./rank_hybrid
```

Finding good pivots is crucial for the performance of any kind of sparse elimination procedure. The pivot-finding code is still a bit naïve. Sometimes it will find much more pivots, much faster, if the matrices are flipped around a vertical axis with the `vertical_swap` utility:
```
curl http://hpac.imag.fr/Matrices/GL7d/GL7d14.sms.gz | gunzip - | ./vertical_swap | ./rank_hybrid --sparse-threshold 0.01
...
curl http://hpac.imag.fr/Matrices/GL7d/GL7d22.sms.gz | gunzip - | ./vertical_swap | ./rank_hybrid --sparse-threshold 0.01
```

Citing SpaSM
------------

If by any luck your research depends on the SpaSM library, please consider citing the project as

```
@manual{spasm,
title = {{SpaSM}: a Sparse direct Solver Modulo $p$},
author = {The SpaSM group},
edition = {v1.2},
year = {2017},
note = {\url{http://github.com/cbouilla/spasm}}
}
```

Contact and discussion
----------------------

Please email <charles.bouillaguet@lip6.fr> for any questions.
