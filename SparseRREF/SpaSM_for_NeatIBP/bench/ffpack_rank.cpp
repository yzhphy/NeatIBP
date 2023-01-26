#include <fflas-ffpack/fflas-ffpack-config.h>
#include <givaro/modular-balanced.h>
#include <fflas-ffpack/fflas/fflas.h>
#include <fflas-ffpack/utils/timer.h>
#include <fflas-ffpack/utils/Matio.h>
#include <fflas-ffpack/utils/args-parser.h>
#include <fflas-ffpack/ffpack/ffpack.h>
#include <iostream>

extern "C" {
#include "spasm.h"
}

using namespace FFLAS;

/** Compute the rank using FFLAS/FFPACK (DENSE) rank algorithm */

int main(int argc, char** argv) {
	typedef Givaro::Modular<int> Ring;
	int prime, i, j, k, n, m, nz, rank, *Ti, *Tj, *Tx;
	
	prime = 3;
	Ring F(prime);
	spasm_triplet *T;
	Ring::Element * A;

	T = spasm_load_sms(stdin, prime);
	A = fflas_new(F, T->n, T->m);

	m = T->m;
    n = T->n;
    Ti = T->i;
    Tj = T->j;
    Tx = T->x;
    nz = T->nz;

    for(k=0; k<nz; k++) {
    	F.init(*(A + Ti[k]*n + Tj[k]), Tx[k]);
    }
    spasm_triplet_free(T);

    double start_time = spasm_wtime();
    rank = FFPACK::Rank(F, n, m, A, n);
	double end_time = spasm_wtime();

    std::cerr << "Time for FFLAS/FFPACK dense rank: " << end_time - start_time << "s" << std::endl;
	std::cout << "Rank = " << rank << std::endl;

	fflas_delete(A);
    return 0;
}