#include <assert.h>
#include <stdio.h>
#include "spasm.h"

/*
 * Finds pivots without performing arithmetic operations (using the
 * Faugère-Lachartre heuristic and some other ideas) permute the matrix so
 * that the pivots are on the top-left. The result is sent to the standard
 * output
 */

int main() {
	int npiv, n, m, prime = 42013, *p, *qinv;
	spasm_triplet *T;
	spasm *A, *B;

	T = spasm_load_sms(stdin, prime);
	A = spasm_compress(T);
	spasm_triplet_free(T);
	n = A->n;
	m = A->m;

	p = spasm_malloc(n * sizeof(int));
	qinv = spasm_malloc(m * sizeof(int));
	npiv = spasm_find_pivots(A, p, qinv);
	B = spasm_permute_pivots(A, p, qinv, npiv);

	spasm_save_csr(stdout, B);
	free(p);
	free(qinv);
	spasm_csr_free(B);
	spasm_csr_free(A);
	return 0;
}
