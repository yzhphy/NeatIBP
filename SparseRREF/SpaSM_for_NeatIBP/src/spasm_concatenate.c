/* indent -nfbs -i2 -nip -npsl -di0 -nut spasm_concatenate.c */
#include <assert.h>
#include "spasm.h"

/*
 * Given two matrix A size na x m and B size nb x m, return a matrix C size
 * (na + nb) x m such as C[0 : na, 0 : m] = A and C[na : nb, 0 : m] = B.
 */
spasm *spasm_row_concatenation(spasm * A, spasm * B, int with_values) {
	spasm *C;
	int px, i, na, nb, m, n, anz, bnz, nzmax, prime;
	int *Ap, *Bp, *Cp, *Aj, *Bj, *Cj;
	spasm_GFp *Ax, *Bx, *Cx;

	/* Check inputs */
	if (A == NULL && B == NULL) {
		return NULL;
	}
	if (A == NULL && B != NULL) {
		return B;
	}
	if (A != NULL && B == NULL) {
		return A;
	}
	assert(A->m == B->m);
	assert(A->prime == B->prime);

	na = A->n;
	nb = B->n;
	n = na + nb;
	m = A->m;
	prime = A->prime;

	Ap = A->p;
	Bp = B->p;
	Aj = A->j;
	Bj = B->j;
	Ax = A->x;
	Bx = B->x;

	anz = A->nzmax;
	bnz = B->nzmax;
	nzmax = anz + bnz;	/* number of entries in C = number of entries
				 * in A + number of entries in B */

	/* Allocate result */

	C = spasm_csr_alloc(n, m, nzmax, prime, with_values);
	Cp = C->p;
	Cj = C->j;
	Cx = C->x;

	/* compute upper part of C */
	for (i = 0; i < na; i++) {
		Cp[i] = Ap[i];
		//<---row pointers

			// compute entries.
			for (px = Ap[i]; px < Ap[i + 1]; px++) {
			Cj[px] = Aj[px];
			if (with_values) {
				Cx[px] = Ax[px];
			}
		}
	}

	/* compute lower part of C */
	for (i = 0; i < nb; i++) {
		Cp[i + na] = Ap[na] + Bp[i];	/* <--- row pointers */

		/* compute entries */
		for (px = Bp[i]; px < Bp[i + 1]; px++) {
			Cj[px + anz] = Bj[px];
			if (with_values) {
				Cx[px + anz] = Bx[px];
			}
		}
	}
	Cp[n] = Ap[na] + Bp[nb];

	return C;
}
