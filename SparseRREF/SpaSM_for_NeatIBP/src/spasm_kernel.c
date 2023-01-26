/* indent -nfbs -i2 -nip -npsl -di0 -nut spasm_kernel.c */
#include <assert.h>
#include "spasm.h"

/*
 * Returns a basis of the (left) kernel of A, i.e. a basis of the vector
 * space K = {x | x*A == 0 }.
 * 
 * A_t : the TRANSPOSE of A. column_permutation : an optional (may be NULL)
 * sparsity-preserving permutation of the columns
 */
spasm *spasm_kernel(const spasm * A_t, const int *column_permutation) {
	spasm_lu *PLUQ;
	spasm *L, *K;
	int i, j, p, m, r, top, prime, nz;
	int *xi, *Kp, *Kj, *Kx, *q;
	spasm_GFp *x;

	PLUQ = spasm_PLUQ(A_t, column_permutation, SPASM_DISCARD_L);

	L = spasm_transpose(PLUQ->U, SPASM_WITH_NUMERICAL_VALUES);

	m = L->n;
	r = L->m;
	prime = L->prime;
	q = spasm_pinv(PLUQ->qinv, m);

	/* allocate result and workspace */
	K = spasm_csr_alloc(m - r, m, L->nzmax, prime, SPASM_WITH_NUMERICAL_VALUES);
	xi = malloc(3 * m * sizeof(int));
	spasm_vector_zero(xi, 3 * m);
	x = malloc(m * sizeof(spasm_GFp));

	nz = 0;
	Kp = K->p;
	for (i = r; i < m; i++) {
		top = spasm_sparse_backward_solve(L, L, i, xi, x, SPASM_IDENTITY_PERMUTATION, 0);

		/* enlarge K if necessary */
		if (nz + m - top + 1 > K->nzmax) {
			spasm_csr_realloc(K, 2 * K->nzmax + m - top + 1);
		}
		Kp = K->p;
		Kj = K->j;
		Kx = K->x;

		/* finalize previous row of K */
		Kp[i - r] = nz;

		for (p = top; p < m; p++) {
			j = xi[p];
			if (x[j] != 0) {
				Kj[nz] = q[j];
				Kx[nz] = x[j];
				nz++;
			}
		}
		Kj[nz] = q[i];
		Kx[nz] = prime - 1;
		nz++;
	}

	/* finalize last row */
	Kp[m - r] = nz;

	/* cleanup and return */
	free(xi);
	free(x);
	free(q);
	spasm_free_LU(PLUQ);
	spasm_csr_free(L);
	return K;
}
