#include <assert.h>
#include "spasm.h"

/*
 * (dense vector) * (sparse) Matrix y <--- y + x*A
 */
void spasm_gaxpy(const spasm * A, const spasm_GFp * x, spasm_GFp * y) {
	const int n = A->n;
	const int *Ap = A->p;
	const int *Aj = A->j;
	const spasm_GFp *Ax = A->x;
	const int prime = A->prime;
	// assert(Ax != NULL);

	for (int i = 0; i < n; i++)
		spasm_scatter(Aj, Ax, Ap[i], Ap[i + 1], x[i], y, prime);
}



/*
 * (sparse vector) * (sparse) Matrix Compute y = x * M, where x and M are
 * sparse.
 * 
 * The result is scattered in y, its pattern is given by yi. The return value nz
 * is the number of non-zero entries in y.
 */
int spasm_sparse_vector_matrix_prod(const spasm * M, const spasm_GFp * x, const int *xi, int xnz, spasm_GFp * y, int *yi)
{
	int p, i, j, k, m, nz, Mnz, prime, *Mp, *Mj, *w;
	spasm_GFp *Mx;

	/* check inputs */
	Mnz = spasm_nnz(M);
	assert(x != NULL);
	assert(Mnz != 0);

	m = M->m;
	Mp = M->p;
	Mj = M->j;
	Mx = M->x;
	prime = M->prime;

	/* get workspace, initialize w */
	w = spasm_calloc(m, sizeof(int));

	/* find pattern of result */
	nz = 0;
	for (k = 0; k < xnz; k++) {
		i = xi[k];

		for (p = Mp[i]; p < Mp[i + 1]; p++) {
			j = Mj[p];

			if (w[j] == 0) {
				w[j] = 1;
				yi[nz] = j;
				nz++;
			}
		}
	}

	/* form result */
	for (k = 0; k < xnz; k++) {
		i = xi[k];
		spasm_scatter(Mj, Mx, Mp[i], Mp[i + 1], x[i], y, prime);
	}

	/* free workspace */
	free(w);
	return nz;
}
