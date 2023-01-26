#include "spasm.h"

spasm_dense_lu *spasm_dense_LU_alloc(int m, int prime) {
	spasm_dense_lu *R;

	R = spasm_malloc(sizeof(spasm_dense_lu));
	R->m = m;
	R->n = 0;
	R->prime = prime;
	R->x = spasm_malloc(m * sizeof(spasm_GFp *));
	R->p = spasm_malloc(m * sizeof(int));
	return R;
}

void spasm_dense_LU_free(spasm_dense_lu * A) {
	for (int i = 0; i < A->n; i++)
		free(A->x[i]);
	free(A->x);
	free(A->p);
	free(A);
}

int spasm_dense_LU_grow(spasm_dense_lu * A, const spasm_GFp * y, int k, int processed) {
	int n, m, status;
	spasm_GFp **Ax;

#pragma omp critical(dense_LU)
	{
#pragma omp atomic read
		n = A->n;
		status = (n == processed);
		if (status) {
			m = A->m;
			A->p[n] = k;
			Ax = A->x;
			Ax[n] = spasm_malloc(m * sizeof(spasm_GFp));
			for (int j = 0; j < m; j++)
				Ax[n][j] = y[j];
#pragma omp atomic update
			A->n++;
		}
	}
	return status;
}


/*
 * if y belongs to the linear span of U, return 0. Else update U and return
 * 1. This function is THREAD-SAFE.
 */
int spasm_dense_LU_process(spasm_dense_lu * A, spasm_GFp * y) {
	int processed, k, n;
	spasm_GFp beta;

	const int m = A->m;
	const int prime = A->prime;
	const int *p = A->p;
	spasm_GFp **Ax = A->x;
	processed = 0;

	while (1) {
#pragma omp atomic read
		n = A->n;

		for (int i = processed; i < n; i++) {
			beta = prime - y[p[i]];
			for (int j = 0; j < m; j++)
				y[j] = (y[j] + beta * Ax[i][j]) % prime;
		}
		processed = n;

		for (k = 0; k < m; k++)
			if (y[k])
				break;
		if (k == m)
			return 0;

		/* make pivot unitary */
		beta = spasm_GFp_inverse(y[k], prime);
		for (int j = 0; j < m; j++)
			y[j] = (y[j] * beta) % prime;

		if (spasm_dense_LU_grow(A, y, k, processed))
			return 1;
	}
}
