#include <stdio.h>
#include <assert.h>
#include <getopt.h>
#include "spasm.h"

/** this program demonstrate a naive dense rank computation */

int main() {
	int r, n, m, *Aj, *Ap, prime, tid;
	spasm_triplet *T;
	spasm *A;
	spasm_dense_lu *LU;
	spasm_GFp *Ax;
	double start;

	prime = 42013;
	tid = 0;

#ifdef USE_OPENMP
	tid = omp_get_thread_num();
#endif

	T = spasm_load_sms(stdin, prime);
	A = spasm_compress(T);
	spasm_triplet_free(T);

	n = A->n;
	m = A->m;
	Ap = A->p;
	Aj = A->j;
	Ax = A->x;

	LU = spasm_dense_LU_alloc(m, prime);
	start = spasm_wtime();
	r = 0;

	#pragma omp parallel
	{
		spasm_GFp *x = spasm_malloc(m * sizeof(spasm_GFp));

		#pragma omp for schedule(dynamic, 1)
		for (int i = 0; i < n; i++) {
			spasm_vector_zero(x, m);
			for (int px = Ap[i]; px < Ap[i + 1]; px++)
				x[Aj[px]] = Ax[px];

			if (spasm_dense_LU_process(LU, x))
				#pragma omp atomic update
				r++;

			if (tid == 0) {
				fprintf(stderr, "\rrow %d/%d, rank >= %d", i + 1, n, r);
				fflush(stderr);
			}
		}
		free(x);
	}

	fprintf(stderr, "\nFinal rank = %d [%.1fs]\n", r, spasm_wtime() - start);
	spasm_dense_LU_free(LU);
	spasm_csr_free(A);
	return 0;
}
