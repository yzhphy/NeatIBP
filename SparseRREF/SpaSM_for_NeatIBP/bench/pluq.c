#include <stdio.h>
#include <assert.h>
#include <getopt.h>

#include "spasm.h"

#ifdef SPASM_TIMING
extern int64 reach, scatter, data_shuffling;
#endif

/** computes a PLUQ decomposition. U is always saved in a file named U.sms.
 *  If the keep-L option is provided, then L is also saved in L.sms.
 *
 * This is still suboptimal: the spasm_lu function should detect pre-existing pivots
 */

int main(int argc, char **argv) {
	int prime = 42013;
	int keep_L = 0;
	int allow_transpose = 1;

	/* options descriptor */
	struct option longopts[6] = {
		{"no-transpose", no_argument, NULL, 'a'},
		{"modulus", required_argument, NULL, 'p'},
		{"keep-L", no_argument, NULL, 'l'},
		{NULL, 0, NULL, 0}
	};

	char ch;
	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
		case 'a':
			allow_transpose = 0;
			break;
		case 'p':
			prime = atoi(optarg);
			break;
		case 'l':
			keep_L = 1;
			break;
		default:
			printf("Unknown option\n");
			exit(1);
		}
	}
	argc -= optind;
	argv += optind;

	spasm_triplet *T = spasm_load_sms(stdin, prime);
	printf("A : %d x %d with %d nnz (density = %.3f %%) -- loaded modulo %d\n", T->n, T->m, T->nz, 100.0 * T->nz / (1.0 * T->n * T->m), prime);
	int transpose = allow_transpose && (T->n < T->m);
	keep_L |= transpose;
	if (transpose) {
		printf("[pluq] transposing matrix\n");
		spasm_triplet_transpose(T);
	}
	spasm *A = spasm_compress(T);
	spasm_triplet_free(T);
	int n = A->n;
	int m = A->m;

	double start_time = spasm_wtime();
	int *p = spasm_malloc(n * sizeof(int));
	int *qinv = spasm_malloc(m * sizeof(int));
	int npiv = spasm_find_pivots(A, p, qinv);
	spasm_make_pivots_unitary(A, p, npiv);

	/* check that pivots are permuted correctly */
	int *Ap = A->p;
	int *Aj = A->j;
	for (int j = 0; j < m; j++)
		qinv[j] = -1;
	
	for (int i = 0; i < npiv; i++) {		
		int I = p[i];
		for (int px = Ap[I]; px < Ap[I + 1]; px++)
			assert(qinv[Aj[px]] == -1);
		qinv[Aj[Ap[I]]] = I;
	}

	spasm_lu *PLUQ = spasm_PLUQ(A, p, keep_L);
	printf("\n");

	spasm *U = NULL;
	spasm *L = NULL;
	if (!transpose) {
		L = PLUQ->L;
		U = PLUQ->U;
	} else {
		L = spasm_transpose(PLUQ->U, SPASM_WITH_NUMERICAL_VALUES);
		U = spasm_transpose(PLUQ->L, SPASM_WITH_NUMERICAL_VALUES);
	}
	int r = U->n;

	printf("LU factorisation took %.2f s\n", spasm_wtime() - start_time);
	printf("U :  %d x %d with %d nnz (density = %.1f %%)\n", r, m, spasm_nnz(U), 100.0 * spasm_nnz(U) / (1.0 * r * m - r * r / 2.0));
	if (L != NULL) {
		printf("L :  %d x %d with %d nnz (density =%.1f %%)\n", L->n, r, spasm_nnz(L), 100.0 * spasm_nnz(L) / (1.0 * r * n - r * r / 2.0));
		FILE *f = fopen("L.sms", "w");
		spasm_save_csr(f, L);
		fclose(f);
	}
#ifdef SPASM_TIMING
	printf("----------------------------------------\n");
	printf("reach   : %12" PRId64 "\n", reach);
	printf("scatter : %12" PRId64 "\n", scatter);
	printf("misc    : %12" PRId64 "\n", data_shuffling);
#endif
	printf("----------------------------------------\n");
	printf("rank of A = %d\n", U->n);

	FILE *f = fopen("U.sms", "w");
	spasm_save_csr(f, U);
	fclose(f);
	spasm_free_LU(PLUQ);

	free(p);
	spasm_csr_free(A);
	return 0;
}
