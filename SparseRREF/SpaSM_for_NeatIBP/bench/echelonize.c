#include <assert.h>
#include <stdio.h>
#include "spasm.h"
#include <getopt.h>

/** produces a matrix U such that:
    *) rowspan(U) == rowspan(A) 
    *) there exists a column permutation Q such that U.Q is upper-trapezoidal 
*/
int main(int argc, char **argv)
{
	char nnz[6];
	int n_times = 3;
	int prime = 42013;
	int keep_L = 0;

	/* options descriptor */
	struct option longopts[7] = {
		{"max-recursion", required_argument, NULL, 'm'},
		{"keep-L", no_argument, NULL, 'l'},
		{"modulus", required_argument, NULL, 'p'},
		{NULL, 0, NULL, 0}
	};

	char ch;
	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
		case 'm':
			n_times = atoi(optarg);
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
	spasm *A = spasm_compress(T);
	spasm_triplet_free(T);

	int n = A->n;
	int m = A->m;
	spasm_human_format(spasm_nnz(A), nnz);
	fprintf(stderr, "A is %d x %d (%s nnz)\n", n, m, nnz);

	/* allocate room for U */
	spasm *U = spasm_csr_alloc(n, m, spasm_nnz(A), A->prime, SPASM_WITH_NUMERICAL_VALUES);
	spasm *L = spasm_csr_alloc(n, m, spasm_nnz(A), A->prime, SPASM_WITH_NUMERICAL_VALUES);

	int *p = spasm_malloc(n * sizeof(int));
	int *full_p = spasm_malloc(n * sizeof(int));
	for (int i = 0; i < n; i++)
		full_p[i] = -1;
	int *qinv = spasm_malloc(m * sizeof(int));
	int *Up = U->p;
	int *Uj = U->j;
	spasm_GFp *Ux = U->x;
	int *Lp = L->p;
	int *Lj = L->j;
	spasm_GFp *Lx = L->x;
	int unz = 0;
	int lnz = 0;
	int u_n = 0;

	double start_time = spasm_wtime();

	for (int iteration = 0; iteration < n_times; iteration++) {
		/* find new pivots */
		int npiv = spasm_find_pivots(A, p, qinv);
		spasm_make_pivots_unitary(A, p, npiv);
	
		/* copy them into U */
		int *Ap = A->p;
		int *Aj = A->j;
		int *Ax = A->x;	
		for (int i = 0; i < npiv; i++) {
			Up[u_n] = unz;
			int I = p[i];
			full_p[u_n] = I;
			/* not enough room in U ? realloc twice the size */
			if (unz + m > U->nzmax) {
				spasm_csr_realloc(U, 2 * U->nzmax + m);
				Uj = U->j;
				Ux = U->x;
			}
			for (int px = Ap[I]; px < Ap[I + 1]; px++) {
				Uj[unz] = Aj[px];
				Ux[unz] = Ax[px];
				unz++;
			}
			u_n++;
		}
		Up[u_n] = unz;


		/* compute schur complement, update matrix */
		spasm *B = spasm_schur(A, p, npiv, -1, 0, NULL);
		spasm_csr_free(A);
		A = B;
	}

	/* final step : GPLU on the remainder */
	int npiv = spasm_find_pivots(A, p, qinv);
	spasm_make_pivots_unitary(A, p, npiv);
	spasm_lu *LU = spasm_LU(A, p, SPASM_DISCARD_L);
	
	/* append last pivots to U */
	spasm_csr_free(A);
	A = LU->U;
	
	int *Ap = A->p;
	int *Aj = A->j;
	int *Ax = A->x;	
	for (int i = 0; i < A->n; i++) {
		/* not enough room in U ? realloc twice the size */
		if (unz + m > U->nzmax) {
			spasm_csr_realloc(U, 2 * U->nzmax + m);
			Uj = U->j;
			Ux = U->x;
		}

		Up[u_n] = unz;
		for (int px = Ap[i]; px < Ap[i + 1]; px++) {
			Uj[unz] = Aj[px];
			Ux[unz] = Ax[px];
			unz++;
		}
		u_n++;
	}
	Up[u_n] = unz;
	U->n = u_n;
	spasm_free_LU(LU);

	spasm_human_format(spasm_nnz(U), nnz);
	fprintf(stderr, "LU factorization done in %.3f s. NNZ(U) = %s. rank = %d\n", spasm_wtime() - start_time, nnz, u_n);
	
	spasm_save_csr(stdout, U);
	spasm_csr_free(U);
	
	free(qinv);
	free(p);
	return 0;
}
