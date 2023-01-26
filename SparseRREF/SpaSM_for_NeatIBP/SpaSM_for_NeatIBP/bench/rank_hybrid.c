#include <assert.h>
#include <stdio.h>
#include "spasm.h"
#include <getopt.h>

/** Computes the rank of the input matrix using the hybrid strategy described in [PASCO'17] */
int main(int argc, char **argv) {

	/* charge la matrice depuis l'entrée standard */
	int prime, n_times, rank, npiv, n, m, dense_final, gplu_final,
	    allow_transpose, ch;
	double start_time, end_time;
	spasm_triplet *T;
	spasm *A, *B;
	int *p, *qinv;
	double density, sparsity_threshold;
	char nnz[6];

	allow_transpose = 1;	/* transpose ON by default */
	n_times = 3;
	dense_final = 0;
	gplu_final = 0;
	sparsity_threshold = 0.1;
	prime = 42013;

	/* options descriptor */
	struct option longopts[7] = {
		{"sparse-threshold", required_argument, NULL, 's'},
		{"max-recursion", required_argument, NULL, 'm'},
		{"dense-last-step", no_argument, NULL, 'd'},
		{"gplu-last-step", no_argument, NULL, 'g'},
		{"no-transpose", no_argument, NULL, 'a'},
		{"modulus", required_argument, NULL, 'p'},
		{NULL, 0, NULL, 0}
	};

	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
		case 's':
			sparsity_threshold = atof(optarg);
			break;
		case 'm':
			n_times = atoi(optarg);
			break;
		case 'd':
			dense_final = 1;
			break;
		case 'a':
			allow_transpose = 0;
			break;
		case 'g':
			gplu_final = 1;
			break;
		case 'p':
			prime = atoi(optarg);
			break;
		default:
			printf("Unknown option\n");
			exit(1);
		}
	}
	argc -= optind;
	argv += optind;


	T = spasm_load_sms(stdin, prime);
	if (allow_transpose && (T->n < T->m)) {
		fprintf(stderr, "[rank] transposing matrix : ");
		fflush(stderr);
		start_time = spasm_wtime();
		spasm_triplet_transpose(T);
		fprintf(stderr, "%.1f s\n", spasm_wtime() - start_time);
	}
	A = spasm_compress(T);
	spasm_triplet_free(T);
	n = A->n;
	m = A->m;
	spasm_human_format(spasm_nnz(A), nnz);
	fprintf(stderr, "start. A is %d x %d (%s nnz)\n", n, m, nnz);

	p = spasm_malloc(n * sizeof(int));
	qinv = spasm_malloc(m * sizeof(int));

	start_time = spasm_wtime();
	rank = 0;
	npiv = spasm_find_pivots(A, p, qinv);
	spasm_make_pivots_unitary(A, p, npiv);
	density = spasm_schur_probe_density(A, p, qinv, npiv, 100);

	for (int i = 0; i < n_times; i++) {
		int64_t nnz = (density * (n - npiv)) * (m - npiv);
		char tmp[6];
		spasm_human_format(sizeof(int) * (n - npiv + nnz) + sizeof(spasm_GFp) * nnz, tmp);
		fprintf(stderr, "round %d / %d. Schur complement is %d x %d, estimated density : %.2f (%s byte)\n", i, n_times, n - npiv, m - npiv, density, tmp);

		if (density > sparsity_threshold)
			break;

		/* compute schur complement, update matrix */
		B = spasm_schur(A, p, npiv, density, 0, NULL);
		spasm_stack_nonpivotal_columns(B, qinv);

		spasm_csr_free(A);
		A = B;
		rank += npiv;
		n = A->n;
		m = A->m;

		npiv = spasm_find_pivots(A, p, qinv);
		spasm_make_pivots_unitary(A, p, npiv);
		density = spasm_schur_probe_density(A, p, qinv, npiv, 100);
	}

	/* ---- final step ---------- */

	/* sparse schur complement : GPLU */
	if (gplu_final || (!dense_final && density < sparsity_threshold)) {
		spasm_lu *LU = spasm_LU(A, p, SPASM_DISCARD_L);
		rank += LU->U->n;
		spasm_free_LU(LU);
	} else {
		/* dense schur complement */
		int r = spasm_schur_rank(A, p, qinv, npiv);
		fprintf(stderr, "rank = %d + %d\n", npiv, r);
		rank += npiv + r;
	}

	end_time = spasm_wtime();
	fprintf(stderr, "done in %.3f s rank = %d\n", end_time - start_time, rank);
	spasm_csr_free(A);
	free(p);
	free(qinv);
	return 0;
}
