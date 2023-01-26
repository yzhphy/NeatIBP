#include <stdio.h>
#include <assert.h>
#include <getopt.h>

#include "spasm.h"

#ifdef SPASM_TIMING
extern int64 reach, scatter, data_shuffling;
#endif
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>

/* computes the rank of the input matrix via a LU factorization using the GPLU algorithm */


jmp_buf Env;

void alarm_handler(int dummy) {
	dummy++;
	longjmp(Env, 1);
}



int main(int argc, char **argv) {
	spasm_triplet *T;
	spasm *A, *U, *L;
	spasm_lu *LU;
	int r, n, m, ch, prime, keep_L, timer, npiv;
	int *qinv, *p;
	double start_time, end_time;

	prime = 42013;
	keep_L = 0;
	timer = -1;

	/* options descriptor */
	struct option longopts[4] = {
		{"modulus", required_argument, NULL, 'p'},
		{"max-time", required_argument, NULL, 't'},
		{"keep-L", no_argument, NULL, 'k'},
		{NULL, 0, NULL, 0}
	};

	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
		case 'p':
			prime = atoi(optarg);
			break;
		case 't':
			timer = atoi(optarg);
			break;
		case 'k':
			keep_L = 1;
			break;
		default:
			printf("Unknown option\n");
			exit(1);
		}
	}
	argc -= optind;
	argv += optind;

	T = spasm_load_sms(stdin, prime);
	if (T->n < T->m) {
		fprintf(stderr, "[rank] transposing matrix\n");
		spasm_triplet_transpose(T);
	}
	A = spasm_compress(T);
	spasm_triplet_free(T);
	n = A->n;
	m = A->m;
	start_time = spasm_wtime();

	qinv = spasm_malloc(m * sizeof(int));
	p = spasm_malloc(n * sizeof(int));
	npiv = spasm_find_pivots(A, p, qinv);
	spasm *B = spasm_permute_pivots(A, p, qinv, npiv);

	spasm_csr_free(A);
	free(p);
	free(qinv);
	p = SPASM_IDENTITY_PERMUTATION;
	A = B;

	fprintf(stderr, "[rank] finding pivots: %.1f s\n", spasm_wtime() - start_time);

	if (timer > 0) {
		signal(SIGALRM, alarm_handler);
		alarm(timer);

		if (setjmp(Env) != 0) {
			fprintf(stderr, "\nTimeout\n");
			exit(2);
		}
	}
	LU = spasm_LU(A, p, keep_L);
	end_time = spasm_wtime();

	U = LU->U;
	r = U->n;

	fprintf(stderr, "U :  %d x %d with %d nnz (density = %.1f %%)\n", r, m, spasm_nnz(U), 100.0 * spasm_nnz(U) / (1.0 * r * m - r * r / 2.0));
	if (LU->L != NULL) {
		L = LU->L;
		fprintf(stderr, "L :  %d x %d with %d nnz (density = %.1f %%)\n", L->n, r, spasm_nnz(L), 100.0 * spasm_nnz(L) / (1.0 * r * n - r * r / 2.0));
	}
#ifdef SPASM_TIMING
	fprintf(stderr, "----------------------------------------\n");
	fprintf(stderr, "reach   : %12" PRId64 "\n", reach);
	fprintf(stderr, "scatter : %12" PRId64 "\n", scatter);
	fprintf(stderr, "misc    : %12" PRId64 "\n", data_shuffling);
	fprintf(stderr, "----------------------------------------\n");
#endif

	printf("rank of A = %d [%.1fs]\n", r, end_time - start_time);
	free(p);
	spasm_free_LU(LU);
	spasm_csr_free(A);
	return 0;
}
