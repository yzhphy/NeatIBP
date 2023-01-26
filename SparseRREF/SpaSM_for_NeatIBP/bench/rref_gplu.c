#include <stdio.h>
#include <assert.h>
#include <getopt.h>

#include "spasm.h"

#ifdef SPASM_TIMING
extern int64 reach, scatter, data_shuffling;
#endif
#include <unistd.h>

/* compute an echelonized form, WITHOUT COLUMN PERMUTATION */
int main(int argc, char **argv) 
{
	int prime = 42013;
	
	/* options descriptor */
	struct option longopts[4] = {
		{"modulus", required_argument, NULL, 'p'},
		{NULL, 0, NULL, 0}
	};

	char ch;
	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
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

	spasm_triplet *T = spasm_load_sms(stdin, prime);
	spasm_triplet_transpose(T);
	spasm *A = spasm_compress(T);
	spasm_triplet_free(T);

	double start_time = spasm_wtime();

	spasm_lu *LU = spasm_LU(A, SPASM_IDENTITY_PERMUTATION, 1);
	fprintf(stderr, "(non-reduced) REF done in %.1fs\n", spasm_wtime() - start_time);
	
	spasm *U = spasm_transpose(LU->L, 1);
	spasm_make_pivots_unitary(U, SPASM_IDENTITY_PERMUTATION, U->n);

	spasm_save_csr(stdout, U);
	spasm_free_LU(LU);
	spasm_csr_free(A);
	spasm_csr_free(U);
	return 0;
}
