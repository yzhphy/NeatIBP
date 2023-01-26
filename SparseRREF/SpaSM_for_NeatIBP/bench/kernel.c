#include <stdio.h>
#include <getopt.h>
#include <assert.h>
#include "spasm.h"

/* computes a basis of the left-kernel of the input matrix,
   i.e. the subspace K = { x | x.M == 0 }
*/

int main(int argc, char **argv) {
	spasm_triplet *T;
	spasm *A, *A_t, *K, *A_clean;
	int ch, output, prime, i, n, m, r, h, *p, *qinv;
	double A_density, K_density;

	/* options descriptor */
	struct option longopts[5] = {
		{"verbose", no_argument, NULL, 'v'},
		{"tabulated", no_argument, NULL, 't'},
		{"matrix", no_argument, NULL, 'm'},
		{"modulus", required_argument, NULL, 'p'},
		{NULL, 0, NULL, 0}
	};

	output = 0;
	prime = 42013;

	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
		case 'p':
			prime = atoi(optarg);
			break;
		case 'v':
			output = 0;
			break;
		case 't':
			output = 1;
			break;
		case 'm':
			output = 2;
			break;
		default:
			printf("Unknown option\n");
			exit(1);
		}
	}
	argc -= optind;
	argv += optind;

	T = spasm_load_sms(stdin, prime);
	A = spasm_compress(T);
	spasm_triplet_free(T);
	n = A->n;
	m = A->m;

	/* remove zero rows */
	p = spasm_malloc(n * sizeof(int));
	qinv = spasm_malloc(m * sizeof(int));
	spasm_find_pivots(A, p, qinv);	/* this does some useless stuff, but
					 * pushes zero rows to the bottom */
	A_clean = spasm_permute(A, p, SPASM_IDENTITY_PERMUTATION, SPASM_WITH_NUMERICAL_VALUES);
	spasm_csr_free(A);

	A = A_clean;
	for (i = 0; i < n; i++) {
		if (spasm_row_weight(A, i) == 0) {
			fprintf(stderr, "[kernel] ignoring %d empty rows\n", n - i);
			A->n = i;
			n = i;
			break;
		}
	}

	A_t = spasm_transpose(A, SPASM_WITH_NUMERICAL_VALUES);
	spasm_find_pivots(A_t, qinv, p);

	K = spasm_kernel(A_t, qinv);

	r = K->n;
	K_density = 100.0 * spasm_nnz(K) / (K->n * K->m);
	A_density = 100.0 * spasm_nnz(A) / (A->n * A->m);

	/* identify heaviest row */
	h = -1;
	for (i = 0; i < r; i++) {
		h = spasm_max(h, spasm_row_weight(K, i));
	}

	switch (output) {
	case 0:
		/* output some stats in human-readable form */
		printf("kernel of dimension %d\n", K->n);
		printf("kernel NNZ = %d   /   density = %.2f %%  /   increase wrt A : %.1f\n", spasm_nnz(K), K_density, K_density / A_density);
		printf("heaviest row NNZ = %d   / density = %.2f %%\n", h, 100.0 * h / n);
		break;
	case 1:
		/* output some stats in machine-readable form */
		printf("%d \t %d \t %d\n", K->n, spasm_nnz(K), h);
		break;

	case 2:
		/* output the kernel basis matrix */
		spasm_save_csr(stdout, K);
		break;
	}

	free(p);
	free(qinv);
	spasm_csr_free(A);
	spasm_csr_free(A_t);
	spasm_csr_free(K);
	return 0;
}
