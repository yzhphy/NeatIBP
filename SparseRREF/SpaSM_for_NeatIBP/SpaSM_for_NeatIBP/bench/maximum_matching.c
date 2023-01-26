#include <stdio.h>
#include <getopt.h>
#include <assert.h>
#include "spasm.h"

/** finds a maximum maching between the rows and columns and output the permuted matrix.
    The matched part can also be extracted. */

int main(int argc, char **argv) {
	spasm_triplet *T;
	spasm *A, *B;
	int k, n, m, r;
	int *p, *qinv, *imatch, *jmatch;
	double start_time;
	int transposed = 0;

	struct option longopts[2] = {
		{"extract", no_argument, NULL, 'e'},
		{NULL, 0, NULL, 0}
	};

	int ch, mode = 'p';
	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
		case 'e':
			mode = 'e';
			break;
		default:
			printf("Unknown option\n");
			exit(1);
		}
	}

	T = spasm_load_sms(stdin, 42013);
	fprintf(stderr, "A : %d x %d with %d nnz (density = %.3f %%)\n", T->n, T->m, T->nz, 100.0 * T->nz / (1.0 * T->n * T->m));

	n = T->n;
	m = T->m;

	if (n > m) {
		fprintf(stderr, "[matching - info] transposing matrix to speed-up maximum matching\n");
		spasm_triplet_transpose(T);
		transposed = 1;
	}
	A = spasm_compress(T);

	jmatch = spasm_malloc(T->n * sizeof(int));
	imatch = spasm_malloc(T->m * sizeof(int));

	fprintf(stderr, "[matching] maximum matching...");
	fflush(stderr);
	
	start_time = spasm_wtime();
	r = spasm_maximum_matching(A, jmatch, imatch);
	fprintf(stderr, " %.1f s\n", spasm_wtime() - start_time);

	if (transposed) {
		spasm_csr_free(A);
		spasm_triplet_transpose(T);
		A = spasm_compress(T);
		p = imatch;
		imatch = jmatch;
		jmatch = p;
	}
	
	spasm_triplet_free(T);

	if (r < spasm_min(n, m)) {
		fprintf(stderr, "[matching] matrix is structurally rank-deficient. Structural rank: %d\n", r);
	} else if (n == m) {
		fprintf(stderr, "[matching] matrix is square and structurally full-rank\n");
	} else if (n < m) {
		fprintf(stderr, "[matching] matrix has full structural row-rank\n");
	} else {
		fprintf(stderr, "[matching] matrix has full structural column-rank\n");
	}

	/* build the permutations */
	p = spasm_malloc(n * sizeof(int));
	qinv = spasm_malloc(m * sizeof(int));
	k = 0;

	/* put matched rows and columns first, in-order */
	for (int i = 0; i < n; i++) {
		int j = jmatch[i];
		if (j == -1)	/* row i is unmatched */
			continue;

		p[k] = i;
		qinv[j] = k;
		k++;
	}
	assert(k == r);

	/* add unmatched rows */
	for (int i = 0; i < n; i++)
		if (jmatch[i] == -1)
			p[k++] = i;
	assert(k == n);

	/* add unmatched columnss */
	k = r;
	for (int j = 0; j < m; j++)
		if (imatch[j] == -1)
			qinv[j] = k++;
	assert(k == m);

	B = spasm_permute(A, p, qinv, SPASM_WITH_NUMERICAL_VALUES);
	spasm_csr_free(A);
	
	if (mode == 'p')
		/* print permuted matrix */
		spasm_save_csr(stdout, B);

	if (mode == 'e') {
		/* extract matched part */
		spasm *C = spasm_submatrix(B, 0, r, 0, r, SPASM_WITH_NUMERICAL_VALUES);
		spasm_save_csr(stdout, C);
		spasm_csr_free(C);
	}
	spasm_csr_free(B);
	
	free(p);
	free(qinv);
	free(imatch);
	free(jmatch);
	return 0;
}
