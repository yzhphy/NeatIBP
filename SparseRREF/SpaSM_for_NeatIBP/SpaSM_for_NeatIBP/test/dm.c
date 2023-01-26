#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
	assert(argc > 1);
	int test = atoi(argv[1]);

	spasm_triplet *T = spasm_load_sms(stdin, 42013);
	spasm *A = spasm_compress(T);
	spasm_triplet_free(T);

	int n = A->n;
	int m = A->m;

	/* generate random row & col permutation */
	int *p = spasm_random_permutation(n);
	int *q = spasm_random_permutation(m);
	spasm *B = spasm_permute(A, p, q, SPASM_IGNORE_VALUES);
	free(p);
	free(q);
	spasm_csr_free(A);

	/* compute DM decomposition of permuted M. */
	spasm_dm * DM = spasm_dulmage_mendelsohn(B);
	int *rr = DM->rr;
	int *cc = DM->cc;
	p = DM->p;
	q = DM->q;

	/* check that p and q are actually permutations */
	int *x = spasm_malloc(n * sizeof(int));
	int *y = spasm_malloc(m * sizeof(int));

	spasm_vector_zero(x, n);
	for (int i = 0; i < n; i++)
		x[p[i]]++;
	for (int i = 0; i < n; i++)
		if (x[i] != 1) {
			printf("not ok %d - DM(A) - p is not bijective\n", test);
			exit(0);
		}

	spasm_vector_zero(y, m);
	for (int i = 0; i < m; i++)
		y[q[i]]++;
	for (int i = 0; i < m; i++)
		if (y[i] != 1) {
			printf("not ok %d - DM(A) - q is not bijective\n", test);
			exit(0);
		}
	free(x);
	free(y);

	/* check that coarse decomposition is really block-upper-triangular */
	int *qinv = spasm_pinv(q, m);
	spasm *C = spasm_permute(B, p, qinv, SPASM_IGNORE_VALUES);
	free(qinv);
	spasm_csr_free(B);

	int *Cp = C->p;
	int *Cj = C->j;
	
	for (int i = rr[1]; i < rr[2]; i++)
		for (int px = Cp[i]; px < Cp[i + 1]; px++) {
			int j = Cj[px];
			if (j < cc[2]) {
				printf("not ok %d - DM(A) - row %d (in R_2) has entries in C_0 or C_1\n", test, i);
				exit(0);
			}
		}

	for (int i = rr[2]; i < rr[4]; i++)
		for (int px = Cp[i]; px < Cp[i + 1]; px++) {
			int j = Cj[px];
			if (j < cc[3]) {
				printf("not ok %d - DM(A) - row %d (in R_3 or R_0) has entries in C_0, C_1 or C_2\n", test, i);
				exit(0);
			}
		}

	printf("ok %d - Dulmage-Mendelsohn decomposition\n", test);

	spasm_csr_free(C);
	spasm_dm_free(DM);
	return 0;
}
