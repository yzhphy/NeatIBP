#include <stdio.h>
#include <assert.h>
#include "spasm.h"
#include <metis.h>

/** Uses METIS to find a row separator, and outputs the permuted matrix */

int main() {
	spasm_triplet *T;
	spasm *A, *B;
	int n, m, i, j, result, *Ap, *Aj, k, npart, edgecut, *row_part,
	   *col_part, *p, *q, *qinv, px;

	T = spasm_load_sms(stdin, 42013);
	A = spasm_compress(T);
	spasm_triplet_free(T);

	/*
	 * in METIS mesh/hypergraph words : "element" = row and "node" =
	 * column
	 */
	n = A->n;
	m = A->m;
	Ap = A->p;
	Aj = A->j;
	npart = 2;
	row_part = spasm_malloc(n * sizeof(int));
	col_part = spasm_malloc(m * sizeof(int));
	p = spasm_malloc(n * sizeof(int));
	q = spasm_malloc(m * sizeof(int));

	result = METIS_PartMeshNodal(&n, &m, Ap, Aj, NULL, NULL, &npart, NULL, NULL, &edgecut, row_part, col_part);

	if (result != METIS_OK) {
		fprintf(stderr, "something went wrong\n");
		exit(1);
	}
	/* find (row) separator */
	k = 0;
	for (int i = 0; i < n; i++)
		for (int px = Ap[i]; px < Ap[i + 1]; px++)
			if (row_part[i] != col_part[Aj[px]]) {
				row_part[i] = npart;	/* row i belongs to
							 * separator */
				k++;
				break;
			}
	fprintf(stderr, "size of row separator: %d  (%.1f %% of rows)\n", k, 100.0 * k / n);

	/* Now, we output the permuted matrix */

	/* build p */
	px = 0;
	for (k = 0; k < npart + 1; k++) {
		for (i = 0; i < n; i++) {
			if (row_part[i] == k) {
				p[px] = i;
				px++;
			}
		}
	}

	/* deal with empty rows */
	for (i = 0; i < n; i++) {
		if (row_part[i] < 0) {
			p[px] = i;
			px++;
		}
	}
	assert(px == n);

	/* build q */
	px = 0;
	for (k = 0; k < npart; k++) {
		for (j = 0; j < m; j++) {
			if (col_part[j] == k) {
				q[px] = j;
				px++;
			}
		}
	}
	assert(px == m);


	qinv = spasm_pinv(q, m);
	B = spasm_permute(A, p, qinv, SPASM_WITH_NUMERICAL_VALUES);
	spasm_save_csr(stdout, B);

	free(row_part);
	free(col_part);
	free(p);
	free(q);
	free(qinv);
	spasm_csr_free(B);
	spasm_csr_free(A);
	return 0;
}
