#include <assert.h>
#include "spasm.h"

/*
 * returns the non-trivial (size > 0) connected component of A, seen as an
 * undirected bipartite graph.
 * If the transpose of A is not given (=NULL), it will be computed.
 */
spasm_dm * spasm_connected_components(const spasm * A, spasm * given_At) {
	int n = A->n;
	int m = A->m;
	spasm * A_t = (given_At != NULL) ? given_At : spasm_transpose(A, SPASM_IGNORE_VALUES);
	int * Ap = A->p;
	int * Aj = A->j;
	int * A_tp = A_t->p;
	int * A_tj = A_t->j;

	int * rmark = spasm_malloc(n * sizeof(int));
	int * cmark = spasm_malloc(m * sizeof(int));
	spasm_vector_set(rmark, 0, n, -1);
	spasm_vector_set(cmark, 0, m, -1);

	spasm_dm * P = spasm_dm_alloc(n, m);
	int *p = P->p;
	int *q = P->q;
	int *rr = P->r;
	int *cc = P->c;
	int rhead = 0;
	int rtail = 0;
	int chead = 0;
	int ctail = 0;
	int n_cc = 0;

	for (int root = 0; root < n; root++) {
		if (rmark[root] != -1)
			continue;
	
		if (spasm_row_weight(A, root) == 0)
			continue;

		/* previous block stops here */
		rr[n_cc] = rhead;
		cc[n_cc] = chead;

		/* start BFS from row root */
		p[rtail++] = root;
		rmark[root] = n_cc;

		/* while row queue is not empty */
		while (rhead < rtail) {
			int i = p[rhead++];

			for (int px = Ap[i]; px < Ap[i + 1]; px++) {
				int j = Aj[px];
				if (cmark[j] != -1)
					continue;
				cmark[j] = n_cc;
				q[ctail++] = j;
			}

			/* while col queue is not empty */
			while (chead < ctail) {
				int j = q[chead++];

				for (int px = A_tp[j]; px < A_tp[j + 1]; px++) {
					int i = A_tj[px];
					if (rmark[i] != -1)
						continue;
					rmark[i] = n_cc;
					p[rtail++] = i;
				}
			}
		}

		n_cc++;
	}

	/* add empty / columns */
	for (int i = 0; i < n; i++)
		if (rmark[i] == -1)
			p[rtail++] = i;

	for (int j = 0; j < m; j++)
		if (cmark[j] == -1)
			q[ctail++] = j;

	assert(rtail == n);
	assert(ctail == m);

	/* finalize */
	rr[n_cc] = n;
	cc[n_cc] = m;
	P->nb = n_cc;

	/* cleanup */
	free(rmark);
	free(cmark);
	if (given_At == NULL)
		spasm_csr_free(A_t);
	return P;
}