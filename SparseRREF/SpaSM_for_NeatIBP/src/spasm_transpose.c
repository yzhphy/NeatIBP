#include "spasm.h"

spasm *spasm_transpose(const spasm * C, int keep_values) {
	int sum, *w;
	spasm *T;

	int m = C->m;
	int n = C->n;
	int *Cp = C->p;
	int *Cj = C->j;
	spasm_GFp *Cx = C->x;

	/* allocate result */
	T = spasm_csr_alloc(m, n, spasm_nnz(C), C->prime, keep_values && (Cx != NULL));
	int *Tp = T->p;
	int *Tj = T->j;
	spasm_GFp *Tx = T->x;

	/* get workspace */
	w = spasm_calloc(m, sizeof(int));

	/* compute column counts */
	for (int i = 0; i < n; i++)
		for (int px = Cp[i]; px < Cp[i + 1]; px++)
			w[Cj[px]]++;

	/* compute column pointers (in both Cp and w) */
	sum = 0;
	for (int j = 0; j < m; j++) {
		Tp[j] = sum;
		sum += w[j];
		w[j] = Tp[j];
	}
	Tp[m] = sum;

	/* dispatch entries */
	for (int i = 0; i < n; i++) {
		for (int px = Cp[i]; px < Cp[i + 1]; px++) {
			int j = Cj[px];
			int py = w[j];
			Tj[py] = i;
			if (Tx != NULL)
				Tx[py] = Cx[px];
			w[j]++;
		}
	}

	free(w);
	return T;
}
