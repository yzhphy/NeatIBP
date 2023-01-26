/* indent -nfbs -i2 -nip -npsl -di0 -nut spasm_triplet.c */
#include <assert.h>
#include "spasm.h"

/* add an entry to a triplet matrix; enlarge it if necessary */
void spasm_add_entry(spasm_triplet * T, int i, int j, spasm_GFp x) {
	spasm_GFp x_p;

	assert((i >= 0) && (j >= 0));
	int prime = T->prime;

	if (T->nz == T->nzmax)
		spasm_triplet_realloc(T, 1 + 2 * T->nzmax);
	if (T->x != NULL) {
		x_p = ((x % prime) + prime) % prime;
		if (x_p == 0)
			return;
		T->x[T->nz] = x_p;
	}

	// fprintf(stderr, "Adding (%d, %d, %d)\n", i, j, x);

	T->i[T->nz] = i;
	T->j[T->nz] = j;
	T->nz += 1;
	T->n = spasm_max(T->n, i + 1);
	T->m = spasm_max(T->m, j + 1);
}

void spasm_triplet_transpose(spasm_triplet * T) {
	int nz = T->nz;
	int *Ti = T->i;
	int *Tj = T->j;
#pragma omp parallel for schedule(static)
	for (int k = 0; k < nz; k++) {
		int i = Ti[k];
		int j = Tj[k];
		Tj[k] = i;
		Ti[k] = j;
	}
	int tmp = T->m;
	T->m = T->n;
	T->n = tmp;
}


/* in-place */
void spasm_deduplicate(spasm * A) {
	int m = A->m;
	int n = A->n;
	int *Ap = A->p;
	int *Aj = A->j;
	spasm_GFp *Ax = A->x;
	int prime = A->prime;

	int *v = spasm_malloc(m * sizeof(*v));
	for (int j = 0; j < m; j++)
		v[j] = -1;

	int nz = 0;
	for (int i = 0; i < n; i++) {
		int p = nz;
		for (int it = Ap[i]; it < Ap[i + 1]; it++) {
			int j = Aj[it];
			if (v[j] < p) { /* occurs in previous row */
				v[j] = nz;
				Aj[nz] = j;
				if (Ax)
					Ax[nz] = Ax[it];
				nz++;
			} else {
				if (Ax)
					Ax[v[j]] = (Ax[v[j]] + Ax[it]) % prime;
			}
		}
		Ap[i] = p;
	}
	Ap[n] = nz;
	free(v);
	spasm_csr_realloc(A, -1);
}

/* C = compressed-row form of a triplet matrix T */
spasm *spasm_compress(const spasm_triplet * T) {
	int m = T->m;
	int n = T->n;
	int nz = T->nz;
	int *Ti = T->i;
	int *Tj = T->j;
	spasm_GFp *Tx = T->x;
	
	double start = spasm_wtime();
	fprintf(stderr, "[CSR] Compressing... ");
	fflush(stderr);

	/* allocate result */
	spasm *C = spasm_csr_alloc(n, m, nz, T->prime, Tx != NULL);

	/* get workspace */
	int *w = spasm_calloc(n, sizeof(int));
	int *Cp = C->p;
	int *Cj = C->j;
	spasm_GFp *Cx = C->x;

	/* compute row counts */
	for (int it = 0; it < nz; it++)
		w[Ti[it]]++;

	/* compute row pointers (in both Cp and w) */
	int sum = 0;
	for (int k = 0; k < n; k++) {
		Cp[k] = sum;
		sum += w[k];
		w[k] = Cp[k];
	}
	Cp[n] = sum;

	/* dispatch entries */
	for (int k = 0; k < nz; k++) {
		int px = w[Ti[k]]++;
		Cj[px] = Tj[k];
		if (Cx != NULL)
			Cx[px] = Tx[k];
	}
	free(w);
	spasm_deduplicate(C);

	/* success; free w and return C */
	char mem[16];
	int size = sizeof(int) * (n + nz) + sizeof(spasm_GFp) * ((Cx != NULL) ? nz : 0);
	spasm_human_format(size, mem);
	fprintf(stderr, "%d actual NZ, Mem usage = %sbyte [%.2fs]\n", spasm_nnz(C), mem, spasm_wtime() - start);
	return C;
}