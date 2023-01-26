#include <sys/time.h>
#include <err.h>
#include "spasm.h"

int spasm_get_num_threads() {
#ifdef _OPENMP
	return omp_get_num_threads();
#else
	return 1;
#endif
}

int spasm_get_thread_num() {
#ifdef _OPENMP
	return omp_get_thread_num();
#else
	return 0;
#endif
}


double spasm_wtime() {
	struct timeval ts;
	gettimeofday(&ts, NULL);
	return (double)ts.tv_sec + ts.tv_usec / 1E6;
}


int spasm_nnz(const spasm * A) {
	return A->p[A->n];
}

/* return a string representing n in 4 bytes */
void spasm_human_format(int64_t n, char *target) {
	if (n < 1000) {
		sprintf(target, "%lld", n);
		return;
	}
	if (n < 1000000) {
		sprintf(target, "%.1fk", n / 1e3);
		return;
	}
	if (n < 1000000000) {
		sprintf(target, "%.1fm", n / 1e6);
		return;
	}
	if (n < 1000000000000ll) {
		sprintf(target, "%.1fg", n / 1e9);
		return;
	}
	if (n < 1000000000000000ll) {
		sprintf(target, "%.1ft", n / 1e12);
		return;
	}
}

void *spasm_malloc(size_t size) {
	void *x = malloc(size);
	if (x == NULL)
		err(1, "malloc failed");
	return x;
}

void *spasm_calloc(size_t count, size_t size) {
	void *x = calloc(count, size);
	if (x == NULL)
		err(1, "calloc failed");
	return x;
}

void *spasm_realloc(void *ptr, size_t size) {
	void *x = realloc(ptr, size);
	if (ptr != NULL && x == NULL && size != 0)
		err(1, "realloc failed");
	return x;
}

/* allocate a sparse matrix (compressed-row form) */
spasm *spasm_csr_alloc(int n, int m, int nzmax, int prime, int with_values) {
	spasm *A;

	if (prime > 46337) {
		prime = 46337;
		fprintf(stderr, "WARNING: modulus has been set to 46337.\n");
	}
	A = spasm_malloc(sizeof(spasm));	/* allocate the cs struct */
	A->m = m;		/* define dimensions and nzmax */
	A->n = n;
	A->nzmax = nzmax;
	A->prime = prime;
	A->p = spasm_malloc((n + 1) * sizeof(int));
	A->j = spasm_malloc(nzmax * sizeof(int));
	A->x = with_values ? spasm_malloc(nzmax * sizeof(spasm_GFp)) : NULL;
	return A;

}

/* allocate a sparse matrix (triplet form) */
spasm_triplet *spasm_triplet_alloc(int n, int m, int nzmax, int prime, int with_values) {
	spasm_triplet *A;

	A = spasm_malloc(sizeof(spasm_triplet));
	A->m = m;
	A->n = n;
	A->nzmax = nzmax;
	A->prime = prime;
	A->nz = 0;
	A->i = spasm_malloc(nzmax * sizeof(int));
	A->j = spasm_malloc(nzmax * sizeof(int));
	A->x = (with_values ? spasm_malloc(nzmax * sizeof(spasm_GFp)) : NULL);
	return A;
}

/*
 * change the max # of entries in a sparse matrix. If nzmax < 0, then the
 * matrix is trimmed to its current nnz.
 */
void spasm_csr_realloc(spasm * A, int nzmax) {
	if (nzmax < 0)
		nzmax = spasm_nnz(A);
	A->j = spasm_realloc(A->j, nzmax * sizeof(int));
	if (A->x != NULL)
		A->x = spasm_realloc(A->x, nzmax * sizeof(spasm_GFp));
	A->nzmax = nzmax;
}

/*
 * change the max # of entries in a sparse matrix. If nzmax < 0, then the
 * matrix is trimmed to its current nnz.
 */
void spasm_triplet_realloc(spasm_triplet * A, int nzmax) {
	if (nzmax < 0)
		nzmax = A->nz;
	A->i = spasm_realloc(A->i, nzmax * sizeof(int));
	A->j = spasm_realloc(A->j, nzmax * sizeof(int));
	if (A->x != NULL)
		A->x = spasm_realloc(A->x, nzmax * sizeof(spasm_GFp));
	A->nzmax = nzmax;
}

/* free a sparse matrix */
void spasm_csr_free(spasm * A) {
	if (A == NULL)
		return;
	free(A->p);
	free(A->j);
	free(A->x);		/* trick : free does nothing on NULL pointer */
	free(A);
}

void spasm_triplet_free(spasm_triplet * A) {
	free(A->i);
	free(A->j);
	free(A->x);		/* trick : free does nothing on NULL pointer */
	free(A);
}

void spasm_csr_resize(spasm * A, int n, int m) {
	A->m = m;
	/* in case of a column shrink, check that no entries are left outside */
	A->p = spasm_realloc(A->p, (n + 1) * sizeof(int));

	if (A->n < n) {
		int *Ap = A->p;
		for (int i = A->n; i < n + 1; i++)
			Ap[i] = Ap[A->n];
	}
	A->n = n;
}

spasm_dm * spasm_dm_alloc(int n, int m) {
	spasm_dm *P;

	P = spasm_malloc(sizeof(spasm_dm));
	P->p = spasm_malloc(n * sizeof(int));
	P->q = spasm_malloc(m * sizeof(int));
	P->r = spasm_malloc((n + 6) * sizeof(int));
	P->c = spasm_malloc((m + 6) * sizeof(int));
	P->nb = 0;
	for (int i = 0; i < 5; i++) {
		P->rr[i] = 0;
		P->cc[i] = 0;
	}
	return P;
}

void spasm_dm_free(spasm_dm * P) {
	free(P->p);
	free(P->q);
	free(P->r);
	free(P->c);
	free(P);
}

void spasm_vector_zero(spasm_GFp * x, int n) {
	for (int i = 0; i < n; i++)
		x[i] = 0;
}

void spasm_vector_set(spasm_GFp * x, int a, int b, spasm_GFp alpha) {
	for (int i = a; i < b; i++)
		x[i] = alpha;
}
