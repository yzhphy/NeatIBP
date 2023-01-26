/* indent -nfbs -i2 -nip -npsl -di0 -nut spasm_lu.c  */
#include <assert.h>
#include <stdbool.h>
#include "spasm.h"

#ifdef SPASM_TIMING
uint64_t data_shuffling = 0;
#endif

#define DEBUG

/*
 * /!\ the ``row_permutation'' argument is NOT embedded in P. This means that
 * : a) L is ***really*** lower-(triangular/trapezoidal) b) PLUQ =
 * row_permutation*A
 * 
 * TODO : change this.
 */
spasm_lu *spasm_PLUQ(const spasm * A, const int *row_permutation, int keep_L) {
	int k;
	spasm *LL;
	
	int m = A->m;
	spasm_lu *N = spasm_LU(A, row_permutation, keep_L);
	spasm *L = N->L;
	spasm *U = N->U;
	int r = U->n;
	int *Up = U->p;
	int *Uj = U->j;
	int *qinv = N->qinv;

	k = 1;
	for (int j = 0; j < m; j++)
		if (qinv[j] == -1) {
			qinv[j] = m - k;
			k++;
		}

	/*
	 * permute the columns of U in place. U becomes really
	 * upper-trapezoidal.
	 */
	for (int i = 0; i < r; i++)
		for (int px = Up[i]; px < Up[i + 1]; px++)
			Uj[px] = qinv[Uj[px]];

	if (keep_L) {
		/*
		 * permute the rows of L (not in place). L becomes really
		 * lower-trapezoidal.
		 */
		LL = spasm_permute(L, N->p, SPASM_IDENTITY_PERMUTATION, SPASM_WITH_NUMERICAL_VALUES);
		N->L = LL;
		spasm_csr_free(L);
	}
	return N;
}

/* eliminate everything in the (dense) vector x using the pivots found in A */
void spasm_eliminate_sparse_pivots(const spasm * A, const int npiv, const int *p, spasm_GFp * x) {
	int *Aj = A->j;
	int *Ap = A->p;
	spasm_GFp *Ax = A->x;
	int prime = A->prime;

	for (int i = 0; i < npiv; i++) {
		int inew = (p != NULL) ? p[i] : i;
		int j = Aj[Ap[inew]];
		if (x[j] == 0)
			continue;
		spasm_scatter(Aj, Ax, Ap[inew], Ap[inew + 1], prime - x[j], x, prime);
	}
}


/**
 *   Computes a random linear combination of A[k:].
 *   returns TRUE iff it belongs to the row-space of U.
 *   This means that with proba >= 1-1/p, all pivots have been found.
 */
int spasm_early_abort(const spasm * A, const int *p, int k, const spasm * U, int nu) {
	int ok;
	spasm_GFp *y;

	int n = A->n;
	int m = A->m;
	int prime = A->prime;
	int *Aj = A->j;
	int *Ap = A->p;
	spasm_GFp *Ax = A->x;

	y = spasm_malloc(m * sizeof(spasm_GFp));

	spasm_vector_set(y, 0, m, 0);
	for (int i = k; i < n; i++) {
		int inew = (p != NULL) ? p[i] : i;
		spasm_scatter(Aj, Ax, Ap[inew], Ap[inew + 1], rand() % prime, y, prime);
	}

	spasm_eliminate_sparse_pivots(U, nu, SPASM_IDENTITY_PERMUTATION, y);

	/* if y != 0, then y does not belong to the row space of U */
	ok = 1;
	for (int j = 0; j < m; j++) {
		if (y[j] != 0) {
			ok = 0;
			break;
		}
	}
	free(y);
	return ok;
}

/*
 * compute a (somewhat) LU decomposition using the GPLU algorithm.
 * 
 * r = min(n, m) is an upper-bound on the rank of A
 * 
 * L n * r U is r * m
 * 
 * L*U == row_permutation*A
 * 
 * qinv[j] = i if the pivot on column j is on row i. -1 if no pivot (yet) found
 * on column j.
 * 
 */
spasm_lu *spasm_LU(const spasm * A, const int *row_permutation, int keep_L) {
	spasm *L, *U;
	spasm_lu *N;
	

#ifdef SPASM_TIMING
	uint64_t start;
#endif

	/* check inputs */
	assert(A != NULL);

	int n = A->n;
	int m = A->m;
	int *Ap = A->p;
	int *Aj = A->j;
	spasm_GFp *Ax = A->x;

	int r = spasm_min(n, m);
	int prime = A->prime;
	int defficiency = 0;
	int verbose_step = spasm_max(1, n / 1000);

	/* educated guess of the size of L,U */
	int lnz = 4 * spasm_nnz(A) + n;
	int unz = 4 * spasm_nnz(A) + n;

	/* workspace */
	int *x = spasm_malloc(m * sizeof(spasm_GFp));
	int *xj = spasm_malloc(3 * m * sizeof(int));
	

	/* allocate result */
	N = spasm_malloc(sizeof(spasm_lu));
	N->L = L = (keep_L) ? spasm_csr_alloc(n, r, lnz, prime, true) : NULL;
	N->U = U = spasm_csr_alloc(r, m, unz, prime, true);
	N->qinv = spasm_malloc(m * sizeof(int));
	N->p = spasm_malloc(n * sizeof(int));
	int *qinv = N->qinv;
	int *p = N->p;
	int *Lp = (keep_L) ? L->p : NULL;
	int *Up = U->p;

	spasm_vector_set(qinv, 0, m, -1);
	spasm_vector_zero(xj, 3 * m);
	int old_unz = lnz = unz = 0;

	/* initialize early abort */
	int rows_since_last_pivot = 0;
	int early_abort_done = 0;

	/* --- Main loop : compute L[i] and U[i] ------------------- */
	int i;
	for (i = 0; i < n; i++) {
		if (!keep_L && i - defficiency == r) {
			fprintf(stderr, "\n[LU] full rank reached ; early abort\n");
			break;
		}
		if (!keep_L && !early_abort_done && rows_since_last_pivot > 10 && (rows_since_last_pivot > (n / 100))) {
			fprintf(stderr, "\n[LU] testing for early abort...");
			if (spasm_early_abort(A, row_permutation, i + 1, U, i - defficiency)) {
				fprintf(stderr, "SUCCESS\n");
				break;
			} else {
				fprintf(stderr, "FAILED\n");
			}
			early_abort_done = 1;
		}
		/* --- Triangular solve: x * U = A[i] ------------------ */
		if (keep_L)
			Lp[i] = lnz;	/* L[i] starts here */
		Up[i - defficiency] = unz;	/* U[i] starts here */

		/* not enough room in L/U ? realloc twice the size */
		if (keep_L && lnz + m > L->nzmax)
			spasm_csr_realloc(L, 2 * L->nzmax + m);
		if (unz + m > U->nzmax)
			spasm_csr_realloc(U, 2 * U->nzmax + m);
		int *Lj = (keep_L) ? L->j : NULL;
		spasm_GFp *Lx = (keep_L) ? L->x : NULL;
		int *Uj = U->j;
		spasm_GFp *Ux = U->x;

		int inew = (row_permutation != NULL) ? row_permutation[i] : i;

		/* check if the row can be taken directly into U */
		int directly_pivotal = (Ap[inew + 1] > Ap[inew]) && (Ax[Ap[inew]] == 1);
		if (directly_pivotal)
			for (int px = Ap[inew]; px < Ap[inew + 1]; px++)
				if (qinv[Aj[px]] != -1) {
					directly_pivotal = 0;
					break;
				}
		if (directly_pivotal) {
			qinv[Aj[Ap[inew]]] = i - defficiency;
			p[i - defficiency] = i;
			if (keep_L) {
				Lj[lnz] = i - defficiency;
				Lx[lnz] = 1;
				lnz++;
			}
			for (int px = Ap[inew]; px < Ap[inew + 1]; px++) {
				Uj[unz] = Aj[px];
				Ux[unz] = Ax[px];
				unz++;
			}
			rows_since_last_pivot = 0;
			early_abort_done = 0;
			continue;
		}

		int top = spasm_sparse_forward_solve(U, A, inew, xj, x, qinv);

		/* --- Find pivot and dispatch coeffs into L and U ------ */
#ifdef SPASM_TIMING
		start = spasm_ticks();
#endif
		int jpiv = -1;	/* column index of best pivot so far. */


		for (int px = top; px < m; px++) {
			/* x[j] is (generically) nonzero */
			int j = xj[px];

			/*
			 * if x[j] == 0 (numerical cancelation), we just
			 * ignore it
			 */
			if (x[j] == 0)
				continue;
			if (qinv[j] < 0) {
				/* column j is not yet pivotal ? */

				/* have found the pivot on row i yet ? */
				if (jpiv == -1 || j < jpiv)
					jpiv = j;
			} else if (keep_L) {
				/* column j is pivotal */
				/* x[j] is the entry L[i, qinv[j] ] */
				Lj[lnz] = qinv[j];
				Lx[lnz] = x[j];
				lnz++;
			}
		}

		/* pivot found ? */
		if (jpiv != -1) {
			old_unz = unz;

			/* L[i,i] <--- x[jpiv]. Last entry of the row ! */
			if (keep_L) {
				Lj[lnz] = i - defficiency;
				Lx[lnz] = x[jpiv];
				lnz++;
			}
			qinv[jpiv] = i - defficiency;
			p[i - defficiency] = i;

			/* pivot must be the first entry in U[i] */
			Uj[unz] = jpiv;
			Ux[unz] = 1;
			unz++;

			/* send remaining non-pivot coefficients into U */
			spasm_GFp beta = spasm_GFp_inverse(x[jpiv], prime);
			for (int px = top; px < m; px++) {
				int j = xj[px];

				if (qinv[j] < 0) {
					Uj[unz] = j;
					Ux[unz] = (x[j] * beta) % prime;
					unz++;
				}
			}

			/* reset early abort */
			rows_since_last_pivot = 0;
			early_abort_done = 0;
		} else {
			defficiency++;
			p[n - defficiency] = i;
			rows_since_last_pivot++;
		}

#ifdef SPASM_TIMING
		data_shuffling += spasm_ticks() - start;
#endif

		if ((i % verbose_step) == 0) {
			fprintf(stderr, "\rLU : %d / %d [|L| = %d / |U| = %d] -- current density= (%.3f vs %.3f) --- rank >= %d", i, n, lnz, unz, 1.0 * (m - top) / (m), 1.0 * (unz - old_unz) / m, i - defficiency);
			fflush(stderr);
		}
	}

	/*
	 * --- Finalize L and U
	 * -------------------------------------------------
	 */
	fprintf(stderr, "\n");

	/* remove extra space from L and U */
	Up[i - defficiency] = unz;
	spasm_csr_resize(U, i - defficiency, m);
	spasm_csr_realloc(U, -1);

	if (keep_L) {
		Lp[n] = lnz;
		spasm_csr_resize(L, n, n - defficiency);
		spasm_csr_realloc(L, -1);
	}
	free(x);
	free(xj);
	return N;
}


void spasm_free_LU(spasm_lu * X) {
	assert(X != NULL);
	spasm_csr_free(X->L);
	spasm_csr_free(X->U);
	free(X->qinv);
	free(X->p);
	free(X);
}
