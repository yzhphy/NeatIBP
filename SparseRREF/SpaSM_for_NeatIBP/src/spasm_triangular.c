/* indent -nfbs -i2 -nip -npsl -di0 -nut spasm_triangular.c */
#include <assert.h>
#include "spasm.h"

#ifdef SPASM_TIMING
#include "cycleclock.h"
uint64_t reach = 0, scatter = 0;
#endif


int spasm_is_upper_triangular(const spasm * A) {
	int i, p, n, m, *Aj, *Ap;
	spasm_GFp *Ax;

	n = A->n;
	m = A->m;
	if (n > m) {
		return 0;
	}
	Ap = A->p;
	Aj = A->j;
	Ax = A->x;
	for (i = 0; i < n; i++) {
		/* check diagonal */
		if (Aj[Ap[i]] != i) {
			return 0;
		}
		if (Ax[Ap[i]] != 1) {
			return 0;
		}
		/* check other entries */
		for (p = Ap[i] + 1; p < Ap[i + 1]; p++) {
			if (Aj[p] < i) {
				return 0;
			}
		}
	}
	return 1;
}


int spasm_is_lower_triangular(const spasm * A) {
	int i, n, m, p, *Aj, *Ap;
	spasm_GFp *Ax;

	n = A->n;
	m = A->m;
	if (n < m) {
		return 0;
	}
	Ap = A->p;
	Aj = A->j;
	Ax = A->x;
	for (i = 0; i < m; i++) {

		/* check diagonal */
		if (Aj[Ap[i + 1] - 1] != i) {
			return 0;
		}
		if (Ax[Ap[i + 1] - 1] == 0) {
			return 0;
		}
		/* check other entries */
		for (p = Ap[i]; p < Ap[i + 1] - 1; p++) {
			if (Aj[p] > i) {
				return 0;
			}
		}
	}
	return 1;
}


/*
 * Solving triangular systems, dense RHS
 */


/*
 * dense backwards substitution solver. Solve x . L = b where x and b are
 * dense.
 * 
 * b is undefined on output
 * 
 * L is assumed to be lower-triangular, with non-zero diagonal.
 * 
 * The diagonal entry is the **last** of each row. More precisely, L[j,j] is Lx[
 * Lp[j+1] - 1 ]
 * 
 * p[j] == i indicates if the "diagonal" entry on column j is on row i
 * 
 */
void spasm_dense_back_solve(const spasm * L, spasm_GFp * b, spasm_GFp * x, const int *p) {
	int i, j, n, m, *Lp, *Lj, prime;
	spasm_GFp *Lx;

	/* check inputs */
	assert(b != NULL);
	assert(x != NULL);
	assert(L != NULL);

	n = L->n;
	m = L->m;
	Lp = L->p;
	Lj = L->j;
	Lx = L->x;
	prime = L->prime;

	for (i = 0; i < n; i++) {
		x[i] = 0;
	}

	for (j = m - 1; j >= 0; j--) {
		i = (p != SPASM_IDENTITY_PERMUTATION) ? p[j] : j;

		/* pivot on the j-th column is on the i-th row */
		const spasm_GFp diagonal_entry = Lx[Lp[i + 1] - 1];
		//assert(diagonal_entry == 1);

		/* axpy - inplace */
		x[i] = (b[j] * spasm_GFp_inverse(diagonal_entry, prime)) % prime;
		spasm_scatter(Lj, Lx, Lp[i], Lp[i + 1] - 1, prime - x[i], b, prime);
	}
}

/*
 * dense forwards substitution solver. Solve x . U = b where x and b are
 * dense.
 * 
 * b is undefined on output
 * 
 * U is upper-triangular
 * 
 * Assumption : the diagonal entry is always present, is always != 0.
 * 
 * The diagonal entry is the first one of each row. More precisely, U[i,i] is
 * Ux[ Up[i] ]
 * 
 * if q != SPASM_IDENTITY_PERMUTATION, then q[i] indicates the column on which
 * the i-th row pivot is.
 * 
 * returns SPASM_SUCCESS or SPASM_NO_SOLUTION
 */
int spasm_dense_forward_solve(const spasm * U, spasm_GFp * b, spasm_GFp * x, const int *q) {
	int i, j, n, m, *Up, *Uj, prime;
	spasm_GFp *Ux;

	/* check inputs */
	assert(b != NULL);
	assert(x != NULL);
	assert(U != NULL);

	n = U->n;
	m = U->m;
	assert(n <= m);

	Up = U->p;
	Uj = U->j;
	Ux = U->x;
	prime = U->prime;

	for (i = 0; i < n; i++) {
		x[i] = 0;
	}

	for (i = 0; i < n; i++) {
		j = (q != SPASM_IDENTITY_PERMUTATION) ? q[i] : i;
		if (b[j] != 0) {
			/* check diagonal entry */
			const spasm_GFp diagonal_entry = Ux[Up[i]];
			assert(diagonal_entry == 1);

			/* axpy - inplace */
			x[i] = b[j];
			spasm_scatter(Uj, Ux, Up[i] + 1, Up[i + 1], prime - x[i], b, prime);
			b[j] = 0;
		}
	}

	for (i = 0; i < m; i++) {
		if (b[i] != 0) {
			return SPASM_NO_SOLUTION;
		}
	}

	return SPASM_SUCCESS;
}



/*************** Triangular solving with sparse RHS
 *
 * solve x * U = B[k], where U is (permuted) upper triangular.
 *
 * x has size m (number of columns of U, paradoxically).
 *
 * when this function returns, the solution is scattered in x, and its pattern
 * is given in xj[top : m].
 *
 * xj must be zero-initialized on the first call (and it stays OK)
 * x does not need to be initialized.
 *
 * top is the return value.
 */
int spasm_sparse_forward_solve(const spasm * U, const spasm * B, int k, int *xj, spasm_GFp * x, const int *qinv) {
	int top, m, prime, *Up, *Uj, *Bp, *Bj;
	spasm_GFp *Ux, *Bx;

#ifdef SPASM_TIMING
	uint64_t start;
#endif

	m = U->m;
	Up = U->p;
	Uj = U->j;
	Ux = U->x;
	prime = U->prime;

	Bp = B->p;
	Bj = B->j;
	Bx = B->x;

#ifdef SPASM_TIMING
	start = spasm_ticks();
#endif

	/* xj[top : m] = Reach(U, B[k]) */
	top = spasm_reach(U, B, k, m, xj, qinv);

#ifdef SPASM_TIMING
	reach += spasm_ticks() - start;
#endif

	/* clear x */
	for (int px = top; px < m; px++)
		x[xj[px]] = 0;

	/* scatter B[k] into x */
	for (int px = Bp[k]; px < Bp[k + 1]; px++)
		x[Bj[px]] = Bx[px];

#ifdef SPASM_TIMING
	start = spasm_ticks();
#endif

	/* iterate over the (precomputed) pattern of x (= the solution) */
	for (int px = top; px < m; px++) {
		/* x[j] is nonzero */
		int j = xj[px];

		/* locate corresponding pivot if there is any */
		int i = (qinv != NULL) ? (qinv[j]) : j;
		if (i < 0)
			continue;

		/*
		 * the pivot entry on row i is 1, so we just have to multiply
		 * by -x[j]
		 */
		assert(Ux[Up[i]] == 1);
		spasm_scatter(Uj, Ux, Up[i] + 1, Up[i + 1], prime - x[j], x, prime);
	}

#ifdef SPASM_TIMING
	scatter += spasm_ticks() - start;
#endif

	return top;
}



/*************** Triangular solving with sparse RHS
 *
 * solve x * L = B[k], where L is (permuted) lower triangular.
 *
 * x has size m (number of columns of L).
 *
 * when this function returns, the solution is scattered in x, and its pattern
 * is given in xi[top : n].
 *
 * top is the return value.
 *
 */
int spasm_sparse_backward_solve(const spasm * L, const spasm * B, int k, int *xi, spasm_GFp * x, const int *pinv, int r_bound) {
	int i, I, p, px, top, n, m, prime, *Lp, *Lj, *Bp, *Bj, tmp;
	spasm_GFp *Lx, *Bx;

#ifdef SPASM_TIMING
	uint64_t start;
#endif

	assert(L != NULL);
	assert(B != NULL);
	assert(xi != NULL);
	assert(x != NULL);

	n = L->n;
	m = L->m;
	Lp = L->p;
	Lj = L->j;
	Lx = L->x;
	prime = L->prime;

	Bp = B->p;
	Bj = B->j;
	Bx = B->x;

#ifdef SPASM_TIMING
	start = spasm_ticks();
#endif
	/* xi[top : m] = Reach( L, B[k] ) */
	top = spasm_reach(L, B, k, n, xi, pinv);

#ifdef SPASM_TIMING
	reach += spasm_ticks() - start;
#endif

	/* clear x */
	for (p = top; p < n; p++) {
		x[xi[p]] = 0;
	}

	/* scatter B[k] into x */
	for (p = Bp[k]; p < Bp[k + 1]; p++) {
		x[Bj[p]] = Bx[p];
	}

	/* iterate over the (precomputed) pattern of x (= the solution) */
#ifdef SPASM_TIMING
	start = spasm_ticks();
#endif

	for (px = top; px < n; px++) {
		/* x[i] is nonzero */
		i = xi[px];

		/* i maps to row I of L */
		I = (pinv == SPASM_IDENTITY_PERMUTATION) ? i : pinv[i];

		if (i >= m) {
			/* column I is part of an implicit identity matrix */
			spasm_scatter(Lj, Lx, Lp[I], Lp[I + 1], prime - x[i], x, prime);
		} else if (i >= r_bound) {
			/* get L[i,i] */
			const spasm_GFp diagonal_entry = Lx[Lp[I + 1] - 1];
			assert(diagonal_entry != 0);
			/* axpy-in-place */
			x[i] = (x[I] * spasm_GFp_inverse(diagonal_entry, prime)) % prime;
			spasm_scatter(Lj, Lx, Lp[I], Lp[I + 1] - 1, prime - x[i], x, prime);
		}
		xi[px] = I;
		tmp = x[i];
		x[i] = 0;
		x[xi[px]] = tmp;

	}

#ifdef SPASM_TIMING
	scatter += spasm_ticks() - start;
#endif

	return top;
}
