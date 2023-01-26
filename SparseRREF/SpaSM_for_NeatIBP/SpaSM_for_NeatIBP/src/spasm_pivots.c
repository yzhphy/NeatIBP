#include <assert.h>
#include "spasm.h"

/*
 * General convention: if a row is pivotal, then the pivot is the first entry of the row.
 * If some pivots have been found, then they are passed from one routine to the other through
 * three variables:
 *   - npiv (int)             : the number of pivots found)
 *   - p (array of size n)    : rows p[0], ..., p[npiv - 1] are pivotal
 *   - qinv (array of size m) : if qinv[j] == -1, then column j is not pivotal;
 *                                 otherwise, the pivot is on row qinv[j].
 *
 * 
 */


/* test if a pivot has already been found on row i (if so, it's the first entry of the row ) */
int spasm_is_row_pivotal(const spasm * A, const int *qinv, const int i)
{

	int * Ap = A->p;
	int * Aj = A->j;

	/* test for empty row before reading Aj[px] */
	if (Ap[i + 1] == Ap[i])
		return 0;
	int j = Aj[Ap[i]];  // first row entry
	return (qinv[j] == i);
}

/* make pivot the first entry of the row */
void spasm_prepare_pivot(spasm * A, const int i, const int px)
{
	int *Ap = A->p;
	int *Aj = A->j;
	spasm_GFp *Ax = A->x;

	spasm_swap(Aj, Ap[i], px);
	if (Ax != NULL)
		spasm_swap(Ax, Ap[i], px);
}


/** Faugère-Lachartre pivot search.
 *
 * The leftmost entry of each row is a candidate pivot. Select the sparsest row
 * with a leftmost entry on the given column.

 * @param qinv must be initialized to -1
 * @param p can be arbitrary.
 * @return number of pivots found. 
 */
int spasm_find_FL_pivots(spasm * A, int *p, int *qinv)
{
	int n = A->n;
	int m = A->m;
	int * Ap = A->p;
	int * Aj = A->j;
	double start = spasm_wtime();

	for (int i = 0; i < n; i++) {
		int j = -1;
		int idx_j;
		for (int px = Ap[i]; px < Ap[i + 1]; px++)
			if (j == -1 || Aj[px] < j) {
				j = Aj[px];
				idx_j = px;
			}
		if (j == -1)	/* Skip empty rows */
			continue;

		/* check if it is a sparser pivot */
		if (qinv[j] == -1 || spasm_row_weight(A, i) < spasm_row_weight(A, qinv[j])) {
			qinv[j] = i;
			spasm_prepare_pivot(A, i, idx_j);
		}
	}

	/* build p */
	int npiv = 0;
	for (int j = 0; j < m; j++)
		if (qinv[j] != -1)
			p[npiv++] = qinv[j];

	fprintf(stderr, "[pivots] Faugère-Lachartre: %d pivots found [%.1fs]\n", npiv, spasm_wtime() - start);
	return npiv;
}



/*
 * Leftovers from FL. Column not occuring on previously selected pivot row
 * can be made pivotal, as this will not create alternating cycles.
 * 
 * w[j] = 1 <===> column j does not appear in a pivotal row
 * 
 */
int spasm_find_FL_column_pivots(spasm * A, int *p, int *qinv, int npiv_fl)
{
	int n = A->n;
	int m = A->m;
	int * Ap = A->p;
	int * Aj = A->j;
	int npiv = npiv_fl;
	int *w = spasm_malloc(m * sizeof(int));
	spasm_vector_set(w, 0, m, 1);

	double start = spasm_wtime();

	/* mark columns on previous pivot rows as obstructed */
	for (int i = 0; i < npiv; i++) {
		int inew = p[i];
		for (int px = Ap[inew]; px < Ap[inew + 1]; px++)
			w[Aj[px]] = 0;
	}

	/* find new pivots */
	for (int i = 0; i < n; i++) {
		if (spasm_is_row_pivotal(A, qinv, i))
			continue;

		/* does A[i,:] have an entry on an unobstructed column? */
		for (int px = Ap[i]; px < Ap[i + 1]; px++) {
			int j = Aj[px];
			if (w[j] == 0)
				continue;	/* this column is closed,
						 * skip this entry */

			/* new pivot found! */
			if (qinv[j] == -1) {
				p[npiv++] = i;
				qinv[j] = i;
				spasm_prepare_pivot(A, i, px);
				/*
				 * mark the columns occuring on this row as
				 * unavailable
				 */
				for (int px = Ap[i]; px < Ap[i + 1]; px++)
					w[Aj[px]] = 0;

				break;
			}
		}
	}
	free(w);

	fprintf(stderr, "[pivots] ``Faugère-Lachartre on columns'': %d pivots found [%.1fs]\n", npiv - npiv_fl, spasm_wtime() - start);
	return npiv;
}



int find_survivor(spasm * A, int i, char *w) 
{
	int *Ap = A->p;
	int *Aj = A->j;

	for (int px = Ap[i]; px < Ap[i + 1]; px++) {
		int j = Aj[px];
		if (w[j] == 1) { /* potential pivot found */
			spasm_prepare_pivot(A, i, px);
			return j;
		}
	}
	return -1;
}

/*
 * provide already know pivots, and this looks for more. Updates qinv, but
 * DFS must be performed afterwards
 */
static inline void BFS_enqueue(char *w, int *queue, int *surviving, int *tail, int j)
{
	queue[(*tail)++] = j;
	*surviving -= w[j];
	w[j] = -1;
}

static inline void BFS_enqueue_row(char *w, int *queue, int *surviving, int *tail, const int *Ap, const int *Aj, int i) 
{
	for (int px = Ap[i]; px < Ap[i + 1]; px++) {
		/* this is the critical section */
		int j = Aj[px];
		if (w[j] >= 0)
			BFS_enqueue(w, queue, surviving, tail, j);
	}
}

int spasm_find_cycle_free_pivots(spasm * A, int *p, int *qinv, int npiv_start)
{
	int n = A->n;
	int m = A->m;
	int * Ap = A->p;
	int * Aj = A->j;
	int v = spasm_max(1, spasm_min(1000, n / 100));
	int processed = 0;
	int retries = 0;
	int npiv = npiv_start;
	double start = spasm_wtime();

#pragma omp parallel
	{
		char *w = spasm_malloc(m * sizeof(char));
		int *queue = spasm_malloc(m * sizeof(int));
		int head, tail, npiv_local, surviving, tid;

		/* workspace initialization */
		tid = spasm_get_thread_num();
		for(int j = 0; j < m; j++)
			w[j] = 0;

#pragma omp for schedule(dynamic, 1000)
		for (int i = 0; i < n; i++) {
			/*
			 * for each non-pivotal row, computes the columns
			 * reachable from its entries by alternating paths.
			 * Unreachable entries on the row can be chosen as
			 * pivots. The w[] array is used for marking during
			 * the graph traversal. 
			 * Before the search: 
			 *   w[j] == 1 for each non-pivotal entry j on the row 
			 *   w[j] == 0 otherwise 
			 * After the search: 
			 *   w[j] ==  1  for each unreachable non-pivotal entry j on the row
			 *                 (candidate pivot) 
			 *   w[j] == -1  column j is reachable by an alternating path,
			 *                 or is pivotal (has entered the queue at some point) 
			 *   w[j] ==  0  column j was absent and is unreachable
			 */
			if ((tid == 0) && (i % v) == 0) {
				fprintf(stderr, "\r[pivots] %d / %d --- found %d new", processed, n - npiv_start, npiv - npiv_start);
				fflush(stderr);
			}
			if (spasm_is_row_pivotal(A, qinv, i))
				continue;

#pragma omp atomic update
			processed++;

			/* we start reading qinv: begining of transaction */
#pragma omp atomic read
			npiv_local = npiv;
			/* scatters columns of A[i] into w, enqueue pivotal entries */
			head = 0;
			tail = 0;
			surviving = 0;
			for (int px = Ap[i]; px < Ap[i + 1]; px++) {
				int j = Aj[px];
				if (qinv[j] < 0) {
					w[j] = 1;
					surviving++;
				} else {
					BFS_enqueue(w, queue, &surviving, &tail, j);
				}
			}

			/* BFS. This is where most of the time is spent */
	BFS:
			while (head < tail && surviving > 0) {
				int j = queue[head++];
				int I = qinv[j];
				if (I == -1)
					continue;	/* j is not pivotal: nothing to do */
				BFS_enqueue_row(w, queue, &surviving, &tail, Ap, Aj, I);
			}

			/* scan w for surviving entries */
			if (surviving > 0) {
				int j = find_survivor(A, i, w);
				int npiv_target = -1;

				/*
				 * si aucun nouveau pivot n'est arrivé, ajouter ...
				 */
#pragma omp critical
				{
					if (npiv == npiv_local) {
						qinv[j] = i;
						p[npiv] = i;
#pragma omp atomic update
						npiv++;
					} else {
#pragma omp atomic read
						npiv_target = npiv;
						retries++;
					}
				}

				if (npiv_target < 0)
					goto cleanup;

				/*
				 * si on a découvert de nouveaux pivots
				 * aiter... les traiter !
				 */
				for (; npiv_local < npiv_target; npiv_local++) {
					int I = p[npiv_local];
					int j = Aj[Ap[I]];
					if (w[j] == 0)	/* the new pivot plays
							 * no role here */
						continue;

					if (w[j] == 1) {
						/*
						 * a survivors becomes
						 * pivotal with this pivot
						 */
						BFS_enqueue(w, queue, &surviving, &tail, j);
					} else {
						/* the new pivot has been hit */
						BFS_enqueue_row(w, queue, &surviving, &tail, Ap, Aj, I);
					}
				}
				goto BFS;
			}
			/* reset w back to zero */
	cleanup:
			for (int px = Ap[i]; px < Ap[i + 1]; px++)
				w[Aj[px]] = 0;
			for (int px = 0; px < tail; px++)
				w[queue[px]] = 0;
		}		/* end for */
		free(w);
		free(queue);
	}			/* end of omp parallel */

	fprintf(stderr, "\r[pivots] greedy alternating cycle-free search: %d pivots found [%.1fs]\n", npiv - npiv_start, spasm_wtime() - start);
	return npiv;
}

/*
 * return the number of pivots found. 
 * @param p : row permutations. Pivotal rows are first. 
 * @param qinv : inverse column permutation. q[j] is the row
 * on which the pivot on column j is, or -1 if there is no pivot on column j.
 *
 * both p and qinv must be preallocated
 */
int spasm_find_pivots(spasm * A, int *p, int *qinv)
{
	int n = A->n;
	int m = A->m;

	spasm_vector_set(qinv, 0, m, -1);
	int npiv = spasm_find_FL_pivots(A, p, qinv);
	npiv = spasm_find_FL_column_pivots(A, p, qinv, npiv);
	npiv = spasm_find_cycle_free_pivots(A, p, qinv, npiv);

	/*
	 * build row permutation. Pivotal rows go first in topological order,
	 * then non-pivotal, non-zero rows, then zero rows
	 */
	int *xj = spasm_malloc(m * sizeof(int));
	int *marks = spasm_malloc(m * sizeof(int));
	int *pstack = spasm_malloc(n * sizeof(int));

	/* topological sort */
	spasm_vector_set(marks, 0, m, 0);
	int top = m;
	for (int j = 0; j < m; j++)
		if (qinv[j] != -1 && !marks[j])
			top = spasm_dfs(j, A, top, xj, pstack, marks, qinv);
	int k = 0;
	for (int j = top; j < m; j++) {
		int i = qinv[xj[j]];
		if (i != -1)
			p[k++] = i;
	}

	for (int i = 0; i < n; i++)
		if (spasm_row_weight(A, i) > 0 && !spasm_is_row_pivotal(A, qinv, i))
			p[k++] = i;

	for (int i = 0; i < n; i++)
		if (spasm_row_weight(A, i) == 0)
			p[k++] = i;

	free(xj);
	free(pstack);
	free(marks);
	fprintf(stderr, "\r[pivots] %d pivots found\n", npiv);
	return npiv;
}

/*
 * returns a permuted version of A where pivots are pushed to the top-left
 * and form an upper-triangular principal submatrix. qinv is modified.
 */
spasm *spasm_permute_pivots(const spasm * A, const int *p, int *qinv, int npiv) {
	int k, m, *Ap, *Aj;

	m = A->m;
	Ap = A->p;
	Aj = A->j;

	/* pivotal columns first */
	k = 0;
	for (int i = 0; i < npiv; i++) {
		/* the pivot is the first entry of each row */
		int j = Aj[Ap[p[i]]];
		qinv[j] = k++;
	}

	/* put remaining non-pivotal columns afterwards, in any order */
	for (int j = 0; j < m; j++)
		if (qinv[j] == -1)
			qinv[j] = k++;

	return spasm_permute(A, p, qinv, SPASM_WITH_NUMERICAL_VALUES);
}
