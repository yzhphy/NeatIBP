#include <assert.h>
#include "spasm.h"

/**
 * depth-first-search of the graph of a matrix, starting at column j.
 *
 * The traversal works as follow : starting from a column j, find the
 * row containing the pivot on this column (row "inew"). If there is not pivot on column j, stop.
 *
 * When a pivot row has been found, append its next unseen entry to the stack, and restart.
 *
 * @param qinv
 *           qinv[j] indicates the row on which the j-th column pivot can be found.
 *           qinv[j] == -1 means that there is no pivot on column j.
 *
 * The matrix need not be square. If n < m, then qinv must be non-NULL, and
 * qinv[j] = -1 that there is no pivot on column j
 *
 * @param j column at which the traversal starts.
 *
 * @param G the graph to search
 *
 * @param xj : size m. Used both as workspace and to return the result. At the end, the
 * list of traversed nodes is in xj[top:m]
 *
 * @param pstack size-n workspace. Used to count the neighbors already traversed.
 *
 * @marks : size-m workspace. Indicates which column nodes have been dealt with.
 *
 * @return top
 */
int spasm_dfs(int j, const spasm * A, int top, int *xj, int *pstack, int *marks, const int *qinv) {
	int px, p2, inew, head, *Ap, *Aj;

	/* check inputs */
	assert(A != NULL);
	assert(xj != NULL);
	assert(pstack != NULL);
	assert(marks != NULL);

	Ap = A->p;
	Aj = A->j;
	/*
	 * initialize the recursion stack (rows waiting to be traversed). The
	 * stack is held at the begining of xj, and has head elements.
	 */
	head = 0;
	xj[head] = j;

	/* stack empty ? */
	while (head >= 0) {
		/* get j from the top of the recursion stack */
		j = xj[head];
		inew = (qinv != NULL) ? qinv[j] : j;

		/*
		 * has row i been seen before ? adjacent columns are Gj[
		 * Gp[jnew]     : Gp[jnew + 1] ] UNSEEN columns are   Gj[
		 * pstack[head] : Gp[jnew + 1] ]
		 */

		if (!marks[j]) {
			/* mark node i as seen and initialize pstack. This is done only once. */
			marks[j] = 1;
			pstack[head] = (inew < 0) ? 0 : Ap[inew];
		}
		/* index of last entry of row inew */
		p2 = (inew < 0) ? 0 : Ap[inew + 1];

		/* examine all yet-unseen entries of row i */
		for (px = pstack[head]; px < p2; px++) {
			j = Aj[px];
			if (marks[j])
				continue;
			/*
			 * interrupt the enumeration of entries of row inew,
			 * and deal with column j instead. Save status of row
			 * inew in the stack.
			 */
			pstack[head] = px + 1;

			/*
			 * push column j onto the recursion stack. This will
			 * start a DFS from j
			 */
			xj[++head] = j;

			/* node i is not done, exit the loop */
			break;
		}

		/* depth-first search at node i done ? */
		if (px == p2)
			/* push initial column in the output stack and pop it from the recursion stack*/
			xj[--top] = xj[head--];
	}
	return top;
}


/*
 * Compute the set of nodes from G reachable from any node in B[k] (used to
 * determine the pattern of a sparse triangular solve)
 * 
 * G : graph to search
 * 
 * B : RHS (starting point of the search)
 * 
 * k : k-th row of B is used.
 * 
 * l : upper-bound on both dimensions of the matrix
 * 
 * xj: size 3l. Used as workspace. Output in xj[top:l]
 * 
 * pinv: mapping of rows to columns of G.
 * 
 * return value : top
 * 
 * xj [top...l-1] = nodes reachable from graph of G*P' via nodes in B(:,k).
 * 
 * xj [l...3l-1] used as workspace
 */
int spasm_reach(const spasm * A, const spasm * B, int k, int l, int *xj, const int *qinv) {
	int  top, *Bp, *Bj, *pstack, *marks;

	/* check inputs */
	assert(A != NULL);
	assert(B != NULL);
	assert(xj != NULL);

	Bp = B->p;
	Bj = B->j;
	top = l;
	pstack = xj + l;
	marks = pstack + l;

	/*
	 * iterates over the k-th row of B.  For each column index j present
	 * in B[k], check if j is in the pattern (i.e. if it is marked). If
	 * not, start a DFS from j and add to the pattern all columns
	 * reachable from j.
	 */
	for (int px = Bp[k]; px < Bp[k + 1]; px++)
		if (!marks[Bj[px]])
			top = spasm_dfs(Bj[px], A, top, xj, pstack, marks, qinv);

	/* unmark all marked nodes. */
	/*
	 * TODO : possible optimization : if stuff is marked "k", and
	 * initialized with -1, then this is not necessary
	 */
	for (int px = top; px < l; px++)
		marks[xj[px]] = 0;

	return top;
}
