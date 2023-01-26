#include <stdio.h>
#include <assert.h>
#include "spasm.h"


/* workspace must have size 5n (ints). Returns the number of scc. 
   Not dry, because this is a clone of the normal scc function with a different interface. */
int spasm_scc_for_uetree(const spasm * A, int maxn, int *p, int *rr, int * workspace) {
	int n = A->n;
	int *Ap = A->p;
	int *Aj = A->j;
	assert (n == A->m); /* square matrices */

	int *pstack = workspace; 
	int *marks = workspace + n;
	int *prev = workspace + 2*n;
	int *stack = workspace + 3*n;
	int *lowlink = workspace + 4*n;
	
	for (int i = 0; i < maxn; i++) {
		marks[i] = -1;
		prev[i] = -1;
		pstack[i] = Ap[i];
	}

	int p_top = 0;
	int n_scc = 0;
	int index = 0;
	rr[0] = 0;
	for (int i = 0; i < maxn; i++) {
		int head, top;
		if (marks[i] >= 0)
			continue;
		
		/* DFS */
		head = 0;
		top = 0;
		stack[top] = i;
		int j = i;
		while (j >= 0) {
			/* get j from the top of the recursion stack */
			assert (j < maxn);
			int px, px2;
			if (marks[j] < 0) {
				/* init */
				lowlink[j] = index;
				marks[j] = index++;
			}
			px2 = Ap[j + 1];
			for (px = pstack[j]; px < px2; px++) {
				int k = Aj[px];
				if (k >= maxn)       /* truncate graph */
					continue;
				if (marks[k] >= 0) { /* update */
					lowlink[j] = spasm_min(lowlink[j], lowlink[k]);
					continue;
				}
				/* push */
				pstack[j] = px + 1;
				stack[++top] = k;
				prev[k] = j;
				j = k;
				break;
			}
			if (px == px2) {
				/* check if we have the root of a SCC */
				if (lowlink[j] == marks[j]) {
					while (stack[top] != j) {
						int k = stack[top--];
						p[p_top++] = k;
						lowlink[k] = n;
					}
					p[p_top++] = j;
					lowlink[j] = n;
					top--;

					rr[++n_scc] = p_top;
				}

				/* pop */
				int k = j;
				j = prev[j];
				if (j >= 0)
					lowlink[j] = spasm_min(lowlink[j], lowlink[k]);
			}
		}
	}
	assert (p_top == maxn);
	return n_scc;
}

/* computes the unsymmetric elimination tree, using a naive algorithm */
int * spasm_uetree(const spasm * A) {
	int n = A->n;
	int *workspace = spasm_malloc(5*n * sizeof(int));
	int *rr = spasm_malloc(n * sizeof(int));
	int *p = spasm_malloc(n * sizeof(int));
	int *T = spasm_malloc(n * sizeof(int));
	for(int i = 0; i < n; i++)
		T[i] = -1;

	for(int i = 0; i < n; i++) {
		int n_scc = spasm_scc_for_uetree(A, i + 1, p, rr, workspace);

		/* locate SCC containing vertex i */
		int scc_idx = -1;
		for(int k = 0; (k < n_scc) && (scc_idx < 0); k++)
			for(int j = rr[k]; j < rr[k+1]; j++)
				if (p[j] == i) {
					scc_idx = k;
					break;
				}
		assert (scc_idx >= 0);

		/* update parent pointers in the SCC */
		for(int px = rr[scc_idx]; px < rr[scc_idx+1]; px++) {
			int j = p[px];
			if ((j != i) && (T[j] == -1))
				T[j] = i;
		}
		T[i] = -1;
	}

	free(rr);
	free(p);
	free(workspace);
	return T;
}

/* computes the height of each node in the tree. This destructs head */
void spasm_depth_dfs(int j, int *head, const int *next, int *depth, int *stack) {
	int top = 0;
	stack[0] = j;
	depth[j] = 0;                       /* j is a root */
	while (top >= 0) {
		int p = stack[top];         /* p = top of stack */
		int i = head[p];            /* i = youngest child of p */
		if (i == -1) {
			top--;              /* p has no unordered children left */
		} else {
			head[p] = next[i];  /* remove i from children of p */
			stack[++top] = i;   /* start dfs on child node i */
			depth[i] = top;
		}
	}
}



/* depth-first search and postorder of a tree rooted at node j. */
int spasm_tree_dfs(int j, int k, int *head, const int *next, int *post, int *stack) {
	int top = 0;
	stack[0] = j;
	while (top >= 0) {
		int p = stack[top];         /* p = top of stack */
		int i = head[p];            /* i = youngest child of p */
		if (i == -1) {
			top--;              /* p has no unordered children left */
			post[k++] = p;      /* node p is the kth postordered node */
		} else {
			head[p] = next[i];  /* remove i from children of p */
			stack[++top] = i;   /* start dfs on child node i */
		}
	}
	return k;
}


/* given the parent pointers, build linked list describing the children */
void spasm_reverse_tree(int n, const int *parent, int *head, int *next, int *order) {
	/* empty linked lists */
	for(int j = 0; j < n; j++)
		head [j] = -1;

	/* traverse nodes in reverse order*/
	for (int px = n-1; px >= 0; px--) {
		int j = (order != NULL) ? order[px] : px;
		int p = parent[j];
		if (p == -1)
			continue;
		next[j] = head[p];
		head[p] = j;
	}
}


int *spasm_tree_postorder(const spasm *A, const int *parent) {
	int n = A->n;

	int *head = spasm_malloc(n * sizeof(int));
	int *next = spasm_malloc(n * sizeof(int));
	int *stack = spasm_malloc(n * sizeof(int));

	spasm_reverse_tree(n, parent, head, next, SPASM_IDENTITY_PERMUTATION);
	
	/* build a real, topologically-ordered, postorder tree traversal */
	int k = 0;
	int *post = spasm_malloc(n * sizeof(int));
	for (int i = 0; i < n; i++) {
		if (parent[i] != -1) /* skip j if it is not a root */
			continue;
		k = spasm_tree_dfs(i, k, head, next, post, stack);
	}
	assert (k == n);

	free(stack);
	free(head);
	free(next);
	return post;
}

int *spasm_tree_topological_postorder(const spasm *A, const int *parent) {
	int n = A->n;
	int *Ap = A->p;
	int *Aj = A->j;

	int *depth = spasm_malloc(n * sizeof(int));
	int *head = spasm_malloc(n * sizeof(int));
	int *next = spasm_malloc(n * sizeof(int));
	int *stack = spasm_malloc(n * sizeof(int));

	/* compute node depth */
	spasm_reverse_tree(n, parent, head, next, SPASM_IDENTITY_PERMUTATION);
	for (int j = 0; j < n; j++) {
		if (parent [j] != -1) /* skip j if it is not a root */
			continue;
		spasm_depth_dfs(j, head, next, depth, stack);
	}

	/* build the graph to sort topologically */
	spasm_triplet *T = spasm_triplet_alloc(n, n, spasm_nnz(A), -1, SPASM_IGNORE_VALUES);
	for(int i = 0; i < n; i++)
		for(int px = Ap[i]; px < Ap[i + 1]; px++) {
			int u = i;
			int v = Aj[px];
			/* edge u --> v */
			while (depth[u] > depth[v])
				u = parent[u];
			while (depth[v] > depth[u])
				v = parent[v];
			if (u == v)
				continue;  /* edge is inside a SCC */
			while (parent[u] != parent[v]) {
				u = parent[u];
				v = parent[v];
			}
			spasm_add_entry(T, u, v, 1);
		}
	free(depth);
	spasm *G = spasm_compress(T);
	spasm_triplet_free(T);

	/* sort G in toplogical order */
	int top = n;
	int *marks = spasm_malloc(n * sizeof(int));
	int *topo = spasm_malloc(n * sizeof(int));
	for (int i = 0; i < n; i++)
		marks[i] = 0;
	for (int i = 0; i < n; i++)
		if (!marks[i])
			top = spasm_dfs(i, G, top, topo, stack, marks, SPASM_IDENTITY_PERMUTATION);
	
	assert(top == 0);
	spasm_csr_free(G);
	free(marks);

	spasm_reverse_tree(n, parent, head, next, topo);
	
	/* build a real, topologically-ordered, postorder tree traversal */
	int k = 0;
	int *post = spasm_malloc(n * sizeof(int));
	for (int px = 0; px < n; px++) {
		int j = topo[px];
		if (parent[j] != -1) /* skip j if it is not a root */
			continue;
		k = spasm_tree_dfs(j, k, head, next, post, stack);
	}
	assert (k == n);

	free(stack);
	free(head);
	free(next);
	free(topo);
	return post;
}