#include <assert.h>
#include <stdio.h>
#include "spasm.h"


/* for test purposes. Mark the subtree rooted in root with the given color */
void spasm_mark_tree(int root, int color, const int *head, const int *next, int *mark, int *queue) {
	int q_head = 0;
	int q_tail = 1;
	queue[0] = root;
	while (q_head < q_tail) {
		int parent = queue[q_head++];
		mark[parent] = color;
		int i = head[parent];
		while (i != -1) {
			queue[q_tail++] = i;
			i = next[i];
		}
	}
}

int main(int argc, char **argv) {
	int prime = 42013;
	spasm_triplet *T;
	spasm *A;
	
	assert(argc > 1);
  	int test = atoi(argv[1]);

	T = spasm_load_sms(stdin, prime);
	assert (T->n == T->m);

	/* add an extra row-column to make the graph stronly connected */
	int n = T->n;
	for(int i = 0; i < n; i++) {
		spasm_add_entry(T, i, n, 1);
		spasm_add_entry(T, n, i, 1);
	}
	spasm_add_entry(T, n, n, 1);

	A = spasm_compress(T);
	spasm_triplet_free(T);
	n = A->n;

	/* get the tree */
	int *uetree = spasm_uetree(A);
	int *postorder = spasm_tree_topological_postorder(A, uetree);

	/* identify the SCC */
	int * marks = spasm_malloc(n * sizeof(int));
	for(int i = 0; i < n; i++)
		marks[i] = 0;
	int k = 0;
	for (int px = n-1; px >= 0; px--) {
		int i = postorder[px];
		if (uetree[i] == -1)
			k++;
		marks[i] = k;
	}

	if (k != 1) {
		printf("not ok %d - the tree is a forest", test);
		exit(0);
	}

	/* check that everyone is present */
	for(int i = 0; i < n; i++)
		if (marks[i] != 1) {
			printf("not ok %d - node %d is not in the tree", test, i);
			exit(0);
	}


	/* check that SCC are topologically ordered */	
	int *Ap = A->p;
	int *Aj = A->j;
	int *head = spasm_malloc(n * sizeof(int));
	int *next = spasm_malloc(n * sizeof(int));
	int *stack = spasm_malloc(n * sizeof(int));

	for(int j = 0; j < n; j++)
		head[j] = -1;
	for (int px = n-1; px >= 0; px--) {
		int j = postorder[px];
		int p = uetree[j];
		if (p == -1)
			continue;  
		next[j] = head[p];
		head[p] = j;
	}

	/* print the tree */
	for(int i = 0; i < n; i++) {
		printf("# T[%d]: ", i+1);
		int j = head[i];
		while (j != -1) {
			printf("T[%d] ", j+1);
			j = next[j];
		}
		printf("\n");
	}

	
	for(int root = 0; root < n; root++) {
		/* check T[root] */

		for(int i = 0; i < n; i++)
			marks[i] = -1;

		int i = head[root];
		k = 0;
		while (i != -1) {
			spasm_mark_tree(i, k, head, next, marks, stack);
			i = next[i];
			k++;
		}

		for (int i = 0; i < n; i++)
			if (marks[i] != -1)
				for (int px = Ap[i]; px < Ap[i + 1]; px++) {
					int j = Aj[px];
					if ((marks[j] != -1) && (marks[i] > marks[j])) {
						printf("not ok %d - BTF violated by edge %d [%d] -> %d [%d]\n", test, i+1, marks[i], j+1, marks[j]);
						exit(0);
					}
				}
	}
	free(marks);
	free(head);
	free(next);
	free(stack);

	printf("ok %d - unsymmetric elimination tree\n", test);

	free(uetree);
	free(postorder);
	spasm_csr_free(A);
	return 0;
}
