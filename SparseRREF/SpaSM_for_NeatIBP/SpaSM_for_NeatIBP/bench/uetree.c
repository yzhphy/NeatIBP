#include <assert.h>
#include <stdio.h>
#include <getopt.h>
#include "spasm.h"

/* computes an unsymmetric elimination tree and permute the rows and columns of a (square) matrix according to it */

int main(int argc, char **argv) {
	int prime = 42013;
	spasm_triplet *T;
	spasm *A;

	/* options descriptor */
	struct option longopts[5] = {
		{"tree", no_argument, NULL, 't'},
		{"matrix", no_argument, NULL, 'm'},
		{"non-topological", no_argument, NULL, 'n'},
		{"stats", no_argument, NULL, 's'},
		{NULL, 0, NULL, 0}
	};
	
	int mode = 'm';
	int topo = 1;
	int ch;
	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
		case 'm':
		case 't':
		case 's':
			mode = ch;
			break;
		case 'n':
			topo = 0;
			break;
		default:
			printf("Unknown option\n");
			exit(1);
		}
	}
	
	T = spasm_load_sms(stdin, prime);
	assert (T->n == T->m);

	A = spasm_compress(T);
	spasm_triplet_free(T);
	int n = A->n;

	int *uetree = spasm_uetree(A);
	int *postorder = topo ? spasm_tree_topological_postorder(A, uetree) : spasm_tree_postorder(A, uetree);

	if (mode == 't') {
		fprintf(stderr, "rendering the tree...\n");
		printf("digraph {\n");
		printf("rank = BT;\n");
		for(int i = 0; i < n; i++) {
			int j = postorder[i];
			if (uetree[j] >= 0)
				printf("  %d -> %d;\n", j + 1, uetree[j] + 1);
			else
				printf("  %d;\n", j + 1);
		}
		printf("}\n");
	}

	if (mode == 'm') {
		fprintf(stderr, "permuting the matrix...\n");
		int *pinv = spasm_pinv(postorder, n);
		spasm *B = spasm_permute(A, postorder, pinv, SPASM_WITH_NUMERICAL_VALUES);
		free(pinv);
		spasm_save_csr(stdout, B);
		spasm_csr_free(B);
	}
	
	if (mode == 's') {
		fprintf(stderr, "producing stats...\n");
		/* no */
	}

	free(uetree);
	free(postorder);
	spasm_csr_free(A);
	return 0;
}
