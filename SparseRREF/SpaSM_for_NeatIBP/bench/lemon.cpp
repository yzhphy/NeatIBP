#include <assert.h>
#include <stdio.h>

extern "C" {
#include "spasm.h"
}

#include <lemon/smart_graph.h>
#include <lemon/matching.h>

using namespace lemon;
/*
 * Finds pivots without performing arithmetic operations (using the
 * Faugère-Lachartre heuristic and some other ideas) and computes the Schur
 * complement w.r.t. these pivots. The result is sent to the standard output
 */

int main() {
	int prime = 3;

	spasm_triplet *T = spasm_load_sms(stdin, prime);
	int n = T->n;
	int nz = T->nz;
	int *Ai = T->i;
	int *Aj = T->j;

	/* build lemon graph */
	SmartGraph G;
	G.reserveNode(T->n);
	G.reserveEdge(T->nz);

	SmartGraph::Node *V = (SmartGraph::Node *) malloc(T->n * sizeof(SmartGraph::Node));
	SmartGraph::NodeMap<int> index(G);

	for (int i = 0; i < n; i++) {
		V[i] = G.addNode();
		index[V[i]] = i;
	}

	for (int px = 0; px < nz; px++) {
		int i = Ai[px];
		int j = Aj[px];
		G.addEdge(V[i], V[j]);
	}
	 
	MaxMatching<SmartGraph> matcher(G);

	fprintf(stderr, "starting matching\n");
  	
	double start = spasm_wtime();
  	matcher.run();
  	int k = matcher.matchingSize();
  	fprintf(stderr, "Done in %.1fs : size = %d\n", spasm_wtime() - start, k);

	
	/* complete permutation */
	int *q = (int *) malloc(n * sizeof(int));  /* final permutation */
	int *p = (int *) malloc(n * sizeof(int));  /* already included */

	for (int i = 0; i < n; i++) 
		p[i] = 0;

	/* insert matching vertices */
	fprintf(stderr, "insert matching vertices\n");
	int j = 0;
	for (int i = 0; i < n; i++) 
		if (!p[i] && matcher.mate(V[i]) != INVALID) {
			int v = index[matcher.mate(V[i])];
			q[2*j] = i;
			q[2*j + 1] = v;
			p[j] = 1;
			p[v] = 1;
			j++;
		}
	
	/* add the rest */
	fprintf(stderr, "insert the rest\n");
	j = 2*j;
	for (int i = 0; i < n; i++) {
		if (matcher.mate(V[i]) == INVALID)
			q[j++] = i;
	}
	assert(j == n);

	FILE *perm = fopen("permutation.txt", "w");
	for (int i = 0; i < n; i++)
		fprintf(perm, "%d\n", q[i]);
	fclose(perm);

	/* permute */
	fprintf(stderr, "permute\n");
	int *qinv = spasm_pinv(q, n);
	for (int px = 0; px < nz; px++) {
		Ai[px] = qinv[Ai[px]];
		Aj[px] = qinv[Aj[px]];
	}

	fprintf(stderr, "extract principal submatrix\n");
	spasm *A = spasm_compress(T);
	spasm *B = spasm_submatrix(A, 0, 2*k, 0, 2*k, 1);

	spasm_save_triplet(stdout, T);
	FILE *principal = fopen("principal.sms", "w");
	spasm_save_csr(principal, B);
	fclose(principal);
	
	spasm_triplet_free(T);
	return 0;
}
