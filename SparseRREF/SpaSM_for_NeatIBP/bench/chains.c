#include <assert.h>
#include <stdio.h>
#include <getopt.h>
#include "spasm.h"

/* computes a chain decomposition of the graph described by a symmetric matrix. */


/* Computes a DFS starting from vertex `start`.
   The vertex stack is... stack[0:sp].
   The resulting permutation is in stack[top:n].

   vmark holds vertex marks, while pstack is a secondary stack
   for neighbor counts.

   The parent array reveals the DFS tree.

   The return value is top.
*/

int dfs(spasm *A, int start, int top, int *stack, int *vmark, int *itstack, int *parent)
{
	assert(vmark[start] == 0);
	
	int *Ap = A->p;
	int *Aj = A->j;
	int sp = 0;
	
	stack[sp] = start;
	itstack[sp] = Ap[start];
	vmark[start] = 1;
	while (sp >= 0) {
		int u;
	start:
		u = stack[sp];
		for (int it = itstack[sp]; it < Ap[u + 1]; it++) {
			int v = Aj[it];
			if (vmark[v])
				continue;
			itstack[sp] = it + 1;
			if (parent)
				parent[v] = u;
			vmark[v] = 1;
			sp++;
			stack[sp] = v;
			itstack[sp] = Ap[v];
			goto start;
		}
		stack[--top] = u;
		sp--;
	}
	return top;
}


/*******************************************************************
 ** Given a graph G and a DFS forest (parent), produces an oriented
 ** graph where the tree edges are directed towards the root, while
 ** the back-edges are directed away from it.
 ** 
 ** parent[v] == u    if    u is the father of v in the DFS tree.
 ** p is a permutation of vertices describing the DFS order. If the graph
 ** is connected, then p[0] is the root of the tree.
 **
 ** Both u --> v and v --> u are present in the input graph, and only
 ** one must remain in the output.
 **
 ** We must keep:
 **      tree edges:      u --> v    where parent[u] == v
 ** 				      (which implies pinv[u] > pinv[v])
 **
 **      back-edges:      u --> v    with pinv[u] < pinv[v]
 **                                   (but only when there is no 
 **                                    corresponding tree edge v --> u)
 ** The tree edges are distinguinshed by being the first of each row.
 *******************************************************************/
spasm * orient_graph(spasm *A, int *p, int *parent)
{
	int *Ap = A->p;
	int *Aj = A->j;
	int n = A->n;

	int *pinv = spasm_pinv(p, n);

	spasm_triplet *T = spasm_triplet_alloc(n, n, A->nzmax, A->prime, 0);
	for (int u = 0; u < n; u++)
		for (int px = Ap[u]; px < Ap[u + 1]; px++) {
			int v = Aj[px];
			if (parent[u] == v) {
				assert(pinv[u] > pinv[v]);
				spasm_add_entry(T, u, v, 0);
			}
			if (pinv[u] < pinv[v] && parent[v] != u)
				spasm_add_entry(T, u, v, 0);
		}
	
	spasm *G = spasm_compress(T);
	spasm_triplet_free(T);
	int *Gp = G->p;
	int *Gj = G->j;
	for (int i = 0; i < n; i++)
		for (int px = Gp[i]; px < Gp[i + 1]; px++)
			if (parent[i] == Gj[px]) {
				spasm_swap(Gj, Gp[i], px);
				break;
			}

	free(pinv);
	return G;
}


int main() 
{
	int prime = -1;	
	spasm_triplet * T = spasm_load_mm(stdin, prime);
	assert (T->n == T->m);

	spasm * A = spasm_compress(T);
	spasm_triplet_free(T);
	int n = A->n;

	/**************** DFS ****************/
	int *stack = spasm_malloc(sizeof(int) * n);
	int *vmark = spasm_malloc(sizeof(int) * n);
	int *itstack = spasm_malloc(sizeof(int) * n);
	int *parent = spasm_malloc(sizeof(int) * n);
	for (int i = 0; i < n; i++)
		parent[i] = -1;
	for (int i = 0; i < n; i++)
		vmark[i] = 0;
	int top = n;
	int CC = 0;

	for (int i = 0; i < n; i++) {
		if (vmark[i])
			continue;
		int prev = top;
		top = dfs(A, i, top, stack, vmark, itstack, parent);
		//fprintf(stderr, "|CC| = %d\n", prev - top);
		CC++;
	}
	/**************** Permute & Isolate CC ****************/
	
	// int * pinv = spasm_pinv(stack, n);
	// spasm *B = spasm_permute(A, stack, pinv, 0);
	// free(pinv);
	// spasm_save_csr(stdout, B);

	/**************** Form Oriented Graph ****************/
	
	spasm *G = orient_graph(A, stack, parent);
	spasm_csr_free(A);
	//spasm_save_csr(stdout, G);
	//exit(1);

	/**************** Chain Decomposition ****************/
	/** edges and vertices are marked with a chain number (initially -1). **/
	int *Gp = G->p;
	int *Gj = G->j;
	int n_cut_vertex = 0;
	int n_bridge = 0;

	int *emark = malloc(sizeof(int) * (Gp[n] + 1));
	for (int i = 0; i < n; i++)
		vmark[i] = -1;
	for (int i = 0; i < Gp[n] + 1; i++)
		emark[i] = -1;

	int chain = 0;
	for (int i = 0; i < n; i++) {
		int u = stack[i];
		/* take every back-edge u--> v */
		//for (int px = (parent[u] >= 0) ? Gp[u] + 1 : Gp[u]; px < Gp[u + 1]; px++) {
		for (int px = Gp[u]; px < Gp[u + 1]; px++) {
			int v = Gj[px];
			if (parent[u] == v) {
				assert(px == Gp[u]);
				continue; // skip tree edge
			}
			if (px == Gp[u]) {
			 	assert(parent[u] == -1);
			}
			//printf("Taking back-edge %d --> %d [current chain = %d]\n", u, v, chain);
			if (vmark[u] < 0) {
				//printf("Vertex %d now belongs to chain %d\n", u, chain);
				vmark[u] = chain;
				if (parent[u] != -1) {
					//fprintf(stderr, "%d is a cut-vertex (start of a cycle)\n", u);
					n_cut_vertex++;
				}
			}
			//printf("back-edge %d --> %d now belongs to chain %d\n", u, v, chain);
			emark[px] = chain;

			while (vmark[v] < 0) {
				//printf("Walking up the tree. Reached vertex %d, that now belongs to chain %d\n", v, chain);
				assert(emark[Gp[v]] < 0); /* the tree edge going out of v should be unmarked */
				assert(parent[v] >= 0);
				assert(parent[v] == Gj[Gp[v]]);
				vmark[v] = chain;
				emark[Gp[v]] = chain;
				v = parent[v]; // v = Gj[Gp[v]]; 
			}
			chain += 1;
		}
	}
	// fprintf(stderr, "chain = %d, E=%d, n=%d, CC=%d\n", chain, spasm_nnz(G), n, CC);
	assert(chain == spasm_nnz(G) - n + CC);

	/*************** reap of the rewards *********************/
	for (int i = 0; i < n; i++)
		for (int px = Gp[i]; px < Gp[i + 1]; px++)
			if (emark[px] < 0) {
				//fprintf(stderr, "edge %d -- %d is a bridge (and both vertices are cut vertices)\n", i + 1, Gj[px] + 1);
				n_bridge++;
			}
	printf("%d; %d ; %d\n", CC, n_cut_vertex, n_bridge);
	exit(0);

	/****************** build the graph with bridges removed *************/
	spasm_triplet *TT = spasm_triplet_alloc(n, n, G->nzmax, G->prime, 0);
	for (int u = 0; u < n; u++)
		for (int px = Gp[u]; px < Gp[u + 1]; px++)
			if (emark[px] >= 0) {
				int v = Gj[px];
				spasm_add_entry(TT, u, v, 0);
				spasm_add_entry(TT, v, u, 0);
			}
	spasm_csr_free(G);
	spasm *B = spasm_compress(TT);
	spasm_triplet_free(TT);

	/****************** permutation that isolates biconnected components **************/

	top = n;
	for (int i = 0; i < n; i++)
		vmark[i] = 0;
	for (int i = 0; i < n; i++) {
		if (vmark[i])
			continue;
		int prev = top;
		top = dfs(B, i, top, stack, vmark, itstack, NULL);
		//fprintf(stderr, "|biCC| = %d\n", prev - top);
	}

	spasm_csr_free(B);
	return 0;
}
