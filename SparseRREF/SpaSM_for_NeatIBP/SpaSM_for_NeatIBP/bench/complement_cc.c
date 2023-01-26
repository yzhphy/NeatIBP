#include <assert.h>
#include <stdio.h>
#include <getopt.h>
#include "spasm.h"

/* compute the connected components of the complement graph. */


int main() 
{
	spasm_triplet * T = spasm_load_mm(stdin, -1);
	assert (T->n == T->m);

	spasm * A = spasm_compress(T);
	spasm_triplet_free(T);
	int n = A->n;
	int *Ap = A->p;
	int *Aj = A->j;

	/**************** BFS ****************/
	int *queue = spasm_malloc(sizeof(int) * n);
	int *mark = spasm_malloc(sizeof(int) * n);

	for (int i = 0; i < n; i++)
		queue[i] = i;
	for (int i = 0; i < n; i++)
		mark[i] = 0;
	
	int k = 0;
	int lo = 0;
	int mid = 0;
	for (int i = 0; i < n; i++) {
		if (mark[i])
			continue;
		if (lo == n)
			break;
		mark[queue[lo]] = 1;
		mid++;
		while (lo < mid) {
			int u = queue[lo++];
			for (int it = Ap[u]; it < Ap[u + 1]; it++)
				mark[Aj[it]] = 1;
			int hi = n;
			for (int it = mid; it < hi; )
				if (mark[queue[it]])
					spasm_swap(queue, it, --hi);
				else
					it++;
			mid = hi;
			for (int it = hi; it < n; it++)
				mark[queue[it]] = 0;
		}
		k++;
	}
	printf("%d\n", k);
	free(queue);
	free(mark);
	spasm_csr_free(A);
	return 0;
}