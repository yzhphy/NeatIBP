#include <assert.h>
#include <stdio.h>
#include <getopt.h>
#include "spasm.h"


int main(int argc, char **argv) {
	int prime = 42013;
	spasm_triplet *T;
	spasm *A;

	T = spasm_load_sms(stdin, prime);
	A = spasm_compress(T);
	spasm_triplet_transpose(T);
	At = spasm_compress(T);
	spasm_triplet_free(T);
	
	int n = A->n;
	int m = A->m;

	int px, p2, inew, head, *Ap, *Aj;

	int *Ap = A->p;
	int *Aj = A->j;

	/* dans stack: i >= 0 --> i est un numéro de ligne // j < 0 --> -i-1 est un numéro de colonne */
	head = 0;
	stack[head] = 0;   /* initialize row stack with row 0 */

	while (head >= 0) { 	/* row stack empty ? */
		int i = stack[head];
		if (i >= 0) {
		  
		  if (!rmarks[i]) {
		    rmarks[i] = 1;
		    pstack[head] = Ap[i];
		  }
		  for (int px = pstack[head]; px <  Ap[i + 1]; px++) {
			int j = Aj[px];
			if (cmarks[j])
				continue;
			pstack[head] = px + 1;
			stack[++head] = -j-1;
			break;
		  }
		  if (px == p2)
		    head--;
		} else { /* column */
		  int j = -i-1;
		  
		  if (!cmarks[j]) {
		    cmarks[j] = 1;
		    pstack[head] = tAp[j];
		  }
		  for (int px = pstack[head]; px < tAp[j + 1];; px++) {
			int i = tAi[px];
			if (rmarks[i])
				continue;
			pstack[head] = px + 1;
			stack[++head] = -j-1;
			break;
		  }
		  if (px == p2)
		    head--;
		} else { /* column */
		}
		
	}
	return;
}

	

	spasm_csr_free(A);
	return 0;
}
