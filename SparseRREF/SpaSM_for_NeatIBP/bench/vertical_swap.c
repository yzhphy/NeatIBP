#include <stdio.h>
#include <assert.h>
#include "spasm.h"

/* permute the columns to flip the matrix */

int main() {
	spasm_triplet *A;
	int m, nz;
	int *Aj;

	A = spasm_load_sms(stdin, 42013);

	m = A->m;
	nz = A->nz;
	Aj = A->j;

	for (int k = 0; k < nz; k++)
		Aj[k] = m - Aj[k] - 1;

	spasm_save_triplet(stdout, A);
	spasm_triplet_free(A);
	return 0;
}
