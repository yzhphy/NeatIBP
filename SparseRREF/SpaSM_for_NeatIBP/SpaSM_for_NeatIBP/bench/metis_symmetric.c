#include <stdio.h>
#include <assert.h>
#include "spasm.h"
#include <metis.h>

/** Uses METIS to find a fill-in reducing ordering */

int main() {
	spasm_triplet *T = spasm_load_sms(stdin, 42013);
	spasm *A = spasm_compress(T);

	int n = A->n;
	int nz = spasm_nnz(A);
	int *Ap = A->p;
	int *Aj = A->j;
	int *p = spasm_malloc(n * sizeof(int));
	int *pinv = spasm_malloc(n * sizeof(int));

	idx_t options[METIS_NOPTIONS];
	METIS_SetDefaultOptions(options);
	// options[METIS_OPTION_DBGLVL] = METIS_DBG_INFO;
	options[METIS_OPTION_NUMBERING] = 0;
	
	fprintf(stderr, "Launching METIS fill-in reducing ordering.");
	double start = spasm_wtime();
	int result = METIS_NodeND(&n, Ap, Aj, NULL, options, p, pinv);
	fprintf(stderr, "done: %.1fs\n", spasm_wtime() - start);

	if (result != METIS_OK) {
		fprintf(stderr, "something went wrong\n");
		exit(1);
	}
	spasm_csr_free(A);
	
	int *Ti = T->i;
	int *Tj = T->j;
	for (int px = 0; px < nz; px++) {
		Ti[px] = pinv[Ti[px]];
		Tj[px] = pinv[Tj[px]];
	}

	spasm_save_triplet(stdout, T);
	return 0;
}
