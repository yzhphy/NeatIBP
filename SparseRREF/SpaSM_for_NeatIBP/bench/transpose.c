#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main() {
	spasm_triplet *T;
	spasm *A, *A_t;

	T = spasm_load_sms(stdin, 42013);

	/* TODO : this is really ugly */
	A = spasm_compress(T);
	spasm_triplet_free(T);
	A_t = spasm_transpose(A, SPASM_WITH_NUMERICAL_VALUES);
	spasm_csr_free(A);

	spasm_save_csr(stdout, A_t);

	spasm_csr_free(A_t);
	return 0;
}
