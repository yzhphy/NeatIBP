#include <assert.h>
#include <stdio.h>
#include "spasm.h"

/* Reads a matrix in MartixMarket format, prints it in SMS format */

int main() {
	int prime = 42013;
	spasm_triplet *T;
	
	T = spasm_load_mm(stdin, prime);
	spasm_save_triplet(stdout, T);
	spasm_triplet_free(T);
	return 0;
}
