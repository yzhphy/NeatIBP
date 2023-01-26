#include <stdio.h>
#include <assert.h>

#include "spasm.h"

/* 
liste globale de pivots qinv, qui contient tous ceux qui sont connus.

INVARIANT: 
i)  les lignes avec des pivots sont en haut.
ii) Ces pivots sont éliminés dans toutes les lignes du dessous. 
    Les lignes du dessous contiennent les coefficients d'élimination sur 
    les colonnes avec pivot (si on en a envie).

Jusque-là, N pivots ont été identifiés et l'invariant est respecté.
on identifie k nouveaux pivots structurels.

Pour les incorporer:

On les pousse vers le haut, puis sur les lignes qui restent on effectue uniquement l'élimination des nouveaux pivots trouvés.
*/


int main(int argc, char **argv) 
{
	assert(argc > 1);
	int test = atoi(argv[1]);
	int prime = 42013;

	spasm_triplet *T = spasm_load_sms(stdin, prime);
	spasm *A = spasm_compress(T);
	spasm_triplet_free(T);

	int n = A->n;
	int m = A->m;
  
	int *p = spasm_malloc(n * sizeof(int));
	int *qinv = spasm_malloc(m * sizeof(int));
	int npiv = spasm_find_pivots(A, p, qinv);
	spasm_make_pivots_unitary(A, p, npiv);

	spasm *S = spasm_schur(A, p, npiv, -1, 0, NULL);
	int Sn = S->n;
	int *Sp = S->p;
	int *Sj = S->j;

	/* checking that nothing remains under the pivots when we don't want it */
	int abort = 0;
	for (int i = 0; i < Sn; i++) {
		for (int px = Sp[i]; px < Sp[i + 1]; px++)
			if (qinv[Sj[px]] >= 0) {
				abort = 1;
				printf("not ok %d - coeff (%d, %d) is below a pivot\n", test, i, Sj[px]);
				break;
			} 
		if (abort)
			break;
	}
	if (!abort)
		printf("ok %d - elimination coeffs are really absent\n", test);
	spasm_csr_free(S);

	test++;

	/* phase II: check the elimination coeffs */
	/*
	int *p_out = spasm_malloc(Sn * sizeof(int));
	S = spasm_schur(A, p, npiv, -1, 1, p_out);
	Sp = S->p;
	Sj = S->j;
	spasm_GFp *Sx = S->x;
	spasm_GFp *x = spasm_malloc(n * sizeof(*x));
	spasm_GFp *y = spasm_malloc(m * sizeof(*y));

	abort = 0;
	for (int k = 0; k < Sn; k++) {
		for (int i = 0; i < n; i++)
			x[i] = 0;
		for (int j = 0; j < m; j++)
			y[j] = 0;
		for (int px = Sp[k]; px < Sp[k + 1]; px++) {
			int j = Sj[px];
			if (qinv[j] >= 0)
				x[qinv[j]] = Sx[px];
			else
				y[j] = Sx[px];
		}

		assert(x[p_out[k]] == 0);		
		x[p_out[k]] = prime - 1;
		
		spasm_gaxpy(A, x, y);

		
		for (int j = 0; j < m; j++)
			abort |= (y[j] != 0);
		if (abort) {
			printf("not ok %d - could not reconstruct row %d of S (=row %d of A)\n", test, k, p_out[k]);
			break;
		}
	}
	spasm_csr_free(S);
	if (!abort)
		printf("ok %d - reconstruction successfull\n", test);
	*/
	printf("not ok %d # TODO not implemented\n", test);

	spasm_csr_free(A);
}
