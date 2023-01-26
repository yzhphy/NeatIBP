#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *A;
  spasm_dense_lu *LU;
  spasm_GFp *x, *Ax;
  int *Ap, *Aj;
  int n, m, test, prime;

  prime = 42013;
  assert(argc > 1);
  test = atoi(argv[1]);

  T = spasm_load_sms(stdin, prime);
  A = spasm_compress(T);
  spasm_triplet_free(T);

  n = A->n;
  m = A->m;
  Ap = A->p;
  Aj = A->j;
  Ax = A->x;

  LU = spasm_dense_LU_alloc(m, prime);
  x = spasm_malloc(m * sizeof(spasm_GFp));

  /* compute a dense "LU" factorisation of the input matrix */
  for(int i = 0; i < n; i++) {
    spasm_vector_zero(x, m);
    for(int px = Ap[i]; px < Ap[i+1]; px++)
      x[Aj[px]] = Ax[px];
    spasm_dense_LU_process(LU, x);
  }

  /* check that all rows of the input matrix belong to the row-space of U */  
  for(int i = 0; i < n; i++) {
    spasm_vector_zero(x, m);
    for(int px = Ap[i]; px < Ap[i+1]; px++)
      x[ Aj[px] ] = Ax[px];
    if (spasm_dense_LU_process(LU, x)) {
      printf("not ok %d - rowspan(U) == rowspan(A) (row %d)\n", test, i);
      exit(0);
    }
  }

  printf("ok %d - rowspan(U) == rowspan(A)\n", test);

  spasm_csr_free(A);
  spasm_dense_LU_free(LU);
  free(x);
  return 0;
}
