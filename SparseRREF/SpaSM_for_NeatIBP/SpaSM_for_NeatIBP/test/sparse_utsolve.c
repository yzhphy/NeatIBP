#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *U, *L, *B;
  int i, j, n, m, test, top, prime, *xi;
  spasm_GFp *x, *y;

  assert(argc == 2);
  test = atoi(argv[1]);

  // load matrix
  T = spasm_load_sms(stdin, 32003);
  U = spasm_compress(T);
  spasm_triplet_free(T);

  printf("# testing L-solver on U.T\n");
  L = spasm_transpose(U, SPASM_WITH_NUMERICAL_VALUES);
  spasm_csr_free(U);

  n = L->n;
  m = L->m;
  prime = L->prime;

  assert( n >= m); // lower-trapezoidal

  // load RHS
  T = spasm_triplet_alloc(1, n, 10, 32003, true);

  spasm_add_entry(T, 0, 0, 1);
  spasm_add_entry(T, 0, m / 2, 2);
  spasm_add_entry(T, 0, m - 1, 3);
  if (n > m) {
    spasm_add_entry(T, 0, n - 1, 4);
  }
  B = spasm_compress(T);
  spasm_triplet_free(T);

  xi = malloc(3*n * sizeof(int));
  spasm_vector_zero(xi, 3*n);

  x = malloc(n * sizeof(spasm_GFp));
  y = malloc(n * sizeof(spasm_GFp));
  spasm_vector_zero(x, n);
  spasm_vector_zero(y, n);

  top = spasm_sparse_backward_solve(L, B, 0, xi, x, SPASM_IDENTITY_PERMUTATION, 0);

  spasm_gaxpy(L, x, y);
  for(j = m; j < n; j++) {
    y[j] = x[j];
  }
  spasm_scatter(B->j, B->x, B->p[0], B->p[1], prime - 1, y, prime);

  for(i = 0; i < n; i++) {
    if (y[i] != 0) {
      printf("not ok %d - sparse triangular L-solve on U.T (idx = %d, n=%d, m=%d)\n", test, i, n, m);
      exit(0);
    }
  }

  printf("ok %d - sparse triangular L-solve on U.T\n", test);

  spasm_csr_free(L);
  spasm_csr_free(B);
  free(xi);
  free(x);
  free(y);
  return 0;
}
