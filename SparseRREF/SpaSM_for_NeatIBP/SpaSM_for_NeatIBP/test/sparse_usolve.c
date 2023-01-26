#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *U, *B;
  int i, n, m, test, *xi, *pinv;
  spasm_GFp *x, *y;

  assert(argc == 2);
  test = atoi(argv[1]);

  // load matrix
  T = spasm_load_sms(stdin, 32003);
  U = spasm_compress(T);
  spasm_triplet_free(T);
  n = U->n;
  m = U->m;

  assert( n<= m); // upper-trapezoidal

  // load RHS
  T = spasm_triplet_alloc(1, m, 10, 32003, true);
  spasm_add_entry(T, 0, 0, 1);
  spasm_add_entry(T, 0, m / 2, 2);
  spasm_add_entry(T, 0, m - 1, 3);
  B = spasm_compress(T);
  spasm_triplet_free(T);

  xi = malloc(3*m * sizeof(int));
  spasm_vector_zero(xi, 3*m);

  x = malloc(m * sizeof(spasm_GFp));
  y = malloc(m * sizeof(spasm_GFp));
  spasm_vector_zero(x, m);
  spasm_vector_zero(y, m);

  pinv = NULL;
  if (n < m) { /* upper-trapezoidal */
    pinv = malloc(m * sizeof(int));
    for(i = 0; i < n; i++) {
      pinv[i] = i;
    }
    for(i = n; i < m; i++) {
      pinv[i] = -1;
    }
  }

  spasm_sparse_forward_solve(U, B, 0, xi, x, pinv);

  spasm_gaxpy(U, x, y);
  for(i = n; i < m; i++) {
    y[i] = (y[i] + x[i]) % B->prime;
  }
  spasm_scatter(B->j, B->x, B->p[0], B->p[1], B->prime - 1, y, B->prime);

  for(i = 0; i < m; i++) {
    if (y[i] != 0) {
      printf("not ok %d - sparse triangular U-solve\n", test);
      exit(0);
    }
  }

  printf("ok %d - sparse triangular U-solve\n", test);

  spasm_csr_free(U);
  spasm_csr_free(B);
  free(xi);
  free(x);
  free(y);
  return 0;
}
