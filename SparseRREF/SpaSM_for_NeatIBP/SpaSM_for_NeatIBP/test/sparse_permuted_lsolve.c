#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *L, *B;
  spasm_lu *N;
  int n, m, test, top, *xi, *p, r;
  spasm_GFp *x, *y;

  assert(argc == 2);
  test = atoi(argv[1]);

  /* load a rank defficient matrix */
  T = spasm_load_sms(stdin, 32003);
  B = spasm_compress(T);
  spasm_triplet_free(T);
  
  /* compute LU decomposition of B */
  N = spasm_LU(B, SPASM_IDENTITY_PERMUTATION, 1);
  r = N->U->n; //<-- matrix rank
  assert(r < spasm_min(B->n, B->m));

  free(B);

  L = N->L; /* <-- permuted lower trapezoidal */
  p = N->p;
  n = L->n;
  m = L->m;

  /* load RHS */
  T = spasm_triplet_alloc(1, n, n, 32003, true);
  for(int j = m; j < n; j++)
    spasm_add_entry(T, 0, j, j);

  B = spasm_compress(T);
  spasm_triplet_free(T);

  xi = malloc(3*n * sizeof(int));
  spasm_vector_zero(xi, 3*n);

  x = malloc(n * sizeof(spasm_GFp));
  y = malloc(n * sizeof(spasm_GFp));
  spasm_vector_zero(x, n);
  spasm_vector_zero(y, n);

  top = spasm_sparse_backward_solve(L, B, 0, xi, x, p, 0);

  spasm_gaxpy(L, x, y);
  for(int j = m; j < n; j++)
    y[j] = x[p[j]];
  spasm_scatter(B->j, B->x, B->p[0], B->p[1], B->prime - 1, y, B->prime);


  for (int i = 0; i < n; i++)
    if (y[i] != 0) {
      printf("not ok %d - sparse triangular L-solve (index %d, n=%d, m=%d, y[i]=%d)\n", test, i, n, m, y[i]);
      exit(0);
    }

  printf("ok %d - sparse triangular L-solve\n", test);

  spasm_csr_free(L);
  spasm_csr_free(B);
  free(xi);
  free(x);
  free(y);
  return 0;
}
