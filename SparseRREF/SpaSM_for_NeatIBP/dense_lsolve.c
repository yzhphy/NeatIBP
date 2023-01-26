#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *G;
  int i, n, m, test, prime;
  spasm_GFp *x, *b, *y;

  assert(argc > 1);
  test = atoi(argv[1]);

  T = spasm_load_sms(stdin, 32003);
  G = spasm_compress(T);
  spasm_triplet_free(T);

  n = G->n;
  m = G->m;
  prime = G->prime;
  assert(n >= m);
  assert(spasm_is_lower_triangular(G));

  x = malloc(n * sizeof(spasm_GFp));
  b = malloc(m * sizeof(spasm_GFp));
  y = malloc(m * sizeof(spasm_GFp));

  for(i = 0; i < m; i++) {
    b[i] = rand() % prime;
    y[i] = b[i];
  }

  spasm_dense_back_solve(G, y, x, SPASM_IDENTITY_PERMUTATION);

  for(i = 0; i < m; i++) {
    y[i] = 0;
  }
  spasm_gaxpy(G, x, y);
  for(i = 0; i < m; i++) {
    if (y[i] != b[i]) {
      printf("not ok %d - dense back-substitution triangular solver [incorrect solution found]\n", test);
      exit(1);
    }
  }

  printf("ok %d -  dense back-substitution triangular solver\n", test);

  spasm_csr_free(G);
  free(x);
  free(y);
  free(b);
  return 0;
}
