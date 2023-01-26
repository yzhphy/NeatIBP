#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *A;
  spasm_GFp *x, *y, *b;
  int n, m, test, i, prime, result;

  assert(argc > 1);
  test = atoi(argv[1]);

  T = spasm_load_sms(stdin, 42013);
  A = spasm_compress(T);
  spasm_triplet_free(T);

  n = A->n;
  m = A->m;
  prime = A->prime;

  x = malloc(n * sizeof(spasm_GFp));
  y = malloc(m * sizeof(spasm_GFp));
  b = malloc(m * sizeof(spasm_GFp));

  /* test A ------------------------- with a sensible RHS ----------- */
  printf("# testing correct solution\n");
  for(i = 0; i < n; i++) {
    x[i] = rand() % prime;
  }
  for(i = 0; i < m; i++) {
    b[i] = 0;
  }
  spasm_gaxpy(A, x, b);

  for(i = 0; i < n; i++) {
    x[i] = 0;
  }
  result = spasm_PLUQ_solve(A, b, x);
  if (result != SPASM_SUCCESS) {
    printf("not ok %d - PLUQ solver [solution not found]\n", test);
    exit(0);
  }

  for(i = 0; i < m; i++) {
    y[i] = 0;
  }
  spasm_gaxpy(A, x, y);
  for(i = 0; i < m; i++) {
    if (y[i] != b[i]) {
      printf("not ok %d - PLUQ solver [incorrect solution found]\n", test);
      exit(0);
    }
  }

  /* test B ------------------------- with a bogus RHS ----------- */
  if (n < m) {
    printf("# testing bogus solution\n");
    for(i = 0; i < m; i++) {
      b[i] = rand() % prime;
    }

    result = spasm_PLUQ_solve(A, b, x);
    if (result == SPASM_SUCCESS) {
      printf("not ok %d - PLUQ solver [bogus solution found]\n", test);
      exit(0);
    }

  }

  printf("ok %d - PLUQ solver\n", test);

  spasm_csr_free(A);
  free(x);
  free(y);
  free(b);
  return 0;
}
