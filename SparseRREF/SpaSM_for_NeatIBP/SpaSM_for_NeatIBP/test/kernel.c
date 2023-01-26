#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv)
{
  assert(argc > 1);
  int test = atoi(argv[1]);

  spasm_triplet *T = spasm_load_sms(stdin, 42013);
  // assert(T->x != NULL);

  spasm * A = spasm_compress(T);
  // assert(A->x != NULL);
  spasm_triplet_free(T);

  int n = A->n;
  int m = A->m;

  spasm * A_t = spasm_transpose(A, SPASM_WITH_NUMERICAL_VALUES);
  spasm * K = spasm_kernel(A_t, SPASM_IDENTITY_PERMUTATION);
  spasm_csr_free(A_t);
  int k = K->n;
  int * Kp = K->p;
  int * Kj = K->j;
  spasm_GFp * Kx = K->x;

  assert(K->m == A->n);

  spasm_GFp * x = spasm_malloc(n * sizeof(spasm_GFp));
  spasm_GFp * y = spasm_malloc(m * sizeof(spasm_GFp));

  /* test that they are really kernel vectors */
  for (int i = 0; i < k; i++) {
    printf("# testing vector %d\n", i);
    spasm_vector_zero(x, n);
    spasm_vector_zero(y, m);

    /* check that vector is not zero */
    if (spasm_row_weight(K, i) == 0) {
      printf("not ok %d - empty vector in kernel\n", test);
      exit(0);
    }

    /* scatter K[i] into x */
    int nonzero = 0;
    for (int p = Kp[i]; p < Kp[i + 1]; p++) {
      x[ Kj[p] ] = Kx[p];
      nonzero += (Kx[p] != 0);
    }

    if (nonzero == 0) {
      printf("not ok %d - zero vector in kernel\n", test);
      exit(0);
    }

    /* y <-- x.A */
    spasm_gaxpy(A, x, y);

    for (int j = 0; j < m; j++) {
      if (y[j] != 0) {
      printf("not ok %d - vector not in kernel\n", test);
      exit(0);
      }
    }
  }

  printf("ok %d - (left-)kernel basis\n", test);

  spasm_csr_free(A);
  spasm_csr_free(K);
  free(x);
  free(y);
  return 0;
}
