#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *A;
  int i, j, n, m;
  int test;
  spasm_GFp *x, *y, *z;

  assert(argc > 1);
  test = atoi(argv[1]);

  T = spasm_load_sms(stdin, 46337);
  A = spasm_compress(T);
  spasm_triplet_free(T);

  n = A->n;
  m = A->m;

  assert(5 < m/2);

  int first_col[4] = {0, 5, m/2, m};
  int n_blocks = 3;
  int *Q = malloc(m*sizeof(int));
  for(i=0; i<5; i++) {
    Q[i] = 0;
  }
  for(i=5; i<m/2; i++) {
    Q[i] = 1;
  }
  for(i=m/2; i<m; i++) {
    Q[i] = 2;
  }
  
  super_spasm **column_slices = super_spasm_column_slices(A, Q, first_col, n_blocks, 1);

  assert(n < A->prime);
  x = malloc(n * sizeof(spasm_GFp));
  y = malloc(m * sizeof(spasm_GFp));
  z = malloc(m * sizeof(spasm_GFp));
  for(i = 0; i < n; i++) {
    x[i] = i + 1;
  }
  for(j = 0; j < m; j++) {
    y[j] = 0;
    z[j] = 0;
  }
  spasm_gaxpy(A, x, y);

  for(i=0; i<3; i++) {
    super_sparse_gaxpy_dense(column_slices[i], x, &z[first_col[i]]);
  }

  for(j = 0; j < m; j++) {
    printf("%d\n", y[j]);
    if (y[j] != z[j]) {
	printf("not ok %d - error on col %d \n", test, j);
	exit(0);
      }
  }

  printf("ok %d - column slices \n", test);

  // cleanup
  for(i=0; i<3; i++) {
    super_spasm_free(column_slices[i]);
  }
  free(column_slices);
  spasm_csr_free(A);
}
