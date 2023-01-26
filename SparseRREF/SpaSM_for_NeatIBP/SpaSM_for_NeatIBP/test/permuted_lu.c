#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *A;
  spasm_lu *LU;
  spasm_GFp *w, *x, *y, *u, *v;
  int n, m, test, i, j, *p, *qinv;

  assert(argc > 1);
  test = atoi(argv[1]);

  T = spasm_load_sms(stdin, 42013);
  A = spasm_compress(T);
  spasm_triplet_free(T);

  n = A->n;
  m = A->m;

  w = spasm_malloc(n * sizeof(spasm_GFp));
  x = spasm_malloc(n * sizeof(spasm_GFp));
  y = spasm_malloc(m * sizeof(spasm_GFp));
  u = spasm_malloc(n * sizeof(spasm_GFp));
  v = spasm_malloc(m * sizeof(spasm_GFp));
  
  p = spasm_malloc(n * sizeof(int));
  qinv = spasm_malloc(m * sizeof(int));
  spasm_find_pivots(A, p, qinv);
  LU = spasm_LU(A, p, SPASM_KEEP_L);

  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      x[j] = 0;
      u[j] = 0;
    }
    for(j = 0; j < m; j++) {
      y[j] = 0;
      v[j] = 0;
    }
    x[i] = 1;

    spasm_ipvec(p, x, w, n);
    spasm_gaxpy(A, w, y);     // y <- x*P*A
    spasm_gaxpy(LU->L, x, u); // u <- x*L
    spasm_gaxpy(LU->U, u, v); // v <- (x*L)*U

    for(j = 0; j < m; j++) {
      if (y[j] != v[j]) {
	      printf("not ok %d - L*U == P*A (col %d)\n", test, j);
	      exit(0);
      }
    }
  }

  printf("ok %d - L*U == P*A\n", test);

  spasm_csr_free(A);
  spasm_free_LU(LU);
  free(x);
  free(y);
  free(u);
  free(v);
  free(p);
  free(qinv);
  return 0;
}
