#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *A, *U, *L;
  spasm_lu *PLUQ;
  spasm_GFp *x, *y, *u, *v, *w, *z;
  int n, m, r, test, i, j;

  assert(argc > 1);
  test = atoi(argv[1]);

  T = spasm_load_sms(stdin, 42013);
  A = spasm_compress(T);
  spasm_triplet_free(T);

  n = A->n;
  m = A->m;

  PLUQ = spasm_PLUQ(A, SPASM_IDENTITY_PERMUTATION, SPASM_KEEP_L);
  U = PLUQ->U;
  L = PLUQ->L;
  r = U->n;


  x = malloc(n * sizeof(spasm_GFp));
  y = malloc(m * sizeof(spasm_GFp));
  u = malloc(n * sizeof(spasm_GFp));
  v = malloc(r * sizeof(spasm_GFp));
  w = malloc(m * sizeof(spasm_GFp));
  z = malloc(m * sizeof(spasm_GFp));

  if (spasm_is_upper_triangular(U)) {
    printf("# PLUQ ---> U is upper\n");
  } else {
    printf("not ok %d - PLUQ : U is not upper-triangular\n", test);
    exit(0);
  }

  if (spasm_is_lower_triangular(L)) {
    printf("# PLUQ ---> L is upper\n");
  } else {
    printf("not ok %d - PLUQ : L is not lower-triangular\n", test);
    exit(0);
  }


  for(i = 0; i < n; i++) {
    for(j = 0; j < n; j++) {
      x[j] = 0;
    }
    for(j = 0; j < r; j++) {
      v[j] = 0;
    }
    for(j = 0; j < m; j++) {
      y[j] = 0;
      w[j] = 0;
    }
    x[i] = 1;

    spasm_gaxpy(A, x, y);             // y <- x*A

    spasm_pvec(PLUQ->p, x, u, n);     // u <--- x.P
    spasm_gaxpy(L, u, v);             // v <--- x.(P.L)
    spasm_gaxpy(U, v, w);             // w <--- x.(P.L.U)
    spasm_pvec(PLUQ->qinv, w, z, m);  // u <--- x.(P.L.U.Q)

    for(j = 0; j < m; j++) {
      if (y[j] != z[j]) {
	printf("not ok %d - A == PLUQ (col %d)\n", test, j);
	exit(0);
      }
    }
  }
  printf("ok %d - PLUQ == A\n", test);

  spasm_csr_free(A);
  spasm_free_LU(PLUQ);
  free(x);
  free(y);
  free(u);
  free(v);
  free(w);
  free(z);
  return 0;
}
