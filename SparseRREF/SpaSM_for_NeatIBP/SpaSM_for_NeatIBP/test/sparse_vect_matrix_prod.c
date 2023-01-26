#include <stdio.h>
#include <assert.h>
#include "spasm.h"


int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *C;
  spasm_GFp *x, *y, *z;
  int i, n, m, nz, xnz, *xi, *yi, test;

  assert(argc > 1);
  test = atoi(argv[1]);

  T = spasm_load_sms(stdin, 277);
  C = spasm_compress(T);
  spasm_triplet_free(T);

  n = C->n;
  m = C->m;
  xnz = 3;
 
  xi = malloc(xnz * sizeof(int));
  x = malloc(n * sizeof(spasm_GFp));
  y = malloc(m * sizeof (spasm_GFp));
  z = malloc(m * sizeof(spasm_GFp));
  yi = malloc(m * sizeof(int));

  for(i = 0; i < m; i++) {
    y[i] = 0;
    z[i] = 0;
    yi[i] = 0;
  }

 for(i = 0; i < n; i++) {
    x[i] = 0;
  }
 
 x[0] = 1;
 x[n/2] = 1;
 x[n-1] = 1;

 xi[0] = 0;
 xi[1] = n/2;
 xi[2] = n-1;

 nz = spasm_sparse_vector_matrix_prod(C, x, xi, xnz, y, yi);

 spasm_gaxpy(C, x, z);
 for (i = 0; i < m; i++) {
   z[i] = z[i] - y[i];
   if (z[i] != 0) {
     printf("not ok\n");
     exit(0);
   }
 }

 printf("ok %d - sparse axpy\n", test);
 spasm_csr_free(C);
 return 0;
}
