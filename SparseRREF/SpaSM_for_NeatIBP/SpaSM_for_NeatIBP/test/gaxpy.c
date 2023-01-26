#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main() {
  spasm_triplet *T;
  spasm *C;
  spasm_GFp *x, *y;
  int i, n;

  T = spasm_load_sms(stdin, 257);
  C = spasm_compress(T);
  spasm_triplet_free(T);

  n = C->n;
  assert(n < C->prime);
  x = malloc(n * sizeof(spasm_GFp));
  y = malloc(n * sizeof(spasm_GFp));
  for(i = 0; i < n; i++) {
    x[i] = i + 1;
    y[i] = 0;
  }
  spasm_gaxpy(C, x, y);
  for(i = 0; i < n; i++) {
    printf("%d\n", y[i]);
  }

  spasm_csr_free(C);
  free(x);
  free(y);
  return 0;
}
