#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *A;
  int n, m, test, size, k;
  int *p, *qinv, *Aj, *Ap;

  assert(argc > 1);
  test = atoi(argv[1]);

  T = spasm_load_sms(stdin, 42013);
  A = spasm_compress(T);
  spasm_triplet_free(T);

  n = A->n;
  m = A->m;
  Ap = A->p;
  Aj = A->j;

  p = spasm_malloc(n * sizeof(int));
  qinv = spasm_malloc(m * sizeof(int));

  size = spasm_maximum_matching(A, p, qinv);
  
  /* check that size is correct */
  k = 0;
  for(int i = 0; i < n; i++)
    if (p[i] >= 0)
      k++;
  if (k != size) {
      printf("not ok %d - maximum matching - size of row matching is wrong (%d vs %d)\n", test, size, k);
      exit(0);
    }

  k = 0;
  for(int j = 0; j < m; j++)
    if (qinv[j] >= 0)
      k++;
  if (k != size) {
      printf("not ok %d - maximum matching - size of column matching is wrong\n", test);
      exit(0);
    }

  /* check consistency */
  for(int j = 0; j < m; j++)
    if (qinv[j] >= 0)
      if (p[qinv[j]] != j) {
        printf("not ok %d - maximum matching - row and column matchings are inconsistent\n", test);
        exit(0);
      }
  
  /* check that the entries actually exist */
  for(int i = 0; i < n; i++) {
    if (p[i] < 0)
      continue;
    int found = 0;
    for(int px = Ap[i]; px < Ap[i+1]; px++)
      if (Aj[px] == p[i]) {
        found = 1;
        break;
      }
    if (!found) {
      printf("not ok %d - maximum matching - matching entries do not exist\n", test);
      exit(0);
    }
  }

  free(p);
  free(qinv);
  spasm_csr_free(A);
  printf("ok %d - maximum matching\n", test);
  return 0;
}
