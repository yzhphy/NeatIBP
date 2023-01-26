#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  assert(argc > 1);

  int test = atoi(argv[1]);
  spasm_triplet * T = spasm_load_sms(stdin, 42013);
  spasm * A = spasm_compress(T);
  spasm_triplet_free(T);

  int n = A->n;
  int m = A->m;

  /* generate random row & col permutation */
  int * p = spasm_random_permutation(n);
  int * q = spasm_random_permutation(m);
  spasm * B = spasm_permute(A, p, q, SPASM_IGNORE_VALUES);
  free(p);
  free(q);
  spasm_csr_free(A);

  spasm_dm * P = spasm_connected_components(B, NULL);
  p = P->p;
  q = P->q;
  int * rr = P->r;
  int * cc = P->c;
  int nb = P->nb;

  /* --- check that p and q are actually permutations ---------------- */
  int * x = spasm_malloc(n * sizeof(int));
  int * y = spasm_malloc(m * sizeof(int));

  spasm_vector_zero(x, n);
  for (int i = 0; i < n; i++)
    x[p[i]]++;
  for (int i = 0; i < n; i++)
    if (x[i] != 1) {
      printf("not ok %d - CC - p is not bijective\n", test);
      exit(0);
    }

  spasm_vector_zero(y, m);
  for (int j = 0; j < m; j++)
    y[q[j]]++;
  for (int j = 0; j < m; j++)
    if (y[j] != 1) {
      printf("not ok %d - CC - q is not bijective\n", test);
      exit(0);
    }

  free(x);
  free(y);

  /* --- verbosity ---------------- */
  printf("# CC = %d\n", nb);

  for (int k = 0; k < nb; k++) {
    printf("# CC_%d : ", k);
    for (int i = rr[k]; i < rr[k + 1]; i++) {
      printf("%d ", p[i] + 1);
    }
    printf(" / ");
    for (int j = cc[k]; j < cc[k + 1]; j++) {
      printf("%d ", q[j] + 1);
    }
    printf("\n");
  }

  /* --- check that decomposition is really block-diagonal ---------------- */
  int * qinv = spasm_pinv(q, m);
  spasm * C = spasm_permute(B, p, qinv, SPASM_IGNORE_VALUES);
  spasm_csr_free(B);
  free(qinv);

  int *Cp = C->p;
  int *Cj = C->j;
  for (int k = 0; k < nb; k++)
    for (int i = rr[k]; i < rr[k + 1]; i++)
      for (int px = Cp[i]; px < Cp[i + 1]; px++) {
	int j = Cj[px];
	if (j < cc[k] || cc[k + 1] < j) {
	  printf("not ok %d - CC - row %d (in C_%d) has entries on column %d\n", test, i + 1, k, j + 1);
	  exit(0);
	}
      }

  printf("ok %d - CC\n", test);

  spasm_dm_free(P);
  spasm_csr_free(C);
  return 0;
}
