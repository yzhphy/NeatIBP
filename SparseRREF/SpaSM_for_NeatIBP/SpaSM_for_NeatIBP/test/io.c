#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main(int argc, char **argv) {
  spasm_triplet *T;
  spasm *C;
  int test;

  assert(argc > 1);
  test = atoi(argv[1]);

  T = spasm_load_sms(stdin, 46337);
  switch(test) {
  case 1:
    spasm_save_triplet(stdout, T);
    spasm_triplet_free(T);
    break;

  case 2:
    C = spasm_compress(T);
    spasm_triplet_free(T);
    spasm_save_csr(stdout, C);
    spasm_csr_free(C);
  }
}
