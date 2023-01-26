#include <stdio.h>
#include <assert.h>
#include "spasm.h"

int main() {
  spasm *C;

  C = spasm_load_gbla_new(stdin);
  spasm_save_csr(stdout, C);
  spasm_csr_free(C);
  return 0;
}