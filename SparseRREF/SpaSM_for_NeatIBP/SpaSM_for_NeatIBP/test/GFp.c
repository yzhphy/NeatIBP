#include <stdio.h>
#include "spasm.h"

int main() {
  int prime, fail;
  spasm_GFp i, j;

  printf("1..1\n");

  fail = 0;
  prime = 257;
  for(i = 1; i < prime; i++) {
    j = spasm_GFp_inverse(i, prime);
    fail |= (((j * i) % prime) != 1);
  }
  if (fail) {
    printf("not ok 1 - GFp inverse\n");
  } else {
    printf("ok 1 - GFp inverse\n");
  }

}
