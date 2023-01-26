/* indent -nfbs -i2 -nip -npsl -di0 -nut spasm_GFp.c */
#include <assert.h>
#include "spasm.h"

spasm_GFp spasm_GFp_inverse(spasm_GFp a, int prime) {
	int b0 = prime, t, q;
	int x0 = 0, x1 = 1;

	assert(prime > 1);
	while (a > 1) {
		q = a / prime;
		t = prime, prime = a % prime, a = t;
		t = x0, x0 = x1 - q * x0, x1 = t;
	}
	if (x1 < 0)
		x1 += b0;
	return x1;
}
