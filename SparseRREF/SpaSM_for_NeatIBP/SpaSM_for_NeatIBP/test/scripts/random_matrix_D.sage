# -*- encoding: utf-8 -*-
K = GF(42013)
n = 10000 # lignes
m = 1000 # colonnes

l = 100

A = random_matrix(K, l, m, density=0.05, sparse=True)

B = matrix(K, n, m, sparse=True)

for i in range(n-1):
	B[i] = random_matrix(K, 1, l, density=0.05, sparse=True)[0] * A
B[n-1] = random_matrix(K, 1, m, density=0.05, sparse=True)[0]

out = open("D.sms", "w")
out.write("{0} {1} M\n".format(n, m))
for (i,j) in B.nonzero_positions():
    out.write("{0} {1} {2}\n".format(i + 1, j + 1, B[i,j]))
out.write("0 0 0\n")
out.close()
