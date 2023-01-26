# -*- encoding: utf-8 -*-
K = GF(42013)
n = 100000 # lignes
m = 1000 # colonnes

A = random_matrix(K, n, m, density=0.01, sparse=True)

out = open("A.sms", "w")
out.write("{0} {1} M\n".format(n, m))
for (i,j) in A.nonzero_positions():
    out.write("{0} {1} {2}\n".format(i + 1, j + 1, A[i,j]))
out.write("0 0 0\n")
out.close()
