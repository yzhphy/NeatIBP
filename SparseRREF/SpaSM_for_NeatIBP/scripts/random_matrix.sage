# -*- encoding: utf-8 -*-
K = GF(7)
n = 16 # lignes
m = 32 # colonnes

A = random_matrix(K, n, m, density=0.33, sparse=True)

out = open("Matrix/rectangular_l", "w")
for (i,j) in A.nonzero_positions():
    out.write("{0} {1} {2}\n".format(i, j, A[i,j]))
out.close()

n = 32 # lignes
m = 16 # colonnes

A = random_matrix(K, n, m, density=0.33, sparse=True)

out = open("Matrix/rectangular_h", "w")
for (i,j) in A.nonzero_positions():
    out.write("{0} {1} {2}\n".format(i, j, A[i,j]))
out.close()

