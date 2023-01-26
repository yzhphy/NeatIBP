# -*- encoding: utf-8 -*-
K = GF(7)
n = 16 # lignes
m = 16 # colonnes

# astuce pour générer une matrice avec un défaut de rang
A = random_matrix(K, m, n)
A[2] = A[1]+3*A[0]
A = A.T
A[7] = A[2] + A[0] + A[5]

A[9] = 0

out = open("Matrix/singular", "w")
for (i,j) in A.nonzero_positions():
    out.write("{0} {1} {2}\n".format(i, j, A[i,j]))
out.close()

