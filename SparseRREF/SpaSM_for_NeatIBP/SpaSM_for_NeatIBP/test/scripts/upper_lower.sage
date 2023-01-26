#
# sage utility script to generate random sparse lower-triangular matrix
#
F = GF(257)
n = 100

# lower-triangular
M = random_matrix(F, n, n, density=0.25, sparse=True)
for i in range(M.nrows()):
    M[i,i] = 1
    for j in range(i+1, M.ncols()):
        M[i,j] = 0

out = open("Matrix/l1", "w")
for (i,j) in M.nonzero_positions():
     out.write("{0} {1} {2}\n".format(i, j, M[i,j]))
out.close()

# cas trapezoidal
M = M[:,:25]
out = open("Matrix/lower_trapeze", "w")
for (i,j) in M.nonzero_positions():
     out.write("{0} {1} {2}\n".format(i, j, M[i,j]))
out.close()


# upper-triangular
M = random_matrix(F, n, n, density=0.25, sparse=True)
for i in range(M.nrows()):
     M[i,i] = 1
     for j in range(i):
         M[i,j] = 0

out = open("Matrix/u1", "w")
for (i,j) in M.nonzero_positions():
     out.write("{0} {1} {2}\n".format(i, j, M[i,j]))
out.close()

#cas trapezoidal
M = M[:25]
out = open("Matrix/upper_trapeze", "w")
for (i,j) in M.nonzero_positions():
     out.write("{0} {1} {2}\n".format(i, j, M[i,j]))
out.close()

