#
# sage utility script to generate random sparse lower-triangular matrix
#
F = GF(40009)
n = 10000

# lower-triangular
M = matrix(F, n, n, sparse=True)
M[0,0  ] = 1
M[0,1] = F.random_element()
for i in range(1, n-1):
    M[i,i-1] = F.random_element()
    M[i,i  ] = F.random_element()
    M[i,i+1] = F.random_element()
M[n-1,n-2] = F.random_element()
M[n-1,n-1] = 1

out = open("bench/Matrices/tridiagonal/{0}.sms".format(n), "w")
out.write("{0} {1} M\n".format(n, n))
for (i,j) in M.nonzero_positions():
     out.write("{0} {1} {2}\n".format(i + 1, j + 1, M[i,j]))
out.write("0 0 0\n".format(n, n))
out.close()
