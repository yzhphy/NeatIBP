#include <assert.h>
#include <stdio.h>
#include <getopt.h>

#include "spasm.h"

int main(int argc, char **argv)
{
	char nnz[6];
	int prime = 42013;

	/* options descriptor */
	struct option longopts[] = {
		{"modulus", required_argument, NULL, 'p'},
		{NULL, 0, NULL, 0}
	};

	char ch;
	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
		case 'p':
			prime = atoi(optarg);
			break;
		default:
			printf("Unknown option\n");
			exit(1);
		}
	}
	argc -= optind;
	argv += optind;

	spasm_triplet *T = spasm_load_sms(stdin, prime);
	spasm *U = spasm_compress(T);
	spasm_triplet_free(T);

	int n = U->n;
	int m = U->m;
	spasm_human_format(spasm_nnz(U), nnz);
	fprintf(stderr, "start. U is %d x %d (%s nnz)\n", n, m, nnz);

	double start_time = spasm_wtime();
	int *p = spasm_malloc(n * sizeof(int));
	int *qinv = spasm_malloc(m * sizeof(int));
	int *Up = U->p;
	int *Uj = U->j;

	/* check that U is permuted lower-triangular*/
	for (int j = 0; j < m; j++)
		qinv[j] = -1;
	
	for (int i = 0; i < n; i++) {		
		for (int px = Up[i]; px < Up[i + 1]; px++)
			if (qinv[Uj[px]] != -1) {
				fprintf(stderr, "the input matrix is not (permuted) upper-triangular.\n");
				exit(1);
			}
		qinv[Uj[Up[i]]] = i;
	}

	/* build the RREF. This code is similar to src/spasm_schur.c ---> in bad need of factorization */
	spasm *R = spasm_csr_alloc(n, m, spasm_nnz(U), U->prime, SPASM_WITH_NUMERICAL_VALUES);
	int *Rp = R->p;
	int *Rj = R->j;
	int *Rx = R->x;	
	int rnz = 0;
	int writing = 0;
	int k = 0;

	#pragma omp parallel
	{
		spasm_GFp *x = spasm_malloc(m * sizeof(*x));
		int *xj = spasm_malloc(3 * m * sizeof(int));
		spasm_vector_zero(xj, 3 * m);
		int tid = spasm_get_thread_num();
		int *qinv_local = spasm_malloc(m * sizeof(int));

		for (int j = 0; j < m; j++)
			qinv_local[j] = qinv[j];
	
		#pragma omp for schedule(dynamic, 10)
	  	for (int i = 0; i < n; i++) {
	  		int j = Uj[Up[i]];
	  		assert(qinv_local[j] == i);
	  		qinv_local[j] = -1;
	  		int top = spasm_sparse_forward_solve(U, U, i, xj, x, qinv_local);
	  		
			/* ensure R has the "pivot first" property */
			for (int px = top + 1; px < m; px++)
				if (xj[px] == j) {
					xj[px] = xj[top];
					xj[top] = j;
					break;
				}
			assert(xj[top] == j);

	  		/* count the NZ in the new row */
	  		int row_nz = 0;
			for (int px = top; px < m; px++) {
				j = xj[px];
				if ((qinv_local[j] < 0) && (x[j] != 0))
					row_nz++;
			}

			int row_k, row_px;
			#pragma omp critical(rref)
			{
				/* not enough room in R ? realloc twice the size */
				if (rnz + m > R->nzmax) {
					/* wait until other threads stop writing into R */
					#pragma omp flush(writing)
					while (writing > 0) {
						#pragma omp flush(writing)
					}
					spasm_csr_realloc(R, 2 * R->nzmax + m);
					Rj = R->j;
					Rx = R->x;
				}
				/* save row k */
				row_k = k++;
				row_px = rnz;
				rnz += row_nz;
				/* this thread will write into R */
				#pragma omp atomic update
				writing++;
			}

			/* write the new row in R */
			Rp[row_k] = row_px;
			for (int px = top; px < m; px++) {
				int j = xj[px];
				if (qinv_local[j] < 0 && x[j] != 0) {
					Rj[row_px] = xj[px];
					Rx[row_px] = x[j];
					row_px++;
				}
			}

			/* we're done writing */
			#pragma omp atomic update
			writing--;

			if (tid == 0) {
				spasm_human_format(rnz, nnz);
	  			fprintf(stderr, "\rRREF: %d/%d, |R| = %s    ", i, n, nnz);
	  			fflush(stderr);
	  		}
		} /* for */
	  	free(x);
		free(xj);
		free(qinv_local);
	} /* parallel section */
	Rp[n] = rnz;
	fprintf(stderr, "\n");
	spasm_csr_free(U);

	/* build p to order the rows */
	for (int j = 0; j < m; j++)
		qinv[j] = -1;
	for (int i = 0; i < n; i++)
		qinv[Rj[Rp[i]]] = i;
	k = 0;
	for (int j = 0; j < m; j++)
		if (qinv[j] >= 0)
			p[k++] = qinv[j];
	assert(k == n);
	
	spasm *S = spasm_permute(R, p, SPASM_IDENTITY_PERMUTATION, SPASM_WITH_NUMERICAL_VALUES);
	free(p);
	spasm_csr_free(R);

	spasm_human_format(spasm_nnz(S), nnz);
	fprintf(stderr, "done in %.3f s. NNZ(R) = %s\n", spasm_wtime() - start_time, nnz);
	
	spasm_save_csr(stdout, S);
	spasm_csr_free(S);
	//spasm_save_csr(A_start);
	free(qinv);
	
	return 0;
}
