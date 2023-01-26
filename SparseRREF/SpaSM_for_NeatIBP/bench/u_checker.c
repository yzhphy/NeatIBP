#include <assert.h>
#include <stdio.h>
#include <getopt.h>
#include <err.h>

#include "spasm.h"

/* so far, this checks that the U matrix is in REF, and that rowspan(A) is included in rowspan(U) */
int * echelon_form_check(spasm *U)
{
	int m = U->m;
	int r = U->n;
	
	int *Up = U->p;
	int *Uj = U->j;
	spasm_GFp *Ux = U->x;

	int *qinv = spasm_malloc(m * sizeof(int));

	fprintf(stderr, "---> Checking that U is really in echelon form...\n");
	for (int j = 0; j < m; j++)
		qinv[j] = -1;

	for (int i = 0; i < r; i++) {
		if (Up[i + 1] == Up[i])
			errx(1, "Row %d of U is empty!\n", i);
		int j = Uj[Up[i]];
		if (qinv[j] != -1)
			errx(1, "First entry of row %d is under another pivot at (%d, %d)!\n", i, qinv[j], j);
		if (Ux[Up[i]] != 1)
			errx(1, "Pivot at (%d, %d) is not unitary!\n", i, j);
		qinv[j] = i;
	}
	return qinv;
}


void deterministic_inclusion_test(spasm *A, spasm *U, int *qinv)
{
	fprintf(stderr, "---> Checking that rowspan(A) is included in rowspan(U) [deterministic]...\n");
	int done = 0;
	int n = A->n;
	int m = A->m;
	#pragma omp parallel
	{
		int tid = spasm_get_thread_num();
		int *xj = spasm_malloc(3*m * sizeof(int));
	  	spasm_GFp *x = spasm_malloc(m * sizeof(spasm_GFp));
	  	for (int j = 0; j < m; j++)
	  		xj[j] = 0;
	  
	  	#pragma omp for schedule(dynamic, 10)
	  	for (int i = 0; i < n; i++) {
	  		int top = spasm_sparse_forward_solve(U, A, i, xj, x, qinv);
	  		
			for (int px = top; px < m; px++) {
				int j = xj[px];
				if ((qinv[j] == -1) && (x[j] != 0))
					errx(1, "Row %d of A does not belong to rowspan(U)!\n", i);
			}

			#pragma omp atomic
			done++;

			if (tid == 0) {
				fprintf(stderr, "\r%d/%d rows checked", done, n);
				fflush(stderr);
			}

		}
		free(x);
		free(xj);
	}
	fprintf(stderr, "\n");
}


void probabilistic_inclusion_test(spasm *A, spasm *U, int n_iterations)
{
	fprintf(stderr, "---> Checking that rowspan(A) is included in rowspan(U) [probabilistic, %d iterations]...\n", n_iterations);
	
	int prime = A->prime;
	int n = A->n;
	int m = A->m;
	int r = U->n;
	int *Ap = A->p;
	int *Aj = A->j;
	spasm_GFp *Ax = A->x;
	int *Up = U->p;
	int *Uj = U->j;
	spasm_GFp *Ux = U->x;

	int done = 0;
	#pragma omp parallel
	{
		int tid = spasm_get_thread_num();
		spasm_GFp *x = spasm_malloc(m * sizeof(spasm_GFp));
	
		#pragma omp for schedule(dynamic, 1)
		for (int k = 0; k < n_iterations; k++) {
			/* x <--- random linear combination of the rows of A */
			for (int j = 0; j < m; j++)
				x[j] = 0;
			for (int i = 0; i < n; i++)
				spasm_scatter(Aj, Ax, Ap[i], Ap[i + 1], rand() % prime, x, prime);
			/* eliminate everything in x */
			for (int i = 0; i < r; i++) {
				int j = Uj[Up[i]];
				if (x[j] != 0)
					spasm_scatter(Uj, Ux, Up[i], Up[i + 1], prime - x[j], x, prime);
			}
			for (int j = 0; j < m; j++)
				if ((x[j] != 0))
					errx(1, "rowspan(A) not included in rowspan(U)! (--deterministic gives a more precise diagnostic)\n");		
			done++;

			if (tid == 0) {
				fprintf(stderr, "\r%d/%d linear combinations checked", done, n_iterations);
				fflush(stderr);
			}
		}
		free(x);
	}
	fprintf(stderr, "\n");
}


spasm *load_matrix(char *filename, int prime)
{
	FILE *file = fopen(filename, "r");
	if (!file)
		err(1, "impossible to open %s: ", filename);
	spasm_triplet *T = spasm_load_sms(file, prime);
	spasm *A = spasm_compress(T);
	spasm_triplet_free(T);
	return A;	
}

/** given an arbitrary matrix A and an echelonized matrix U, check that rowspan(A) == rowspan(U). */
int main(int argc, char **argv)
{
	int prime = 42013;
	int deterministic = 0;

	/* options descriptor */
	struct option longopts[7] = {
		{"A", required_argument, NULL, 'A'},
		{"L", required_argument, NULL, 'L'},
		{"U", required_argument, NULL, 'U'},
		{"deterministic", no_argument, NULL, 'd'},
		{"modulus", required_argument, NULL, 'p'},
		{NULL, 0, NULL, 0}
	};


	char *A_filename = NULL;
	char *U_filename = NULL;
	char *L_filename = NULL;

	char ch;
	while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
		switch (ch) {
		case 'A':
			A_filename = optarg;
			break;
		case 'U':
			U_filename = optarg;
			break;
		case 'p':
			prime = atoi(optarg);
			break;
		case 'd':
			deterministic = 1;
			break;
		default:
			printf("Unknown option\n");
			exit(1);
		}
	}
	argc -= optind;
	argv += optind;

	if (!A_filename || !U_filename) {
		printf("USAGE: %s --A [filename.sms] --U [filename.sms] (--modulus [42013]) (--deterministic)\n", argv[0]);
		exit(1);
	}

	fprintf(stderr, "---> Loading A....\n");
	spasm *A = load_matrix(A_filename, prime);

	fprintf(stderr, "---> Loading U....\n");
	spasm *U = load_matrix(U_filename, prime);	

	if (A->m != U->m)
		errx(1, "Column dimension mismatch!\n");
	
	if (A->n < U->n)
		errx(1, "U has more rows than A!\n");


	int *qinv = echelon_form_check(U);
	if (deterministic)
		deterministic_inclusion_test(A, U, qinv);
	else
		probabilistic_inclusion_test(A, U, 100);
	
	// probabilistic_rank_check(A, U, qinv, 100);

	spasm_csr_free(A);
	spasm_csr_free(U);
	free(qinv);
	
	return 0;
}
