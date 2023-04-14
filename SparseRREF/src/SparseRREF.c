#include "mathlink.h"
#include "WolframLibrary.h"
#include "WolframSparseLibrary.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "/home/rwinters/Documents/RREF_bak/Unleased_beta_test_20221008/src/spasm.h"

#include <getopt.h>
#include <math.h>
#include <err.h>
#include <sys/time.h>

#ifdef SPASM_TIMING
extern int64 reach, scatter, data_shuffling;
#endif
#include <unistd.h>

DLLEXPORT mint WolframLibrary_getVersion( ) {
	return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize( WolframLibraryData libData) {
	return 0;
}

DLLEXPORT int sparse_properties( WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) 
{
    int err = LIBRARY_NO_ERROR;
    char *what;
    mint *data;
    MSparseArray S;
    MTensor *T, Tres = 0;
    WolframSparseLibrary_Functions sparseFuns = libData->sparseLibraryFunctions;

    if (Argc != 2) return LIBRARY_FUNCTION_ERROR;

    what = MArgument_getUTF8String(Args[0]);
    S = MArgument_getMSparseArray(Args[1]);

    if (!strcmp(what, "ImplicitValue")) {
        T = (*(sparseFuns->MSparseArray_getImplicitValue))(S);
    } else if (!strcmp(what, "ExplicitValues")) {
        T = (*(sparseFuns->MSparseArray_getExplicitValues))(S);
    } else if (!strcmp(what, "RowPointers")) {
        T = (*(sparseFuns->MSparseArray_getRowPointers))(S);
    } else if (!strcmp(what, "ColumnIndices")) {
        T = (*(sparseFuns->MSparseArray_getColumnIndices))(S);
    } else if (!strcmp(what, "ExplicitPositions")) {
        err = (*(sparseFuns->MSparseArray_getExplicitPositions))(S, &Tres);
    } else if (!strcmp(what, "Normal")) {
        err = (*(sparseFuns->MSparseArray_toMTensor))(S, &Tres);
    } else {
        err = LIBRARY_FUNCTION_ERROR;
    }
    if (err) return err;
    if (!Tres) (*(libData->MTensor_clone))(*T, &Tres);

    MArgument_setMTensor(Res, Tres);
    return err;
}

DLLEXPORT int rowreduce( WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) 
{
	int Err = LIBRARY_NO_ERROR;

	MTensor Pos = 0;
	MTensor Val = 0;
	MTensor Dims = 0;

	MTensor value = 0;
	MTensor positions = 0;
	MTensor dimensionsbelow = 0;
	mint nonzerovalues;
	mint prime;
    MSparseArray A;

    mint x = 0;

	mint tmp[2];
	mint dims[2];
	
    char nnz[6];

    WolframSparseLibrary_Functions sparseFuncs = libData->sparseLibraryFunctions;

	if(Argc!=5) return LIBRARY_FUNCTION_ERROR; 

	Pos = MArgument_getMTensor(Args[0]);
	Val = MArgument_getMTensor(Args[1]);
	Dims = MArgument_getMTensor(Args[2]);
	nonzerovalues = MArgument_getInteger(Args[3]);
	prime = MArgument_getInteger(Args[4]);

	mint *vals;

	mint *pos;

	mint *dimensions;

		vals = libData->MTensor_getIntegerData(Val);
		
		pos = libData->MTensor_getIntegerData(Pos);

		dimensions = libData->MTensor_getIntegerData(Dims);

		int i = dimensions[0];
		int j = dimensions[1];

		/* allocate result */
		spasm_triplet *Test;
		Test = spasm_triplet_alloc(i, j, 1, prime, prime != -1);

		for(int m = 0; m < nonzerovalues; m++){
			int g = pos[2*m];
			int h = pos[2*m+1];
			int o = vals[m];
			spasm_add_entry(Test, g-1, h-1, o);
		}

		spasm_triplet_transpose(Test);
		spasm *Mtest;
		Mtest = spasm_compress(Test);
		spasm_triplet_free(Test);

		spasm_lu *LU; 
		LU= spasm_LU(Mtest, SPASM_IDENTITY_PERMUTATION, 1);		
		spasm *U;
		U = spasm_transpose(LU->L, 1);
		spasm_make_pivots_unitary(U, SPASM_IDENTITY_PERMUTATION, U->n);
		spasm_free_LU(LU);

		int n = U->n;
		int m = U->m;
		spasm_human_format(spasm_nnz(U), nnz);
		int *p = spasm_malloc(n * sizeof(int));
		int *qinv = spasm_malloc(m * sizeof(int));
		int *Up = U->p;
		int *Uj = U->j;

		for (int j = 0; j < m; j++)
		qinv[j] = -1;
	
		for (int i = 0; i < n; i++) {		
			for (int px = Up[i]; px < Up[i + 1]; px++)
				if (qinv[Uj[px]] != -1) {
					exit(1);
				}
			qinv[Uj[Up[i]]] = i;
		}

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

				long row_k, row_px;
				#pragma omp critical(rref)
				{
					/* not enough room in R ? realloc twice the size */
					if (rnz + m > R->nzmax) {
						/* wait until other threads stop writing into R */
						#pragma omp flush(writing)
						while (writing > 0) {
							#pragma omp flush(writing)
						}
						spasm_csr_realloc(R, R->nzmax + m);
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
	  			//fprintf(stderr, "\rRREF: %d/%d, |R| = %s    ", i, n, nnz);
	  			
	  			fflush(stderr);
	  		}
			  //printf("%d\n",i);
			//printf("check\n");
		} /* for */
			free(x);
			free(xj);
			free(qinv_local);
		} 
		Rp[n] = rnz;
		spasm_csr_free(U);

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

		m = 0;
		n = 0;
		int l = 0;
		int *Sj = S->j;
		int *Sp = S->p;
		spasm_GFp *Sx = S->x;
		n = S->n;
		m = S->m;
		long nnzs = spasm_nnz(S);
		MTensor Imp = 0;
		MSparseArray r = 0;

		dims[0] = nnzs;
		dims[1] = 2;
		Err = libData->MTensor_new(MType_Integer, 2, dims, &positions);
		dims[1] = 1;
		Err = libData->MTensor_new(MType_Integer, 1, dims, &value);
		Err = libData->MTensor_new(MType_Integer, 1, dims, &dimensionsbelow);
	
		for(l = 0; l < n; l++){
			for(long pl = Sp[l]; pl < Sp[l+1]; pl++){
			tmp[0] = pl+1;
			tmp[1] = 1;
			spasm_GFp y = (Sx != NULL) ? Sx[pl] : 1;
			y = (y > prime / 2) ? y - prime : y;
			Err = libData->MTensor_setInteger(value, tmp, y);
			Err = libData->MTensor_setInteger(positions, tmp, l+1);
			tmp[1] = 2;
			Err = libData->MTensor_setInteger(positions, tmp, Sj[pl]+1);
			}
		}



	Err = sparseFuncs->MSparseArray_fromExplicitPositions(positions, value, Dims, Imp, &r);

	if (!Err)
    MArgument_setMSparseArray(Res, r);

    return Err;
}

DLLEXPORT int findpivots( WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) 
{
	int Err = LIBRARY_NO_ERROR;

	MTensor Pos = 0;
	MTensor Val = 0;
	MTensor Dims = 0;

	MTensor value = 0;
	MTensor positions = 0;
	MTensor dimensionsbelow = 0;
	mint nonzerovalues;
	mint prime;
    MSparseArray A;

    mint x = 0;

	mint tmp[2];
	mint dims[2];
	
    char nnz[6];

    WolframSparseLibrary_Functions sparseFuncs = libData->sparseLibraryFunctions;

	if(Argc!=5) return LIBRARY_FUNCTION_ERROR; 

	Pos = MArgument_getMTensor(Args[0]);
	Val = MArgument_getMTensor(Args[1]);
	Dims = MArgument_getMTensor(Args[2]);
	nonzerovalues = MArgument_getInteger(Args[3]);
	prime = MArgument_getInteger(Args[4]);

	mint *vals;

	mint *pos;

	mint *dimensions;

		vals = libData->MTensor_getIntegerData(Val);
		
		pos = libData->MTensor_getIntegerData(Pos);

		dimensions = libData->MTensor_getIntegerData(Dims);

		int i = dimensions[0];
		int j = dimensions[1];

		/* allocate result */
		spasm_triplet *Test;
		Test = spasm_triplet_alloc(i, j, 1, prime, prime != -1);

		for(int m = 0; m < nonzerovalues; m++){
			int g = pos[2*m];
			int h = pos[2*m+1];
			int o = vals[m];
			spasm_add_entry(Test, g-1, h-1, o);
		}

		spasm_triplet_transpose(Test);
		spasm *Mtest;
		Mtest = spasm_compress(Test);
		spasm_triplet_free(Test);

		spasm_lu *LU; 
		LU= spasm_LU(Mtest, SPASM_IDENTITY_PERMUTATION, 1);		
		spasm *U;
		U = spasm_transpose(LU->L, 1);
		spasm_make_pivots_unitary(U, SPASM_IDENTITY_PERMUTATION, U->n);
		spasm_free_LU(LU);

		int n = U->n;
		int m = U->m;
		spasm_human_format(spasm_nnz(U), nnz);
		int *p = spasm_malloc(n * sizeof(int));
		int *qinv = spasm_malloc(m * sizeof(int));
		int *Up = U->p;
		int *Uj = U->j;

		for (int j = 0; j < m; j++)
		qinv[j] = -1;
	
		for (int i = 0; i < n; i++) {		
			for (int px = Up[i]; px < Up[i + 1]; px++)
				if (qinv[Uj[px]] != -1) {
					exit(1);
				}
			qinv[Uj[Up[i]]] = i;
		}

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

				long row_k, row_px;
				#pragma omp critical(rref)
				{
					/* not enough room in R ? realloc twice the size */
					if (rnz + m > R->nzmax) {
						/* wait until other threads stop writing into R */
						#pragma omp flush(writing)
						while (writing > 0) {
							#pragma omp flush(writing)
						}
						spasm_csr_realloc(R, R->nzmax + m);
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
	  			//fprintf(stderr, "\rRREF: %d/%d, |R| = %s    ", i, n, nnz);
	  			
	  			fflush(stderr);
	  		}
			  //printf("%d\n",i);
			//printf("check\n");
		} /* for */
			free(x);
			free(xj);
			free(qinv_local);
		} 
		Rp[n] = rnz;
		spasm_csr_free(U);

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

		m = 0;
		n = 0;
		int l = 0;
		int *Sj = S->j;
		int *Sp = S->p;
		spasm_GFp *Sx = S->x;
		n = S->n;
		m = S->m;
		long nnzs = spasm_nnz(S);
		MTensor Imp = 0;
		MSparseArray r = 0;

		// dims[0] = nnzs;
		dims[0] = n;
		// dims[1] = 2;
		dims[1] = 1;
		Err = libData->MTensor_new(MType_Integer, 1, dims, &positions);
		// Err = libData->MTensor_new(MType_Integer, 2, dims, &positions);
		// dims[1] = 1;
		// Err = libData->MTensor_new(MType_Integer, 1, dims, &value);
		// Err = libData->MTensor_new(MType_Integer, 1, dims, &dimensionsbelow);
	
		// for(l = 0; l < n; l++){
		// 	for(long pl = Sp[l]; pl < Sp[l+1]; pl++){
		// 	tmp[0] = pl+1;
		// 	tmp[1] = 1;
		// 	spasm_GFp y = (Sx != NULL) ? Sx[pl] : 1;
		// 	y = (y > prime / 2) ? y - prime : y;
		// 	Err = libData->MTensor_setInteger(value, tmp, y);
		// 	Err = libData->MTensor_setInteger(positions, tmp, l+1);
		// 	tmp[1] = 2;
		// 	Err = libData->MTensor_setInteger(positions, tmp, Sj[pl]+1);
		// 	}
		// }


		for(l = 0; l < n; l++){
			long pl = Sp[l];
			tmp[0] = l+1;
			// tmp[1] = 1;
			// Err = libData->MTensor_setInteger(positions, tmp, l+1);
			// tmp[1] = 2;
			tmp[1] = 1;
			Err = libData->MTensor_setInteger(positions, tmp, Sj[pl]+1);
		}

	// Err = sparseFuncs->MSparseArray_fromExplicitPositions(positions, value, Dims, Imp, &r);

	if (!Err)
    MArgument_setMTensor(Res, positions);

    return Err;
}

DLLEXPORT int kernel( WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) 
{
	int Err = LIBRARY_NO_ERROR;

	MTensor Pos = 0;
	MTensor Val = 0;
	MTensor Dims = 0;

	MTensor value = 0;
	MTensor result_kernel = 0;
	mint nonzerovalues;
	mint prime;

	mint tmp[2];
	mint dims[2];
	mint result_pos[1];

	mint x = 0;

    char nnz[6];

    WolframSparseLibrary_Functions sparseFuncs = libData->sparseLibraryFunctions;

	if(Argc!=5) return LIBRARY_FUNCTION_ERROR; 

	Pos = MArgument_getMTensor(Args[0]);
	Val = MArgument_getMTensor(Args[1]);
	Dims = MArgument_getMTensor(Args[2]);
	nonzerovalues = MArgument_getInteger(Args[3]);
	prime = MArgument_getInteger(Args[4]);

	mint *vals;

	mint *pos;

	mint *dimensions;

		vals = libData->MTensor_getIntegerData(Val);
		
		pos = libData->MTensor_getIntegerData(Pos);

		dimensions = libData->MTensor_getIntegerData(Dims);

		int i = dimensions[0];
		int j = dimensions[1];

		/* allocate result */
		spasm_triplet *Test;
		Test = spasm_triplet_alloc(i, j, 1, prime, prime != -1);

		for(int m = 0; m< nonzerovalues; m++){
			int g = pos[2*m];
			int h = pos[2*m+1];
			int o = vals[m];
			spasm_add_entry(Test, g-1, h-1, o);
		}

		spasm_triplet_transpose(Test);
		spasm *Mtest;
		Mtest = spasm_compress(Test);
		spasm_triplet_free(Test);

		int n = Mtest->n;
		int m = Mtest->m;

		spasm * Mtest_t = spasm_transpose(Mtest, SPASM_WITH_NUMERICAL_VALUES);
		spasm * Mtest_K = spasm_kernel(Mtest_t, SPASM_IDENTITY_PERMUTATION);
		spasm_csr_free(Mtest_t);

		int k = Mtest_K->n;
		int * Mtest_Kp = Mtest_K->p;
		int * Mtest_Kj = Mtest_K->j;
		spasm_GFp * Mtest_Kx = Mtest_K->x;

		

		assert(Mtest_K->m == Mtest->n);

		spasm_GFp * x_1 = spasm_malloc(n * sizeof(spasm_GFp));

		int sample_cnt = 0;
		

		dims[0] = n;

		Err = libData->MTensor_new(MType_Integer, 1, dims, &value);
		
		mint result_dims[2] = {n, k};

		Err = libData->MTensor_new(MType_Integer, 2, result_dims, &result_kernel);

		mint check_k = k;

		mint real_x_1 = 0;
		mint checked_real_x_1 = 0;
		mint lowerbound = 0;
		mint conservativebound = 1;
		mint lower_item = prime;
		mint interval = (prime - 1) / 2;
		mint check_indices = 0;
		mint mediate = 0;
		mint comp_1 = 0;
		mint comp_2 = 0;

		double real_bound = sqrt(interval);

		result_pos[0]=1;

		spasm_vector_zero(x_1, n);

		for(int dummy_count_2 = 0; dummy_count_2 < k; dummy_count_2++){	
				spasm_vector_zero(x_1, n);	
				for (int p = Mtest_Kp[dummy_count_2]; p < Mtest_Kp[dummy_count_2 + 1]; p++) {
					x_1[ Mtest_Kj[p] ] = Mtest_Kx[p];
				}	
				tmp[1] = dummy_count_2 + 1;
				for(int dummy_count = 0; dummy_count < n; dummy_count++){
					tmp[0] = dummy_count + 1;
					/* real_x_1 = (x_1[dummy_count] < (prime + 1) / 2) ? x_1[dummy_count] : -x_1[dummy_count] + prime; */
					/* if(real_x_1 % 42013 == 0 \\ ) real_x_1 = 0; */
					/* real_x_1 = x_1[dummy_count] % 42013;*/
					/* real_x_1 = (real_x_1 > prime / 2) ? real_x_1 - prime : real_x_1; */
						
					real_x_1 = (x_1[dummy_count] > prime / 2) ? x_1[dummy_count] - prime : x_1[dummy_count];
					/*real_x_1 = real_x_1 % 42013;*/
					checked_real_x_1 = -real_x_1;
					Err = libData -> MTensor_setInteger(result_kernel, tmp, checked_real_x_1); 
					/* Err = libData -> MTensor_setInteger(result_kernel, tmp, x_1[dummy_count]); */
				}
		}

		if(!Err) MArgument_setMTensor(Res, result_kernel);

    return Err;
}

DLLEXPORT int linearbackwardsolve( WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) 
{
	int Err = LIBRARY_NO_ERROR;

	MTensor Pos = 0;
	MTensor Val = 0;
	MTensor Dims = 0;
	MTensor Solv = 0;

	MTensor solution = 0;
	mint nonzerovalues;
	mint prime;
    MSparseArray A;

    mint x = 0;

	mint tmp[2];
	mint dims[2];
	
    char nnz[6];

    WolframSparseLibrary_Functions sparseFuncs = libData->sparseLibraryFunctions;

	if(Argc!=6) return LIBRARY_FUNCTION_ERROR; 

	Pos = MArgument_getMTensor(Args[0]);
	Val = MArgument_getMTensor(Args[1]);
	Dims = MArgument_getMTensor(Args[2]);
	nonzerovalues = MArgument_getInteger(Args[3]);
	Solv = MArgument_getMTensor(Args[4]);
	prime = MArgument_getInteger(Args[5]);

	mint *vals;

	mint *pos;

	mint *dimensions;

	mint *solv;

		vals = libData->MTensor_getIntegerData(Val);
		
		pos = libData->MTensor_getIntegerData(Pos);

		dimensions = libData->MTensor_getIntegerData(Dims);

		solv = libData->MTensor_getIntegerData(Solv);

		int i = dimensions[0];
		int j = dimensions[1];

		// load the LHS matrix of the equation
		spasm_triplet *Test;
		spasm_triplet *B;
		Test = spasm_triplet_alloc(i, j, 0, prime, prime != -1);

		for(int m = 0; m < nonzerovalues; m++){
			int g = pos[2*m];
			int h = pos[2*m+1];
			int o = vals[m];
			spasm_add_entry(Test, g-1, h-1, o);
		}

		spasm_triplet_transpose(Test);
		spasm *Mtest;
		spasm *Btest;
		Mtest = spasm_compress(Test);
		spasm_triplet_free(Test);
		
		// load RHS of the equation
		B = spasm_triplet_alloc(1, j, 1, prime, prime != -1);
		for (int checki = 0; checki < j; checki++)
		{
			int checkb = solv[checki];
			int makecheck = checki + 1;
			spasm_add_entry(B, makecheck, 1, checkb);
		}
		Btest = spasm_compress(B);
		spasm_triplet_free(B);

		int *Btest_j = Btest->j;
		int *Btest_p = Btest->p;

		// solve the linear system Ax=B
		int *xi, *solx;
		xi = malloc(j * sizeof(int));
		spasm_vector_zero(xi, j);

		for (int checki_1 = 0; checki_1 < j; checki_1++){
			xi[checki_1] = 1;
		}

		solx = malloc(j * sizeof(spasm_GFp));
		spasm_vector_zero(solx, j);

		// spasm_reach()

		spasm_sparse_forward_solve(Mtest, Btest, 0, xi, solx, SPASM_IDENTITY_PERMUTATION);

		dims[0] = j;
		dims[1] = 1;
		Err = libData->MTensor_new(MType_Integer, 1, dims, &solution);
		
		int check = 0;
		int real_value = 0;
	
		// for(int l = 0; l < j; l++){
		// 	tmp[0] = l+1;
		// 	tmp[1] = 1;

		// 	check = (solx[l] > prime / 2) ? solx[l] - prime : solx[l];
		// 	real_value = -check;

		// 	Err = libData->MTensor_setInteger(solution, tmp, real_value);
		// 	}

		for(int l = 0; l < j; l++){
			tmp[0] = l+1;
			tmp[1] = 1;

			// check = (solx[l] > prime / 2) ? solx[l] - prime : solx[l];
			// real_value = -check;

			// Err = libData->MTensor_setInteger(solution, tmp, real_value);
			check = solx[l]%42013;
			 Err = libData->MTensor_setInteger(solution, tmp, solx[l]);
			}
		

	// if(!Err) MArgument_setInteger(Res, Btest_j[0]);
if(!Err) MArgument_setMTensor(Res, solution);
    return Err;
}
