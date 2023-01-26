#include <assert.h>
#include <math.h>
#include <err.h>

#include "spasm.h"
#include "mmio.h"


/*
 * load a matrix in SMS format from f (an opened file). 
 * set prime == -1 to avoid loading values.
 */
spasm_triplet *spasm_load_sms(FILE * f, int prime) {
	int i, j;
	spasm_GFp x;
	char type;
	
	assert(f != NULL);

	double start = spasm_wtime();
	if (fscanf(f, "%d %d %c\n", &i, &j, &type) != 3)
		errx(1, "[spasm_load_sms] bad SMS file (header)\n");

	if (prime != -1 && type != 'M')
		errx(1, "[spasm_load_sms] only ``Modular'' type supported\n");

	fprintf(stderr, "[IO] loading %d x %d SMS matrix modulo %d... ", i, j, prime);
	fflush(stderr);

	/* allocate result */
	spasm_triplet *T = spasm_triplet_alloc(i, j, 1, prime, prime != -1);

	while (fscanf(f, "%d %d %d\n", &i, &j, &x) == 3) {
		if (i == 0 && j == 0 && x == 0)
			break;
		spasm_add_entry(T, i - 1, j - 1, x);
	}

	char nnz[16];
	spasm_human_format(T->nz, nnz);
	fprintf(stderr, "%s NNZ [%.1fs]\n", nnz, spasm_wtime() - start);
	return T;
}


/*
 * load a matrix in CSR binary format from f (an opened file). 
 * set prime == -1 to avoid loading values.
 */
spasm *spasm_load_bin(FILE * f, int prime) {
	int n, m;

	assert(prime == -1); /* values are not handled yet */
	double start = spasm_wtime();

	/* get rows */
	if (fread(&n, sizeof(int), 1, f) != 1)
		err(1, "error reading n");
	/* get columns */
	if (fread(&m, sizeof(int), 1, f) != 1)
		err(1, "error reading m");

	fprintf(stderr, "[IO] loading %d x %d binary matrix... ", n, m);
	fflush(stderr);

	spasm *M = spasm_csr_alloc(n, m, n+m, prime, SPASM_IGNORE_VALUES);
	int * const Mp = M->p;
	
	/* read the row pointers */
	size_t r = n + 1;
	if (fread(Mp, sizeof(int), n+1, f) != r)
		err(1, "error while reading the row lengths (read %zu items)", r);
	
	int nnz = Mp[n];
	spasm_csr_realloc(M, nnz);
	int * const Mj = M->j;

	/* read columns indices */
	r = nnz;
	if (fread(&Mj[0], sizeof(int), nnz, f) != r)
		err(1, "error while reading the compressed column ids (read %zu items)", r);

	char nnz_s[16];
	spasm_human_format(nnz, nnz_s);
	fprintf(stderr, "%s NNZ [%.1fs]\n", nnz_s, spasm_wtime() - start);
	return M;
}

/*
 * Load a matrix in MatrixMarket sparse format.
 * Heavily inspired by the example program:
 *     http://math.nist.gov/MatrixMarket/mmio/c/example_read.c
 */
spasm_triplet *spasm_load_mm(FILE * f, int prime) {
	MM_typecode matcode;
	int n, m, nnz;

	double start = spasm_wtime();
	if (mm_read_banner(f, &matcode) != 0) 
		errx(1, "Could not process Matrix Market banner.\n");

	if (!mm_is_matrix(matcode) || !mm_is_sparse(matcode))
		errx(1, "Matrix Market type: [%s] not supported", mm_typecode_to_str(matcode));
	
	int symmetric = mm_is_symmetric(matcode);
	int skew = mm_is_skew(matcode);

	if (!mm_is_general(matcode) && !symmetric && !skew)
		errx(1, "Matrix market type [%s] not supported",  mm_typecode_to_str(matcode));

	if (mm_read_mtx_crd_size(f, &n, &m, &nnz) != 0)
		errx(1, "Cannot read matrix size");

	fprintf(stderr, "[IO] loading %d x %d MTX [%s] modulo %d, %d nnz...", n, m, mm_typecode_to_str(matcode), prime, nnz);
	fflush(stderr);
	
	if (mm_is_pattern(matcode))
		prime = -1;

	spasm_triplet *T = spasm_triplet_alloc(n, m, nnz, prime, prime != -1);

	for (int i = 0; i < nnz; i++) {
		int u, v, w;
		double x, y;

		if (mm_is_pattern(matcode)) {
			if (2 != fscanf(f, "%d %d\n", &u, &v))
				errx(1, "parse error entry %d\n", i);
			spasm_add_entry(T, u - 1, v - 1, 1);
		} else if (mm_is_integer(matcode)) {
			if (3 != fscanf(f, "%d %d %d\n", &u, &v, &w))
				errx(1, "parse error entry %d\n", i);
			spasm_add_entry(T, u - 1, v - 1, w);
		} else if (mm_is_real(matcode)) {
			if (3 != fscanf(f, "%d %d %lg\n", &u, &v, &x))
				errx(1, "parse error entry %d\n", i);
			spasm_add_entry(T, u - 1, v - 1, (int) (100000 * x));
		} else if (mm_is_complex(matcode)) {
			if (4 != fscanf(f, "%d %d %lg %lg\n", &u, &v, &y, &x))
				errx(1, "parse error entry %d\n", i);
			spasm_add_entry(T, u - 1, v - 1, (int) (1000 * (y + 100 * x)));
		} else {
			errx(1, "Don't know how to read matrix");
		}
	}

	if (symmetric || skew)
		nnz *= 2;

	if (symmetric) {
		int mult = skew ? -1 : 1;
		int nz = T->nz;
		for (int px = 0; px < nz; px++)
			if (T->j[px] != T->i[px])
				spasm_add_entry(T, T->j[px], T->i[px], (T->x != NULL) ? (mult * T->x[px]) : 1);
	}

	char s_nnz[16];
	spasm_human_format(T->nz, s_nnz);
	fprintf(stderr, "%s NNZ [%.1fs]\n", s_nnz, spasm_wtime() - start);
	return T;
}

/* load a matrix in old GBLA format */
spasm *spasm_load_gbla_old(FILE * f, int with_values) {
	int m, n, p;
	long long int nnz;
	spasm *M;

	/* get rows */
	if (fread(&n, sizeof(uint32_t), 1, f) != 1)
		err(1, "error reading m");

	/* get columns */
	if (fread(&m, sizeof(uint32_t), 1, f) != 1)
		err(1, "error reading n");

	if ((fread(&p, sizeof(uint32_t), 1, f) != 1))
		err(1, "error reading p");

	/* get number of nonzero elements */
	if (fread(&nnz, sizeof(uint64_t), 1, f) != 1)
		err(1, "error reading nnz");

	if (nnz >> 31ull)
		errx(2, "more than 2^31 NNZ. You are going to hit limitations of spasm... Sorry\n");

	M = spasm_csr_alloc(n, m, nnz, p, with_values);
	int * const Mp = M->p;
	int * const Mj = M->j;
	spasm_GFp * const Mx = M->x;

	/*
	 * dirty hack : we load the 16-bit coefficient in the area dedicated
	 * to storing the column indices (unused at this point). This avoids
	 * allocating extra memory
	 */
	uint16_t *buffer = (uint16_t *) M->j;
	size_t r = nnz;
	if (fread(buffer, sizeof(uint16_t), nnz, f) != r)
		err(1, "error while reading the coefficients");
	if (with_values)
		for (int i = 0; i < nnz; i++)
			Mx[i] = buffer[i];
	/* read the column indices */
	r = nnz;
	if (fread(Mj, sizeof(uint32_t), nnz, f) != r)
		err(1, "error while reading the column indices");
	/* read the row lengths */
	r = n;
	if (fread(&Mp[1], sizeof(uint32_t), n, f) != r)
		err(1, "error while reading the row lengths (read %zu items)", r);
	/* sum-prefix */
	Mp[0] = 0;
	for (int i = 1; i <= n; i++)
		Mp[i] += Mp[i - 1];
	return M;
}


#define VERMASK (1U<<31)

/*
 * load a matrix in new GBLA format. Only the pattern is loaded, not the
 * values.
 */
spasm *spasm_load_gbla_new(FILE * f) {
	unsigned int i, j, m, n;
	uint64_t nnz;
	uint16_t p;
	spasm *M;
	uint32_t b;

	/* get header */
	if (fread(&b, sizeof(uint32_t), 1, f) != 1)
		err(1, "error reading b");
	if ((b & VERMASK) != VERMASK)
		errx(1, "wrong format version");
	b = b ^ VERMASK;
	if (((b >> 1) & 3) != 1)
		errx(1, "field elements are not on 16 bits");
	/* get rows */
	if (fread(&n, sizeof(uint32_t), 1, f) != 1)
		err(1, "error reading m");
	/* get columns */
	if (fread(&m, sizeof(uint32_t), 1, f) != 1)
		err(1, "error reading n");
	if ((fread(&p, sizeof(uint16_t), 1, f) != 1))
		err(1, "error reading p");
	/* get number of nonzero elements */
	if (fread(&nnz, sizeof(uint64_t), 1, f) != 1)
		err(1, "error reading nnz");
	if (nnz >> 31ull)
		errx(2, "more than 2^31 NNZ. You are going to hit limitations of spasm... Sorry\n");
	M = spasm_csr_alloc(n, m, nnz, (int)p, SPASM_IGNORE_VALUES);
	int * const Mp = M->p;
	int * const Mj = M->j;

	/* read the row lengths */
	size_t r = n;
	if (fread(&Mp[1], sizeof(uint32_t), n, f) != r)
		err(1, "error while reading the row lengths (read %zu items)", r);
	/* sum-prefix */
	Mp[0] = 0;
	for (unsigned int i = 1; i <= n; i++)
		Mp[i] += Mp[i - 1];

	/*
	 * read (and ignore) the polmaps. Dirty hack: we send them to the
	 * area for the column indices
	 */
	r = n;
	if (fread(Mj, sizeof(uint32_t), n, f) != r)
		err(1, "error while reading the polmaps (read %zu items)", r);

	uint64_t k;
	/* get number of compressed columns element */
	if (fread(&k, sizeof(uint64_t), 1, f) != 1)
		err(1, "error reading k");

	/* read compressed columns indices */
	uint32_t *buffer = spasm_malloc(k * sizeof(uint32_t));
	r = k;
	if (fread(buffer, sizeof(uint32_t), k, f) != r)
		err(1, "error while reading the compressed column ids (read %zu items)", r);

	/* uncompress columns indices */
	i = 0;
	j = 0;
	const uint32_t MASK = 0x80000000;
	while (i < k) {
		uint32_t col = buffer[i++];

		if (col & MASK) {	/* single column */
			Mj[j++] = col ^ MASK;
		} else {
			uint32_t x = buffer[i++];
			for (p = 0; p < x; p++) {
				Mj[j++] = col + p;
			}
		}
	}
	assert(i == k);
	assert(j == nnz);
	free(buffer);

	return M;
}


/*
 * save a matrix in SMS format. TODO : change name to spasm_csr_save
 */
void spasm_save_csr(FILE * f, const spasm * A) {
	assert(f != NULL);

	int *Aj = A->j;
	int *Ap = A->p;
	spasm_GFp *Ax = A->x;
	int n = A->n;
	int m = A->m;
	int prime = A->prime;

	fprintf(f, "%d %d M\n", n, m);
	for (int i = 0; i < n; i++)
		for (int px = Ap[i]; px < Ap[i + 1]; px++) {
			spasm_GFp x = (Ax != NULL) ? Ax[px] : 1;
			x = (x > prime / 2) ? x - prime : x;
			fprintf(f, "%d %d %d\n", i + 1, Aj[px] + 1, x);
		}
	fprintf(f, "0 0 0\n");
}

/*
 * save a matrix in SMS format. TODO : change name to spasm_triplet_save
 */
void spasm_save_triplet(FILE * f, const spasm_triplet * A) {
	assert(f != NULL);

	int *Ai = A->i;
	int *Aj = A->j;
	spasm_GFp *Ax = A->x;
	int nz = A->nz;

	fprintf(f, "%d %d M\n", A->n, A->m);
	for (int px = 0; px < nz; px++)
		fprintf(f, "%d %d %d\n", Ai[px] + 1, Aj[px] + 1, (Ax != NULL) ? Ax[px] : 1);
	fprintf(f, "0 0 0\n");
}

/* Saves a PBM (bitmap) of specified dimensions of A.
 * Mode: 1 --> create a PBM file (B/W bitmap)
 *       2 --> create a PGM file (gray levels) 
 *       3 --> create a PNM file (colors)
 */
void spasm_save_pnm(const spasm * A, FILE * f, int x, int y, int mode, spasm_dm *DM) {
	int *Aj = A->j;
	int *Ap = A->p;
	int n = A->n;
	int m = A->m;
	x = spasm_min(x, m);
	y = spasm_min(y, n);
	int *w = spasm_malloc(x * y * sizeof(int));
	
	assert(f != NULL);
	assert((mode-1)*(mode-2)*(mode-3) == 0);
	assert((mode != 3) || (DM != NULL));

	spasm_vector_zero(w, x * y);

	fprintf(f, "P%d\n", mode);
	fprintf(f, "%d %d\n", x, y);
	if (mode > 1)
		fprintf(f, "255\n");


	for (int i = 0; i < n; i++) {
		int k = ((long long int) i) * ((long long int) y) / ((long long int) n);
		int *ww = w + k * x;
		for (int px = Ap[i]; px < Ap[i + 1]; px++) {
			int t = ((long long int) Aj[px]) * ((long long int) x) / ((long long int) m);
			ww[t]++;
		}
	}

	double max = 0;
	for (int j = 0; j < x * y; j++)
		max = spasm_max(max, w[j]);

	int bgcolor[3][3] = {
		{0xFF0000, 0xCC0000, 0x990000},
		{0xFFFFFF, 0xFFCC00, 0xCC9900},    /* 0xFFFF66 inside SCC */
	    {0xFFFFFF, 0xFFFFFF, 0x33CC00}
	};

	int limits_h[2];
	int limits_v[2];
	int *r, *c;

	if (DM != NULL) {
		int *rr = DM->rr;
		int *cc = DM->cc;
		limits_h[0] = ((long long int) cc[2]) * ((long long int) x) / ((long long int) m);
		limits_h[1] = ((long long int) cc[3]) * ((long long int) x) / ((long long int) m);
		limits_v[0] = ((long long int) rr[1]) * ((long long int) y) / ((long long int) n);
		limits_v[1] = ((long long int) rr[2]) * ((long long int) y) / ((long long int) n);
		fprintf(stderr, "limits_v = %d, %d\n", limits_v[0], limits_v[1]);
		r = DM->r;
		c = DM->c;
	}

	int t = 0;
	double intensity;
	int scc = 0;
	int scc_left_edge = 0;
	int scc_right_edge = 0;
	int scc_bot_edge = 0;
	for (int i = 0; i < y; i++) {
		for (int j = 0; j < x; j++) {
			switch(mode) {
			case 1:
				fprintf(f, "%d ", (w[i * x + j] > 0) ? 1 : 0);
				break;
			case 2: 
				intensity = 1.0 - exp(0.1 * log(w[i * x + j] / max));
				/* intensity = 1.0 - ((double) w[i * x + j]) / max; */
				assert(0 <= intensity && intensity <= 1.0);
				fprintf(f, "%.0f ", 255.0 * intensity);
				break;
			case 3:
				/* find out which blocks we are in */
				;
				int block_h = 0;
				int block_v = 0;
				if (limits_v[0] <= i)
					block_v = (i < limits_v[1]) ? 1 : 2;
				if (limits_h[0] <= j)
					block_h = (j < limits_h[1]) ? 1 : 2;

				int bg = bgcolor[block_v][block_h];

				if (block_h == 1 && block_v == 1) {
					/* inside S, we need to take care of SCC's */
					while(scc_bot_edge <= i) {
						scc_left_edge = scc_right_edge;
						++scc;
						scc_right_edge = ((long long int) c[scc]) * ((long long int) x) / ((long long int) m);
						scc_bot_edge = ((long long int) r[scc]) * ((long long int) y) / ((long long int) n);
					}

					if (j < scc_left_edge)
						bg = 0xFFFFFF;
					else if (j < scc_right_edge)
						bg += 0x003366;
				}

				int pixel = (w[i * x + j] > 0) ? 0 : bg;
				fprintf(f, "%d %d %d ", (pixel >> 16) & 0xFF, (pixel >> 8) & 0xFF, pixel & 0xFF);
			}
			
			if (((t++) & 31) == 0)
				fprintf(f, "\n");
		}
	}

	free(w);
}
