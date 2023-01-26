#include <assert.h>
#include <stdio.h>
#include <getopt.h>
#include <err.h>
#include <math.h>
#include "spasm.h"

/** computes a Dulmage-Mendelson decomposition */

int main(int argc, char **argv) {
    int ch;
    double mpix;

    /* options descriptor */
    struct option longopts[5] = {
        {"permuted", no_argument, NULL, 'p'},
        {"verbose", no_argument, NULL, 'v'},
        {"tabulated", no_argument, NULL, 't'},
        {"image", required_argument, NULL, 'i'},
        {NULL, 0, NULL, 0}
    };

  
    char mode = 'p';
    while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
        switch (ch) {
        case 'i':
            mpix = atof(optarg);
        case 'p':
        case 'v':
        case 't':
            mode = ch;
            break;
        default:
            errx(1, "Unknown option");
        }
    }

    spasm_triplet *T = spasm_load_sms(stdin, 42013);
    spasm * A = spasm_compress(T);
    spasm_triplet_free(T);

    int n = A->n;
    int m = A->m;

    spasm_dm *DM = spasm_dulmage_mendelsohn(A);
    int *rr = DM->rr;
    int *cc = DM->cc;

    int * qinv = spasm_pinv(DM->q, m);
    spasm * B = spasm_permute(A, DM->p, qinv, SPASM_WITH_NUMERICAL_VALUES);
    free(qinv);
    spasm_csr_free(A);

    switch(mode) {
    case 't':
        /* printf("%5d \t %5d \t %6d \t %6d \t %.1f \t %6d \t %.1f\n", n, m, spasm_nnz(A), i, 100.0 * i / spasm_min(n, m), j, 1.0 * i / j); */
        break;

    case 'v':
        printf("structural rank = %d\n", rr[2] + cc[4] - cc[3]);
        int h_n = rr[1] - rr[0];
        int h_m = cc[2] - cc[0];
        if (h_n > 0 && h_m > 0)
            printf("*) H (%d x %d)\n", h_n, h_m);
        
        int s_n = rr[2] - rr[1];
        int s_m = cc[3] - cc[2];
        if (s_n > 0 && s_m > 0) {
            printf("*) S (%d x %d) : \n", s_n, s_m);
            int *r = DM->r;
            int n_trivial = 0;
            for(int i = 1; i < DM->nb - 1; i++) {
                int size = r[i+1] - r[i];
                if (size == 1)
                    n_trivial++;
                else
                    printf("    *) SCC of size %d\n", size);
            }
            if (n_trivial > 0)
                printf("    -> plus %d SCC of size 1\n", n_trivial);
        }
            /* Do something with S */
        
        int v_n = rr[4] - rr[2];
        int v_m = cc[4] - cc[3];
        if (v_n > 0 && v_m > 0)
            printf("*) V (%d x %d)\n", v_n, v_m);
        break;
        
    case 'p':
        spasm_save_csr(stdout, B);
        break;

    case 'i':
        ;
        double alpha = sqrt((mpix * 1e6) / (((double) n) * m));
        int w = alpha * m;
        int h = alpha * n;
        spasm_save_pnm(B, stdout, w, h, 3, DM);
        break;
    }    

    spasm_csr_free(B);
    return 0;
}
