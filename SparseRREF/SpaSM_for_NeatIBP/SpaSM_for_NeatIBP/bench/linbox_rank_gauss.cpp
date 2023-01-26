#include "linbox/algorithms/gauss.h"
#include "linbox/solutions/rank.h"
#include "linbox/util/timer.h"

#include <givaro/modular.h>
#include <iostream>
#include <getopt.h>
#include <setjmp.h>
#include <signal.h>
#include <unistd.h>

#define STOR_T SparseMatrixFormat::SparseSeq

using namespace LinBox;

/** Computes the rank using LinBox's Right-looking sparse elimination */

jmp_buf Env;

void alarm_handler(int dummy) {
  longjmp(Env, 1);
}

int main (int argc, char **argv)
{
    typedef Givaro::Modular<uint32_t> Field;
    typedef SparseMatrix<Field, STOR_T> Blackbox;
    int ch;
    int timer = -1;

    /* options descriptor */
    struct option longopts[2] = {
      { "max-time",     required_argument, NULL,          't' },
      { NULL,           0,                 NULL,           0  }
    };

    while ((ch = getopt_long(argc, argv, "", longopts, NULL)) != -1) {
      switch (ch) {
      case 't':
        timer = atoi(optarg);
        break;
      default:
        printf("Unknown option\n");
        exit(1);
      }
    }
    argc -= optind;
    argv += optind;



    /* Params */
    static integer q = 42013U;
    std::string matfile = "data/sms.matrix";

    Field F(q);

    MatrixStream<Field> ms(F, std::cin); /* read matrix on standard input */
    Blackbox A(ms);
    size_t m, n;
    ms.getDimensions(m, n);

    /* Elements retournés */
    unsigned long rank;

    Method::Elimination ME;
    Timer chrono;
    chrono.clear();

    if (timer > 0) {
        signal(SIGALRM, alarm_handler);
        alarm(timer);

        if (setjmp(Env) != 0) {
          fprintf(stderr, "\nTimeout after %d seconds\n", timer);
          exit(2);
        }
    }

    chrono.start();
    LinBox::rank(rank, A, ME);
    chrono.stop();


    /* Output */
    std::cout << chrono << std::endl;
    std::cerr << "Rank:" << rank << std::endl;

    return 0;
}