dnl Check for Lemon (Library for Efficient Modeling and Optimization in Networks)
dnl Charles Bouillaguet 06-03-2021
dnl Boyer Brice 07/04/11
dnl Bradford Hovinen, 2001-06-13
dnl Modified by Pascal Giorgi, 2003-12-03
dnl Inspired by gnome-bonobo-check.m4 by Miguel de Icaza, 99-04-12
dnl Stolen from Chris Lahey       99-2-5
dnl stolen from Manish Singh again
dnl stolen back from Frank Belew
dnl stolen from Manish Singh
dnl Shamelessly stolen from Owen Taylor

AC_DEFUN([SPASM_CHECK_LEMON],
[
AC_LANG_PUSH([C++])

AC_ARG_WITH(LEMON,
[AC_HELP_STRING([--with-lemon=<path>|yes], [Use the Lemon library. If argument is yes or <empty>,
    that means the library is reachable with the standard
    search path (/usr or /usr/local). Otherwise you give
    the <path> to the directory which contains the
    library.
])],
    [if test "$withval" = yes ; then
        LEMON_HOME_PATH="${DEFAULT_CHECKING_PATH}"
        elif test "$withval" != no ; then
        LEMON_HOME_PATH="$withval ${DEFAULT_CHECKING_PATH}"
        fi],
    [LEMON_HOME_PATH="${DEFAULT_CHECKING_PATH}"])


dnl Check for existence
BACKUP_CFLAGS=${CFLAGS}
BACKUP_LIBS=${LIBS}

AC_MSG_CHECKING(for lemon)

for LEMON_HOME in ${LEMON_HOME_PATH}
do
        #AC_MSG_NOTICE($LEMON_HOME)
        if test -r "$LEMON_HOME/include/lemon/matching.h"; then
                if test "x$LEMON_HOME" != "x/usr"; then
                    LEMON_CFLAGS="-I${LEMON_HOME}/include"
                    LEMON_LIBS="-L${LEMON_HOME}/lib -llemon"
                else
                    LEMON_CFLAGS=
                    LEMON_LIBS="-llemon"
                fi
                # AC_MSG_NOTICE(found include)
                CFLAGS="${BACKUP_CFLAGS} ${LEMON_CFLAGS}"
                LIBS="${BACKUP_LIBS} ${LEMON_LIBS}"
                # try linking
                AC_LINK_IFELSE(
                        [AC_LANG_PROGRAM(
                                [[#include <lemon/smart_graph.h>
                                  using namespace lemon;]],
                                [[SmartGraph G; G.reserveNode(0); G.reserveEdge(0);]]
                        )],
                        [
                                LEMON_found="yes"
                                break
                        ],
                        [
                                LEMON_found="no (lemon/matching.h found but linking failed)"
                                unset LEMON_CFLAGS
                                unset LEMON_LIBS
                                break
                        ]
                )
        else
                LEMON_found="no (lemon/matching.h not found)"
        fi
done

AC_MSG_RESULT($LEMON_found)

if test "x$LEMON_found" = "xyes" ; then
    AC_SUBST(LEMON_CFLAGS)
    AC_SUBST(LEMON_LIBS)
    AC_DEFINE(HAVE_LEMON,1,[Define if LEMON is installed])
    HAVE_LEMON=yes
fi

AM_CONDITIONAL(SPASM_HAVE_LEMON, test "x$HAVE_LEMON" = "xyes")

CFLAGS=${BACKUP_CFLAGS}
LIBS=${BACKUP_LIBS}

AC_LANG_POP([C++])
])
