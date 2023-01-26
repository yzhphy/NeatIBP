dnl Check for Metis
dnl Charles Bouillaguet 27-03-2015
dnl Boyer Brice 07/04/11
dnl Bradford Hovinen, 2001-06-13
dnl Modified by Pascal Giorgi, 2003-12-03
dnl Inspired by gnome-bonobo-check.m4 by Miguel de Icaza, 99-04-12
dnl Stolen from Chris Lahey       99-2-5
dnl stolen from Manish Singh again
dnl stolen back from Frank Belew
dnl stolen from Manish Singh
dnl Shamelessly stolen from Owen Taylor

AC_DEFUN([SPASM_CHECK_METIS],
[

AC_ARG_WITH(metis,
[AC_HELP_STRING([--with-metis=<path>|yes], [Use the METIS library. If argument is yes or <empty>,
    that means the library is reachable with the standard
    search path (/usr or /usr/local). Otherwise you give
    the <path> to the directory which contains the
    library.
])],
    [if test "$withval" = yes ; then
        METIS_HOME_PATH="${DEFAULT_CHECKING_PATH}"
        elif test "$withval" != no ; then
        METIS_HOME_PATH="$withval ${DEFAULT_CHECKING_PATH}"
        fi],
    [METIS_HOME_PATH="${DEFAULT_CHECKING_PATH}"])


dnl Check for existence
BACKUP_CFLAGS=${CFLAGS}
BACKUP_LIBS=${LIBS}

AC_MSG_CHECKING(for METIS)

for METIS_HOME in ${METIS_HOME_PATH}
  do
#    AC_MSG_NOTICE($METIS_HOME)
    if test -r "$METIS_HOME/include/metis.h"; then

       if test "x$METIS_HOME" != "x/usr"; then
           METIS_CFLAGS="-I${METIS_HOME}/include"
           METIS_LIBS="-L${METIS_HOME}/lib -lmetis"
       else
           METIS_CFLAGS=
           METIS_LIBS="-lmetis"
       fi

#       AC_MSG_NOTICE(found include)


       CFLAGS="${BACKUP_CFLAGS} ${METIS_CFLAGS}"
       LIBS="${BACKUP_LIBS} ${METIS_LIBS}"

       AC_LINK_IFELSE(
       [AC_LANG_PROGRAM([[#include <metis.h>
                          #include <stdlib.h>]],
        [[ METIS_PartMeshDual(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL); ]]
	)],
	[
	  metis_found="yes"
	  break
	],
	[
	   metis_found="no (metis.h found but linking failed)"
	   unset METIS_CFLAGS
	   unset METIS_LIBS
	])
    else
       metis_found="no (metis.h not found)"
    fi
done

AC_MSG_RESULT($metis_found)

if test "x$metis_found" = "xyes" ; then
    AC_SUBST(METIS_CFLAGS)
    AC_SUBST(METIS_LIBS)
    AC_DEFINE(HAVE_METIS,1,[Define if METIS is installed])
    HAVE_METIS=yes
fi

AM_CONDITIONAL(SPASM_HAVE_METIS, test "x$HAVE_METIS" = "xyes")

CFLAGS=${BACKUP_CFLAGS}
LIBS=${BACKUP_LIBS}

])
