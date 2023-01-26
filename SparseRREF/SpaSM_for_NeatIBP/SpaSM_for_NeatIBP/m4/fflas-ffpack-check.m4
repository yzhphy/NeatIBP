dnl Check for Linbox
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

AC_DEFUN([SPASM_CHECK_FFLAS_FFPACK],
[

AC_ARG_WITH(fflas-ffpack,
[AC_HELP_STRING([--with-fflas-ffpack=<path>|yes], [Use the FFLAS-FFPACK library. If argument is yes or <empty>,
    that means the library is reachable with the standard
    search path (/usr or /usr/local). Otherwise you give
    the <path> to the directory which contains the
    library.
])],
    [if test "$withval" = yes ; then
        FFLAS_FFPACK_HOME_PATH="${DEFAULT_CHECKING_PATH}"
     elif test "$withval" != no ; then
        FFLAS_FFPACK_HOME_PATH="$withval ${DEFAULT_CHECKING_PATH}"
     fi],
    [FFLAS_FFPACK_HOME_PATH=""])


dnl Check for existence
BACKUP_CXXFLAGS=${CXXFLAGS}
BACKUP_LIBS=${LIBS}

AC_MSG_CHECKING(for FFLAS-FFPACK)
ffpack_found="no"

for FFLAS_FFPACK_HOME in ${FFLAS_FFPACK_HOME_PATH}
  do
    if test -r "$FFLAS_FFPACK_HOME/bin/fflas-ffpack-config"; then
           ffpack_found="yes"
           FFLAS_FFPACK_CXXFLAGS=`$FFLAS_FFPACK_HOME/bin/fflas-ffpack-config --cflags-full`
           FFLAS_FFPACK_LIBS=`$FFLAS_FFPACK_HOME/bin/fflas-ffpack-config --libs`
	   break
    elif test -r "$FFLAS_FFPACK_HOME/fflas-ffpack-config"; then
           ffpack_found="yes"
           FFLAS_FFPACK_CXXFLAGS=`$FFLAS_FFPACK_HOME/fflas-ffpack-config --cflags-full`
           FFLAS_FFPACK_LIBS=`$FFLAS_FFPACK_HOME/fflas-ffpack-config --libs`
	   break
    else
       ffpack_found="no (fflas-ffpack-config not found)"
    fi
done

AC_MSG_RESULT($ffpack_found)

if test "x$ffpack_found" = "xyes" ; then
    AC_SUBST(FFLAS_FFPACK_CXXFLAGS)
    AC_SUBST(FFLAS_FFPACK_LIBS)
    AC_DEFINE(HAVE_FFLAS_FFPACK, 1, [Define if FFLAS-FFPACK is installed])
    HAVE_FFLAS_FFPACK=yes
fi

AM_CONDITIONAL(SPASM_HAVE_FFLAS_FFPACK, test "x$HAVE_FFLAS_FFPACK" = "xyes")

CXXFLAGS=${BACKUP_CXXFLAGS}
LIBS=${BACKUP_LIBS}

])
