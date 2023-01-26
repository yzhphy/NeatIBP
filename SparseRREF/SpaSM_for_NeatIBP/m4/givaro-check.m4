dnl Check for Givaro
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

AC_DEFUN([SPASM_CHECK_GIVARO],
[

AC_ARG_WITH(givaro,
[AC_HELP_STRING([--with-givaro=<path>|yes], [Use the Givaro library. If argument is yes or <empty>,
    that means the library is reachable with the standard
    search path (/usr or /usr/local). Otherwise you give
    the <path> to the directory which contains the
    library.
])],
    [if test "$withval" = yes ; then
        GIVARO_HOME_PATH="${DEFAULT_CHECKING_PATH}"
     elif test "$withval" != no ; then
        GIVARO_HOME_PATH="$withval ${DEFAULT_CHECKING_PATH}"
     fi],
    [GIVARO_HOME_PATH=""])


dnl Check for existence
BACKUP_CXXFLAGS=${CXXFLAGS}
BACKUP_LIBS=${LIBS}

AC_MSG_CHECKING(for Givaro)
givaro_found="no"

for GIVARO_HOME in ${GIVARO_HOME_PATH}
  do
    if test -r "$GIVARO_HOME/bin/givaro-config"; then
           givaro_found="yes"
           GIVARO_CXXFLAGS=`$GIVARO_HOME/bin/givaro-config --cflags`
           GIVARO_LIBS=`$GIVARO_HOME/bin/givaro-config --libs`
	   break
    elif test -r "$GIVARO_HOME/givaro-config"; then
           givaro_found="yes"
           GIVARO_CXXFLAGS=`$GIVARO_HOME/givaro-config --cflags`
           GIVARO_LIBS=`$GIVARO_HOME/givaro-config --libs`
	   break
    else
       givaro_found="no (givaro-config not found)"
    fi
done

AC_MSG_RESULT($givaro_found)

if test "x$givaro_found" = "xyes" ; then
    AC_SUBST(GIVARO_CXXFLAGS)
    AC_SUBST(GIVARO_LIBS)
    AC_DEFINE(HAVE_GIVARO, 1, [Define if Givaro is installed])
    HAVE_GIVARO=yes
fi

AM_CONDITIONAL(SPASM_HAVE_GIVARO, test "x$HAVE_GIVARO" = "xyes")

CXXFLAGS=${BACKUP_CXXFLAGS}
LIBS=${BACKUP_LIBS}

])
