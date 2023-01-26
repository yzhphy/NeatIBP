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

AC_DEFUN([SPASM_CHECK_LINBOX],
[

AC_ARG_WITH(linbox,
[AC_HELP_STRING([--with-linbox=<path>|yes], [Use the LINBOX library. If argument is yes or <empty>,
    that means the library is reachable with the standard
    search path (/usr or /usr/local). Otherwise you give
    the <path> to the directory which contains the
    library.
])],
    [if test "$withval" = yes ; then
        LINBOX_HOME_PATH="${DEFAULT_CHECKING_PATH}"
     elif test "$withval" != no ; then
        LINBOX_HOME_PATH="$withval ${DEFAULT_CHECKING_PATH}"
     fi],
    [LINBOX_HOME_PATH=""])


dnl Check for existence
BACKUP_CXXFLAGS=${CXXFLAGS}
BACKUP_LIBS=${LIBS}

AC_MSG_CHECKING(for LINBOX)
linbox_found="no"

for LINBOX_HOME in ${LINBOX_HOME_PATH}
  do
    if test -r "$LINBOX_HOME/bin/linbox-config"; then
           linbox_found="yes"
           LINBOX_CXXFLAGS=`$LINBOX_HOME/bin/linbox-config --cflags-full`
           LINBOX_LIBS=`$LINBOX_HOME/bin/linbox-config --libs`
	   break
    elif test -r "$LINBOX_HOME/linbox-config"; then
           linbox_found="yes"
           LINBOX_CXXFLAGS=`$LINBOX_HOME/linbox-config --cflags-full`
           LINBOX_LIBS=`$LINBOX_HOME/linbox-config --libs`
	   break
    else
       linbox_found="no (linbox-config not found)"
    fi
done

AC_MSG_RESULT($linbox_found)

if test "x$linbox_found" = "xyes" ; then
    AC_SUBST(LINBOX_CXXFLAGS)
    AC_SUBST(LINBOX_LIBS)
    AC_DEFINE(HAVE_LINBOX, 1, [Define if LINBOX is installed])
    HAVE_LINBOX=yes
fi

AM_CONDITIONAL(SPASM_HAVE_LINBOX, test "x$HAVE_LINBOX" = "xyes")

CXXFLAGS=${BACKUP_CXXFLAGS}
LIBS=${BACKUP_LIBS}

])
