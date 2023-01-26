AC_INIT([spasm],[1.2],[charles.bouillaguet@lip6.fr])
AC_PREREQ([2.69])
#AC_CANONICAL_HOST
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([m4])
LT_INIT

AM_INIT_AUTOMAKE([1.8 check-news no-dependencies foreign silent-rules subdir-objects -Wall -Wportability])
AC_CONFIG_HEADERS([src/config.h])
AM_MAINTAINER_MODE

AC_LANG([C])

# user-definable options

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX
AC_PROG_CC_C99
AC_PROG_MAKE_SET
AC_PROG_AWK     # for TAP
AC_PROG_LIBTOOL
AC_PROG_INSTALL
AM_PROG_CC_C_O

AC_REQUIRE_AUX_FILE([tap-driver.sh])

# Checks for header files.
#AC_HEADER_STDC

# Checks for typedefs, structures, and compiler characteristics.
#AC_FUNC_MALLOC

DEFAULT_CHECKING_PATH="/usr /usr/local /opt/local"
SPASM_CHECK_OMP
SPASM_CHECK_METIS
SPASM_CHECK_GIVARO
SPASM_CHECK_FFLAS_FFPACK
SPASM_CHECK_LINBOX
SPASM_CHECK_LEMON

CFLAGS="$CFLAGS $OMPFLAGS -Wall -Wextra -fPIC"
CXXFLAGS="$CXXFLAGS $OMPFLAGS $FFLAS_FFPACK_CXXFLAGS $GIVARO_CXXFLAGS -Wall -Wextra -fPIC"
LDFLAGS="$LDFLAGS $FFLAS_FFPACK_LIBS $GIVARO_LIBS"

AC_SUBST(CFLAGS)
AC_SUBST(CXXFLAGS)
AC_SUBST(LDFLAGS)

AC_CONFIG_FILES([Makefile src/Makefile test/Makefile bench/Makefile])
AC_OUTPUT
