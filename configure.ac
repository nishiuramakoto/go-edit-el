dnl Process this file with autoconf to produce a configure script.

AC_INIT(src/gen-main.el)
dnl Every other copy of the package version number gets its value from here
AC_CONFIG_AUX_DIR(config)
AM_INIT_AUTOMAKE(myelisp, 0.1.0)


AC_SUBST(VERSION)
ISODATE=`date +%Y-%m-%d`
AC_SUBST(ISODATE)
AC_SUBST(ac_aux_dir)
AC_CANONICAL_HOST

dnl Checks for programs.
AC_PROG_INSTALL

dnl Checks for libraries.
AM_PATH_LISPDIR

dnl Checks for typedefs, structures, and compiler characteristics.

dnl Checks for library functions.

AC_OUTPUT(Makefile src/Makefile myelisp.lsm myelisp.spec)
