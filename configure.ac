## configure.ac
##
## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT([ocs], [0.1], [issues@sigscale.org])
AC_COPYRIGHT([Copyright (c) 2016 SigScale Global Inc.])
AC_PREREQ(2.63)
AC_CONFIG_SRCDIR([src/ocs_app.erl])
AC_CONFIG_HEADERS([config.h])
LT_INIT([disable-static dlopen])
AM_INIT_AUTOMAKE([-Wno-portability])

##
## Options:  control options to configure, support code for the options
##
AC_ARG_ENABLE([debug],
		[AS_HELP_STRING([--enable-debug], [enable debugging (default is no)])])
CFLAGS="${CFLAGS} -Wall"

##
## Programs:  check for programs needed by the configure process,
##            the build process, or by one of the programs being built
##
AC_LANG_PUSH([Erlang])
AC_ERLANG_NEED_ERL
AC_ERLANG_NEED_ERLC
if test -z "${ERLCFLAGS}" ; then
	ERLCFLAGS="-W -v"
fi
if test "x$enable_debug" == xyes ; then
	ERLCFLAGS="$ERLCFLAGS +debug_info"
fi
AC_ERLANG_SUBST_LIB_DIR
if test -z "${ERLANG_INSTALL_LIB_DIR}" ; then
	ERLANG_INSTALL_LIB_DIR=$ERLANG_LIB_DIR
fi
AC_ERLANG_SUBST_ROOT_DIR
AC_PROG_INSTALL
AC_ERLANG_SUBST_INSTALL_LIB_DIR
appdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION
beamdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/ebin
eincludedir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/include
docdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/doc
libdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/lib
AC_SUBST(appdir)
AC_SUBST(beamdir)
AC_SUBST(eincludedir)
AC_SUBST(privdir)
AC_LANG_POP([Erlang])


##
## Libraries:  check for libraries
##
AC_CHECK_LIB([crypto], [EC_GROUP_new], [],
		[AC_MSG_ERROR([openssl crypto was not found!])])
AC_LANG_PUSH([Erlang])
AC_ERLANG_CHECK_LIB([crypto], [], [AC_MSG_ERROR([crypto was not found!])])
AC_ERLANG_CHECK_LIB([radius], [], [AC_MSG_ERROR([radius was not found!])])
AC_LANG_POP([Erlang])


##
## Headers:  check for header files
##
AM_CPPFLAGS="$AM_CPPFLAGS -I ${ERLANG_ROOT_DIR}/usr/include"
AC_CHECK_HEADERS([openssl/bn.h])
AC_CHECK_HEADERS([openssl/ec.h])
AC_CHECK_HEADERS([erl_nif.h])


##
## Typedefs & Structures:  check for typedefs, structures,
##                         and compiler characteristics.
##


##
## Functions:  check for library functions
##
AC_CHECK_FUNC([EC_GROUP_new], [], [AC_MSG_ERROR([EC_GROUP_new function not found!])])
AC_CHECK_FUNC([EC_POINT_new], [], [AC_MSG_ERROR([EC_POINT_new function not found!])])
AC_CHECK_FUNC([BN_new], [], [AC_MSG_ERROR([BN_new function not found!])])

##
## Output:  create output files
## 
AC_OUTPUT([Makefile include/Makefile ebin/Makefile doc/Makefile
		c_src/Makefile priv/Makefile priv/lib/Makefile test/Makefile])

