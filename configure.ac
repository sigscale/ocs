## configure.ac
## vim: ts=3
##
## Process this file with autoconf to produce a configure script.
##
## Boilerplate:  standard boilerplate code
##
AC_INIT([ocs], [3.4.41], [issues@sigscale.org])
AC_COPYRIGHT([Copyright (c) 2016 - 2025 SigScale Global Inc.])
AC_PREREQ([2.63])
AC_CONFIG_SRCDIR([src/ocs_app.erl])
AC_CONFIG_HEADERS([config.h])
LT_INIT([disable-static dlopen])
AM_INIT_AUTOMAKE([-Wno-portability])

##
## Options:  control options to configure, support code for the options
##
AC_ARG_ENABLE([debug],
		[AS_HELP_STRING([--enable-debug], [enable debugging (default is no)])])
AC_ARG_WITH([ssl],
		[AS_HELP_STRING([--with-ssl=PATH], [specify location of OpenSSL include and lib])])
CFLAGS="${CFLAGS} -Wall"
PKG_PROG_PKG_CONFIG
AC_ARG_WITH([systemdsystemunitdir],
		[AS_HELP_STRING([--with-systemdsystemunitdir=DIR], [Directory for systemd service files])],,
		[with_systemdsystemunitdir=auto])
AS_IF([test "x$with_systemdsystemunitdir" = "xyes" -o "x$with_systemdsystemunitdir" = "xauto"],
		[def_systemdsystemunitdir=$($PKG_CONFIG --variable=systemdsystemunitdir systemd)
		AS_IF([test "x$def_systemdsystemunitdir" = "x"],
				[AS_IF([test "x$with_systemdsystemunitdir" = "xyes"],
						[AC_MSG_ERROR([systemd support requested but pkg-config unable to query systemd package])],
						[with_systemdsystemunitdir=no])],
				[with_systemdsystemunitdir="$def_systemdsystemunitdir"])])
AS_IF([test "x$with_systemdsystemunitdir" != "xno"],
		[AC_SUBST([systemdsystemunitdir], [$with_systemdsystemunitdir])])
AM_CONDITIONAL([HAVE_SYSTEMD], [test "x$with_systemdsystemunitdir" != "xno"])


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
if test "x$with_ssl" != xyes ; then
	CPPFLAGS="${CPPFLAGS} -I$with_ssl/include"
	LDFLAGS="${LDFLAGS} -L$with_ssl/lib"
fi
AC_ERLANG_SUBST_LIB_DIR
if test -z "${ERLANG_INSTALL_LIB_DIR}" ; then
	ERLANG_INSTALL_LIB_DIR=$ERLANG_LIB_DIR
fi
AC_ERLANG_SUBST_ROOT_DIR
CPPFLAGS="${CPPFLAGS} -I ${ERLANG_ROOT_DIR}/usr/include"
AC_PROG_MKDIR_P
AC_PROG_INSTALL
AC_PROG_LN_S
AC_ERLANG_SUBST_INSTALL_LIB_DIR
otpdir=$ERLANG_INSTALL_LIB_DIR/..
appdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION
erldir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/src
beamdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/ebin
emibdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/mibs
mibsdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/mibs
eincludedir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/include
docdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/doc
libdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/lib
wwwdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/www
examplesdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/examples
schemadir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/schema
scriptdir=$ERLANG_INSTALL_LIB_DIR/$PACKAGE_NAME-$PACKAGE_VERSION/priv/bin
reldir=$ERLANG_INSTALL_LIB_DIR/../releases
envdir=/etc/default
AC_SUBST(otpdir)
AC_SUBST(appdir)
AC_SUBST(erldir)
AC_SUBST(beamdir)
AC_SUBST(emibdir)
AC_SUBST(mibsdir)
AC_SUBST(eincludedir)
AC_SUBST(privdir)
AC_SUBST(wwwdir)
AC_SUBST(examplesdir)
AC_SUBST(schemadir)
AC_SUBST(scriptdir)
AC_SUBST(reldir)
AC_SUBST(envdir)
AC_LANG_POP([Erlang])


##
## Libraries:  check for libraries
##
AC_ERLANG_SUBST_ERTS_VER
AC_CHECK_LIB([crypto], [EC_GROUP_new], [],
		[AC_MSG_ERROR([openssl crypto was not found!])])
AC_LANG_PUSH([Erlang])
AC_ERLANG_CHECK_LIB([kernel], [], [AC_MSG_ERROR([kernel was not found!])])
AC_ERLANG_CHECK_LIB([stdlib], [], [AC_MSG_ERROR([stdlib was not found!])])
AC_ERLANG_CHECK_LIB([sasl], [], [AC_MSG_ERROR([sasl was not found!])])
AM_CONDITIONAL([MAKE_TAR_EXTRA_FILES],
		[AS_VERSION_COMPARE([$ERLANG_LIB_VER_sasl], ["4.0"], [false], [true], [true])])
AC_ERLANG_CHECK_LIB([mnesia], [], [AC_MSG_ERROR([mnesia was not found!])])
AC_ERLANG_CHECK_LIB([crypto], [], [AC_MSG_ERROR([crypto was not found!])])
AC_ERLANG_CHECK_LIB([asn1], [], [AC_MSG_ERROR([asn1 was not found!])])
AC_ERLANG_CHECK_LIB([public_key], [], [AC_MSG_ERROR([public_key was not found!])])
AC_ERLANG_CHECK_LIB([ssl], [], [AC_MSG_ERROR([ssl was not found!])])
AC_ERLANG_CHECK_LIB([inets], [], [AC_MSG_ERROR([inets was not found!])])
AC_ERLANG_CHECK_LIB([xmerl], [], [AC_MSG_ERROR([xmerl was not found!])])
AC_ERLANG_CHECK_LIB([compiler], [], [AC_MSG_ERROR([compiler was not found!])])
AC_ERLANG_CHECK_LIB([parsetools], [], [AC_MSG_ERROR([parsetools was not found!])])
AC_ERLANG_CHECK_LIB([syntax_tools], [], [AC_MSG_ERROR([syntax_tools was not found!])])
AC_ERLANG_CHECK_LIB([os_mon], [], [AC_MSG_ERROR([os_mon was not found!])])
AC_ERLANG_CHECK_LIB([snmp], [], [AC_MSG_ERROR([snmp was not found!])])
AC_ERLANG_CHECK_LIB([sigscale_mibs], [], [AC_MSG_ERROR([sigscale_mibs was not found!])])
AC_ERLANG_CHECK_LIB([mochiweb], [], [AC_MSG_ERROR([mochiweb was not found!])])
AC_ERLANG_CHECK_LIB([radius], [], [AC_MSG_ERROR([radius was not found!])])
AC_ERLANG_CHECK_LIB([diameter], [], [AC_MSG_ERROR([diameter was not found!])])
AC_LANG_POP([Erlang])


##
## Headers:  check for header files
##
AM_CPPFLAGS="$AM_CPPFLAGS -I ${ERLANG_ROOT_DIR}/usr/include"
AC_CHECK_HEADERS([openssl/bn.h], [], [AC_MSG_ERROR([openssl not found!])])
AC_CHECK_HEADERS([openssl/ec.h], [], [AC_MSG_ERROR([openssl not found!])])
AC_CHECK_HEADERS([erl_nif.h], [], [AC_MSG_ERROR([Erlang NIFs not found!])])


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

AC_OUTPUT([Makefile include/Makefile src/Makefile ebin/Makefile doc/Makefile
		c_src/Makefile priv/Makefile priv/lib/Makefile
		priv/mibs/Makefile mibs/Makefile priv/www/Makefile priv/examples/Makefile
		priv/www/package.json
		priv/schema/Makefile test/Makefile
		sys.config $PACKAGE_NAME.rel ebin/$PACKAGE_NAME.appup])

