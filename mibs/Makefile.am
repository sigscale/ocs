## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

mibs_DATA = SIGSCALE-OCS-MIB.mib \
		RADIUS-AUTH-SERVER-MIB.mib \
		RADIUS-ACC-SERVER-MIB.mib \
		SIGSCALE-DIAMETER-BASE-PROTOCOL-MIB.mib \
		SIGSCALE-DIAMETER-CC-APPLICATION-MIB.mib

check_DATA = .tree_ok

.tree_ok:
	@if snmptranslate -V > /dev/null 2>&1; then \
		snmptranslate -M +$(ERLANG_LIB_DIR_sigscale_mibs)/mibs:$(top_srcdir)/mibs \
			-m +SIGSCALE-OCS-MIB -Lo -Tp -IR ocsMIB; \
		snmptranslate -M +$(top_srcdir)/mibs -m +RADIUS-ACC-SERVER-MIB \
			-Lo -Tp -IR radiusAccServMIB; \
		snmptranslate -M +$(ERLANG_LIB_DIR_sigscale_mibs)/mibs:$(top_srcdir)/mibs \
			-m +SIGSCALE-DIAMETER-BASE-PROTOCOL-MIB \
			-Lo -Tp -IR diameterBaseProtocolMIB; \
		snmptranslate -M +$(ERLANG_LIB_DIR_sigscale_mibs)/mibs:$(top_srcdir)/mibs \
			-m +SIGSCALE-DIAMETER-CC-APPLICATION-MIB \
			-Lo -Tp -IR diameterCCAMIB; \
	fi

