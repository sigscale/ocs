## Makefile.am
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = ocs.app
EXTRA_DIST = ocs.app

beam_DATA = ocs.beam \
			ocs_app.beam \
			ocs_sup.beam \
			ocs_radius_sup.beam \
			ocs_server.beam \
			ocs_authentication_sup.beam \
			ocs_accounting_sup.beam

%.beam:$(top_srcdir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_srcdir)/src $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' < $< > $@

CLEANFILES = $(beam_DATA) $(app_DATA)

