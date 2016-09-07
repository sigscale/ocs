## Makefile.am
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = ocs.app
EXTRA_DIST = ocs.app

beam_DATA = ocs.beam \
			ocs_app.beam \
			ocs_sup.beam \
			ocs_radius_auth_sup.beam \
			ocs_radius_acct_sup.beam \
			ocs_radius_acct_server_sup.beam \
			ocs_radius_auth_server_sup.beam \
			ocs_eap_sup.beam \
			ocs_eap_fsm_sup.beam \
			ocs_eap_fsm.beam \
			ocs_eap_server_sup.beam \
			ocs_eap_server.beam \
			ocs_eap_codec.beam \
			ocs_radius_accounting.beam \
			ocs_radius_authentication.beam \
			ocs_server.beam \
			ocs_eap_pwd.beam \
			ocs_wm_config.beam

%.beam:$(top_srcdir)/src/%.erl	$(top_srcdir)/include/ocs.hrl $(top_srcdir)/include/ocs_eap_codec.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_srcdir)/src $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' \
			-e 's#%APP_PRIV%#$(ERLANG_INSTALL_LIB_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/priv/#' < $< > $@ 

CLEANFILES = $(beam_DATA) $(app_DATA)

