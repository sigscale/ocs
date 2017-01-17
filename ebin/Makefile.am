## Makefile.am
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = ocs.app
EXTRA_DIST = ocs.app

beam_DATA = ocs.beam \
			ocs_app.beam \
			ocs_sup.beam \
			ocs_radius_acct_port_server.beam \
			ocs_radius_acct_port_sup.beam \
			ocs_radius_acct_top_sup.beam \
			ocs_radius_acct_server_sup.beam \
			ocs_radius_auth_server_sup.beam \
			ocs_radius_auth_sup.beam \
			ocs_eap_pwd_fsm_sup.beam \
			ocs_eap_ttls_fsm_sup_sup.beam \
			ocs_eap_ttls_fsm_sup.beam \
			ocs_radius_auth_port_sup.beam \
			ocs_simple_auth_fsm_sup.beam \
			ocs_radius_disconnect_fsm_sup.beam\
			ocs_radius_disconnect_fsm.beam\
			ocs_eap_pwd_fsm.beam \
			ocs_eap_ttls_fsm.beam \
			ocs_eap_ttls_aaah_fsm.beam \
			ocs_simple_auth_fsm.beam \
			ocs_radius_auth_port_server.beam \
			ocs_eap_codec.beam \
			ocs_radius_accounting.beam \
			ocs_radius_authentication.beam \
			ocs_server.beam \
			ocs_eap_pwd.beam \
			ocs_eap_tls_transport.beam \
			mod_ocs_rest_dispatcher.beam \
			mod_ocs_rest_accepted_content.beam \
			mod_ocs_rest_get.beam \
			mod_ocs_rest_post.beam \
			mod_ocs_rest_patch.beam \
			mod_ocs_rest_delete.beam \
			ocs_rest_res_subscriber.beam \
			ocs_rest_res_client.beam


%.beam:$(top_srcdir)/src/%.erl	$(top_srcdir)/include/ocs.hrl $(top_srcdir)/include/ocs_eap_codec.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_srcdir)/src $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' \
			-e 's#%APP_PRIV%#$(ERLANG_INSTALL_LIB_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/priv#' < $< > $@ 

CLEANFILES = $(beam_DATA) $(app_DATA)

