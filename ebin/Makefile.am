## Makefile.am
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = ocs.app
EXTRA_DIST = ocs.app

beam_DATA = ocs.beam \
			ocs_app.beam \
			ocs_sup.beam \
			ocs_diameter_auth_port_server.beam \
			ocs_diameter_auth_port_sup.beam \
			ocs_diameter_auth_service_fsm_sup.beam \
			ocs_diameter_acct_service_fsm_sup.beam \
			ocs_diameter_auth_service_fsm.beam \
			ocs_diameter_acct_service_fsm.beam \
			ocs_diameter_auth_sup.beam \
			ocs_diameter_base_application_cb.beam \
			ocs_diameter_nas_application_cb.beam \
			ocs_diameter_eap_application_cb.beam \
			ocs_diameter_cc_application_cb.beam \
			ocs_diameter_disconnect_fsm_sup.beam\
			ocs_diameter_disconnect_fsm.beam\
			ocs_radius_acct_port_server.beam \
			ocs_diameter_acct_port_server.beam \
			ocs_radius_acct_port_sup.beam \
			ocs_diameter_acct_port_sup.beam \
			ocs_diameter_acct_top_sup.beam \
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
			ocs_log.beam \
			ocs_log_rotate_server.beam \
			ocs_eap_pwd.beam \
			ocs_eap_tls_transport.beam \
			mod_ocs_rest_lang.beam \
			mod_ocs_rest_accepted_content.beam \
			mod_ocs_rest_get.beam \
			mod_ocs_rest_post.beam \
			mod_ocs_rest_put.beam \
			mod_ocs_rest_patch.beam \
			mod_ocs_rest_delete.beam \
			ocs_rest.beam \
			ocs_rest_res_subscriber.beam \
			ocs_rest_res_client.beam \
			ocs_rest_res_usage.beam \
			ocs_rest_res_user.beam \
			ocs_rest_res_balance.beam \
			diameter_gen_nas_application_rfc7155.beam \
			diameter_gen_cc_application_rfc4006.beam \
			diameter_gen_eap_application_rfc4072.beam \
			ocs_rest_res_access.beam \
			ocs_rest_res_accounting.beam \
			ocs_rest_res_http.beam \
			ocs_rest_page_sup.beam \
			ocs_rest_page_server.beam

%.beam:$(top_srcdir)/src/%.erl	$(top_srcdir)/include/ocs.hrl \
		$(top_srcdir)/include/ocs_log.hrl \
		$(top_srcdir)/include/ocs_eap_codec.hrl \
		$(top_builddir)/include/diameter_gen_nas_application_rfc7155.hrl \
		$(top_builddir)/include/diameter_gen_cc_application_rfc4006.hrl \
		$(top_builddir)/include/diameter_gen_eap_application_rfc4072.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

diameter_gen_nas_application_rfc7155.beam:$(top_builddir)/src/diameter_gen_nas_application_rfc7155.erl \
		$(top_builddir)/include/diameter_gen_nas_application_rfc7155.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/include $<

diameter_gen_cc_application_rfc4006.beam:$(top_builddir)/src/diameter_gen_cc_application_rfc4006.erl \
		$(top_builddir)/include/diameter_gen_nas_application_rfc7155.hrl \
		$(top_builddir)/include/diameter_gen_cc_application_rfc4006.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

diameter_gen_eap_application_rfc4072.beam:$(top_builddir)/src/diameter_gen_eap_application_rfc4072.erl \
		$(top_builddir)/include/diameter_gen_nas_application_rfc7155.hrl \
		$(top_builddir)/include/diameter_gen_eap_application_rfc4072.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' \
			-e 's#%APP_PRIV%#$(ERLANG_INSTALL_LIB_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/priv#' < $< > $@ 

CLEANFILES = $(beam_DATA) $(app_DATA)

