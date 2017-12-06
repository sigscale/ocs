## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

modhtml = ocs.html \
			ocs_app.html \
			ocs_sup.html \
			ocs_diameter_auth_port_server.html \
			ocs_diameter_auth_port_sup.html \
			ocs_diameter_auth_service_fsm_sup.html \
			ocs_diameter_acct_service_fsm_sup.html \
			ocs_diameter_auth_service_fsm.html \
			ocs_diameter_acct_service_fsm.html \
			ocs_diameter_auth_sup.html \
			ocs_diameter_acct_port_server.html \
			diameter_gen_3gpp_ro_application.html \
			diameter_gen_cc_application_rfc4006.html \
			diameter_gen_nas_application_rfc7155.html \
			diameter_gen_eap_application_rfc4072.html \
			ocs_diameter_base_application_cb.html \
			ocs_diameter_3gpp_ro_application_cb.html \
			ocs_diameter_cc_application_cb.html \
			ocs_diameter_nas_application_cb.html \
			ocs_diameter_eap_application_cb.html \
			ocs_diameter_disconnect_fsm_sup.html\
			ocs_diameter_disconnect_fsm.html\
			ocs_radius_acct_port_server.html \
			ocs_diameter_acct_port_sup.html \
			ocs_radius_acct_port_sup.html \
			ocs_radius_acct_top_sup.html \
			ocs_diameter_acct_top_sup.html \
			ocs_radius_acct_server_sup.html \
			ocs_radius_auth_server_sup.html \
			ocs_simple_auth_fsm_sup.html \
			ocs_radius_auth_sup.html \
			ocs_eap_pwd_fsm_sup.html \
			ocs_eap_ttls_fsm_sup_sup.html \
			ocs_eap_ttls_fsm_sup.html \
			ocs_radius_auth_port_sup.html \
			ocs_radius_disconnect_fsm_sup.html\
			ocs_radius_disconnect_fsm.html\
			ocs_eap_pwd_fsm.html \
			ocs_eap_ttls_fsm.html \
			ocs_eap_ttls_aaah_fsm.html \
			ocs_simple_auth_fsm.html \
			ocs_radius_auth_port_server.html \
			ocs_eap_codec.html \
			ocs_radius_accounting.html \
			ocs_radius_authentication.html \
			ocs_server.html \
			ocs_log.html \
			ocs_gtt.html \
			ocs_log_rotate_server.html \
			ocs_eap_pwd.html \
			ocs_eap_tls_transport.html \
			ocs_rating.html \
			ocs_rest.html \
			ocs_rest_res_subscriber.html \
			ocs_rest_res_client.html \
			ocs_rest_res_usage.html \
			ocs_rest_res_user.html \
			ocs_rest_res_access.html \
			ocs_rest_res_accounting.html \
			ocs_rest_res_balance.html \
			ocs_rest_res_product.html \
			ocs_rest_res_http.html \
			ocs_rest_pagination_sup.html \
			ocs_rest_pagination_server.html \
			ocs_scheduler.html \
			mod_ocs_rest_lang.html \
			mod_ocs_rest_accepted_content.html \
			mod_ocs_rest_delete.html \
			mod_ocs_rest_get.html \
			mod_ocs_rest_patch.html \
			mod_ocs_rest_post.html \
			mod_ocs_rest_put.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html \
		index.html stylesheet.css \
		sigscale-logo.png supervision.png

overview.edoc: overview.edoc-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%DESCRIPTION%!$(PACKAGE)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc $(top_srcdir)/doc/supervision.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src"]},{includes,["$(ERLANG_LIB_DIR_radius)"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{stylesheet_file, "$(top_srcdir)/doc/sigscale.css"}]'
	for i in `ls *.html`; do \
			sed -e 's!erlang logo!sigscale logo!' \
					-e 's!href=\"http://www.erlang.org/\"!href=\"http://www.sigscale.org/\"!' \
					-e 's!erlang.png!sigscale-logo.png!' $$i > $$i.tmp; \
			mv $$i.tmp $$i; \
	done
	-cp -n $(top_srcdir)/doc/sigscale-logo.png .
	-cp -n $(top_srcdir)/doc/supervision.png .

CLEANFILES = overview.edoc edoc-info index.html \
		modules-frame.html overview-summary.html packages-frame.html \
		stylesheet.css erlang.png $(modhtml)

