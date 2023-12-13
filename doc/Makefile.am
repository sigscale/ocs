## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

modhtml = ocs.html \
			ocs_app.html \
			ocs_sup.html \
			ocs_diameter.html \
			ocs_diameter_auth_port_server.html \
			ocs_diameter_auth_port_sup.html \
			ocs_diameter_auth_service_fsm_sup.html \
			ocs_diameter_acct_service_fsm_sup.html \
			ocs_diameter_auth_service_fsm.html \
			ocs_diameter_acct_service_fsm.html \
			ocs_diameter_auth_sup.html \
			diameter_gen_3gpp.html \
			diameter_gen_ietf.html \
			diameter_gen_etsi.html \
			diameter_gen_3gpp_ro_application.html \
			diameter_gen_3gpp_sta_application.html \
			diameter_gen_3gpp_swm_application.html \
			diameter_gen_3gpp_swx_application.html \
			diameter_gen_3gpp_s6a_application.html \
			diameter_gen_3gpp_s6b_application.html \
			diameter_gen_3gpp_gx_application.html \
			diameter_gen_cc_application_rfc4006.html \
			diameter_gen_nas_application_rfc7155.html \
			diameter_gen_eap_application_rfc4072.html \
			ocs_diameter_base_application_cb.html \
			ocs_diameter_3gpp_ro_application_cb.html \
			ocs_diameter_3gpp_ro_nrf_app_cb.html \
			ocs_diameter_3gpp_sta_application_cb.html \
			ocs_diameter_3gpp_swm_application_cb.html \
			ocs_diameter_3gpp_swx_application_cb.html \
			ocs_diameter_3gpp_s6a_application_cb.html \
			ocs_diameter_3gpp_s6b_application_cb.html \
			ocs_diameter_3gpp_gx_application_cb.html \
			ocs_diameter_cc_application_cb.html \
			ocs_diameter_nas_application_cb.html \
			ocs_diameter_eap_application_cb.html \
			ocs_diameter_codec_swm.html \
			ocs_diameter_codec_sta.html \
			ocs_diameter_codec_s6b.html \
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
			ocs_eap_aka_fsm_sup_sup.html \
			ocs_eap_aka_fsm_sup.html \
			ocs_eap_aka_fsm.html \
			ocs_eap_akap_fsm_sup_sup.html \
			ocs_eap_akap_fsm_sup.html \
			ocs_eap_akap_fsm.html \
			ocs_eap_aka_auc_fsm.html \
			ocs_pgw_fsm_sup.html \
			ocs_pgw_fsm.html \
			ocs_terminate_fsm_sup.html \
			ocs_terminate_fsm.html \
			ocs_deregister_fsm_sup.html \
			ocs_deregister_fsm.html \
			ocs_radius_auth_port_sup.html \
			ocs_radius_disconnect_fsm_sup.html\
			ocs_radius_disconnect_fsm.html\
			ocs_eap_pwd_fsm.html \
			ocs_eap_ttls_fsm.html \
			ocs_eap_ttls_aaah_fsm.html \
			ocs_simple_auth_fsm.html \
			ocs_radius_auth_port_server.html \
			ocs_radius_accounting.html \
			ocs_radius_authentication.html \
			ocs_server.html \
			ocs_log.html \
			ocs_gtt.html \
			ocs_log_rotate_server.html \
			ocs_eap_codec.html \
			ocs_eap_pwd.html \
			ocs_eap_aka.html \
			ocs_eap_tls_transport.html \
			ocs_milenage.html \
			ocs_rating.html \
			ocs_rest.html \
			ocs_rest_res_client.html \
			ocs_rest_res_usage.html \
			ocs_rest_res_user.html \
			ocs_rest_res_balance.html \
			ocs_rest_res_product.html \
			ocs_rest_res_service.html \
			ocs_rest_res_resource.html \
			ocs_rest_res_nrf.html \
			ocs_rest_res_health.html \
			ocs_rest_res_http.html \
			ocs_rest_res_prometheus.html \
			ocs_rest_pagination_sup.html \
			ocs_rest_pagination_server.html \
			ocs_scheduler.html \
			mod_ocs_rest_accepted_content.html \
			mod_ocs_rest_delete.html \
			mod_ocs_rest_get.html \
			mod_ocs_rest_head.html \
			mod_ocs_rest_patch.html \
			mod_ocs_rest_post.html \
			mod_oauth.html \
			ocs_rest_query_scanner.html \
			ocs_rest_query_parser.html \
			ocs_mib.html \
			ocs_rest_hub_sup.html \
			ocs_rest_hub_fsm.html \
			ocs_rest_res_hub_balance.html \
			ocs_rest_res_hub_product.html \
			ocs_rest_res_hub_service.html \
			ocs_rest_res_hub_user.html \
			ocs_rest_res_hub_resource.html \
			ocs_rest_res_hub_usage.html \
			ocs_rest_res_role.html \
			ocs_rest_res_hub_role.html \
			ocs_statistics_sup.html \
			ocs_statistics_server.html \
			ocs_event.html \
			user_default.html \
			ocs_event_log.html \
			ocs_event_log_sup.html \
			ocs_event_log_fsm.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html \
		overview-summary.html \
		index.html \
		stylesheet.css \
		sigscale-logo.png \
		oas-logo.svg \
		swaggerhub-logo.svg \
		supervision.svg \
		bss-integration.svg \
		bss-rating-function.svg \
		interfaces.svg \
		non-3gpp-trusted.svg \
		non-3gpp-untrusted.svg \
		schema.svg \
		product-specifications.svg \
		disk_logs.svg \
		event_log.svg \
		pub_sub_events.svg \
		pagination-msc.svg \
		ctf-ro-msc.svg \
		pcef-gx-msc.svg \
		swm-swx-msc.svg

overview.edoc: overview.edoc-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%DESCRIPTION%!$(PACKAGE)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc $(top_srcdir)/doc/supervision.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{preprocess,true},{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src"]},{includes,["$(top_srcdir)/include","$(top_builddir)/include","$(ERLANG_LIB_DIR_radius)/include"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{stylesheet_file, "$(top_srcdir)/doc/sigscale.css"}]'
	for i in `ls *.html`; do \
			sed -e 's!erlang logo!sigscale logo!' \
					-e 's!href=\"http://www.erlang.org/\"!href=\"http://www.sigscale.org/\"!' \
					-e 's!erlang.png!sigscale-logo.png!' $$i > $$i.tmp; \
			mv $$i.tmp $$i; \
	done
	-cp -n $(top_srcdir)/doc/sigscale-logo.png .
	-cp -n $(top_srcdir)/doc/oas-logo.svg .
	-cp -n $(top_srcdir)/doc/swaggerhub-logo.svg .
	-cp -n $(top_srcdir)/doc/supervision.svg .
	-cp -n $(top_srcdir)/doc/bss-integration.svg .
	-cp -n $(top_srcdir)/doc/bss-rating-function.svg .
	-cp -n $(top_srcdir)/doc/interfaces.svg .
	-cp -n $(top_srcdir)/doc/non-3gpp-trusted.svg .
	-cp -n $(top_srcdir)/doc/non-3gpp-untrusted.svg .
	-cp -n $(top_srcdir)/doc/schema.svg .
	-cp -n $(top_srcdir)/doc/product-specifications.svg .
	-cp -n $(top_srcdir)/doc/disk_logs.svg .
	-cp -n $(top_srcdir)/doc/event_log.svg .
	-cp -n $(top_srcdir)/doc/pub_sub_events.svg .
	-cp -n $(top_srcdir)/doc/pagination-msc.svg .
	-cp -n $(top_srcdir)/doc/ctf-ro-msc.svg .
	-cp -n $(top_srcdir)/doc/pcef-gx-msc.svg .
	-cp -n $(top_srcdir)/doc/swm-swx-msc.svg .

CLEANFILES = overview.edoc \
		edoc-info \
		index.html \
		modules-frame.html \
		overview-summary.html \
		packages-frame.html \
		stylesheet.css \
		erlang.png \
		$(modhtml) \
		supervision.svg \
		bss-integration.svg \
		bss-rating-function.svg \
		interfaces.svg \
		non-3gpp-trusted.svg \
		non-3gpp-untrusted.svg \
		oas-logo.svg \
		swaggerhub-logo.svg \
		disk_logs.svg \
		event_log.svg \
		pub_sub_events.svg \
		pagination-msc.svg \
		ctf-ro-msc.svg \
		pcef-gx-msc.svg \
		swm-swx-msc.svg \
		schema.svg \
		product-specifications.svg \
		sigscale-logo.png

