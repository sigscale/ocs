## Makefile.am
##
## Process this file with automake to produce Makefile.in

modhtml = ocs.html \
			ocs_app.html \
			ocs_sup.html \
			ocs_acct_server.html \
			ocs_radius_acct_sup.html \
			ocs_radius_acct_top_sup.html \
			ocs_radius_acct_server_sup.html \
			ocs_radius_auth_server_sup.html \
			ocs_simple_auth_fsm_sup.html \
			ocs_radius_auth_sup.html \
			ocs_eap_pwd_fsm_sup.html \
			ocs_eap_ttls_fsm_sup.html \
			ocs_radius_auth_port_sup.html \
			ocs_radius_disconnect_fsm_sup.html\
			ocs_radius_disconnect_fsm.html\
			ocs_eap_pwd_fsm.html \
			ocs_eap_ttls_fsm.html \
			ocs_simple_auth_fsm.html \
			ocs_radius_auth_port_server.html \
			ocs_eap_codec.html \
			ocs_radius_accounting.html \
			ocs_radius_authentication.html \
			ocs_server.html \
			ocs_eap_pwd.html \
			ocs_eap_ttls_transport.html \
			ocs_wm_config.html \
			ocs_wm_res_subscriber.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html \
		index.html stylesheet.css \
		sigscale-logo.png supervision.png

overview.edoc: overview.edoc-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%DESCRIPTION%!$(PACKAGE)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_builddir)'" \
			'[{sort_functions,false},{private,true},{todo,true},{source_path,["$(top_srcdir)/src"]},{includes,["$(ERLANG_LIB_DIR_radius)"]},{subpackages,false},{def,{docsrc,"$(top_srcdir)/doc/{@?}"}},{stylesheet_file, "$(top_srcdir)/doc/stylesheet.css"}]'
	for i in `ls *.html`; do \
			sed -i -e 's!erlang logo!sigscale logo!' $$i; \
			sed -i -e 's!href=\"http://www.erlang.org/\"!href=\"http://www.sigscale.org/\"!' $$i; \
			sed -i -e 's!erlang.png!sigscale-logo.png!' $$i; \
			cp $(top_srcdir)/doc/sigscale-logo.png .; \
			cp $(top_srcdir)/doc/supervision.png .; done

CLEANFILES = overview.edoc edoc-info index.html \
		modules-frame.html overview-summary.html packages-frame.html \
		stylesheet.css erlang.png $(modhtml)

INSTALL += -o otpuser -g otp

