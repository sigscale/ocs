## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

CERT_DIR = certificates

testmods = ocs_api_SUITE \
			ocs_rest_api_SUITE \
			ocs_rest_lib_SUITE \
			ocs_codec_diameter_SUITE \
			ocs_codec_eap_SUITE \
			ocs_eap_pwd_SUITE \
			ocs_eap_aka_SUITE \
			ocs_eap_akap_SUITE \
			ocs_eap_ttls_SUITE \
			ocs_milenage_SUITE \
			ocs_simple_auth_SUITE \
			ocs_accounting_SUITE \
			ocs_log_SUITE \
			ocs_rating_SUITE \
			ocs_product_SUITE \
			ocs_charging_SUITE \
			ocs_snmp_SUITE \
			ocs_re_interface_SUITE \
			peer_tls_transport \
			diameter_test_client_cb \
			test_event \
			ocs_test_lib \
			mod_ct_nrf

check_DATA = .dialyzer_ok .common_test_ok

CT_RUN_ARGS = -spec test.spec

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer -Wno_unknown --src -I $(top_builddir)/include $(top_srcdir)/src $(top_builddir)/src

%.beam:$(top_srcdir)/test/%.erl $(top_srcdir)/include/ocs.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/include $<

ocs_accounting_SUITE.beam:$(top_srcdir)/test/ocs_accounting_SUITE.erl \
		$(top_srcdir)/include/ocs.hrl \
		$(top_srcdir)/include/ocs_eap_codec.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/include \
			+"{nowarn_removed, [{crypto,hmac,3}]}" \
			+"{nowarn_deprecated_function, [{crypto,hmac,3}]}" $<

ocs_eap_ttls_SUITE.beam:$(top_srcdir)/test/ocs_eap_ttls_SUITE.erl \
		$(top_srcdir)/include/ocs.hrl \
		$(top_srcdir)/include/ocs_eap_codec.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/include \
			+"{nowarn_removed, [{crypto,hmac,3}]}" \
			+"{nowarn_deprecated_function, [{crypto,hmac,3}]}" $<

ocs_eap_pwd_SUITE.beam:$(top_srcdir)/test/ocs_eap_pwd_SUITE.erl \
		$(top_srcdir)/include/ocs.hrl \
		$(top_srcdir)/include/ocs_eap_codec.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/include \
			+"{nowarn_removed, [{crypto,hmac,3}]}" \
			+"{nowarn_deprecated_function, [{crypto,hmac,3}]}" $<

ocs_eap_aka_SUITE.beam:$(top_srcdir)/test/ocs_eap_aka_SUITE.erl \
		$(top_srcdir)/include/ocs.hrl \
		$(top_srcdir)/include/ocs_eap_codec.hrl \
		$(top_builddir)//include/diameter_gen_eap_application_rfc4072.hrl \
		$(top_builddir)//include/diameter_gen_3gpp_swm_application.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/include \
			+"{nowarn_removed, [{crypto,hmac,3}]}" \
			+"{nowarn_deprecated_function, [{crypto,hmac,3}]}" $<

ocs_eap_akap_SUITE.beam:$(top_srcdir)/test/ocs_eap_akap_SUITE.erl \
		$(top_srcdir)/include/ocs.hrl \
		$(top_srcdir)/include/ocs_eap_codec.hrl \
		$(top_builddir)//include/diameter_gen_eap_application_rfc4072.hrl \
		$(top_builddir)//include/diameter_gen_3gpp_swm_application.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include -I $(top_builddir)/include \
			+"{nowarn_removed, [{crypto,hmac,3}]}" \
			+"{nowarn_deprecated_function, [{crypto,hmac,3}]}" $<

.common_test_ok:	$(addsuffix .beam, $(testmods)) sys.config
	@if [ ! -d $(CERT_DIR) ]; then mkdir $(CERT_DIR); fi
	@if [ ! -d ocs_eap_ttls_SUITE_data ]; then mkdir ocs_eap_ttls_SUITE_data; fi
	@if [ ! -d ocs_rest_api_SUITE_data ]; then mkdir ocs_rest_api_SUITE_data; fi
	@if [ ! -d ocs_re_interface_SUITE_data ]; then mkdir ocs_re_interface_SUITE_data; fi
	@if [ ! -d log ]; then mkdir log; fi
	@if [ ! -f ct.cfg ]; then cp $(top_srcdir)/test/ct.cfg .; fi
	@if [ ! -f test.spec ]; then cp $(top_srcdir)/test/test.spec .; fi
	for i in $(addsuffix _data,$(testmods)); do \
		if [ -d $(top_srcdir)/test/$$i ]; then \
			if [ ! -d $$i ]; then \
				mkdir $$i; \
			fi; \
			for j in `ls $(top_srcdir)/test/$$i`; do \
				if [ $(top_srcdir)/test/$$i/$$j -nt $$i/$$j ]; then \
					cp $(top_srcdir)/test/$$i/$$j $$i/$$j; \
				fi; \
			done; \
		fi; \
	done
	(cd $(CERT_DIR) && \
		if [ ! -f CAkey.pem -o ! -f CAcert.pem ]; then \
			openssl req -newkey rsa:2048 -nodes -x509 -days 30 \
					-subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=ca.$$(hostname)\/emailAddress=support@$$(hostname) \
					-keyout CAkey.pem -out CAcert.pem; \
		fi; \
		if [ ! -f key.pem -o ! -f cert.csr ]; then \
		openssl req -newkey rsa:2048 -nodes \
				-subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=$$(hostname)\/emailAddress=support@$$(hostname) \
				-keyout key.pem -out cert.csr; \
				chmod 600 key.pem; \
		fi; \
		if [ ! -f extensions ]; then \
			echo "extendedKeyUsage = serverAuth" > extensions; \
			echo "subjectAltName = DNS:$$(hostname)" >> extensions; \
		fi; \
		if [ ! -f cert.pem ]; then \
			openssl x509 -req -CA CAcert.pem -CAkey CAkey.pem -CAcreateserial \
					-extfile extensions -in cert.csr -out cert.pem; \
		fi)
	if [ ! -f ocs_eap_ttls_SUITE_data/CAcert.pem ]; then \
		cp $(CERT_DIR)/CAcert.pem ocs_eap_ttls_SUITE_data; \
	fi
	if [ ! -f ocs_rest_api_SUITE_data/CAcert.pem ]; then \
		cp $(CERT_DIR)/CAcert.pem ocs_rest_api_SUITE_data; \
	fi
	if [ ! -f ocs_rest_api_SUITE_data/key.pem ]; then \
		openssl rsa -in $(CERT_DIR)/key.pem -out ocs_rest_api_SUITE_data/key.pem; \
		chmod 600 ocs_rest_api_SUITE_data/key.pem; \
	fi
	NODENAME=`id -un`
	ct_run -no_auto_compile -pa $(top_builddir)/ebin -logdir log -config ct.cfg \
			$(CT_RUN_ARGS) -dir . -erl_args -config sys -sname $(LOGNAME)-ct

sys.config:	$(top_srcdir)/test/sys.config-in
	sed -e 's#%CERT_DIR%#$(abs_top_builddir)/test/$(CERT_DIR)#' \
			-e 's#%APP_PRIV%#$(abs_top_srcdir)/priv#' \
			-e 's#%SERVER_ROOT%#$(abs_top_builddir)/test#' \
			-e "s#%RADIUS_AUTH_PORT%#$$(($$RANDOM + 32768))#" \
			-e "s#%RADIUS_ACCT_PORT%#$$(($$RANDOM + 32768))#" \
			-e "s#%DIAMETER_AUTH_PORT%#$$(($$RANDOM + 32768))#" \
			-e "s#%DIAMETER_ACCT_PORT%#$$(($$RANDOM + 32768))#" \
			-e "s#%HTTP_ADDRESS%#$$(hostname)#" \
			-e "s#%HTTP_PORT%#$$(($$RANDOM + 32768))#" < $< > $@

clean-local:
	-rm -r *.beam certificates/* log/*

