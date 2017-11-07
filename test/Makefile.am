## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

CERT_DIR = certificates

testmods = ocs_api_SUITE \
			ocs_rest_api_SUITE \
			ocs_rest_lib_SUITE \
			ocs_codec_eap_SUITE \
			ocs_eap_pwd_SUITE \
			ocs_eap_ttls_SUITE \
			ocs_simple_auth_SUITE \
			ocs_accounting_SUITE \
			ocs_log_SUITE \
			ocs_rating_SUITE \
			peer_tls_transport\
			diameter_test_client_cb \
			ocs_test_lib

check_DATA = .dialyzer_ok .common_test_ok

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl $(top_srcdir)/include/ocs.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

.common_test_ok:	$(addsuffix .beam, $(testmods)) sys.config
	@if [ ! -d $(CERT_DIR) ]; then mkdir $(CERT_DIR); fi
	@if [ ! -d ocs_eap_ttls_SUITE_data ]; then mkdir ocs_eap_ttls_SUITE_data; fi
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
					-subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=$$(hostname)\/emailAddress=support@$$(hostname) \
					-keyout CAkey.pem -out CAcert.pem; \
		fi; \
		if [ ! -f key.pem -o ! -f cert.csr ]; then \
		openssl req -newkey rsa:2048 -nodes \
				-subj /C=CA/ST=Ontario/L=Toronto/O=Example/CN=$$(hostname)\/emailAddress=support@$$(hostname) \
				-keyout key.pem -out cert.csr; \
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
	ct_run -no_auto_compile -pa $(top_builddir)/ebin -logdir log -config ct.cfg -spec test.spec -dir . -erl_args -noshell -config sys -sname $(LOGNAME)-ct

sys.config:	$(top_srcdir)/test/sys.config-in
	sed -e 's#%CERT_DIR%#$(abs_top_builddir)/test/$(CERT_DIR)#' \
			-e 's#%APP_PRIV%#$(abs_top_srcdir)/priv#' \
			-e 's#%SERVER_ROOT%#$(abs_top_builddir)/test#' \
			-e "s#%RADIUS_AUTH_PORT%#$$(($$RANDOM + 32768))#" \
			-e "s#%RADIUS_ACCT_PORT%#$$(($$RANDOM + 32768))#" \
			-e "s#%DIAMETER_AUTH_PORT%#$$(($$RANDOM + 32768))#" \
			-e "s#%DIAMETER_ACCT_PORT%#$$(($$RANDOM + 32768))#" \
			-e "s#%HTTP_PORT%#$$(($$RANDOM + 32768))#" < $< > $@

clean-local:
	-rm -r *.beam certificates/* log/*

