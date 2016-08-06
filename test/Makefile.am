## Makefile.am
##
## Process this file with automake to produce Makefile.in

testmods = ocs_api_SUITE \
			ocs_codec_eap_SUITE

check_DATA = .dialyzer_ok .common_test_ok

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl $(top_srcdir)/include/ocs.hrl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

.common_test_ok:	$(addsuffix .beam, $(testmods)) sys.config
	@if [ ! -d log ]; then mkdir log; fi
	@if [ ! -f sys.config ]; then cp $(top_srcdir)/test/sys.config .; fi
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
	ct_run -no_auto_compile -pa $(top_builddir)/ebin -logdir log -config ct.cfg -spec test.spec -dir . -erl_args -config sys -sname $(LOGNAME)-ct

clean-local:
	-rm -r *.beam log/*

sys.config:	sys.config-in
	sed -e 's#%APP_PRIV%#$(abs_top_srcdir)/priv/#' < $< > $@ 

