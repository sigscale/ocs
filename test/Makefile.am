## Makefile.am
##
## Process this file with automake to produce Makefile.in

testmods = ocs_api_SUITE

check_DATA = .dialyzer_ok .common_test_ok

.dialyzer_ok:	$(addsuffix .beam, $(testmods))
	dialyzer --src $(top_srcdir)/src

%.beam:$(top_srcdir)/test/%.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_srcdir)/include $<

.common_test_ok:	$(addsuffix .beam, $(testmods))
	@if [ ! -d log ]; then mkdir log; fi
	@if [ ! -f ct.cfg ]; then cp $(top_srcdir)/test/ct.cfg .; fi
	for i in $(addsuffix _data, $(testmods)); do \
		if [ -d $(top_srcdir)/test/$$i -a ! -d $$i ]; then \
			mkdir $$i; \
			for j in `ls $(top_srcdir)/test/$$i`; do \
				if [ $(top_srcdir)/test/$$i/$$j -nt $$i/$$j ]; then \
					cp $(top_srcdir)/test/$$i/$$j $$i/$$j; \
				fi; \
			done; \
		fi; \
	done
	ct_run -no_auto_compile -pa $(top_builddir)/ebin -logdir log -config ct.cfg -suite $(testmods)

clean-local:
	-rm -r *.beam log/*

