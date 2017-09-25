## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

einclude_DATA = ocs.hrl ocs_log.hrl ocs_eap_codec.hrl \
		diameter_gen_nas_application_rfc7155.hrl \
		diameter_gen_cc_application_rfc4006.hrl \
		diameter_gen_eap_application_rfc4072.hrl

DIAMETERC=$(ERLANG_LIB_DIR_diameter)/bin/diameterc

$(top_builddir)/ebin/%.beam:$(top_builddir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) -o $(top_builddir)/ebin $<

$(top_builddir)/src/diameter_gen_nas_application_rfc7155.erl:	$(top_builddir)/src/nas_application_rfc7155.dia
	$(DIAMETERC) -H -o $(top_builddir)/src $<

diameter_gen_nas_application_rfc7155.hrl:	$(top_srcdir)/src/nas_application_rfc7155.dia
	$(DIAMETERC) -E $<

diameter_gen_cc_application_rfc4006.hrl:	$(top_srcdir)/src/cc_application_rfc4006.dia $(top_builddir)/ebin/diameter_gen_nas_application_rfc7155.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_eap_application_rfc4072.hrl:	$(top_srcdir)/src/eap_application_rfc4072.dia $(top_builddir)/ebin/diameter_gen_nas_application_rfc7155.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

CLEANFILES = diameter_gen_nas_application_rfc7155.hrl diameter_gen_cc_application_rfc4006.hrl diameter_gen_eap_application_rfc4072.hrl

