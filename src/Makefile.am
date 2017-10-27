## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

erl_DATA = diameter_gen_3gpp.erl \
		diameter_gen_3gpp_ro_application.erl \
		diameter_gen_nas_application_rfc7155.erl \
		diameter_gen_cc_application_rfc4006.erl \
		diameter_gen_eap_application_rfc4072.erl

DIAMETERC=$(ERLANG_LIB_DIR_diameter)/bin/diameterc

$(top_builddir)/ebin/%.beam:%.erl
	$(ERLC) $(ERLCFLAGS) $<

diameter_gen_3gpp.erl:	3gpp.dia
	$(DIAMETERC) -H $<

diameter_gen_3gpp.hrl:	3gpp.dia
	$(DIAMETERC) -E $<

diameter_gen_3gpp_ro_application.erl:	3gpp_ro_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam 
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_3gpp_ro_application.hrl:	3gpp_ro_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_nas_application_rfc7155.erl:	nas_application_rfc7155.dia
	$(DIAMETERC) -H $<

diameter_gen_nas_application_rfc7155.hrl:	nas_application_rfc7155.dia
	$(DIAMETERC) -E $<

diameter_gen_cc_application_rfc4006.erl:	cc_application_rfc4006.dia $(top_builddir)/ebin/diameter_gen_nas_application_rfc7155.beam 
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_cc_application_rfc4006.hrl:	cc_application_rfc4006.dia $(top_builddir)/ebin/diameter_gen_nas_application_rfc7155.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_eap_application_rfc4072.erl:	eap_application_rfc4072.dia $(top_builddir)/ebin/diameter_gen_nas_application_rfc7155.beam 
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_eap_application_rfc4072.hrl:	eap_application_rfc4072.dia $(top_builddir)/ebin/diameter_gen_nas_application_rfc7155.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

CLEANFILES = $(erl_DATA)

