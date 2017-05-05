## Makefile.am
##
## Process this file with automake to produce Makefile.in

erl_DATA = diameter_gen_nas_application_rfc7155.erl \
		diameter_gen_cc_application_rfc4006.erl

$(top_builddir)/ebin/%.beam:%.erl
	$(ERLC) $(ERLCFLAGS) $<

diameter_gen_nas_application_rfc7155.erl:	nas_application_rfc7155.dia
	diameterc -H $<

diameter_gen_nas_application_rfc7155.hrl:	nas_application_rfc7155.dia
	diameterc -E $<

diameter_gen_cc_application_rfc4006.erl:	cc_application_rfc4006.dia $(top_builddir)/ebin/diameter_gen_nas_application_rfc7155.beam 
	diameterc -i $(top_builddir)/ebin -H $<

diameter_gen_cc_application_rfc4006.hrl:	cc_application_rfc4006.dia $(top_builddir)/ebin/diameter_gen_nas_application_rfc7155.beam
	diameterc -i $(top_builddir)/ebin -E $<

CLEANFILES = $(erl_DATA)

