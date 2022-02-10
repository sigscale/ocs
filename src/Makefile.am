## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

noinst_DATA = diameter_gen_ietf.erl \
		diameter_gen_etsi.erl \
		diameter_gen_3gpp.erl \
		diameter_gen_3gpp_ro_application.erl \
		diameter_gen_3gpp_sta_application.erl \
		diameter_gen_3gpp_swm_application.erl \
		diameter_gen_3gpp_swx_application.erl \
		diameter_gen_3gpp_s6a_application.erl \
		diameter_gen_3gpp_s6b_application.erl \
		diameter_gen_3gpp_gx_application.erl \
		diameter_gen_nas_application_rfc7155.erl \
		diameter_gen_cc_application_rfc4006.erl \
		diameter_gen_eap_application_rfc4072.erl \
		ocs_rest_query_scanner.erl \
		ocs_rest_query_parser.erl

DIAMETERC=$(ERLANG_LIB_DIR_diameter)/bin/diameterc

diameter_gen_ietf.erl:	ietf.dia
	$(DIAMETERC) -H $<

diameter_gen_etsi.erl:	etsi.dia
	$(DIAMETERC) -H $<

diameter_gen_ietf.hrl:	ietf.dia
	$(DIAMETERC) -E $<

diameter_gen_3gpp.erl:	3gpp.dia $(top_builddir)/ebin/diameter_gen_ietf.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_3gpp.hrl:	3gpp.dia $(top_builddir)/ebin/diameter_gen_ietf.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_3gpp_ro_application.erl:	3gpp_ro_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_3gpp_ro_application.hrl:	3gpp_ro_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_3gpp_sta_application.erl:	3gpp_sta_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_3gpp_sta_application.hrl:	3gpp_sta_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_3gpp_swm_application.erl:	3gpp_swm_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_3gpp_swm_application.hrl:	3gpp_swm_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_3gpp_swx_application.erl:	3gpp_swx_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_3gpp_swx_application.hrl:	3gpp_swx_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_3gpp_s6a_application.erl:	3gpp_s6a_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_3gpp_s6a_application.hrl:	3gpp_s6a_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_3gpp_s6b_application.erl:	3gpp_s6b_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_3gpp_s6b_application.hrl:	3gpp_s6b_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -E $<

diameter_gen_3gpp_gx_application.erl:	3gpp_gx_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
	$(DIAMETERC) -i $(top_builddir)/ebin -H $<

diameter_gen_3gpp_gx_application.hrl:	3gpp_gx_application.dia $(top_builddir)/ebin/diameter_gen_3gpp.beam
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

%.erl:%.xrl
	$(ERLC) $<

%.erl:%.yrl
	$(ERLC) $<

CLEANFILES = $(noinst_DATA)

