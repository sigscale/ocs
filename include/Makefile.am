## Makefile.am
##
## Process this file with automake to produce Makefile.in

einclude_DATA = ocs.hrl ocs_log.hrl ocs_eap_codec.hrl \
		diameter_gen_nas_application_rfc7155.hrl

diameter_gen_nas_application_rfc7155.hrl:	$(top_srcdir)/src/nas_application_rfc7155.dia
	diameterc -E $<

CLEANFILES = $(erl_DATA)

