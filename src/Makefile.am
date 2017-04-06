## Makefile.am
##
## Process this file with automake to produce Makefile.in

erl_DATA = diameter_gen_nas_application_rfc7155.erl

diameter_gen_nas_application_rfc7155.erl:	$(top_srcdir)/src/nas_application_rfc7155.dia
	diameterc -H $<

CLEANFILES = $(erl_DATA)

