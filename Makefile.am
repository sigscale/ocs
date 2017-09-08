## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = include src ebin doc c_src priv test

rel_DATA = $(PACKAGE_NAME).tar.gz

EXTRA_DIST = $(PACKAGE_NAME).tar.gz

if HAVE_SYSTEMD
systemdsystemunit_DATA = scripts/ocs.service
env_DATA = scripts/ocs
endif


.PHONY:
release:	$(PACKAGE_NAME).tar.gz

$(PACKAGE_NAME).tar.gz:
	ERL_LIBS=$(ERLANG_INSTALL_LIB_DIR) $(ERL) -s systools make_script $(PACKAGE_NAME) -s init stop -noinput
	ERL_LIBS=$(ERLANG_INSTALL_LIB_DIR) $(ERL) -s systools make_tar $(PACKAGE_NAME) -s init stop -noinput

CLEANFILES = $(PACKAGE_NAME).script $(PACKAGE_NAME).boot $(PACKAGE_NAME).tar.gz

