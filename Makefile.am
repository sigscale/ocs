## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = include src ebin doc c_src priv test

if HAVE_SYSTEMD
systemdsystemunit_DATA = scripts/ocs.service
env_DATA = scripts/ocs
endif

.PHONY: release
release:	$(PACKAGE_NAME).tar.gz

$(PACKAGE_NAME).tar.gz:
	ERL_LIBS=$(DESTDIR)/$(ERLANG_INSTALL_LIB_DIR) $(ERL) -s systools make_script $(PACKAGE_NAME) -s init stop -noinput
	ERL_LIBS=$(DESTDIR)/$(ERLANG_INSTALL_LIB_DIR) $(ERL) -s ocs_app make_relup $(PACKAGE_NAME) -s init stop -noinput
	ERL_LIBS=$(DESTDIR)/$(ERLANG_INSTALL_LIB_DIR) $(ERL) -s systools make_tar $(PACKAGE_NAME) -s init stop -noinput

install-data-hook: $(PACKAGE_NAME).tar.gz
	$(MKDIR_P) $(DESTDIR)/$(reldir)
	$(INSTALL_DATA) $< $(DESTDIR)/$(reldir)

EXTRA_DIST = $(PACKAGE_NAME).tar.gz

CLEANFILES = $(PACKAGE_NAME).script $(PACKAGE_NAME).boot $(PACKAGE_NAME).tar.gz

