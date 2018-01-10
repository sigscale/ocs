## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = include src ebin doc c_src priv test

if HAVE_SYSTEMD
systemdsystemunit_DATA = scripts/ocs.service
env_DATA = scripts/ocs
endif

.PHONY: release
release:	$(PACKAGE_VERSION).tar.gz

$(PACKAGE_VERSION).tar.gz:
	mkdir -p .tmp/lib/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
	cp -r $(DESTDIR)/$(ERLANG_INSTALL_LIB_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/* .tmp/lib/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
	cp ocs.rel $(PACKAGE_VERSION).rel
	ERL_LIBS=.tmp/lib RELDIR=releases $(ERL) -noinput -boot_var OTPHOME lib \
			-eval "systools:make_script(\"$(PACKAGE_VERSION)\", [{variables, [{\"OTPHOME\", \".tmp\"}]}])" \
			-eval "systools:make_tar(\"$(PACKAGE_VERSION)\", [{dirs, [include]}])" \
			-s init stop

install-data-hook: $(PACKAGE_VERSION).tar.gz
	$(MKDIR_P) $(DESTDIR)/$(reldir)
	$(INSTALL_DATA) $< $(DESTDIR)/$(reldir)

EXTRA_DIST = $(PACKAGE_VERSION).tar.gz

CLEANFILES = $(PACKAGE_NAME).script $(PACKAGE_NAME).boot $(PACKAGE_VERSION).tar.gz

