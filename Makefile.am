## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = include src ebin doc c_src priv mibs test

beam_DATA = ebin/ocs.appup

if HAVE_SYSTEMD
systemdsystemunit_DATA = scripts/ocs.service
env_DATA = scripts/ocs
endif

.PHONY: release
release:	$(PACKAGE_VERSION).tar.gz

ERLTMP := $(shell mktemp -d)

$(PACKAGE_VERSION).rel:	$(PACKAGE_NAME).rel
	cp $(PACKAGE_NAME).rel $(PACKAGE_VERSION).rel

$(PACKAGE_VERSION).tar.gz: $(PACKAGE_VERSION).rel
	mkdir -p $(ERLTMP)/lib/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
	cp -r $(DESTDIR)/$(ERLANG_INSTALL_LIB_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION)/* $(ERLTMP)/lib/$(PACKAGE_NAME)-$(PACKAGE_VERSION)
	ERL_LIBS=$(ERLTMP)/lib:$(ERL_LIBS) RELDIR=releases $(ERL) -noinput -boot_var OTPHOME lib \
			-eval "systools:make_script(\"$(PACKAGE_VERSION)\", [{variables, [{\"OTPHOME\", \"$(ERLTMP)\"}]}])" \
			-eval "systools:make_tar(\"$(PACKAGE_VERSION)\", [{dirs, [include,mibs]}])" \
			-s init stop
	rm -rf $(ERLTMP)

install-data-hook: $(PACKAGE_VERSION).tar.gz
	$(MKDIR_P) $(DESTDIR)/$(reldir)
	$(INSTALL_DATA) $< $(DESTDIR)/$(reldir)

EXTRA_DIST = $(PACKAGE_VERSION).tar.gz

CLEANFILES = $(PACKAGE_VERSION).rel $(PACKAGE_VERSION).script $(PACKAGE_VERSION).boot $(PACKAGE_VERSION).tar.gz

