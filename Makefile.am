## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = include src ebin doc c_src priv mibs test

if HAVE_SYSTEMD
systemdsystemunit_DATA = scripts/ocs.service
env_DATA = scripts/ocs
endif

script_SCRIPTS = scripts/install_tables.escript \
		scripts/join_cluster.escript \
		scripts/install_snmp.sh \
		scripts/install_certs.sh \
		scripts/install_release.sh \
		scripts/snapshot.escript \
		scripts/export_ipdr.escript \
		scripts/send_sms.escript \
		scripts/voice_call.escript \
		scripts/data_session.escript \
		scripts/aaa-sta.escript \
		scripts/nrf.escript \
		scripts/fill_subs.escript

RELEASE_NAME = $(PACKAGE_NAME)-$(PACKAGE_VERSION)

.PHONY: release
release: $(RELEASE_NAME).tar.gz

$(RELEASE_NAME).rel: $(PACKAGE_NAME).rel
	cp $< $@

ERLTMP := $(shell mktemp -d)

if MAKE_TAR_EXTRA_FILES
$(RELEASE_NAME).tar.gz: $(RELEASE_NAME).rel ebin/$(PACKAGE_NAME).appup sys.config
	mkdir -p $(ERLTMP)/lib/$(RELEASE_NAME)
	cp -r $(DESTDIR)/$(ERLANG_INSTALL_LIB_DIR)/$(RELEASE_NAME)/* $(ERLTMP)/lib/$(RELEASE_NAME)
	ERL_LIBS=$(ERLTMP)/lib:$(ERL_LIBS) RELDIR=releases $(ERL) \
			-noinput -boot_var OTPHOME lib \
			-eval "systools:make_script(\"$(RELEASE_NAME)\", \
					[{variables, [{\"OTPHOME\", \"$(ERLTMP)\"}]}])" \
			-eval "{ok, [{release, _, _, Apps}]} = file:consult(\"$(RELEASE_NAME).rel\"), \
					ExtraFiles = [{code:where_is_file(atom_to_list(App) ++ \".appup\"), \
							filename:join([lib, atom_to_list(App) ++ \"-\" ++ Vsn, ebin, \
									atom_to_list(App) ++ \".appup\"])} \
							|| {App, Vsn} <- Apps, \
							filelib:is_regular(code:where_is_file(atom_to_list(App) ++ \".appup\"))], \
					systools:make_tar(\"$(RELEASE_NAME)\", \
							[{dirs, [include,mibs,doc]}, {extra_files, ExtraFiles}])" \
			-s init stop
	rm -rf $(ERLTMP)
else
$(RELEASE_NAME).tar.gz: $(RELEASE_NAME).rel ebin/$(PACKAGE_NAME).appup sys.config
	mkdir -p $(ERLTMP)/lib/$(RELEASE_NAME)
	cp -r $(DESTDIR)/$(ERLANG_INSTALL_LIB_DIR)/$(RELEASE_NAME)/* $(ERLTMP)/lib/$(RELEASE_NAME)
	ERL_LIBS=$(ERLTMP)/lib:$(ERL_LIBS) RELDIR=releases $(ERL) \
			-noinput -boot_var OTPHOME lib \
			-eval "systools:make_script(\"$(RELEASE_NAME)\", \
					[{variables, [{\"OTPHOME\", \"$(ERLTMP)\"}]}])" \
			-eval "systools:make_tar(\"$(RELEASE_NAME)\", [{dirs, [include,mibs,doc]}])" \
			-s init stop
	rm -rf $(ERLTMP)
endif

install-data-hook: $(RELEASE_NAME).tar.gz
	$(MKDIR_P) $(DESTDIR)/$(reldir)
	$(INSTALL_DATA) $< $(DESTDIR)/$(reldir)
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/db
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/ssl
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log/abmf
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log/acct
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log/auth
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log/export
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log/http
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log/ipdr
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/log/sasl
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/snmp/conf
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/snmp/db
	$(MKDIR_P) $(DESTDIR)/$(otpdir)/bin

EXTRA_DIST = $(RELEASE_NAME).tar.gz

CLEANFILES = $(RELEASE_NAME).rel $(RELEASE_NAME).script $(RELEASE_NAME).boot $(RELEASE_NAME).tar.gz

