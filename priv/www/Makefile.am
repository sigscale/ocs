## Makefile.am
##
## Process this file with automake to produce Makefile.in

www_DATA = favicon.ico \
	toolbar.png \
	progress.png \
	filter.png \
	sort-ascend.png \
	sort-descend.png \
	index.html \
	sig-ipdr-list.html \
	sig-access-list.html \
	sig-accounting-list.html \
	sig-http-list.html \
	sig-ipdr-log-files.html \
	sig-app.html \
	sig-help.html \
	sig-client-add.html \
	sig-client-list.html \
	sig-client-update.html \
	sig-user-list.html \
	sig-user-add.html \
	sig-user-update.html \
	sig-sub-add.html \
	sig-sub-list.html \
	sig-sub-update.html

noinst_DATA = bower_components

bower_components: $(srcdir)/bower.json
	@if [ ! -e $(builddir)/bower.json ]; then \
		$(LN_S) $(srcdir)/bower.json; \
	fi
	bower install
	@touch $(builddir)/bower_components

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(wwwdir)
	cp -R $(abs_builddir)/bower_components/* $(DESTDIR)$(wwwdir)
	$(MKDIR_P) $(DESTDIR)$(wwwdir)/locales
	cp $(abs_srcdir)/locales/* $(DESTDIR)$(wwwdir)/locales

clean-local:
	-rm -rf bower_components

sig-help.html: sig-help.html-in
	sed -e 's!%VERSION%!$(VERSION)!' < $< > $@

