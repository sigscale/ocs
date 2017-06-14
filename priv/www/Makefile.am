## Makefile.am
##
## Process this file with automake to produce Makefile.in

www_DATA = favicon.ico \
	index.html \
	sig-ipdr-list.html \
	sig-access-list.html \
	sig-accounting-list.html \
	sig-http-list.html \
	sig-ipdr-log-files.html \
	sig-app.html \
	sig-client-add.html \
	sig-client-list.html \
	sig-client-update.html \
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

