## Makefile.am
##
## Process this file with automake to produce Makefile.in

www_DATA = favicon.ico \
	index.html \
	sig-add-client.html \
	sig-add.html \
	sig-list.html \
	sig-search.html

noinst_DATA = bower_components

bower_components: $(srcdir)/bower.json
	if [ ! -e $(builddir)/bower.json ]; then \
		$(LN_S) $(srcdir)/bower.json; \
	fi
	bower install

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(wwwdir)/bower_components
	cp -r $(abs_builddir)/bower_components $(DESTDIR)$(wwwdir)

clean-local:
	-rm -rf bower_components

