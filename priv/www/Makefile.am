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

$(abs_builddir)/bower.json:	$(srcdir)/bower.json
	cp $(srcdir)/bower.json $@

bower_components: $(abs_builddir)/bower.json
	bower install

install-data-local:
	cp -r $(abs_builddir)/bower_components $(DESTDIR)$(wwwdir)

clean-local:
	-rm -rf bower_components

