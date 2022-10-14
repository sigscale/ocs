## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

www_DATA = $(abs_builddir)/build/polymer.json

LINKS = index.html manifest.json images src

MODULES = sig-ipdr-list-wlan.js \
		sig-ipdr-list-voip.js \
		sig-access-list.js \
		sig-accounting-list.js \
		sig-http-list.js \
		sig-ipdr-log-files-voip.js \
		sig-app.js \
		sig-help.js \
		sig-client-add.js \
		sig-client-list.js \
		sig-client-update.js \
		sig-user-list.js \
		sig-user-add.js \
		sig-user-update.js \
		sig-sub-add.js \
		sig-sub-list.js \
		sig-sub-update.js \
		sig-prefix-list.js \
		sig-prefix-update.js \
		sig-prefix-table-add.js \
		sig-prefix-add.js \
		sig-offer-list.js \
		sig-offer-update.js \
		sig-offer-add.js \
		sig-balance-list.js \
		sig-product-list.js \
		sig-product-add.js \
		sig-bucket-list.js \
		sig-bucket-add.js \
		sig-policy-list.js \
		sig-policy-add.js \
		sig-policy-table-add.js \
		sig-dashboard.js \
		style-element.js

$(abs_builddir)/index.html:
	for i in $(LINKS); do \
		if [ ! -e $(abs_builddir)/$$i ]; then \
			$(LN_S) $(srcdir)/$$i; \
		fi; \
	done

package.json: yarn.lock

yarn.lock:
	yarn add d3
	yarn global add polymer-cli
	yarn global add workbox-cli
	yarn install

$(abs_builddir)/build/polymer.json: package.json $(abs_builddir)/index.html workbox-config.js ${patsubst %,$(abs_builddir)/src/%,$(MODULES)}
	if [ ! -e $(abs_builddir)/polymer.json ]; then \
		cp $(srcdir)/polymer.json $(abs_builddir); \
	fi
	polymer build
	workbox generateSW $(srcdir)/workbox-config.js

check_DATA = .polymer_lint_ok

.polymer_lint_ok:
	yarn run lint

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(wwwdir)
	cp -R $(abs_builddir)/build/esm-bundled/* $(DESTDIR)$(wwwdir)
	$(MKDIR_P) $(DESTDIR)$(wwwdir)/locales
	cp $(abs_srcdir)/locales/* $(DESTDIR)$(wwwdir)/locales
	cp $(abs_builddir)/service-worker.js $(DESTDIR)$(wwwdir)/
	cp $(abs_builddir)/service-worker.js.map $(DESTDIR)$(wwwdir)/
	cp $(abs_builddir)/workbox-*.js $(DESTDIR)$(wwwdir)/
	cp $(abs_builddir)/workbox-*.js.map $(DESTDIR)$(wwwdir)/

clean-local:
	-rm -rf yarn.lock build
	-rm $(abs_builddir)/src/sig-help.js
	-rm  $(abs_builddir)/service-worker.js
	-rm  $(abs_builddir)/service-worker.js.map
	-rm  $(abs_builddir)/workbox-*.js
	-rm  $(abs_builddir)/workbox-*.js.map
	for i in $(LINKS); do \
		if [ -L $$i ]; then \
			rm $$i; \
		fi; \
	done

$(abs_builddir)/src/sig-help.js: $(srcdir)/src/sig-help.js-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%PACKAGE%!$(PACKAGE)!' < $< > $@

