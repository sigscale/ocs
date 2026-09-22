## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

www_DATA = $(abs_builddir)/dist/index.html

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
		sig-rate-table-list.js \
		sig-period-table-list.js \
		sig-roaming-table-list.js \
		sig-tariff-rate-list.js \
		sig-tariff-rate-update.js \
		sig-tariff-period-update.js \
		sig-tariff-roaming-update.js \
		sig-rate-table-add.js \
		sig-period-table-add.js \
		sig-roaming-table-add.js \
		sig-tariff-rate-add.js \
		sig-tariff-period-list.js \
		sig-tariff-period-add.js \
		sig-tariff-roaming-list.js \
		sig-tariff-roaming-add.js \
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

$(abs_builddir)/dist/index.html: vite.config.js $(abs_builddir)/index.html ${patsubst %,$(abs_builddir)/src/%,$(MODULES)}
	$(MKDIR_P) $(abs_builddir)/dist
	if [ ! -e $(abs_builddir)/package.json ]; then \
		cp $(srcdir)/package.json $(abs_builddir); \
	fi
	if [ ! -e $(abs_builddir)/vite.config.js ]; then \
		cp $(srcdir)/vite.config.js $(abs_builddir); \
	fi
	npm install vite vite-plugin-pwa workbox-cli --save-dev
	npm run build

install-data-local:
	$(MKDIR_P) $(DESTDIR)$(wwwdir)
	cp -R $(abs_builddir)/dist/* $(DESTDIR)$(wwwdir)

clean-local:
	-rm $(abs_builddir)/src/sig-help.js
	for i in $(LINKS); do \
		if [ -L $$i ]; then \
			rm $$i; \
		fi; \
	done

$(abs_builddir)/src/sig-help.js: $(srcdir)/src/sig-help.js-in
	sed -e 's!%VERSION%!$(VERSION)!' \
			-e 's!%PACKAGE%!$(PACKAGE)!' < $< > $@

