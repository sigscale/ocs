## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = include src ebin doc c_src priv test

.PHONY:
release:
	$(ERL) -s systools make_script $(PACKAGE_NAME) -s init stop -noinput
	$(ERL) -s systools make_tar $(PACKAGE_NAME) -s init stop -noinput

CLEANFILES = ocs.script ocs.boot ocs.tar.gz

