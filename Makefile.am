## Makefile.am
##
## Process this file with automake to produce Makefile.in

SUBDIRS = ebin doc include c_src priv test

.PHONY:
release: $(SUBDIRS)
	$(ERL) -s systools make_script $(PACKAGE_NAME) -s init stop -noinput
	$(ERL) -s systools make_tar $(PACKAGE_NAME) -s init stop -noinput

