## Makefile.am
## vim: ts=3
##
## Process this file with automake to produce Makefile.in

schema_DATA = $(abs_builddir)/partyManagement.swagger.json \
				$(abs_builddir)/usageManagement.swagger.json \
				$(abs_builddir)/resourceInventoryManagement.swagger.json \
				$(abs_builddir)/balanceManagement.swagger.json


$(abs_builddir)/%.json:	$(srcdir)/%.json
	if [ ! -e $@ ]; then \
		$(LN_S) $< $@; \
	fi

check_DATA = .validation_ok

.validation_ok:
	@if ajv help > /dev/null 2>&1; then \
		if ! ajv compile -s partyManagement.swagger.json; \
         then exit 1; fi; \
		if ! ajv compile -s usageManagement.swagger.json; \
         then exit 1; fi; \
		if ! ajv compile -s balanceManagement.swagger.json; \
         then exit 1; fi; \
		if ! ajv compile -s resourceInventoryManagement.swagger.json; \
         then exit 1; fi; \
	else \
      echo "ajv-cli needed to validate JSON Schema"; \
   fi
