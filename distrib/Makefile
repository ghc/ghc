MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

ProjectVersion:=$(shell bin/ghc --numeric-version)

include mk/install.mk

define GHC_WRAPPER
#!/bin/sh
exec "$(libdir)/bin/ghc" -B"$(libdir)" $${1+"$$@"}
endef

export GHC_WRAPPER

.PHONY: default
default:
	@echo 'Run "make install" to install'
	@false

.PHONY: install
install:
	@cp settings lib/
	@echo $(prefix) $(bindir) $(libdir)
	@mkdir -p $(prefix) $(libdir) $(bindir)
	@cp -R lib/* $(libdir)/
	# cp the rest to $(prefix) ? or maybe handle $(datadir) etc too?
	@if [ "$(bindir)" = "$(prefix)/bin" ] || [ "$(libdir)" != "$(prefix)/lib" ]; then \
	   echo "custom bindir or libdir"; \
	   mkdir -p $(libdir)/bin; \
	   cp bin/* $(libdir)/bin/; \
	   echo "$$GHC_WRAPPER" > $(bindir)/ghc; \
	fi
	@echo "ghc available at $(bindir)/ghc"
	@echo done
