
include Makefile-vars

.PHONY: install-strip install postinstall denounce show-install-setup

#.PHONY: in-place
# This has bitrotted:
#in-place ::
#	@if test -x "./post-inplace-script" ; then \
#		echo "Running project-specific post-inplace script ..." ; \
#		./post-inplace-script $(platform) `pwd` \
#			$(package)-$(version); \
#		echo "Done" ; \
#	fi
#	@echo "Finished configuring..to use, add `pwd`/bin/$(platform) to your PATH."

install-strip:
	$(MAKE) INSTALL_PROGRAM='$(INSTALL_PROGRAM) -s' install

install ::
	$(INSTALL_DIR) $(bindir)
	$(INSTALL_DIR) $(datadir)
	echo "[]" > $(datadir)/package.conf

install::
	$(MAKE) -C includes  install      DOING_BIN_DIST=YES
	$(MAKE) -C utils     install      DOING_BIN_DIST=YES
	$(MAKE) -C rts       install      DOING_BIN_DIST=YES
	$(MAKE) -C libraries install      DOING_BIN_DIST=YES
	$(MAKE) -C libffi    install      DOING_BIN_DIST=YES
	$(MAKE) -C compiler  install      DOING_BIN_DIST=YES
	$(MAKE) -C ghc       install      DOING_BIN_DIST=YES
	$(MAKE) -C driver    install      DOING_BIN_DIST=YES
	$(MAKE) -C gmp       install      DOING_BIN_DIST=YES
	$(MAKE) -C docs      install-docs DOING_BIN_DIST=YES
	$(MAKE) -C libraries/Cabal/doc install-docs DOING_BIN_DIST=YES
	$(INSTALL_DATA) $(INSTALL_OPTS) settings $(libdir)

install :: postinstall denounce

# Look to see if $(bindir) is in $(PATH). Assumes there are no funky
# characters.
GREPPED_PATH=$(shell echo ":$(PATH):" | grep ":$(bindir):")

denounce:
	@echo
	@echo =======================================================================
	@echo Installation of $(package)-$(version) was successful.
	@echo
ifeq "$(GREPPED_PATH)" ""
	@echo To use, add $(bindir) to your PATH.
	@echo
endif
	@if test -f $(htmldir)/index.html; then \
	   echo For documentation, see $(htmldir)/index.html ; \
	else \
	   echo "Warning: this binary distribution does NOT contain documentation!" ; \
	fi
	@echo =======================================================================

postinstall:
	@if test -x "./post-install-script" ; then \
		echo "Running project-specific post-install script ..." ; \
		./post-install-script $(platform) $(libdir) ; \
		echo "Done" ; \
	fi

show-install-setup:
	@echo "Install setup..."
	@echo "bindir  = $(bindir)"
	@echo "libdir  = $(libdir) (libdir  = $(libdir))"
	@echo "datadir = $(datadir) (datadir = $(datadir))"

