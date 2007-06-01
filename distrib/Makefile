
include Makefile-vars

.PHONY: in-place mk-version-symlinks install-dirs install

in-place ::
	@if test -x "./post-inplace-script" ; then \
		echo "Running project-specific post-inplace script ..." ; \
		./post-inplace-script $(platform) `pwd` \
			$(package)-$(version); \
		echo "Done" ; \
	fi
	@echo "Finished configuring..to use, add `pwd`/bin/$(platform) to your PATH."

install-dirs ::
	$(INSTALL_DIR) $(bindir)
	(cd lib/$(platform); find . -type d -exec sh -c '$(INSTALL_DIR) $$0 $(libdir)/$$0' {} \; )
	(cd share; find . -type d -exec sh -c '$(INSTALL_DIR) $(datadir)/$$0' {} \; )

install :: install-dirs install-driver install-utils install-libs install-datas install-docs postinstall denounce

.PHONY: install-libs install-utils install-datas postinstall denounce

denounce:
	@echo
	@echo =======================================================================
	@echo Installation of $(package)-$(version) was successful.
	@echo
	@echo To use, add $(bindir) to your PATH.
	@echo
	@if test -f share/html/index.html; then \
	   echo For documentation, see $(htmldir)/index.html ; \
	elif test -d share/html ; then \
	   echo Docs can be found in $(htmldir). ; \
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

install-libs:
	(cd lib/$(platform); find . -type f -exec sh -c '$(CP) $$0 $(libdir)/$$0' {} \; )
	$(MAKE) -C libraries install DOING_BIN_DIST=YES

install-driver:
	$(MAKE) -C driver install DOING_BIN_DIST=YES

install-utils:
	$(MAKE) -C utils install DOING_BIN_DIST=YES

install-datas:
	set -e; for i in `(cd share; find . -type f )`; do \
	   $(INSTALL_DATA) share/$$i $(datadir)/$$i; \
	done

show-install-setup:
	@echo "Install setup..."
	@echo "bindir  = $(bindir)"
	@echo "libdir  = $(libdir) (libdir  = $(libdir))"
	@echo "datadir = $(datadir) (datadir = $(datadir))"

#
# Documentation targets: install-docs.
#

.PHONY: show-install-setup install-docs install-dirs-docs

install-docs : install-dirs-docs
	if test -d share/html ; then $(CP) -r share/html/* $(htmldir) ; fi
	set -e; for i in share/*.ps; do \
		if test -f "$$i"; then \
		      $(CP) $$i $(psdir) ; \
		fi \
	done

install-dirs-docs:
	$(INSTALL_DIR) $(htmldir) 

