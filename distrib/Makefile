
include Makefile-vars

.PHONY: in-place config-pkgs mk-version-symlinks install-dirs install

config-pkgs ::
	@echo "Configuring $(package), version $(version), on $(platform) ..."
	@for i in $(PACKAGE_PRL_SCRIPTS) ""; do \
	  if test "$$i"; then \
	   echo "Creating a configured version of $$i .."; \
	   $(RM) bin/$(platform)/$$i.bak; \
	   if [ -f bin/$(platform)/$$i ]; then $(MV) bin/$(platform)/$$i bin/$(platform)/$$i.bak; fi; \
	   echo "#! $(PERL)"  		            >  bin/$(platform)/$$i ; \
	   echo '$$'"bindir='$(bindir)';"           >> bin/$(platform)/$$i ; \
	   echo '$$'"libdir='$(libdir)';"      	    >> bin/$(platform)/$$i ; \
	   echo '$$'"libexecdir='$(libdir)';"       >> bin/$(platform)/$$i ; \
	   echo '$$'"datadir='$(datadir)';"         >> bin/$(platform)/$$i ; \
	   echo '$$'"SED='$(SED)';"		    >> bin/$(platform)/$$i ; \
	   echo '$$'"DEFAULT_TMPDIR='$(DEFAULT_TMPDIR)';" >> bin/$(platform)/$$i ; \
	   cat bin/$(platform)/$$i.prl   	    >> bin/$(platform)/$$i ; \
	   $(CHMOD) $(BIN_PERMS) bin/$(platform)/$$i; \
	   echo "Done."; \
	  fi; \
	done
	@for i in $(PACKAGE_LIB_PRL_SCRIPTS) ""; do \
	  if test "$$i"; then \
	   echo "Creating a configured version of $$i .."; \
	   $(RM) lib/$(platform)/$$i.bak; \
	   if [ -f lib/$(platform)/$$i ]; then $(MV) lib/$(platform)/$$i lib/$(platform)/$$i.bak; fi; \
	   echo "#! $(PERL)"                       >  lib/$(platform)/$$i ; \
	   echo '$$'"bindir='$(bindir)';"          >> lib/$(platform)/$$i ; \
	   echo '$$'"libdir='$(libdir)';"          >> lib/$(platform)/$$i ; \
	   echo '$$'"libexecdir='$(libdir)';"      >> lib/$(platform)/$$i ; \
	   echo '$$'"datadir='$(datadir)';"        >> lib/$(platform)/$$i ; \
	   echo '$$'"SED='$(SED)';"		   >> lib/$(platform)/$$i ; \
	   echo '$$'"DEFAULT_TMPDIR='$(DEFAULT_TMPDIR)';"  >> lib/$(platform)/$$i ; \
	   cat lib/$(platform)/$$i.prl >> lib/$(platform)/$$i; \
	   $(CHMOD) $(BIN_PERMS) lib/$(platform)/$$i; \
	   echo "Done."; \
	  fi; \
	done
	@for i in $(PACKAGE_SH_SCRIPTS) ""; do \
	  if test "$$i"; then \
	   echo "Creating a configured version of $$i .."; \
	   $(RM) bin/$(platform)/$$i.bak; \
	   if [ -f bin/$(platform)/$$i ]; then $(MV) bin/$(platform)/$$i bin/$(platform)/$$i.bak; fi; \
	   echo "#! $(SH)"                      >  bin/$(platform)/$$i ; \
	   echo "bindir='$(bindir)'"            >> bin/$(platform)/$$i ; \
	   echo "libdir='$(libdir)'"            >> bin/$(platform)/$$i ; \
	   echo "libexecdir='$(libdir)'"        >> bin/$(platform)/$$i ; \
	   echo "datadir='$(datadir)'"          >> bin/$(platform)/$$i ; \
	   echo "SED='$(SED)'"			>> bin/$(platform)/$$i ; \
	   echo "DEFAULT_TMPDIR='$(DEFAULT_TMPDIR)'" >> bin/$(platform)/$$i ; \
	   cat bin/$(platform)/$$i.sh >> bin/$(platform)/$$i; \
	   $(CHMOD) $(BIN_PERMS) bin/$(platform)/$$i; \
	   echo "Done."; \
	  fi; \
	done
	@for i in $(PACKAGE_LIB_SPLICED_FILES) ""; do \
	  if test "$$i"; then \
	    echo "Creating a configured version of $$i .."; \
            $(PERL) -i.bak -lpe 's|\$$libdir|$(libdir)|g;s|\$$datadir|$(datadir)|g' lib/$(platform)/$$i; \
          fi; \
	done
	@if test "$(platform)" = "powerpc-apple-darwin"; then \
	  echo "Configuring install names..."; \
	  for i in lib/$(platform)/*; do \
	    sh fix_install_names.sh "$(libdir)" "$$i"; \
	  done; \
	  echo "Done."; \
	fi

in-place ::
	$(MAKE) $(MFLAGS) config-pkgs bindir=`pwd`/bin/$(platform) libdir=`pwd`/lib/$(platform) datadir=`pwd`/share
	@if test -x "./post-inplace-script" ; then \
		echo "Running project-specific post-inplace script ..." ; \
		./post-inplace-script $(platform) `pwd` \
			$(package)-$(version); \
		echo "Done" ; \
	fi
	@echo "Finished configuring..to use, add `pwd`/bin/$(platform) to your PATH."

install-dirs ::
	$(INSTALL_DIR) $(bindir)
	(cd lib/$(platform); find . -type d -exec sh -c '../../$(INSTALL_DIR) $$0 $(libdir)/$$0' {} \; )
	(cd share; find . -type d -exec sh -c '../$(INSTALL_DIR) $(datadir)/$$0' {} \; )

install :: config-pkgs install-dirs install-bin install-libs install-datas install-docs postinstall denounce

.PHONY: install-bin install-libs install-datas postinstall denounce

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

install-bin:
	for i in $(PACKAGE_BIN_INSTALL) ""; do \
	   if test -n "$$i" ; then $(INSTALL_BIN) bin/$(platform)/$$i $(bindir); fi; \
	done;
	for i in $(PACKAGE_BINS) ""; do \
	   if test -n "$$i" ; then $(INSTALL_BIN) bin/$(platform)/$$i$(exeext) $(bindir); fi; \
	done;
	for i in $(PACKAGE_OPT_BINS) ""; do \
	   if test -n "$$i" -a ! -f "$(bindir)/$$i" ; then $(INSTALL_BIN) bin/$(platform)/$$i$(exeext) $(bindir); fi; \
	done;
	@for i in $(VERSION_SYMLINKS_FOR) ""; do \
	   if [ "x$$i" != "x" ]; then 		\
	   	( cd $(bindir); 		\
		  if ( $(PERL) -e '$$fn="'$$i'"; exit ((! -f $$fn || -l $$fn) ? 0 : 1);' ); then \
	   	     echo "Creating a symbolic link from $$i-$(version) to $$i"; \
	   	     $(RM) $$i; \
	             $(LN_S) $$i-$(version) $$i; \
	          else \
	             echo "Creating a symbolic link from $$i-$(version) to $$i failed: \`$$i' already exists"; \
	             echo "Perhaps remove \`$$i' manually?"; \
	             exit 1; \
	          fi;\
		); 				\
	   fi; 					\
	done

install-libs:
	(cd lib/$(platform); find . -type f -exec sh -c '$(CP) $$0 $(libdir)/$$0' {} \; )
	$(MAKE) -C libraries install IN_BIN_DIST=1

install-datas:
	for i in `(cd share; find . -type f )`; do \
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
	for i in share/*.ps; do \
		if test -f "$$i"; then \
		      $(CP) $$i $(psdir) ; \
		fi \
	done

install-dirs-docs:
	$(INSTALL_DIR) $(htmldir) 

