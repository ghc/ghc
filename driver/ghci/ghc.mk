# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

ifeq "$(GhcWithInterpreter)" "YES"
ifneq "$(Windows_Host)" "YES"

install: install_driver_ghci

.PHONY: install_driver_ghci
install_driver_ghci: WRAPPER=$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghci-$(ProjectVersion)
install_driver_ghci:
	$(INSTALL_DIR) "$(DESTDIR)$(bindir)"
	$(call removeFiles,                                "$(WRAPPER)")
	$(CREATE_SCRIPT)                                   "$(WRAPPER)"
	echo '#!/bin/sh'                                   >> "$(WRAPPER)"
	echo 'exec "$(bindir)/$(CrossCompilePrefix)ghc-$(ProjectVersion)" --interactive "$$@"' >> "$(WRAPPER)"
	$(EXECUTABLE_FILE)                                 "$(WRAPPER)"
	$(call removeFiles,"$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghci")
	$(LN_S) $(CrossCompilePrefix)ghci-$(ProjectVersion) "$(DESTDIR)$(bindir)/$(CrossCompilePrefix)ghci"

else # Windows_Host...

driver/ghci_dist_C_SRCS  = ghci.c ../utils/cwrapper.c ../utils/getLocation.c ../utils/isMinTTY.c
driver/ghci_dist_CC_OPTS += -I driver/utils
driver/ghci_dist_PROGNAME = ghci
driver/ghci_dist_INSTALL = YES
driver/ghci_dist_INSTALL_INPLACE = YES
driver/ghci_dist_OTHER_OBJS = driver/ghci/ghci.res

$(eval $(call build-prog,driver/ghci,dist,1))

driver/ghci_dist_PROG_VER = ghci-$(ProjectVersion)$(exeext1)

INSTALL_BINS += driver/ghci/dist/build/tmp/$(driver/ghci_dist_PROG_VER)

driver/ghci/ghci.res : driver/ghci/ghci.rc driver/ghci/ghci.ico
	"$(WINDRES)" --preprocessor="$(CPP) -xc -DRC_INVOKED" -o driver/ghci/ghci.res -i driver/ghci/ghci.rc -O coff

driver/ghci/dist/build/tmp/$(driver/ghci_dist_PROG_VER) : driver/ghci/dist/build/tmp/$(driver/ghci_dist_PROG)
	"$(CP)" $< $@

install : install_driver_ghcii

.PHONY: install_driver_ghcii
install_driver_ghcii: GHCII_SCRIPT=$(DESTDIR)$(bindir)/ghcii.sh
install_driver_ghcii: GHCII_SCRIPT_VERSIONED = $(DESTDIR)$(bindir)/ghcii-$(ProjectVersion).sh
install_driver_ghcii:
	$(INSTALL_DIR) "$(DESTDIR)$(bindir)"
	$(call removeFiles,"$(GHCII_SCRIPT)")
	echo "#!/bin/sh"                                        >> $(GHCII_SCRIPT)
	echo 'exec "$$(dirname "$$0")"/ghc --interactive "$$@"' >> $(GHCII_SCRIPT)
	$(EXECUTABLE_FILE) $(GHCII_SCRIPT)
	cp $(GHCII_SCRIPT) $(GHCII_SCRIPT_VERSIONED)
	$(EXECUTABLE_FILE) $(GHCII_SCRIPT_VERSIONED)

endif
endif

