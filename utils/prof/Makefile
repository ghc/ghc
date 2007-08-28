TOP=../..
include $(TOP)/mk/boilerplate.mk

SUBDIRS = cgprof icons

INPLACE_SCRIPT=ghcprof-inplace
all::
	$(RM) -f $(INPLACE_SCRIPT)
	echo '#!$(PERL)'                                                              >> $(INPLACE_SCRIPT)
	echo '$$DEFAULT_TMPDIR = "$(DEFAULT_TMPDIR)";'                                >> $(INPLACE_SCRIPT)
	echo '$$TARGETPLATFORM = "$(TARGETPLATFORM)";'                                >> $(INPLACE_SCRIPT)
	echo '$$icondir = "$(FPTOOLS_TOP_ABS)/ghc/utils/prof/icons";'                 >> $(INPLACE_SCRIPT)
	echo '$$xmlparser = "$(FPTOOLS_TOP_ABS)/ghc/utils/prof/xmlparser/xmlparser";' >> $(INPLACE_SCRIPT)
	echo '$$cgprof_dir = "$(FPTOOLS_TOP_ABS)/ghc/utils/prof/cgprof";'             >> $(INPLACE_SCRIPT)
	cat ghcprof.prl                                                               >> $(INPLACE_SCRIPT)
	$(EXECUTABLE_FILE) $(INPLACE_SCRIPT)

INSTALLED_SCRIPT=$(DESTDIR)$(bindir)/ghcprof
install::
	$(RM) -f $(INSTALLED_SCRIPT)
	echo '#!$(PERL)'                                >> $(INSTALLED_SCRIPT)
	echo '$$DEFAULT_TMPDIR = "$(DEFAULT_TMPDIR)";'  >> $(INSTALLED_SCRIPT)
	echo '$$TARGETPLATFORM = "$(TARGETPLATFORM)";'  >> $(INSTALLED_SCRIPT)
	echo '$$icondir = "$(libexecdir)/icons";'       >> $(INSTALLED_SCRIPT)
	echo '$$xmlparser = "$(libexecdir)/xmlparser";' >> $(INSTALLED_SCRIPT)
	echo '$$cgprof_dir = "$(libexecdir)";'          >> $(INSTALLED_SCRIPT)
	cat ghcprof.prl                                 >> $(INSTALLED_SCRIPT)
	$(EXECUTABLE_FILE) $(INSTALLED_SCRIPT)

binary-dist:
	$(INSTALL_DIR)              $(BIN_DIST_DIR)/utils/prof
	$(INSTALL_DATA) Makefile    $(BIN_DIST_DIR)/utils/prof/
	$(INSTALL_DATA) ghcprof.prl $(BIN_DIST_DIR)/utils/prof/
	set -e; for d in $(SUBDIRS); do $(MAKE) -C $$d binary-dist; done

include $(TOP)/mk/target.mk
