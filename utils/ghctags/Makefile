TOP=../..
include $(TOP)/mk/boilerplate.mk

SRC_HC_OPTS += -package ghc

HC=$(GHC_STAGE1)

# On Windows, ghc-pkg is a standalone program
# ($bindir/ghc-pkg.exe), whereas on Unix it needs a wrapper script
# to pass the appropriate flag to the real binary
# ($libexecdir/ghc-pkg.bin) so that it can find package.conf.
ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
HS_PROG           = ghctags.exe
INSTALL_PROGS    += $(HS_PROG)
else
HS_PROG           = ghctags.bin
INSTALL_LIBEXECS += $(HS_PROG)
endif

# -----------------------------------------------------------------------------
# ghctags and ghctags-inplace scripts

# See commentary in ../ghc-pkg/Makefile

INPLACE_HS=ghctags-inplace.hs
INPLACE_PROG=ghctags-inplace
EXCLUDED_SRCS+=$(INPLACE_HS)

$(INPLACE_HS): Makefile $(FPTOOLS_TOP)/mk/config.mk
	echo "import System.Cmd; import System.Environment; import System.Exit" > $@
	echo "main = do args <- getArgs; rawSystem \"$(FPTOOLS_TOP_ABS)/$(GHC_GHCTAGS_DIR_REL)/$(HS_PROG)\" (\"--topdir\":\"$(FPTOOLS_TOP_ABS)\":args) >>= exitWith" >> $@

$(INPLACE_PROG): $(INPLACE_HS)
	$(HC) --make $< -o $@

all :: $(INPLACE_PROG)

CLEAN_FILES += $(INPLACE_HS) $(INPLACE_PROG)

ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
LINK = ghctags
LINK_TARGET = $(LINK)-$(ProjectVersion)
INSTALLED_SCRIPT=$(DESTDIR)$(bindir)/$(LINK_TARGET)
install::
	$(INSTALL_DIR) $(DESTDIR)$(bindir)
	$(RM) -f $(INSTALLED_SCRIPT)
	echo "#!$(SHELL)"                                      >> $(INSTALLED_SCRIPT)
	echo "GHCTAGSBIN=$(libexecdir)/$(HS_PROG)"             >> $(INSTALLED_SCRIPT)
	echo "TOPDIR=$(libdir)"                  	       >> $(INSTALLED_SCRIPT)
	echo 'exec $$GHCTAGSBIN --topdir $$TOPDIR $${1+"$$@"}' >> $(INSTALLED_SCRIPT)
	$(EXECUTABLE_FILE) $(INSTALLED_SCRIPT)
endif

binary-dist:
	$(INSTALL_DIR)                $(BIN_DIST_DIR)/utils/ghctags
	$(INSTALL_DATA)    Makefile   $(BIN_DIST_DIR)/utils/ghctags/
	$(INSTALL_PROGRAM) $(HS_PROG) $(BIN_DIST_DIR)/utils/ghctags/

include $(TOP)/mk/target.mk

