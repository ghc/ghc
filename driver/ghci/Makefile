#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.11 2005/05/05 00:58:38 sof Exp $
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# ghci script

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
C_PROG = ghci
C_OBJS += ghci.res
else
C_SRCS=
endif

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
INSTALL_PROGS    += $(C_PROG)
GHCII_SCRIPT=$(DESTDIR)$(bindir)/ghcii.sh
install::
	$(RM) -f $(GHCII_SCRIPT)
	echo "#!$(SHELL)"                                  >> $(GHCII_SCRIPT)
	echo 'exec "$$0"/../ghc --interactive $${1+"$$@"}' >> $(GHCII_SCRIPT)
else
LINK = ghci
LINK_TARGET = $(LINK)-$(ProjectVersion)
INSTALLED_SCRIPT=$(DESTDIR)$(bindir)/$(LINK_TARGET)
install::
	$(RM) -f $(INSTALLED_SCRIPT)
	echo "#!$(SHELL)"                                          >> $(INSTALLED_SCRIPT)
	echo "GHCBIN=$(libexecdir)/ghc-$(ProjectVersion)"          >> $(INSTALLED_SCRIPT)
	echo "TOPDIROPT=-B$(libdir)"                               >> $(INSTALLED_SCRIPT)
	echo 'exec $$GHCBIN $$TOPDIROPT --interactive $${1+"$$@"}' >> $(INSTALLED_SCRIPT)
	$(EXECUTABLE_FILE) $(INSTALLED_SCRIPT)
endif

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
INPLACE_SCRIPT=ghci-inplace
CLEAN_FILES += $(INPLACE_SCRIPT)
all::
	$(RM) -f $(INPLACE_SCRIPT)
	echo "#!$(SHELL)"                                                              >> $(INPLACE_SCRIPT)
	echo "GHCBIN=$(FPTOOLS_TOP_ABS)/$(GHC_COMPILER_DIR_REL)/ghc-$(ProjectVersion)" >> $(INPLACE_SCRIPT)
	echo "TOPDIROPT=$(FPTOOLS_TOP_ABS)"                                            >> $(INPLACE_SCRIPT)
	echo 'exec $$GHCPKGBIN --global-conf $$PKGCONF $${1+"$$@"}'                    >> $(INPLACE_SCRIPT)
	$(EXECUTABLE_FILE) $(INPLACE_SCRIPT)
endif

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ghci.res : ghci.rc ghci.ico
	windres --preprocessor="$(CPP) -xc -DRC_INVOKED" -o ghci.res -i ghci.rc -O coff
endif

binary-dist:
	$(INSTALL_DIR)           $(BIN_DIST_DIR)/driver/ghci
	$(INSTALL_DATA) Makefile $(BIN_DIST_DIR)/driver/ghci/

include $(TOP)/mk/target.mk
