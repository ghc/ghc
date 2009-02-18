#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.11 2005/05/05 00:58:38 sof Exp $
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# ghci script

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
C_PROG = ghci$(exeext)
C_PROG_VERSIONED = ghci-$(ProjectVersion)$(exeext)
C_OBJS += ghci.res

all :: $(C_PROG_VERSIONED)
$(C_PROG_VERSIONED) : $(C_PROG)
	cp $< $@
else
C_SRCS=
endif

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
INSTALL_PROGS    += $(C_PROG) $(C_PROG_VERSIONED)
GHCII_SCRIPT=$(DESTDIR)$(bindir)/ghcii.sh
GHCII_SCRIPT_VERSIONED = $(DESTDIR)$(bindir)/ghcii-$(ProjectVersion).sh
install::
	$(RM) -f $(GHCII_SCRIPT)
	echo "#!$(SHELL)"                                  >> $(GHCII_SCRIPT)
	echo 'exec "$$0"/../ghc --interactive $${1+"$$@"}' >> $(GHCII_SCRIPT)
	chmod +x $(GHCII_SCRIPT)
	cp $(GHCII_SCRIPT) $(GHCII_SCRIPT_VERSIONED)
	chmod +x $(GHCII_SCRIPT_VERSIONED)
else
LINK = ghci
LINK_TARGET = $(LINK)-$(ProjectVersion)
INSTALLED_SCRIPT=$(DESTDIR)$(bindir)/$(LINK_TARGET)
install::
	$(RM) -f $(INSTALLED_SCRIPT)
	echo "#!$(SHELL)"                                   >> $(INSTALLED_SCRIPT)
	echo 'exec $(bindir)/ghc-$(ProjectVersion) --interactive $${1+"$$@"}' >> $(INSTALLED_SCRIPT)
	$(EXECUTABLE_FILE) $(INSTALLED_SCRIPT)
endif

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
INPLACE_SCRIPT = ghci
INPLACE_GHC = $(FPTOOLS_TOP_ABS)/ghc/stage2-inplace/ghc
CLEAN_FILES += $(INPLACE_SCRIPT)
all::
	$(RM) -f $(INPLACE_SCRIPT)
	echo "#!$(SHELL)"                                    >> $(INPLACE_SCRIPT)
	echo "exec $(INPLACE_GHC) --interactive $${1+"$$@"}" >> $(INPLACE_SCRIPT)
	$(EXECUTABLE_FILE) $(INPLACE_SCRIPT)
endif

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
ghci.res : ghci.rc ghci.ico
	windres --preprocessor="$(CPP) -xc -DRC_INVOKED" -o ghci.res -i ghci.rc -O coff
endif

include $(TOP)/mk/bindist.mk

include $(TOP)/mk/target.mk
