# -----------------------------------------------------------------------------
# To compile with nhc98 on unix:
#     nhc98 -cpp -package base -o hsc2hs-bin Main.hs

TOP=../..
include $(TOP)/mk/boilerplate.mk

# This causes libghccompat.a to be used:
include $(GHC_COMPAT_DIR)/compat.mk

# This is required because libghccompat.a must be built with
# $(GhcHcOpts) because it is linked to the compiler, and hence
# we must also build with $(GhcHcOpts) here:
SRC_HC_OPTS += $(GhcHcOpts) $(GhcStage1HcOpts)

HS_PROG           = hsc2hs-bin
ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
HS_PROG           = hsc2hs$(exeext)
endif
ifeq "$(HOSTPLATFORM)" "i386-unknown-cygwinw32"
HS_PROG           = hsc2hs$(exeext)
endif

# Note: Somehow we should pass $(exeext) here, but the history of changes used
# for calling the C preprocessor via GHC has changed a few times, making a
# clean solution impossible. So we revert to a hack in Main.hs...
SRC_HC_OPTS      += -Wall

# -----------------------------------------------------------------------------
# hsc2hs script

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
extra_flags=$(addprefix --cflag=,$(filter-out -O,$(SRC_CC_OPTS)))
endif
HSC2HS_INPLACE_EXTRA=--cc=$(CC) --ld=$(CC) $(extra_flags) --cflag=-D__GLASGOW_HASKELL__=$(ProjectVersionInt) -I$(FPTOOLS_TOP_ABS_PLATFORM)/$(GHC_INCLUDE_DIR_REL)

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
# See "MSys Note 3" in compiler/Makefile
INPLACE_SCRIPT=hsc2hs-inplace.bat
FULL_INPLACE_PATH=$(subst /,\,$(FPTOOLS_TOP_ABS_PLATFORM)/utils/hsc2hs/)
all::
	$(RM) -f $(INPLACE_SCRIPT)
	echo '@call $(FULL_INPLACE_PATH)$(HS_PROG) --template=$(FULL_INPLACE_PATH)template-hsc.h $(HSC2HS_INPLACE_EXTRA) %*' >> $(INPLACE_SCRIPT)
	$(EXECUTABLE_FILE) $(INPLACE_SCRIPT)
else
INPLACE_SCRIPT=hsc2hs-inplace
all::
	$(RM) -f $(INPLACE_SCRIPT)
	echo "#!$(SHELL)"                                                   >> $(INPLACE_SCRIPT)
	echo "HSC2HS_BINDIR=$(FPTOOLS_TOP_ABS)/$(GHC_HSC2HS_DIR_REL)"       >> $(INPLACE_SCRIPT)
	echo "HSC2HS_DIR=$(FPTOOLS_TOP_ABS_PLATFORM)/$(GHC_HSC2HS_DIR_REL)" >> $(INPLACE_SCRIPT)
	echo "HS_PROG=$(HS_PROG)"                                           >> $(INPLACE_SCRIPT)
	echo "HSC2HS_EXTRA=$(HSC2HS_INPLACE_EXTRA)"                         >> $(INPLACE_SCRIPT)
	cat hsc2hs.sh                                                       >> $(INPLACE_SCRIPT)
	$(EXECUTABLE_FILE) $(INPLACE_SCRIPT)
endif
CLEAN_FILES += $(INPLACE_SCRIPT)

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_PROGS    += $(HS_PROG)
else
INSTALL_LIBEXECS += $(HS_PROG)
LINK = hsc2hs
LINK_TARGET = $(LINK)-$(ProjectVersion)
INSTALLED_SCRIPT=$(bindir)/$(LINK_TARGET)
install::
	$(RM) -f $(INSTALLED_SCRIPT)
	echo "#!$(SHELL)"                  >> $(INSTALLED_SCRIPT)
	echo "HSC2HS_BINDIR=$(libexecdir)" >> $(INSTALLED_SCRIPT)
	echo "HSC2HS_DIR=$(libdir)"        >> $(INSTALLED_SCRIPT)
	echo "HS_PROG=$(HS_PROG)"          >> $(INSTALLED_SCRIPT)
	echo "HSC2HS_EXTRA="               >> $(INSTALLED_SCRIPT)
	cat hsc2hs.sh                      >> $(INSTALLED_SCRIPT)
	$(EXECUTABLE_FILE) $(INSTALLED_SCRIPT)
endif

# -----------------------------------------------------------------------------

override datadir=$(libdir)
INSTALL_DATAS += template-hsc.h

binary-dist:
	$(INSTALL_DIR)                      $(BIN_DIST_DIR)/utils/hsc2hs
	$(INSTALL_DATA)    Makefile         $(BIN_DIST_DIR)/utils/hsc2hs/
	$(INSTALL_DATA)    hsc2hs.sh        $(BIN_DIST_DIR)/utils/hsc2hs/
	$(INSTALL_DATA)    $(INSTALL_DATAS) $(BIN_DIST_DIR)/utils/hsc2hs/
	$(INSTALL_PROGRAM) $(HS_PROG)       $(BIN_DIST_DIR)/utils/hsc2hs/

include $(TOP)/mk/target.mk
