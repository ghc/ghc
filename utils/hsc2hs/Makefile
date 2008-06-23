# -----------------------------------------------------------------------------
# To compile with nhc98 on unix:
#     nhc98 -cpp -package base -o hsc2hs-bin Main.hs

TOP=../..
include $(TOP)/mk/boilerplate.mk

# Beyond stage 1, honour any Mac OS X depolyment target options.  If we use 
# these options in stage 1 we get a linker error if the bootstrap compiler is
#  for a more recent OS version
ifeq "$(stage)" "2"
SRC_CC_OPTS += $(MACOSX_DEPLOYMENT_CC_OPTS)
SRC_HC_OPTS += $(addprefix -optc, $(MACOSX_DEPLOYMENT_CC_OPTS))
SRC_LD_OPTS += $(addprefix -optl, $(MACOSX_DEPLOYMENT_LD_OPTS))
endif

ifeq "$(ghc_ge_605)" "NO"
SRC_HC_OPTS += -cpp
endif

# This causes libghccompat.a to be used:
include $(GHC_COMPAT_DIR)/compat.mk

# This is required because libghccompat.a must be built with
# $(GhcHcOpts) because it is linked to the compiler, and hence
# we must also build with $(GhcHcOpts) here:
SRC_HC_OPTS += $(GhcHcOpts) $(GhcStage1HcOpts)

# We have two version: the inplace version compiled by the bootstrap compiler
#   and the install version compiled by the stage 1 compiler
ifeq "$(stage)" "2"
HS_PROG           = hsc2hs.bin
else
HS_PROG           = hsc2hs-inplace.bin
endif

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

INPLACE_HS=hsc2hs-inplace.hs
INPLACE_PROG=hsc2hs-inplace
EXCLUDED_SRCS+=$(INPLACE_HS)

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
extra_flags=$(addprefix --cflag=,$(filter-out -O,$(SRC_CC_OPTS)))
else
extra_flags=
endif

nothing=
space=$(nothing) $(nothing)
# Given
#   foo bar
# make
#   :\"-Ifoo\":\"-Ibar\"
GMP_INCLUDE_DIRS_STRINGS = $(subst $(space),,$(foreach d,$(GMP_INCLUDE_DIRS),:\"-I$(d)\"))

$(INPLACE_HS): Makefile $(FPTOOLS_TOP)/mk/config.mk
	echo "import System.Cmd; import System.Environment; import System.Exit" > $@
	echo "main = do args <- getArgs; rawSystem \"$(FPTOOLS_TOP_ABS)/$(GHC_HSC2HS_DIR_REL)/$(HS_PROG)\" (\"--template=$(FPTOOLS_TOP_ABS)/$(GHC_HSC2HS_DIR_REL)/template-hsc.h\":\"--cc=$(CC)\":\"--ld=$(CC)\"$(patsubst %,:\"%\",$(extra_flags)):\"--cflag=-D__GLASGOW_HASKELL__=$(ProjectVersionInt)\":\"-I$(FPTOOLS_TOP_ABS)/$(GHC_INCLUDE_DIR_REL)\":\"-I$(FPTOOLS_TOP_ABS)/gmp/gmpbuild\"$(GMP_INCLUDE_DIRS_STRINGS):args) >>= exitWith" >> $@

$(INPLACE_PROG): $(INPLACE_HS)
	$(HC) --make $< -o $@

all :: $(INPLACE_PROG)

CLEAN_FILES += $(INPLACE_HS) $(INPLACE_PROG)

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALL_PROGS    += $(HS_PROG)
else
INSTALL_LIBEXECS += $(HS_PROG)
LINK = hsc2hs-ghc
LINK_TARGET = $(LINK)-$(ProjectVersion)
INSTALLED_SCRIPT=$(DESTDIR)$(bindir)/$(LINK_TARGET)
install::
	$(RM) -f $(INSTALLED_SCRIPT)
	echo "#!$(SHELL)"                  >> $(INSTALLED_SCRIPT)
	echo "HSC2HS_BINDIR=$(libexecdir)" >> $(INSTALLED_SCRIPT)
	echo "HSC2HS_DIR=$(libdir)"        >> $(INSTALLED_SCRIPT)
	echo "HS_PROG=$(HS_PROG)"          >> $(INSTALLED_SCRIPT)
	echo "HSC2HS_EXTRA="               >> $(INSTALLED_SCRIPT)
	cat hsc2hs.sh                      >> $(INSTALLED_SCRIPT)
	$(EXECUTABLE_FILE) $(INSTALLED_SCRIPT)

ifneq "$(NO_INSTALL_HSC2HS)" "YES"
install::
	$(CP) $(INSTALLED_SCRIPT) $(DESTDIR)/$(bindir)/hsc2hs
endif
endif

# hsc2hs-inplace is needed to 'make boot' in compiler.
# Do a recursive 'make all' after generating dependencies, because this
# will work with 'make -j'.
ifneq "$(BootingFromHc)" "YES"
boot :: depend
	$(MAKE) all
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
