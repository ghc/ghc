# -----------------------------------------------------------------------------
# To compile with nhc98 on unix:
#     nhc98 -cpp -package base -o hsc2hs-bin Main.hs

TOP=../..
include $(TOP)/mk/boilerplate.mk

CURRENT_DIR=ghc/utils/hsc2hs
INCLUDE_DIR=ghc/includes

INSTALLING=1

SRC_HC_OPTS += -i$(GHC_LIB_COMPAT_DIR)
SRC_LD_OPTS += -L$(GHC_LIB_COMPAT_DIR) -lghccompat

HS_PROG           = hsc2hs-bin
ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
HS_PROG           = hsc2hs$(exeext)
endif
ifeq "$(HOSTPLATFORM)" "i386-unknown-cygwinw32"
HS_PROG           = hsc2hs$(exeext)
endif

ifeq "$(ghc_ge_504)" "NO"
SRC_HC_OPTS +=  -package util
endif

# Note: Somehow we should pass $(exeext) here, but the history of changes used
# for calling the C preprocessor via GHC has changed a few times, making a
# clean solution impossible. So we revert to a hack in Main.hs...
SRC_HC_OPTS      += -Wall

ifneq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
INSTALLED_SCRIPT_PROG  = hsc2hs
endif
INPLACE_SCRIPT_PROG    = hsc2hs-inplace

ifeq "$(INSTALLING)" "1"
TOP_PWD 	:= $(prefix)
SCRIPT_PROG 	=  $(INSTALLED_SCRIPT_PROG)
else
TOP_PWD 	:= $(FPTOOLS_TOP_ABS)
SCRIPT_PROG 	=  $(INPLACE_SCRIPT_PROG)
endif

ifeq "$(INSTALLING)" "1"
ifeq "$(BIN_DIST)"   "1"
HSC2HS_BINDIR=$$\"\"libexecdir
HSC2HS_DIR=$$\"\"libdir
HSC2HS_EXTRA=
else
HSC2HS_BINDIR=$(libexecdir)
HSC2HS_DIR=$(libdir)
HSC2HS_EXTRA=--cc=$(bindir)/ghc-$(ProjectVersion)
endif # BIN_DIST
else
HSC2HS_BINDIR=$(FPTOOLS_TOP_ABS)/$(CURRENT_DIR)
HSC2HS_DIR=$(FPTOOLS_TOP_ABS_PLATFORM)/$(CURRENT_DIR)

ifeq "$(HOSTPLATFORM)" "i386-unknown-mingw32"
extra_flags=$(addprefix --cflag=,$(filter-out -O,$(SRC_CC_OPTS)))
endif

HSC2HS_EXTRA="--cc=$(CC) --ld=$(CC) $(extra_flags) --cflag=-D__GLASGOW_HASKELL__=$(ProjectVersionInt) -I$(FPTOOLS_TOP_ABS_PLATFORM)/$(INCLUDE_DIR)"
endif

$(SCRIPT_PROG) : Makefile
$(INSTALLED_SCRIPT_PROG) : $(TOP)/mk/config.mk

SCRIPT_SUBST_VARS = HSC2HS_BINDIR HSC2HS_DIR HS_PROG HSC2HS_EXTRA

SCRIPT_OBJS=hsc2hs.sh
INTERP=$(SHELL)

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
INSTALL_SCRIPTS  += $(SCRIPT_PROG)
INSTALL_LIBEXECS += $(HS_PROG)
else
INSTALL_PROGS    += $(HS_PROG)
endif

override datadir=$(libdir)
INSTALL_DATAS += template-hsc.h

# -----------------------------------------------------------------------------
# don't recurse on 'make install'
#
ifeq "$(INSTALLING)" "1"
all :: $(HS_PROG)
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@

clean distclean maintainer-clean :: 
	$(MAKE) INSTALLING=0 BIN_DIST=0 $(MFLAGS) $@
endif

include $(TOP)/mk/target.mk
