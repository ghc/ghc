# -----------------------------------------------------------------------------
# $Id: bootstrap.mk,v 1.1 2001/03/23 16:36:23 simonmar Exp $
#
# Makefile rules for booting from .hc files without a driver.
#

TOP_SAVED := $(TOP)
TOP:=$(TOP)/ghc

include $(FPTOOLS_TOP_ABS)/ghc/mk/version.mk
include $(FPTOOLS_TOP_ABS)/ghc/mk/paths.mk

# Reset TOP
TOP:=$(TOP_SAVED)

# -----------------------------------------------------------------------------
# Set the platform-specific options to send to the C compiler.  These should
# match the list in machdepCCOpts in ghc/compiler/DriverFlags.hs.
#

PLATFORM_CC_OPTS =
PLATFORM_HC_BOOT_CC_OPTS =

ifeq "$(i386_TARGET_ARCH)" "1"
PLATFORM_CC_OPTS += -DDONT_WANT_WIN32_DLL_SUPPORT
PLATFORM_HC_BOOT_CC_OPTS += -fno-defer-pop -fomit-frame-pointer 
# ToDo:
STOLEN_X86_REGS = 4
endif

ifeq "$(hppa_TARGET_ARCH)" "1"
PLATFORM_CC_OPTS += -static -D_HPUX_SOURCE
endif

ifeq "$(powerpc_TARGET_ARCH)" "1"
PLATFORM_CC_OPTS += -static
PLATFORM_HC_BOOT_CC_OPTS += -finhibit-size-directive
endif

ifeq "$(rs6000_TARGET_ARCH)" "1"
PLATFORM_CC_OPTS += -static
PLATFORM_HC_BOOT_CC_OPTS += -static -finhibit-size-directive
endif

ifeq "$(mingw32_TARGET_OS)" "1"
PLATFORM_CC_OPTS += -mno-cygwin
endif

PLATFORM_CC_OPTS += -D__GLASGOW_HASKELL__=$(ProjectVersionInt) 

HC_BOOT_CC_OPTS = $(PLATFORM_HC_BOOT_CC_OPTS)

# -----------------------------------------------------------------------------
# suffix rules for building a .o from a .hc file.  The normal build system
# should be able to take care of the rest.

%.raw_s : %.hc
	$(CC) -x c $< -o $@ -S -O $(HC_BOOT_CC_OPTS) -I. -I$(FPTOOLS_TOP_ABS)/ghc/includes -I$(FPTOOLS_TOP_ABS)/ghc/lib/std/cbits

%.s : %.raw_s
	$(GHC_MANGLER) $< $@ $(STOLEN_X86_REGS)

%.o : %.s
	$(CC) -c -o $@ $<
