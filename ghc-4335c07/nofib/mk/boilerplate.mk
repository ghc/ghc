#################################################################################
#
#			    nofib/mk/boilerplate.mk
#
#		Boilerplate Makefile for an fptools project
#
#################################################################################

# Begin by slurping in the boilerplate from one level up, 
# with standard TOP-mangling
# Remember, TOP is the top level of the innermost level

default : all

show:
	@echo '$(VALUE)="$($(VALUE))"'

RM = rm -f
SIZE = size
STRIP = strip
PERL = /usr/bin/perl
CONTEXT_DIFF_RAW = diff -U 1
EXECUTABLE_FILE = chmod +x

# Windows MSYS specific settings
ifeq ($(shell uname -s | grep -c MSYS), 1)
	exeext=.exe
	CONTEXT_DIFF=$(CONTEXT_DIFF_RAW) --strip-trailing-cr
else
	CONTEXT_DIFF=$(CONTEXT_DIFF_RAW)
endif

# Benchmarks controls which set of tests should be run
# You can run one or more of
#	imaginary 
#	spectral
#	real
#	parallel
#	gc
#	smp
#	fibon
NoFibSubDirs = imaginary spectral real shootout

# Haskell compiler options for nofib
NoFibHcOpts = -O2

# Number of times to run each program
NoFibRuns = 5

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# (these files are optional, so `make' won't fret if it
#  cannot get to them).
# -----------------------------------------------------------------

SRC_HC_OPTS += $(NoFibHcOpts) -Rghc-timing

ifeq "$(WithNofibHc)" ""

STAGE1_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage1)
STAGE2_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage2)
STAGE3_GHC := $(abspath $(TOP)/../inplace/bin/ghc-stage3)

ifneq "$(wildcard $(STAGE1_GHC) $(STAGE1_GHC).exe)" ""

ifeq "$(BINDIST)" "YES"
HC := $(abspath $(TOP)/../)/bindisttest/install   dir/bin/ghc
else ifeq "$(stage)" "1"
HC := $(STAGE1_GHC)
else ifeq "$(stage)" "3"
HC := $(STAGE3_GHC)
else
# use stage2 by default
HC := $(STAGE2_GHC)
endif

else
HC := $(shell which ghc)
endif

else

# We want to support both "ghc" and "/usr/bin/ghc" as values of WithNofibHc
# passed in by the user, but
#     which ghc          == /usr/bin/ghc
#     which /usr/bin/ghc == /usr/bin/ghc
# so on unix-like platforms we can just always 'which' it.
# However, on cygwin, we can't just use which:
#     $ which c:/ghc/ghc-7.4.1/bin/ghc.exe
#     which: no ghc.exe in (./c:/ghc/ghc-7.4.1/bin)
# so we start off by using realpath, and if that succeeds then we use
# that value. Otherwise we fall back on 'which'.
HC_REALPATH := $(realpath $(WithNofibHc))
ifeq "$(HC_REALPATH)" ""
HC := $(shell which '$(WithNofibHc)')
else
HC := $(HC_REALPATH)
endif

endif

MKDEPENDHS := $(HC) # ToDo: wrong, if $(HC) isn't GHC.

# We need a GHC that can build nofib-analyse. $(HC) will often be a
# freshly built compiler, without the necessary packages installed,
# so it isn't a good bet. Just using plain 'ghc' seems like our best
# bet to find a suitable compiler.
ifeq "$(BOOT_HC)" ""
BOOT_HC = ghc
endif

# We want to support both "ghc" and "/usr/bin/ghc" as values of BOOT_HC
# passed in by the user, but
#     which ghc          == /usr/bin/ghc
#     which /usr/bin/ghc == /usr/bin/ghc
# so on unix-like platforms we can just always 'which' it.
# However, on cygwin, we can't just use which:
#     $ which c:/ghc/ghc-7.4.1/bin/ghc.exe
#     which: no ghc.exe in (./c:/ghc/ghc-7.4.1/bin)
# so we start off by using realpath, and if that succeeds then we use
# that value. Otherwise we fall back on 'which'.
#
# Note also that we need to use 'override' in order to override a
# value given on the commandline.
BOOT_HC_REALPATH := $(realpath $(BOOT_HC))
ifeq "$(BOOT_HC_REALPATH)" ""
override BOOT_HC := $(shell which '$(BOOT_HC)')
else
override BOOT_HC := $(BOOT_HC_REALPATH)
endif

ifeq "$(BOOT_HC)" ""
$(error Could not find BOOT_HC)
endif

define get-ghc-rts-field # $1 = result variable, $2 = field name
$1 := $$(shell '$$(HC)' +RTS --info | grep '^ .("$2",' | tr -d '\r' | sed -e 's/.*", *"//' -e 's/")$$$$//')
endef

define get-ghc-field # $1 = result variable, $2 = field name
$1 := $$(shell '$$(HC)' --info | grep '^ .("$2",' | tr -d '\r' | sed -e 's/.*", *"//' -e 's/")$$$$//')
endef

$(eval $(call get-ghc-rts-field,HC_VERSION,GHC version))
$(eval $(call get-ghc-field,SplitObjs,Object splitting supported))
$(eval $(call get-ghc-field,CC,C compiler command))

define ghc-ge # $1 = major version, $2 = minor version
HC_VERSION_GE_$1_$2 := $$(shell if [ `echo $$(HC_VERSION) | sed 's/\..*//'` -gt $1 ]; then echo YES; else if [ `echo $$(HC_VERSION) | sed 's/\..*//'` -ge $1 ] && [ `echo $$(HC_VERSION) | sed -e 's/[^.]*\.//' -e 's/\..*//'` -ge $2 ]; then echo YES; else echo NO; fi; fi)
endef

$(eval $(call ghc-ge,6,13))

RUNTEST   = $(TOP)/runstdtest/runstdtest

include $(TOP)/mk/ghc-paths.mk
include $(TOP)/mk/ghc-opts.mk
include $(TOP)/mk/paths.mk
include $(TOP)/mk/opts.mk

-include .depend
