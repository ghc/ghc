TOP=../..
include $(TOP)/mk/boilerplate.mk

HS_PROG		= runghc$(exeext)
INSTALL_PROGS   += $(HS_PROG)

UseGhcForCc = YES
SRC_MKDEPENDC_OPTS += -I$(GHC_INCLUDE_DIR)

SRC_HC_OPTS += -i$(GHC_LIB_COMPAT_DIR)
SRC_LD_OPTS += -L$(GHC_LIB_COMPAT_DIR) -lghccompat

# This is required because libghccompat.a must be built with
# $(GhcHcOpts) because it is linked to the compiler, and hence
# we must also build with $(GhcHcOpts) here:
SRC_HC_OPTS += $(GhcHcOpts)

# This is horrible.  We ought to be able to omit the entire directory
# from mkDependHS.
SRC_MKDEPENDHS_OPTS += \
	-optdep--exclude-module=Compat.RawSystem \
	-optdep--exclude-module=Compat.Directory \
	-optdep--exclude-module=Distribution.Compat.ReadP \
	-optdep--exclude-module=Distribution.Extension \
	-optdep--exclude-module=Distribution.GetOpt \
	-optdep--exclude-module=Distribution.InstalledPackageInfo \
	-optdep--exclude-module=Distribution.License \
	-optdep--exclude-module=Distribution.Package \
	-optdep--exclude-module=Distribution.ParseUtils \
	-optdep--exclude-module=Distribution.Setup \
	-optdep--exclude-module=Distribution.Version \
	-optdep--exclude-module=System.Directory.Internals

all :: runhaskell

runhaskell : $(HS_PROG)
	$(CP) $< runhaskell$(exeext)

# Only install runhaskell if there isn't already one installed
ifneq "$(findstring install, $(MAKECMDGOALS))" ""
ifeq "$(wildcard $(bindir)/runhaskell)" ""
INSTALL_PROGS += runhaskell
endif
endif

include $(TOP)/mk/target.mk
