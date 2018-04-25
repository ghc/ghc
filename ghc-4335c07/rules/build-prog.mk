# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------


# Build a program.
#
# $(eval $(call build-prog,utils/genapply,dist-install,1))

define build-prog
$(call trace, build-prog($1,$2,$3))
$(call profStart, build-prog($1,$2,$3))
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

ifeq "$$($1_$2_PROGNAME)" ""
$$(error $1_$2_PROGNAME is not set)
endif
ifneq "$$($1_$2_PROG)" ""
$$(error $1_$2_PROG is set)
endif
$1_$2_PROG = $$($1_$2_PROGNAME)$$(exeext$3)

ifeq "$$(findstring $3,0 1 2)" ""
$$(error $1/$2: stage argument to build-prog should be 0, 1, or 2)
endif

$(call clean-target,$1,$2,$1/$2)

$$(eval $$(call build-prog-vars,$1,$2,$3))

ifneq "$$($1_$2_NOT_NEEDED)" "YES"
$$(eval $$(call build-prog-helper,$1,$2,$3))
endif
$(call profEnd, build-prog($1,$2,$3))
endef





define build-prog-vars
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

ifeq "$$($1_USES_CABAL)" "YES"
$1_$2_USES_CABAL = YES
endif

ifeq "$$(Windows_Host)" "YES"
$1_$2_WANT_INPLACE_WRAPPER = NO
else ifneq "$$($1_$2_INSTALL_INPLACE)" "YES"
$1_$2_WANT_INPLACE_WRAPPER = NO
else ifeq "$$($1_$2_SHELL_WRAPPER)" "YES"
$1_$2_WANT_INPLACE_WRAPPER = YES
else ifeq "$$(DYNAMIC_GHC_PROGRAMS)" "YES"
$1_$2_WANT_INPLACE_WRAPPER = YES
else
$1_$2_WANT_INPLACE_WRAPPER = NO
endif

ifeq "$$(Windows_Host)" "YES"
$1_$2_WANT_INSTALLED_WRAPPER = NO
else ifneq "$$($1_$2_INSTALL)" "YES"
$1_$2_WANT_INSTALLED_WRAPPER = NO
else ifeq "$$($1_$2_SHELL_WRAPPER)" "YES"
$1_$2_WANT_INSTALLED_WRAPPER = YES
else
$1_$2_WANT_INSTALLED_WRAPPER = NO
endif

$1_$2_depfile_base = $1/$2/build/.depend

ifeq "$$($1_$2_INSTALL_INPLACE)" "NO"
ifneq "$$(CLEANING)" "YES"
$1_$2_INPLACE = $$(error $1_$2 should not be installed inplace, but INPLACE var evaluated)
else
$1_$2_INPLACE =
endif
else
ifneq "$$($$($1_$2_PROGNAME)_INPLACE)" ""
$$(error $$($1_$2_PROGNAME)_INPLACE defined twice)
endif
#
# Where do we install the wrapper and the binary?
#   $$($1_$2_PROGNAME)_INPLACE  The thing we run (script or binary)
#   $1_$2_INPLACE               The binary
#
ifeq "$$($1_$2_TOPDIR)" "YES"
$$($1_$2_PROGNAME)_INPLACE = $$(INPLACE_LIB)/bin/$$($1_$2_PROG)
ifeq "$$($1_$2_WANT_INPLACE_WRAPPER)" "YES"
$1_$2_INPLACE = $$(INPLACE_LIB)/bin/$$($1_$2_PROG).bin
else
$1_$2_INPLACE = $$(INPLACE_LIB)/bin/$$($1_$2_PROG)
endif
else
$$($1_$2_PROGNAME)_INPLACE = $$(INPLACE_BIN)/$$($1_$2_PROG)
ifeq "$$($1_$2_WANT_INPLACE_WRAPPER)" "YES"
$1_$2_INPLACE = $$(INPLACE_LIB)/bin/$$($1_$2_PROG)
else
$1_$2_INPLACE = $$($$($1_$2_PROGNAME)_INPLACE)
endif
endif
endif

endef





define build-prog-helper
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

$(call package-config,$1,$2,$3)

ifeq "$$($1_$2_USES_CABAL)" "YES"
$(call build-package-data,$1,$2,$3)
ifneq "$$(NO_INCLUDE_PKGDATA)" "YES"
ifeq "$3" "0"
include $1/$2/package-data.mk
else ifeq "$(phase)" "final"
include $1/$2/package-data.mk
endif
endif
endif

$(call all-target,$1,all_$1_$2)
$(call all-target,$1_$2,$1/$2/build/tmp/$$($1_$2_PROG))

$(call shell-wrapper,$1,$2)

ifeq "$$($1_$2_PROGRAM_WAY)" ""
ifeq "$3" "0"
$1_$2_PROGRAM_WAY = v
else ifeq "$$(DYNAMIC_GHC_PROGRAMS)" "YES"
$1_$2_PROGRAM_WAY = dyn
else
$1_$2_PROGRAM_WAY = v
endif
endif

$1_$2_WAYS = $$($1_$2_PROGRAM_WAY)

$1_$2_DYNAMIC_TOO = NO

$(call hs-sources,$1,$2)
$(call c-sources,$1,$2)

# --- IMPLICIT RULES

$(call distdir-opts,$1,$2,$3)
$(call distdir-way-opts,$1,$2,$$($1_$2_PROGRAM_WAY),$3)

ifeq "$3" "0"
# For stage 0, we use GHC to compile C sources so that we don't have to
# worry about where the RTS header files are
$(call c-suffix-rules,$1,$2,$$($1_$2_PROGRAM_WAY),YES)
else
ifeq "$$($1_$2_UseGhcForCC)" "YES"
$(call c-suffix-rules,$1,$2,$$($1_$2_PROGRAM_WAY),YES)
else
$(call c-suffix-rules,$1,$2,$$($1_$2_PROGRAM_WAY),NO)
endif
endif

$$(foreach dir,$$($1_$2_HS_SRC_DIRS),\
  $$(eval $$(call hs-suffix-rules-srcdir,$1,$2,$$(dir))))
$(call hs-suffix-way-rules,$1,$2,$$($1_$2_PROGRAM_WAY))

$(call c-objs,$1,$2,$$($1_$2_PROGRAM_WAY))
$(call hs-objs,$1,$2,$$($1_$2_PROGRAM_WAY))

$1_$2_LINK_WITH_GCC = NO

ifeq "$$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS)" ""
# We don't want to link the GHC RTS into C-only programs. There's no
# point, and it confuses the test that all GHC-compiled programs
# were compiled with the right GHC.
$1_$2_$$($1_$2_PROGRAM_WAY)_GHC_LD_OPTS += -no-auto-link-packages -no-hs-main
endif

ifneq "$$(BINDIST)" "YES"

# The quadrupled $'s here are because the _<way>_LIB variables aren't
# necessarily set when this part of the makefile is read
$1/$2/build/tmp/$$($1_$2_PROG) $1/$2/build/tmp/$$($1_$2_PROG).dll : \
    $$(foreach dep,$$($1_$2_TRANSITIVE_DEP_COMPONENT_IDS),\
        $$$$($$(dep)_dist-$(if $(filter 0,$3),boot,install)_PROGRAM_DEP_LIB))
# Workaround: We use TRANSITIVE_DEP_COMPONENT_IDS here as a workaround for
# Trac #12078.

$1_$2_PROG_NEEDS_C_WRAPPER = NO
$1_$2_PROG_INPLACE = $$($1_$2_PROG)
ifeq "$$(Windows_Host) $$($1_$2_PROGRAM_WAY)" "YES dyn"
ifneq "$$($1_$2_HS_SRCS)" ""
$1_$2_PROG_NEEDS_C_WRAPPER = YES
$1_$2_PROG_INPLACE = inplace-$$($1_$2_PROG)
endif
endif

ifeq "$$($1_$2_PROG_NEEDS_C_WRAPPER)" "YES"

$1_$2_RTS_OPTS_FLAG = $$(lastword $$(filter -rtsopts -rtsopts=all -rtsopts=some -rtsopts=none -no-rtsopts,$$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_HC_OPTS)))
ifeq "$$($1_$2_RTS_OPTS_FLAG)" "-rtsopts"
$1_$2_RTS_OPTS = RtsOptsAll
else ifeq "$$($1_$2_RTS_OPTS_FLAG)" "-rtsopts=all"
$1_$2_RTS_OPTS = RtsOptsAll
else ifeq "$$($1_$2_RTS_OPTS_FLAG)" "-rtsopts=some"
$1_$2_RTS_OPTS = RtsOptsSafeOnly
else ifeq "$$($1_$2_RTS_OPTS_FLAG)" "-rtsopts=none"
$1_$2_RTS_OPTS = RtsOptsNone
else ifeq "$$($1_$2_RTS_OPTS_FLAG)" "-no-rtsopts"
$1_$2_RTS_OPTS = RtsOptsNone
else
$1_$2_RTS_OPTS = RtsOptsSafeOnly
endif

$1/$2/build/tmp/$$($1_$2_PROG)-inplace-wrapper.c: driver/utils/dynwrapper.c | $$$$(dir $$$$@)/.
	$$(call removeFiles,$$@)
	echo '#include <Windows.h>' >> $$@
	echo '#include "Rts.h"' >> $$@
	echo 'LPTSTR path_dirs[] = {' >> $$@
	$$(foreach d,$$($1_$2_DEP_LIB_REL_DIRS),$$(call make-command,echo '    TEXT("/../../$$d")$$(comma)' >> $$@))
	echo '    TEXT("/../../$1/$2/build/tmp/"),' >> $$@
	echo '    NULL};' >> $$@
	echo 'LPTSTR progDll = TEXT("../../$1/$2/build/tmp/$$($1_$2_PROG).dll");' >> $$@
	echo 'LPTSTR rtsDll = TEXT("$$($$(WINDOWS_DYN_PROG_RTS))");' >> $$@
	echo 'int rtsOpts = $$($1_$2_RTS_OPTS);' >> $$@
	cat driver/utils/dynwrapper.c >> $$@

$1/$2/build/tmp/$$($1_$2_PROG)-wrapper.c: driver/utils/dynwrapper.c | $$$$(dir $$$$@)/.
	$$(call removeFiles,$$@)
	echo '#include <Windows.h>' >> $$@
	echo '#include "Rts.h"' >> $$@
	echo 'LPTSTR path_dirs[] = {' >> $$@
	$$(foreach p,$$($1_$2_TRANSITIVE_DEP_COMPONENT_IDS),$$(call make-command,echo '    TEXT("/../lib/$$p")$$(comma)' >> $$@))
	echo '    TEXT("/../lib/"),' >> $$@
	echo '    NULL};' >> $$@
	echo 'LPTSTR progDll = TEXT("../lib/$$($1_$2_PROG).dll");' >> $$@
	echo 'LPTSTR rtsDll = TEXT("$$($$(WINDOWS_DYN_PROG_RTS))");' >> $$@
	echo 'int rtsOpts = $$($1_$2_RTS_OPTS);' >> $$@
	cat driver/utils/dynwrapper.c >> $$@

$1/$2/build/tmp/$$($1_$2_PROG_INPLACE) : $1/$2/build/tmp/$$($1_$2_PROG)-inplace-wrapper.c $1/$2/build/tmp/$$($1_$2_PROG).dll | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) -no-hs-main -no-auto-link-packages -optc-g -optc-O0 -Iincludes $$< -o $$@

$1/$2/build/tmp/$$($1_$2_PROG) : $1/$2/build/tmp/$$($1_$2_PROG)-wrapper.c $1/$2/build/tmp/$$($1_$2_PROG).dll | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) -no-hs-main -no-auto-link-packages -optc-g -optc-O0 -Iincludes $$< -o $$@

$1/$2/build/tmp/$$($1_$2_PROG).dll : $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) | $$$$(dir $$$$@)/.
	$$(call build-dll,$1,$2,$$($1_$2_PROGRAM_WAY),,$$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS),$$@)
else # $1_$2_PROG_NEEDS_C_WRAPPER=NO
ifeq "$$($1_$2_LINK_WITH_GCC)" "NO"
$1/$2/build/tmp/$$($1_$2_PROG) : $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) -o $$@ $$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_HC_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_GHC_LD_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) $$(addprefix -l,$$($1_$2_EXTRA_LIBRARIES))

else
$1/$2/build/tmp/$$($1_$2_PROG) : $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_CC) -o $$@ $$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_CC_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_ALL_LD_OPTS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_C_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_S_OBJS) $$($1_$2_OTHER_OBJS) $$($1_$2_$$($1_$2_PROGRAM_WAY)_EXTRA_CC_OPTS) $$(addprefix -l,$$($1_$2_EXTRA_LIBRARIES))
endif
endif # $1_$2_PROG_NEEDS_C_WRAPPER

# Note [lib-depends] if this program is built with stage1 or greater, we
# need to depend on the libraries too.  NB. since $(ALL_STAGE1_LIBS) and
# $(ALL_RTS_LIBS) are not defined until after libraries/*/ghc.mk have
# been included, this introduces an ordering dependency.
ifneq "$$(CLEANING)" "YES"
ifneq "$3" "0"
ifneq "$$($1_$2_HS_SRCS)" ""
ifeq "$$(strip $$(ALL_STAGE1_$$($1_$2_PROGRAM_WAY)_LIBS))" ""
$$(error ordering failure in $1 ($2): ALL_STAGE1_$$($1_$2_PROGRAM_WAY)_LIBS is empty)
endif
$1/$2/build/tmp/$$($1_$2_PROG) : $$(ALL_STAGE1_$$($1_$2_PROGRAM_WAY)_LIBS) $$(ALL_RTS_LIBS)
endif
endif
endif

ifeq "$$($1_$2_INSTALL_INPLACE)" "YES"
$$($1_$2_INPLACE) : $1/$2/build/tmp/$$($1_$2_PROG_INPLACE) | $$$$(dir $$$$@)/.
	$$(INSTALL) -m 755 $$< $$@
endif

endif # BINDIST

ifneq "$$($1_$2_INSTALL_INPLACE)" "NO"
$(call all-target,$1_$2,$$($1_$2_INPLACE))
endif
$(call clean-target,$1,$2_inplace,$$($1_$2_INPLACE))

ifeq "$$($1_$2_INSTALL)" "YES"
ifeq "$$($1_$2_PROG_NEEDS_C_WRAPPER)" "YES"
INSTALL_LIBS     += $1/$2/build/tmp/$$($1_$2_PROG).dll
endif
ifeq "$$($1_$2_WANT_INSTALLED_WRAPPER)" "YES"
INSTALL_LIBEXECS += $1/$2/build/tmp/$$($1_$2_PROG)
else ifeq "$$($1_$2_TOPDIR)" "YES"
INSTALL_LIBEXECS += $1/$2/build/tmp/$$($1_$2_PROG)
else
INSTALL_BINS     += $1/$2/build/tmp/$$($1_$2_PROG)
endif
endif

$(call dependencies,$1,$2,$3)

# The Main module of a program implicitly depends on GHC.TopHandler
# so we need to add a dependency for that. As we don't know which
# module contains Main, we just make all modules in the program
# depend on it.
ifneq "$3" "0"
$$(foreach o,$$($1_$2_$$($1_$2_PROGRAM_WAY)_HS_OBJS),$$(eval $$(call add-dependency,$$o,libraries/base/dist-install/build/GHC/TopHandler.$$($$($1_$2_PROGRAM_WAY)_osuf))))
endif

endef
