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


# Build a perl script.  Invoke like this:
#
# driver/mangler_PERL_SRC = ghc-asm.lprl
# driver/mangler_dist_PROGNAME = ghc-asm
#
# $(eval $(call build-perl,driver/mangler,dist))

define build-perl
$(call trace, build-perl($1,$2))
$(call profStart, build-perl($1,$2))
# $1 = dir
# $2 = distdir

ifneq "$$(CLEANING)" "YES"
ifeq "$$($1_$2_PROGNAME)" ""
$$(error $1_$2_PROGNAME is not set)
endif
ifneq "$$($1_$2_PROG)" ""
$$(error $1_$2_PROG is set)
endif
$1_$2_PROG = $$($1_$2_PROGNAME)

ifneq "$$($$($1_$2_PROG)_INPLACE)" ""
$$(error $$($1_$2_PROG)_INPLACE defined twice)
endif
ifeq "$$($1_$2_TOPDIR)" "YES"
$$($1_$2_PROG)_INPLACE = $$(INPLACE_TOPDIR)/$$($1_$2_PROG)
else
$$($1_$2_PROG)_INPLACE = $$(INPLACE_BIN)/$$($1_$2_PROG)
endif
endif

$1_$2_INPLACE = $$($$($1_$2_PROG)_INPLACE)

$(call all-target,$1_$2,$$($1_$2_INPLACE))

$(call clean-target,$1,$2,$1/$2 $$($1_$2_INPLACE))
.PHONY: clean_$1
clean_$1 : clean_$1_$2

# INPLACE_BIN etc. might be empty if we're cleaning
ifeq "$(findstring clean,$(MAKECMDGOALS))" ""
ifneq "$$(BINDIST)" "YES"
$1/$2/$$($1_$2_PROG).prl: $1/$$($1_PERL_SRC) $$$$(unlit_INPLACE) | $$$$(dir $$$$@)/.
	"$$(unlit_INPLACE)" $$(UNLIT_OPTS) $$< $$@
endif

$1/$2/$$($1_$2_PROG): $1/$2/$$($1_$2_PROG).prl
	$$(call removeFiles,$$@)
	echo '#!$$(PERL)'                                  >> $$@
	echo '$$$$TARGETPLATFORM  = "$$(TARGETPLATFORM)";' >> $$@
	echo '$$$$TABLES_NEXT_TO_CODE  = "$(GhcEnableTablesNextToCode)";' >> $$@
	cat $$<                                            >> $$@

$$($1_$2_INPLACE): $1/$2/$$($1_$2_PROG) | $$$$(dir $$$$@)/.
	"$$(CP)" $$< $$@
	$$(EXECUTABLE_FILE) $$@

endif

$(call profEnd, build-perl($1,$2))
endef
