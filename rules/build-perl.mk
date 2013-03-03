# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------


# Build a perl script.  Invoke like this:
#
# driver/mangler_PERL_SRC = ghc-asm.lprl
# driver/mangler_dist_PROG = ghc-asm
#
# $(eval $(call build-perl,driver/mangler,dist))

define build-perl
$(call trace, build-perl($1,$2))
$(call profStart, build-perl($1,$2))
# $1 = dir
# $2 = distdir

ifeq "$$($1_$2_TOPDIR)" "YES"
$1_$2_INPLACE = $$(INPLACE_TOPDIR)/$$($1_$2_PROG)
else
$1_$2_INPLACE = $$(INPLACE_BIN)/$$($1_$2_PROG)
endif

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

ifneq "$$($1_$2_INSTALL_IN)" ""
BINDIST_PERL_SOURCES += $1/$2/$$($1_$2_PROG).prl

install: install_$1_$2

.PHONY: install_$1_$2
install_$1_$2: $1/$2/$$($1_$2_PROG)
	$$(call INSTALL_DIR,"$$($1_$2_INSTALL_IN)")
	$$(call INSTALL_SCRIPT,$$(INSTALL_OPTS),$$<,"$$($1_$2_INSTALL_IN)")
endif
endif

$(call profEnd, build-perl($1,$2))
endef
