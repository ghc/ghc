# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------


define cxx-suffix-rules
# $1 = dir
# $2 = distdir
# $3 = way
# $4 = use GHC (YES/NO)

ifneq "$$(BINDIST)" "YES"

ifeq "$4" "YES"

$1/$2/build/%.$$($3_osuf) : $1/%.cpp $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP) | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.cpp $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_GHC_CC_OPTS) -c $$< -o $$@


else

$1/$2/build/%.$$($3_osuf) : $1/%.cpp | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_CC) $$($1_$2_$3_ALL_CC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.cpp
	$$(call cmd,$1_$2_CC) $$($1_$2_$3_ALL_CC_OPTS) -c $$< -o $$@

endif

endif

endef

