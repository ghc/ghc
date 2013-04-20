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


define cmm-suffix-rules
# $1 = dir
# $2 = distdir
# $3 = way

# .cmm files depend on all the .h files, to a first approximation.

ifneq "$$(CLEANING)" "YES"

$1/$2/build/%.$$($3_way_)o : $1/%.cmm $$(rts_H_FILES) $$(includes_H_FILES) $$(includes_DERIVEDCONSTANTS) $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP) | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_MOST_HC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_way_)o : $1/$2/build/%.cmm $$(rts_H_FILES) $$(includes_H_FILES) $$(includes_DERIVEDCONSTANTS) $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP) | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_MOST_HC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_way_)hc : $1/%.cmm $$(rts_H_FILES) $$(includes_H_FILES) $$(includes_DERIVEDCONSTANTS) $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP) | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_MOST_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_way_)hc : $1/$2/build/%.cmm $$(rts_H_FILES) $$(includes_H_FILES) $$(includes_DERIVEDCONSTANTS) $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP) | $$$$(dir $$$$@)/.
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_MOST_HC_OPTS) -C $$< -o $$@

# XXX
# When we started using "| $$$$(dir $$$$@)/." for directory deps, these
# rules started getting used when object splitting is enabled for some
# reason. But they fail with
#   **splitmangle**: openBinaryFile: does not exist (No such file or directory)
# so for now they're commented out. They aren't needed, as we can always
# go directly to .o files.
#
# $1/$2/build/%.$$($3_way_)s : $1/%.cmm $$(rts_H_FILES) $$(includes_H_FILES) $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP) | $$$$(dir $$$$@)/.
# 	$$(call cmd,$1_$2_HC) $$($1_$2_$3_MOST_HC_OPTS) -S $$< -o $$@
#
# $1/$2/build/%.$$($3_way_)s : $1/$2/build/%.cmm $$(rts_H_FILES) $$(includes_H_FILES) $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP) | $$$$(dir $$$$@)/.
# 	$$(call cmd,$1_$2_HC) $$($1_$2_$3_MOST_HC_OPTS) -S $$< -o $$@

endif

endef

