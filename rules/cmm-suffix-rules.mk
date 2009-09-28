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

ifneq "$$(BootingFromHc)" "YES"

$1/$2/build/%.$$($3_way_)o : $1/%.cmm $$(rts_H_FILES) $$($1_$2_HC_DEP)
	"$$(MKDIRHIER)" $$(dir $$@)
	"$$($1_$2_HC)" $$($1_$2_$3_MOST_HC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_way_)o : $1/$2/build/%.cmm $$(rts_H_FILES) $$($1_$2_HC_DEP)
	"$$(MKDIRHIER)" $$(dir $$@)
	"$$($1_$2_HC)" $$($1_$2_$3_MOST_HC_OPTS) -c $$< -o $$@

$1/$2/build/%.$$($3_way_)hc : $1/%.cmm $$(rts_H_FILES) $$($1_$2_HC_DEP)
	"$$(MKDIRHIER)" $$(dir $$@)
	"$$($1_$2_HC)" $$($1_$2_$3_MOST_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_way_)hc : $1/$2/build/%.cmm $$(rts_H_FILES) $$($1_$2_HC_DEP)
	"$$(MKDIRHIER)" $$(dir $$@)
	"$$($1_$2_HC)" $$($1_$2_$3_MOST_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_way_)s : $1/%.cmm $$(rts_H_FILES) $$($1_$2_HC_DEP)
	"$$(MKDIRHIER)" $$(dir $$@)
	"$$($1_$2_HC)" $$($1_$2_$3_MOST_HC_OPTS) -S $$< -o $$@

$1/$2/build/%.$$($3_way_)s : $1/$2/build/%.cmm $$(rts_H_FILES) $$($1_$2_HC_DEP)
	"$$(MKDIRHIER)" $$(dir $$@)
	"$$($1_$2_HC)" $$($1_$2_$3_MOST_HC_OPTS) -S $$< -o $$@

endif

endif

endef

