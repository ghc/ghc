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


define hs-suffix-rules-srcdir
# args: $1 = dir,  $2 = distdir, $3 = srcdir

# Preprocessing Haskell source

ifneq "$$(BINDIST)" "YES"

$1/$2/build/%.hs : $1/$3/%.ly | $$$$(dir $$$$@)/.
	$$(call cmd,HAPPY) $$($1_$2_ALL_HAPPY_OPTS) $$< -o $$@

$1/$2/build/%.hs : $1/$3/%.y | $$$$(dir $$$$@)/.
	$$(call cmd,HAPPY) $$($1_$2_ALL_HAPPY_OPTS) $$< -o $$@

$1/$2/build/%.hs : $1/$2/build/%.ly | $$$$(dir $$$$@)/.
	$$(call cmd,HAPPY) $$($1_$2_ALL_HAPPY_OPTS) $$< -o $$@

$1/$2/build/%.hs : $1/$2/build/%.y | $$$$(dir $$$$@)/.
	$$(call cmd,HAPPY) $$($1_$2_ALL_HAPPY_OPTS) $$< -o $$@

$1/$2/build/%.hs : $1/$3/%.x | $$$$(dir $$$$@)/.
	$$(call cmd,ALEX) $$($1_$2_ALL_ALEX_OPTS) $$< -o $$@

$1/$2/build/%_hsc.c $1/$2/build/%_hsc.h $1/$2/build/%.hs : $1/$3/%.hsc $$(HSC2HS_INPLACE) | $$$$(dir $$$$@)/.
	$$(call cmd,HSC2HS_INPLACE) $$($1_$2_ALL_HSC2HS_OPTS) $$< -o $$@

# Now the rules for hs-boot files.

$1/$2/build/%.hs-boot : $1/$3/%.hs-boot
	"$$(CP)" $$< $$@

$1/$2/build/%.lhs-boot : $1/$3/%.lhs-boot
	"$$(CP)" $$< $$@

endif

endef

