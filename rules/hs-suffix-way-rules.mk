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


define hs-suffix-way-rules  # args: $1 = dir,  $2 = distdir, $3 = way

ifeq "$3 $$($1_$2_DYNAMIC_TOO)" "dyn YES"
# We only want this rule to be used for Haskell sources, not for
# e.g. C sources, so we depend on the v_hisuf rather than v_osuf.
$1/$2/build/%.$$(dyn_osuf): $1/$2/build/%.$$(v_hisuf)
	@if [ ! -f $$@ ] ; then \
	    echo "Panic! $$< exists, but $$@ does not."; \
	    exit 1; \
	fi

$1/$2/build/%.$$(dyn_osuf)-boot: $1/$2/build/%.$$(v_hisuf)-boot
	@if [ ! -f $$@ ] ; then \
	    echo "Panic! $$< exists, but $$@ does not."; \
	    exit 1; \
	fi
else

ifneq "$$(BINDIST)" "YES"

$1/$2/build/%.$$($3_hcsuf) : $1/$2/build/%.hs $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.hs $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf),$$(basename $$@)))

$1/$2/build/%.$$($3_hcsuf) : $1/$2/build/autogen/%.hs $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/autogen/%.hs $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf),$$(basename $$@)))

endif

$$(foreach dir,$$($1_$2_HS_SRC_DIRS),\
  $$(eval $$(call hs-suffix-way-rules-srcdir,$1,$2,$3,$$(dir))))

endif

endef # hs-suffix-way-rules

