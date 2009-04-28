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


define build-package-way # $1 = dir, $2 = distdir, $3 = way, $4 = stage

$(call distdir-way-opts,$1,$2,$3)
$(call hs-suffix-rules,$1,$2,$3)
$$(foreach dir,$$($1_$2_HS_SRC_DIRS),\
  $$(eval $$(call hs-suffix-rules-srcdir,$1,$2,$3,$$(dir))))

$(call hs-objs,$1,$2,$3)

$1_$2_$3_LIB = $1/$2/build/libHS$$($1_PACKAGE)-$$($1_$2_VERSION)$$($3__way).a

$1_$2_$3_MKSTUBOBJS = find $1/$2/build -name "*_stub.$$($3_osuf)" -print
# HACK ^^^ we tried to use $(wildcard), but apparently it fails due to
# make using cached directory contents, or something.

# Build the ordinary .a library
ifeq "$$($1_$2_SplitObjs)" "YES"
$$($1_$2_$3_LIB) : $$($1_$2_$3_HS_OBJS) $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS)
	$$(RM) $$@
	(echo $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS) `$$($1_$2_$3_MKSTUBOBJS)`; find $$(patsubst %.$$($3_osuf),%_split,$$($1_$2_$3_HS_OBJS)) -name '*.$$($3_osuf)' -print) | xargs $$(AR) $$(EXTRA_AR_ARGS) $$@
else
$$($1_$2_$3_LIB) : $$($1_$2_$3_HS_OBJS) $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS)
	$$(RM) $$@
	echo $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS) $$($1_$2_$3_HS_OBJS) `$$($1_$2_$3_MKSTUBOBJS)` | xargs $$(AR) $$(EXTRA_AR_ARGS) $$@
endif

$(call all-target,$1_$2,all_$1_$2_$3)
$(call all-target,$1_$2_$3,$$($1_$2_$3_LIB))

# Don't put bootstrapping packages in the bindist
ifneq "$4" "0"
BINDIST_HI += $$($1_$2_$3_HI)
BINDIST_LIBS += $$($1_$2_$3_LIB)
endif

# Build the GHCi library
ifeq "$3" "v"
$1_$2_GHCI_LIB = $1/$2/build/HS$$($1_PACKAGE)-$$($1_$2_VERSION).$$($3_osuf)
# Don't put bootstrapping packages in the bindist
ifneq "$4" "0"
BINDIST_LIBS += $$($1_$2_GHCI_LIB)
endif
$$($1_$2_GHCI_LIB) : $$($1_$2_$3_HS_OBJS) $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS)
	$$(RM) $$@
	$$(LD) -r -o $$@ $$(EXTRA_LD_OPTS) $$($1_$2_$3_HS_OBJS) $$($1_$2_v_C_OBJS) $$($1_$2_v_S_OBJS) `$$($1_$2_$3_MKSTUBOBJS)`

$(call all-target,$1_$2,$$($1_$2_GHCI_LIB))
endif

endef

