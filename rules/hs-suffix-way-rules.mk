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


define hs-suffix-way-rules  # args: $1 = dir,  $2 = distdir, $3 = way, $4 = stage

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

# Note [Implicit rule search algorithm]
#
# The order in which implicit rules are defined can influence a build.
#
# Case study: genprimpos/Lexer.hs
#
# We have two implicit rules for creating .o files, which after instantiating
# with a specific directory ($1=utils/genprimops) and distdir ($2=dist) look
# like this:
#
# utils/genprimops/dist/build/%.o : utils/genprimops/dist/build/%.hs
# 	<recipe1>
# utils/genprimops/dist/build/%.o : utils/genprimops/./%.hs
# 	<recipe2>
#
# The first rule is defined in hs-suffix-way-rules.mk (this file), the other
# in hs-suffix-way-rules-srcdir.mk.
#
# Assume for rest of this story that %=Lexer.
#
# In a normal repository checkout, neither Lexer.hs exists, but we have a rule
# to generate the one in the build directory by running alex on Lexer.x (the
# rule for that operation is defined in hs-suffix-rules-srcdir.mk). Since make
# needs to make a choice which of the above two implicit rules to follow (it
# never runs 2 recipes for the same target, unless double colon rules are
# used, which we don't), logically it will choose the first rule: Lexer.o will
# depend on Lexer.hs in the build directory, that file will be built, and then
# Lexer.o can be built.
#
# In an sdist however, Lexer.hs is present in the source directory. It was
# copied there during the creation of the sdist by a rule in
# sdist-ghc-file.mk. And this time we *don't* know how to generate the
# Lexer.hs in the build directory, because 1) alex is not installed when
# building from sdist and 2) the sdist creation process renamed Lexer.x to
# Lexer.x.source. So normally make would now choose the second rule: Lexer.o
# will depend on Lexer.hs in the source directory, for which nothing needs to
# be done, and then Lexer.o can be built.
#
# There is however another actor in play, a rule in sdist-ghc-file.mk, which
# after after instantiating with the same directory ($1=utils/genprimops) and
# distdir ($2=dist) looks like this:
#
#     sdist_utils/genprimops_dist_Lexer : utils/genprimops/dist/build/Lexer.hs
#
# Note that this is not an implicit rule, there aren't any %'s. This rule
# *explicitly* depends on Lexer.hs in the build directory. What follows is the
# following:
#
#    * make thinks Lexer.hs in the build directory "ought to exist" [1],
#      because it is an explicit dependency of /some/ target.
#
#    * this puts our two implicit rules on equal footing: one depends on a
#      file that exists, the other on a file that ought to exist. Make's
#      implicit rule search algorithm doesn't distinguish between these two
#      cases [1].
#
#    * to break the tie, make chooses the rule that is defined first. Lexer.o
#      will depend on Lexer.hs in the build directory, which doesn't exist,
#      and which make doesn't know how to build, causing a build failure.
#
# To prevent this from happening we define rules for haskell source files in
# the source directory before those in the distdir.
#
# Alternative solutions:
#
#     * Don't include the explicit rule above when not creating an sdist, as
#       that is the only time when it is needed.
#
#     * Merge the two implicit rules, with help from $1_$2_HS_SRCS from
#       hs-sources.mk, which is sdist aware.
#
#     * Require alex and happy to be installed when building from an sdist,
#       simplifying all this drastically.
#
# [1] https://www.gnu.org/software/make/manual/make.html#Implicit-Rule-Search

$$(foreach dir,$$($1_$2_HS_SRC_DIRS),\
  $$(eval $$(call hs-suffix-way-rules-srcdir,$1,$2,$3,$$(dir),$4)))


ifneq "$$(BINDIST)" "YES"

$1/$2/build/%.$$($3_hcsuf) : $1/$2/build/%.hs $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/%.hs $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf),$$(basename $$@)))

$1/$2/build/%.$$($3_hcsuf) : $1/$2/build/$$(or $$($1_EXECUTABLE),$$($1_$2_PROGNAME),.)/autogen/%.hs $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -C $$< -o $$@

$1/$2/build/%.$$($3_osuf) : $1/$2/build/$$(or $$($1_EXECUTABLE),$$($1_$2_PROGNAME),.)/autogen/%.hs $$(LAX_DEPS_FOLLOW) $$$$($1_$2_HC_DEP)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) -c $$< -o $$@ $$(if $$(findstring YES,$$($1_$2_DYNAMIC_TOO)),-dyno $$(addsuffix .$$(dyn_osuf),$$(basename $$@)))

endif


endif

endef # hs-suffix-way-rules

