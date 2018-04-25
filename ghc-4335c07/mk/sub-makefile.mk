# This file is included by all the "stub makefiles" in every directory
# in the tree except the root.  Its job is to invoke $(MAKE) on the
# top-level Makefile, but modifying the target so that it applies to
# the current directory only.
#
# eg.
#  make foo.o  ==>  make -C $(TOP) dir/foo.o
#  make all    ==>  make -C $(TOP) all_dir
#  make clean  ==>  make -C $(TOP) clean_dir
#

# Eliminate use of the built-in implicit rules, and clear out the default list
# of suffixes for suffix rules. Speeds up make quite a bit. Both are needed
# for the shortest `make -d` output.
# Don't set --no-builtin-variables; some rules might stop working if you do
# (e.g. 'make clean' in testsuite/ currently relies on an implicit $RM).
MAKEFLAGS += --no-builtin-rules
.SUFFIXES:

TOPMAKE = $(MAKE) -C $(TOP)

default: all

fast: all

FAST_MAKE_OPTS =\
  $(dir)_dist_NO_BUILD_DEPS=YES \
  $(dir)_dist-boot_NO_BUILD_DEPS=YES \
  $(dir)_dist-install_NO_BUILD_DEPS=YES \
  NO_GENERATED_MAKEFILE_RULES=YES \
  OMIT_PHASE_0=YES OMIT_PHASE_1=YES

ifneq "$(filter fast,$(MAKECMDGOALS))" ""
EXTRA_MAKE_OPTS += $(FAST_MAKE_OPTS)
else
ifeq "$(FAST)" "YES"
EXTRA_MAKE_OPTS += $(FAST_MAKE_OPTS)
endif
endif

# We must not execute multiple recursive invocations of make in parallel.
.NOTPARALLEL:

STD_TARGETS = all clean distclean maintainer_clean install html ps pdf
DIRECTORY_INDEPENDENT_TARGETS = show show!

# The + tells make that we're recursively invoking make, otherwise 'make -j2'
# goes wrong.
$(STD_TARGETS): 
	+$(TOPMAKE) $@_$(dir) $(EXTRA_MAKE_OPTS)

$(DIRECTORY_INDEPENDENT_TARGETS):
	+$(TOPMAKE) $@ $(EXTRA_MAKE_OPTS)

OTHERTARGETS=$(filter-out fast help $(DIRECTORY_INDEPENDENT_TARGETS) $(STD_TARGETS) $(SPEC_TARGETS),$(MAKECMDGOALS))
.PHONY: $(OTHERTARGETS)
$(OTHERTARGETS):
	+$(TOPMAKE) $(dir)/$@ $(EXTRA_MAKE_OPTS)

.PHONY: help
help : sub-help

.PHONY: sub-help
sub-help :
	@echo "You are in subdirectory \"$(dir)\"."
	@echo "Useful targets in this directory:"
	@sed '1,/Using `make` in subdirectories/d' $(TOP)/MAKEHELP.md
