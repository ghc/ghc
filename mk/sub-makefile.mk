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

# Important, otherwise we get silly built-in rules:
.SUFFIXES:

TOPMAKE = $(MAKE) -C $(TOP)

default: all

fast :
	+$(TOPMAKE) all_$(dir) $(dir)_dist-install_NO_BUILD_DEPS=YES \
	   OMIT_PHASE_1=YES OMIT_PHASE_2=YES OMIT_PHASE_3=YES

# We must not execute multiple recursive invocations of make in parallel.
.NOTPARALLEL:

STD_TARGETS = all clean distclean maintainer_clean install html ps pdf

# The + tells make that we're recursively invoking make, otherwise 'make -j2'
# goes wrong.
$(STD_TARGETS): 
	+$(TOPMAKE) $@_$(dir)

OTHERTARGETS=$(filter-out fast $(STD_TARGETS) $(SPEC_TARGETS),$(MAKECMDGOALS))
.PHONY: $(OTHERTARGETS)
$(OTHERTARGETS):
	+$(TOPMAKE) $(dir)/$@
