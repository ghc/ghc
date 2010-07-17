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

define build-dependencies
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)

$1_$2_depfile_haskell = $$($1_$2_depfile_base).haskell
$1_$2_depfile_c_asm = $$($1_$2_depfile_base).c_asm

$1_$2_C_FILES_DEPS = $$(filter-out $$($1_$2_C_FILES_NODEPS),$$($1_$2_C_FILES))

$1_$2_MKDEPENDHS_FLAGS = -dep-makefile $$($1_$2_depfile_haskell).tmp $$(foreach way,$$(filter-out v,$$($1_$2_WAYS)),-dep-suffix $$(way))
ifneq "$3" "0"
$1_$2_MKDEPENDHS_FLAGS += -include-pkg-deps
endif

ifneq "$$($1_$2_NO_BUILD_DEPS)" "YES"

$$($1_$2_depfile_haskell) : $$($1_$2_HS_SRCS) $$($1_$2_HS_BOOT_SRCS) $$($1_$2_HC_MK_DEPEND_DEP) | $$$$(dir $$$$@)/.
	"$$(RM)" $$(RM_OPTS) $$@.tmp
	touch $$@.tmp
ifneq "$$($1_$2_HS_SRCS)" ""
	"$$($1_$2_HC_MK_DEPEND)" -M $$($1_$2_MKDEPENDHS_FLAGS) \
	    $$(filter-out -split-objs, $$($1_$2_v_ALL_HC_OPTS)) \
	    $$($1_$2_HS_SRCS)
endif
	echo "$1_$2_depfile_haskell_EXISTS = YES" >> $$@.tmp
ifneq "$$($1_$2_SLASH_MODS)" ""
	for dir in $$(sort $$(foreach mod,$$($1_$2_SLASH_MODS),$1/$2/build/$$(dir $$(mod)))); do \
		if test ! -d $$$$dir; then mkdir -p $$$$dir; fi \
	done
endif
	mv $$@.tmp $$@

$$($1_$2_depfile_c_asm) : $$($1_$2_C_FILES_DEPS) $$($1_$2_S_FILES) | $$$$(dir $$$$@)/.
	"$$(RM)" $$(RM_OPTS) $$@.tmp
	touch $$@.tmp
ifneq "$$(strip $$($1_$2_C_FILES_DEPS)$$($1_$2_S_FILES))" ""
# We ought to actually do this for each way in $$($1_$2_WAYS), but then
# it takes a long time to make the C deps for the RTS (30 seconds rather
# than 3), so instead we just pass the list of ways in and let addCFileDeps
# copy the deps for each way on the assumption that they are the same
	$$(foreach f,$$($1_$2_C_FILES_DEPS) $$($1_$2_S_FILES), \
	    $$(call addCFileDeps,$1,$2,$$($1_$2_depfile_c_asm),$$f,$$($1_$2_WAYS)))
	"$$(RM)" $$(RM_OPTS) $$@.bit
endif
	echo "$1_$2_depfile_c_asm_EXISTS = YES" >> $$@.tmp
	mv $$@.tmp $$@

endif # $1_$2_NO_BUILD_DEPS

# Note sed magic above: mkdependC can't do -odir stuff, so we have to
# munge the dependencies it generates to refer to the correct targets.

# Seems as good a place as any to attach the unlit dependency
$$($1_$2_depfile_haskell) : $$(UNLIT)

ifneq "$$(NO_INCLUDE_DEPS)" "YES"
ifneq "$$(strip $$($1_$2_HS_SRCS) $$($1_$2_HS_BOOT_SRCS))" ""
include $$($1_$2_depfile_haskell)
endif
include $$($1_$2_depfile_c_asm)
else
ifeq "$$(DEBUG)" "YES"
$$(warning not building dependencies in $1)
endif
endif

endef

# This comment is outside the "define addCFileDeps" as that definition
# is a list of command lines, and if it is inside it then we pass this
# comment to the shell every time we call the definition.
# $1 = dir
# $2 = distdir
# $3 = depfile
# $4 = file
# $5 = ways
#
# The formatting of this definition (e.g. the blank line above) is
# important, in order to get make to generate the right makefile code.
#
# "1s|\.o|\.$($w_osuf)|"
#    We will have dependencies for .o files, so we need to fix them up
#    for the right object suffix for the way we're doing
# "1s|^|$(dir $4)|"
#    We always get deps for just foo.o when the file we're making is
#    a/b/c/foo.o, so we need to prepend the directory of the source file
# "1s|$1/|$1/$2/build/|"
#    Well, almost. We actually need to insert e.g. "dist/build" in the
#    middle of that directory
# "1s|$2/build/$2/build|$2/build|g"
#    But some source files, e.g. sm/Evac_thr.c, are also inside the
#    "dist/build" directory, so now we've just made
#    "dist/build/dist/build", so we need to remove the duplication
#    again
# "s|$(TOP)/||g$(CASE_INSENSITIVE_SED)"
#    Finally, when making deps for packages like ghc stage2, we have
#    some include paths for packages registered in the in-tree package
#    database. These include paths are full (i.e. not relative) paths,
#    which means that the "cpp -MM" output uses full paths in some cases.
#    This causes 2 problems:
#    * they don't match up with the rules to rebuild the files, where
#      appropriate.
#    * on Windows, make interprets the colon in c:/foo/bar.h as make
#      syntax.
#    So we sed off $(TOP). Unfortunately, on Windows, the case for the
#    drive letter is sometimes different in what $(TOP) starts with, and
#    what the path in the package database starts with. We therefore
#	 need to do the substitution case-insensitively on Windows. But
#    the s///i modifier isn't portable, so we set CASE_INSENSITIVE_SED
#    to "i" on Windows and "" on any other platform.
define addCFileDeps

	$(CPP) $($1_$2_MKDEPENDC_OPTS) $($1_$2_v_ALL_CC_OPTS) $($(basename $4)_CC_OPTS) -MM $4 -MF $3.bit
	$(foreach w,$5,sed -e "1s|\.o|\.$($w_osuf)|" -e "1s|^|$(dir $4)|" -e "1s|$1/|$1/$2/build/|" -e "1s|$2/build/$2/build|$2/build|g" -e "s|$(TOP)/||g$(CASE_INSENSITIVE_SED)" $3.bit >> $3.tmp &&) true
endef

ifeq "$(Windows)" "YES"
CASE_INSENSITIVE_SED = i
else
CASE_INSENSITIVE_SED =
endif

