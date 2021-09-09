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


define build-package-way # $1 = dir, $2 = distdir, $3 = way, $4 = stage
$(call trace, build-package-way($1,$2,$3))
$(call profStart, build-package-way($1,$2,$3))

$(call distdir-way-opts,$1,$2,$3,$4)
$(call hs-suffix-way-rules,$1,$2,$3)

$(call hs-objs,$1,$2,$3)

# The .a/.so library file, indexed by two different sets of vars:
# the first is indexed by the dir, distdir and way
# the second is indexed by the package id, distdir and way
$1_$2_$3_LIB_FILE = libHS$$($1_$2_COMPONENT_ID)$(subst .,%,$$($3_libsuf))
$1_$2_$3_LIB = $1/$2/build/$$($1_$2_$3_LIB_FILE)
$$($1_$2_COMPONENT_ID)_$2_$3_LIB = $$($1_$2_$3_LIB)

# Note [inconsistent distdirs]
#
# hack: the DEPS_LIBS mechanism assumes that the distdirs for packages
# that depend on each other are the same, but that is not the case for
# ghc where we use stage1/stage2 rather than dist/dist-install.
# Really we should use a consistent scheme for distdirs, but in the
# meantime we work around it by defining ghc-<ver>_dist-install_way_LIB:
#
# A similar hack is applied to the PROGRAM_DEP_LIB mechanism in
# rules/build-package.mk.
ifeq "$$($1_PACKAGE) $2" "ghc stage2"
$$($1_$2_COMPONENT_ID)_dist-install_$3_LIB = $$($1_$2_$3_LIB)
endif

# All the .a/.so library file dependencies for this library.
#
# The $(subst stage2,dist-install,..) is needed due to Note
# [inconsistent distdirs].
#
# NB: Use DEP_COMPONENT_IDS for the /directory/ parameter.
$1_$2_$3_DEPS_LIBS=$$(foreach dep,$$($1_$2_DEP_COMPONENT_IDS),$$($$(dep)_$(subst stage2,dist-install,$2)_$3_LIB))

$1_$2_$3_NON_HS_OBJS = $$($1_$2_$3_CMM_OBJS) $$($1_$2_$3_C_OBJS) $$($1_$2_$3_CXX_OBJS)  $$($1_$2_$3_S_OBJS) $$($1_$2_EXTRA_OBJS)
$1_$2_$3_ALL_OBJS = $$($1_$2_$3_HS_OBJS) $$($1_$2_$3_NON_HS_OBJS)

ifeq "$3" "dyn"

# Link a dynamic library
# On windows we have to supply the extra libs this one links to when building it.
ifeq "$$(TargetOS_CPP)" "mingw32"
$$($1_$2_$3_LIB) : $$($1_$2_$3_ALL_OBJS) $$(ALL_RTS_LIBS) $$($1_$2_$3_DEPS_LIBS)
	$$(call build-dll,$1,$2,$3,-L$1/$2/build,,$$($1_$2_$3_HS_OBJS) $$($1_$2_$3_NON_HS_OBJS),"$$@","NO","$$($1_PACKAGE)","$$($1_$2_VERSION)")

else # ifneq "$$(TargetOS_CPP)" "mingw32"
$$($1_$2_$3_LIB) : $$($1_$2_$3_ALL_OBJS) $$(ALL_RTS_LIBS) $$($1_$2_$3_DEPS_LIBS)
	$$(call cmd,$1_$2_HC) $$($1_$2_$3_ALL_HC_OPTS) $$($1_$2_$3_GHC_LD_OPTS) $$($1_$2_$3_ALL_OBJS) \
         -shared -dynamic -dynload deploy \
	 $$(addprefix -l,$$($1_$2_EXTRA_LIBRARIES)) $$(addprefix -L,$$($1_$2_EXTRA_LIBDIRS)) \
         -no-auto-link-packages \
         -o $$@
endif # "$$(TargetOS_CPP)" "mingw32"

else # ifneq "$3" "dyn"

# Build the ordinary .a library
$$($1_$2_$3_LIB) : $$($1_$2_$3_ALL_OBJS)
	$$(call removeFiles,$$@ $$@.contents)
	echo $$($1_$2_$3_ALL_OBJS) >> $$@.contents
ifeq "$$($1_$2_ArSupportsAtFile)" "YES"
	$$(call cmd,$1_$2_AR) $$($1_$2_AR_OPTS) $$($1_$2_EXTRA_AR_ARGS) $$@ @$$@.contents
else
	"$$(XARGS)" $$(XARGS_OPTS) "$$($1_$2_AR)" $$($1_$2_AR_OPTS) $$($1_$2_EXTRA_AR_ARGS) $$@ < $$@.contents
endif
	$$(call removeFiles,$$@.contents)

endif # "$3" "dyn"

$(call all-target,$1_$2,all_$1_$2_$3)
$(call all-target,$1_$2_$3,$$($1_$2_$3_LIB))

# Don't put bootstrapping packages in the bindist
ifneq "$4" "0"
BINDIST_HI += $$($1_$2_$3_HI)
BINDIST_LIBS += $$($1_$2_$3_LIB)
# Need to put the split libs and import libraries here
endif

ifeq "$$($1_$2_SplitSections)" "YES"
ifeq "$(LdIsGNULd)" "YES"
ifeq "$$(HostOS_CPP)" "mingw32"
$1_$2_LD_SCRIPT_CMD =
$1_$2_LD_SCRIPT = driver/utils/merge_sections_pe.ld
else
$1_$2_LD_SCRIPT_CMD = -T
$1_$2_LD_SCRIPT = driver/utils/merge_sections.ld
endif
endif
endif

# Build the GHCi library
# See Note [Merging object files for GHCi] in GHC.Driver.Pipeline.
ifneq "$(filter $3, v p)" ""
$1_$2_$3_GHCI_LIB = $1/$2/build/HS$$($1_$2_COMPONENT_ID).$$($3_osuf)
ifeq "$$($1_$2_BUILD_GHCI_LIB)" "YES"
# Don't put bootstrapping packages in the bindist
ifneq "$4" "0"
BINDIST_LIBS += $$($1_$2_$3_GHCI_LIB)
endif
endif
$$($1_$2_$3_GHCI_LIB) : $$($1_$2_$3_HS_OBJS) $$($1_$2_$3_CMM_OBJS) $$($1_$2_$3_C_OBJS) $$($1_$2_$3_CXX_OBJS) $$($1_$2_$3_S_OBJS) $$($1_$2_EXTRA_OBJS) $$($1_$2_LD_SCRIPT)
	$$(call cmd,MERGE_OBJS_STAGE$4) $(MERGE_OBJS_STAGE$4_FLAGS) $$(if $$($1_$2_LD_SCRIPT),$$($1_$2_LD_SCRIPT_CMD) $$($1_$2_LD_SCRIPT)) -o $$@ $$(EXTRA_LD_LINKER_OPTS) $$($1_$2_$3_HS_OBJS) $$($1_$2_$3_CMM_OBJS) $$($1_$2_$3_C_OBJS) $$($1_$2_$3_CXX_OBJS) $$($1_$2_$3_S_OBJS) $$($1_$2_EXTRA_OBJS)
ifeq "$$($1_$2_BUILD_GHCI_LIB)" "YES"
# Don't bother making ghci libs for bootstrapping packages
ifneq "$4" "0"
$(call all-target,$1_$2,$$($1_$2_$3_GHCI_LIB))
endif
endif # "$$($1_$2_BUILD_GHCI_LIB)" "YES"
endif # "$(filter $3, v p)" ""

$(call profEnd, build-package-way($1,$2,$3))
endef # build-package-way

define build-dll
# Call out to the shell script to decide how to build the util dll.
# 1  = dir
# 2  = distdir
# 3  = way
# 4  = extra flags
# 5  = extra libraries to link
# 6  = object files to link
# 7  = output filename
# 8  = link command
# 9  = create delay load import lib
# 10 = SxS Name
# 11 = SxS Version
$(gen-dll_INPLACE) link "$1" "$2" "$3" "$4" "$5" "$6" "$7" "$(call cmd,$1_$2_HC) $(subst -no-hs-main,,$($1_$2_$3_ALL_HC_OPTS) $($1_$2_$3_GHC_LD_OPTS)) \
           -shared -dynamic -dynload deploy \
           $(addprefix -l,$($1_$2_EXTRA_LIBRARIES)) \
           -no-auto-link-packages" "$8" \
           "$9" "${10}"
endef

