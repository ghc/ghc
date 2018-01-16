
ifneq "$(libraries/dph_NOT_NEEDED)" "YES"

DPH_DIR  = libraries/dph

# -----------------------------------------------------------------------------
# Backend packages.
#
# These are the packages that we want to compile and install by default.
# This doesn't include the dph-examples, dph-test or dph-buildbot, as most
# end users won't want these.
#
DPH_PACKAGES = \
	dph-base \
	dph-prim-interface \
	dph-prim-seq \
	dph-prim-par \
	dph-lifted-base \
	dph-lifted-boxed \
	dph-lifted-copy \
	dph-lifted-vseg 


# -----------------------------------------------------------------------------
# Cleaning rules.
#
# Generate rules to clean each of the DPH_PACKAGES.
#
.PHONY: all_$(DPH_DIR)
all_$(DPH_DIR)   : $(foreach pkg, $(DPH_PACKAGES), all_$(DPH_DIR)/$(pkg))

clean : clean_$(DPH_DIR)
.PHONY: clean_$(DPH_DIR)
clean_$(DPH_DIR) : $(foreach pkg, $(DPH_PACKAGES), clean_$(DPH_DIR)/$(pkg))
distclean        : clean_$(DPH_DIR)

define dph_package
# $1 = package name
ifneq "$(CLEANING)" "YES"
.PHONY: $(DPH_DIR)/$1
$(DPH_DIR)/$1 : all_$(DPH_DIR)/$1
endif
endef

$(foreach pkg, $(DPH_PACKAGES), $(eval $(call dph_package,$(pkg))))


# -----------------------------------------------------------------------------
# Template Haskell dependencies.
# 
# The dph-lifted-copy package contains some Template Haskell code in TH.Repr.
# When compiling modules that use TH.Repr, we will try to run some TH,
# which means using the vanilla TH.Repr object files. If we are not
# building in the vanilla way then we need to be sure that the vanilla
# object files exist. These deps take care of that for us.
#
define dph_th_deps
# $1 = way
ifeq "$1" "$${GHCI_WAY}"
# Do nothing: This way is the GHCi way
else ifeq "$1 $${GHCI_WAY} $${DYNAMIC_TOO}" "v dyn YES"
# Do nothing: This way is built at the same time as the GHCi way
else ifeq "$1 $${GHCI_WAY} $${DYNAMIC_TOO}" "dyn v YES"
# Do nothing: This way is built at the same time as the GHCi way
else
libraries/dph/dph-lifted-copy/dist-install/build/Data/Array/Parallel/Lifted/TH/Repr.$$($1_osuf): libraries/dph/dph-lifted-copy/dist-install/build/Data/Array/Parallel/Lifted/TH/Repr.$${$${GHCI_WAY}_osuf}
libraries/dph/dph-lifted-copy/dist-install/build/Data/Array/Parallel/Lifted/PArray.$${$1_osuf} : libraries/dph/dph-lifted-copy/dist-install/build/Data/Array/Parallel/Lifted/PArray.$${$${GHCI_WAY}_osuf}
endif

# The following modules use Template Haskell, or contain ANN pragmas. Both of
# these features use compile-time evaluation. During this evaluation we may
# need to load the dph-prim-* packages, but if they haven't been build yet the
# compilation will die. This results in a build race, where the compilation
# will succeed or not depending on whether another make thread has already
# completed building the dph-prim-* packages.
#
# Note that the GHC build system does NOT respect the package dependencies
# present in .cabal files. Even though the dph-lifted-*.cabal files list
# the dph-prim-* packages as dependencies, these dependencies are silently
# ignored.
#
# The hack-around is to add the following explicit dependencies:
# The .o for every module that uses Template Haskell or annotations must
# must depend on the dph-prim-* GHCI libraries, so that they can be 
# loaded at compile time.
# 
# If the dependencies are wrong you will get a build race that can result in 
# the following error:
# 
#   "inplace/bin/ghc-stage2"  ... -o .../Data/Array/Parallel/Lifted/PArray.dyn_o
#    Loading package dph-prim-seq-0.4.0 ... linking ... done.
#    Loading package dph-prim-par-0.4.0 ... <command line>: can't load .so/.DLL for: HSdph-prim-par-0.4.0-ghc6.13.20091222
#       (libHSdph-prim-seq-0.4.0-ghc6.13.20091222.so: cannot open shared object file: No such file or directory)
#

# -- modules with TH or ANN in dph-lifted-copy
libraries/dph/dph-lifted-copy/dist-install/build/Data/Array/Parallel/Lifted/TH/Repr.$$($1_osuf): \
	$$(libraries/dph/dph-base_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-par_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-seq_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-interface_dist-install_GHCI_LIB)

libraries/dph/dph-lifted-copy/dist-install/build/Data/Array/Parallel/PArray/PData.$${$1_osuf} : \
	$$(libraries/dph/dph-base_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-par_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-seq_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-interface_dist-install_GHCI_LIB)

libraries/dph/dph-lifted-copy/dist-install/build/Data/Array/Parallel/PArray/Base.$${$1_osuf} : \
	$$(libraries/dph/dph-base_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-par_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-seq_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-interface_dist-install_GHCI_LIB)

# -- modules with TH or ANN in dph-lifted-vseg
libraries/dph/dph-lifted-vseg/dist-install/build/Data/Array/Parallel/PArray/PData/Base.$${$1_osuf} : \
	$$(libraries/dph/dph-base_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-par_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-seq_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-prim-interface_dist-install_GHCI_LIB) \
	$$(libraries/dph/dph-lifted-base_dist-install_GHCI_LIB)

ifeq "$$(libraries/dph/dph-base_dist-install_GHCI_LIB)" ""
ifeq "$(GhcProfiled)" "YES"
$$(error Cannot build profiled GHC with DPH; try deleting libraries/dph and trying again)
endif
$$(error dph_th_deps($1): libraries/dph/dph-base_dist-install_GHCI_LIB not defined!)
endif

endef

# Instantiate the above dph_th_deps definition for each build way.
ifneq "$(CLEANING)" "YES"
$(foreach way, $(GhcLibWays), $(eval $(call dph_th_deps,$(way))))
endif

endif

