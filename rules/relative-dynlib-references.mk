# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------


# Make dynlib references use relative paths, so that everything works
# without the build tree.

define relative-dynlib-references
# $1 = dir
# $2 = distdir
# $3 = GHC stage to use (0 == bootstrapping compiler)
# $4 = RTSway

ifeq "$$(TargetOS_CPP)" "darwin"
ifneq "$3" "0"
# Use relative paths for all the libraries
ifneq "$$($1_$2_TRANSITIVE_DEP_NAMES)" ""
	install_name_tool $$(foreach d,$$($1_$2_TRANSITIVE_DEP_NAMES), -change $$(TOP)/$$($$($$d_INSTALL_INFO)_dyn_LIB) @rpath/$$d-$$($$($$d_INSTALL_INFO)_VERSION)/$$($$($$d_INSTALL_INFO)_dyn_LIB_NAME)) $$@
endif
# Change absolute library name/path to a relative name/path
ifeq "$$($1_$2_PROGNAME)" ""
ifeq "$1" "rts"
	install_name_tool -id @rpath/rts-$$(rts_VERSION)/$$(rts_$4_LIB_NAME) $$@
else
	install_name_tool -id @rpath/$$($1_PACKAGE)-$$($1_$2_VERSION)/$$($1_$2_dyn_LIB_NAME) $$@
endif
endif
# Use relative paths for the RTS. Rather than try to work out which RTS
# way is being linked, we just change it for all ways
	install_name_tool $$(foreach w,$$(rts_WAYS), -change $$(TOP)/$$(rts_$$w_LIB) @rpath/rts-$$(rts_VERSION)/$$(rts_$$w_LIB_NAME)) $$@
	install_name_tool -change $$(TOP)/$$(wildcard libffi/build/inst/lib/libffi.*.dylib) @rpath/rts-$$(rts_VERSION)/libffi.dylib $$@
endif
endif

endef

define relative-dynlib-path
# $1 = GHC stage to use (0 == bootstrapping compiler)

ifeq "$$(TargetOS_CPP)" "darwin"
ifneq "$1" "0"
	install_name_tool -rpath $$(TOP)/inplace/lib @loader_path/.. $$@
endif
endif

endef
