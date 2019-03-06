libraries/ghc-prim_PACKAGE = ghc-prim
libraries/ghc-prim_dist-install_GROUP = libraries

# ----------------------------------------
# Special magic for the ghc-prim package

# We want the ghc-prim package to include the GHC.Prim module when it
# is registered, but not when it is built, because GHC.Prim is not a
# real source module, it is built-in to GHC.

# Strip it out again before building the package:
define libraries/ghc-prim_PACKAGE_MAGIC
libraries/ghc-prim_dist-install_MODULES := $$(filter-out GHC.Prim,$$(libraries/ghc-prim_dist-install_MODULES))
endef

PRIMOPS_TXT_STAGE1 = libraries/ghc-prim/dist-boot/build/primops.txt

libraries/ghc-prim/dist-install/build/GHC/PrimopWrappers.hs : $$(genprimopcode_INPLACE) $(PRIMOPS_TXT_STAGE1) | $$(dir $$@)/.
	"$(genprimopcode_INPLACE)" --make-haskell-wrappers < $(PRIMOPS_TXT_STAGE1) >$@

# Required so that Haddock documents the primops.
libraries/ghc-prim_dist-install_EXTRA_HADDOCK_SRCS = libraries/ghc-prim/dist-install/build/autogen/GHC/Prim.hs


# -----------------------------------------------------------------------------
# Create platform includes

# Here we generate a little header file containing CPP symbols that GHC
# uses to determine which platform it is building on/for.  The platforms
# can differ between stage1 and stage2 if we're cross-compiling, so we
# need one of these header files per stage.

PLATFORM_H = ghc_boot_platform.h

libraries/ghc-prim/dist-boot/$(PLATFORM_H) : mk/config.mk mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo "Creating $@..."
	@echo "#ifndef __PLATFORM_H__"                           >> $@
	@echo "#define __PLATFORM_H__"                           >> $@
	@echo                                                    >> $@
	@echo "#define BuildPlatform_NAME  \"$(BUILDPLATFORM)\""  >> $@
	@echo "#define HostPlatform_NAME   \"$(HOSTPLATFORM)\""   >> $@
	@echo "#define TargetPlatform_NAME \"$(TARGETPLATFORM)\"" >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildPlatform_CPP)_BUILD 1"              >> $@
	@echo "#define $(HostPlatform_CPP)_HOST 1"                >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET 1"            >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildArch_CPP)_BUILD_ARCH 1"             >> $@
	@echo "#define $(HostArch_CPP)_HOST_ARCH 1"               >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH 1"           >> $@
	@echo "#define BUILD_ARCH \"$(BuildArch_CPP)\""           >> $@
	@echo "#define HOST_ARCH \"$(HostArch_CPP)\""             >> $@
	@echo "#define TARGET_ARCH \"$(TargetArch_CPP)\""         >> $@
	@echo "#define LLVM_TARGET \"$(LLVMTarget_CPP)\""         >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildOS_CPP)_BUILD_OS 1"                 >> $@
	@echo "#define $(HostOS_CPP)_HOST_OS 1"                   >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS 1"               >> $@
	@echo "#define BUILD_OS \"$(BuildOS_CPP)\""               >> $@
	@echo "#define HOST_OS \"$(HostOS_CPP)\""                 >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\""             >> $@
	@echo                                                     >> $@
	@echo "#define $(BuildVendor_CPP)_BUILD_VENDOR 1"         >> $@
	@echo "#define $(HostVendor_CPP)_HOST_VENDOR 1"           >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1"      >> $@
	@echo "#define BUILD_VENDOR \"$(BuildVendor_CPP)\""       >> $@
	@echo "#define HOST_VENDOR \"$(HostVendor_CPP)\""         >> $@
	@echo "#define TARGET_VENDOR \"$(TargetVendor_CPP)\""     >> $@
	@echo                                                     >> $@
	@echo "#endif /* __PLATFORM_H__ */"                       >> $@
	@echo "Done."

# For stage2 and above, the BUILD platform is the HOST of stage1, and
# the HOST platform is the TARGET of stage1.  The TARGET remains the same
# (stage1 is the cross-compiler, not stage2).
libraries/ghc-prim/dist-install/$(PLATFORM_H) : mk/config.mk mk/project.mk | $$(dir $$@)/.
	$(call removeFiles,$@)
	@echo "Creating $@..."
	@echo "#ifndef __PLATFORM_H__"                            >> $@
	@echo "#define __PLATFORM_H__"                            >> $@
	@echo                                                     >> $@
	@echo "#define BuildPlatform_NAME  \"$(HOSTPLATFORM)\""   >> $@
	@echo "#define HostPlatform_NAME   \"$(TARGETPLATFORM)\"" >> $@
	@echo "#define TargetPlatform_NAME \"$(TARGETPLATFORM)\"" >> $@
	@echo                                                     >> $@
	@echo "#define $(HostPlatform_CPP)_BUILD 1"               >> $@
	@echo "#define $(TargetPlatform_CPP)_HOST 1"              >> $@
	@echo "#define $(TargetPlatform_CPP)_TARGET 1"            >> $@
	@echo                                                     >> $@
	@echo "#define $(HostArch_CPP)_BUILD_ARCH 1"              >> $@
	@echo "#define $(TargetArch_CPP)_HOST_ARCH 1"             >> $@
	@echo "#define $(TargetArch_CPP)_TARGET_ARCH 1"           >> $@
	@echo "#define BUILD_ARCH \"$(HostArch_CPP)\""            >> $@
	@echo "#define HOST_ARCH \"$(TargetArch_CPP)\""           >> $@
	@echo "#define TARGET_ARCH \"$(TargetArch_CPP)\""         >> $@
	@echo "#define LLVM_TARGET \"$(LLVMTarget_CPP)\""         >> $@
	@echo                                                     >> $@
	@echo "#define $(HostOS_CPP)_BUILD_OS 1"                  >> $@
	@echo "#define $(TargetOS_CPP)_HOST_OS 1"                 >> $@
	@echo "#define $(TargetOS_CPP)_TARGET_OS 1"               >> $@
	@echo "#define BUILD_OS \"$(HostOS_CPP)\""                >> $@
	@echo "#define HOST_OS \"$(TargetOS_CPP)\""               >> $@
	@echo "#define TARGET_OS \"$(TargetOS_CPP)\""             >> $@
	@echo                                                     >> $@
	@echo "#define $(HostVendor_CPP)_BUILD_VENDOR 1"          >> $@
	@echo "#define $(TargetVendor_CPP)_HOST_VENDOR 1"         >> $@
	@echo "#define $(TargetVendor_CPP)_TARGET_VENDOR  1"      >> $@
	@echo "#define BUILD_VENDOR \"$(HostVendor_CPP)\""        >> $@
	@echo "#define HOST_VENDOR \"$(TargetVendor_CPP)\""       >> $@
	@echo "#define TARGET_VENDOR \"$(TargetVendor_CPP)\""     >> $@
	@echo                                                     >> $@
	@echo "#endif /* __PLATFORM_H__ */"                       >> $@
	@echo "Done."

# ----------------------------------------------------------------------------
#		Generate supporting stuff for prelude/PrimOp.hs
#		from prelude/primops.txt

PRIMOP_BITS_NAMES = primop-data-decl.hs-incl        \
                    primop-tag.hs-incl              \
                    primop-list.hs-incl             \
                    primop-has-side-effects.hs-incl \
                    primop-out-of-line.hs-incl      \
                    primop-commutable.hs-incl       \
                    primop-code-size.hs-incl        \
                    primop-can-fail.hs-incl         \
                    primop-strictness.hs-incl       \
                    primop-fixity.hs-incl           \
                    primop-primop-info.hs-incl      \
                    primop-vector-uniques.hs-incl   \
                    primop-vector-tys.hs-incl       \
                    primop-vector-tys-exports.hs-incl \
                    primop-vector-tycons.hs-incl

PRIMOP_BITS_STAGE1 = $(addprefix libraries/ghc-prim/dist-boot/,$(PRIMOP_BITS_NAMES))
PRIMOP_BITS_STAGE2 = $(addprefix libraries/ghc-prim/dist-install/,$(PRIMOP_BITS_NAMES))
PRIMOP_BITS_STAGE3 = $(addprefix libraries/ghc-prim/dist-install/,$(PRIMOP_BITS_NAMES))

compiler_CPP_OPTS += $(addprefix -I,$(GHC_INCLUDE_DIRS))
compiler_CPP_OPTS += ${GhcCppOpts}

# We add these paths to the Haskell compiler's #include search path list since
# we must avoid #including files by paths relative to the source file as Hadrian
# moves the build artifacts out of the source tree. See #8040.
compiler_HC_OPTS += $(addprefix -I,$(GHC_INCLUDE_DIRS))

define preprocessCompilerFiles
# $0 = stage
libraries/ghc-prim/$1/primops.txt: libraries/ghc-prim/primops.txt.pp libraries/ghc-prim/$1/$$(PLATFORM_H)
	$$(HS_CPP) -P $$(compiler_CPP_OPTS) -Ilibraries/ghc-prim/$1 -x c $$< | grep -v '^#pragma GCC' > $$@

libraries/ghc-prim/$1/primop-data-decl.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --data-decl          < $$< > $$@
libraries/ghc-prim/$1/primop-tag.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-tag         < $$< > $$@
libraries/ghc-prim/$1/primop-list.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-list        < $$< > $$@
libraries/ghc-prim/$1/primop-has-side-effects.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --has-side-effects   < $$< > $$@
libraries/ghc-prim/$1/primop-out-of-line.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --out-of-line        < $$< > $$@
libraries/ghc-prim/$1/primop-commutable.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --commutable         < $$< > $$@
libraries/ghc-prim/$1/primop-code-size.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --code-size          < $$< > $$@
libraries/ghc-prim/$1/primop-can-fail.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --can-fail           < $$< > $$@
libraries/ghc-prim/$1/primop-strictness.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --strictness         < $$< > $$@
libraries/ghc-prim/$1/primop-fixity.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --fixity             < $$< > $$@
libraries/ghc-prim/$1/primop-primop-info.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-primop-info < $$< > $$@
libraries/ghc-prim/$1/primop-vector-uniques.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-vector-uniques     < $$< > $$@
libraries/ghc-prim/$1/primop-vector-tys.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-vector-tys         < $$< > $$@
libraries/ghc-prim/$1/primop-vector-tys-exports.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-vector-tys-exports < $$< > $$@
libraries/ghc-prim/$1/primop-vector-tycons.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --primop-vector-tycons      < $$< > $$@

# Usages aren't used any more; but the generator
# can still generate them if we want them back
libraries/ghc-prim/$1/primop-usage.hs-incl: libraries/ghc-prim/$1/primops.txt $$$$(genprimopcode_INPLACE)
	"$$(genprimopcode_INPLACE)" --usage              < $$< > $$@

endef

$(eval $(call preprocessCompilerFiles,dist-boot))
$(eval $(call preprocessCompilerFiles,dist-install))


$(if $(filter ghc-prim,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/ghc-prim,dist-boot,0)))
$(if $(filter ghc-prim,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/ghc-prim,dist-install,1)))
$(if $(filter ghc-prim,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/ghc-prim,dist-install,2)))
