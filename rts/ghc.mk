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

# -----------------------------------------------------------------------------
# Building the RTS

# We build the RTS with stage 1
rts_dist-install_HC = $(GHC_STAGE1)

rts_INSTALL_INFO = rts
rts_VERSION = 1.0.2

# Minimum supported Windows version.
# These numbers can be found at:
#  https://msdn.microsoft.com/en-us/library/windows/desktop/aa383745(v=vs.85).aspx
# If we're compiling on windows, enforce that we only support Windows 7+
# Adding this here means it doesn't have to be done in individual .c files
# and also centralizes the versioning.
rts_WINVER = 0x06010000

# merge GhcLibWays and GhcRTSWays but strip out duplicates
rts_WAYS = $(GhcLibWays) $(filter-out $(GhcLibWays),$(GhcRTSWays))
rts_dist-install_WAYS = $(rts_WAYS)

ALL_RTS_LIBS = $(foreach way,$(rts_WAYS),rts/dist-install/build/libHSrts$($(way)_libsuf))
$(eval $(call all-target,rts,$(ALL_RTS_LIBS)))

# -----------------------------------------------------------------------------
# Defining the sources

ALL_DIRS = hooks sm eventlog linker linker/macho

ifeq "$(TargetOS_CPP)" "mingw32"
ALL_DIRS += win32
else
ALL_DIRS += posix
endif

tmp_rts_C_SRCS := $(wildcard rts/*.c $(foreach dir,$(ALL_DIRS),rts/$(dir)/*.c))
# We shouldn't include this file in the binary, it's a common function so
# it will likely clash with something later. See #19948
rts_C_SRCS = $(filter-out rts/xxhash.c, $(tmp_rts_C_SRCS))
rts_C_HOOK_SRCS := $(wildcard rts/hooks/*.c)
rts_CMM_SRCS := $(wildcard rts/*.cmm)

# Don't compile .S files when bootstrapping a new arch
ifneq "$(PORTING_HOST)" "YES"

# unregisterised builds use the mini interpreter
ifneq "$(GhcUnregisterised)" "YES"
rts_S_SRCS += rts/StgCRunAsm.S
endif

# select adjustor implementation. This much match the logic in rts.cabal.in.
ifneq "$(CLEANING)" "YES"
# N.B. we don't source config.mk when CLEANING=YES so none of the below
# variables will be set. See #20166.
ifeq "$(UseLibffiForAdjustors)" "YES"
rts_C_SRCS += rts/adjustor/LibffiAdjustor.c
else
ifneq "$(findstring $(TargetArch_CPP), i386)" ""
rts_S_SRCS += rts/AdjustorAsm.S
rts_C_SRCS += rts/adjustor/Nativei386.c
else
ifneq "$(findstring $(TargetArch_CPP), x86_64)" ""
rts_C_SRCS += rts/adjustor/NativeAmd64.c
else
ifneq "$(findstring $(TargetArch_CPP), powerpc64le powerpc)" ""
rts_S_SRCS += rts/AdjustorAsm.S
rts_C_SRCS += rts/adjustor/NativePowerPC.c
else
$(error Target architecture has no native adjustor implementation)
endif
endif
endif
endif
endif
endif

ifeq "$(GhcUnregisterised)" "YES"
GENAPPLY_OPTS = -u
endif

rts_AUTO_APPLY_CMM = rts/dist-install/build/AutoApply.cmm

$(rts_AUTO_APPLY_CMM): $$(genapply_INPLACE)
	"$(genapply_INPLACE)" >$@


rts_H_FILES := $(wildcard rts/*.h rts/*/*.h)

ifeq "$(USE_DTRACE)" "YES"
DTRACEPROBES_H = rts/dist-install/build/include/RtsProbes.h
rts_H_FILES += $(DTRACEPROBES_H)
endif

# collect the -l and -L flags that we need to link the rts dyn lib.
# Note that, as sed on OS X doesn't handle \+, we use [^ ][^ ]* rather
# than [^ ]\+
rts/dist-install/libs.depend : $$(ghc-pkg_INPLACE) | $$(dir $$@)/.
	"$(ghc-pkg_INPLACE)" --simple-output field rts extra-libraries \
	  | sed -e 's/\([^ ][^ ]*\)/-l\1/g' > $@
	"$(ghc-pkg_INPLACE)" --simple-output field rts library-dirs \
	  | sed -e 's/\([^ ][^ ]*\)/-L\1/g' >> $@


# ----------------------------------------------------------------------------
# On Windows, as the RTS and base libraries have recursive imports,
# 	we have to break the loop with "import libraries".
# 	These are made from rts/win32/libHS*.def which contain lists of
# 	all the symbols in those libraries used by the RTS.
#
ifeq "$(TargetOS_CPP)" "mingw32"

ALL_RTS_DEF_LIBNAMES 	= base ghc-prim
ALL_RTS_DEF_LIBS	= \
	rts/dist-install/build/win32/libHSbase.dll.a \
	rts/dist-install/build/win32/libHSghc-prim.dll.a

# -- import libs for the regular Haskell libraries
define make-importlib-def # args $1 = lib name
rts/dist-install/build/win32/libHS$1.def : rts/win32/libHS$1.def
	cat rts/win32/libHS$1.def \
		| sed "s/@LibVersion@/$$(libraries/$1_dist-install_VERSION)/" \
		| sed "s/@ProjectVersion@/$$(ProjectVersion)/" \
		> rts/dist-install/build/win32/libHS$1.def

rts/dist-install/build/win32/libHS$1.dll.a : rts/dist-install/build/win32/libHS$1.def
	"$$(DLLTOOL)" 	-d rts/dist-install/build/win32/libHS$1.def \
			-l rts/dist-install/build/win32/libHS$1.dll.a
endef
$(foreach lib,$(ALL_RTS_DEF_LIBNAMES),$(eval $(call make-importlib-def,$(lib))))
endif

ifneq "$(BINDIST)" "YES"
ifneq "$(UseSystemLibFFI)" "YES"
ifeq "$(TargetOS_CPP)" "mingw32"
rts/dist-install/build/$(LIBFFI_DLL): libffi/build/inst/bin/$(LIBFFI_DLL)
	cp $< $@
else
# This is a little hacky. We don't know the SO version, so we only
# depend on libffi.so, but copy libffi.so*
rts/dist-install/build/lib$(LIBFFI_NAME)$(soext): libffi/build/inst/lib/lib$(LIBFFI_NAME)$(soext)
	cp libffi/build/inst/lib/lib$(LIBFFI_NAME)$(soext)* rts/dist-install/build
ifeq "$(TargetOS_CPP)" "darwin"
	install_name_tool -id @rpath/lib$(LIBFFI_NAME)$(soext) rts/dist-install/build/lib$(LIBFFI_NAME)$(soext)
endif
endif
endif
endif


ifeq "$(USE_DTRACE)" "YES"
ifneq "$(findstring $(TargetOS_CPP), linux solaris2 freebsd)" ""
NEED_DTRACE_PROBES_OBJ = YES
endif
endif

#-----------------------------------------------------------------------------
# Building one way
define build-rts-way # args: $1 = way

ifneq "$$(BINDIST)" "YES"

rts_dist-install_$1_HC_OPTS := $$(GhcRtsHcOpts)
rts_dist-install_$1_CC_OPTS := $$(GhcRtsCcOpts)

# The per-way CC_OPTS
ifneq "$$(findstring debug, $1)" ""
rts_dist-install_$1_HC_OPTS += -O0
rts_dist-install_$1_CC_OPTS += -fno-omit-frame-pointer -O0 -g3
# Useful to ensure that inline functions can be called within GDB but not
# supported by clang
#rts_dist-install_$1_CC_OPTS += -fkeep-inline-functions
endif

ifneq "$$(findstring dyn, $1)" ""
ifeq "$$(TargetOS_CPP)" "mingw32"
rts_dist-install_$1_CC_OPTS += -DCOMPILING_WINDOWS_DLL
endif
rts_dist-install_$1_CC_OPTS += -DDYNAMIC
endif


$(call distdir-way-opts,rts,dist-install,$1,1) # 1 because the rts is built with stage1
$(call c-suffix-rules,rts,dist-install,$1,YES)
$(call cmm-suffix-rules,rts,dist-install,$1)

rts_$1_LIB_FILE = libHSrts$$($1_libsuf)
rts_$1_LIB = rts/dist-install/build/$$(rts_$1_LIB_FILE)

rts_$1_C_OBJS      = $$(patsubst rts/%.c,rts/dist-install/build/%.$$($1_osuf),$$(rts_C_SRCS)) $$(patsubst %.c,%.$$($1_osuf),$$(rts_$1_EXTRA_C_SRCS))
rts_$1_CXX_OBJS    = $$(patsubst rts/%.c,rts/dist-install/build/%.$$($1_osuf),$$(rts_CXX_SRCS))
rts_$1_C_HOOK_OBJS = $$(patsubst rts/hooks/%.c,rts/dist-install/build/hooks/%.$$($1_osuf),$$(rts_C_HOOK_SRCS))
rts_$1_S_OBJS      = $$(patsubst rts/%.S,rts/dist-install/build/%.$$($1_osuf),$$(rts_S_SRCS))
rts_$1_CMM_OBJS    = $$(patsubst rts/%.cmm,rts/dist-install/build/%.$$($1_osuf),$$(rts_CMM_SRCS)) $$(patsubst %.cmm,%.$$($1_osuf),$$(rts_AUTO_APPLY_CMM))

rts_$1_OBJS = $$(rts_$1_C_OBJS) $$(rts_$1_CXX_OBJS) $$(rts_$1_S_OBJS) $$(rts_$1_CMM_OBJS)

ifeq "$(USE_DTRACE)" "YES"
ifeq "$(NEED_DTRACE_PROBES_OBJ)" "YES"
# On Darwin we don't need to generate binary containing probes defined
# in DTrace script, but DTrace on Solaris expects generation of binary
# from the DTrace probes definitions
rts_$1_DTRACE_OBJS = rts/dist-install/build/RtsProbes.$$($1_osuf)

$$(rts_$1_DTRACE_OBJS) : $$(rts_$1_OBJS)
	$(DTRACE) -G -C \
		$$(addprefix -Irts/,$$(rts_dist-install_DIST_INCLUDE_DIRS)) \
		-DDTRACE -s rts/RtsProbes.d \
		-o $$@ \
		$$(rts_$1_OBJS)
endif
endif

rts_dist-install_$1_CC_OPTS += -DRtsWay=\"rts_$1\"

# If we're compiling on windows, enforce that we only support XP+
# Adding this here means it doesn't have to be done in individual .c files
# and also centralizes the versioning.
ifeq "$$(TargetOS_CPP)" "mingw32"
rts_dist-install_$1_CC_OPTS += -D_WIN32_WINNT=$(rts_WINVER)
endif

ifneq "$$(UseSystemLibFFI)" "YES"
rts_dist-install_FFI_SO = rts/dist-install/build/lib$$(LIBFFI_NAME)$$(soext)
else
rts_dist-install_FFI_SO =
endif

# Making a shared library for the RTS.
ifneq "$$(findstring dyn, $1)" ""
ifeq "$$(TargetOS_CPP)" "mingw32"
$$(rts_$1_LIB) : $$(rts_$1_OBJS) $$(ALL_RTS_DEF_LIBS) rts/dist-install/libs.depend rts/dist-install/build/$$(LIBFFI_DLL)
	"$$(RM)" $$(RM_OPTS) $$@
	# Call out to the shell script to decide how to build the dll.
	# Making a shared library for the RTS.
	# $$1  = dir
	# $$2  = distdir
	# $$3  = way
	# $$4  = extra flags
	# $$5  = extra libraries to link
	# $$6  = object files to link
	# $$7  = output filename
	# $$8  = link command
	# $$9  = create delay load import lib
	# $$10 = SxS Name
	# $$11 = SxS Version
	$$(gen-dll_INPLACE) link "rts/dist-install/build" "rts/dist-install/build" "" "" "$$(ALL_RTS_DEF_LIBS)" "$$(rts_$1_OBJS)" "$$@" "$$(rts_dist-install_HC) -this-unit-id rts -no-hs-main -shared -dynamic -dynload deploy \
         -no-auto-link-packages -Lrts/dist-install/build -l$$(LIBFFI_NAME) \
         `cat rts/dist-install/libs.depend | tr '\n' ' '` \
         $$(rts_dist-install_$1_GHC_LD_OPTS)" "NO" \
         "$(rts_INSTALL_INFO)-$(subst dyn,,$(subst _dyn,,$(subst v,,$1)))" "$(ProjectVersion)"

else
ifneq "$$(UseSystemLibFFI)" "YES"
LIBFFI_LIBS = -Lrts/dist-install/build -l$$(LIBFFI_NAME)
ifeq "$$(TargetElf)" "YES"
LIBFFI_LIBS += -optl-Wl,-rpath -optl-Wl,'$$$$ORIGIN' -optl-Wl,-zorigin
endif
ifeq "$(TargetOS_CPP)" "darwin"
LIBFFI_LIBS += -optl-Wl,-rpath -optl-Wl,@loader_path
endif

else
# flags will be taken care of in rts/dist-install/libs.depend
LIBFFI_LIBS =
endif
$$(rts_$1_LIB) : $$(rts_$1_OBJS) $$(rts_$1_DTRACE_OBJS) rts/dist-install/libs.depend $$(rts_dist-install_FFI_SO)
	"$$(RM)" $$(RM_OPTS) $$@
	"$$(rts_dist-install_HC)" -this-unit-id rts -shared -dynamic -dynload deploy \
	  -no-auto-link-packages $$(LIBFFI_LIBS) `cat rts/dist-install/libs.depend` $$(rts_$1_OBJS) \
          $$(rts_dist-install_$1_GHC_LD_OPTS) \
	  $$(rts_$1_DTRACE_OBJS) -o $$@
endif
else

ifeq "$(USE_DTRACE)" "YES"
ifeq "$(NEED_DTRACE_PROBES_OBJ)" "YES"
# A list of objects that do not get included in the RTS object that is created
# during the linking step. To prevent future linking errors, especially when
# using the compiler as a bootstrap compiler, we need to exclude the hook
# objects from being re-linked into the single LINKED_OBJS object file. When the
# hooks are being linked into the RTS object this will result in duplicated
# symbols causing the linker to fail (e.g. `StackOverflowHook` in RTS.o and
# hschooks.o). The excluded objects do not get relinked into the RTS object but
# get included separately so prevent linker errors.
# (see issue #15040)
rts_$1_EXCLUDED_OBJS = $$(rts_$1_C_HOOK_OBJS)
# The RTS object that gets generated to package up all of the runtime system
# with the dtrace probe code.
rts_$1_LINKED_OBJS = rts/dist-install/build/RTS.$$($1_osuf)

$$(rts_$1_LINKED_OBJS) : $$(rts_$1_OBJS) $$(rts_$1_DTRACE_OBJS) $$(rts_$1_C_HOOK_OBJS)
	"$$(RM)" $$(RM_OPTS) $$@

	# When linking an archive the linker will only include the object files that
	# are actually needed during linking. It therefore does not include the dtrace
	# specific code for initializing the probes. By creating a single object that
	# also includes the probe object code we force the linker to include the
	# probes when linking the static runtime.
	#
	# The reason why we are re-linking all the objects into a single object file
	# is stated in this thread:
	# https://thr3ads.net/dtrace-discuss/2005/08/384778-Problem-with-probes-defined-in-static-libraries
	$(LD) -r -o $$(rts_$1_LINKED_OBJS) $$(rts_$1_DTRACE_OBJS) $$(filter-out $$(rts_$1_EXCLUDED_OBJS), $$(rts_$1_OBJS))
else
rts_$1_EXCLUDED_OBJS =
rts_$1_LINKED_OBJS = $$(rts_$1_OBJS)
endif
else
rts_$1_EXCLUDED_OBJS =
rts_$1_LINKED_OBJS = $$(rts_$1_OBJS)
endif


$$(rts_$1_LIB) : $$(rts_$1_LINKED_OBJS)
	"$$(RM)" $$(RM_OPTS) $$@

	echo $$(rts_$1_LINKED_OBJS) $$(rts_$1_EXCLUDED_OBJS) | "$$(XARGS)" $$(XARGS_OPTS) "$$(AR_STAGE1)" \
		$$(AR_OPTS_STAGE1) $$(EXTRA_AR_ARGS_STAGE1) $$@

ifneq "$$(UseSystemLibFFI)" "YES"
$$(rts_$1_LIB) : rts/dist-install/build/libC$$(LIBFFI_NAME)$$($1_libsuf)
rts/dist-install/build/libC$$(LIBFFI_NAME)$$($1_libsuf): libffi/build/inst/lib/libffi.a
	cp $$< $$@
endif

endif

endif

endef

# And expand the above for each way:
$(foreach way,$(rts_WAYS),$(eval $(call build-rts-way,$(way))))

$(eval $(call distdir-opts,rts,dist-install,1))

#-----------------------------------------------------------------------------
# Flags for compiling every file

# We like plenty of warnings.
WARNING_OPTS += -Wall
WARNING_OPTS += -Wextra
WARNING_OPTS += -Wstrict-prototypes
WARNING_OPTS += -Wmissing-prototypes
WARNING_OPTS += -Wmissing-declarations
WARNING_OPTS += -Winline
WARNING_OPTS += -Wpointer-arith
WARNING_OPTS += -Wmissing-noreturn
WARNING_OPTS += -Wnested-externs
WARNING_OPTS += -Wredundant-decls
# Some gccs annoyingly enable this archaic specimen by default
WARNING_OPTS += -Wno-aggregate-return

# These ones are hard to avoid:
#WARNING_OPTS += -Wconversion
#WARNING_OPTS += -Wbad-function-cast
#WARNING_OPTS += -Wshadow
#WARNING_OPTS += -Wcast-qual

# This one seems buggy on GCC 4.1.2, which is the only GCC version we
# have that can bootstrap the SPARC build. We end up with lots of supurious
# warnings of the form "cast increases required alignment of target type".
# Some legitimate warnings can be fixed by adding an intermediate cast to
# (void*), but we get others in rts/sm/GCUtils.c concerning the gct var
# that look innocuous to me. We could enable this again once we deprecate
# support for registerised builds on this arch. -- BL 2010/02/03
# WARNING_OPTS += -Wcast-align

rts_INCLUDE_DIRS = \
	$(addprefix include/,$(includes_INCLUDE_DIRS)) \
	.

rts_dist-install_INCLUDE_DIRS = \
	$(addprefix include/,$(includes_dist-install_INCLUDE_DIRS))

# COMPILING_RTS is only used when building Win32 DLL support.
rts_CPP_OPTS += -DCOMPILING_RTS -DFS_NAMESPACE=rts

# HC_OPTS is included in both .c and .cmm compilations, whereas CC_OPTS is
# only included in .c compilations.  HC_OPTS included the WAY_* opts, which
# must be included in both types of compilations.

rts_CC_OPTS += $(WARNING_OPTS)

rts_HC_OPTS += -this-unit-id rts

ifneq "$(GhcWithSMP)" "YES"
rts_CC_OPTS += -DNOSMP
rts_HC_OPTS += -optc-DNOSMP
endif

# We *want* type-checking of hand-written cmm.
rts_HC_OPTS += -dcmm-lint

# -fno-strict-aliasing is required for the runtime, because we often
# use a variety of types to represent closure pointers (StgPtr,
# StgClosure, StgMVar, etc.), and without -fno-strict-aliasing gcc is
# allowed to assume that these pointers do not alias.  eg. without
# this flag we get problems in sm/Evac.c:copy() with gcc 3.4.3, the
# upd_evacuee() assignments get moved before the object copy.
rts_CC_OPTS += -fno-strict-aliasing

rts_CC_OPTS += -fno-common

ifeq "$(BeConservative)" "YES"
rts_CC_OPTS += -DBE_CONSERVATIVE
endif

# Set Windows version
ifeq "$$(TargetOS_CPP)" "mingw32"
rts_CC_OPTS += -DWINVER=$(rts_WINVER)
endif

#-----------------------------------------------------------------------------
# Flags for compiling specific files
rts/RtsMessages_CC_OPTS += -DProjectVersion=\"$(ProjectVersion)\"
rts/RtsUtils_CC_OPTS += -DProjectVersion=\"$(ProjectVersion)\"
rts/Trace_CC_OPTS += -DProjectVersion=\"$(ProjectVersion)\"
#
rts/RtsUtils_CC_OPTS += -DHostPlatform=\"$(HOSTPLATFORM)\"
rts/RtsUtils_CC_OPTS += -DHostArch=\"$(HostArch_CPP)\"
rts/RtsUtils_CC_OPTS += -DHostOS=\"$(HostOS_CPP)\"
rts/RtsUtils_CC_OPTS += -DHostVendor=\"$(HostVendor_CPP)\"
#
rts/RtsUtils_CC_OPTS += -DBuildPlatform=\"$(BUILDPLATFORM)\"
rts/RtsUtils_CC_OPTS += -DBuildArch=\"$(BuildArch_CPP)\"
rts/RtsUtils_CC_OPTS += -DBuildOS=\"$(BuildOS_CPP)\"
rts/RtsUtils_CC_OPTS += -DBuildVendor=\"$(BuildVendor_CPP)\"
#
rts/RtsUtils_CC_OPTS += -DTargetPlatform=\"$(TARGETPLATFORM)\"
rts/RtsUtils_CC_OPTS += -DTargetArch=\"$(TargetArch_CPP)\"
rts/RtsUtils_CC_OPTS += -DTargetOS=\"$(TargetOS_CPP)\"
rts/RtsUtils_CC_OPTS += -DTargetVendor=\"$(TargetVendor_CPP)\"
#
rts/RtsUtils_CC_OPTS += -DGhcUnregisterised=\"$(GhcUnregisterised)\"
rts/RtsUtils_CC_OPTS += -DTablesNextToCode=\"$(TablesNextToCode)\"
#
rts/Hash_CC_OPTS += -O3

# Compile various performance-critical pieces *without* -fPIC -dynamic
# even when building a shared library.  If we don't do this, then the
# GC runs about 50% slower on x86 due to the overheads of PIC.  The
# cost of doing this is a little runtime linking and less sharing, but
# not much.
#
# On x86_64 this doesn't work, because all objects in a shared library
# must be compiled with -fPIC (since the 32-bit relocations generated
# by the default small memory can't be resolved at runtime).  So we
# only do this on i386.
#
# This apparently doesn't work on OS X (Darwin) nor on Solaris.
# On Darwin we get errors of the form
#
#  ld: absolute addressing (perhaps -mdynamic-no-pic) used in _stg_ap_0_fast from rts/dist-install/build/Apply.dyn_o not allowed in slidable image
#
# and lots of these warnings:
#
#  ld: warning codegen in _stg_ap_pppv_fast (offset 0x0000005E) prevents image from loading in dyld shared cache
#
# On Solaris we get errors like:
#
# Text relocation remains                         referenced
#     against symbol                  offset      in file
# .rodata (section)                   0x11        rts/dist-install/build/Apply.dyn_o
#   ...
# ld: fatal: relocations remain against allocatable but non-writable sections
# collect2: ld returned 1 exit status

ifeq "$(TargetArch_CPP)" "i386"
i386_SPEED_HACK := "YES"
ifeq "$(TargetOS_CPP)" "darwin"
i386_SPEED_HACK := "NO"
endif
ifeq "$(TargetOS_CPP)" "solaris2"
i386_SPEED_HACK := "NO"
endif
endif

ifeq "$(TargetArch_CPP)" "i386"
ifeq "$(i386_SPEED_HACK)" "YES"
rts/sm/Evac_HC_OPTS           += -fno-PIC
rts/sm/Evac_thr_HC_OPTS       += -fno-PIC
rts/sm/Scav_HC_OPTS           += -fno-PIC
rts/sm/Scav_thr_HC_OPTS       += -fno-PIC
rts/sm/Compact_HC_OPTS        += -fno-PIC
rts/sm/GC_HC_OPTS             += -fno-PIC

# -static is also necessary for these bits, otherwise the NCG
# -generates dynamic references:
rts/Updates_HC_OPTS += -fno-PIC -static
rts/StgMiscClosures_HC_OPTS += -fno-PIC -static
rts/PrimOps_HC_OPTS += -fno-PIC -static
rts/Apply_HC_OPTS += -fno-PIC -static
rts/dist-install/build/AutoApply_HC_OPTS += -fno-PIC -static
endif
endif

# add CFLAGS for libffi
ifeq "$(UseSystemLibFFI)" "YES"
LIBFFI_CFLAGS = $(addprefix -I,$(FFIIncludeDir))
rts/Interpreter_CC_OPTS += $(LIBFFI_CFLAGS)
rts/Adjustor_CC_OPTS    += $(LIBFFI_CFLAGS)
rts/sm/Storage_CC_OPTS  += $(LIBFFI_CFLAGS)
endif

# inlining warnings happen in Compact
rts/sm/Compact_CC_OPTS += -Wno-inline

# emits warnings about call-clobbered registers on x86_64
rts/StgCRun_CC_OPTS += -w

# On Windows:
rts/win32/ConsoleHandler_CC_OPTS += -w
rts/win32/ThrIOManager_CC_OPTS += -w
# The above warning suppression flags are a temporary kludge.
# While working on this module you are encouraged to remove it and fix
# any warnings in the module. See
#     https://gitlab.haskell.org/ghc/ghc/wikis/working-conventions#Warnings
# for details

# Without this, thread_obj will not be inlined (at least on x86 with GCC 4.1.0)
ifneq "$(CcLlvmBackend)" "YES"
rts/sm/Compact_CC_OPTS += -finline-limit=2500
endif

# -O3 helps unroll some loops (especially in copy() with a constant argument).
rts/sm/Evac_CC_OPTS += -funroll-loops
rts/sm/Evac_thr_HC_OPTS += -optc-funroll-loops


#-----------------------------------------------------------------------------
# Use system provided libffi

ifeq "$(UseSystemLibFFI)" "YES"

rts_PACKAGE_CPP_OPTS += -DFFI_INCLUDE_DIR=$(FFIIncludeDir)
rts_PACKAGE_CPP_OPTS += -DFFI_LIB_DIR=$(FFILibDir)
rts_PACKAGE_CPP_OPTS += '-DFFI_LIB='

else # UseSystemLibFFI==YES

rts_PACKAGE_CPP_OPTS += -DFFI_INCLUDE_DIR=
rts_PACKAGE_CPP_OPTS += -DFFI_LIB_DIR=
rts_PACKAGE_CPP_OPTS += '-DFFI_LIB="C$(LIBFFI_NAME)"'

endif

ifeq "$(UseLibdw)" "YES"
rts_PACKAGE_CPP_OPTS += -DLIBDW_INCLUDE_DIR=$(LibdwIncludeDir)
rts_PACKAGE_CPP_OPTS += -DLIBDW_LIB_DIR=$(LibdwLibDir)
else
rts_PACKAGE_CPP_OPTS += -DLIBDW_INCLUDE_DIR=
rts_PACKAGE_CPP_OPTS += -DLIBDW_LIB_DIR=
endif

# -----------------------------------------------------------------------------
# dependencies

rts_WAYS_DASHED = $(subst $(space),,$(patsubst %,-%,$(strip $(rts_WAYS))))
rts_dist-install_depfile_base = rts/dist-install/build/.depend$(rts_WAYS_DASHED)

rts_dist-install_C_SRCS    = $(rts_C_SRCS) $(rts_thr_EXTRA_C_SRCS)
rts_dist-install_S_SRCS    = $(rts_S_SRCS)
rts_dist-install_CMM_SRCS  = $(rts_CMM_SRCS)
rts_dist-install_C_FILES   = $(rts_dist-install_C_SRCS)
rts_dist-install_CXX_FILES = $(rts_dist-install_CXX_SRCS)
rts_dist-install_S_FILES   = $(rts_dist-install_S_SRCS)
rts_dist-install_CMM_FILES = $(rts_dist-install_CMM_SRCS)

# Hack: we define every way-related option here, so that we get (hopefully)
# a superset of the dependencies.  To do this properly, we should generate
# a different set of dependencies for each way.  Further hack: PROFILING an

# TICKY_TICKY can't be used together, so we omit TICKY_TICKY for now.
rts_dist-install_MKDEPENDC_OPTS += -DPROFILING -DTHREADED_RTS -DDEBUG

ifeq "$(USE_DTRACE)" "YES"

rts_dist-install_MKDEPENDC_OPTS += -Irts/dist-install/build

endif

$(eval $(call dependencies,rts,dist-install,1))

$(rts_dist-install_depfile_c_asm) : $(includes_dist-install_H_FILES)

$(rts_dist-install_depfile_c_asm) : $(DTRACEPROBES_H)
ifneq "$(UseSystemLibFFI)" "YES"
$(rts_dist-install_depfile_c_asm) : $(libffi_HEADERS)
endif

# -----------------------------------------------------------------------------
# compile dtrace probes if dtrace is supported

ifeq "$(USE_DTRACE)" "YES"

rts_CC_OPTS		+= -DDTRACE
rts_HC_OPTS		+= -DDTRACE

# Apple's dtrace (the only one supported by ghc at the moment) uses
# gcc as its preprocessor. If gcc isn't at /usr/bin/gcc, or we need
# to force it to use a different gcc, we need to give the path in
# the option cpppath.

ifeq "$(TargetOS_CPP)" "darwin"
# Darwin has a flag to tell dtrace which cpp to use.
# Unfortunately, this isn't supported on Solaris (See Solaris Dynamic Tracing
# Guide, Chapter 16, for the configuration variables available on Solaris)
DTRACE_FLAGS = -x cpppath=$(CC)
endif

DTRACEPROBES_SRC = rts/RtsProbes.d
$(DTRACEPROBES_H): $(DTRACEPROBES_SRC) $(includes_1_H_CONFIG) $(includes_1_H_PLATFORM) | $$(dir $$@)/.
	"$(DTRACE)" $(filter -I%,$(rts_dist-install_DIST_CC_OPTS)) -C $(DTRACE_FLAGS) -h -o $@ -s $<
endif

# -----------------------------------------------------------------------------
# The RTS package config

# If -DDEBUG is in effect, adjust package conf accordingly..
ifneq "$(strip $(filter -optc-DDEBUG,$(GhcRtsHcOpts)))" ""
rts_PACKAGE_CPP_OPTS += -DDEBUG
endif

ifeq "$(HaveLibMingwEx)" "YES"
rts_PACKAGE_CPP_OPTS += -DHAVE_LIBMINGWEX
endif

rts_dist-install_PACKAGE_CPP_OPTS = \
	$(rts_PACKAGE_CPP_OPTS) \
	$(addprefix -Irts/,$(rts_dist-install_DIST_INCLUDE_DIRS))

$(eval $(call manual-package-config,rts,dist-install))

rts/dist-install/package.conf.inplace : $(includes_dist-install_H_FILES)
rts/dist-install/package.conf.install : $(includes_dist-install_H_FILES)

# -----------------------------------------------------------------------------
# installing

RTS_INSTALL_LIBS += $(ALL_RTS_LIBS)
ifneq "$(UseSystemLibFFI)" "YES"
RTS_INSTALL_LIBS += $(wildcard rts/dist-install/build/lib$(LIBFFI_NAME)*$(soext)*)
RTS_INSTALL_LIBS += $(foreach w,$(filter-out %dyn,$(rts_WAYS)),rts/dist-install/build/libC$(LIBFFI_NAME)$($w_libsuf))
endif

ifneq "$(UseSystemLibFFI)" "YES"
install: install_libffi_headers
endif

.PHONY: install_libffi_headers
install_libffi_headers :
	$(INSTALL_DIR) "$(DESTDIR)$(ghcheaderdir)"
	$(INSTALL_HEADER) $(INSTALL_OPTS) $(libffi_HEADERS) "$(DESTDIR)$(ghcheaderdir)/"

# -----------------------------------------------------------------------------
# cleaning

$(eval $(call clean-target,rts,dist-install,rts/dist-install))

BINDIST_EXTRAS += rts/package.conf.in
