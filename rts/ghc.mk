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

# -----------------------------------------------------------------------------
# Building the RTS

# We build the RTS with stage 1
rts_dist_HC = $(GHC_STAGE1)

rts_INSTALL_INFO = rts
rts_VERSION = 1.0

# merge GhcLibWays and GhcRTSWays but strip out duplicates
rts_WAYS = $(GhcLibWays) $(filter-out $(GhcLibWays),$(GhcRTSWays))
rts_dist_WAYS = $(rts_WAYS)

ALL_RTS_LIBS = $(foreach way,$(rts_WAYS),rts/dist/build/libHSrts$($(way)_libsuf))
all_rts : $(ALL_RTS_LIBS)

# -----------------------------------------------------------------------------
# Defining the sources

ALL_DIRS = hooks sm eventlog

ifeq "$(HostOS_CPP)" "mingw32"
ALL_DIRS += win32
else
ALL_DIRS += posix
endif

rts_C_SRCS := $(wildcard rts/*.c $(foreach dir,$(ALL_DIRS),rts/$(dir)/*.c))
rts_CMM_SRCS := $(wildcard rts/*.cmm)

# Don't compile .S files when bootstrapping a new arch
ifneq "$(PORTING_HOST)" "YES"
ifneq "$(findstring $(TargetArch_CPP), i386 powerpc powerpc64)" ""
rts_S_SRCS += rts/AdjustorAsm.S
endif
endif

ifeq "$(GhcUnregisterised)" "YES"
GENAPPLY_OPTS = -u
endif

rts_AUTO_APPLY_CMM = rts/dist/build/AutoApply.cmm

$(rts_AUTO_APPLY_CMM): $$(genapply_INPLACE)
	"$(genapply_INPLACE)" >$@

rts/dist/build/sm/Evac_thr.c : rts/sm/Evac.c | $$(dir $$@)/.
	cp $< $@
rts/dist/build/sm/Scav_thr.c : rts/sm/Scav.c | $$(dir $$@)/.
	cp $< $@

rts_H_FILES := $(wildcard rts/*.h rts/*/*.h)

ifeq "$(USE_DTRACE)" "YES"
DTRACEPROBES_H = rts/dist/build/RtsProbes.h
rts_H_FILES += $(DTRACEPROBES_H)
endif

# collect the -l and -L flags that we need to link the rts dyn lib.
# Note that, as sed on OS X doesn't handle \+, we use [^ ][^ ]* rather
# than [^ ]\+
rts/libs.depend : $$(ghc-pkg_INPLACE)
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
ifeq "$(HostOS_CPP)" "mingw32" 

ALL_RTS_DEF_LIBNAMES 	= base ghc-prim
ALL_RTS_DEF_LIBS	= \
	rts/dist/build/win32/libHSbase.dll.a \
	rts/dist/build/win32/libHSghc-prim.dll.a

# -- import libs for the regular Haskell libraries
define make-importlib-def # args $1 = lib name
rts/dist/build/win32/libHS$1.def : rts/win32/libHS$1.def
	cat rts/win32/libHS$1.def \
		| sed "s/@LibVersion@/$$(libraries/$1_dist-install_VERSION)/" \
		| sed "s/@ProjectVersion@/$$(ProjectVersion)/" \
		> rts/dist/build/win32/libHS$1.def

rts/dist/build/win32/libHS$1.dll.a : rts/dist/build/win32/libHS$1.def
	"$$(DLLTOOL)" 	-d rts/dist/build/win32/libHS$1.def \
			-l rts/dist/build/win32/libHS$1.dll.a
endef
$(foreach lib,$(ALL_RTS_DEF_LIBNAMES),$(eval $(call make-importlib-def,$(lib))))
endif

ifneq "$(BINDIST)" "YES"
ifneq "$(UseSystemLibFFI)" "YES"
ifeq "$(HostOS_CPP)" "mingw32" 
rts/dist/build/$(LIBFFI_DLL): libffi/build/inst/bin/$(LIBFFI_DLL)
	cp $< $@
else
# This is a little hacky. We don't know the SO version, so we only
# depend on libffi.so, but copy libffi.so*
rts/dist/build/lib$(LIBFFI_NAME)$(soext): libffi/build/inst/lib/lib$(LIBFFI_NAME)$(soext)
	cp libffi/build/inst/lib/lib$(LIBFFI_NAME)$(soext)* rts/dist/build
endif
endif
endif

#-----------------------------------------------------------------------------
# Building one way
define build-rts-way # args: $1 = way

ifneq "$$(BINDIST)" "YES"

rts_dist_$1_HC_OPTS := $$(GhcRtsHcOpts)
rts_dist_$1_CC_OPTS := $$(GhcRtsCcOpts)

# The per-way CC_OPTS
ifneq "$$(findstring debug, $1)" ""
rts_dist_$1_HC_OPTS += -O0
rts_dist_$1_CC_OPTS += -fno-omit-frame-pointer -g -O0
endif

ifneq "$$(findstring dyn, $1)" ""
ifeq "$$(HostOS_CPP)" "mingw32" 
rts_dist_$1_CC_OPTS += -DCOMPILING_WINDOWS_DLL
endif
endif

ifneq "$$(findstring thr, $1)" ""
rts_$1_EXTRA_C_SRCS  =  rts/dist/build/sm/Evac_thr.c rts/dist/build/sm/Scav_thr.c
endif

$(call distdir-way-opts,rts,dist,$1)
$(call c-suffix-rules,rts,dist,$1,YES)
$(call cmm-suffix-rules,rts,dist,$1)

rts_$1_LIB_NAME = libHSrts$$($1_libsuf)
rts_$1_LIB = rts/dist/build/$$(rts_$1_LIB_NAME)

rts_$1_C_OBJS   = $$(patsubst rts/%.c,rts/dist/build/%.$$($1_osuf),$$(rts_C_SRCS)) $$(patsubst %.c,%.$$($1_osuf),$$(rts_$1_EXTRA_C_SRCS))
rts_$1_S_OBJS   = $$(patsubst rts/%.S,rts/dist/build/%.$$($1_osuf),$$(rts_S_SRCS))
rts_$1_CMM_OBJS = $$(patsubst rts/%.cmm,rts/dist/build/%.$$($1_osuf),$$(rts_CMM_SRCS)) $$(patsubst %.cmm,%.$$($1_osuf),$$(rts_AUTO_APPLY_CMM))

rts_$1_OBJS = $$(rts_$1_C_OBJS) $$(rts_$1_S_OBJS) $$(rts_$1_CMM_OBJS)

ifeq "$(USE_DTRACE)" "YES"
ifeq "$(TargetOS_CPP)" "solaris2"
# On Darwin we don't need to generate binary containing probes defined
# in DTrace script, but DTrace on Solaris expects generation of binary
# from the DTrace probes definitions
rts_$1_DTRACE_OBJS = rts/dist/build/RtsProbes.$$($1_osuf)

rts/dist/build/RtsProbes.$$($1_osuf) : $$(rts_$1_OBJS)
	$(DTRACE) -G -C $$(addprefix -I,$$(GHC_INCLUDE_DIRS)) -DDTRACE -s rts/RtsProbes.d -o \
		$$@ $$(rts_$1_OBJS)
endif
endif

rts_dist_$1_CC_OPTS += -DRtsWay=\"rts_$1\"

ifneq "$$(UseSystemLibFFI)" "YES"
rts_dist_FFI_SO = rts/dist/build/lib$$(LIBFFI_NAME)$$(soext)
else
rts_dist_FFI_SO =
endif

# Making a shared library for the RTS.
ifneq "$$(findstring dyn, $1)" ""
ifeq "$$(HostOS_CPP)" "mingw32" 
$$(rts_$1_LIB) : $$(rts_$1_OBJS) $$(ALL_RTS_DEF_LIBS) rts/libs.depend rts/dist/build/$$(LIBFFI_DLL)
	"$$(RM)" $$(RM_OPTS) $$@
	"$$(rts_dist_HC)" -package-name rts -shared -dynamic -dynload deploy \
	  -no-auto-link-packages -Lrts/dist/build -l$$(LIBFFI_NAME) \
	  `cat rts/libs.depend` $$(rts_$1_OBJS) $$(ALL_RTS_DEF_LIBS) -o $$@
else
ifneq "$$(UseSystemLibFFI)" "YES"
LIBFFI_LIBS = -Lrts/dist/build -l$$(LIBFFI_NAME)
ifeq "$$(TargetElf)" "YES"
LIBFFI_LIBS += -optl-Wl,-rpath -optl-Wl,'$$$$ORIGIN' -optl-Wl,-z -optl-Wl,origin
endif

else
# flags will be taken care of in rts/libs.depend
LIBFFI_LIBS =
endif
$$(rts_$1_LIB) : $$(rts_$1_OBJS) $$(rts_$1_DTRACE_OBJS) rts/libs.depend $$(rts_dist_FFI_SO)
	"$$(RM)" $$(RM_OPTS) $$@
	"$$(rts_dist_HC)" -package-name rts -shared -dynamic -dynload deploy \
	  -no-auto-link-packages $$(LIBFFI_LIBS) `cat rts/libs.depend` $$(rts_$1_OBJS) \
	  $$(rts_$1_DTRACE_OBJS) -o $$@
endif
else
$$(rts_$1_LIB) : $$(rts_$1_OBJS) $$(rts_$1_DTRACE_OBJS)
	"$$(RM)" $$(RM_OPTS) $$@
	echo $$(rts_$1_OBJS) $$(rts_$1_DTRACE_OBJS) | "$$(XARGS)" $$(XARGS_OPTS) "$$(AR_STAGE1)" \
		$$(AR_OPTS_STAGE1) $$(EXTRA_AR_ARGS_STAGE1) $$@

ifneq "$$(UseSystemLibFFI)" "YES"
$$(rts_$1_LIB) : rts/dist/build/libC$$(LIBFFI_NAME)$$($1_libsuf)
rts/dist/build/libC$$(LIBFFI_NAME)$$($1_libsuf): libffi/build/inst/lib/libffi.a
	cp $$< $$@
endif

endif

endif

endef

# And expand the above for each way:
$(foreach way,$(rts_WAYS),$(eval $(call build-rts-way,$(way))))

$(eval $(call distdir-opts,rts,dist,1))

#-----------------------------------------------------------------------------
# Flags for compiling every file

# We like plenty of warnings.
WARNING_OPTS += -Wall
ifeq "$(GccLT34)" "YES"
WARNING_OPTS += -W
else
WARNING_OPTS += -Wextra
endif
WARNING_OPTS += -Wstrict-prototypes 
WARNING_OPTS += -Wmissing-prototypes 
WARNING_OPTS += -Wmissing-declarations
WARNING_OPTS += -Winline
WARNING_OPTS += -Waggregate-return
WARNING_OPTS += -Wpointer-arith
WARNING_OPTS += -Wmissing-noreturn
WARNING_OPTS += -Wnested-externs
WARNING_OPTS += -Wredundant-decls 

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

STANDARD_OPTS += $(addprefix -I,$(GHC_INCLUDE_DIRS)) -Irts -Irts/dist/build
# COMPILING_RTS is only used when building Win32 DLL support.
STANDARD_OPTS += -DCOMPILING_RTS

# HC_OPTS is included in both .c and .cmm compilations, whereas CC_OPTS is
# only included in .c compilations.  HC_OPTS included the WAY_* opts, which
# must be included in both types of compilations.

rts_CC_OPTS += $(WARNING_OPTS)
rts_CC_OPTS += $(STANDARD_OPTS)

rts_HC_OPTS += $(STANDARD_OPTS) -package-name rts

ifneq "$(GhcWithSMP)" "YES"
rts_CC_OPTS += -DNOSMP
rts_HC_OPTS += -optc-DNOSMP
endif

ifeq "$(UseLibFFIForAdjustors)" "YES"
rts_CC_OPTS += -DUSE_LIBFFI_FOR_ADJUSTORS
endif

# We *want* type-checking of hand-written cmm.
rts_HC_OPTS += -dcmm-lint 

# -fno-strict-aliasing is required for the runtime, because we often
# use a variety of types to represent closure pointers (StgPtr,
# StgClosure, StgMVar, etc.), and without -fno-strict-aliasing gcc is
# allowed to assume that these pointers do not alias.  eg. without
# this flag we get problems in sm/Evac.c:copy() with gcc 3.4.3, the
# upd_evacee() assigments get moved before the object copy.
rts_CC_OPTS += -fno-strict-aliasing

rts_CC_OPTS += -fno-common

ifeq "$(BeConservative)" "YES"
rts_CC_OPTS += -DBE_CONSERVATIVE
endif

#-----------------------------------------------------------------------------
# Flags for compiling specific files

# If RtsMain.c is built with optimisation then the SEH exception stuff on
# Windows gets confused.
# This has to be in HC rather than CC opts, as otherwise there's a
# -optc-O2 that comes after it.
rts/RtsMain_HC_OPTS += -optc-O0

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
rts/RtsUtils_CC_OPTS += -DGhcEnableTablesNextToCode=\"$(GhcEnableTablesNextToCode)\"

ifeq "$(DYNAMIC_GHC_PROGRAMS)" "YES"
rts/Linker_CC_OPTS += -DDYNAMIC_GHC_PROGRAMS
endif

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
#  ld: absolute addressing (perhaps -mdynamic-no-pic) used in _stg_ap_0_fast from rts/dist/build/Apply.dyn_o not allowed in slidable image
#
# and lots of these warnings:
#
#  ld: warning codegen in _stg_ap_pppv_fast (offset 0x0000005E) prevents image from loading in dyld shared cache
#
# On Solaris we get errors like:
#
# Text relocation remains                         referenced
#     against symbol                  offset      in file
# .rodata (section)                   0x11        rts/dist/build/Apply.dyn_o
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
rts/dist/build/AutoApply_HC_OPTS += -fno-PIC -static
endif
endif

# add CFLAGS for libffi
# ffi.h triggers prototype warnings, so disable them here:
ifeq "$(UseSystemLibFFI)" "YES"
LIBFFI_CFLAGS = $(addprefix -I,$(FFIIncludeDir))
else
LIBFFI_CFLAGS =
endif
rts/Interpreter_CC_OPTS += -Wno-strict-prototypes $(LIBFFI_CFLAGS)
rts/Adjustor_CC_OPTS    += -Wno-strict-prototypes $(LIBFFI_CFLAGS)
rts/sm/Storage_CC_OPTS  += -Wno-strict-prototypes $(LIBFFI_CFLAGS)

# inlining warnings happen in Compact
rts/sm/Compact_CC_OPTS += -Wno-inline

# emits warnings about call-clobbered registers on x86_64
rts/StgCRun_CC_OPTS += -w

rts/RetainerProfile_CC_OPTS += -w
rts/RetainerSet_CC_OPTS += -Wno-format
# On Windows:
rts/win32/ConsoleHandler_CC_OPTS += -w
rts/win32/ThrIOManager_CC_OPTS += -w
# The above warning supression flags are a temporary kludge.
# While working on this module you are encouraged to remove it and fix
# any warnings in the module. See
#     http://hackage.haskell.org/trac/ghc/wiki/WorkingConventions#Warnings
# for details

# Without this, thread_obj will not be inlined (at least on x86 with GCC 4.1.0)
rts/sm/Compact_CC_OPTS += -finline-limit=2500

# -O3 helps unroll some loops (especially in copy() with a constant argument).
rts/sm/Evac_CC_OPTS += -funroll-loops
rts/dist/build/sm/Evac_thr_HC_OPTS += -optc-funroll-loops

# These files are just copies of sm/Evac.c and sm/Scav.c respectively,
# but compiled with -DPARALLEL_GC.
rts/dist/build/sm/Evac_thr_CC_OPTS += -DPARALLEL_GC -Irts/sm
rts/dist/build/sm/Scav_thr_CC_OPTS += -DPARALLEL_GC -Irts/sm

#-----------------------------------------------------------------------------
# Add PAPI library if needed

ifeq "$(GhcRtsWithPapi)" "YES"

rts_CC_OPTS		+= -DUSE_PAPI

rts_PACKAGE_CPP_OPTS	+= -DUSE_PAPI
rts_PACKAGE_CPP_OPTS    += -DPAPI_INCLUDE_DIR=$(PapiIncludeDir)
rts_PACKAGE_CPP_OPTS    += -DPAPI_LIB_DIR=$(PapiLibDir)

ifneq "$(PapiIncludeDir)" ""
rts_HC_OPTS     += -I$(PapiIncludeDir)
rts_CC_OPTS     += -I$(PapiIncludeDir)
rts_HSC2HS_OPTS += -I$(PapiIncludeDir)
endif
ifneq "$(PapiLibDirs)" ""
rts_LD_OPTS     += -L$(PapiLibDirs)
endif

else # GhcRtsWithPapi==YES

rts_PACKAGE_CPP_OPTS += -DPAPI_INCLUDE_DIR=""
rts_PACKAGE_CPP_OPTS += -DPAPI_LIB_DIR=""

endif

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

# -----------------------------------------------------------------------------
# dependencies

rts_WAYS_DASHED = $(subst $(space),,$(patsubst %,-%,$(strip $(rts_WAYS))))
rts_dist_depfile_base = rts/dist/build/.depend$(rts_WAYS_DASHED)

rts_dist_C_SRCS    = $(rts_C_SRCS) $(rts_thr_EXTRA_C_SRCS)
rts_dist_S_SRCS    = $(rts_S_SRCS)
rts_dist_CMM_SRCS  = $(rts_CMM_SRCS)
rts_dist_C_FILES   = $(rts_dist_C_SRCS)
rts_dist_S_FILES   = $(rts_dist_S_SRCS)
rts_dist_CMM_FILES = $(rts_dist_CMM_SRCS)

# Hack: we define every way-related option here, so that we get (hopefully)
# a superset of the dependencies.  To do this properly, we should generate
# a different set of dependencies for each way.  Further hack: PROFILING an

# TICKY_TICKY can't be used together, so we omit TICKY_TICKY for now.
rts_dist_MKDEPENDC_OPTS += -DPROFILING -DTHREADED_RTS -DDEBUG

ifeq "$(USE_DTRACE)" "YES"

rts_dist_MKDEPENDC_OPTS += -Irts/dist/build

endif

$(eval $(call dependencies,rts,dist,1))

$(rts_dist_depfile_c_asm) : $(DTRACEPROBES_H)
ifneq "$(UseSystemLibFFI)" "YES"
$(rts_dist_depfile_c_asm) : $(libffi_HEADERS)
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
DTRACE_FLAGS = -x cpppath=$(WhatGccIsCalled)
endif

DTRACEPROBES_SRC = rts/RtsProbes.d
$(DTRACEPROBES_H): $(DTRACEPROBES_SRC) includes/ghcplatform.h | $$(dir $$@)/.
	"$(DTRACE)" $(filter -I%,$(rts_CC_OPTS)) -C $(DTRACE_FLAGS) -h -o $@ -s $<
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

$(eval $(call manual-package-config,rts))

rts/package.conf.inplace : $(includes_H_CONFIG) $(includes_H_PLATFORM)

# -----------------------------------------------------------------------------
# installing

RTS_INSTALL_LIBS += $(ALL_RTS_LIBS)
ifneq "$(UseSystemLibFFI)" "YES"
RTS_INSTALL_LIBS += $(wildcard rts/dist/build/lib$(LIBFFI_NAME)*$(soext)*)
RTS_INSTALL_LIBS += $(foreach w,$(filter-out %dyn,$(rts_WAYS)),rts/dist/build/libC$(LIBFFI_NAME)$($w_libsuf))
endif

ifneq "$(UseSystemLibFFI)" "YES"
install: install_libffi_headers
endif

.PHONY: install_libffi_headers
install_libffi_headers :
	$(call INSTALL_DIR,"$(DESTDIR)$(ghcheaderdir)")
	$(call INSTALL_HEADER,$(INSTALL_OPTS),$(libffi_HEADERS),"$(DESTDIR)$(ghcheaderdir)/")

# -----------------------------------------------------------------------------
# cleaning

$(eval $(call clean-target,rts,dist,rts/dist))

BINDIST_EXTRAS += rts/package.conf.in

