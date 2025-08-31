# Top-level Makefile
#
# This file is still _TOO_ large (should be < 100L). There are too many moving
# _global_ parts, most of this should be relegated to the respective packages.
# The whole version replacement therapy is utterly ridiculous. It should be done
# in the respective packages.

# ┌─────────────────────────────────────────────────────────────────────────┐
# │                        GHC Bootstrapping Stages                         │
# ├─────────────────────────────────────────────────────────────────────────┤
# │                                                                         │
# │  Stage 0 (Bootstrap)                                                    │
# │  ┌─────────┐     ┌─────────┐                                            │
# │  │  ghc0   │     │  pkg0   │  (initial boot packages)                   │
# │  │ (binary)│     │         │                                            │
# │  └────┬────┘     └────┬────┘                                            │
# │       │               │                                                 │
# │       └───────┬───────┘                                                 │
# │               ▼                                                         │
# │         ┌─────────┐                                                     │
# │         │  pkg0+  │  (augmented boot packages)                          │
# │         └────┬────┘                                                     │
# │              │                                                          │
# │  ············│························································· │
# │              ▼                                                          │
# │  Stage 1     │                                                          │
# │  ┌─────────┐ │                                                          │
# │  │  ghc1   │◄┘  (built with ghc0, linked with rts0)                     │
# │  │         │                                                            │
# │  └────┬────┘                                                            │
# │       │                                                                 │
# │       │     ┌─────────┐                                                 │
# │       └────►│  pkg1   │  (initially empty, then populated)              │
# │       ┌─────│         │  (built with ghc1)                              │
# │       │     └─────────┘                                                 │
# │       │           ▲                                                     │
# │       │           │ (mutual dependency; ghc1 needs to sees pkg1)        │
# │       ▼           │                                                     │
# │  ┌─────────┐      │                                                     │
# │  │  ghc1   │──────┘                                                     │
# │  │ (uses)  │                                                            │
# │  └────┬────┘                                                            │
# │       │                                                                 │
# │  ·····│································································ │
# │       ▼                                                                 │
# │  Stage 2                                                                │
# │  ┌─────────┐  ┌──────────┐  ┌─────────┐                                 │
# │  │  ghc2   │  │ ghc-pkg2 │  │  ...    │                                 │
# │  │         │  │          │  │         │                                 │
# │  └─────────┘  └──────────┘  └─────────┘                                 │
# │  (built with ghc1, linked with rts1)                                    │
# │                                                                         │
# │  ┌─────────────────────────────────┐                                    │
# │  │        SHIPPED RESULT           │                                    │
# │  │  ┌─────────┐   ┌─────────┐      │                                    │
# │  │  │  pkg1   │ + │  ghc2   │      │                                    │
# │  │  └─────────┘   └─────────┘      │                                    │
# │  └─────────────────────────────────┘                                    │
# │                                                                         │
# │  Notes:                                                                 │
# │  • Binaries: one stage ahead (ghc1 builds pkg1, ghc2 ships with pkg1)   │
# │  • Libraries: one stage below (pkg1 ships with ghc2)                    │
# │  • ghc1 and ghc2 are ABI compatible                                     |
# |  • ghc0 and ghc1 are not guaruateed to be ABI compatible                |
# │  • ghc1 is linked against rts0, ghc2 against rts1                       │
# |  • augmented packages are needed because ghc1 may require newer         |
# |    versions or even new pacakges, not shipped with the boot compiler    |
# │                                                                         │
# └─────────────────────────────────────────────────────────────────────────┘


# ISSUES:
# - [ ] Where do we get the version number from? The configure script _does_ contain
#       one and sets it, but should it come from the last release tag this branch is
#       contains?
# - [ ] HADRIAN_SETTINGS needs to be removed.
# - [ ] The hadrian folder needs to be removed.
# - [ ] All sublibs should be SRPs in the relevant cabal.project files. No more
#       submodules.

SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c

VERBOSE ?= 0

ROOT_DIR := $(patsubst %/,%,$(dir $(realpath $(lastword $(MAKEFILE_LIST)))))

GHC0 ?= ghc-9.8.4
PYTHON ?= python3
CABAL ?= cabal

LD ?= ld

EMCC ?= emcc
EMCXX ?= em++
EMAR ?= emar
EMRANLIB ?= emranlib

GHC_CONFIGURE_ARGS ?=

EXTRA_LIB_DIRS ?=
EXTRA_INCLUDE_DIRS ?=

MUSL_EXTRA_LIB_DIRS ?=
MUSL_EXTRA_INCLUDE_DIRS ?=

JS_EXTRA_LIB_DIRS ?=
JS_EXTRA_INCLUDE_DIRS ?=

WASM_EXTRA_LIB_DIRS ?=
WASM_EXTRA_INCLUDE_DIRS ?=
WASM_CC_OPTS = -fno-strict-aliasing -Wno-error=int-conversion -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types
WASM_CXX_OPTS = -fno-exceptions -fno-strict-aliasing -Wno-error=int-conversion -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types

# :exploding-head: It turns out override doesn't override the command-line
# value but it overrides Make's normal behavior of ignoring assignments to
# command-line variables. This allows the += operations to append to whatever
# was passed from the command line.

override CABAL_ARGS += \
	--remote-repo-cache _build/packages \
	--store-dir=_build/$(STAGE)/$(TARGET_PLATFORM)/store \
	--logs-dir=_build/$(STAGE)/logs

override CABAL_BUILD_ARGS += \
	-j -w $(GHC) --with-gcc=$(CC) --with-ld=$(LD) \
	--project-file=cabal.project.$(STAGE) \
	$(foreach lib,$(EXTRA_LIB_DIRS),--extra-lib-dirs=$(lib)) \
	$(foreach include,$(EXTRA_INCLUDE_DIRS),--extra-include-dirs=$(include)) \
	--builddir=_build/$(STAGE)/$(TARGET_PLATFORM) \
	--ghc-options="-fhide-source-paths"

GHC_TOOLCHAIN_ARGS ?= --disable-ld-override

# just some defaults
STAGE ?= stage1
GHC ?= $(GHC0)

CABAL_BUILD = $(CABAL) $(CABAL_ARGS) build $(CABAL_BUILD_ARGS)

GHC1 = _build/stage1/bin/ghc
GHC2 = _build/stage2/bin/ghc

define GHC_INFO
$(shell sh -c "$(GHC) --info | $(GHC0) -e 'getContents >>= foldMap putStrLn . lookup \"$1\" . read'")
endef

HOST_PLATFORM   = $(call GHC_INFO,Host platform)
TARGET_PLATFORM = $(call GHC_INFO,target platform string)
TARGET_ARCH     = $(call GHC_INFO,target arch)
TARGET_OS       = $(call GHC_INFO,target os)
TARGET_TRIPLE   = $(call GHC_INFO,Target platform)
GIT_COMMIT_ID  := $(shell git rev-parse HEAD)

define HADRIAN_SETTINGS
[ ("hostPlatformArch",    "$(TARGET_ARCH)") \
, ("hostPlatformOS",      "$(TARGET_OS)") \
, ("cProjectGitCommitId", "$(GIT_COMMIT_ID)") \
, ("cProjectVersion",     "9.14") \
, ("cProjectVersionInt",  "914") \
, ("cProjectPatchLevel",  "0") \
, ("cProjectPatchLevel1", "0") \
, ("cProjectPatchLevel2", "0") \
]
endef

# Handle CPUS and THREADS
CPUS_DETECT_SCRIPT := ./mk/detect-cpu-count.sh
CPUS := $(shell if [ -x $(CPUS_DETECT_SCRIPT) ]; then $(CPUS_DETECT_SCRIPT); else echo 2; fi)
THREADS ?= $(shell echo $$(( $(CPUS) + 1 )))

# Files that will be generated by config.status from their .in counterparts
# FIXME: This is stupid. Why do we patch versions across multiple libraries? Idiotic.
#        also, why on earth do we use a non standard SnakeCase convention for substitutions
#        when CAPITAL_CASE is the standard?
CONFIGURED_FILES := \
	ghc/ghc-bin.cabal \
	compiler/ghc.cabal \
	libraries/ghc-boot/ghc-boot.cabal \
	libraries/ghc-boot-th/ghc-boot-th.cabal \
	libraries/ghc-heap/ghc-heap.cabal \
	libraries/template-haskell/template-haskell.cabal \
	libraries/ghci/ghci.cabal \
	utils/ghc-pkg/ghc-pkg.cabal \
	utils/ghc-iserv/ghc-iserv.cabal \
	utils/runghc/runghc.cabal \
	libraries/ghc-internal/ghc-internal.cabal \
	libraries/ghc-experimental/ghc-experimental.cabal \
	libraries/base/base.cabal \
	rts/include/ghcversion.h

# --- Main Targets ---
all: _build/bindist # booted will depend on prepare-sources

STAGE_UTIL_TARGETS := \
	deriveConstants:deriveConstants \
	genapply:genapply \
	genprimopcode:genprimopcode \
	ghc-pkg:ghc-pkg \
	hsc2hs:hsc2hs \
	rts-headers:rts-headers \
	unlit:unlit

STAGE1_TARGETS := $(STAGE_UTIL_TARGETS) ghc-bin:ghc ghc-toolchain-bin:ghc-toolchain-bin

# TODO: dedup
STAGE1_EXECUTABLES := \
	deriveConstants \
	genapply \
	genprimopcode \
	ghc \
	ghc-pkg \
	ghc-toolchain-bin \
	hsc2hs \
	unlit

# We really want to work towards `cabal build/instsall ghc-bin:ghc`.
STAGE2_TARGETS := \
	ghc-bin:ghc

STAGE2_UTIL_TARGETS := \
	$(STAGE_UTIL_TARGETS) \
	ghc-iserv:ghc-iserv \
	rts:nonthreaded-debug \
	rts:nonthreaded-nodebug \
	hp2ps:hp2ps \
	hpc-bin:hpc \
	runghc:runghc \
	ghc-bignum:ghc-bignum \
	ghc-compact:ghc-compact \
	ghc-experimental:ghc-experimental \
	ghc-toolchain:ghc-toolchain \
	integer-gmp:integer-gmp \
	system-cxx-std-lib:system-cxx-std-lib \
	terminfo:terminfo \
	xhtml:xhtml

# These things should be built on demand.
# hp2ps:hp2ps \
# hpc-bin:hpc \
# ghc-iserv:ghc-iserv \
# runghc:runghc \

# This package is just utterly retarded
# I don't understand why this following line somehow breaks the build...
# STAGE2_TARGETS += system-cxx-std-lib:system-cxx-std-lib

# TODO: dedup
STAGE2_EXECUTABLES := \
	ghc

STAGE2_UTIL_EXECUTABLES := \
	deriveConstants \
	genapply \
	genprimopcode \
	hsc2hs \
	ghc-iserv \
	ghc-pkg \
	hp2ps \
	hpc \
	runghc \
	unlit

BINDIST_EXECTUABLES := \
	ghc \
	ghc-iserv \
	ghc-pkg \
	hp2ps \
	hpc \
	hsc2hs \
	runghc

STAGE3_LIBS := \
    rts:nonthreaded-nodebug \
	Cabal \
	Cabal-syntax \
	array \
	base \
	binary \
	bytestring \
	containers \
	deepseq \
	directory \
	exceptions \
	file-io \
	filepath \
	ghc-bignum \
	hpc \
	integer-gmp \
	mtl \
	os-string \
	parsec \
	pretty \
	process \
	stm \
	template-haskell \
	text \
	time \
	transformers \
	xhtml

# --- Source headers ---
# TODO: this is a hack, because of https://github.com/haskell/cabal/issues/11172
#
# $1 = headers
# $2 = source base dirs
# $3 = pkgname
# $4 = ghc-pkg
define copy_headers
  set -e; \
  dest=`$4 field $3 include-dirs | awk '{ print $$2 }'` ;\
  for h in $1 ; do \
	  mkdir -p "$$dest/`dirname $$h`" ; \
	  for sdir in $2 ; do \
	    if [ -e "$$sdir/$$h" ] ; then \
	      cp -frp "$$sdir/$$h" "$$dest/$$h" ; \
		  break ; \
        fi ; \
	  done ; \
	  [ -e "$$dest/$$h" ] || { echo "Copying $$dest/$$h failed... tried source dirs $2" >&2 ;  exit 2 ; } ; \
  done
endef

RTS_HEADERS_H := \
    rts/Bytecodes.h \
    rts/storage/ClosureTypes.h \
    rts/storage/FunTypes.h \
    stg/MachRegs.h \
    stg/MachRegs/arm32.h \
    stg/MachRegs/arm64.h \
    stg/MachRegs/loongarch64.h \
    stg/MachRegs/ppc.h \
    stg/MachRegs/riscv64.h \
    stg/MachRegs/s390x.h \
    stg/MachRegs/wasm32.h \
    stg/MachRegs/x86.h

define copy_rts_headers_h
  $(call copy_headers,$(RTS_HEADERS_H),rts-headers/include/,rts-headers,$1)
endef

RTS_FS_H := \
    fs.h

define copy_rts_fs_h
  $(call copy_headers,$(RTS_FS_H),rts-fs/,rts-fs,$1)
endef

RTS_H := \
      Cmm.h \
	  HsFFI.h \
	  MachDeps.h \
	  Jumps.h \
	  Rts.h \
	  RtsAPI.h \
	  RtsSymbols.h \
	  Stg.h \
      ghcconfig.h \
	  ghcversion.h \
      rts/ghc_ffi.h \
      rts/Adjustor.h \
      rts/ExecPage.h \
      rts/BlockSignals.h \
      rts/Config.h \
      rts/Constants.h \
      rts/EventLogFormat.h \
      rts/EventLogWriter.h \
      rts/FileLock.h \
      rts/Flags.h \
      rts/ForeignExports.h \
      rts/GetTime.h \
      rts/Globals.h \
      rts/Hpc.h \
      rts/IOInterface.h \
      rts/Libdw.h \
      rts/LibdwPool.h \
      rts/Linker.h \
      rts/Main.h \
      rts/Messages.h \
      rts/NonMoving.h \
      rts/OSThreads.h \
      rts/Parallel.h \
      rts/PrimFloat.h \
      rts/Profiling.h \
      rts/IPE.h \
      rts/PosixSource.h \
      rts/Signals.h \
      rts/SpinLock.h \
      rts/StableName.h \
      rts/StablePtr.h \
      rts/StaticPtrTable.h \
      rts/TTY.h \
      rts/Threads.h \
      rts/Ticky.h \
      rts/Time.h \
      rts/Timer.h \
      rts/TSANUtils.h \
      rts/Types.h \
      rts/Utils.h \
      rts/prof/CCS.h \
      rts/prof/Heap.h \
      rts/prof/LDV.h \
      rts/storage/Block.h \
      rts/storage/ClosureMacros.h \
      rts/storage/Closures.h \
      rts/storage/Heap.h \
      rts/storage/HeapAlloc.h \
      rts/storage/GC.h \
      rts/storage/InfoTables.h \
      rts/storage/MBlock.h \
      rts/storage/TSO.h \
      stg/DLL.h \
      stg/MiscClosures.h \
      stg/Prim.h \
      stg/Regs.h \
      stg/SMP.h \
      stg/Ticky.h \
      stg/MachRegsForHost.h \
      stg/Types.h

RTS_H_DIRS := \
      rts/ \
      rts/include/

define copy_rts_h
  $(call copy_headers,$(RTS_H),$(RTS_H_DIRS),rts,$1)
endef

RTS_JS_H := \
      HsFFI.h \
	  MachDeps.h \
	  Rts.h \
	  RtsAPI.h \
	  Stg.h \
      ghcconfig.h \
      ghcversion.h \
      stg/MachRegsForHost.h \
      stg/Types.h

define copy_rts_js_h
  $(call copy_headers,$(RTS_JS_H),rts/include/,rts,$1)
endef

HASKELINE_H := \
      win_console.h

define copy_haskeline_h
  $(call copy_headers,$(HASKELINE_H),libraries/haskeline/includes,haskeline,$1)
endef

WIN32_H := \
      HsWin32.h \
      HsGDI.h \
      WndProc.h \
      windows_cconv.h \
      alphablend.h \
      wincon_compat.h \
      winternl_compat.h \
      winuser_compat.h \
      winreg_compat.h \
      tlhelp32_compat.h \
      winnls_compat.h \
      winnt_compat.h \
      namedpipeapi_compat.h

define copy_win32_h
  $(call copy_headers,$(WIN32_H),libraries/Win32/include/,Win32,$1)
endef

GHC_INTERNAL_H := \
      HsBase.h \
      consUtils.h

define copy_ghc_internal_h
  $(call copy_headers,$(GHC_INTERNAL_H),libraries/ghc-internal/include/,ghc-internal,$1)
endef

PROCESS_H := \
      runProcess.h \
      processFlags.h

define copy_process_h
  $(call copy_headers,$(PROCESS_H),libraries/process/include/,process,$1)
endef

BYTESTRING_H := \
      fpstring.h \
      bytestring-cpp-macros.h

define copy_bytestring_h
  $(call copy_headers,$(BYTESTRING_H),libraries/bytestring/include/,bytestring,$1)
endef

TIME_H := \
	HsTime.h

define copy_time_h
  $(call copy_headers,$(TIME_H),libraries/time/lib/include/,time,$1)
endef

UNIX_H := \
    HsUnix.h \
    execvpe.h

define copy_unix_h
  $(call copy_headers,$(UNIX_H),libraries/unix/include/,unix,$1)
endef

define copy_all_stage3_h
  $(call copy_rts_headers_h,$1)
  $(call copy_rts_fs_h,$1)
  if [ "$2" = "javascript-unknown-ghcjs" ] ; then $(call copy_rts_js_h,$1) ; else $(call copy_rts_h,$1) ; fi
  $(call copy_ghc_internal_h,$1)
  $(call copy_process_h,$1)
  $(call copy_bytestring_h,$1)
  $(call copy_time_h,$1)
  if [ "$(OS)" = "Windows_NT" ] ; then $(call copy_win32_h,$1) ; else $(call copy_unix_h,$1) ; fi
endef

define copy_all_stage2_h
  $(call copy_all_stage3_h,$1,none)
  $(call copy_haskeline_h,$1)
endef


# --- Bootstrapping and stage 0 ---

# export CABAL := $(shell cabal update 2>&1 >/dev/null && cabal build cabal-install -v0 --disable-tests --project-dir libraries/Cabal && cabal list-bin -v0 --project-dir libraries/Cabal cabal-install:exe:cabal)
$(abspath _build/stage0/bin/cabal): _build/stage0/bin/cabal

# --- Stage 0 build ---

# This just builds cabal-install, which is used to build the rest of the project.

# We need an absolute path here otherwise cabal will consider the path relative to `the project directory
_build/stage0/bin/cabal: BUILD_ARGS=-j -w $(GHC0) --disable-tests --project-dir libraries/Cabal --builddir=$(abspath _build/stage0) --ghc-options="-fhide-source-paths"
_build/stage0/bin/cabal:
	@echo "::group::Building Cabal..."
	@mkdir -p _build/stage0/bin _build/logs
	cabal build $(BUILD_ARGS) cabal-install:exe:cabal
	cp -rfp $(shell cabal list-bin -v0 $(BUILD_ARGS) cabal-install:exe:cabal) _build/stage0/bin/cabal
	@echo "::endgroup::"

# --- Stage 1 build ---

_build/stage1/%: private STAGE=stage1
_build/stage1/%: private GHC=$(GHC0)

.PHONY: $(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES))
$(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES)) &: private TARGET_PLATFORM=
$(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES)) &: $(CABAL) | _build/booted
	@echo "::group::Building stage1 executables ($(STAGE1_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage1/cache
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' $(CABAL_BUILD) $(STAGE1_TARGETS)
	@echo "::endgroup::"

_build/stage1/lib/settings: _build/stage1/bin/ghc-toolchain-bin
	@echo "::group::Creating settings for $(TARGET_TRIPLE)..."
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin $(GHC_TOOLCHAIN_ARGS) --triple $(TARGET_TRIPLE) --output-settings -o $@ --cc $(CC) --cxx $(CXX)
	@echo "::endgroup::"

# The somewhat strange thing is, we might not even need this at all now anymore. cabal seems to
# pass all the necessary flags correctly. Thus even with an _empty_ package-db here (and it will
# stay empty until we are done with the build), the build succeeds.
#
# For now, we are tying the knot here by making sure the stage1 compiler (stage1/bin/ghc) sees
# the packages it builds (to build stage2/bin/ghc), by symlining cabal's target package-db into
# the compilers global package-db. Another maybe even better solution might be to set the
# Global Package DB in the settings file to the absolute path where cabal will place the
# package db. This would elminate this rule outright.
_build/stage1/lib/package.conf.d/package.cache: _build/stage1/bin/ghc-pkg _build/stage1/lib/settings
	@echo "::group::Creating stage1 package cache..."
	@mkdir -p _build/stage1/lib/package.conf.d
# 	@mkdir -p _build/stage2/packagedb/host
# 	ln -s $(abspath ./_build/stage2/packagedb/host/ghc-9.14) _build/stage1/lib/package.conf.d
# 	_build/stage1/bin/ghc-pkg init $(abspath ./_build/stage2/packagedb/host/ghc-9.14)
	@echo "::endgroup::"

_build/stage1/lib/template-hsc.h: utils/hsc2hs/data/template-hsc.h
	@mkdir -p $(@D)
	cp -rfp $< $@

.PHONY: stage1
stage1: $(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES)) _build/stage1/lib/settings _build/stage1/lib/package.conf.d/package.cache _build/stage1/lib/template-hsc.h

# --- Stage 2 build ---

_build/stage2/%: private STAGE=stage2
_build/stage2/%: private GHC=$(realpath _build/stage1/bin/ghc)

.PHONY: $(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES))
$(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES)) &: private TARGET_PLATFORM=
$(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES)) &: $(CABAL) stage1
	@echo "::group::Building stage2 executables ($(STAGE2_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage2/cache
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
		PATH=$(PWD)/_build/stage1/bin:$(PATH) \
		$(CABAL_BUILD) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" -W $(GHC0) $(STAGE2_TARGETS)
	@echo "::endgroup::"

# Do we want to build these with the stage2 GHC or the stage1 GHC?
# Traditionally we build them with the stage1 ghc, but we could just as well
# build them with the stage2 ghc; seems like a better/cleaner idea to me (moritz).
.PHONY: $(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES))
$(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES)) &: private TARGET_PLATFORM=
$(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES)) &: $(CABAL) stage1
	@echo "::group::Building stage2 utilities ($(STAGE2_UTIL_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage2/cache
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
		PATH=$(PWD)/_build/stage1/bin:$(PATH) \
		$(CABAL_BUILD) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" -W $(GHC0) $(STAGE2_UTIL_TARGETS)
	@echo "::endgroup::"

_build/stage2/lib/settings: _build/stage1/lib/settings
	@mkdir -p $(@D)
	cp -rfp _build/stage1/lib/settings _build/stage2/lib/settings

_build/stage2/lib/package.conf.d/package.cache: _build/stage2/bin/ghc-pkg _build/stage2/lib/settings
	@echo "::group::Creating stage2 package cache..."
	@mkdir -p _build/stage2/lib/package.conf.d
	@rm -rf _build/stage2/lib/package.conf.d/*
	cp -rfp _build/stage2/packagedb/host/*/* _build/stage2/lib/package.conf.d
	_build/stage2/bin/ghc-pkg recache
	@echo "::endgroup::"

_build/stage2/lib/template-hsc.h: utils/hsc2hs/data/template-hsc.h
	@mkdir -p $(@D)
	cp -rfp $< $@

.PHONY: stage2
stage2: $(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES)) _build/stage2/lib/settings _build/stage2/lib/package.conf.d/package.cache _build/stage2/lib/template-hsc.h

# --- Stage 3 generic ---

_build/stage2/lib/targets/%:
	@mkdir -p _build/stage3/lib/targets/$(@F)
	@rm -f _build/stage2/lib/targets/$(@F)
	@mkdir -p _build/stage2/lib/targets/
	@ln -sf ../../../stage3/lib/targets/$(@F) _build/stage2/lib/targets/$(@F)

_build/stage3/bin/%-ghc-pkg: _build/stage2/bin/ghc-pkg
	@mkdir -p $(@D)
	@ln -sf ../../stage2/bin/ghc-pkg $@

_build/stage3/bin/%-ghc: _build/stage2/bin/ghc
	@mkdir -p $(@D)
	@ln -sf ../../stage2/bin/ghc $@

_build/stage3/bin/%-hsc2hs: _build/stage2/bin/hsc2hs
	@mkdir -p $(@D)
	@ln -sf ../../stage2/bin/hsc2hs $@

_build/stage3/lib/targets/%/lib/package.conf.d: _build/stage3/lib/targets/%
	@mkdir -p $@

# ghc-toolchain borks unlit
_build/stage3/lib/targets/%/bin/unlit: _build/stage2/bin/unlit
	@mkdir -p $(@D)
	cp -rfp $< $@

# $1 = TIPLET
define build_cross
	HADRIAN_SETTINGS='$(call HADRIAN_SETTINGS)' \
		PATH=$(PWD)/_build/stage2/bin:$(PWD)/_build/stage3/bin:$(PATH) \
		$(CABAL_BUILD) -W $(GHC2) --happy-options="--template=$(abspath _build/stage2/src/happy-lib-2.1.5/data/)" --with-hsc2hs=$1-hsc2hs --hsc2hs-options='-x' --configure-option='--host=$1' \
		$(foreach lib,$(CROSS_EXTRA_LIB_DIRS),--extra-lib-dirs=$(lib)) \
		$(foreach include,$(CROSS_EXTRA_INCLUDE_DIRS),--extra-include-dirs=$(include)) \
		$(STAGE3_LIBS)
endef

# --- Stage 3 javascript build ---

.PHONY: stage3-javascript-unknown-ghcjs
stage3-javascript-unknown-ghcjs: _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings javascript-unknown-ghcjs-libs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d/package.cache

_build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings: _build/stage2/lib/targets/javascript-unknown-ghcjs _build/stage1/bin/ghc-toolchain-bin
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin $(GHC_TOOLCHAIN_ARGS) --triple javascript-unknown-ghcjs --output-settings -o $@ --cc $(EMCC) --cxx $(EMCXX) --ar $(EMAR) --ranlib $(EMRANLIB)

_build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d/package.cache: _build/stage3/bin/javascript-unknown-ghcjs-ghc-pkg _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings javascript-unknown-ghcjs-libs
	@mkdir -p $(@D)
	@rm -rf $(@D)/*
	cp -rfp _build/stage3/javascript-unknown-ghcjs/packagedb/host/*/* $(@D)
	_build/stage3/bin/javascript-unknown-ghcjs-ghc-pkg recache

.PHONY: javascript-unknown-ghcjs-libs
javascript-unknown-ghcjs-libs: private GHC=$(abspath _build/stage3/bin/javascript-unknown-ghcjs-ghc)
javascript-unknown-ghcjs-libs: private GHC2=$(abspath _build/stage2/bin/ghc)
javascript-unknown-ghcjs-libs: private STAGE=stage3
javascript-unknown-ghcjs-libs: private CC=emcc
javascript-unknown-ghcjs-libs: private CROSS_EXTRA_LIB_DIRS=$(JS_EXTRA_LIB_DIRS)
javascript-unknown-ghcjs-libs: private CROSS_EXTRA_INCLUDE_DIRS=$(JS_EXTRA_INCLUDE_DIRS)
javascript-unknown-ghcjs-libs: _build/stage3/bin/javascript-unknown-ghcjs-ghc-pkg _build/stage3/bin/javascript-unknown-ghcjs-ghc _build/stage3/bin/javascript-unknown-ghcjs-hsc2hs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings _build/stage3/lib/targets/javascript-unknown-ghcjs/bin/unlit _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d
	$(call build_cross,javascript-unknown-ghcjs)

# --- Stage 3 musl build ---

.PHONY: stage3-x86_64-musl-linux
stage3-x86_64-musl-linux: x86_64-musl-linux-libs _build/stage3/lib/targets/x86_64-musl-linux/lib/package.conf.d/package.cache

_build/stage3/lib/targets/x86_64-musl-linux/lib/settings: _build/stage2/lib/targets/x86_64-musl-linux _build/stage1/bin/ghc-toolchain-bin
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin $(GHC_TOOLCHAIN_ARGS) --triple x86_64-musl-linux --output-settings -o $@ --cc x86_64-unknown-linux-musl-cc --cxx x86_64-unknown-linux-musl-c++ --ar x86_64-unknown-linux-musl-ar --ranlib x86_64-unknown-linux-musl-ranlib --ld x86_64-unknown-linux-musl-ld

_build/stage3/lib/targets/x86_64-musl-linux/lib/package.conf.d/package.cache: _build/stage3/bin/x86_64-musl-linux-ghc-pkg _build/stage3/lib/targets/x86_64-musl-linux/lib/settings x86_64-musl-linux-libs
	@mkdir -p $(@D)
	@rm -rf $(@D)/*
	cp -rfp _build/stage3/x86_64-musl-linux/packagedb/host/*/* $(@D)
	_build/stage3/bin/x86_64-musl-linux-ghc-pkg recache

.PHONY: x86_64-musl-linux-libs
x86_64-musl-linux-libs: private GHC=$(abspath _build/stage3/bin/x86_64-musl-linux-ghc)
x86_64-musl-linux-libs: private GHC2=$(abspath _build/stage2/bin/ghc)
x86_64-musl-linux-libs: private STAGE=stage3
x86_64-musl-linux-libs: private CC=x86_64-unknown-linux-musl-cc
x86_64-musl-linux-libs: private CROSS_EXTRA_LIB_DIRS=$(MUSL_EXTRA_LIB_DIRS)
x86_64-musl-linux-libs: private CROSS_EXTRA_INCLUDE_DIRS=$(MUSL_EXTRA_INCLUDE_DIRS)
x86_64-musl-linux-libs: _build/stage3/bin/x86_64-musl-linux-ghc-pkg _build/stage3/bin/x86_64-musl-linux-ghc _build/stage3/bin/x86_64-musl-linux-hsc2hs _build/stage3/lib/targets/x86_64-musl-linux/lib/settings _build/stage3/lib/targets/x86_64-musl-linux/bin/unlit _build/stage3/lib/targets/x86_64-musl-linux/lib/package.conf.d
	$(call build_cross,x86_64-musl-linux)

# --- Stage 3 wasm build ---

.PHONY: stage3-wasm32-unknown-wasi
stage3-wasm32-unknown-wasi: wasm32-unknown-wasi-libs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache _build/stage3/lib/targets/wasm32-unknown-wasi/lib/dyld.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/post-link.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/prelude.mjs

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings: _build/stage2/lib/targets/wasm32-unknown-wasi _build/stage1/bin/ghc-toolchain-bin
	@mkdir -p $(@D)
	PATH=/home/hasufell/.ghc-wasm/wasi-sdk/bin:$(PATH) _build/stage1/bin/ghc-toolchain-bin $(GHC_TOOLCHAIN_ARGS) --triple wasm32-unknown-wasi --output-settings -o $@ --cc wasm32-wasi-clang --cxx wasm32-wasi-clang++ --ar ar --ranlib ranlib --ld wasm-ld --merge-objs wasm-ld --merge-objs-opt="-r" --disable-ld-override --disable-tables-next-to-code $(foreach opt,$(WASM_CC_OPTS),--cc-opt=$(opt)) $(foreach opt,$(WASM_CXX_OPTS),--cxx-opt=$(opt))

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache: _build/stage3/bin/wasm32-unknown-wasi-ghc-pkg _build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings wasm32-unknown-wasi-libs
	@mkdir -p $(@D)
	@rm -rf $(@D)/*
	cp -rfp _build/stage3/wasm32-unknown-wasi/packagedb/host/*/* $(@D)
	_build/stage3/bin/wasm32-unknown-wasi-ghc-pkg recache

.PHONY: wasm32-unknown-wasi-libs
wasm32-unknown-wasi-libs: private GHC=$(abspath _build/stage3/bin/wasm32-unknown-wasi-ghc)
wasm32-unknown-wasi-libs: private GHC2=$(abspath _build/stage2/bin/ghc)
wasm32-unknown-wasi-libs: private STAGE=stage3
wasm32-unknown-wasi-libs: private CC=wasm32-wasi-clang
wasm32-unknown-wasi-libs: private CROSS_EXTRA_LIB_DIRS=$(WASM_EXTRA_LIB_DIRS)
wasm32-unknown-wasi-libs: private CROSS_EXTRA_INCLUDE_DIRS=$(WASM_EXTRA_INCLUDE_DIRS)
wasm32-unknown-wasi-libs: _build/stage3/bin/wasm32-unknown-wasi-ghc-pkg _build/stage3/bin/wasm32-unknown-wasi-ghc _build/stage3/bin/wasm32-unknown-wasi-hsc2hs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings _build/stage3/lib/targets/wasm32-unknown-wasi/bin/unlit _build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d
	$(call build_cross,wasm32-unknown-wasi)

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/dyld.mjs:
	@mkdir -p $(@D)
	@cp -f utils/jsffi/dyld.mjs $@
	@chmod +x $@

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/post-link.mjs:
	@mkdir -p $(@D)
	@cp -f utils/jsffi/post-link.mjs $@
	@chmod +x $@

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/prelude.mjs:
	@mkdir -p $(@D)
	@cp -f utils/jsffi/prelude.mjs $@
	@chmod +x $@

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/ghc-interp.js:
	@mkdir -p $(@D)
	@cp -f ghc-interp.js $@

# --- Bindist ---

# patchpackageconf
#
# Hacky function to patch up the paths in the package .conf files
#
# $1 = package name (ex: 'bytestring')
# $2 = path to .conf file
# $3 = (relative) path from $${pkgroot} to docs directory
# $4 = host triple
# $5 = package name and version (ex: bytestring-0.13)
#
# NOTE: We must make sure we keep sub-folder structures alive.  There might be
#       references to $5/build/FOO, we must keep /FOO at the end.  One thing not
#       retaining this that will break are pubilc sublibraries.
#
# FIXME: cabal should just be able to create .conf file properly relocated.  And
#        allow us to install them into a pre-defined package-db, this would
#        eliminate this nonsense.
define patchpackageconf
	sed -i \
		-e "s|haddock-interfaces:.*|haddock-interfaces: \"\$${pkgroot}/$3/html/libraries/$5/$1.haddock\"|" \
		-e "s|haddock-html:.*|haddock-html: \"\$${pkgroot}/$3/html/libraries/$5\"|" \
		-e "s|data-dir:.*|data-dir: \"\$${pkgroot}/../lib/$4/$5\"|" \
		-e "s|$(CURDIR)/_build/stage2/build/host/.*/ghc-.*/$5/build|\$${pkgroot}/../lib/$4/$5|" \
		-e "s|^    $(CURDIR).*||" \
		$2
endef

# $1 = triplet
define copycrosslib
	@cp -rfp _build/stage3/lib/targets/$1 _build/bindist/lib/targets/
	@cd _build/bindist/lib/targets/$1/lib/package.conf.d ; \
		for pkg in *.conf ; do \
		  pkgname=`echo $${pkg} | sed 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
		  pkgnamever=`echo $${pkg} | sed 's/\.conf//'` ; \
		  mkdir -p $(CURDIR)/_build/bindist/lib/targets/$1/lib/$1/$${pkg%.conf} && \
	      cp -rfp $(CURDIR)/_build/stage3/$1/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/_build/bindist/lib/targets/$1/lib/$1/$${pkg%.conf}/ && \
		  $(call patchpackageconf,$${pkgname},$${pkg},../../..,$1,$${pkgnamever}) ; \
		done
endef

# Target for creating the final binary distribution directory
#_build/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
_build/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
	@echo "::group::Creating binary distribution in $@"
	@mkdir -p $@/bin
	@mkdir -p $@/lib
	# Copy executables from stage2 bin
	@cp -rfp _build/stage2/bin/* $@/bin/
	# Copy libraries and settings from stage2 lib
	@cp -rfp _build/stage2/lib/{package.conf.d,settings,template-hsc.h} $@/lib/
	@mkdir -p $@/lib/$(HOST_PLATFORM)
	@cd $@/lib/package.conf.d ; \
		for pkg in *.conf ; do \
		  pkgname=`echo $${pkg} | sed 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
		  pkgnamever=`echo $${pkg} | sed 's/\.conf//'` ; \
		  mkdir -p $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
		  cp -rfp $(CURDIR)/_build/stage2/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
		  $(call patchpackageconf,$${pkgname},$${pkg},../../..,$(HOST_PLATFORM),$${pkgnamever}) ; \
		done
	# Copy driver usage files
	@cp -rfp driver/ghc-usage.txt $@/lib/
	@cp -rfp driver/ghci-usage.txt $@/lib/
	@echo "FIXME: Changing 'Support SMP' from YES to NO in settings file"
	@sed 's/("Support SMP","YES")/("Support SMP","NO")/' -i.bck $@/lib/settings
	# Recache
	$@/bin/ghc-pkg recache
	# Copy headers
	@$(call copy_all_stage2_h,$@/bin/ghc-pkg)
	@echo "::endgroup::"

_build/bindist/ghc.tar.gz: _build/bindist
	@tar czf $@ \
		--directory=_build/bindist \
		$(foreach exe,$(BINDIST_EXECTUABLES),bin/$(exe)) \
		lib/ghc-usage.txt \
		lib/ghci-usage.txt \
		lib/package.conf.d \
		lib/settings \
		lib/template-hsc.h \
		lib/$(HOST_PLATFORM)

_build/bindist/lib/targets/%: _build/bindist driver/ghc-usage.txt driver/ghci-usage.txt stage3-%
	@echo "::group::Creating binary distribution in $@"
	@mkdir -p _build/bindist/bin
	@mkdir -p _build/bindist/lib/targets
	# Symlinks
	@cd _build/bindist/bin ; for binary in * ; do \
		test -L $$binary || ln -sf $$binary $(@F)-$$binary \
		; done
	# Copy libraries and settings
	@if [ -e $(CURDIR)/_build/bindist/lib/targets/$(@F)/lib/$(@F) ] ; then find $(CURDIR)/_build/bindist/lib/targets/$(@F)/lib/$(@F)/ -mindepth 1 -type f -name "*.so" -execdir mv '{}' $(CURDIR)/_build/bindist/lib/targets/$(@F)/lib/$(@F)/'{}' \; ; fi
	$(call copycrosslib,$(@F))
	# --help
	@cp -rfp driver/ghc-usage.txt _build/bindist/lib/targets/$(@F)/lib/
	@cp -rfp driver/ghci-usage.txt _build/bindist/lib/targets/$(@F)/lib/
	# Recache
	@_build/bindist/bin/$(@F)-ghc-pkg recache
	# Copy headers
	@$(call copy_all_stage3_h,_build/bindist/bin/$(@F)-ghc-pkg,$(@F))
	@echo "::endgroup::"

_build/bindist/ghc-%.tar.gz: _build/bindist/lib/targets/% _build/bindist/ghc.tar.gz
	@triple=`basename $<` ; \
		tar czf $@ \
		--directory=_build/bindist \
		$(foreach exe,$(BINDIST_EXECTUABLES),bin/$${triple}-$(exe)) \
		lib/targets/$${triple}


# --- Configuration ---

$(GHC1) $(GHC2): | hackage
hackage: _build/packages/hackage.haskell.org/01-index.tar.gz
_build/packages/hackage.haskell.org/01-index.tar.gz: | $(CABAL)
	@mkdir -p $(@D)
	$(CABAL) $(CABAL_ARGS) update --index-state @1745256340

# booted depends on successful source preparation
_build/booted: libraries/ghc-boot-th-next/.synth-stamp
	@echo "::group::Running ./boot script..."
	@mkdir -p _build/logs
	./boot
	@echo ">>> Running ./configure script..."
	./configure $(GHC_CONFIGURE_ARGS)
	touch $@
	@echo "::endgroup::"

# --- Clean Targets ---
clean:
	@echo "::group::Cleaning build artifacts..."
	rm -rf _build
	rm -f libraries/ghc-boot-th-next/ghc-boot-th-next.cabal
	rm -f libraries/ghc-boot-th-next/ghc-boot-th-next.cabal.in
	rm -f libraries/ghc-boot-th-next/.synth-stamp
	@echo "::endgroup::"

clean-stage1:
	@echo "::group::Cleaning stage1 build artifacts..."
	rm -rf _build/stage1
	@echo "::endgroup::"

clean-stage2:
	@echo "::group::Cleaning stage2 build artifacts..."
	rm -rf _build/stage2
	@echo "::endgroup::"

clean-stage3:
	@echo "::group::Cleaning stage3 build artifacts..."
	rm -rf _build/stage3
	rm -rf _build/stage2/lib/targets
	@echo "::endgroup::"

distclean: clean
	@echo "::group::Cleaning all generated files (distclean)..."
	rm -rf autom4te.cache
	rm -f config.status config.log config.h configure aclocal.m4
	rm -rf build-aux/config.guess build-aux/config.sub build-aux/install-sh build-aux/missing build-aux/compile depcomp
	find . -name 'Makefile.in' -delete
	rm -f $(CONFIGURED_FILES)
	rm -rf libraries/ghc-boot-th-next/ghc-boot-th-next.cabal
	rm -f libraries/ghc-boot-th-next/ghc-boot-th-next.cabal.in
	rm -f libraries/ghc-boot-th-next/.synth-stamp
	@echo "::endgroup::"

# --- Synthesis Targets ---
# This is such a hack 😱
.PHONY: synth-ghc-boot-th-next
synth-ghc-boot-th-next:
	@echo "::group::Synthesizing ghc-boot-th-next (copy & sed from ghc-boot-th)..."
	@mkdir -p libraries/ghc-boot-th-next
	@src=libraries/ghc-boot-th/ghc-boot-th.cabal.in; \
	 dst=libraries/ghc-boot-th-next/ghc-boot-th-next.cabal.in; \
	 if [ ! -f $$src ]; then echo "Source $$src not found" >&2; exit 1; fi; \
	 cp -f $$src $$dst.tmp; \
	 sed -e 's/^name:[[:space:]]*ghc-boot-th$$/name:           ghc-boot-th-next/' \
	     -e 's/ ghc-boot-th/ ghc-boot-th-next/g' \
	     $$dst.tmp > $$dst; \
	 rm -f $$dst.tmp; \
	 touch libraries/ghc-boot-th-next/.synth-stamp
	@echo "::endgroup::"

libraries/ghc-boot-th-next/.synth-stamp: synth-ghc-boot-th-next

# Default: skip performance tests (can override with SKIP_PERF_TESTS=NO)
SKIP_PERF_TESTS ?= YES
export SKIP_PERF_TESTS

# --- Test Suite Helper Tool Paths & Flags (Hadrian parity light) ---
# We approximate Hadrian's test invocation without depending on Hadrian.
# Bindist places test tools in _build/bindist/bin (created by the bindist target).
TEST_TOOLS_DIR := _build/bindist/bin
TEST_GHC       := $(abspath $(TEST_TOOLS_DIR)/ghc)
TEST_GHC_PKG   := $(abspath $(TEST_TOOLS_DIR)/ghc-pkg)
TEST_HP2PS     := $(abspath $(TEST_TOOLS_DIR)/hp2ps)
TEST_HPC       := $(abspath $(TEST_TOOLS_DIR)/hpc)
TEST_RUN_GHC   := $(abspath $(TEST_TOOLS_DIR)/runghc)

# Canonical GHC flags used by the testsuite (mirrors testsuite/mk/test.mk & Hadrian runTestGhcFlags)
CANONICAL_TEST_HC_OPTS = \
	-dcore-lint -dstg-lint -dcmm-lint -no-user-package-db -fno-dump-with-ways \
	-fprint-error-index-links=never -rtsopts -fno-warn-missed-specialisations \
	-fshow-warning-groups -fdiagnostics-color=never -fno-diagnostics-show-caret \
	-Werror=compat -dno-debug-output

# Build timeout utility (needed for some tests) if not already built.
.PHONY: testsuite-timeout
testsuite-timeout:
	$(MAKE) -C testsuite/timeout


# --- Test Target ---
test: _build/bindist testsuite-timeout
	@echo "::group::Running tests with THREADS=$(THREADS)" >&2
	# If any required tool is missing, testsuite logic will skip related tests.
	TEST_HC='$(TEST_GHC)' \
	GHC_PKG='$(TEST_GHC_PKG)' \
	HP2PS_ABS='$(TEST_HP2PS)' \
	HPC='$(TEST_HPC)' \
	RUNGHC='$(TEST_RUN_GHC)' \
	TEST_CC='$(CC)' \
	TEST_CXX='$(CXX)' \
	TEST_HC_OPTS='$(CANONICAL_TEST_HC_OPTS)' \
	METRICS_FILE='$(CURDIR)/_build/test-perf.csv' \
	SUMMARY_FILE='$(CURDIR)/_build/test-summary.txt' \
	JUNIT_FILE='$(CURDIR)/_build/test-junit.xml' \
	SKIP_PERF_TESTS='$(SKIP_PERF_TESTS)' \
	THREADS='$(THREADS)' \
	$(MAKE) -C testsuite/tests test
	@echo "::endgroup::"

# Inform Make that these are not actual files if they get deleted by other means
.PHONY: clean clean-stage1 clean-stage2 clean-stage3 distclean test all configure
