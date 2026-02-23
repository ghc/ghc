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
# |  • ghc0 and ghc1 are not guaranteed to be ABI compatible                |
# │  • ghc1 is linked against rts0, ghc2 against rts1                       │
# |  • augmented packages are needed because ghc1 may require newer         |
# |    versions or even new packages, not shipped with the boot compiler    |
# │                                                                         │
# └─────────────────────────────────────────────────────────────────────────┘


# ISSUES:
# - [ ] Where do we get the version number from? The configure script _does_ contain
#       one and sets it, but should it come from the last release tag this branch is
#       contains?
# - [ ] The hadrian folder needs to be removed.
# - [ ] All sublibs should be SRPs in the relevant cabal.project files. No more
#       submodules.

SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c

VERBOSE ?= 0

UNAME := $(shell uname)

# If using autoconf feature toggles you can instead run:
#   ./configure --enable-dynamic --enable-profiling --enable-debug
# which generates cabal.project.stage2.settings (imported by cabal.project.stage2).
# The legacy DYNAMIC=1 path still appends flags directly; if both are used the
# configure-generated settings file (import) and these args should agree.
#
# Enable dynamic runtime/linking support when DYNAMIC=1 is passed on the make
# command line. This will build shared libraries, a dynamic RTS (defining
# -DDYNAMIC) and allow tests requiring dynamic linking (e.g. plugins-external)
# to run. The default remains static to keep rebuild cost low.
DYNAMIC ?= 0

#
# System tools
#
# Note: ?= uses the environment variable if set
#

CABAL0 ?= cabal
GHC0   ?= ghc-9.8.4

AR     ?= ar
CC     ?= cc
LD     ?= ld
PYTHON ?= python3
SED    ?= sed
LN     ?= ln
LN_S   ?= $(LN) -s
LN_SF  ?= $(LN) -sf

ifeq ($(UNAME), Darwin)
DLL       := *.dylib
else
DLL       := *.so
endif

# Notes
#
# - rts configure script is a bit evil.
#   λ rg AC_PATH rts/configure.ac
#   489:AC_PATH_PROG([NM], nm)
#   495:AC_PATH_PROG([OBJDUMP], objdump)
#   501:AC_PATH_PROG([DERIVE_CONSTANTS], deriveConstants)
#   505:AC_PATH_PROG([GENAPPLY], genapply)
#

#
# Some compiler toolchain settings
#

CABAL_ARGS         ?=
CC_LINK_OPT         =
GHC_CONFIGURE_ARGS  =

ifeq ($(DYNAMIC),1)
GHC_CONFIGURE_ARGS += --enable-dynamic
endif

GHC_TOOLCHAIN_ARGS  = --disable-ld-override

#
# Build directories and paths
#

# NOTE: it's tricky to know when and where we need an absolute path or we can
# get away with a relative path. We make BUILD_DIR absolute and all derived
# paths will be absolute too.
BUILD_DIR  := _build
STAGE_DIR   = $(BUILD_DIR)/$(STAGE)
STORE_DIR   = $(STAGE_DIR)/store
LOGS_DIR    = $(STAGE_DIR)/logs

DIST_DIR   := $(BUILD_DIR)/dist

# Timing directory for phase start/end timestamps
TIMING_DIR := $(BUILD_DIR)/timing

# Metrics directory for CPU/memory CSV data
METRICS_DIR := $(BUILD_DIR)/metrics

# Stamp files — Make uses these to know a stage is complete.
# Phony targets like `stage2` always re-run their recipe, which causes `test`
# (which depends on `stage2`) to re-execute the entire build even when nothing
# changed. File-based stamps let Make skip already-completed stages.
STAGE0_STAMP := $(BUILD_DIR)/.stamp-stage0
STAGE1_STAMP := $(BUILD_DIR)/.stamp-stage1
STAGE2_STAMP := $(BUILD_DIR)/.stamp-stage2

# Stamp fallback rules: if a stamp doesn't exist, invoke the corresponding
# stage via recursive make. The stage recipe touches the stamp on success.
# Because there are no prerequisites, Make won't re-run these when the stamp
# file already exists — which is the whole point: `test: $(STAGE2_STAMP)` will
# skip the build if stage2 already completed.
$(STAGE0_STAMP): ; @$(MAKE) stage0
$(STAGE1_STAMP): ; @$(MAKE) stage1
$(STAGE2_STAMP): ; @$(MAKE) stage2

# HOST_PLATFROM is always from the bootstrap compiler
HOST_PLATFORM := $(shell $(GHC0) --print-host-platform)

CABAL      := $(BUILD_DIR)/cabal/bin/cabal$(EXE_EXT)

# Handle CPUS and THREADS
CPUS_DETECT_SCRIPT := ./mk/detect-cpu-count.sh
CPUS := $(shell if [ -x $(CPUS_DETECT_SCRIPT) ]; then $(CPUS_DETECT_SCRIPT); else echo 2; fi)
THREADS ?= $(shell echo $$(( $(CPUS) + 1 )))

#
# Build macros
#

ifeq ($(MAKE_HOST),x86_64-pc-msys)
# Windows executables require .exe extension for native programs to find them
EXE_EXT := .exe

# FIXME Are we sure about this? Do we need to check if it exists?
CC      = x86_64-w64-mingw32-clang.exe
CXX     = x86_64-w64-mingw32-clang++.exe
LD      = ld.lld.exe

# https://gitlab.haskell.org/ghc/ghc/-/issues/7289#note_646155
CC_LINK_OPT   = -Wl,CRT_fp8.o
CYGPATH       = cygpath --windows -f -
CYGPATH_MIXED = cygpath --mixed -f -

PATCHELF ?= echo
else
EXE_EXT      :=
CYGPATH       = cat
CYGPATH_MIXED = cat

PATCHELF ?= patchelf
INSTALL_NAME_TOOL ?= install_name_tool
endif


#
# Logging utilities
#

# LOG_GROUP_START = @echo "::group::$1"
# LOG_GROUP_END = @echo "::endgroup::"

BOLD = $(shell tput bold)
NORMAL = $(shell tput sgr0)

LOG_GROUP_START = @echo "$(BOLD)>>>>> $1$(NORMAL)"
LOG_GROUP_END = @echo ""

LOG = @echo "$(BOLD)[$(STAGE)]$(NORMAL): $(1)"

define NORMALIZE_FP
$(shell echo $(1) | $(CYGPATH_MIXED))
endef

# CABAL_BUILD
#
# Generic "cabal build"
#
# $(1): the cabal binary to use
#
# NOTE: Do not pass --with-ar or --with-ld to cabal! it will screw up things
#
define CABAL_BUILD_WITH
	$(1) \
		--remote-repo-cache $(call NORMALIZE_FP,$(CURDIR)/$(BUILD_DIR)/packages) \
		--store-dir $(call NORMALIZE_FP,$(CURDIR)/$(STORE_DIR)) \
		--logs-dir $(call NORMALIZE_FP,$(CURDIR)/$(LOGS_DIR)) \
	build \
		--project-file cabal.project.$(STAGE) \
		--builddir $(call NORMALIZE_FP,$(CURDIR)/$(STAGE_DIR)) \
		$(CABAL_ARGS)
endef

define CABAL_BUILD
	$(call CABAL_BUILD_WITH,$(CABAL))
endef

define CABAL_BUILD_STAGE0
	$(CABAL0) \
		--store-dir $(call NORMALIZE_FP,$(CURDIR)/$(STORE_DIR)) \
		--logs-dir $(call NORMALIZE_FP,$(CURDIR)/$(LOGS_DIR)) \
	build \
		--project-file cabal.project.$(STAGE) \
		--builddir $(call NORMALIZE_FP,$(CURDIR)/$(STAGE_DIR)) \
		$(CABAL_ARGS)
endef

# LIB_NAME_GLOB
#
# $(1): library target, possibly with sublibrary after colon
#
# pkg     -> pkg-*
# pkg:lib -> pkg-*-lib
LIB_NAME_GLOB = $(let pkg lib,$(subst :, ,$(1)),$(pkg)-*$(if $(lib),-$(lib)))

# DIST_COPY_EXE
#
# Copies a executable named $(1) from the local store into the distribution
# directory.
#
# $(1) name of the executable to copy
#
# NOTE: the ending empty line is important
define DIST_COPY_EXE
	$(call LOG,Copying executable $(1) into $(DIST_DIR)/bin)
	@cp -a \
		$(CURDIR)/$(STORE_DIR)/host/$(HOST_PLATFORM)/bin/$(1)$(EXE_EXT) \
		$(CURDIR)/$(DIST_DIR)/bin/$(1)$(EXE_EXT)

endef

# $(1) name of the executable to link
# $(2) platform
define DIST_TARGET_EXE_LINK
	@$(LN_S) \
		$(1)$(EXE_EXT) \
		$(CURDIR)/$(DIST_DIR)/bin/$(2)-$(1)$(EXE_EXT)

endef

# DIST_COPY_LIB
#
# Copies a library from the local store into the distribution directory.
#
# $(1) name of the library to copy
#
# NOTE: the ending empty line is important
define DIST_COPY_LIB
	$(call LOG,Copying library $(1) into $(DIST_DIR)/lib/$(TARGET_PLATFORM))
	@cp -a \
		$(CURDIR)/$(STORE_DIR)/host/$(TARGET_PLATFORM)/lib/$(call LIB_NAME_GLOB,$(1)) \
		$(CURDIR)/$(DIST_DIR)/lib/$(TARGET_PLATFORM)

endef

# DIST_COPY_LIB_CROSS
#
# Copies a library from the local store into the distribution directory.
#
# $(1) name of the library to copy
#
# NOTE: the ending empty line is important
define DIST_COPY_LIB_CROSS
	$(call LOG,Copying library $(1) into $(DIST_DIR)/lib/targets/$(TARGET_PLATFORM)/lib/$(TARGET_PLATFORM))
	@cp -a \
		$(CURDIR)/$(STORE_DIR)/host/$(TARGET_PLATFORM)/lib/$(call LIB_NAME_GLOB,$(1)) \
		$(CURDIR)/$(DIST_DIR)/lib/targets/$(TARGET_PLATFORM)/lib/$(TARGET_PLATFORM)

endef

# DIST_COPY_LIB_CONF
#
# Copies a library packagedb entry from the local store into the distribution
# directory.
#
# $(1) library to copy
#
# NOTE: the ending empty line is important
# NOTE: sed *has* to run in-place becase we do not know the exact filename of
# the file. With -i we can get away with a glob.
define DIST_COPY_LIB_CONF
	$(call LOG,Copying $(1) packagedb entry into $(DIST_DIR)/lib/package.conf.d/)
	@cp -a \
		$(CURDIR)/$(STORE_DIR)/host/$(TARGET_PLATFORM)/package.conf.d/$(call LIB_NAME_GLOB,$(1)).conf \
		$(CURDIR)/$(DIST_DIR)/lib/package.conf.d/
	@$(SED) -i \
		-e "s|$(call PATH_REGEX,$(CURDIR)/$(STORE_DIR)/host/$(TARGET_PLATFORM)/lib)|\$${pkgroot}/../lib/$(TARGET_PLATFORM)|g" \
		$(CURDIR)/$(DIST_DIR)/lib/package.conf.d/$(call LIB_NAME_GLOB,$(1)).conf

endef

# PATH_REGEX
#
# Creates a path regex that, on windows, matches any path separator
# and starts with a proper drive.
#
# On unix, this should do nothing.
# $(1) path to create regex for
define PATH_REGEX
$(shell echo $(1) | $(CYGPATH_MIXED) | sed 's|/|[/\\]|g')
endef

# DIST_COPY_LIB_CONF_CROSS
#
# Copies a library packagedb entry from the local store into the distribution
# directory.
#
# $(1) library to copy
#
# NOTE: the ending empty line is important
# NOTE: sed *has* to run in-place becase we do not know the exact filename of
# the file. With -i we can get away with a glob.
define DIST_COPY_LIB_CONF_CROSS
	$(call LOG,Copying $(1) packagedb entry into $(DIST_DIR)/lib/targets/$(TARGET_PLATFORM)/lib/package.conf.d/)
	@cp -a \
		$(CURDIR)/$(STORE_DIR)/host/$(TARGET_PLATFORM)/package.conf.d/$(call LIB_NAME_GLOB,$(1)).conf \
		$(CURDIR)/$(DIST_DIR)/lib/targets/$(TARGET_PLATFORM)/lib/package.conf.d/
	@$(SED) -i \
		-e 's|$(CURDIR)/$(STORE_DIR)/host/$(TARGET_PLATFORM)/lib|\$${pkgroot}/../lib/$(TARGET_PLATFORM)|g' \
		$(CURDIR)/$(DIST_DIR)/lib/targets/$(TARGET_PLATFORM)/lib/package.conf.d/$(call LIB_NAME_GLOB,$(1)).conf

endef

# SET_RPATH
#
# $(1) = rpath
# $(2) = binary
# set rpath relative to the current executable
# TODO: on darwin, this doesn't overwrite rpath, but just adds to it,
#       so we'll have the old rpaths from the build host in there as well
# set_rpath: Add rpath to binary. On Darwin, check if rpath already exists
# before adding (install_name_tool fails if rpath is duplicate).
define SET_RPATH
	$(if $(filter Darwin,$(UNAME)), \
		if ! otool -l "$(2)" 2>/dev/null | grep -A2 'LC_RPATH' | grep -q "@executable_path/$(1)"; then \
			$(INSTALL_NAME_TOOL) -add_rpath "@executable_path/$(1)" "$(2)"; \
		fi, \
		$(PATCHELF) --force-rpath --set-rpath "\$$ORIGIN/$(1)" "$(2)")
endef

DIST_COPY_EXES            = $(if $(1),$(foreach exe,$(1),$(call DIST_COPY_EXE,$(exe),$(2))))
DIST_COPY_LIBS            = $(if $(1),$(foreach lib,$(1),$(call DIST_COPY_LIB,$(lib))))
DIST_COPY_LIBS_CROSS      = $(if $(1),$(foreach lib,$(1),$(call DIST_COPY_LIB_CROSS,$(lib))))
DIST_COPY_LIBS_CONF       = $(if $(1),$(foreach lib,$(1),$(call DIST_COPY_LIB_CONF,$(lib))))
DIST_COPY_LIBS_CONF_CROSS = $(if $(1),$(foreach lib,$(1),$(call DIST_COPY_LIB_CONF_CROSS,$(lib))))

define DIST_COPY_LIBS_SO
	@find $(CURDIR)/$(STORE_DIR)/host/$(TARGET_PLATFORM)/lib/ -mindepth 1 -type f -name "$(DLL)" -execdir cp '{}' $(CURDIR)/$(DIST_DIR)/lib/$(TARGET_PLATFORM)/'{}' \;
endef

define DIST_COPY_LIBS_SO_CROSS
	@find $(CURDIR)/$(STORE_DIR)/host/$(TARGET_PLATFORM)/lib/ -mindepth 1 -type f -name "$(DLL)" -execdir cp '{}' $(CURDIR)/$(DIST_DIR)/lib/targets/$(TARGET_PLATFORM)/lib/$(TARGET_PLATFORM)/'{}' \;
endef


#
# Files and targets
#

CONFIGURE_SCRIPTS = \
	configure \
	rts/configure \
	libraries/ghc-internal/configure

# Files that will be generated by config.status from their .in counterparts
CONFIGURED_FILES := \
	ghc/ghc-bin.cabal \
	compiler/GHC/CmmToLlvm/Version/Bounds.hs \
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
	rts/include/ghcversion.h \
	cabal.project.stage2.settings

# __  __       _         _                       _
# |  \/  | __ _(_)_ __   | |_ __ _ _ __ __ _  ___| |_
# | |\/| |/ _` | | '_ \  | __/ _` | '__/ _` |/ _ \ __|
# | |  | | (_| | | | | | | || (_| | | | (_| |  __/ |_
# |_|  |_|\__,_|_|_| |_|  \__\__,_|_|  \__, |\___|\__|
#                                      |___/

.PHONY: all
all: stage2

#            _           _       _           _        _ _
#   ___ __ _| |__   __ _| |     (_)_ __  ___| |_ __ _| | |
#  / __/ _` | '_ \ / _` | |_____| | '_ \/ __| __/ _` | | |
# | (_| (_| | |_) | (_| | |_____| | | | \__ \ || (_| | | |
#  \___\__,_|_.__/ \__,_|_|     |_|_| |_|___/\__\__,_|_|_|

.PHONY: $(CABAL)
$(CABAL): STAGE=stage0
$(CABAL):
	$(call LOG,Building $@)
	$(CABAL_INSTALL_STAGE0) --with-compiler $(GHC0) cabal-install:exe:cabal
	$(call PHASE_END_OK,cabal)
	@touch $(STAGE0_STAMP)

stage0 : $(CABAL)

#  ____  _                     _
# / ___|| |_ __ _  __ _  ___  / |
# \___ \| __/ _` |/ _` |/ _ \ | |
#  ___) | || (_| | (_| |  __/ | |
# |____/ \__\__,_|\__, |\___| |_|
#                 |___/

# These are configuration variables for stage one

# TODO we should not need genprimops code here, it is needed by compiler/Setup.hs
# but it is also listed as a build-tool-depends in compiler/ghc.cabal so cabal-install
# will build it automatically. The effect of listing genprimops here is that it
# will be included as a host target rather as a build target. So we end up compiling it
# twice for no reason.
STAGE1_EXECUTABLES = \
	deriveConstants \
	genapply \
	genprimopcode \
	ghc \
	ghc-pkg \
	ghc-toolchain-bin \
	hsc2hs \
	unlit

STAGE1_LIBRARIES =

STAGE1_EXTRA_INCLUDE_DIRS ?=
STAGE1_EXTRA_LIB_DIRS	  ?=

STAGE1_CABAL_BUILD = \
	$(CABAL_BUILD) \
	--with-compiler=$(GHC0) \
	--with-build-compiler=$(GHC0) \
	--ghc-options "-ghcversion-file=$(call NORMALIZE_FP,$(CURDIR)/rts/include/ghcversion.h)"

stage1: STAGE=stage1
stage1: $(CABAL) $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES) cabal.project.stage1 cabal.project.common libraries/ghc-boot-th-next | hackage
	$(call LOG,Starting build of $(STAGE))

	$(call LOG,Building executables $(STAGE1_EXECUTABLES))
	$(STAGE1_CABAL_BUILD) $(addprefix exe:,$(STAGE1_EXECUTABLES))

	$(call LOG,Creating $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/settings)
	@$(STORE_DIR)/host/$(HOST_PLATFORM)/bin/ghc-toolchain-bin $(GHC_TOOLCHAIN_ARGS) --triple $(HOST_PLATFORM) --cc $(CC) --cxx $(CXX) --cc-link-opt "$(CC_LINK_OPT)" --output-settings -o $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/settings
ifeq ($(DYNAMIC),1)
	$(SED) -i -e 's/"RTS ways","/"RTS ways","dyn debug_dyn thr_dyn thr_debug_dyn /' $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/settings
endif

	$(call LOG,Creating packagedb in $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/package.conf.d)
	@rm -rf $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/package.conf.d
	@$(STORE_DIR)/host/$(HOST_PLATFORM)/bin/ghc-pkg init $(STORE_DIR)/host/$(HOST_PLATFORM)/lib/package.conf.d

	$(call LOG,Finished building $(STAGE))
	$(call PHASE_END_OK,stage1)
	@touch $(STAGE1_STAMP)

$(addprefix $(STAGE1_PATH)/bin/,$(STAGE1_EXECUTABLES)) : stage1

#  ____  _                     ____
# / ___|| |_ __ _  __ _  ___  |___ \
# \___ \| __/ _` |/ _` |/ _ \   __) |
#  ___) | || (_| | (_| |  __/  / __/
# |____/ \__\__,_|\__, |\___| |_____|
#                 |___/

# These are configuration variables for the second stage

STAGE2_EXECUTABLES = \
	ghc \
	ghc-iserv \
	ghc-pkg \
	haddock \
	hsc2hs \
	hpc \
	hp2ps \
	runghc \
	unlit

STAGE2_LIBRARIES = \
	array \
	base \
	binary \
	bytestring \
	Cabal \
	Cabal-syntax \
	containers \
	deepseq \
	directory \
	exceptions \
	file-io \
	filepath \
	ghc \
	ghc-bignum \
	ghc-boot \
	ghc-boot-th \
	ghc-compact \
	ghc-experimental \
	ghc-heap \
	ghci \
	ghc-internal \
	ghc-platform \
	ghc-prim \
	ghc-toolchain \
	haddock-api \
	haddock-library \
	haskeline \
	hpc \
	integer-gmp \
	libffi-clib \
	mtl \
	os-string \
	parsec \
	pretty \
	process \
	rts \
	rts:nonthreaded-debug \
	rts:nonthreaded-nodebug \
	rts:threaded-debug \
	rts:threaded-nodebug \
	rts-fs \
	rts-headers \
	semaphore-compat \
	stm \
	system-cxx-std-lib \
	template-haskell \
	text \
	time \
	transformers \
	xhtml

ifeq ($(MAKE_HOST),x86_64-pc-msys)
STAGE2_LIBRARIES += Win32
else
STAGE2_LIBRARIES += terminfo unix
endif

STAGE2_EXTRA_INCLUDE_DIRS ?=
STAGE2_EXTRA_LIB_DIRS     ?=

STAGE2_CABAL_BUILD = \
	env \
	DERIVE_CONSTANTS=$(call NORMALIZE_FP,$(CURDIR)/$(STAGE1_PATH)/bin/deriveConstants) \
	GENAPPLY=$(call NORMALIZE_FP,$(CURDIR)/$(STAGE1_PATH)/bin/genapply) \
	NM=$(NM) \
	OBJDUMP=$(OBJDUMP) \
	$(CABAL_BUILD) \
	--with-compiler=$(call NORMALIZE_FP,$(CURDIR)/$(GHC1)) \
	--with-build-compiler=$(GHC0) \
	--ghc-options "-ghcversion-file=$(call NORMALIZE_FP,$(CURDIR)/rts/include/ghcversion.h)" \
	$(foreach dir,$(STAGE2_EXTRA_LIB_DIRS),--extra-lib-dirs=$(dir)) \
	$(foreach dir,$(STAGE2_EXTRA_INCLUDE_DIRS),--extra-include-dirs=$(dir))

stage2: STAGE=stage2
stage2: TARGET_PLATFORM:=$(HOST_PLATFORM)
stage2: $(GHC1) $(CABAL) $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES) cabal.project.stage2 cabal.project.stage2.settings cabal.project.common libraries/ghc-boot-th-next | stage1
	$(call LOG,Starting build of $(STAGE))

	$(call LOG,Building rts)
	$(STAGE2_CABAL_BUILD) rts

	$(call LOG,Building executables $(STAGE2_EXECUTABLES))
	$(STAGE2_CABAL_BUILD) $(addprefix exe:,$(STAGE2_EXECUTABLES))

	$(call LOG,Building libraries $(filter-out rts%,$(STAGE2_LIBRARIES)))
	$(STAGE2_CABAL_BUILD) $(filter-out rts%,$(STAGE2_LIBRARIES))

	$(call LOG,Building distribution in $(DIST_DIR))
	@rm -rf $(DIST_DIR)

	@mkdir -p $(DIST_DIR)/bin
	$(call DIST_COPY_EXES,$(STAGE2_EXECUTABLES))

	@mkdir -p $(DIST_DIR)/lib/$(TARGET_PLATFORM)
	$(call DIST_COPY_LIBS,$(filter-out system-cxx-std-lib%,$(STAGE2_LIBRARIES)))
	$(call DIST_COPY_LIBS_SO)

	@mkdir -p $(DIST_DIR)/lib/package.conf.d
	$(call DIST_COPY_LIBS_CONF,$(STAGE2_LIBRARIES))

	$(call LOG,Creating $(DIST_DIR)/lib/settings)
	@cp $(STAGE1_PATH)/lib/settings $(DIST_DIR)/lib/settings

	$(call LOG,Creating $(DIST_DIR)/lib/template-hsc.h)
	@cp $(STAGE2_PATH)/lib/hsc2hs-*-hsc2hs/share/template-hsc.h $(DIST_DIR)/lib/template-hsc.h

	# set rpath
	@for binary in $(DIST_DIR)/bin/* ; do \
		$(call SET_RPATH,../lib/$(HOST_PLATFORM),$${binary}) ; \
	done
ifneq ($(UNAME), Darwin)
	$(PATCHELF) --force-rpath --set-rpath "\$$ORIGIN" $(CURDIR)/$(DIST_DIR)/lib/$(TARGET_PLATFORM)/$(DLL)
endif
ifeq ($(DYNAMIC),1)
	$(call LOG,Create -dyn iserv executable symlink so ghc can find ghc-iserv-dyn)
	@$(LN_SF) ghc-iserv$(EXE_EXT) "$(DIST_DIR)/bin/ghc-iserv-dyn$(EXE_EXT)"
endif
	$(call LOG,Refreshing $(DIST_DIR)/lib/package.conf.d cache)
	@$(DIST_DIR)/bin/ghc-pkg recache --package-db $(CURDIR)/$(DIST_DIR)/lib/package.conf.d

	$(call LOG,Verifying $(DIST_DIR)/lib/package.conf.d)
	@$(DIST_DIR)/bin/ghc-pkg check --package-db $(CURDIR)/$(DIST_DIR)/lib/package.conf.d

	$(call LOG,Copying ghc-usage files)
	@cp -rfp driver/ghc-usage.txt $(DIST_DIR)/lib/
	@cp -rfp driver/ghci-usage.txt $(DIST_DIR)/lib/

	$(call LOG,Finished building $(STAGE) in $(DIST_DIR))
	$(call PHASE_END_OK,stage2.dist)
	$(call PHASE_END_OK,stage2)
	@touch $(STAGE2_STAMP)

$(addprefix $(STAGE2_PATH)/bin/,$(STAGE2_EXECUTABLES)) : stage2

#  ____  _                     _____
# / ___|| |_ __ _  __ _  ___  |___ /
# \___ \| __/ _` |/ _` |/ _ \   |_ \
#  ___) | || (_| | (_| |  __/  ___) |
# |____/ \__\__,_|\__, |\___| |____/
#                 |___/

# these are GHC names
# TODO: x86_64-musl-linux -> x86_64-unknown-linux-musl
STAGE3_PLATFORMS := \
	x86_64-musl-linux \
	javascript-unknown-ghcjs \
	wasm32-unknown-wasi

STAGE3_EXECUTABLES := \
    ghc \
    ghc-iserv \
    ghc-pkg \
    hp2ps \
    hpc \
    hsc2hs \
    runghc \
    unlit \
    haddock

# TODO: this won't work for musl stage3
STAGE3_LIBRARIES = \
	array \
	base \
	binary \
	bytestring \
	Cabal \
	Cabal-syntax \
	containers \
	deepseq \
	directory \
	exceptions \
	file-io \
	filepath \
	ghc \
	ghc-bignum \
	ghc-boot \
	ghc-boot-th \
	ghc-compact \
	ghc-experimental \
	ghc-heap \
	ghci \
	ghc-internal \
	ghc-platform \
	ghc-prim \
	hpc \
	integer-gmp \
	mtl \
	os-string \
	parsec \
	pretty \
	process \
	rts \
	rts:nonthreaded-nodebug \
	rts-fs \
	rts-headers \
	semaphore-compat \
	stm \
	template-haskell \
	text \
	time \
	transformers \
	unix \
	xhtml

STAGE3_x86_64-musl-linux_AR                 = x86_64-unknown-linux-musl-ar
STAGE3_x86_64-musl-linux_CC                 = x86_64-unknown-linux-musl-gcc
STAGE3_x86_64-musl-linux_CC_OPTS            =
STAGE3_x86_64-musl-linux_CXX                = x86_64-unknown-linux-musl-g++
STAGE3_x86_64-musl-linux_CXX_OPTS           =
STAGE3_x86_64-musl-linux_EXTRA_INCLUDE_DIRS =
STAGE3_x86_64-musl-linux_EXTRA_LIB_DIRS     =
STAGE3_x86_64-musl-linux_LD                 = x86_64-unknown-linux-musl-ld
STAGE3_x86_64-musl-linux_RANLIB             = x86_64-unknown-linux-musl-ranlib
STAGE3_x86_64-musl-linux_GHC_TOOLCHAIN_ARGS = $(GHC_TOOLCHAIN_ARGS)

STAGE3_javascript-unknown-ghcjs_AR                 = emar
STAGE3_javascript-unknown-ghcjs_CC                 = emcc
STAGE3_javascript-unknown-ghcjs_CC_OPTS            =
STAGE3_javascript-unknown-ghcjs_CXX                = em++
STAGE3_javascript-unknown-ghcjs_CXX_OPTS           =
STAGE3_javascript-unknown-ghcjs_EXTRA_INCLUDE_DIRS =
STAGE3_javascript-unknown-ghcjs_EXTRA_LIB_DIRS     =
STAGE3_javascript-unknown-ghcjs_LD                 = emcc
STAGE3_javascript-unknown-ghcjs_NM                 = emnm
STAGE3_javascript-unknown-ghcjs_RANLIB             = emranlib
STAGE3_javascript-unknown-ghcjs_STRIP              = emstrip
STAGE3_javascript-unknown-ghcjs_GHC_TOOLCHAIN_ARGS = $(GHC_TOOLCHAIN_ARGS) --disable-tables-next-to-code

STAGE3_wasm32-unknown-wasi_CC                 = wasm32-wasi-clang
STAGE3_wasm32-unknown-wasi_CC_OPTS            = -fno-strict-aliasing -Wno-error=int-conversion -Oz -msimd128 -mnontrapping-fptoint -msign-ext -mbulk-memory -mmutable-globals -mmultivalue -mreference-types
STAGE3_wasm32-unknown-wasi_CXX                = wasm32-wasi-clang++
STAGE3_wasm32-unknown-wasi_CXX_OPTS           = $(STAGE3_wasm32-unknown-wasi_CC_OPTS)
STAGE3_wasm32-unknown-wasi_EXTRA_INCLUDE_DIRS =
STAGE3_wasm32-unknown-wasi_EXTRA_LIB_DIRS     =
STAGE3_wasm32-unknown-wasi_GHC_TOOLCHAIN_ARGS = $(GHC_TOOLCHAIN_ARGS) --merge-objs wasm-ld --merge-objs-opt="-r" --disable-tables-next-to-code


TARGET_DIR = $(DIST_DIR)/lib/targets/$(TARGET_PLATFORM)

# NOTE: disable-library-for-ghci is repeated here but it should be sufficient
# to put it in cabal.project.stage3

define stage3

STAGE3_$(1)_CABAL_BUILD = \
	env \
	DERIVE_CONSTANTS=$$(call NORMALIZE_FP,$$(CURDIR)/$$(STAGE1_PATH)/bin/deriveConstants) \
	GENAPPLY=$$(call NORMALIZE_FP,$$(CURDIR)/$$(STAGE1_PATH)/bin/genapply) \
	NM=$$(STAGE3_$(1)_NM) \
	OBJDUMP=$$(STAGE3_$(1)_OBJDUMP) \
	$$(CABAL_BUILD) \
	--with-compiler=$$(call NORMALIZE_FP,$$(CURDIR)/$$(DIST_DIR)/bin/$(1)-ghc) \
	--with-build-compiler=$$(call NORMALIZE_FP,$$(CURDIR)/$$(DIST_DIR)/bin/ghc) \
	--ghc-options "-ghcversion-file=$$(call NORMALIZE_FP,$$(CURDIR)/rts/include/ghcversion.h)" \
	--with-hsc2hs=$$(call NORMALIZE_FP,$$(CURDIR)/$$(DIST_DIR)/bin/$(1)-hsc2hs) \
	--hsc2hs-options='-x' \
	--with-gcc $$(STAGE3_$(1)_CC) \
	$$(foreach dir,$$(STAGE3_$(1)_EXTRA_LIB_DIRS),--extra-lib-dirs=$$(dir)) \
	$$(foreach dir,$$(STAGE3_$(1)_EXTRA_INCLUDE_DIRS),--extra-include-dirs=$$(dir))

.PHONY: stage3-$(1)
stage3-$(1): STAGE=stage3
stage3-$(1): TARGET_PLATFORM=$(1)
stage3-$(1): $(GHC2) $$(STAGE1_PATH)/bin/ghc-toolchain-bin $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES) libraries/ghc-boot-th-next cabal.project.common cabal.project.stage3 stage3-$(1)-additional-files
	$$(call LOG,Linking executables)
	$$(foreach exe,$$(STAGE3_EXECUTABLES),$(LN_SF) $$(exe) $(DIST_DIR)/bin/$(1)-$$(exe);)

	@mkdir -p $$(TARGET_DIR)/lib
	$$(STAGE1_PATH)/bin/ghc-toolchain-bin \
		--output-settings \
		--output $$(TARGET_DIR)/lib/settings \
		--triple $(1) \
		--cc $$(STAGE3_$(1)_CC) \
		$$(foreach opt,$$(STAGE3_$(1)_CC_OPTS),--cc-opt=$$(opt)) \
		--cxx $$(STAGE3_$(1)_CXX) \
		$$(foreach opt,$$(STAGE3_$(1)_CXX_OPTS),--cxx-opt=$$(opt)) \
		$(if $(STAGE3_$(1)_AR),--ar $$(STAGE3_$(1)_AR),) \
		$(if $(STAGE3_$(1)_LD),--ld $$(STAGE3_$(1)_LD),) \
		$(if $(STAGE3_$(1)_ND),--nm $$(STAGE3_$(1)_NM),) \
		$(if $(STAGE3_$(1)_RANLIB),--ranlib $$(STAGE3_$(1)_RANLIB),) \
		--disable-ld-override \
		$$(STAGE3_$(1)_GHC_TOOLCHAIN_ARGS)

	$$(DIST_DIR)/bin/$(1)-ghc --info

	@rm -rf $$(TARGET_DIR)/lib/package.conf.d
	$$(DIST_DIR)/bin/$(1)-ghc-pkg init $$(TARGET_DIR)/lib/package.conf.d

	$$(call LOG,Building library rts:nonthreaded-nodebug)
	$$(STAGE3_$(1)_CABAL_BUILD) rts:nonthreaded-nodebug

	$$(call LOG,Building libraries $(STAGE3_LIBRARIES))
	$$(STAGE3_$(1)_CABAL_BUILD) $(filter-out rts%,$(STAGE3_LIBRARIES))

	$$(call LOG,Copying libraries into distribution for target $(1))
	@mkdir -p $$(TARGET_DIR)/lib/package.conf.d
	@mkdir -p $$(TARGET_DIR)/lib/$(1)
	$$(call DIST_COPY_LIBS_CROSS,$(STAGE3_LIBRARIES),$(1))
	$$(call DIST_COPY_LIBS_SO_CROSS)
	$$(call DIST_COPY_LIBS_CONF_CROSS,$(STAGE3_LIBRARIES),$(1))

	$(call LOG,Refreshing $$(TARGET_DIR)/lib/package.conf.d cache)
	@$(DIST_DIR)/bin/$(1)-ghc-pkg recache --package-db $$(CURDIR)/$$(TARGET_DIR)/lib/package.conf.d

	$(call LOG,Verifying $$(TARGET_DIR)/lib/package.conf.d)
	@$(DIST_DIR)/bin/$(1)-ghc-pkg check --package-db $$(CURDIR)/$$(TARGET_DIR)/lib/package.conf.d

	$$(call LOG,Copying ghc-usage files)
	@cp -rfp driver/ghc-usage.txt $$(TARGET_DIR)/lib/
	@cp -rfp driver/ghci-usage.txt $$(TARGET_DIR)/lib/

$(DIST_DIR)/ghc-$(1).tar.gz: stage3-$(1)
	@echo "::group::Creating ghc-$(1).tar.gz..."
	tar czf $$@ \
		--directory=$$(DIST_DIR) \
		$(foreach exe,$(STAGE3_EXECUTABLES),bin/$(1)-$(exe)$(EXE_EXT)) \
		lib/targets/$(1)
	@echo "::endgroup::"

endef

stage3-javascript-unknown-ghcjs-additional-files: STAGE=stage3
stage3-javascript-unknown-ghcjs-additional-files: TARGET_PLATFORM=javascript-unknown-ghcjs
stage3-javascript-unknown-ghcjs-additional-files:
	@mkdir -p $(TARGET_DIR)/lib/
	$(call LOG,Copying dyld.mjs)
	@cp -f utils/jsffi/dyld.mjs $(TARGET_DIR)/lib/dyld.mjs
	$(call LOG,Copying ghc-interp.js)
	@cp -f ghc-interp.js $(TARGET_DIR)/lib/ghc-interp.js
	$(call LOG,Copying post-link.mjs)
	@cp -f utils/jsffi/post-link.mjs $(TARGET_DIR)/lib/post-link.mjs
	$(call LOG,Copying prelude.mjs)
	@cp -f utils/jsffi/prelude.mjs $(TARGET_DIR)/lib/prelude.mjs

stage3-wasm32-unknown-wasi-additional-files: STAGE=stage3
stage3-wasm32-unknown-wasi-additional-files: TARGET_PLATFORM=wasm32-unknown-wasi
stage3-wasm32-unknown-wasi-additional-files:
	@mkdir -p $(TARGET_DIR)/lib/
	$(call LOG,Copying dyld.mjs)
	@cp -f utils/jsffi/dyld.mjs $(TARGET_DIR)/lib/dyld.mjs
	$(call LOG,Copying ghc-interp.js)
	@cp -f ghc-interp.js $(TARGET_DIR)/lib/ghc-interp.js
	$(call LOG,Copying post-link.mjs)
	@cp -f utils/jsffi/post-link.mjs $(TARGET_DIR)/lib/post-link.mjs
	$(call LOG,Copying prelude.mjs)
	@cp -f utils/jsffi/prelude.mjs $(TARGET_DIR)/lib/prelude.mjs

stage3-x86_64-musl-linux-additional-files: STAGE=stage3
stage3-x86_64-musl-linux-additional-files: TARGET_PLATFORM=x86_64-musl-linux
stage3-x86_64-musl-linux-additional-files:
	$(call LOG,No additional files to be copied)


$(foreach platform,$(STAGE3_PLATFORMS),$(eval $(call stage3,$(platform))))

stage3: $(foreach platform,$(STAGE3_PLATFORMS),stage3-$(platform))

#  ____  _           _ _     _
# | __ )(_)_ __   __| (_)___| |_ ___
# |  _ \| | '_ \ / _` | / __| __/ __|
# | |_) | | | | | (_| | \__ \ |_\__ \
# |____/|_|_| |_|\__,_|_|___/\__|___/
#

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
      rts/RtsToHsIface.h \
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
  $(call copy_rts_h,$1)
  if [ "$2" = "javascript-unknown-ghcjs" ] ; then $(call copy_rts_js_h,$1) ; fi
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
$(abspath _build/stage0/bin/cabal$(EXE_EXT)): _build/stage0/bin/cabal$(EXE_EXT)

# --- Stage 0 build ---

# This just builds cabal-install, which is used to build the rest of the project.

# We need an absolute path here otherwise cabal will consider the path relative to `the project directory
_build/stage0/bin/cabal$(EXE_EXT): BUILD_ARGS=-j -w $(GHC0) --disable-tests --project-dir libraries/Cabal --builddir=$(abspath _build/stage0) --ghc-options="-fhide-source-paths"
_build/stage0/bin/cabal$(EXE_EXT):
	@echo "::group::Building Cabal..."
	@mkdir -p _build/stage0/bin _build/logs
	cabal build $(BUILD_ARGS) cabal-install:exe:cabal
	cp -rfp $(shell cabal list-bin -v0 $(BUILD_ARGS) cabal-install:exe:cabal | $(CYGPATH)) $@
	@echo "::endgroup::"

# --- Stage 1 build ---

_build/stage1/%: private STAGE=stage1
_build/stage1/%: private GHC=$(GHC0)

.PHONY: cabal.project.stage1.local

cabal.project.stage1.local: cabal.project.stage1
ifeq ($(OS),Windows_NT)
	echo "extra-prog-path: $(shell echo '$(GHC_LIBDIR)' | $(CYGPATH_MIXED))/../mingw/bin" > $@
else
	echo "" > $@
endif

.PHONY: $(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES))
$(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES)) &: private TARGET_PLATFORM=
$(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES)) &: $(CABAL) $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES) libraries/ghc-boot-th-next/ghc-boot-th-next.cabal cabal.project.stage1 cabal.project.stage1.local
	@echo "::group::Building stage1 executables ($(STAGE1_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage1/cache
	$(CABAL_BUILD) $(STAGE1_TARGETS)
	@echo "::endgroup::"

_build/stage1/lib/settings: _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT)
	@echo "::group::Creating settings for $(TARGET_TRIPLE)..."
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin$(EXE_EXT) $(GHC_TOOLCHAIN_ARGS) --triple $(TARGET_TRIPLE) --output-settings -o $@ --cc $(CC) --cxx $(CXX) --cc-link-opt "$(CC_LINK_OPT)"
ifeq ($(DYNAMIC),1)
	$(SED) -i -e 's/"RTS ways","/"RTS ways","dyn debug_dyn thr_dyn thr_debug_dyn /' $@
endif
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
_build/stage1/lib/package.conf.d/package.cache: _build/stage1/bin/ghc-pkg$(EXE_EXT) _build/stage1/lib/settings
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
_build/stage2/%: private GHC=$(realpath _build/stage1/bin/ghc$(EXE_EXT))

.PHONY: $(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES))
$(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES)) &: private TARGET_PLATFORM=
$(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES)) &: $(CABAL) stage1 cabal.project.stage2 stage2-rts
	@echo "::group::Building stage2 executables ($(STAGE2_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage2/cache
	GHC=$(GHC) \
		PATH='$(PWD)/_build/stage1/bin:$(PATH)' \
		$(CABAL_BUILD) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" -W $(GHC0) $(STAGE2_TARGETS)
	@echo "::endgroup::"

.PHONY: stage2-rts
stage2-rts: private STAGE=stage2
stage2-rts: private GHC=$(realpath _build/stage1/bin/ghc$(EXE_EXT))
stage2-rts: private TARGET_PLATFORM=
stage2-rts: $(CABAL) stage1 cabal.project.stage2
	@echo "::group::Building stage2 RTSes..."
	# Force cabal to replan
	rm -rf _build/stage2/cache
	GHC=$(GHC) \
		PATH='$(PWD)/_build/stage1/bin:$(PATH)' \
		$(CABAL_BUILD) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" -W $(GHC0) $(STAGE2_UTIL_RTS)
	@echo "::endgroup::"


# Do we want to build these with the stage2 GHC or the stage1 GHC?
# Traditionally we build them with the stage1 ghc, but we could just as well
# build them with the stage2 ghc; seems like a better/cleaner idea to me (moritz).
.PHONY: $(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES))
$(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES)) &: private TARGET_PLATFORM=
$(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES)) &: $(CABAL) stage1 cabal.project.stage2.settings stage2-rts
	@echo "::group::Building stage2 utilities ($(STAGE2_UTIL_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage2/cache
	GHC=$(GHC) \
		PATH='$(PWD)/_build/stage1/bin:$(PATH)' \
		$(CABAL_BUILD) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" -W $(GHC0) $(STAGE2_UTIL_TARGETS)
	@echo "::endgroup::"

_build/stage2/lib/settings: _build/stage1/lib/settings
	@mkdir -p $(@D)
	cp -rfp _build/stage1/lib/settings _build/stage2/lib/settings

_build/stage2/lib/package.conf.d/package.cache: _build/stage2/bin/ghc-pkg$(EXE_EXT) _build/stage2/lib/settings
	@echo "::group::Creating stage2 package cache..."
	@mkdir -p _build/stage2/lib/package.conf.d
	@mkdir -p _build/stage2/lib/$(HOST_PLATFORM)
	@find $(CURDIR)/_build/stage2/build/host/*/ghc-*/ -type f -name '*.so' -exec mv '{}' $(CURDIR)/_build/stage2/lib/$(HOST_PLATFORM)/ \;
	@rm -rf _build/stage2/lib/package.conf.d/*
	cp -rfp _build/stage2/packagedb/host/*/* _build/stage2/lib/package.conf.d
	LD_LIBRARY_PATH=$(CURDIR)/_build/stage2/lib/$(HOST_PLATFORM) _build/stage2/bin/ghc-pkg$(EXE_EXT) recache
	@echo "::endgroup::"

_build/stage2/lib/template-hsc.h: utils/hsc2hs/data/template-hsc.h
	@mkdir -p $(@D)
	cp -rfp $< $@

.PHONY: stage2
stage2: $(addprefix _build/stage2/bin/,$(STAGE2_EXECUTABLES)) _build/stage2/lib/settings _build/stage2/lib/package.conf.d/package.cache _build/stage2/lib/template-hsc.h

# --- Stage 3 generic ---

_build/stage2/lib/targets/% _build/stage3/lib/targets/%:
	@mkdir -p _build/stage3/lib/targets/$(@F)
	@rm -f _build/stage2/lib/targets/$(@F)
	@mkdir -p _build/stage2/lib/targets/
	@ln -sf ../../../stage3/lib/targets/$(@F) _build/stage2/lib/targets/$(@F)

_build/stage3/bin/%-ghc-pkg$(EXE_EXT): _build/stage2/bin/ghc-pkg$(EXE_EXT)
	@mkdir -p $(@D)
	@ln -sf ../../stage2/bin/ghc-pkg$(EXE_EXT) $@

_build/stage3/bin/%-ghc$(EXE_EXT): _build/stage2/bin/ghc$(EXE_EXT)
	@mkdir -p $(@D)
	@ln -sf ../../stage2/bin/ghc$(EXE_EXT) $@

_build/stage3/bin/%-hsc2hs$(EXE_EXT): _build/stage2/bin/hsc2hs$(EXE_EXT)
	@mkdir -p $(@D)
	@ln -sf ../../stage2/bin/hsc2hs$(EXE_EXT) $@

_build/stage3/lib/targets/%/lib/package.conf.d: _build/stage3/lib/targets/%
	@mkdir -p $@

# ghc-toolchain borks unlit
_build/stage3/lib/targets/%/bin/unlit$(EXE_EXT): _build/stage2/bin/unlit$(EXE_EXT)
	@mkdir -p $(@D)
	cp -rfp $< $@

_build/stage3/lib/targets/%/lib/dyld.mjs:
	@mkdir -p $(@D)
	@cp -f utils/jsffi/dyld.mjs $@
	@chmod +x $@

_build/stage3/lib/targets/%/lib/post-link.mjs:
	@mkdir -p $(@D)
	@cp -f utils/jsffi/post-link.mjs $@
	@chmod +x $@

_build/stage3/lib/targets/%/lib/prelude.mjs:
	@mkdir -p $(@D)
	@cp -f utils/jsffi/prelude.mjs $@
	@chmod +x $@

_build/stage3/lib/targets/%/lib/ghc-interp.js:
	@mkdir -p $(@D)
	@cp -f ghc-interp.js $@

# $1 = TIPLET
define build_cross
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) GHC=$(GHC) \
		PATH=$(PWD)/_build/stage2/bin:$(PWD)/_build/stage3/bin:$(PATH) \
		$(CABAL_BUILD) -W $(GHC2) --happy-options="--template=$(abspath _build/stage2/src/happy-lib-2.1.5/data/)" --with-hsc2hs=$1-hsc2hs --hsc2hs-options='-x' --configure-option='--host=$1' \
		$(foreach lib,$(CROSS_EXTRA_LIB_DIRS),--extra-lib-dirs=$(lib)) \
		$(foreach include,$(CROSS_EXTRA_INCLUDE_DIRS),--extra-include-dirs=$(include)) \
		$(STAGE3_LIBS)
endef

# --- Stage 3 javascript build ---

.PHONY: stage3-javascript-unknown-ghcjs
stage3-javascript-unknown-ghcjs: _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings javascript-unknown-ghcjs-libs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d/package.cache _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/dyld.mjs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/post-link.mjs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/prelude.mjs _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/ghc-interp.js

_build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings: _build/stage2/lib/targets/javascript-unknown-ghcjs _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT)
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin$(EXE_EXT) $(GHC_TOOLCHAIN_ARGS) --triple javascript-unknown-ghcjs --output-settings -o $@ --cc $(EMCC) --cxx $(EMCXX) --ar $(EMAR) --ranlib $(EMRANLIB)

_build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d/package.cache: private LD_LIBRARY_PATH=$(CURDIR)/_build/stage2/lib/$(HOST_PLATFORM)
_build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d/package.cache: _build/stage3/bin/javascript-unknown-ghcjs-ghc-pkg$(EXE_EXT) _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings javascript-unknown-ghcjs-libs
	@mkdir -p $(@D)
	@rm -rf $(@D)/*
	cp -rfp _build/stage3/javascript-unknown-ghcjs/packagedb/host/*/* $(@D)
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) _build/stage3/bin/javascript-unknown-ghcjs-ghc-pkg$(EXE_EXT) recache

.PHONY: javascript-unknown-ghcjs-libs
javascript-unknown-ghcjs-libs: private LD_LIBRARY_PATH=$(CURDIR)/_build/stage2/lib/$(HOST_PLATFORM)
javascript-unknown-ghcjs-libs: private GHC=$(abspath _build/stage3/bin/javascript-unknown-ghcjs-ghc$(EXE_EXT))
javascript-unknown-ghcjs-libs: private GHC2=$(abspath _build/stage2/bin/ghc$(EXE_EXT))
javascript-unknown-ghcjs-libs: private STAGE=stage3
javascript-unknown-ghcjs-libs: private CC=emcc
javascript-unknown-ghcjs-libs: private CROSS_EXTRA_LIB_DIRS=$(JS_EXTRA_LIB_DIRS)
javascript-unknown-ghcjs-libs: private CROSS_EXTRA_INCLUDE_DIRS=$(JS_EXTRA_INCLUDE_DIRS)
javascript-unknown-ghcjs-libs: cabal.project.stage3 _build/stage3/bin/javascript-unknown-ghcjs-ghc-pkg$(EXE_EXT) _build/stage3/bin/javascript-unknown-ghcjs-ghc$(EXE_EXT) _build/stage3/bin/javascript-unknown-ghcjs-hsc2hs$(EXE_EXT) _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/settings _build/stage3/lib/targets/javascript-unknown-ghcjs/bin/unlit$(EXE_EXT) _build/stage3/lib/targets/javascript-unknown-ghcjs/lib/package.conf.d
	$(call build_cross,javascript-unknown-ghcjs)

# --- Stage 3 musl build ---

.PHONY: stage3-x86_64-musl-linux
stage3-x86_64-musl-linux: x86_64-musl-linux-libs _build/stage3/lib/targets/x86_64-musl-linux/lib/package.conf.d/package.cache

_build/stage3/lib/targets/x86_64-musl-linux/lib/settings: _build/stage2/lib/targets/x86_64-musl-linux _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT)
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin$(EXE_EXT) $(GHC_TOOLCHAIN_ARGS) --triple x86_64-musl-linux --output-settings -o $@ --cc x86_64-unknown-linux-musl-cc --cxx x86_64-unknown-linux-musl-c++ --ar x86_64-unknown-linux-musl-ar --ranlib x86_64-unknown-linux-musl-ranlib --ld x86_64-unknown-linux-musl-ld

_build/stage3/lib/targets/x86_64-musl-linux/lib/package.conf.d/package.cache: _build/stage3/bin/x86_64-musl-linux-ghc-pkg$(EXE_EXT) _build/stage3/lib/targets/x86_64-musl-linux/lib/settings x86_64-musl-linux-libs
	@mkdir -p $(@D)
	@rm -rf $(@D)/*
	cp -rfp _build/stage3/x86_64-musl-linux/packagedb/host/*/* $(@D)
	_build/stage3/bin/x86_64-musl-linux-ghc-pkg$(EXE_EXT) recache

.PHONY: x86_64-musl-linux-libs
x86_64-musl-linux-libs: private LD_LIBRARY_PATH=$(CURDIR)/_build/stage2/lib/$(HOST_PLATFORM)
x86_64-musl-linux-libs: private GHC=$(abspath _build/stage3/bin/x86_64-musl-linux-ghc$(EXE_EXT))
x86_64-musl-linux-libs: private GHC2=$(abspath _build/stage2/bin/ghc$(EXE_EXT))
x86_64-musl-linux-libs: private STAGE=stage3
x86_64-musl-linux-libs: private CC=x86_64-unknown-linux-musl-cc
x86_64-musl-linux-libs: private CROSS_EXTRA_LIB_DIRS=$(MUSL_EXTRA_LIB_DIRS)
x86_64-musl-linux-libs: private CROSS_EXTRA_INCLUDE_DIRS=$(MUSL_EXTRA_INCLUDE_DIRS)
x86_64-musl-linux-libs: _build/stage3/bin/x86_64-musl-linux-ghc-pkg$(EXE_EXT) _build/stage3/bin/x86_64-musl-linux-ghc$(EXE_EXT) _build/stage3/bin/x86_64-musl-linux-hsc2hs$(EXE_EXT) _build/stage3/lib/targets/x86_64-musl-linux/lib/settings _build/stage3/lib/targets/x86_64-musl-linux/bin/unlit$(EXE_EXT) _build/stage3/lib/targets/x86_64-musl-linux/lib/package.conf.d
	$(call build_cross,x86_64-musl-linux)

# --- Stage 3 wasm build ---

.PHONY: stage3-wasm32-unknown-wasi
stage3-wasm32-unknown-wasi: wasm32-unknown-wasi-libs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache _build/stage3/lib/targets/wasm32-unknown-wasi/lib/dyld.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/post-link.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/prelude.mjs _build/stage3/lib/targets/wasm32-unknown-wasi/lib/ghc-interp.js

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings: _build/stage2/lib/targets/wasm32-unknown-wasi _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT)
	@mkdir -p $(@D)
	PATH=/home/hasufell/.ghc-wasm/wasi-sdk/bin:$(PATH) _build/stage1/bin/ghc-toolchain-bin$(EXE_EXT) $(GHC_TOOLCHAIN_ARGS) --triple wasm32-unknown-wasi --output-settings -o $@ --cc wasm32-wasi-clang --cxx wasm32-wasi-clang++ --ar ar --ranlib ranlib --ld wasm-ld --merge-objs wasm-ld --merge-objs-opt="-r" --disable-ld-override --disable-tables-next-to-code $(foreach opt,$(WASM_CC_OPTS),--cc-opt=$(opt)) $(foreach opt,$(WASM_CXX_OPTS),--cxx-opt=$(opt))

_build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d/package.cache: _build/stage3/bin/wasm32-unknown-wasi-ghc-pkg$(EXE_EXT) _build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings wasm32-unknown-wasi-libs
	@mkdir -p $(@D)
	@rm -rf $(@D)/*
	cp -rfp _build/stage3/wasm32-unknown-wasi/packagedb/host/*/* $(@D)
	_build/stage3/bin/wasm32-unknown-wasi-ghc-pkg$(EXE_EXT) recache

.PHONY: wasm32-unknown-wasi-libs
wasm32-unknown-wasi-libs: private LD_LIBRARY_PATH=$(CURDIR)/_build/stage2/lib/$(HOST_PLATFORM)
wasm32-unknown-wasi-libs: private GHC=$(abspath _build/stage3/bin/wasm32-unknown-wasi-ghc$(EXE_EXT))
wasm32-unknown-wasi-libs: private GHC2=$(abspath _build/stage2/bin/ghc$(EXE_EXT))
wasm32-unknown-wasi-libs: private STAGE=stage3
wasm32-unknown-wasi-libs: private CC=wasm32-wasi-clang
wasm32-unknown-wasi-libs: private CROSS_EXTRA_LIB_DIRS=$(WASM_EXTRA_LIB_DIRS)
wasm32-unknown-wasi-libs: private CROSS_EXTRA_INCLUDE_DIRS=$(WASM_EXTRA_INCLUDE_DIRS)
wasm32-unknown-wasi-libs: cabal.project.stage3 _build/stage3/bin/wasm32-unknown-wasi-ghc-pkg$(EXE_EXT) _build/stage3/bin/wasm32-unknown-wasi-ghc$(EXE_EXT) _build/stage3/bin/wasm32-unknown-wasi-hsc2hs$(EXE_EXT) _build/stage3/lib/targets/wasm32-unknown-wasi/lib/settings _build/stage3/lib/targets/wasm32-unknown-wasi/bin/unlit$(EXE_EXT) _build/stage3/lib/targets/wasm32-unknown-wasi/lib/package.conf.d
	$(call build_cross,wasm32-unknown-wasi)

# --- Bindist ---

RTS_SUBLIBS := \
  nonthreaded-nodebug \
  nonthreaded-debug \
  threaded-nodebug \
  threaded-debug

# patchpackageconf
#
# Hacky function to patch up the paths in the package .conf files
#
# $1 = package name (ex: 'bytestring')
# TODO: package name is borked for sublibs
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
    case $5 in \
		rts-*-nonthreaded-nodebug) \
	      sublib="/nonthreaded-nodebug" ;; \
		rts-*-nonthreaded-debug) \
	      sublib="/nonthreaded-debug" ;; \
		rts-*-threaded-nodebug) \
	      sublib="/threaded-nodebug" ;; \
		rts-*-threaded-debug) \
	      sublib="/threaded-debug" ;; \
		*) \
		  sublib="" ;; \
	esac ; \
	$(SED) -i \
		-e "s|haddock-interfaces:.*|haddock-interfaces: \"\$${pkgroot}/$3/html/libraries/$5/$1.haddock\"|" \
		-e "s|haddock-html:.*|haddock-html: \"\$${pkgroot}/$3/html/libraries/$5\"|" \
        -e "s|import-dirs:.*|import-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|library-dirs:.*|library-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|library-dirs-static:.*|library-dirs-static: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|dynamic-library-dirs:.*|dynamic-library-dirs: \"\$${pkgroot}/../lib/$4\"|" \
		-e "s|data-dir:.*|data-dir: \"\$${pkgroot}/../lib/$4/$5$${sublib}\"|" \
		-e "s|include-dirs:.*|include-dirs: \"\$${pkgroot}/../lib/$4/$5$${sublib}/include\"|" \
		-e "s|^    /.*||" \
		-e "s|^    [A-Z]:.*||" \
		$2
endef

# $1 = triplet
define copycrosslib
	@cp -rfp _build/stage3/lib/targets/$1 _build/bindist/lib/targets/
	@ffi_incdir=`LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) $(CURDIR)/_build/bindist/bin/$1-ghc-pkg$(EXE_EXT) field libffi-clib include-dirs | grep '/libffi-clib/src/' | sed 's|.*$(CURDIR)/||' || echo "none"` ; cd _build/bindist/lib/targets/$1/lib/package.conf.d ; \
		for pkg in *.conf ; do \
		  pkgname=`echo $${pkg} | $(SED) 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
		  pkgnamever=`echo $${pkg} | $(SED) 's/\.conf//'` ; \
		  mkdir -p $(CURDIR)/_build/bindist/lib/targets/$1/lib/$1/$${pkg%.conf} && \
	      cp -rfp $(CURDIR)/_build/stage3/$1/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/_build/bindist/lib/targets/$1/lib/$1/$${pkg%.conf}/ && \
	      if [ $${pkgname} = "libffi-clib" ] ; then \
		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$1,$${pkgnamever}) ; \
	      else \
		    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$1,$${pkgnamever}) ; \
	      fi ; \
		done ; \
		if [ $${ffi_incdir} != "none" ] ; then $(call copy_headers,ffitarget.h,$(CURDIR)/$${ffi_incdir},libffi-clib,LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) $(CURDIR)/_build/bindist/bin/$1-ghc-pkg$(EXE_EXT)) ; fi
endef

# $1 = rpath
# $2 = binary
# set rpath relative to the current executable
# TODO: on darwin, this doesn't overwrite rpath, but just adds to it,
#       so we'll have the old rpaths from the build host in there as well
# set_rpath: Add rpath to binary. On Darwin, check if rpath already exists
# before adding (install_name_tool fails if rpath is duplicate).
define set_rpath
	$(if $(filter Darwin,$(UNAME)), \
		if ! otool -l "$(2)" 2>/dev/null | grep -A2 'LC_RPATH' | grep -q "@executable_path/$(1)"; then \
			$(INSTALL_NAME_TOOL) -add_rpath "@executable_path/$(1)" "$(2)"; \
		fi, \
		$(PATCHELF) --force-rpath --set-rpath "\$$ORIGIN/$(1)" "$(2)")
endef

# Target for creating the final binary distribution directory
_build/bindist: private LD_LIBRARY_PATH=$(CURDIR)/_build/bindist/lib/$(HOST_PLATFORM)
_build/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
	@echo "::group::Creating binary distribution in $@"
	@mkdir -p $@/bin
	@mkdir -p $@/lib
	# Copy executables from stage2 bin
	@cp -rfp _build/stage2/bin/* $@/bin/
	# Copy libraries and settings from stage2 lib
	@cp -rfp _build/stage2/lib/{package.conf.d,settings,template-hsc.h,$(HOST_PLATFORM)} $@/lib/
	@mkdir -p $@/lib/$(HOST_PLATFORM)
	@ffi_incdir=`LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) $(CURDIR)/$@/bin/ghc-pkg$(EXE_EXT) field libffi-clib include-dirs | grep 'libffi-clib[/\\]src/' | sed 's/^[ \t]*//' | $(CYGPATH) | sed 's|.*$(CURDIR)/||'` ; \
		cd $@/lib/package.conf.d ; \
			for pkg in *.conf ; do \
		  	pkgname=`echo $${pkg} | $(SED) 's/-[0-9.]*\(-[0-9a-zA-Z]*\)\?\.conf//'` ; \
		  	pkgnamever=`echo $${pkg} | $(SED) 's/\.conf//'` ; \
		  	mkdir -p $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
		  	cp -rfp $(CURDIR)/_build/stage2/build/host/*/ghc-*/$${pkg%.conf}/build/* $(CURDIR)/$@/lib/$(HOST_PLATFORM)/$${pkg%.conf} ; \
	      	if [ $${pkgname} = "libffi-clib" ] ; then \
			    $(call patchpackageconf,$${pkgname},$${pkg},../../..,$(HOST_PLATFORM),$${pkgnamever}) ; \
	    	  else \
		    	$(call patchpackageconf,$${pkgname},$${pkg},../../..,$(HOST_PLATFORM),$${pkgnamever}) ; \
	      	fi ; \
			done ; \
			$(call copy_headers,ffitarget.h,$(CURDIR)/$${ffi_incdir},libffi-clib,LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) $(CURDIR)/$@/bin/ghc-pkg$(EXE_EXT))
	# Copy driver usage files
	@cp -rfp driver/ghc-usage.txt $@/lib/
	@cp -rfp driver/ghci-usage.txt $@/lib/
	@echo "FIXME: Changing 'Support SMP' from YES to NO in settings file"
	@$(SED) 's/("Support SMP","YES")/("Support SMP","NO")/' -i.bck $@/lib/settings
	# Recache
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) $@/bin/ghc-pkg$(EXE_EXT) recache
	# Copy headers
	@$(call copy_all_stage2_h,LD_LIBRARY_PATH=$(LD_LIBRARY_PATH) $@/bin/ghc-pkg$(EXE_EXT))
	# Add basename symlinks for nested shared libs (.dylib, .so) in
	# lib/$(HOST_PLATFORM). Shared libraries may be installed in subdirectories
	# (e.g., lib/x86_64-linux/rts-1.0.3/). We create symlinks at the top level
	# so all shared libraries are in one folder.
	@if [ -d "$@/lib/$(HOST_PLATFORM)" ]; then \
	  cd "$@/lib/$(HOST_PLATFORM)" && \
	  find . -mindepth 2 \( -name "*.dylib" -o -name "*.so" \) -type f \
	    -exec sh -c 'ln -sf "$$1" "$$(basename "$$1")"' _ {} \; ; \
	fi
	# Create -dyn iserv executable (symlink so ghc can find ghc-iserv-dyn)
	@ln -sf ghc-iserv$(EXE_EXT) "$@/bin/ghc-iserv-dyn$(EXE_EXT)"
	# set rpath on executables
	@for binary in _build/bindist/bin/* ; do \
		$(call set_rpath,../lib/$(HOST_PLATFORM),$${binary}) ; \
	done
	# Patch rpath on shared libraries so they can find sibling .so files.
	# Build-time RUNPATH entries point to _build/stage2/build/... which won't
	# exist on other machines. Replace with $ORIGIN (Linux) or @loader_path (macOS).
	@if [ -d "$@/lib/$(HOST_PLATFORM)" ]; then \
		find "$@/lib/$(HOST_PLATFORM)" \( -name '*.so' -o -name '*.dylib' \) -type f | while read lib; do \
			$(if $(filter Darwin,$(UNAME)), \
				$(INSTALL_NAME_TOOL) -delete_rpath "$$lib" 2>/dev/null || true ; \
				$(INSTALL_NAME_TOOL) -add_rpath "@loader_path" "$$lib" 2>/dev/null || true, \
				$(PATCHELF) --force-rpath --set-rpath '$$ORIGIN' "$$lib") ; \
		done ; \
	fi
	@echo "::endgroup::"

_build/bindist/ghc.tar.gz: _build/bindist
	@tar czf $@ \
		--directory=$(DIST_DIR) \
		$(foreach exe,$(STAGE2_EXECUTABLES),bin/$(exe)$(EXE_EXT)) \
		$(shell if [ "$(DYNAMIC)" = 1 ] ; then echo "bin/ghc-iserv-dyn$(EXE_EXT)" ; fi) \
		lib/ghc-usage.txt \
		lib/ghci-usage.txt \
		lib/package.conf.d \
		lib/settings \
		lib/template-hsc.h \
		lib/$(HOST_PLATFORM)
	@echo "::endgroup::"

$(DIST_DIR)/cabal.tar.gz: $(CABAL)
	@echo "::group::Creating cabal.tar.gz..."
	@mkdir -p $(DIST_DIR)/bin
	@cp $< $(DIST_DIR)/bin/
	@tar czf $@ \
		--directory=$(DIST_DIR) \
		bin/cabal
	@echo "::endgroup::"

$(DIST_DIR)/haskell-toolchain.tar.gz: $(CABAL) stage2 stage3-javascript-unknown-ghcjs
	@echo "::group::Creating haskell-toolchain.tar.gz..."
	@mkdir -p $(DIST_DIR)/bin
	@cp $< $(DIST_DIR)/bin/
	@tar czf $@ \
		--directory=$(DIST_DIR) \
		$(foreach exe,$(STAGE2_EXECUTABLES),bin/$(exe)$(EXE_EXT)) \
		lib/ghc-usage.txt \
		lib/ghci-usage.txt \
		lib/package.conf.d \
		lib/settings \
		lib/template-hsc.h \
		lib/$(HOST_PLATFORM) \
		$(foreach exe,$(STAGE3_EXECUTABLES),bin/javascript-unknown-ghcjs-$(exe)$(EXE_EXT)) \
		lib/targets/javascript-unknown-ghcjs \
		bin/cabal
	@echo "::endgroup::"

$(DIST_DIR)/tests.tar.gz:
	@echo "::group::Creating tests.tar.gz..."
	@tar czf $@ \
		testsuite
	@echo "::endgroup::"

#  _   _            _
# | | | | __ _  ___| | ____ _  __ _  ___
# | |_| |/ _` |/ __| |/ / _` |/ _` |/ _ \
# |  _  | (_| | (__|   < (_| | (_| |  __/
# |_| |_|\__,_|\___|_|\_\__,_|\__, |\___|
#                             |___/

# .PHONY: hackage
hackage: $(BUILD_DIR)/packages/hackage.haskell.org/01-index.tar.gz

$(BUILD_DIR)/packages/hackage.haskell.org/01-index.tar.gz:
	$(CABAL) --remote-repo-cache $(call NORMALIZE_FP,$(CURDIR)/$(BUILD_DIR)/packages) update

#   ____             __ _
#  / ___|___  _ __  / _(_) __ _ _   _ _ __ ___
# | |   / _ \| '_ \| |_| |/ _` | | | | '__/ _ \
# | |__| (_) | | | |  _| | (_| | |_| | | |  __/
#  \____\___/|_| |_|_| |_|\__, |\__,_|_|  \___|
#                         |___/

$(CONFIGURE_SCRIPTS) : % : %.ac
	@echo ">>> Running autoreconf $(@D)"
	autoreconf $(@D)
	@echo "::endgroup::"

# Top level configure script.
#
# NOTE: configure scripts in packages with `Build-Type: Configure`
# are run by Cabal not here.
#
# We use --no-create to avoid regenerating files if not needed.
# Each configured file is tracked independently below.
config.status: configure
	@echo ">>> Running $(@D)/configure"
	$(@D)/configure --no-create $(GHC_CONFIGURE_ARGS)
	@echo "::endgroup::"

# Configured files are obtained from their .in counterparts via config.status
$(CONFIGURED_FILES) : % : ./config.status %.in
	./config.status $@

libraries/ghc-boot-th-next/%: libraries/ghc-boot-th/%
	@mkdir -p $(@D)
	@cp -v $< $@

libraries/ghc-boot-th-next/ghc-boot-th-next.cabal: libraries/ghc-boot-th/ghc-boot-th.cabal
	@echo "::group::Synthesizing ghc-boot-th-next (copy & sed from ghc-boot-th)..."
	@mkdir -p $(@D)
	@$(SED) -e 's/^name:[[:space:]]*ghc-boot-th$$/name:           ghc-boot-th-next/' $< > $@
	@echo "::endgroup::"

.PHONY: libraries/ghc-boot-th-next
libraries/ghc-boot-th-next: \
	libraries/ghc-boot-th-next/changelog.md \
	libraries/ghc-boot-th-next/LICENSE \
	libraries/ghc-boot-th-next/ghc-boot-th-next.cabal

# --- Clean Targets ---
clean-cabal: clean-stage0
clean-stage0:
	@echo "::group::Cleaning build artifacts..."
	rm -rf $(BUILD_DIR)/cabal
	rm -rf $(BUILD_DIR)/stage0
	rm -f $(STAGE0_STAMP)
	@echo "::endgroup::"

clean: clean-stage1 clean-stage2 clean-stage3
	@echo "Not removing stage0 (cabal), use clean-stage0 to remove cabal too."

clean-stage1:
	@echo "::group::Cleaning stage1 build artifacts..."
	rm -rf $(BUILD_DIR)/stage1
	rm -f $(STAGE1_STAMP)
	@echo "::endgroup::"

clean-stage2:
	@echo "::group::Cleaning stage2 build artifacts..."
	rm -rf $(BUILD_DIR)/stage2
	rm -f $(STAGE2_STAMP)
	@echo "::endgroup::"

clean-stage3:
	@echo "::group::Cleaning stage3 build artifacts..."
	rm -rf $(BUILD_DIR)/stage3
	@echo "::endgroup::"

distclean: clean
	@echo "::group::Cleaning all generated files (distclean)..."
	rm -rf autom4te.cache
	rm -f config.status config.log config.h aclocal.m4
	rm -f $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES)
	rm -rf libraries/ghc-boot-th-next
	@echo "::endgroup::"

# Default: skip performance tests (can override with SKIP_PERF_TESTS=NO)
SKIP_PERF_TESTS ?= YES
export SKIP_PERF_TESTS

# --- Test Suite Helper Tool Paths & Flags (Hadrian parity light) ---
# We approximate Hadrian's test invocation without depending on Hadrian.
# Bindist places test tools in $(BUILD_DIR)/bindist/bin (created by the bindist target).
TEST_TOOLS_DIR := $(BUILD_DIR)/bindist/bin
TEST_GHC       := $(TEST_TOOLS_DIR)/ghc
TEST_GHC_PKG   := $(TEST_TOOLS_DIR)/ghc-pkg
TEST_HP2PS     := $(TEST_TOOLS_DIR)/hp2ps
TEST_HPC       := $(TEST_TOOLS_DIR)/hpc
TEST_RUN_GHC   := $(TEST_TOOLS_DIR)/runghc

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

test: $(STAGE2_STAMP) testsuite-timeout
	$(call PHASE_START,test)
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
	METRICS_FILE='$(CURDIR)/$(BUILD_DIR)/test-perf.csv' \
	SUMMARY_FILE='$(CURDIR)/$(BUILD_DIR)/test-summary.txt' \
	JUNIT_FILE='$(CURDIR)/$(BUILD_DIR)/test-junit.xml' \
	SKIP_PERF_TESTS='$(SKIP_PERF_TESTS)' \
	THREADS='$(THREADS)' \
	$(MAKE) -C testsuite/tests test
	@echo "::endgroup::"

# Inform Make that these are not actual files if they get deleted by other means
.PHONY: clean clean-stage1 clean-stage2 clean-stage3 distclean test
