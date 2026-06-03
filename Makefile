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
.DEFAULT_GOAL := all

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

# Quiet mode: suppress output unless error (QUIET=1)
QUIET ?= 0

# Metrics collection: start background CPU/memory sampling (METRICS=1)
METRICS ?= 0
METRICS_INTERVAL ?= 0.5

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
ifeq ($(UNAME), FreeBSD)
TAR    ?= gtar
else
TAR    ?= tar
endif

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
$(STAGE0_STAMP): ; @$(MAKE) stable-cabal
$(STAGE1_STAMP): ; @$(MAKE) stage1
$(STAGE2_STAMP): ; @$(MAKE) stage2

# HOST_PLATFROM is always from the bootstrap compiler
HOST_PLATFORM := $(shell $(GHC0) --print-host-platform)

CABAL      ?= $(BUILD_DIR)/cabal/bin/cabal$(EXE_EXT)

STAGE1_PATH := $(let STAGE,stage1,$(STORE_DIR)/host/$(HOST_PLATFORM))
STAGE2_PATH := $(let STAGE,stage2,$(STORE_DIR)/host/$(HOST_PLATFORM))

GHC1        := $(STAGE1_PATH)/bin/ghc$(EXE_EXT)
GHC2        := $(STAGE2_PATH)/bin/ghc$(EXE_EXT)

#
# Misc settings
#

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

ifeq ($(QUIET),1)
LOG = @:
LOG_GROUP_START = @:
LOG_GROUP_END = @:
endif

# Phase timing: records start/end timestamps and status.
#
# Guards prevent overwrites when PHONY targets re-run without doing real work.
# For example, `test: stage2` triggers the PHONY stage2 recipe again; without
# guards, the no-op re-check would overwrite the real stage2 build timings with
# near-zero durations.
#
# Policy:
#   - If a phase already completed successfully (status=0), skip re-timing.
#   - If a phase failed (status=1) or never ran, (re)start timing.
define PHASE_START
	@mkdir -p $(TIMING_DIR)
	@if [ ! -f $(TIMING_DIR)/$(1).end ] || [ "$$(cat $(TIMING_DIR)/$(1).status 2>/dev/null)" != "0" ]; then \
		date +%s > $(TIMING_DIR)/$(1).start; \
		rm -f $(TIMING_DIR)/$(1).end $(TIMING_DIR)/$(1).status; \
	fi
endef

define PHASE_END_OK
	@if [ ! -f $(TIMING_DIR)/$(1).end ]; then \
		date +%s > $(TIMING_DIR)/$(1).end; \
		echo "0" > $(TIMING_DIR)/$(1).status; \
	fi
endef

define PHASE_END_FAIL
	@if [ ! -f $(TIMING_DIR)/$(1).end ]; then \
		date +%s > $(TIMING_DIR)/$(1).end; \
		echo "1" > $(TIMING_DIR)/$(1).status; \
	fi
endef

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

define CABAL_INSTALL_STAGE0
	$(CABAL0) \
		--store-dir $(call NORMALIZE_FP,$(CURDIR)/$(STORE_DIR)) \
		--logs-dir $(call NORMALIZE_FP,$(CURDIR)/$(LOGS_DIR)) \
	install \
		--installdir $(dir $(CABAL)) \
		--builddir $(call NORMALIZE_FP,$(CURDIR)/$(STAGE_DIR)) \
		--project-file cabal.project.$(STAGE) \
		--overwrite-policy=always --install-method=copy \
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

.PHONY: timing-summary
timing-summary:
	@./mk/timing-summary.sh $(TIMING_DIR)

.PHONY: metrics-start metrics-stop metrics-plot
metrics-start:
	@./mk/collect-metrics.sh start $(METRICS_DIR) $(METRICS_INTERVAL)

metrics-stop:
	@./mk/collect-metrics.sh stop $(METRICS_DIR)

metrics-plot:
	@if [ -f "$(METRICS_DIR)/metrics.csv" ]; then \
		$(PYTHON) ./mk/plot-metrics.py $(METRICS_DIR) $(TIMING_DIR) $(METRICS_DIR)/metrics; \
	else \
		echo "No metrics data found. Run 'make METRICS=1 all' first."; \
	fi

.PHONY: all
all: stage2

#            _           _       _           _        _ _
#   ___ __ _| |__   __ _| |     (_)_ __  ___| |_ __ _| | |
#  / __/ _` | '_ \ / _` | |_____| | '_ \/ __| __/ _` | | |
# | (_| (_| | |_) | (_| | |_____| | | | \__ \ || (_| | | |
#  \___\__,_|_.__/ \__,_|_|     |_|_| |_|___/\__\__,_|_|_|

# TODO: Building cabal-install from source as part of the Makefile is a
# temporary workaround. We should eventually require cabal to be provided
# externally (e.g. via ghcup) and drop this target entirely.
.PHONY: stable-cabal
stable-cabal: STAGE=stage0
stable-cabal:
ifeq (,$(USE_SYSTEM_CABAL))
	$(call PHASE_START,cabal)
	$(call LOG,Building $(CABAL))
	$(CABAL_INSTALL_STAGE0) --with-compiler $(GHC0) cabal-install:exe:cabal
	$(call PHASE_END_OK,cabal)
	@touch $(STAGE0_STAMP)
endif

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
stage1: stable-cabal $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES) cabal.project.stage1 cabal.project.common libraries/ghc-boot-th-next | hackage
	$(call PHASE_START,stage1)
	$(call LOG,Starting build of $(STAGE))

	$(call PHASE_START,stage1.executables)
	$(call LOG,Building executables $(STAGE1_EXECUTABLES))
	$(STAGE1_CABAL_BUILD) $(addprefix exe:,$(STAGE1_EXECUTABLES))
	$(call PHASE_END_OK,stage1.executables)

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
stage2: $(GHC1) stable-cabal $(CONFIGURE_SCRIPTS) $(CONFIGURED_FILES) cabal.project.stage2 cabal.project.stage2.settings cabal.project.common libraries/ghc-boot-th-next | stage1
	$(call PHASE_START,stage2)
	$(call LOG,Starting build of $(STAGE))

	$(call PHASE_START,stage2.rts)
	$(call LOG,Building rts)
	$(STAGE2_CABAL_BUILD) rts
	$(call PHASE_END_OK,stage2.rts)

	$(call PHASE_START,stage2.executables)
	$(call LOG,Building executables $(STAGE2_EXECUTABLES))
	$(STAGE2_CABAL_BUILD) $(addprefix exe:,$(STAGE2_EXECUTABLES))
	$(call PHASE_END_OK,stage2.executables)

	$(call PHASE_START,stage2.libraries)
	$(call LOG,Building libraries $(filter-out rts%,$(STAGE2_LIBRARIES)))
	$(STAGE2_CABAL_BUILD) $(filter-out rts%,$(STAGE2_LIBRARIES))
	$(call PHASE_END_OK,stage2.libraries)

	$(call PHASE_START,stage2.dist)
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
ifeq ($(DYNAMIC),1)
ifneq ($(UNAME), Darwin)
	$(PATCHELF) --force-rpath --set-rpath "\$$ORIGIN" $(CURDIR)/$(DIST_DIR)/lib/$(TARGET_PLATFORM)/$(DLL)
endif
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
STAGE3_wasm32-unknown-wasi_CXX_OPTS           = $(STAGE3_wasm32-unknown-wasi_CC_OPTS) -fno-exceptions
STAGE3_wasm32-unknown-wasi_AR                 = wasm32-wasi-ar
STAGE3_wasm32-unknown-wasi_RANLIB             = wasm32-wasi-ranlib
STAGE3_wasm32-unknown-wasi_EXTRA_INCLUDE_DIRS =
STAGE3_wasm32-unknown-wasi_EXTRA_LIB_DIRS     =
STAGE3_wasm32-unknown-wasi_GHC_TOOLCHAIN_ARGS = $(GHC_TOOLCHAIN_ARGS) --merge-objs wasm-ld --merge-objs-opt="-r" --disable-tables-next-to-code --disable-libffi-adjustors


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
	$$(call PHASE_START,stage3-$(1))
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

	$$(call PHASE_START,stage3-$(1).rts)
	$$(call LOG,Building library rts:nonthreaded-nodebug)
	$$(STAGE3_$(1)_CABAL_BUILD) rts:nonthreaded-nodebug
	$$(call PHASE_END_OK,stage3-$(1).rts)

	$$(call PHASE_START,stage3-$(1).libraries)
	$$(call LOG,Building libraries $(STAGE3_LIBRARIES))
	$$(STAGE3_$(1)_CABAL_BUILD) $(filter-out rts%,$(STAGE3_LIBRARIES))
	$$(call PHASE_END_OK,stage3-$(1).libraries)

	$$(call PHASE_START,stage3-$(1).dist)
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
	$$(call PHASE_END_OK,stage3-$(1).dist)
	$$(call PHASE_END_OK,stage3-$(1))

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

$(DIST_DIR)/ghc.tar.gz: stage2
	@echo "::group::Creating ghc.tar.gz..."
	@$(TAR) czf $@ \
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

$(DIST_DIR)/cabal.tar.gz: stable-cabal
	@echo "::group::Creating cabal.tar.gz..."
	@mkdir -p $(DIST_DIR)/bin
	@cp $(CABAL) $(DIST_DIR)/bin/
	@$(TAR) czf $@ \
		--directory=$(DIST_DIR) \
		bin/cabal
	@echo "::endgroup::"

$(DIST_DIR)/haskell-toolchain.tar.gz: stable-cabal stage2 stage3-javascript-unknown-ghcjs
	@echo "::group::Creating haskell-toolchain.tar.gz..."
	@mkdir -p $(DIST_DIR)/bin
	@cp $(CABAL) $(DIST_DIR)/bin/
	@$(TAR) czf $@ \
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
	@$(TAR) czf $@ \
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
ifeq (,$(USE_SYSTEM_CABAL))
	rm -rf $(BUILD_DIR)/cabal
endif
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
# $(CURDIR) is needed because the test recipe runs $(MAKE) -C testsuite/tests,
# so relative paths would resolve from the wrong directory. This matters both
# for CI and local `make test` invocations.
TEST_TOOLS_DIR := $(CURDIR)/$(DIST_DIR)/bin
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
	$(call PHASE_END_OK,test)

# Inform Make that these are not actual files if they get deleted by other means
.PHONY: clean clean-stage1 clean-stage2 clean-stage3 distclean test
