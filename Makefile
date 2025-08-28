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
.SHELLFLAGS := -eu -o pipefail -O globstar -c

ROOT_DIR := $(patsubst %/,%,$(dir $(realpath $(lastword $(MAKEFILE_LIST)))))

GHC0 ?= ghc-9.8.4
PYTHON ?= python3
CABAL ?= cabal

# :exploding-head: It turns out override doesn't override the command-line
# value but it overrides Make's normal behavior of ignoring assignments to
# command-line variables. This allows the += operations to append to whatever
# was passed from the command line.

override CABAL_ARGS += \
	--remote-repo-cache _build/packages \
	--store-dir=_build/$(STAGE)/store \
	--logs-dir=_build/$(STAGE)/logs

override CABAL_BUILD_ARGS += \
	-j -w $(GHC) --with-gcc=$(CC) \
	--project-file=cabal.project.$(STAGE) \
	--builddir=_build/$(STAGE) \
	--ghc-options="-fhide-source-paths"

# just some defaults
STAGE ?= stage1
GHC ?= $(GHC0)

CABAL_BUILD = $(CABAL) $(CABAL_ARGS) build $(CABAL_BUILD_ARGS)

GHC1 = _build/stage1/bin/ghc
GHC2 = _build/stage2/bin/ghc

define GHC_INFO
$(shell $(GHC0) --info | grep -oP '"$1",\s*"\K[^"]+')
endef

TARGET_PLATFORM := $(call GHC_INFO,target platform string)
TARGET_ARCH     := $(call GHC_INFO,target arch)
TARGET_OS       := $(call GHC_INFO,target os)
TARGET_TRIPLE   := $(call GHC_INFO,Target platform)
GIT_COMMIT_ID   := $(shell git rev-parse HEAD)

define HADRIAN_SETTINGS
[ ("hostPlatformArch",    "$(TARGET_ARCH)") \
, ("hostPlatformOS",      "$(TARGET_OS)") \
, ("cProjectGitCommitId", "$(GIT_COMMIT_ID)") \
, ("cProjectVersion",     "9.13") \
, ("cProjectVersionInt",  "913") \
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
$(addprefix _build/stage1/bin/,$(STAGE1_EXECUTABLES)) &: $(CABAL) | _build/booted
	@echo "::group::Building stage1 executables ($(STAGE1_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage1/cache
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' $(CABAL_BUILD) $(STAGE1_TARGETS)
	@echo "::endgroup::"

_build/stage1/lib/settings: _build/stage1/bin/ghc-toolchain-bin
	@echo "::group::Creating settings for $(TARGET_TRIPLE)..."
	@mkdir -p $(@D)
	_build/stage1/bin/ghc-toolchain-bin --triple $(TARGET_TRIPLE) --output-settings -o $@ --cc $(CC) --cxx $(CXX)
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
# 	ln -s $(abspath ./_build/stage2/packagedb/host/ghc-9.13) _build/stage1/lib/package.conf.d
# 	_build/stage1/bin/ghc-pkg init $(abspath ./_build/stage2/packagedb/host/ghc-9.13)
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
$(addprefix _build/stage2/bin/,$(STAGE2_UTIL_EXECUTABLES)) &: $(CABAL) stage1
	@echo "::group::Building stage2 utilities ($(STAGE2_UTIL_EXECUTABLES))..."
	# Force cabal to replan
	rm -rf _build/stage2/cache
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
		PATH=$(PWD)/_build/stage1/bin:$(PATH) \
		$(CABAL_BUILD) --ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" -W $(GHC0) $(STAGE2_UTIL_TARGETS)
	@echo "::endgroup::"


# # We use PATH=... here to ensure all the build-tool-depends (deriveConstants, genapply, genprimopcode, ...) are
# # available in PATH while cabal evaluates configure files. Cabal sadly does not support build-tool-depends or
# # handle build-depends properly prior to building the package.  Thus Configure/Setup/... do not have build-tool-depends
# # available in PATH.  This is a workaround for that.  I consider this a defect in cabal.
# _build/stage2/bin/ghc: _build/stage1.done
# 	@$(LIB)
# 	@echo ">>> Building with GHC: $(GHC1) and Cabal: $(CABAL)"
# 	@echo ">>> Using $(THREADS) threads"

# 	# this is stupid, having to build the rts first. We need to find a better way to do this.
# 	# We might be able to just have the `ghc` executable depend on the specific rts we want to
# 	# set as a default.
# 	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
# 		PATH=$(PWD)/_build/stage1/bin:$(PATH) \
# 		$(CABAL) $(CABAL_ARGS) build --project-file=cabal.project.stage2 --builddir=_build/stage2/cabal -j -w ghc \
# 		$(CABAL_BUILD_ARGS) \
# 		--ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" \
# 		rts:nonthreaded-nodebug rts:nonthreaded-debug \
# 		|& tee _build/logs/rts.log

# 	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS)' \
# 		PATH=$(PWD)/_build/stage1/bin:$(PATH) \
# 		$(CABAL) $(CABAL_ARGS) build --project-file=cabal.project.stage2 --builddir=_build/stage2/cabal -j -w ghc \
# 		$(CABAL_BUILD_ARGS) \
# 		--ghc-options="-ghcversion-file=$(abspath ./rts/include/ghcversion.h)" \
# 		$(STAGE2_TARGETS) \
# 		|& tee _build/logs/stage2.log

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

# Target for creating the final binary distribution directory
_build/bindist: stage2 driver/ghc-usage.txt driver/ghci-usage.txt
	@echo "::group::Creating binary distribution in _build/bindist"
	@mkdir -p _build/bindist/bin
	@mkdir -p _build/bindist/lib
	# Copy executables from stage2 bin
	@cp -rfp _build/stage2/bin/* _build/bindist/bin/
	# Copy libraries and settings from stage2 lib
	@cp -rfp _build/stage2/lib/* _build/bindist/lib/
	# Copy driver usage files
	@cp -rfp driver/ghc-usage.txt _build/bindist/lib/
	@cp -rfp driver/ghci-usage.txt _build/bindist/lib/
	@echo "FIXME: Changing 'Support SMP' from YES to NO in settings file"
	@sed 's/("Support SMP","YES")/("Support SMP","NO")/' -i.bck _build/bindist/lib/settings
	@echo "::endgroup::"

# --- Configuration ---

$(GHC1) $(GHC2): | hackage
hackage: _build/packages/hackage.haskell.org/01-index.tar.gz
_build/packages/hackage.haskell.org/01-index.tar.gz: | $(CABAL)
	@echo "::group::Updating Hackage index..."
	@mkdir -p $(@D)
	$(CABAL) $(CABAL_ARGS) update --index-state 2025-04-22T01:25:40Z
	@echo "::endgroup::"

# booted depends on successful source preparation
_build/booted:
	@echo "::group::Running ./boot script..."
	@mkdir -p _build/logs
	./boot
	@echo "::endgroup::"
	@echo "::group::Running ./configure script..."
	./configure
	@echo "::endgroup::"
	touch $@

# --- Clean Targets ---
clean:
	@echo "::group::Cleaning build artifacts..."
	rm -rf _build
	@echo "::endgroup::"

clean-stage1:
	@echo "::group::Cleaning stage1 build artifacts..."
	rm -rf _build/stage1
	@echo "::endgroup::"

clean-stage2:
	@echo "::group::Cleaning stage2 build artifacts..."
	rm -rf _build/stage2
	@echo "::endgroup::"

distclean: clean
	@echo "::group::Cleaning all generated files (distclean)..."
	rm -rf autom4te.cache
	rm -f config.status config.log config.h configure aclocal.m4
	rm -rf build-aux/config.guess build-aux/config.sub build-aux/install-sh build-aux/missing build-aux/compile depcomp
	find . -name 'Makefile.in' -delete
	rm -f $(CONFIGURED_FILES)
	@echo "::endgroup::"


# --- Test Target ---
test: _build/bindist
	@echo "::group::Running tests with THREADS=${THREADS}" >&2
	TEST_HC=`pwd`/_build/bindist/bin/ghc \
	TEST_CC=$(CC) \
	TEST_CXX=$(CXX) \
	METRICS_FILE=`pwd`/_build/test-perf.csv \
	SUMMARY_FILE=`pwd`/_build/test-summary.txt \
	JUNIT_FILE=`pwd`/_build/test-junit.xml \
	make -C testsuite/tests test THREADS=${THREADS}
	@echo "::endgroup::" >&2

# Inform Make that these are not actual files if they get deleted by other means
.PHONY: clean distclean test all
