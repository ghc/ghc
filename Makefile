HADRIAN_SETTINGS_STAGE0 := $(shell ghc --info | runghc GenSettings.hs ghc-boot)
HADRIAN_SETTINGS_STAGE1 := $(shell ghc --info | runghc GenSettings.hs ghc-boot)
SETTINGS_STAGE1 := $(shell ghc --info | runghc GenSettings.hs stage1)


# CABAL := /home/hsyl20/repo/cabal/dist-newstyle/build/x86_64-linux/ghc-9.8.2/cabal-install-3.15.0.0/x/cabal/build/cabal/cabal
CABAL := cabal

all: _build/stage1/bin/ghc

_build/stage0/bin/ghc:
	# Preparing source files...
	mkdir -p _build/stage0/src/
	cp -rf ./libraries   _build/stage0/src/
	cp -rf ./compiler    _build/stage0/src/libraries/ghc
	cp -rf ./ghc   	     _build/stage0/src/ghc-bin
	cp -rf ./utils       _build/stage0/src/
	
	## Substituting variables
	cp _build/stage0/src/ghc-bin/ghc-bin.cabal{.in,}
	cp _build/stage0/src/libraries/ghc/ghc.cabal{.in,}
	cp _build/stage0/src/libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs{.in,}
	cp _build/stage0/src/libraries/ghc-boot/ghc-boot.cabal{.in,}
	cp _build/stage0/src/libraries/ghc-boot-th/ghc-boot-th.cabal{.in,}
	cp _build/stage0/src/libraries/ghc-heap/ghc-heap.cabal{.in,}
	cp _build/stage0/src/libraries/ghci/ghci.cabal{.in,}
	cp _build/stage0/src/utils/ghc-pkg/ghc-pkg.cabal{.in,}
	
	sed -i 's/@ProjectVersion@/9.13/' _build/stage0/src/ghc-bin/ghc-bin.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage0/src/ghc-bin/ghc-bin.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage0/src/libraries/ghc/ghc.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage0/src/libraries/ghc/ghc.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage0/src/libraries/ghc-boot/ghc-boot.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage0/src/libraries/ghc-boot/ghc-boot.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage0/src/libraries/ghc-boot-th/ghc-boot-th.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage0/src/libraries/ghc-boot-th/ghc-boot-th.cabal
	sed -i 's/@Suffix@//' _build/stage0/src/libraries/ghc-boot-th/ghc-boot-th.cabal
	sed -i 's/@SourceRoot@/./' _build/stage0/src/libraries/ghc-boot-th/ghc-boot-th.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage0/src/libraries/ghc-heap/ghc-heap.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage0/src/libraries/ghc-heap/ghc-heap.cabal
	sed -i 's/@ProjectVersionForLib@/9.13/' _build/stage0/src/libraries/ghc-heap/ghc-heap.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage0/src/libraries/ghci/ghci.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage0/src/libraries/ghci/ghci.cabal
	sed -i 's/@ProjectVersionForLib@/9.13/' _build/stage0/src/libraries/ghci/ghci.cabal
	sed -i 's/@ProjectVersion@/9.13/'       _build/stage0/src/utils/ghc-pkg/ghc-pkg.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage0/src/utils/ghc-pkg/ghc-pkg.cabal
	sed -i 's/@ProjectVersionForLib@/9.13/' _build/stage0/src/utils/ghc-pkg/ghc-pkg.cabal
	
	sed -i 's/@LlvmMinVersion@/13/' _build/stage0/src/libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs
	sed -i 's/@LlvmMaxVersion@/20/' _build/stage0/src/libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs
	
	# Building...
	mkdir -p _build/stage0/cabal/
	mkdir -p _build/stage0/bin/
	
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS_STAGE0)' \
	  $(CABAL) build --project-file=cabal.project-stage0 \
	  ghc-bin:ghc ghc-pkg:ghc-pkg genprimopcode:genprimopcode deriveConstants:deriveConstants \
	  -j --builddir=_build/stage0/cabal/
	
	# Installing binaries
	cp `$(CABAL) list-bin --project-file=cabal.project-stage0 --builddir=_build/stage0/cabal/ ghc-bin:ghc` _build/stage0/bin/ghc
	cp `$(CABAL) list-bin --project-file=cabal.project-stage0 --builddir=_build/stage0/cabal/ ghc-pkg:ghc-pkg` _build/stage0/bin/ghc-pkg
	cp `$(CABAL) list-bin --project-file=cabal.project-stage0 --builddir=_build/stage0/cabal/ deriveConstants:deriveConstants` _build/stage0/bin/deriveConstants
	cp `$(CABAL) list-bin --project-file=cabal.project-stage0 --builddir=_build/stage0/cabal/ genprimopcode:genprimopcode` _build/stage0/bin/genprimopcode
	
	# Generate settings
	mkdir -p _build/stage0/lib
	echo '$(SETTINGS_STAGE1)' > _build/stage0/lib/settings
	
	_build/stage0/bin/ghc --version
	_build/stage0/bin/ghc --info


_build/stage1/bin/ghc: _build/stage0/bin/ghc
	rm -rf _build/stage1
	mkdir -p _build/stage1
	$(CABAL) --version
	
	# Initialize empty package db
	_build/stage0/bin/ghc-pkg init _build/stage1/pkgs
	_build/stage0/bin/ghc-pkg recache --global-package-db=_build/stage1/pkgs --no-user-package-db
	
	# Preparing source files...
	mkdir -p _build/stage1/src/
	cp -rf ./libraries    _build/stage1/src/
	cp -rf ./compiler     _build/stage1/src/libraries/ghc
	cp -rf ./rts   	      _build/stage1/src/libraries/
	cp -rf ./ghc   	      _build/stage1/src/ghc-bin
	cp -rf ./config.sub   _build/stage1/src/libraries/rts/
	cp -rf ./config.guess _build/stage1/src/libraries/rts/
	
	python rts/gen_event_types.py --event-types-defines _build/stage1/src/libraries/rts/include/rts/EventLogConstants.h
	python rts/gen_event_types.py --event-types-array   _build/stage1/src/libraries/rts/include/rts/EventTypes.h
	
	## Substituting variables
	cp _build/stage1/src/ghc-bin/ghc-bin.cabal{.in,}
	cp _build/stage1/src/libraries/ghc/ghc.cabal{.in,}
	cp _build/stage1/src/libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs{.in,}
	cp _build/stage1/src/libraries/ghc-boot/ghc-boot.cabal{.in,}
	cp _build/stage1/src/libraries/ghc-boot-th/ghc-boot-th.cabal{.in,}
	cp _build/stage1/src/libraries/ghc-heap/ghc-heap.cabal{.in,}
	cp _build/stage1/src/libraries/ghci/ghci.cabal{.in,}
	cp _build/stage1/src/libraries/rts/include/ghcversion.h{.in,}
	
	sed -i 's/@ProjectVersion@/9.13/' _build/stage1/src/ghc-bin/ghc-bin.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage1/src/ghc-bin/ghc-bin.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage1/src/libraries/ghc/ghc.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage1/src/libraries/ghc/ghc.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage1/src/libraries/ghc-boot/ghc-boot.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage1/src/libraries/ghc-boot/ghc-boot.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage1/src/libraries/ghc-boot-th/ghc-boot-th.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage1/src/libraries/ghc-boot-th/ghc-boot-th.cabal
	sed -i 's/@Suffix@//' _build/stage1/src/libraries/ghc-boot-th/ghc-boot-th.cabal
	sed -i 's/@SourceRoot@/./' _build/stage1/src/libraries/ghc-boot-th/ghc-boot-th.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage1/src/libraries/ghc-heap/ghc-heap.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage1/src/libraries/ghc-heap/ghc-heap.cabal
	sed -i 's/@ProjectVersionForLib@/9.13/' _build/stage1/src/libraries/ghc-heap/ghc-heap.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage1/src/libraries/ghci/ghci.cabal
	sed -i 's/@ProjectVersionMunged@/9.13/' _build/stage1/src/libraries/ghci/ghci.cabal
	sed -i 's/@ProjectVersionForLib@/9.13/' _build/stage1/src/libraries/ghci/ghci.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage1/src/libraries/rts/include/ghcversion.h
	sed -i 's/@ProjectVersionInt@/913/' _build/stage1/src/libraries/rts/include/ghcversion.h
	sed -i 's/@ProjectPatchLevel1@/0/' _build/stage1/src/libraries/rts/include/ghcversion.h
	sed -i 's/@ProjectPatchLevel2@/0/' _build/stage1/src/libraries/rts/include/ghcversion.h
	
	sed -i 's/@LlvmMinVersion@/13/' _build/stage1/src/libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs
	sed -i 's/@LlvmMaxVersion@/20/' _build/stage1/src/libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs
	
	# Building boot libraries
	mkdir -p _build/stage1/cabal/
	
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS_STAGE1)' \
	  $(CABAL) build --project-file=cabal.project-stage1 \
	  rts \
	  --with-compiler=`pwd`/_build/stage0/bin/ghc \
	  --with-hc-pkg=`pwd`/_build/stage0/bin/ghc-pkg \
	  --ghc-options="-ghcversion-file=`pwd`/_build/stage1/src/libraries/rts/include/ghcversion.h" \
	  --ghc-options="-I`pwd`/_build/stage1/src/libraries/rts/include/" \
	  --ghc-options="-I`pwd`/_build/stage1/src/libraries/rts/" \
	  --ghc-options='"-optc=-DProjectVersion=\"913\""' \
	  --ghc-options='"-optc=-DRtsWay=\"FIXME\""' \
	  --ghc-options='"-optc=-DHostPlatform=\"FIXME\""' \
	  --ghc-options='"-optc=-DHostArch=\"FIXME\""' \
	  --ghc-options='"-optc=-DHostOS=\"FIXME\""' \
	  --ghc-options='"-optc=-DHostVendor=\"FIXME\""' \
	  --ghc-options='"-optc=-DBuildPlatform=\"FIXME\""' \
	  --ghc-options='"-optc=-DBuildArch=\"FIXME\""' \
	  --ghc-options='"-optc=-DBuildOS=\"FIXME\""' \
	  --ghc-options='"-optc=-DBuildVendor=\"FIXME\""' \
	  --ghc-options='"-optc=-DTargetPlatform=\"FIXME\""' \
	  --ghc-options='"-optc=-DTargetArch=\"FIXME\""' \
	  --ghc-options='"-optc=-DTargetOS=\"FIXME\""' \
	  --ghc-options='"-optc=-DTargetVendor=\"FIXME\""' \
	  --ghc-options='"-optc=-DGhcUnregisterised=\"FIXME\""' \
	  --ghc-options='"-optc=-DTablesNextToCode=\"FIXME\""' \
	  --ghc-options='-v3' \
	  --builddir=_build/stage1/cabal/ -v3

clean:
	rm -rf _build

