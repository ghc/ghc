HADRIAN_SETTINGS_STAGE0 := $(shell ghc --info | runghc GenSettings.hs ghc-boot)
HADRIAN_SETTINGS_STAGE1 := $(shell ghc --info | runghc GenSettings.hs ghc-boot)
SETTINGS_STAGE1 := $(shell ghc --info | runghc GenSettings.hs stage1)

CABAL := /home/hsyl20/repo/cabal/dist-newstyle/build/x86_64-linux/ghc-9.8.2/cabal-install-3.15.0.0/x/cabal/build/cabal/cabal
# CABAL := cabal

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
	  ghc-bin:ghc ghc-pkg:ghc-pkg genprimopcode:genprimopcode deriveConstants:deriveConstants genapply:genapply \
	  -j --builddir=_build/stage0/cabal/
	
	# Installing binaries
	cp `$(CABAL) list-bin --project-file=cabal.project-stage0 --builddir=_build/stage0/cabal/ ghc-bin:ghc` _build/stage0/bin/ghc
	cp `$(CABAL) list-bin --project-file=cabal.project-stage0 --builddir=_build/stage0/cabal/ ghc-pkg:ghc-pkg` _build/stage0/bin/ghc-pkg
	cp `$(CABAL) list-bin --project-file=cabal.project-stage0 --builddir=_build/stage0/cabal/ deriveConstants:deriveConstants` _build/stage0/bin/deriveConstants
	cp `$(CABAL) list-bin --project-file=cabal.project-stage0 --builddir=_build/stage0/cabal/ genprimopcode:genprimopcode` _build/stage0/bin/genprimopcode
	cp `$(CABAL) list-bin --project-file=cabal.project-stage0 --builddir=_build/stage0/cabal/ genapply:genapply` _build/stage0/bin/genapply
	
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
	cp _build/stage1/src/libraries/ghc-internal/ghc-internal.cabal{.in,}
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
	sed -i 's/@ProjectVersionForLib@/9.13/' _build/stage1/src/libraries/ghc-internal/ghc-internal.cabal
	sed -i 's/@ProjectVersion@/9.13/' _build/stage1/src/libraries/rts/include/ghcversion.h
	sed -i 's/@ProjectVersionInt@/913/' _build/stage1/src/libraries/rts/include/ghcversion.h
	sed -i 's/@ProjectPatchLevel1@/0/' _build/stage1/src/libraries/rts/include/ghcversion.h
	sed -i 's/@ProjectPatchLevel2@/0/' _build/stage1/src/libraries/rts/include/ghcversion.h
	
	sed -i 's/@LlvmMinVersion@/13/' _build/stage1/src/libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs
	sed -i 's/@LlvmMaxVersion@/20/' _build/stage1/src/libraries/ghc/GHC/CmmToLlvm/Version/Bounds.hs

	# Generating headers
	# FIXME: deriveConstants requires ghcautoconf.h and ghcplatform.h
	# Let's run cabal until it fails so that these files are generated...
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS_STAGE1)' \
	  $(CABAL) build --project-file=cabal.project-stage1-rts \
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
	  --builddir=_build/stage1/cabal/ >/dev/null 2>&1 || true

	# Deriving constants
	mkdir -p _build/stage1/temp/derive_constants
	_build/stage0/bin/deriveConstants --gen-header -o _build/stage1/src/libraries/rts/include/DerivedConstants.h \
		--target-os linux \
		--tmpdir _build/stage1/temp/derive_constants \
		--gcc-program gcc \
		--nm-program nm \
		--objdump-program objdump \
		--gcc-flag "-I_build/stage1/src/libraries/rts/include" \
		--gcc-flag "-I_build/stage1/src/libraries/rts" \
		--gcc-flag "-I_build/stage1/cabal/build/x86_64-linux/ghc-9.13/rts-1.0.2/build/include"

	# Generate autoapply
	_build/stage0/bin/genapply _build/stage1/src/libraries/rts/include/DerivedConstants.h > _build/stage1/src/libraries/rts/AutoApply.cmm
	_build/stage0/bin/genapply _build/stage1/src/libraries/rts/include/DerivedConstants.h -V16 > _build/stage1/src/libraries/rts/AutoApply_V16.cmm
	_build/stage0/bin/genapply _build/stage1/src/libraries/rts/include/DerivedConstants.h -V32 > _build/stage1/src/libraries/rts/AutoApply_V32.cmm
	_build/stage0/bin/genapply _build/stage1/src/libraries/rts/include/DerivedConstants.h -V64 > _build/stage1/src/libraries/rts/AutoApply_V64.cmm
	
	# Build libffi
	mkdir -p _build/stage1/src/libffi
	mkdir -p _build/stage1/libffi
	(cd _build/stage1/src/libffi; tar -xvf ../../../../libffi-tarballs/libffi-3.4.6.tar.gz)
	(cd _build/stage1/src/libffi/libffi-3.4.6; ./configure --disable-docs --with-pics=yes --disable-multi-os-directory --prefix=`pwd`/../../../../../_build/stage1/libffi/ && make install -j)
	cp -f _build/stage1/libffi/include/* _build/stage1/src/libraries/rts/include/
	cp -f _build/stage1/libffi/lib/libffi.a _build/stage1/cabal/build/x86_64-linux/ghc-9.13/rts-1.0.2/build/libCffi.a

	# Building boot libraries
	mkdir -p _build/stage1/cabal/

	# we need to pass "-this-unit-id=rts", otherwise GHC tries to lookup the
	# platform constants in the package db and fails. The flag is already
	# set in rts.cabal but for some reason it isn't always passed :shrug:
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS_STAGE1)' \
	  $(CABAL) build --project-file=cabal.project-stage1-rts \
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
	  --ghc-options='-this-unit-id=rts' \
	  --builddir=_build/stage1/cabal/
	
	# generate files related to primops

	gcc -E -undef -traditional -P -x c  _build/stage1/src/libraries/ghc/GHC/Builtin/primops.txt.pp > _build/stage1/src/libraries/ghc/GHC/Builtin/primops.txt
	_build/stage0/bin/genprimopcode --make-haskell-source < _build/stage1/src/libraries/ghc/GHC/Builtin/primops.txt > _build/stage1/src/libraries/ghc-prim/GHC/Prim.hs
	_build/stage0/bin/genprimopcode --make-haskell-wrappers < _build/stage1/src/libraries/ghc/GHC/Builtin/primops.txt >  _build/stage1/src/libraries/ghc-prim/GHC/PrimopWrappers.hs


	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS_STAGE1)' \
	  $(CABAL) build --project-file=cabal.project-stage1 \
	  rts ghc-prim ghc-bignum ghc-internal \
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
	  --builddir=_build/stage1/cabal/ -v

clean:
	rm -rf _build

