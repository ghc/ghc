HADRIAN_SETTINGS_STAGE0 := $(shell ghc --info | runghc GenSettings.hs)


build:
	rm -rf _build
	
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
	cabal configure --project-file=cabal.project-stage0
	HADRIAN_SETTINGS='$(HADRIAN_SETTINGS_STAGE0)' \
	  cabal install --project-file=cabal.project-stage0 ghc-bin:ghc -j --builddir=_build/stage0/cabal/ --installdir=_build/stage0/bin --overwrite-policy=always --install-method=copy
