module Settings.Builders.Haddock (haddockBuilderArgs) where

import Development.Shake.FilePath
import Base
import GHC
import Package
import Expression
import Oracles.PackageData
import Predicates hiding (file)
import Settings
import Settings.Builders.Ghc

haddockBuilderArgs :: Args
haddockBuilderArgs = builder Haddock ? do
    output   <- getOutput
    pkg      <- getPackage
    path     <- getTargetPath
    version  <- getPkgData Version
    synopsis <- getPkgData Synopsis
    hidden   <- getPkgDataList HiddenModules
    deps     <- getPkgDataList Deps
    depNames <- getPkgDataList DepNames
    hVersion <- lift . pkgData . Version $ targetPath Stage2 haddock
    ghcOpts  <- fromDiffExpr commonGhcArgs
    mconcat
        [ arg $ "--odir=" ++ takeDirectory output
        , arg "--verbosity=0"
        , arg "--no-tmp-comp-dir"
        , arg $ "--dump-interface=" ++ output
        , arg "--html"
        , arg "--hoogle"
        , arg $ "--title=" ++ pkgNameString pkg ++ "-" ++ version ++ ": " ++ synopsis
        , arg $ "--prologue=" ++ path -/- "haddock-prologue.txt"
        , arg $ "--optghc=-D__HADDOCK_VERSION__=" ++ show (versionToInt hVersion)
        , append $ map ("--hide=" ++) hidden
        , append $ [ "--read-interface=../" ++ dep
                     ++ ",../" ++ dep ++ "/src/%{MODULE/./-}.html\\#%{NAME},"
                     ++ pkgHaddockFile depPkg
                   | (dep, depName) <- zip deps depNames
                   , Just depPkg <- [findKnownPackage $ PackageName depName]
                   , depPkg /= rts ]
        , append [ "--optghc=" ++ opt | opt <- ghcOpts ]
        , specified HsColour ?
          arg "--source-module=src/%{MODULE/./-}.html"
        , specified HsColour ?
          arg "--source-entity=src/%{MODULE/./-}.html\\#%{NAME}"
        , append =<< getInputs
        , arg "+RTS"
        , arg $ "-t" ++ path -/- "haddock.t"
        , arg "--machine-readable"
        , arg "-RTS" ]

-- From ghc.mk:
-- # -----------------------------------------------
-- # Haddock-related bits

-- # Build the Haddock contents and index
-- ifeq "$(HADDOCK_DOCS)" "YES"
-- libraries/dist-haddock/index.html: $(haddock_INPLACE) $(ALL_HADDOCK_FILES)
--     cd libraries && sh gen_contents_index --intree
-- ifeq "$(phase)" "final"
-- $(eval $(call all-target,library_doc_index,libraries/dist-haddock/index.html))
-- endif
-- INSTALL_LIBRARY_DOCS += libraries/dist-haddock/*
-- endif
