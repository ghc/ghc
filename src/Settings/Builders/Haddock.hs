module Settings.Builders.Haddock (haddockArgs) where

import Base
import Util
import Builder
import Package
import Expression
import Predicates (builder, package, stage1)
import Oracles.PackageData
import Settings.Util
import Settings.Packages
import Settings.Builders.Ghc

haddockArgs :: Args
haddockArgs = builder Haddock ? do
    file     <- getFile
    srcs     <- getSources
    pkg      <- getPackage
    path     <- getTargetPath
    version  <- getPkgData Version
    synopsis <- getPkgData Synopsis
    hidden   <- getPkgDataList HiddenModules
    deps     <- getPkgDataList Deps
    depNames <- getPkgDataList DepNames
    ghcOpts  <- fromDiffExpr commonGhcArgs
    mconcat
        [ arg $ "--odir=" ++ takeDirectory file
        , arg "--verbosity=0"
        , arg "--no-tmp-comp-dir"
        , arg $ "--dump-interface=" ++ file
        , arg "--html"
        , arg "--hoogle"
        , arg $ "--title=" ++ pkgName pkg ++ "-" ++ version ++ ": " ++ synopsis
        , arg $ "--prologue=" ++ path -/- "haddock-prologue.txt"
        , append $ map ("--hide=" ++) hidden
        , append $ [ "--read-interface=../" ++ dep
                     ++ ",../" ++ dep ++ "/src/%{MODULE/./-}.html\\#%{NAME},"
                     ++ pkgHaddockFile depPkg
                   | (dep, depName) <- zip deps depNames
                   , Just depPkg <- [findKnownPackage depName] ]
        , append [ "--optghc=" ++ opt | opt <- ghcOpts ]
        , specified HsColour ?
          arg "--source-module=src/%{MODULE/./-}.html"
        , specified HsColour ?
          arg "--source-entity=src/%{MODULE/./-}.html\\#%{NAME}"
        , customPackageArgs
        , append srcs
        , arg "+RTS"
        , arg $ "-t" ++ path </> "haddock.t"
        , arg "--machine-readable" ]

customPackageArgs :: Args
customPackageArgs = mconcat
    [ package compiler ? stage1 ?
      arg "--optghc=-DSTAGE=2" ]
    -- TODO: move to getPackageSources
    -- , package ghcPrim  ? stage1 ?
    --   arg "libraries/ghc-prim/dist-install/build/autogen/GHC/Prim.hs" ]

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
