module Settings.Builders.Haddock (haddockBuilderArgs) where

import Hadrian.Utilities
import Hadrian.Haskell.Cabal

import Rules.Documentation
import Settings.Builders.Common
import Settings.Builders.Ghc

-- | Given a version string such as "2.16.2" produce an integer equivalent.
versionToInt :: String -> Int
versionToInt s = case map read . words $ replaceEq '.' ' ' s of
    [major, minor, patch] -> major * 1000 + minor * 10 + patch
    _                     -> error "versionToInt: cannot parse version."

haddockBuilderArgs :: Args
haddockBuilderArgs = builder Haddock ? do
    output   <- getOutput
    pkg      <- getPackage
    path     <- getBuildPath
    version  <- expr $ pkgVersion pkg
    synopsis <- fromMaybe "" <$> expr (pkgSynopsis pkg)
    deps     <- getPkgDataList Deps
    haddocks <- expr . haddockDependencies =<< getContext
    hVersion <- expr $ pkgVersion haddock
    ghcOpts  <- haddockGhcArgs
    mconcat
        [ arg $ "--odir=" ++ takeDirectory output
        , arg "--verbosity=0"
        , arg "--no-tmp-comp-dir"
        , arg $ "--dump-interface=" ++ output
        , arg "--html"
        , arg "--hoogle"
        , arg $ "--title=" ++ pkgName pkg ++ "-" ++ version ++ ": " ++ synopsis
        , arg $ "--prologue=" ++ path -/- "haddock-prologue.txt"
        , arg $ "--optghc=-D__HADDOCK_VERSION__=" ++ show (versionToInt hVersion)
        , map ("--hide=" ++) <$> getPkgDataList HiddenModules
        , pure [ "--read-interface=../" ++ dep
                 ++ ",../" ++ dep ++ "/src/%{MODULE/./-}.html\\#%{NAME},"
                 ++ haddock | (dep, haddock) <- zip deps haddocks ]
        , pure [ "--optghc=" ++ opt | opt <- ghcOpts ]
        , isSpecified HsColour ?
          pure [ "--source-module=src/%{MODULE/./-}.html"
               , "--source-entity=src/%{MODULE/./-}.html\\#%{NAME}" ]
        , getInputs
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
