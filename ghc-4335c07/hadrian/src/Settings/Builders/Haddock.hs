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
haddockBuilderArgs = withHsPackage $ \cabalFile -> mconcat
    [ builder (Haddock BuildIndex) ? do
        output <- getOutput
        inputs <- getInputs
        mconcat
            [ arg "--gen-index"
            , arg "--gen-contents"
            , arg "-o", arg $ takeDirectory output
            , arg "-t", arg "Haskell Hierarchical Libraries"
            , arg "-p", arg "libraries/prologue.txt"
            , pure [ "--read-interface="
                     ++ (takeFileName . takeDirectory) haddock
                     ++ "," ++ haddock | haddock <- inputs ] ]

    , builder (Haddock BuildPackage) ? do
        output   <- getOutput
        pkg      <- getPackage
        path     <- getBuildPath
        version  <- expr $ pkgVersion  cabalFile
        synopsis <- expr $ pkgSynopsis cabalFile
        deps     <- getPkgDataList DepNames
        haddocks <- expr . haddockDependencies =<< getContext
        hVersion <- expr $ pkgVersion (unsafePkgCabalFile haddock) -- TODO: improve
        ghcOpts  <- haddockGhcArgs
        mconcat
            [ arg $ "--odir=" ++ takeDirectory output
            , arg "--verbosity=0"
            , arg "--no-tmp-comp-dir"
            , arg $ "--dump-interface=" ++ output
            , arg "--html"
            , arg "--hyperlinked-source"
            , arg "--hoogle"
            , arg $ "--title=" ++ pkgName pkg ++ "-" ++ version
                    ++ ": " ++ synopsis
            , arg $ "--prologue=" ++ path -/- "haddock-prologue.txt"
            , arg $ "--optghc=-D__HADDOCK_VERSION__="
                    ++ show (versionToInt hVersion)
            , map ("--hide=" ++) <$> getPkgDataList HiddenModules
            , pure [ "--read-interface=../" ++ dep
                     ++ ",../" ++ dep ++ "/src/%{MODULE}.html#%{NAME},"
                     ++ haddock | (dep, haddock) <- zip deps haddocks ]
            , pure [ "--optghc=" ++ opt | opt <- ghcOpts ]
            , getInputs
            , arg "+RTS"
            , arg $ "-t" ++ path -/- "haddock.t"
            , arg "--machine-readable"
            , arg "-RTS" ] ]
