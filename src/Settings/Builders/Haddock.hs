module Settings.Builders.Haddock (haddockBuilderArgs) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.PackageData as PD
import Hadrian.Utilities
import Rules.Documentation
import Settings.Builders.Common
import Settings.Builders.Ghc

-- | Given a version string such as "2.16.2" produce an integer equivalent.
versionToInt :: String -> Int
versionToInt = read . dropWhile (=='0') . filter (/='.')

-- TODO: Get rid of partiality (see @Just foo <- @).
haddockBuilderArgs :: Args
haddockBuilderArgs = withHsPackage $ \ctx -> mconcat
    [ builder (Haddock BuildIndex) ? do
        output <- getOutput
        inputs <- getInputs
        root   <- getBuildRoot
        mconcat
            [ arg $ "-B" ++ root -/- "stage1" -/- "lib"
            , arg $ "--lib=" ++ root -/- "docs"
            , arg "--gen-index"
            , arg "--gen-contents"
            , arg "-o", arg $ takeDirectory output
            , arg "-t", arg "Haskell Hierarchical Libraries"
            , arg "-p", arg "libraries/prologue.txt"
            , pure [ "--read-interface="
                     ++ (takeFileName . takeDirectory) haddock
                     ++ "," ++ haddock | haddock <- inputs ] ]

    , builder (Haddock BuildPackage) ? do
        output        <- getOutput
        pkg           <- getPackage
        root          <- getBuildRoot
        path          <- getBuildPath
        Just version  <- expr $ pkgVersion  ctx
        Just synopsis <- expr $ pkgSynopsis ctx
        deps          <- getPackageData PD.depNames
        haddocks      <- expr . haddockDependencies =<< getContext
        Just hVersion <- expr $ pkgVersion ctx
        ghcOpts       <- haddockGhcArgs
        mconcat
            [ arg "--verbosity=0"
            , arg $ "-B" ++ root -/- "stage1" -/- "lib"
            , arg $ "--lib=" ++ root -/- "docs"
            , arg $ "--odir=" ++ takeDirectory output
            , arg "--no-tmp-comp-dir"
            , arg $ "--dump-interface=" ++ output
            , arg "--html"
            , arg "--hyperlinked-source"
            , arg "--hoogle"
            , arg "--quickjump"
            , arg $ "--title=" ++ pkgName pkg ++ "-" ++ version
                    ++ ": " ++ synopsis
            , arg $ "--prologue=" ++ takeDirectory output -/- "haddock-prologue.txt"
            , arg $ "--optghc=-D__HADDOCK_VERSION__="
                    ++ show (versionToInt hVersion)
            , map ("--hide=" ++) <$> getPackageData PD.otherModules
            , pure [ "--read-interface=../" ++ dep
                     ++ ",../" ++ dep ++ "/src/%{MODULE}.html#%{NAME},"
                     ++ haddock | (dep, haddock) <- zip deps haddocks ]
            , pure [ "--optghc=" ++ opt | opt <- ghcOpts, not ("--package-db" `isInfixOf` opt) ]
            , getInputs
            , arg "+RTS"
            , arg $ "-t" ++ path -/- "haddock.t"
            , arg "--machine-readable"
            , arg "-RTS" ] ]
