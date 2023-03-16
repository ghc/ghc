{-# LANGUAGE OverloadedStrings #-}
module Settings.Builders.Haddock (haddockBuilderArgs) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type
import Hadrian.Utilities

import Packages
import Rules.Documentation
import Settings.Builders.Common
import Settings.Builders.Ghc
import CommandLine
import qualified Data.Text as T

-- | Given a version string such as "2.16.2" produce an integer equivalent.
versionToInt :: String -> Int
versionToInt = read . dropWhile (=='0') . filter (/='.')

haddockBuilderArgs :: Args
haddockBuilderArgs = mconcat
    [ builder (Haddock BuildIndex) ? do
        output <- getOutput
        inputs <- getInputs
        root   <- getBuildRoot
        mconcat
            [ arg $ "-B" ++ root -/- stageString Stage1 -/- "lib"
            , arg $ "--lib=" ++ root -/- stageString Stage1 -/- "lib"
            , arg "--gen-index"
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
        root     <- getBuildRoot
        stage    <- getStage
        context  <- getContext
        version  <- expr $ pkgVersion  pkg
        synopsis <- expr $ pkgSynopsis pkg
        haddocks <- expr $ haddockDependencies context
        haddocks_with_versions <- expr $ sequence $ [(,h) <$> pkgUnitId stage p | (p, h) <- haddocks]

        hVersion <- expr $ pkgVersion haddock
        statsDir <- expr $ haddockStatsFilesDir
        baseUrlTemplate <- expr (docsBaseUrl <$> userSetting defaultDocArgs)
        let baseUrl p = substituteTemplate baseUrlTemplate p
        ghcOpts  <- haddockGhcArgs
        mconcat
            [ arg "--verbosity=0"
            , arg $ "-B" ++ root -/- stageString Stage1 -/- "lib"
            , arg $ "--lib=" ++ root -/- stageString Stage1 -/- "lib"
            , arg $ "--odir=" ++ takeDirectory output
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
            , map ("--hide=" ++) <$> getContextData otherModules
            , pure [ "--read-interface=../" ++ p
                     ++ "," ++ baseUrl p ++ "/src/%{MODULE}.html#%{NAME},"
                     ++ haddock | (p, haddock) <- haddocks_with_versions ]
            , pure [ "--optghc=" ++ opt | opt <- ghcOpts, not ("--package-db" `isInfixOf` opt) ]
            , arg "+RTS"
            , arg $ "-t" ++ (statsDir -/- pkgName pkg ++ ".t")
            , arg "--machine-readable"
            , arg "-RTS" ] ]

substituteTemplate :: String -> String -> String
substituteTemplate baseTemplate pkgId = T.unpack . T.replace "%pkg%" (T.pack pkgId) . T.pack $ baseTemplate
