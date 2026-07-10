{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies #-}

module Test.SelfMake(main) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Util
import Test.Self(cabalBuildDepends)
import Test.Type

import Data.List.Extra


newtype GhcPkg = GhcPkg () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype GhcFlags = GhcFlags () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

type instance RuleResult GhcPkg = [String]
type instance RuleResult GhcFlags = [String]

-- Doesn't work on CI, seems self-building under `cabal test` is just hard
main = testBuild (notCI . defaultTest) $ do
    want ["Main" <.> exe]

    ghcPkg <- addOracleHash $ \GhcPkg{} -> do
        Stdout out <- quietly $ cmd "ghc-pkg list --simple-output"
        pure $ words out

    ghcFlags <- addOracleHash $ \GhcFlags{} ->
        map ("-package=" ++) <$> readFileLines ".pkgs"

    let ghc args = do
            trackAllow ["**/package.cache", "**/.ghc.environment.*"]
            -- since ghc-pkg includes the ghc package, it changes if the version does
            ghcPkg $ GhcPkg ()
            flags <- ghcFlags $ GhcFlags ()
            cmd "ghc" flags args

    "Main" <.> exe %> \out -> do
        let run = shakeRoot </> "src/Run.hs"
        copyFileChanged (shakeRoot </> "src" </> "Paths.hs") "Paths_shake.hs"
        let flags =
                ["-i" ++ shakeRoot </> "src","-dep-suffix=.","-main-is","Run.main"
                ,"-hide-all-packages","-outputdir=."
                ,"-DPORTABLE","-fwarn-unused-imports","-Werror"] -- to test one CPP branch

        trackAllow ["**/*.o","**/*.hi","Makefile"]
        ghc $ ["-M",run] ++ flags
        need . concatMap (filter (\x -> takeExtension x == ".hs") . snd) . parseMakefile =<< liftIO (readFile "Makefile")
        ghc $ ["-o",out,run,"-j4"] ++ flags

    ".pkgs" %> \out -> do
        src <- readFile' $ shakeRoot </> "shake.cabal"
        writeFileLines out $ sort $ cabalBuildDepends src
