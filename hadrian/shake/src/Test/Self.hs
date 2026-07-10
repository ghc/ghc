{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TypeFamilies #-}

module Test.Self(main, cabalBuildDepends) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Test.Type

import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import System.Info.Extra


newtype GhcPkg = GhcPkg () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)
newtype GhcFlags = GhcFlags () deriving (Show,Typeable,Eq,Hashable,Binary,NFData)

type instance RuleResult GhcPkg = [String]
type instance RuleResult GhcFlags = [String]

-- Doesn't work on CI, seems self-building under `cabal test` is just hard
main = testBuild (notCI . defaultTest) $ do
    let moduleToFile ext xs = replace "." "/" xs <.> ext
    want ["Main" <.> exe]

    -- fixup to cope with Cabal's generated files
    let fixPaths x = if x == "Paths_shake.hs" then "Paths.hs" else x

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
        src <- readFileLines "Run.deps"
        let os = map (moduleToFile "o") $ "Run" : src
        need os
        ghc $ ["-o",out] ++ os

    "**/*.deps" %> \out -> do
        dep <- readFileLines $ out -<.> "dep"
        let xs = map (moduleToFile "deps") dep
        need xs
        ds <- nubOrd . sort . (++) dep <$> concatMapM readFileLines xs
        writeFileLines out ds

    "**/*.dep" %> \out -> do
        src <- readFile' $ shakeRoot </> "src" </> fixPaths (out -<.> "hs")
        let xs = hsImports src
        xs <- filterM (doesFileExist . (\x -> shakeRoot </> "src" </> x) . fixPaths . moduleToFile "hs") xs
        writeFileLines out xs

    ["**/*.o","**/*.hi"] &%> \[out,_] -> do
        deps <- readFileLines $ out -<.> "deps"
        let hs = shakeRoot </> "src" </> fixPaths (out -<.> "hs")
        need $ hs : map (moduleToFile "hi") deps
        ghc ["-c",hs,"-i" ++ shakeRoot </> "src","-main-is","Run.main"
            ,"-hide-all-packages","-outputdir=."
            ,"-DPORTABLE"] -- to test one CPP branch

    ".pkgs" %> \out -> do
        src <- readFile' $ shakeRoot </> "shake.cabal"
        writeFileLines out $ sort $ cabalBuildDepends src


---------------------------------------------------------------------
-- GRAB INFORMATION FROM FILES

hsImports :: String -> [String]
hsImports xs = [ takeWhile (\x -> isAlphaNum x || x `elem` "._") $ dropWhile (not . isUpper) x
               | x <- concatMap (wordsBy (== ';')) $ lines xs, "import " `isPrefixOf` trim x]


-- FIXME: Should actually parse the list from the contents of the .cabal file
cabalBuildDepends :: String -> [String]
cabalBuildDepends _ = packages ++ ["unix" | not isWindows]

packages = words $
    "base transformers binary unordered-containers hashable heaps time bytestring primitive " ++
    "filepath directory process deepseq random utf8-string extra js-dgtable js-jquery js-flot filepattern"
