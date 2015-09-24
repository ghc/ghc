{-# LANGUAGE DeriveGeneric #-}
module Builder (
    Builder (..), builderPath, getBuilderPath, specified, needBuilder
    ) where

import Base
import GHC.Generics (Generic)
import Oracles
import Stage

-- A Builder is an external command invoked in separate process using Shake.cmd
--
-- Ghc Stage0 is the bootstrapping compiler
-- Ghc StageN, N > 0, is the one built on stage (N - 1)
-- GhcPkg Stage0 is the bootstrapping GhcPkg
-- GhcPkg StageN, N > 0, is the one built in Stage0 (TODO: need only Stage1?)
-- TODO: add Cpp builders
-- TODO: rename Gcc to Cc?
data Builder = Alex
             | Ar
             | Gcc Stage
             | GccM Stage
             | GenPrimopCode
             | Ghc Stage
             | GhcCabal
             | GhcCabalHsColour
             | GhcM Stage
             | GhcPkg Stage
             | GhcSplit
             | Haddock
             | Happy
             | HsColour
             | HsCpp
             | Hsc2Hs
             | Ld
             | Unlit
             deriving (Show, Eq, Generic)

-- Configuration files refer to Builders as follows:
builderKey :: Builder -> String
builderKey builder = case builder of
    Alex             -> "alex"
    Ar               -> "ar"
    Gcc Stage0       -> "system-gcc"
    Gcc _            -> "gcc"
    GccM stage       -> builderKey $ Gcc stage -- synonym for 'Gcc -MM'
    GenPrimopCode    -> "genprimopcode"
    Ghc Stage0       -> "system-ghc"
    Ghc Stage1       -> "ghc-stage1"
    Ghc Stage2       -> "ghc-stage2"
    Ghc Stage3       -> "ghc-stage3"
    GhcM stage       -> builderKey $ Ghc stage -- synonym for 'Ghc -M'
    GhcCabal         -> "ghc-cabal"
    GhcCabalHsColour -> builderKey $ GhcCabal -- synonym for 'GhcCabal hscolour'
    GhcPkg Stage0    -> "system-ghc-pkg"
    GhcPkg _         -> "ghc-pkg"
    GhcSplit         -> "ghc-split"
    Happy            -> "happy"
    Haddock          -> "haddock"
    HsColour         -> "hscolour"
    Hsc2Hs           -> "hsc2hs"
    HsCpp            -> "hs-cpp"
    Ld               -> "ld"
    Unlit            -> "unlit"

builderPath :: Builder -> Action FilePath
builderPath builder = do
    path <- askConfigWithDefault (builderKey builder) $
            putError $ "\nCannot find path to '" ++ (builderKey builder)
                     ++ "' in configuration files."
    fixAbsolutePathOnWindows $ if null path then "" else path -<.> exe

getBuilderPath :: Builder -> ReaderT a Action FilePath
getBuilderPath = lift . builderPath

specified :: Builder -> Action Bool
specified = fmap (not . null) . builderPath

-- Make sure a builder exists on the given path and rebuild it if out of date.
-- If laxDependencies is True then we do not rebuild GHC even if it is out of
-- date (can save a lot of build time when changing GHC).
needBuilder :: Bool -> Builder -> Action ()
needBuilder laxDependencies builder = do
    path <- builderPath builder
    if laxDependencies && allowOrderOnlyDependency builder
    then orderOnly [path]
    else need      [path]
  where
    allowOrderOnlyDependency :: Builder -> Bool
    allowOrderOnlyDependency b = case b of
        Ghc  _ -> True
        GhcM _ -> True
        _      -> False

-- On Windows: if the path starts with "/", prepend it with the correct path to
-- the root, e.g: "/usr/local/bin/ghc.exe" => "C:/msys/usr/local/bin/ghc.exe".
fixAbsolutePathOnWindows :: FilePath -> Action FilePath
fixAbsolutePathOnWindows path = do
    windows <- windowsHost
    -- Note, below is different from FilePath.isAbsolute:
    if (windows && "/" `isPrefixOf` path)
    then do
        root <- windowsRoot
        return . unifyPath $ root ++ drop 1 path
    else
        return path

-- Instances for storing in the Shake database
instance Binary Builder
instance Hashable Builder
