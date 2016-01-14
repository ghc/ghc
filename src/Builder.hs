{-# LANGUAGE DeriveGeneric #-}
module Builder (
    Builder (..), isStaged, builderPath, getBuilderPath, specified, needBuilder
    ) where

import Control.Monad.Trans.Reader

import Base
import GHC.Generics (Generic)
import Oracles
import Stage

-- | A 'Builder' is an external command invoked in separate process using 'Shake.cmd'
--
-- @Ghc Stage0@ is the bootstrapping compiler
-- @Ghc StageN@, N > 0, is the one built on stage (N - 1)
-- @GhcPkg Stage0@ is the bootstrapping @GhcPkg@
-- @GhcPkg StageN@, N > 0, is the one built in Stage0 (TODO: need only Stage1?)
-- TODO: Do we really need HsCpp builder? Can't we use a generic Cpp
--       builder instead? It would also be used instead of GccM.
-- TODO: rename Gcc to CCompiler? We sometimes use gcc and sometimes clang.
-- TODO: why are Gcc/GccM staged?
data Builder = Alex
             | Ar
             | DeriveConstants
             | Gcc Stage
             | GccM Stage
             | GenApply
             | GenPrimopCode
             | Ghc Stage
             | GhcCabal
             | GhcCabalHsColour
             | GhcM Stage
             | GhcPkg Stage
             | Haddock
             | Happy
             | HsColour
             | HsCpp
             | Hsc2Hs
             | Ld
             | Nm
             | Objdump
             | Patch
             | Perl
             | Ranlib
             | Tar
             | Unlit
             deriving (Show, Eq, Generic)

isStaged :: Builder -> Bool
isStaged (Gcc    _) = True
isStaged (GccM   _) = True
isStaged (Ghc    _) = True
isStaged (GhcM   _) = True
isStaged (GhcPkg _) = True
isStaged _          = False

-- Configuration files refer to Builders as follows:
builderKey :: Builder -> String
builderKey builder = case builder of
    Alex             -> "alex"
    Ar               -> "ar"
    DeriveConstants  -> "derive-constants"
    Gcc Stage0       -> "system-gcc"
    Gcc _            -> "gcc"
    GccM stage       -> builderKey $ Gcc stage -- synonym for 'Gcc -MM'
    GenApply         -> "genapply"
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
    Happy            -> "happy"
    Haddock          -> "haddock"
    HsColour         -> "hscolour"
    Hsc2Hs           -> "hsc2hs"
    HsCpp            -> "hs-cpp"
    Ld               -> "ld"
    Nm               -> "nm"
    Objdump          -> "objdump"
    Patch            -> "patch"
    Perl             -> "perl"
    Ranlib           -> "ranlib"
    Tar              -> "tar"
    Unlit            -> "unlit"

-- | Determine the location of a 'Builder'
-- TODO: Paths to some builders should be determined using 'defaultProgramPath'
builderPath :: Builder -> Action FilePath
builderPath builder = do
    path <- askConfigWithDefault (builderKey builder) $
            putError $ "\nCannot find path to '" ++ (builderKey builder)
                     ++ "' in configuration files."
    windows <- windowsHost
    case (path, windows) of
        ("", _)    -> return path
        (p, True)  -> fixAbsolutePathOnWindows (p -<.> exe)
        (p, False) -> lookupInPath (p -<.> exe)

getBuilderPath :: Builder -> ReaderT a Action FilePath
getBuilderPath = lift . builderPath

specified :: Builder -> Action Bool
specified = fmap (not . null) . builderPath

-- | Make sure a builder exists on the given path and rebuild it if out of date.
-- If 'laxDependencies' is True then we do not rebuild GHC even if it is out of
-- date (can save a lot of build time when changing GHC).
needBuilder :: Bool -> Builder -> Action ()
needBuilder laxDependencies builder = whenM (specified builder) $ do
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

-- Instances for storing in the Shake database
instance Binary Builder
instance Hashable Builder
instance NFData Builder
