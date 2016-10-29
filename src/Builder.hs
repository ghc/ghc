{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module Builder (
    CcMode (..), GhcMode (..), Builder (..), builderPath, getBuilderPath,
    builderEnvironment, specified, trackedArgument, needBuilder
    ) where

import Control.Monad.Trans.Reader
import Data.Char
import GHC.Generics

import Base
import Context
import GHC
import Oracles.Config
import Oracles.LookupInPath
import Oracles.WindowsPath
import Stage

-- | A compiler can typically be used in different modes:
-- * Compiling or preprocessing a source file;
-- * Extracting source dependencies, e.g. by passing @-M@ command line argument;
-- * Linking object files & static libraries into an executable.
-- We have CcMode for C compiler and GhcMode for GHC.
data CcMode  = CompileC  | FindCDependencies deriving (Eq, Generic, Show)
data GhcMode = CompileHs | FindHsDependencies | LinkHs
    deriving (Eq, Generic, Show)

-- | A 'Builder' is an external command invoked in a separate process via 'cmd'.
-- @Ghc Stage0@ is the bootstrapping compiler.
-- @Ghc StageN@, N > 0, is the one built in stage (N - 1).
-- @GhcPkg Stage0@ is the bootstrapping @GhcPkg@.
-- @GhcPkg Stage1@ is the one built in Stage0.
data Builder = Alex
             | Ar
             | DeriveConstants
             | Cc CcMode Stage
             | Configure FilePath
             | GenApply
             | GenPrimopCode
             | Ghc GhcMode Stage
             | GhcCabal
             | GhcCabalHsColour   -- synonym for 'GhcCabal hscolour'
             | GhcPkg Stage
             | Haddock
             | Happy
             | Hpc
             | HsColour
             | HsCpp
             | Hsc2Hs
             | Ld
             | Make FilePath
             | Nm
             | Objdump
             | Patch
             | Perl
             | Ranlib
             | Tar
             | Unlit
             deriving (Eq, Generic, Show)

-- | Some builders are built by this very build system, in which case
-- 'builderProvenance' returns the corresponding build 'Context' (which includes
-- 'Stage' and GHC 'Package').
builderProvenance :: Builder -> Maybe Context
builderProvenance = \case
    DeriveConstants  -> context Stage0 deriveConstants
    GenApply         -> context Stage0 genapply
    GenPrimopCode    -> context Stage0 genprimopcode
    Ghc _ Stage0     -> Nothing
    Ghc _ stage      -> context (pred stage) ghc
    GhcCabal         -> context Stage0 ghcCabal
    GhcCabalHsColour -> builderProvenance $ GhcCabal
    GhcPkg stage     -> if stage > Stage0 then context Stage0 ghcPkg else Nothing
    Haddock          -> context Stage2 haddock
    Hpc              -> context Stage1 hpcBin
    Hsc2Hs           -> context Stage0 hsc2hs
    Unlit            -> context Stage0 unlit
    _                -> Nothing
  where
    context s p = Just $ vanillaContext s p

isInternal :: Builder -> Bool
isInternal = isJust . builderProvenance

-- TODO: Some builders are required only on certain platforms. For example,
-- Objdump is only required on OpenBSD and AIX, as mentioned in #211. Add
-- support for platform-specific optional builders as soon as we can reliably
-- test this feature.
isOptional :: Builder -> Bool
isOptional = \case
    HsColour -> True
    Objdump  -> True
    _        -> False

-- | Determine the location of a 'Builder'.
builderPath :: Builder -> Action FilePath
builderPath builder = case programPath =<< builderProvenance builder of
    Just path -> return path
    Nothing   -> case builder of
        Alex          -> fromKey "alex"
        Ar            -> fromKey "ar"
        Cc  _  Stage0 -> fromKey "system-cc"
        Cc  _  _      -> fromKey "cc"
        -- We can't ask configure for the path to configure!
        Configure _   -> return "bash configure"
        Ghc _  Stage0 -> fromKey "system-ghc"
        GhcPkg Stage0 -> fromKey "system-ghc-pkg"
        Happy         -> fromKey "happy"
        HsColour      -> fromKey "hscolour"
        HsCpp         -> fromKey "hs-cpp"
        Ld            -> fromKey "ld"
        Make _        -> fromKey "make"
        Nm            -> fromKey "nm"
        Objdump       -> fromKey "objdump"
        Patch         -> fromKey "patch"
        Perl          -> fromKey "perl"
        Ranlib        -> fromKey "ranlib"
        Tar           -> fromKey "tar"
        _ -> error $ "Cannot determine builderPath for " ++ show builder
  where
    fromKey key = do
        let unpack = fromMaybe . error $ "Cannot find path to builder "
                ++ quote key ++ " in system.config file. Did you skip configure?"
        path <- unpack <$> askConfig key
        if null path
        then do
            unless (isOptional builder) . error $ "Non optional builder "
                ++ quote key ++ " is not specified in system.config file."
            return "" -- TODO: Use a safe interface.
        else fixAbsolutePathOnWindows =<< lookupInPath path

getBuilderPath :: Builder -> ReaderT a Action FilePath
getBuilderPath = lift . builderPath

-- | Write a Builder's path into a given environment variable.
builderEnvironment :: String -> Builder -> Action CmdOption
builderEnvironment variable builder = do
    needBuilder builder
    path <- builderPath builder
    return $ AddEnv variable path

-- | Was the path to a given 'Builder' specified in configuration files?
specified :: Builder -> Action Bool
specified = fmap (not . null) . builderPath

-- | Some arguments do not affect build results and therefore do not need to be
-- tracked by the build system. A notable example is "-jN" that controls Make's
-- parallelism. Given a 'Builder' and an argument, this function should return
-- 'True' only if the argument needs to be tracked.
trackedArgument :: Builder -> String -> Bool
trackedArgument (Make _) = not . threadArg
trackedArgument _        = const True

threadArg :: String -> Bool
threadArg s = dropWhileEnd isDigit s `elem` ["-j", "MAKEFLAGS=-j", "THREADS="]

-- | Make sure a Builder exists on the given path and rebuild it if out of date.
needBuilder :: Builder -> Action ()
needBuilder = \case
    Configure dir -> need [dir -/- "configure"]
    builder       -> when (isInternal builder) $ do
        path <- builderPath builder
        need [path]

instance Binary Builder
instance Hashable Builder
instance NFData Builder

instance Binary CcMode
instance Hashable CcMode
instance NFData CcMode

instance Binary GhcMode
instance Hashable GhcMode
instance NFData GhcMode
