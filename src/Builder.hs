{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module Builder (
    CompilerMode (..), Builder (..),
    builderPath, getBuilderPath, builderEnvironment, specified, needBuilder
    ) where

import Control.Monad.Trans.Reader

import Base
import Context
import GHC
import GHC.Generics (Generic)
import Oracles.Config
import Oracles.LookupInPath
import Oracles.WindowsPath
import Stage

-- | A compiler can typically be used in one of three modes:
-- 1) Compiling sources into object files.
-- 2) Extracting source dependencies, e.g. by passing -M command line argument.
-- 3) Linking object files & static libraries into an executable.
data CompilerMode = Compile
                  | FindDependencies
                  | Link
                  deriving (Show, Eq, Generic)

-- TODO: Do we really need HsCpp builder? Can't we use Cc instead?
-- | A 'Builder' is an external command invoked in separate process using 'Shake.cmd'
--
-- @Ghc Stage0@ is the bootstrapping compiler
-- @Ghc StageN@, N > 0, is the one built on stage (N - 1)
-- @GhcPkg Stage0@ is the bootstrapping @GhcPkg@
-- @GhcPkg StageN@, N > 0, is the one built in Stage0 (TODO: need only Stage1?)
data Builder = Alex
             | Ar
             | DeriveConstants
             | Cc CompilerMode Stage
             | Configure FilePath
             | GenApply
             | GenPrimopCode
             | Ghc CompilerMode Stage
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
             | Make
             | Nm
             | Objdump
             | Patch
             | Perl
             | Ranlib
             | Tar
             | Unlit
             deriving (Show, Eq, Generic)

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

-- TODO: get rid of fromJust
-- | Determine the location of a 'Builder'.
builderPath :: Builder -> Action FilePath
builderPath builder = case builderProvenance builder of
    Just context -> return . fromJust $ programPath context
    Nothing -> case builder of
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
        Make          -> fromKey "make"
        Nm            -> fromKey "nm"
        Objdump       -> fromKey "objdump"
        Patch         -> fromKey "patch"
        Perl          -> fromKey "perl"
        Ranlib        -> fromKey "ranlib"
        Tar           -> fromKey "tar"
        _ -> error $ "Cannot determine builderPath for " ++ show builder
  where
    fromKey key = do
        path <- askConfigWithDefault key . putError $ "\nCannot find path to '"
            ++ key ++ "' in system.config file. Did you forget to run configure?"
        if null path
        then do
            if isOptional builder
            then return ""
            else putError $ "Builder '" ++ key ++ "' is not specified in"
                ++ " system.config file. Cannot proceed without it."
        else fixAbsolutePathOnWindows =<< lookupInPath path

getBuilderPath :: Builder -> ReaderT a Action FilePath
getBuilderPath = lift . builderPath

-- | Write a Builder's path into a given environment variable.
builderEnvironment :: String -> Builder -> Action CmdOption
builderEnvironment variable builder = do
    needBuilder builder
    path <- builderPath builder
    return $ AddEnv variable path

specified :: Builder -> Action Bool
specified = fmap (not . null) . builderPath

-- | Make sure a Builder exists on the given path and rebuild it if out of date.
needBuilder :: Builder -> Action ()
needBuilder = \case
    Configure dir -> need [dir -/- "configure"]
    builder       -> when (isInternal builder) $ do
        path <- builderPath builder
        need [path]

-- Instances for storing in the Shake database
instance Binary CompilerMode
instance Hashable CompilerMode
instance NFData CompilerMode

instance Binary Builder
instance Hashable Builder
instance NFData Builder
