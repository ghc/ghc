module Builder (
    CcMode (..), GhcMode (..), GhcPkgMode (..), Builder (..),
    builderProvenance, systemBuilderPath, builderPath, getBuilderPath,
    isSpecified, needBuilder,
    ) where

import Development.Shake.Classes
import GHC.Generics
import Hadrian.Expression
import Hadrian.Oracles.Path
import Hadrian.Oracles.TextFile

import Base
import Context
import GHC

-- | C compiler can be used in two different modes:
-- * Compile or preprocess a source file.
-- * Extract source dependencies by passing @-MM@ command line argument.
data CcMode = CompileC | FindCDependencies deriving (Eq, Generic, Show)

instance Binary   CcMode
instance Hashable CcMode
instance NFData   CcMode

-- | GHC can be used in four different modes:
-- * Compile a Haskell source file.
-- * Compile a C source file.
-- * Extract source dependencies by passing @-M@ command line argument.
-- * Link object files & static libraries into an executable.
data GhcMode = CompileHs | CompileCWithGhc | FindHsDependencies | LinkHs
    deriving (Eq, Generic, Show)

instance Binary   GhcMode
instance Hashable GhcMode
instance NFData   GhcMode

-- | GhcPkg can initialise a package database and register packages in it.
data GhcPkgMode = Init | Update deriving (Eq, Generic, Show)

instance Binary   GhcPkgMode
instance Hashable GhcPkgMode
instance NFData   GhcPkgMode

-- | A 'Builder' is an external command invoked in a separate process via 'cmd'.
-- @Ghc Stage0@ is the bootstrapping compiler.
-- @Ghc StageN@, N > 0, is the one built in stage (N - 1).
-- @GhcPkg Stage0@ is the bootstrapping @GhcPkg@.
-- @GhcPkg Stage1@ is the one built in Stage0.
data Builder = Alex
             | Ar Stage
             | DeriveConstants
             | Cc CcMode Stage
             | Configure FilePath
             | GenApply
             | GenPrimopCode
             | Ghc GhcMode Stage
             | GhcCabal
             | GhcCabalHsColour -- synonym for 'GhcCabal hscolour'
             | GhcPkg GhcPkgMode Stage
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

instance Binary   Builder
instance Hashable Builder
instance NFData   Builder

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
    GhcPkg _ Stage0  -> Nothing
    GhcPkg _ _       -> context Stage0 ghcPkg
    Haddock          -> context Stage2 haddock
    Hpc              -> context Stage1 hpcBin
    Hsc2Hs           -> context Stage0 hsc2hs
    Unlit            -> context Stage0 unlit
    _                -> Nothing
  where
    context s p = Just $ vanillaContext s p

-- | Make sure a 'Builder' exists and rebuild it if out of date.
needBuilder :: Builder -> Action ()
needBuilder (Configure dir) = need [dir -/- "configure"]
needBuilder (Make      dir) = need [dir -/- "Makefile"]
needBuilder builder         = when (isJust $ builderProvenance builder) $ do
    path <- builderPath builder
    need [path]

-- TODO: Some builders are required only on certain platforms. For example,
-- Objdump is only required on OpenBSD and AIX, as mentioned in #211. Add
-- support for platform-specific optional builders as soon as we can reliably
-- test this feature.
isOptional :: Builder -> Bool
isOptional = \case
    HsColour -> True
    Objdump  -> True
    _        -> False

-- | Determine the location of a system 'Builder'.
systemBuilderPath :: Builder -> Action FilePath
systemBuilderPath builder = case builder of
    Alex            -> fromKey "alex"
    Ar Stage0       -> fromKey "system-ar"
    Ar _            -> fromKey "ar"
    Cc  _  Stage0   -> fromKey "system-cc"
    Cc  _  _        -> fromKey "cc"
    -- We can't ask configure for the path to configure!
    Configure _     -> return "sh configure"
    Ghc _  Stage0   -> fromKey "system-ghc"
    GhcPkg _ Stage0 -> fromKey "system-ghc-pkg"
    Happy           -> fromKey "happy"
    HsColour        -> fromKey "hscolour"
    HsCpp           -> fromKey "hs-cpp"
    Ld              -> fromKey "ld"
    Make _          -> fromKey "make"
    Nm              -> fromKey "nm"
    Objdump         -> fromKey "objdump"
    Patch           -> fromKey "patch"
    Perl            -> fromKey "perl"
    Ranlib          -> fromKey "ranlib"
    Tar             -> fromKey "tar"
    _               -> error $ "No entry for " ++ show builder ++ inCfg
  where
    inCfg = " in " ++ quote configFile ++ " file."
    fromKey key = do
        let unpack = fromMaybe . error $ "Cannot find path to builder "
                ++ quote key ++ inCfg ++ " Did you skip configure?"
        path <- unpack <$> lookupValue configFile key
        if null path
        then do
            unless (isOptional builder) . error $ "Non optional builder "
                ++ quote key ++ " is not specified" ++ inCfg
            return "" -- TODO: Use a safe interface.
        else fixAbsolutePathOnWindows =<< lookupInPath path

-- | Determine the location of a 'Builder'.
builderPath :: Builder -> Action FilePath
builderPath builder = case builderProvenance builder of
    Nothing      -> systemBuilderPath builder
    Just context -> programPath context

-- | Was the path to a given system 'Builder' specified in configuration files?
isSpecified :: Builder -> Action Bool
isSpecified = fmap (not . null) . systemBuilderPath

getBuilderPath :: Builder -> Expr c Builder FilePath
getBuilderPath = expr . builderPath
