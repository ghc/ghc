{-# LANGUAGE DeriveGeneric, LambdaCase #-}
module Builder (
    Builder (..), isStaged, builderPath, getBuilderPath, specified, needBuilder
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
             | GccM Stage         -- synonym for 'Gcc -MM'
             | GenApply
             | GenPrimopCode
             | Ghc Stage
             | GhcCabal
             | GhcCabalHsColour   -- synonym for 'GhcCabal hscolour'
             | GhcM Stage         -- synonym for 'Ghc -M'
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
    Ghc stage        -> if stage == Stage0 then Nothing else context (pred stage) ghc
    GhcM stage       -> builderProvenance $ Ghc stage
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

isStaged :: Builder -> Bool
isStaged = \case
    (Gcc    _) -> True
    (GccM   _) -> True
    (Ghc    _) -> True
    (GhcM   _) -> True
    (GhcPkg _) -> True
    _          -> False

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
-- | Determine the location of a 'Builder'
builderPath :: Builder -> Action FilePath
builderPath builder = case builderProvenance builder of
    Just context -> return . fromJust $ programPath context
    Nothing -> do
        let builderKey = case builder of
                Alex          -> "alex"
                Ar            -> "ar"
                Gcc Stage0    -> "system-gcc"
                Gcc _         -> "gcc"
                GccM Stage0   -> "system-gcc"
                GccM _        -> "gcc"
                Ghc Stage0    -> "system-ghc"
                GhcM Stage0   -> "system-ghc"
                GhcPkg Stage0 -> "system-ghc-pkg"
                Happy         -> "happy"
                HsColour      -> "hscolour"
                HsCpp         -> "hs-cpp"
                Ld            -> "ld"
                Make          -> "make"
                Nm            -> "nm"
                Objdump       -> "objdump"
                Patch         -> "patch"
                Perl          -> "perl"
                Ranlib        -> "ranlib"
                Tar           -> "tar"
                _ -> error $ "Cannot determine builderKey for " ++ show builder
        path <- askConfigWithDefault builderKey . putError $
            "\nCannot find path to '" ++ builderKey
            ++ "' in system.config file. Have you forgot to run configure?"
        if null path
        then do
            if isOptional builder
            then return ""
            else putError $ "Builder '" ++ builderKey ++ "' is not specified in"
                ++ " system.config file. Cannot proceed without it."
        else do
            path' <- lookupInPath path
            fixAbsolutePathOnWindows $ path' -<.> exe

getBuilderPath :: Builder -> ReaderT a Action FilePath
getBuilderPath = lift . builderPath

specified :: Builder -> Action Bool
specified = fmap (not . null) . builderPath

-- TODO: split into two functions: needBuilder (without laxDependencies) and
-- unsafeNeedBuilder (with the laxDependencies parameter)
-- | Make sure a builder exists on the given path and rebuild it if out of date.
-- If 'laxDependencies' is True then we do not rebuild GHC even if it is out of
-- date (can save a lot of build time when changing GHC).
needBuilder :: Bool -> Builder -> Action ()
needBuilder laxDependencies builder = when (isInternal builder) $ do
    path <- builderPath builder
    if laxDependencies && allowOrderOnlyDependency builder
    then orderOnly [path]
    else need      [path]
  where
    allowOrderOnlyDependency :: Builder -> Bool
    allowOrderOnlyDependency = \case
        Ghc  _ -> True
        GhcM _ -> True
        _      -> False

-- Instances for storing in the Shake database
instance Binary Builder
instance Hashable Builder
instance NFData Builder
