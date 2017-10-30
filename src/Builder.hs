{-# LANGUAGE InstanceSigs #-}
module Builder (
    -- * Data types
    ArMode (..), CcMode (..), GhcMode (..), GhcPkgMode (..), HaddockMode (..),
    SphinxMode (..), TarMode (..), Builder (..),

    -- * Builder properties
    builderProvenance, systemBuilderPath, builderPath, isSpecified, needBuilder,
    runBuilder, runBuilderWith, runBuilderWithCmdOptions, getBuilderPath,
    builderEnvironment,

    -- * Ad hoc builder invokation
    applyPatch, installDirectory, installData, installScript, installProgram,
    linkSymbolic
    ) where

import Development.Shake.Classes
import GHC.Generics
import qualified Hadrian.Builder as H
import Hadrian.Builder hiding (Builder)
import Hadrian.Builder.Ar
import Hadrian.Builder.Sphinx
import Hadrian.Builder.Tar
import Hadrian.Oracles.Path
import Hadrian.Oracles.TextFile
import Hadrian.Utilities
import qualified System.Directory.Extra as IO

import Base
import Context
import GHC
import Oracles.Flag
import Oracles.Setting

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

-- | Haddock can be used in two different modes:
-- * Generate documentation for a single package
-- * Generate an index page for a collection of packages
data HaddockMode = BuildPackage | BuildIndex deriving (Eq, Generic, Show)

instance Binary   HaddockMode
instance Hashable HaddockMode
instance NFData   HaddockMode

-- | A 'Builder' is an external command invoked in a separate process via 'cmd'.
-- @Ghc Stage0@ is the bootstrapping compiler.
-- @Ghc StageN@, N > 0, is the one built in stage (N - 1).
-- @GhcPkg Stage0@ is the bootstrapping @GhcPkg@.
-- @GhcPkg Stage1@ is the one built in Stage0.
data Builder = Alex
             | Ar ArMode Stage
             | DeriveConstants
             | Cc CcMode Stage
             | Configure FilePath
             | GenApply
             | GenPrimopCode
             | Ghc GhcMode Stage
             | GhcCabal
             | GhcPkg GhcPkgMode Stage
             | Haddock HaddockMode
             | Happy
             | Hpc
             | HsCpp
             | Hsc2Hs
             | Ld
             | Make FilePath
             | Nm
             | Objdump
             | Patch
             | Perl
             | Ranlib
             | Sphinx SphinxMode
             | Tar TarMode
             | Unlit
             | Xelatex
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
    GhcPkg _ Stage0  -> Nothing
    GhcPkg _ _       -> context Stage0 ghcPkg
    Haddock _        -> context Stage2 haddock
    Hpc              -> context Stage1 hpcBin
    Hsc2Hs           -> context Stage0 hsc2hs
    Unlit            -> context Stage0 unlit
    _                -> Nothing
  where
    context s p = Just $ vanillaContext s p

instance H.Builder Builder where
    builderPath :: Builder -> Action FilePath
    builderPath builder = case builderProvenance builder of
        Nothing      -> systemBuilderPath builder
        Just context -> programPath context

    needBuilder :: Builder -> Action ()
    needBuilder builder = do
        path <- H.builderPath builder
        case builder of
            Configure dir -> need [dir -/- "configure"]
            Hsc2Hs        -> need [path, templateHscPath]
            Make dir      -> need [dir -/- "Makefile"]
            _             -> when (isJust $ builderProvenance builder) $ need [path]

    runBuilderWith :: Builder -> BuildInfo -> Action ()
    runBuilderWith builder BuildInfo {..} = do
        path <- builderPath builder
        withResources buildResources $ do
            verbosity <- getVerbosity
            let input  = fromSingleton msgIn buildInputs
                msgIn  = "[runBuilderWith] Exactly one input file expected."
                output = fromSingleton msgOut buildOutputs
                msgOut = "[runBuilderWith] Exactly one output file expected."
                -- Suppress stdout depending on the Shake's verbosity setting.
                echo = EchoStdout (verbosity >= Loud)
                -- Capture stdout and write it to the output file.
                captureStdout = do
                    Stdout stdout <- cmd [path] buildArgs
                    writeFileChanged output stdout
            case builder of
                Ar Pack _ -> do
                    useTempFile <- flag ArSupportsAtFile
                    if useTempFile then runAr                path buildArgs
                                   else runArWithoutTempFile path buildArgs

                Ar Unpack _ -> cmd echo [Cwd output] [path] buildArgs

                Configure dir -> do
                    -- Inject /bin/bash into `libtool`, instead of /bin/sh,
                    -- otherwise Windows breaks. TODO: Figure out why.
                    bash <- bashPath
                    let env = AddEnv "CONFIG_SHELL" bash
                    cmd echo env [Cwd dir] ["sh", path] buildOptions buildArgs

                HsCpp    -> captureStdout
                GenApply -> captureStdout

                GenPrimopCode -> do
                    stdin <- readFile' input
                    Stdout stdout <- cmd (Stdin stdin) [path] buildArgs
                    writeFileChanged output stdout

                Make dir -> cmd echo path ["-C", dir] buildArgs

                Xelatex -> do
                    unit $ cmd [Cwd output] [path]        buildArgs
                    unit $ cmd [Cwd output] [path]        buildArgs
                    unit $ cmd [Cwd output] [path]        buildArgs
                    unit $ cmd [Cwd output] ["makeindex"] (input -<.> "idx")
                    unit $ cmd [Cwd output] [path]        buildArgs
                    unit $ cmd [Cwd output] [path]        buildArgs

                _  -> cmd echo [path] buildArgs

-- TODO: Some builders are required only on certain platforms. For example,
-- 'Objdump' is only required on OpenBSD and AIX. Add support for platform
-- specific optional builders as soon as we can reliably test this feature.
-- See https://github.com/snowleopard/hadrian/issues/211.
isOptional :: Builder -> Bool
isOptional = \case
    Objdump  -> True
    _        -> False

-- | Determine the location of a system 'Builder'.
systemBuilderPath :: Builder -> Action FilePath
systemBuilderPath builder = case builder of
    Alex            -> fromKey "alex"
    Ar _ Stage0     -> fromKey "system-ar"
    Ar _ _          -> fromKey "ar"
    Cc  _  Stage0   -> fromKey "system-cc"
    Cc  _  _        -> fromKey "cc"
    -- We can't ask configure for the path to configure!
    Configure _     -> return "configure"
    Ghc _  Stage0   -> fromKey "system-ghc"
    GhcPkg _ Stage0 -> fromKey "system-ghc-pkg"
    Happy           -> fromKey "happy"
    HsCpp           -> fromKey "hs-cpp"
    Ld              -> fromKey "ld"
    Make _          -> fromKey "make"
    Nm              -> fromKey "nm"
    Objdump         -> fromKey "objdump"
    Patch           -> fromKey "patch"
    Perl            -> fromKey "perl"
    Ranlib          -> fromKey "ranlib"
    Sphinx _        -> fromKey "sphinx-build"
    Tar _           -> fromKey "tar"
    Xelatex         -> fromKey "xelatex"
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

-- | Was the path to a given system 'Builder' specified in configuration files?
isSpecified :: Builder -> Action Bool
isSpecified = fmap (not . null) . systemBuilderPath

-- | Apply a patch by executing the 'Patch' builder in a given directory.
applyPatch :: FilePath -> FilePath -> Action ()
applyPatch dir patch = do
    let file = dir -/- patch
    needBuilder Patch
    path <- builderPath Patch
    putBuild $ "| Apply patch " ++ file
    quietly $ cmd [Cwd dir, FileStdin file] [path, "-p0"]

-- | Install a directory.
installDirectory :: FilePath -> Action ()
installDirectory dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallDir
    putBuild $ "| Install directory " ++ dir
    quietly $ cmd path dir

-- | Install data files to a directory and track them.
installData :: [FilePath] -> FilePath -> Action ()
installData fs dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallData
    need fs
    forM_ fs $ \f -> putBuild $ "| Install data " ++ f ++ " to " ++ dir
    quietly $ cmd path fs dir

-- | Install an executable file to a directory and track it.
installProgram :: FilePath -> FilePath -> Action ()
installProgram f dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallProgram
    need [f]
    putBuild $ "| Install program " ++ f ++ " to " ++ dir
    quietly $ cmd path f dir

-- | Install an executable script to a directory and track it.
installScript :: FilePath -> FilePath -> Action ()
installScript f dir = do
    path <- fixAbsolutePathOnWindows =<< setting InstallScript
    need [f]
    putBuild $ "| Install script " ++ f ++ " to " ++ dir
    quietly $ cmd path f dir

-- | Create a symbolic link from source file to target file (when symbolic links
-- are supported) and track the source file.
linkSymbolic :: FilePath -> FilePath -> Action ()
linkSymbolic source target = do
    lns <- setting LnS
    unless (null lns) $ do
        need [source] -- Guarantee source is built before printing progress info.
        let dir = takeDirectory target
        liftIO $ IO.createDirectoryIfMissing True dir
        putProgressInfo =<< renderAction "Create symbolic link" source target
        quietly $ cmd lns source target
