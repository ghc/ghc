{-# LANGUAGE InstanceSigs, TypeOperators #-}
module Builder (
    -- * Data types
    ArMode (..), CcMode (..), ConfigurationInfo (..), DependencyType (..),
    GhcMode (..), GhcPkgMode (..), HaddockMode (..), TestMode(..), SphinxMode (..),
    TarMode (..), GitMode (..), Builder (..), Win32TarballsMode(..),

    -- * Builder properties
    builderProvenance, systemBuilderPath, builderPath, isSpecified, needBuilder,
    runBuilder, runBuilderWith, runBuilderWithCmdOptions, getBuilderPath,
    builderEnvironment,

    -- * Ad hoc builder invocation
    applyPatch
    ) where

import Control.Exception.Extra (Partial)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Development.Shake.Classes
import Development.Shake.Command
import Development.Shake.FilePath
import GHC.Generics
import qualified Hadrian.Builder as H
import Hadrian.Builder hiding (Builder)
import Hadrian.Builder.Ar
import Hadrian.Builder.Sphinx
import Hadrian.Builder.Tar
import Hadrian.Oracles.Path
import Hadrian.Oracles.TextFile
import Hadrian.Utilities
import Oracles.Setting (bashPath)
import System.Exit
import System.IO (stderr)

import Base
import Context
import Oracles.Flag
import Oracles.Setting (setting, Setting(..))
import Packages

import GHC.IO.Encoding (getFileSystemEncoding)
import qualified Data.ByteString as BS
import qualified GHC.Foreign as GHC

-- | C compiler can be used in two different modes:
-- * Compile or preprocess a source file.
-- * Extract source dependencies by passing @-MM@ command line argument.
data CcMode = CompileC | FindCDependencies DependencyType deriving (Eq, Generic, Show)
data DependencyType = CDep | CxxDep deriving (Eq, Generic, Show)

instance Binary   CcMode
instance Hashable CcMode
instance NFData   CcMode

instance Binary   DependencyType
instance Hashable DependencyType
instance NFData   DependencyType

-- | GHC can be used in four different modes:
-- * Compile a Haskell source file.
-- * Compile a C source file.
-- * Extract source dependencies by passing @-M@ command line argument.
-- * Link object files & static libraries into an executable.
data GhcMode = CompileHs
             | CompileCWithGhc
             | CompileCppWithGhc
             | FindHsDependencies
             | LinkHs
             | ToolArgs
    deriving (Eq, Generic, Show)

instance Binary   GhcMode
instance Hashable GhcMode
instance NFData   GhcMode

-- | To configure a package we need two pieces of information, which we choose
-- to record separately for convenience.
--
-- * Command line arguments to be passed to the setup script.
--
-- * Package configuration flags that enable/disable certain package features.
--   Here is an example from "Settings.Packages":
--
--   > package rts
--   >   ? builder (Cabal Flags)
--   >   ? any (wayUnit Profiling) rtsWays
--   >   ? arg "profiling"
--
--   This instructs package configuration functions (such as 'configurePackage')
--   to enable the @profiling@ Cabal flag when processing @rts.cabal@ and
--   building RTS with profiling information.
data ConfigurationInfo = Setup | Flags deriving (Eq, Generic, Show)

instance Binary   ConfigurationInfo
instance Hashable ConfigurationInfo
instance NFData   ConfigurationInfo

-- TODO: Do we really need all these modes? Why do we need 'Dependencies'? We
-- can extract dependencies using the Cabal library. Note: we used to also have
-- the @Init@ mode for initialising a new package database but we've deleted it.
-- | 'GhcPkg' can initialise a package database and register packages in it.
data GhcPkgMode = Copy         -- ^ Copy a package from one database to another.
                | Dependencies -- ^ Compute package dependencies.
                | Unregister   -- ^ Unregister a package.
                | Update       -- ^ Update a package.
                deriving (Eq, Generic, Show)

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

-- | The testsuite driver can be called in two different modes:
-- * Actually run the tests
-- * Get all the hadrian targets needed for the current test configuration
data TestMode = RunTest | GetExtraDeps deriving (Eq, Generic, Show)

instance Binary   TestMode
instance Hashable TestMode
instance NFData   TestMode

-- | Git is used to create source distributions
data GitMode = ListFiles deriving (Eq, Generic, Show)

instance Binary   GitMode
instance Hashable GitMode
instance NFData   GitMode

data Win32TarballsMode = ListTarballs | VerifyTarballs | DownloadTarballs deriving (Eq, Generic, Show)

instance Binary   Win32TarballsMode
instance Hashable Win32TarballsMode
instance NFData   Win32TarballsMode



-- | A 'Builder' is a (usually external) command invoked in a separate process
-- via 'cmd'. Here are some examples:
-- * 'Alex' is a lexical analyser generator that builds @Lexer.hs@ from @Lexer.x@.
-- * 'Ghc' 'Stage0' is the bootstrapping Haskell compiler used in 'Stage0'.
-- * 'Ghc' @StageN@ (N > 0) is the GHC built in stage (N - 1) and used in @StageN@.
--
-- The 'Cabal' builder is unusual in that it does not correspond to an external
-- program but instead relies on the Cabal library for package configuration.
data Builder = Alex
             | Ar ArMode Stage
             | Autoreconf FilePath
             | Cabal ConfigurationInfo Stage
             | Cc CcMode Stage
             | Configure FilePath
             | DeriveConstants
             | GenApply
             | GenPrimopCode
             | Ghc GhcMode Stage
             | GhcPkg GhcPkgMode Stage
             | Haddock HaddockMode
             | Happy
             | Hp2Ps
             | Hpc
             | HsCpp
             | Hsc2Hs Stage
             | Ld Stage --- ^ linker
             | Make FilePath
             | Makeinfo
             | MergeObjects Stage -- ^ linker to be used to merge object files.
             | Nm
             | Objdump
             | Patch
             | Python
             | Ranlib
             | Testsuite TestMode
             | Sphinx SphinxMode
             | Tar TarMode
             | Unlit
             | Xelatex
             | Makeindex  -- ^ from xelatex
             | Git GitMode
             | Win32Tarballs Win32TarballsMode
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
    GhcPkg _ Stage0  -> Nothing
    GhcPkg _ s       -> context (pred s) ghcPkg
    Haddock _        -> context Stage1 haddock
    Hpc              -> context Stage1 hpcBin
    Hp2Ps            -> context Stage0 hp2ps
    Hsc2Hs _         -> context Stage0 hsc2hs
    Unlit            -> context Stage0 unlit
    _                -> Nothing
  where
    context s p = Just $ vanillaContext s p

instance H.Builder Builder where
    builderPath :: Builder -> Action FilePath
    builderPath builder = case builderProvenance builder of
        Nothing      -> systemBuilderPath builder
        Just context -> programPath context

    runtimeDependencies :: Builder -> Action [FilePath]
    runtimeDependencies = \case
        Autoreconf dir -> return [dir -/- "configure.ac"]
        Configure  dir -> return [dir -/- "configure"]

        Ghc _ Stage0 -> do
          -- Read the boot GHC version here to make sure we rebuild when it
          -- changes (#18001).
          _bootGhcVersion <- setting GhcVersion
          pure []
        Ghc _ stage -> do
            root <- buildRoot
            touchyPath <- programPath (vanillaContext Stage0 touchy)
            unlitPath  <- builderPath Unlit

            -- GHC from the previous stage is used to build artifacts in the
            -- current stage. Need the previous stage's GHC deps.
            ghcdeps <- ghcBinDeps (pred stage)

            return $ [ unlitPath ]
                  ++ ghcdeps
                  ++ [ touchyPath          | windowsHost ]
                  ++ [ root -/- mingwStamp | windowsHost ]
                     -- proxy for the entire mingw toolchain that
                     -- we have in inplace/mingw initially, and then at
                     -- root -/- mingw.

        Hsc2Hs stage -> (\p -> [p]) <$> templateHscPath stage
        Make dir  -> return [dir -/- "Makefile"]
        Haddock _ -> haddockDeps Stage1  -- Haddock currently runs in Stage1
        _         -> return []

    -- query the builder for some information.
    -- contrast this with runBuilderWith, which returns @Action ()@
    -- this returns the @stdout@ from running the builder.
    -- For now this only implements asking @ghc-pkg@ about package
    -- dependencies and asking the testsuite driver about hadrian
    -- dependencies for tests.
    askBuilderWith :: Builder -> BuildInfo -> Action String
    askBuilderWith builder BuildInfo {..} = case builder of
        GhcPkg Dependencies _ -> do
            let input  = fromSingleton msgIn buildInputs
                msgIn  = "[askBuilder] Exactly one input file expected."
            needBuilder builder
            path <- H.builderPath builder
            -- we do not depend on bare builders. E.g. we won't depend on `clang`
            -- or `ld` or `ar`.  Unless they are provided with fully qualified paths
            -- this is the job of the person invoking ./configure to pass e.g.
            -- CC=$(which clang) if they want the fully qualified clang path!
            when (path /= takeFileName path) $
                need [path]
            Stdout stdout <- cmd' [path] ["--no-user-package-db", "field", input, "depends"]
            return stdout
        Testsuite GetExtraDeps -> do
          path <- builderPath builder
          withResources buildResources $
              withTempFile $ \temp -> do
                () <- cmd' [path] (buildArgs ++ ["--only-report-hadrian-deps", temp])
                readFile' temp
        Git ListFiles -> do
          path <- builderPath builder
          withResources buildResources $ do
              -- NUL separated list of files
              -- We need to read this in the filesystem encoding
              enc <- liftIO getFileSystemEncoding
              Stdout stdout <- cmd' BinaryPipes [path] buildArgs
              liftIO $ BS.useAsCStringLen stdout $ \fp -> GHC.peekCStringLen enc fp
        Win32Tarballs ListTarballs -> do
          path <- builderPath builder
          withResources buildResources $ do
              Stdout stdout <- cmd' [path] buildArgs
              pure stdout
        _ -> error $ "Builder " ++ show builder ++ " can not be asked!"

    runBuilderWith :: Builder -> BuildInfo -> Action ()
    runBuilderWith builder BuildInfo {..} = do
        path <- builderPath builder
        withResources buildResources $ do
            verbosity <- getVerbosity
            let input  = fromSingleton msgIn buildInputs
                msgIn  = "[runBuilderWith] Exactly one input file expected."
                output = fromSingleton msgOut buildOutputs
                msgOut = "[runBuilderWith] Exactly one output file expected."
                -- Capture stdout and write it to the output file.
                captureStdout = do
                    Stdout stdout <- cmd' [path] buildArgs
                    writeFileChanged output stdout
            case builder of
                Ar Pack _ -> do
                    useTempFile <- flag ArSupportsAtFile
                    if useTempFile then runAr                path buildArgs
                                   else runArWithoutTempFile path buildArgs

                Ar Unpack _ -> cmd' [Cwd output] [path] buildArgs

                Autoreconf dir -> do
                  bash <- bashPath
                  cmd' [Cwd dir] [bash, path] buildArgs

                Configure  dir -> do
                    -- Inject /bin/bash into `libtool`, instead of /bin/sh,
                    -- otherwise Windows breaks. TODO: Figure out why.
                    bash <- bashPath
                    let env = AddEnv "CONFIG_SHELL" bash
                    cmd' env [Cwd dir] ["sh", path] buildOptions buildArgs

                GenApply -> captureStdout

                GenPrimopCode -> do
                    stdin <- readFile' input
                    Stdout stdout <- cmd' (Stdin stdin) [path] buildArgs
                    writeFileChanged output stdout

                GhcPkg Copy _ -> do
                    Stdout pkgDesc <- cmd' [path]
                      [ "--expand-pkgroot"
                      , "--no-user-package-db"
                      , "describe"
                      , input -- the package name
                      ]
                    cmd' (Stdin pkgDesc) [path] (buildArgs ++ ["-"])

                GhcPkg Unregister _ -> do
                    -- unregistering is allowed to fail (e.g. when a package
                    -- isn't already present)
                    Exit _ <- cmd' [path] (buildArgs ++ [input])
                    return ()

                HsCpp    -> captureStdout

                Make dir -> cmd' path ["-C", dir] buildArgs

                Makeinfo -> do
                  cmd' [path] "--no-split" [ "-o", output] [input]

                Xelatex   ->
                  -- xelatex produces an incredible amount of output, almost
                  -- all of which is useless. Suppress it unless user
                  -- requests a loud build.
                  if verbosity >= Diagnostic
                    then cmd' [Cwd output] [path] buildArgs
                    else do (Stdouterr out, Exit code) <- cmd' [Cwd output] [path] buildArgs
                            when (code /= ExitSuccess) $ do
                              liftIO $ BSL.hPutStrLn stderr out
                              putFailure "xelatex failed!"
                              fail "xelatex failed"

                Makeindex -> unit $ cmd' [Cwd output] [path] (buildArgs ++ [input])

                Tar _ -> cmd' buildOptions [path] buildArgs

                -- RunTest produces a very large amount of (colorised) output;
                -- Don't attempt to capture it.
                Testsuite RunTest -> do
                  Exit code <- cmd [path] buildArgs
                  when (code /= ExitSuccess) $ do
                    fail "tests failed"

                _  -> cmd' [path] buildArgs

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
    Autoreconf _    -> stripExe =<< fromKey "autoreconf"
    Cc  _  Stage0   -> fromKey "system-cc"
    Cc  _  _        -> fromKey "cc"
    -- We can't ask configure for the path to configure!
    Configure _     -> return "configure"
    Ghc _  Stage0   -> fromKey "system-ghc"
    GhcPkg _ Stage0 -> fromKey "system-ghc-pkg"
    Happy           -> fromKey "happy"
    HsCpp           -> fromKey "hs-cpp"
    Ld _            -> fromKey "ld"
    -- MergeObjects Stage0 is a special case in case of
    -- cross-compiling. We're building stage1, e.g. code which will be
    -- executed on the host and hence we need to use host's merge
    -- objects tool and not the target merge object tool.
    -- Note, merge object tool is usually platform linker with some
    -- parameters. E.g. building a cross-compiler on and for x86_64
    -- which will target ppc64 means that MergeObjects Stage0 will use
    -- x86_64 linker and MergeObject _ will use ppc64 linker.
    MergeObjects Stage0 -> fromKey "system-merge-objects"
    MergeObjects _  -> fromKey "merge-objects"
    Make _          -> fromKey "make"
    Makeinfo        -> fromKey "makeinfo"
    Nm              -> fromKey "nm"
    Objdump         -> fromKey "objdump"
    Patch           -> fromKey "patch"
    Python          -> fromKey "python"
    Ranlib          -> fromKey "ranlib"
    Testsuite _     -> fromKey "python"
    Sphinx _        -> fromKey "sphinx-build"
    Tar _           -> fromKey "tar"
    Git _           -> fromKey "git"
    Xelatex         -> fromKey "xelatex"
    Makeindex       -> fromKey "makeindex"
    Win32Tarballs _ -> fromKey "python"
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
        else do
            -- angerman: I find this lookupInPath rather questionable.
            -- if we specify CC, LD, ... *without* a path, that is intentional
            -- lookupInPath should be done by the person invoking the configure
            -- script iif they want to have that full path, if they just want
            -- some generic tool name (on purpose!) the build system should not
            -- go behind their backs to add a path they likely never wanted.
            fullPath <- lookupInPath path
            case (windowsHost, hasExtension fullPath) of
                (False, _    ) -> return path
                (True , True ) -> fixAbsolutePathOnWindows fullPath
                (True , False) -> fixAbsolutePathOnWindows fullPath <&> (<.> exe)

    -- Without this function, on Windows we can observe a bad builder path
    -- for 'autoreconf'. If the relevant system.config field is set to
    -- /usr/bin/autoreconf in the file, the path that we read
    -- is C:/msys64/usr/bin/autoreconf.exe. A standard msys2 set up happens
    -- to have an executable named 'autoreconf' there, without the 'exe'
    -- extension. Hence this function.
    stripExe s = do
        let sNoExt = dropExtension s
        exists <- doesFileExist s
        if exists then return s else return sNoExt


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
    quietly $ cmd' [Cwd dir, FileStdin file] [path, "-p0"]

-- Note [cmd wrapper]
-- ~~~~~~~~~~~~~~~~~~
-- `cmd'` is a wrapper for Shake's `cmd` that allows us to customize what is
-- output in the terminal in case of failure.
--
-- However `cmd` is quite a complex function because:
--
--  1) it relies on a CmdArguments type class to be variadic -- it can be called
--  with any number of arguments, as long as they are valid arguments -- and to
--  return either "Action r" or "IO r".
--
--  2) its behavior depends on the returned "r" type! In particular, if it has
--  to return a value of type Exit or ExitCode, then it doesn't raise an
--  exception if the exit code isn't 0! It also doesn't echo the command
--  stdout/stderr if it is requested in a Stdout/Stderr/Stdouterr result. Result
--  types are handled via CmdResult type class.
--
-- To wrap `cmd` while keeping its behavior, we need to replicate some of these
-- type classes.
--
--  1) CmdWrap corresponds to CmdArguments except that we do our own stuff in
--  the base case (i.e. in the instance for `Action r`).
--
--  2) Sadly CmdResult internals aren't exposed by Shake, so when we get a
--  `CmdResult r => r` we can't tell anything about `r`. In particular, we can't
--  tell if an Exit or ExitCode value is returned in `r`. So we use our own
--  HasExit type class to provide the `hasExit` predicate that tells us if we
--  should throw an exception as `cmd` would do in case of failure or not.


-- | Wrapper for Shake's 'cmd'
--
-- See Note [cmd wrapper]
cmd' :: (Partial, CmdWrap args) => args :-> Action r
cmd' = cmdArgs mempty


-- See Note [cmd wrapper]
class HasExit a where
  -- | Indicate if `a` is Exit or ExitCode
  -- See Note [cmd wrapper]
  hasExit :: a -> Bool

instance HasExit ExitCode      where hasExit = const True
instance HasExit Exit          where hasExit = const True
instance HasExit ()            where hasExit = const False
instance HasExit (Stdouterr a) where hasExit = const False
instance HasExit (Stdout    a) where hasExit = const False

instance (HasExit a, HasExit b) => HasExit (a,b) where
  hasExit (a,b) = hasExit a || hasExit b
instance (HasExit a, HasExit b, HasExit c) => HasExit (a,b,c) where
  hasExit (a,b,c) = hasExit a || hasExit b || hasExit c

class CmdWrap t where
  cmdArgs :: Partial => CmdArgument -> t

instance (IsCmdArgument a, CmdWrap r) => CmdWrap (a -> r) where
  cmdArgs xs x = cmdArgs $ xs `mappend` toCmdArgument x

instance CmdWrap CmdArgument where
  cmdArgs = id

instance (HasExit r, CmdResult r) => CmdWrap (Action r) where
  cmdArgs (CmdArgument x) = do
    verbosity <- getVerbosity

    let real_args = mconcat
          [ -- don't print stderr and stdout in command failure exception
            toCmdArgument (WithStderr False)
          , toCmdArgument (WithStdout False)
            -- caller specified arguments come last to allow them to overload
            -- the previous ones.
          , CmdArgument x
          ]
    (Stdout out, Stderr err, cmdline :: CmdLine, Exit code, r :: r) <- cmd real_args

    if hasExit r
      -- if the caller queries the exit code of the command, we don't do
      -- anything here. In particular we don't throw an exception.
      -- (this is used e.g. to allow ghc-pkg to fail to unregister)
      -- See Note [cmd wrapper]
      then pure r
      else do
        -- In every case, we only print both command outputs (stdout/stderr)
        -- onto Hadrian's stderr because Hadrian's stdout may be piped into
        -- another process and we don't want random command output to break
        -- this.
        --
        -- For example, the result of "hadrian tool:ghc/Main.hs --flavour=ghc-in-ghci"
        -- is directly passed as arguments for ghc in "hadrian/ghci-cabal" script.
        let dump x = liftIO (BSL.hPutStr stderr x)
        case code of
          ExitSuccess   -> do
            -- Suppress stdout/stderr depending on Shake's verbosity setting
            when (verbosity >  Silent)  (dump err)
            when (verbosity >= Verbose) (dump out)
            pure r
          ExitFailure i -> do
            putError ("Command line: " ++ fromCmdLine cmdline)
            putError ("===> Command failed with error code: " ++ show i)
            dump err
            dump out
            error "Command failed"
