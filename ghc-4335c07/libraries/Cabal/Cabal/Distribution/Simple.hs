{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the command line front end to the Simple build system. When given
-- the parsed command-line args and package information, is able to perform
-- basic commands like configure, build, install, register, etc.
--
-- This module exports the main functions that Setup.hs scripts use. It
-- re-exports the 'UserHooks' type, the standard entry points like
-- 'defaultMain' and 'defaultMainWithHooks' and the predefined sets of
-- 'UserHooks' that custom @Setup.hs@ scripts can extend to add their own
-- behaviour.
--
-- This module isn't called \"Simple\" because it's simple.  Far from
-- it.  It's called \"Simple\" because it does complicated things to
-- simple software.
--
-- The original idea was that there could be different build systems that all
-- presented the same compatible command line interfaces. There is still a
-- "Distribution.Make" system but in practice no packages use it.

{-
Work around this warning:
libraries/Cabal/Distribution/Simple.hs:78:0:
    Warning: In the use of `runTests'
             (imported from Distribution.Simple.UserHooks):
             Deprecated: "Please use the new testing interface instead!"
-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Distribution.Simple (
        module Distribution.Package,
        module Distribution.Version,
        module Distribution.License,
        module Distribution.Simple.Compiler,
        module Language.Haskell.Extension,
        -- * Simple interface
        defaultMain, defaultMainNoRead, defaultMainArgs,
        -- * Customization
        UserHooks(..), Args,
        defaultMainWithHooks, defaultMainWithHooksArgs,
        defaultMainWithHooksNoRead, defaultMainWithHooksNoReadArgs,
        -- ** Standard sets of hooks
        simpleUserHooks,
        autoconfUserHooks,
        defaultUserHooks, emptyUserHooks,
        -- ** Utils
        defaultHookedPackageDesc
  ) where

import Prelude ()
import Distribution.Compat.Prelude

-- local
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Simple.UserHooks
import Distribution.Package
import Distribution.PackageDescription hiding (Flag)
import Distribution.PackageDescription.Configuration
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.Simple.Command

import Distribution.Simple.Build
import Distribution.Simple.SrcDist
import Distribution.Simple.Register

import Distribution.Simple.Configure

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Bench
import Distribution.Simple.BuildPaths
import Distribution.Simple.Test
import Distribution.Simple.Install
import Distribution.Simple.Haddock
import Distribution.Simple.Doctest
import Distribution.Simple.Utils
import Distribution.Utils.NubList
import Distribution.Verbosity
import Language.Haskell.Extension
import Distribution.Version
import Distribution.License
import Distribution.Text

-- Base
import System.Environment (getArgs, getProgName)
import System.Directory   (removeFile, doesFileExist
                          ,doesDirectoryExist, removeDirectoryRecursive)
import System.Exit                          (exitWith,ExitCode(..))
import System.FilePath                      (searchPathSeparator)
import Distribution.Compat.Environment      (getEnvironment)
import Distribution.Compat.GetShortPathName (getShortPathName)

import Data.List       (unionBy, (\\))

import Distribution.PackageDescription.Parsec

-- | A simple implementation of @main@ for a Cabal setup script.
-- It reads the package description file using IO, and performs the
-- action specified on the command line.
defaultMain :: IO ()
defaultMain = getArgs >>= defaultMainHelper simpleUserHooks

-- | A version of 'defaultMain' that is passed the command line
-- arguments, rather than getting them from the environment.
defaultMainArgs :: [String] -> IO ()
defaultMainArgs = defaultMainHelper simpleUserHooks

-- | A customizable version of 'defaultMain'.
defaultMainWithHooks :: UserHooks -> IO ()
defaultMainWithHooks hooks = getArgs >>= defaultMainHelper hooks

-- | A customizable version of 'defaultMain' that also takes the command
-- line arguments.
defaultMainWithHooksArgs :: UserHooks -> [String] -> IO ()
defaultMainWithHooksArgs = defaultMainHelper

-- | Like 'defaultMain', but accepts the package description as input
-- rather than using IO to read it.
defaultMainNoRead :: GenericPackageDescription -> IO ()
defaultMainNoRead = defaultMainWithHooksNoRead simpleUserHooks

-- | A customizable version of 'defaultMainNoRead'.
defaultMainWithHooksNoRead :: UserHooks -> GenericPackageDescription -> IO ()
defaultMainWithHooksNoRead hooks pkg_descr =
  getArgs >>=
  defaultMainHelper hooks { readDesc = return (Just pkg_descr) }

-- | A customizable version of 'defaultMainNoRead' that also takes the
-- command line arguments.
--
-- @since 2.2.0.0
defaultMainWithHooksNoReadArgs :: UserHooks -> GenericPackageDescription -> [String] -> IO ()
defaultMainWithHooksNoReadArgs hooks pkg_descr =
  defaultMainHelper hooks { readDesc = return (Just pkg_descr) }

defaultMainHelper :: UserHooks -> Args -> IO ()
defaultMainHelper hooks args = topHandler $
  case commandsRun (globalCommand commands) commands args of
    CommandHelp   help                 -> printHelp help
    CommandList   opts                 -> printOptionsList opts
    CommandErrors errs                 -> printErrors errs
    CommandReadyToGo (flags, commandParse)  ->
      case commandParse of
        _ | fromFlag (globalVersion flags)        -> printVersion
          | fromFlag (globalNumericVersion flags) -> printNumericVersion
        CommandHelp     help           -> printHelp help
        CommandList     opts           -> printOptionsList opts
        CommandErrors   errs           -> printErrors errs
        CommandReadyToGo action        -> action

  where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (intercalate "\n" errs)
      exitWith (ExitFailure 1)
    printNumericVersion = putStrLn $ display cabalVersion
    printVersion        = putStrLn $ "Cabal library version "
                                  ++ display cabalVersion

    progs = addKnownPrograms (hookedPrograms hooks) defaultProgramDb
    commands =
      [configureCommand progs `commandAddAction`
        \fs as -> configureAction hooks fs as >> return ()
      ,buildCommand     progs `commandAddAction` buildAction        hooks
      ,replCommand      progs `commandAddAction` replAction         hooks
      ,installCommand         `commandAddAction` installAction      hooks
      ,copyCommand            `commandAddAction` copyAction         hooks
      ,doctestCommand         `commandAddAction` doctestAction      hooks
      ,haddockCommand         `commandAddAction` haddockAction      hooks
      ,cleanCommand           `commandAddAction` cleanAction        hooks
      ,sdistCommand           `commandAddAction` sdistAction        hooks
      ,hscolourCommand        `commandAddAction` hscolourAction     hooks
      ,registerCommand        `commandAddAction` registerAction     hooks
      ,unregisterCommand      `commandAddAction` unregisterAction   hooks
      ,testCommand            `commandAddAction` testAction         hooks
      ,benchmarkCommand       `commandAddAction` benchAction        hooks
      ]

-- | Combine the preprocessors in the given hooks with the
-- preprocessors built into cabal.
allSuffixHandlers :: UserHooks
                  -> [PPSuffixHandler]
allSuffixHandlers hooks
    = overridesPP (hookedPreProcessors hooks) knownSuffixHandlers
    where
      overridesPP :: [PPSuffixHandler] -> [PPSuffixHandler] -> [PPSuffixHandler]
      overridesPP = unionBy (\x y -> fst x == fst y)

configureAction :: UserHooks -> ConfigFlags -> Args -> IO LocalBuildInfo
configureAction hooks flags args = do
    distPref <- findDistPrefOrDefault (configDistPref flags)
    let flags' = flags { configDistPref = toFlag distPref
                       , configArgs = args }

    -- See docs for 'HookedBuildInfo'
    pbi <- preConf hooks args flags'

    (mb_pd_file, pkg_descr0) <- confPkgDescr hooks verbosity
                                    (flagToMaybe (configCabalFilePath flags))

    let epkg_descr = (pkg_descr0, pbi)

    localbuildinfo0 <- confHook hooks epkg_descr flags'

    -- remember the .cabal filename if we know it
    -- and all the extra command line args
    let localbuildinfo = localbuildinfo0 {
                           pkgDescrFile = mb_pd_file,
                           extraConfigArgs = args
                         }
    writePersistBuildConfig distPref localbuildinfo

    let pkg_descr = localPkgDescr localbuildinfo
    postConf hooks args flags' pkg_descr localbuildinfo
    return localbuildinfo
  where
    verbosity = fromFlag (configVerbosity flags)

confPkgDescr :: UserHooks -> Verbosity -> Maybe FilePath
             -> IO (Maybe FilePath, GenericPackageDescription)
confPkgDescr hooks verbosity mb_path = do
  mdescr <- readDesc hooks
  case mdescr of
    Just descr -> return (Nothing, descr)
    Nothing -> do
        pdfile <- case mb_path of
                    Nothing -> defaultPackageDesc verbosity
                    Just path -> return path
        info verbosity "Using Parsec parser"
        descr  <- readGenericPackageDescription verbosity pdfile
        return (Just pdfile, descr)

buildAction :: UserHooks -> BuildFlags -> Args -> IO ()
buildAction hooks flags args = do
  distPref <- findDistPrefOrDefault (buildDistPref flags)
  let verbosity = fromFlag $ buildVerbosity flags
      flags' = flags { buildDistPref = toFlag distPref }

  lbi <- getBuildConfig hooks verbosity distPref
  progs <- reconfigurePrograms verbosity
             (buildProgramPaths flags')
             (buildProgramArgs flags')
             (withPrograms lbi)

  hookedAction preBuild buildHook postBuild
               (return lbi { withPrograms = progs })
               hooks flags' { buildArgs = args } args

replAction :: UserHooks -> ReplFlags -> Args -> IO ()
replAction hooks flags args = do
  distPref <- findDistPrefOrDefault (replDistPref flags)
  let verbosity = fromFlag $ replVerbosity flags
      flags' = flags { replDistPref = toFlag distPref }

  lbi <- getBuildConfig hooks verbosity distPref
  progs <- reconfigurePrograms verbosity
             (replProgramPaths flags')
             (replProgramArgs flags')
             (withPrograms lbi)

  -- As far as I can tell, the only reason this doesn't use
  -- 'hookedActionWithArgs' is because the arguments of 'replHook'
  -- takes the args explicitly.  UGH.   -- ezyang
  pbi <- preRepl hooks args flags'
  let pkg_descr0 = localPkgDescr lbi
  sanityCheckHookedBuildInfo pkg_descr0 pbi
  let pkg_descr = updatePackageDescription pbi pkg_descr0
      lbi' = lbi { withPrograms = progs
                 , localPkgDescr = pkg_descr }
  replHook hooks pkg_descr lbi' hooks flags' args
  postRepl hooks args flags' pkg_descr lbi'

hscolourAction :: UserHooks -> HscolourFlags -> Args -> IO ()
hscolourAction hooks flags args = do
    distPref <- findDistPrefOrDefault (hscolourDistPref flags)
    let verbosity = fromFlag $ hscolourVerbosity flags
        flags' = flags { hscolourDistPref = toFlag distPref }
    hookedAction preHscolour hscolourHook postHscolour
                 (getBuildConfig hooks verbosity distPref)
                 hooks flags' args

doctestAction :: UserHooks -> DoctestFlags -> Args -> IO ()
doctestAction hooks flags args = do
  distPref <- findDistPrefOrDefault (doctestDistPref flags)
  let verbosity = fromFlag $ doctestVerbosity flags
      flags' = flags { doctestDistPref = toFlag distPref }

  lbi <- getBuildConfig hooks verbosity distPref
  progs <- reconfigurePrograms verbosity
             (doctestProgramPaths flags')
             (doctestProgramArgs  flags')
             (withPrograms lbi)

  hookedAction preDoctest doctestHook postDoctest
               (return lbi { withPrograms = progs })
               hooks flags' args

haddockAction :: UserHooks -> HaddockFlags -> Args -> IO ()
haddockAction hooks flags args = do
  distPref <- findDistPrefOrDefault (haddockDistPref flags)
  let verbosity = fromFlag $ haddockVerbosity flags
      flags' = flags { haddockDistPref = toFlag distPref }

  lbi <- getBuildConfig hooks verbosity distPref
  progs <- reconfigurePrograms verbosity
             (haddockProgramPaths flags')
             (haddockProgramArgs flags')
             (withPrograms lbi)

  hookedAction preHaddock haddockHook postHaddock
               (return lbi { withPrograms = progs })
               hooks flags' args

cleanAction :: UserHooks -> CleanFlags -> Args -> IO ()
cleanAction hooks flags args = do
    distPref <- findDistPrefOrDefault (cleanDistPref flags)
    let flags' = flags { cleanDistPref = toFlag distPref }

    pbi <- preClean hooks args flags'

    (_, ppd) <- confPkgDescr hooks verbosity Nothing
    -- It might seem like we are doing something clever here
    -- but we're really not: if you look at the implementation
    -- of 'clean' in the end all the package description is
    -- used for is to clear out @extra-tmp-files@.  IMO,
    -- the configure script goo should go into @dist@ too!
    --          -- ezyang
    let pkg_descr0 = flattenPackageDescription ppd
    -- We don't sanity check for clean as an error
    -- here would prevent cleaning:
    --sanityCheckHookedBuildInfo pkg_descr0 pbi
    let pkg_descr = updatePackageDescription pbi pkg_descr0

    cleanHook hooks pkg_descr () hooks flags'
    postClean hooks args flags' pkg_descr ()
  where
    verbosity = fromFlag (cleanVerbosity flags)

copyAction :: UserHooks -> CopyFlags -> Args -> IO ()
copyAction hooks flags args = do
    distPref <- findDistPrefOrDefault (copyDistPref flags)
    let verbosity = fromFlag $ copyVerbosity flags
        flags' = flags { copyDistPref = toFlag distPref }
    hookedAction preCopy copyHook postCopy
                 (getBuildConfig hooks verbosity distPref)
                 hooks flags' { copyArgs = args } args

installAction :: UserHooks -> InstallFlags -> Args -> IO ()
installAction hooks flags args = do
    distPref <- findDistPrefOrDefault (installDistPref flags)
    let verbosity = fromFlag $ installVerbosity flags
        flags' = flags { installDistPref = toFlag distPref }
    hookedAction preInst instHook postInst
                 (getBuildConfig hooks verbosity distPref)
                 hooks flags' args

sdistAction :: UserHooks -> SDistFlags -> Args -> IO ()
sdistAction hooks flags args = do
    distPref <- findDistPrefOrDefault (sDistDistPref flags)
    let flags' = flags { sDistDistPref = toFlag distPref }
    pbi <- preSDist hooks args flags'

    mlbi <- maybeGetPersistBuildConfig distPref

    -- NB: It would be TOTALLY WRONG to use the 'PackageDescription'
    -- store in the 'LocalBuildInfo' for the rest of @sdist@, because
    -- that would result in only the files that would be built
    -- according to the user's configure being packaged up.
    -- In fact, it is not obvious why we need to read the
    -- 'LocalBuildInfo' in the first place, except that we want
    -- to do some architecture-independent preprocessing which
    -- needs to be configured.  This is totally awful, see
    -- GH#130.

    (_, ppd) <- confPkgDescr hooks verbosity Nothing

    let pkg_descr0 = flattenPackageDescription ppd
    sanityCheckHookedBuildInfo pkg_descr0 pbi
    let pkg_descr = updatePackageDescription pbi pkg_descr0
        mlbi' = fmap (\lbi -> lbi { localPkgDescr = pkg_descr }) mlbi

    sDistHook hooks pkg_descr mlbi' hooks flags'
    postSDist hooks args flags' pkg_descr mlbi'
  where
    verbosity = fromFlag (sDistVerbosity flags)

testAction :: UserHooks -> TestFlags -> Args -> IO ()
testAction hooks flags args = do
    distPref <- findDistPrefOrDefault (testDistPref flags)
    let verbosity = fromFlag $ testVerbosity flags
        flags' = flags { testDistPref = toFlag distPref }

    localBuildInfo <- getBuildConfig hooks verbosity distPref
    let pkg_descr = localPkgDescr localBuildInfo
    -- It is safe to do 'runTests' before the new test handler because the
    -- default action is a no-op and if the package uses the old test interface
    -- the new handler will find no tests.
    runTests hooks args False pkg_descr localBuildInfo
    hookedActionWithArgs preTest testHook postTest
            (getBuildConfig hooks verbosity distPref)
            hooks flags' args

benchAction :: UserHooks -> BenchmarkFlags -> Args -> IO ()
benchAction hooks flags args = do
    distPref <- findDistPrefOrDefault (benchmarkDistPref flags)
    let verbosity = fromFlag $ benchmarkVerbosity flags
        flags' = flags { benchmarkDistPref = toFlag distPref }
    hookedActionWithArgs preBench benchHook postBench
            (getBuildConfig hooks verbosity distPref)
            hooks flags' args

registerAction :: UserHooks -> RegisterFlags -> Args -> IO ()
registerAction hooks flags args = do
    distPref <- findDistPrefOrDefault (regDistPref flags)
    let verbosity = fromFlag $ regVerbosity flags
        flags' = flags { regDistPref = toFlag distPref }
    hookedAction preReg regHook postReg
                 (getBuildConfig hooks verbosity distPref)
                 hooks flags' { regArgs = args } args

unregisterAction :: UserHooks -> RegisterFlags -> Args -> IO ()
unregisterAction hooks flags args = do
    distPref <- findDistPrefOrDefault (regDistPref flags)
    let verbosity = fromFlag $ regVerbosity flags
        flags' = flags { regDistPref = toFlag distPref }
    hookedAction preUnreg unregHook postUnreg
                 (getBuildConfig hooks verbosity distPref)
                 hooks flags' args

hookedAction :: (UserHooks -> Args -> flags -> IO HookedBuildInfo)
        -> (UserHooks -> PackageDescription -> LocalBuildInfo
                      -> UserHooks -> flags -> IO ())
        -> (UserHooks -> Args -> flags -> PackageDescription
                      -> LocalBuildInfo -> IO ())
        -> IO LocalBuildInfo
        -> UserHooks -> flags -> Args -> IO ()
hookedAction pre_hook cmd_hook =
    hookedActionWithArgs pre_hook (\h _ pd lbi uh flags ->
                                     cmd_hook h pd lbi uh flags)

hookedActionWithArgs :: (UserHooks -> Args -> flags -> IO HookedBuildInfo)
        -> (UserHooks -> Args -> PackageDescription -> LocalBuildInfo
                      -> UserHooks -> flags -> IO ())
        -> (UserHooks -> Args -> flags -> PackageDescription
                      -> LocalBuildInfo -> IO ())
        -> IO LocalBuildInfo
        -> UserHooks -> flags -> Args -> IO ()
hookedActionWithArgs pre_hook cmd_hook post_hook
  get_build_config hooks flags args = do
   pbi <- pre_hook hooks args flags
   lbi0 <- get_build_config
   let pkg_descr0 = localPkgDescr lbi0
   sanityCheckHookedBuildInfo pkg_descr0 pbi
   let pkg_descr = updatePackageDescription pbi pkg_descr0
       lbi = lbi0 { localPkgDescr = pkg_descr }
   cmd_hook hooks args pkg_descr lbi hooks flags
   post_hook hooks args flags pkg_descr lbi

sanityCheckHookedBuildInfo :: PackageDescription -> HookedBuildInfo -> IO ()
sanityCheckHookedBuildInfo PackageDescription { library = Nothing } (Just _,_)
    = die $ "The buildinfo contains info for a library, "
         ++ "but the package does not have a library."

sanityCheckHookedBuildInfo pkg_descr (_, hookExes)
    | not (null nonExistant)
    = die $ "The buildinfo contains info for an executable called '"
         ++ display (head nonExistant) ++ "' but the package does not have a "
         ++ "executable with that name."
  where
    pkgExeNames  = nub (map exeName (executables pkg_descr))
    hookExeNames = nub (map fst hookExes)
    nonExistant  = hookExeNames \\ pkgExeNames

sanityCheckHookedBuildInfo _ _ = return ()


getBuildConfig :: UserHooks -> Verbosity -> FilePath -> IO LocalBuildInfo
getBuildConfig hooks verbosity distPref = do
  lbi_wo_programs <- getPersistBuildConfig distPref
  -- Restore info about unconfigured programs, since it is not serialized
  let lbi = lbi_wo_programs {
    withPrograms = restoreProgramDb
                     (builtinPrograms ++ hookedPrograms hooks)
                     (withPrograms lbi_wo_programs)
  }

  case pkgDescrFile lbi of
    Nothing -> return lbi
    Just pkg_descr_file -> do
      outdated <- checkPersistBuildConfigOutdated distPref pkg_descr_file
      if outdated
        then reconfigure pkg_descr_file lbi
        else return lbi

  where
    reconfigure :: FilePath -> LocalBuildInfo -> IO LocalBuildInfo
    reconfigure pkg_descr_file lbi = do
      notice verbosity $ pkg_descr_file ++ " has been changed. "
                      ++ "Re-configuring with most recently used options. "
                      ++ "If this fails, please run configure manually.\n"
      let cFlags = configFlags lbi
      let cFlags' = cFlags {
            -- Since the list of unconfigured programs is not serialized,
            -- restore it to the same value as normally used at the beginning
            -- of a configure run:
            configPrograms_ = restoreProgramDb
                               (builtinPrograms ++ hookedPrograms hooks)
                               `fmap` configPrograms_ cFlags,

            -- Use the current, not saved verbosity level:
            configVerbosity = Flag verbosity
          }
      configureAction hooks cFlags' (extraConfigArgs lbi)


-- --------------------------------------------------------------------------
-- Cleaning

clean :: PackageDescription -> CleanFlags -> IO ()
clean pkg_descr flags = do
    let distPref = fromFlagOrDefault defaultDistPref $ cleanDistPref flags
    notice verbosity "cleaning..."

    maybeConfig <- if fromFlag (cleanSaveConf flags)
                     then maybeGetPersistBuildConfig distPref
                     else return Nothing

    -- remove the whole dist/ directory rather than tracking exactly what files
    -- we created in there.
    chattyTry "removing dist/" $ do
      exists <- doesDirectoryExist distPref
      when exists (removeDirectoryRecursive distPref)

    -- Any extra files the user wants to remove
    traverse_ removeFileOrDirectory (extraTmpFiles pkg_descr)

    -- If the user wanted to save the config, write it back
    traverse_ (writePersistBuildConfig distPref) maybeConfig

  where
        removeFileOrDirectory :: FilePath -> NoCallStackIO ()
        removeFileOrDirectory fname = do
            isDir <- doesDirectoryExist fname
            isFile <- doesFileExist fname
            if isDir then removeDirectoryRecursive fname
              else when isFile $ removeFile fname
        verbosity = fromFlag (cleanVerbosity flags)

-- --------------------------------------------------------------------------
-- Default hooks

-- | Hooks that correspond to a plain instantiation of the
-- \"simple\" build system
simpleUserHooks :: UserHooks
simpleUserHooks =
    emptyUserHooks {
       confHook  = configure,
       postConf  = finalChecks,
       buildHook = defaultBuildHook,
       replHook  = defaultReplHook,
       copyHook  = \desc lbi _ f -> install desc lbi f,
                   -- 'install' has correct 'copy' behavior with params
       testHook  = defaultTestHook,
       benchHook = defaultBenchHook,
       instHook  = defaultInstallHook,
       sDistHook = \p l h f -> sdist p l f srcPref (allSuffixHandlers h),
       cleanHook = \p _ _ f -> clean p f,
       hscolourHook = \p l h f -> hscolour p l (allSuffixHandlers h) f,
       haddockHook  = \p l h f -> haddock  p l (allSuffixHandlers h) f,
       doctestHook  = \p l h f -> doctest  p l (allSuffixHandlers h) f,
       regHook   = defaultRegHook,
       unregHook = \p l _ f -> unregister p l f
      }
  where
    finalChecks _args flags pkg_descr lbi =
      checkForeignDeps pkg_descr lbi (lessVerbose verbosity)
      where
        verbosity = fromFlag (configVerbosity flags)

-- | Basic autoconf 'UserHooks':
--
-- * 'postConf' runs @.\/configure@, if present.
--
-- * the pre-hooks 'preBuild', 'preClean', 'preCopy', 'preInst',
--   'preReg' and 'preUnreg' read additional build information from
--   /package/@.buildinfo@, if present.
--
-- Thus @configure@ can use local system information to generate
-- /package/@.buildinfo@ and possibly other files.

{-# DEPRECATED defaultUserHooks
     "Use simpleUserHooks or autoconfUserHooks, unless you need Cabal-1.2\n             compatibility in which case you must stick with defaultUserHooks" #-}
defaultUserHooks :: UserHooks
defaultUserHooks = autoconfUserHooks {
          confHook = \pkg flags -> do
                       let verbosity = fromFlag (configVerbosity flags)
                       warn verbosity
                         "defaultUserHooks in Setup script is deprecated."
                       confHook autoconfUserHooks pkg flags,
          postConf = oldCompatPostConf
    }
    -- This is the annoying old version that only runs configure if it exists.
    -- It's here for compatibility with existing Setup.hs scripts. See:
    -- https://github.com/haskell/cabal/issues/158
    where oldCompatPostConf args flags pkg_descr lbi
              = do let verbosity = fromFlag (configVerbosity flags)
                   confExists <- doesFileExist "configure"
                   when confExists $
                       runConfigureScript verbosity
                         backwardsCompatHack flags lbi

                   pbi <- getHookedBuildInfo verbosity
                   sanityCheckHookedBuildInfo pkg_descr pbi
                   let pkg_descr' = updatePackageDescription pbi pkg_descr
                       lbi' = lbi { localPkgDescr = pkg_descr' }
                   postConf simpleUserHooks args flags pkg_descr' lbi'

          backwardsCompatHack = True

autoconfUserHooks :: UserHooks
autoconfUserHooks
    = simpleUserHooks
      {
       postConf    = defaultPostConf,
       preBuild    = readHookWithArgs buildVerbosity,
       preCopy     = readHookWithArgs copyVerbosity,
       preClean    = readHook cleanVerbosity,
       preInst     = readHook installVerbosity,
       preHscolour = readHook hscolourVerbosity,
       preHaddock  = readHook haddockVerbosity,
       preReg      = readHook regVerbosity,
       preUnreg    = readHook regVerbosity
      }
    where defaultPostConf :: Args -> ConfigFlags -> PackageDescription
                          -> LocalBuildInfo -> IO ()
          defaultPostConf args flags pkg_descr lbi
              = do let verbosity = fromFlag (configVerbosity flags)
                   confExists <- doesFileExist "configure"
                   if confExists
                     then runConfigureScript verbosity
                            backwardsCompatHack flags lbi
                     else die "configure script not found."

                   pbi <- getHookedBuildInfo verbosity
                   sanityCheckHookedBuildInfo pkg_descr pbi
                   let pkg_descr' = updatePackageDescription pbi pkg_descr
                       lbi' = lbi { localPkgDescr = pkg_descr' }
                   postConf simpleUserHooks args flags pkg_descr' lbi'

          backwardsCompatHack = False

          readHookWithArgs :: (a -> Flag Verbosity) -> Args -> a
                           -> IO HookedBuildInfo
          readHookWithArgs get_verbosity _ flags = do
              getHookedBuildInfo verbosity
            where
              verbosity = fromFlag (get_verbosity flags)

          readHook :: (a -> Flag Verbosity) -> Args -> a -> IO HookedBuildInfo
          readHook get_verbosity a flags = do
              noExtraFlags a
              getHookedBuildInfo verbosity
            where
              verbosity = fromFlag (get_verbosity flags)

runConfigureScript :: Verbosity -> Bool -> ConfigFlags -> LocalBuildInfo
                   -> IO ()
runConfigureScript verbosity backwardsCompatHack flags lbi = do
  env <- getEnvironment
  let programDb = withPrograms lbi
  (ccProg, ccFlags) <- configureCCompiler verbosity programDb
  ccProgShort <- getShortPathName ccProg
  -- The C compiler's compilation and linker flags (e.g.
  -- "C compiler flags" and "Gcc Linker flags" from GHC) have already
  -- been merged into ccFlags, so we set both CFLAGS and LDFLAGS
  -- to ccFlags
  -- We don't try and tell configure which ld to use, as we don't have
  -- a way to pass its flags too
  let extraPath = fromNubList $ configProgramPathExtra flags
  let cflagsEnv = maybe (unwords ccFlags) (++ (" " ++ unwords ccFlags))
                  $ lookup "CFLAGS" env
      spSep = [searchPathSeparator]
      pathEnv = maybe (intercalate spSep extraPath)
                ((intercalate spSep extraPath ++ spSep)++) $ lookup "PATH" env
      overEnv = ("CFLAGS", Just cflagsEnv) :
                [("PATH", Just pathEnv) | not (null extraPath)]
      args' = args ++ ["CC=" ++ ccProgShort]
      shProg = simpleProgram "sh"
      progDb = modifyProgramSearchPath
               (\p -> map ProgramSearchPathDir extraPath ++ p) emptyProgramDb
  shConfiguredProg <- lookupProgram shProg
                      `fmap` configureProgram  verbosity shProg progDb
  case shConfiguredProg of
      Just sh -> runProgramInvocation verbosity
                 (programInvocation (sh {programOverrideEnv = overEnv}) args')
      Nothing -> die notFoundMsg

  where
    args = "./configure" : configureArgs backwardsCompatHack flags

    notFoundMsg = "The package has a './configure' script. "
               ++ "If you are on Windows, This requires a "
               ++ "Unix compatibility toolchain such as MinGW+MSYS or Cygwin. "
               ++ "If you are not on Windows, ensure that an 'sh' command "
               ++ "is discoverable in your path."

getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
getHookedBuildInfo verbosity = do
  maybe_infoFile <- defaultHookedPackageDesc
  case maybe_infoFile of
    Nothing       -> return emptyHookedBuildInfo
    Just infoFile -> do
      info verbosity $ "Reading parameters from " ++ infoFile
      readHookedBuildInfo verbosity infoFile

defaultTestHook :: Args -> PackageDescription -> LocalBuildInfo
                -> UserHooks -> TestFlags -> IO ()
defaultTestHook args pkg_descr localbuildinfo _ flags =
    test args pkg_descr localbuildinfo flags

defaultBenchHook :: Args -> PackageDescription -> LocalBuildInfo
                 -> UserHooks -> BenchmarkFlags -> IO ()
defaultBenchHook args pkg_descr localbuildinfo _ flags =
    bench args pkg_descr localbuildinfo flags

defaultInstallHook :: PackageDescription -> LocalBuildInfo
                   -> UserHooks -> InstallFlags -> IO ()
defaultInstallHook pkg_descr localbuildinfo _ flags = do
  let copyFlags = defaultCopyFlags {
                      copyDistPref   = installDistPref flags,
                      copyDest       = toFlag NoCopyDest,
                      copyVerbosity  = installVerbosity flags
                  }
  install pkg_descr localbuildinfo copyFlags
  let registerFlags = defaultRegisterFlags {
                          regDistPref  = installDistPref flags,
                          regInPlace   = installInPlace flags,
                          regPackageDB = installPackageDB flags,
                          regVerbosity = installVerbosity flags
                      }
  when (hasLibs pkg_descr) $ register pkg_descr localbuildinfo registerFlags

defaultBuildHook :: PackageDescription -> LocalBuildInfo
        -> UserHooks -> BuildFlags -> IO ()
defaultBuildHook pkg_descr localbuildinfo hooks flags =
  build pkg_descr localbuildinfo flags (allSuffixHandlers hooks)

defaultReplHook :: PackageDescription -> LocalBuildInfo
        -> UserHooks -> ReplFlags -> [String] -> IO ()
defaultReplHook pkg_descr localbuildinfo hooks flags args =
  repl pkg_descr localbuildinfo flags (allSuffixHandlers hooks) args

defaultRegHook :: PackageDescription -> LocalBuildInfo
        -> UserHooks -> RegisterFlags -> IO ()
defaultRegHook pkg_descr localbuildinfo _ flags =
    if hasLibs pkg_descr
    then register pkg_descr localbuildinfo flags
    else setupMessage (fromFlag (regVerbosity flags))
           "Package contains no library to register:" (packageId pkg_descr)
