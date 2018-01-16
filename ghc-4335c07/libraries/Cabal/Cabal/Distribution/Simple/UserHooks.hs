{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.UserHooks
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines the API that @Setup.hs@ scripts can use to customise the way
-- the build works. This module just defines the 'UserHooks' type. The
-- predefined sets of hooks that implement the @Simple@, @Make@ and @Configure@
-- build systems are defined in "Distribution.Simple". The 'UserHooks' is a big
-- record of functions. There are 3 for each action, a pre, post and the action
-- itself. There are few other miscellaneous hooks, ones to extend the set of
-- programs and preprocessors and one to override the function used to read the
-- @.cabal@ file.
--
-- This hooks type is widely agreed to not be the right solution. Partly this
-- is because changes to it usually break custom @Setup.hs@ files and yet many
-- internal code changes do require changes to the hooks. For example we cannot
-- pass any extra parameters to most of the functions that implement the
-- various phases because it would involve changing the types of the
-- corresponding hook. At some point it will have to be replaced.

module Distribution.Simple.UserHooks (
        UserHooks(..), Args,
        emptyUserHooks,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.PackageDescription
import Distribution.Simple.Program
import Distribution.Simple.Command
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo

type Args = [String]

-- | Hooks allow authors to add specific functionality before and after a
-- command is run, and also to specify additional preprocessors.
--
-- * WARNING: The hooks interface is under rather constant flux as we try to
-- understand users needs. Setup files that depend on this interface may
-- break in future releases.
data UserHooks = UserHooks {

    -- | Used for @.\/setup test@
    runTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO (),
    -- | Read the description file
    readDesc :: IO (Maybe GenericPackageDescription),
    -- | Custom preprocessors in addition to and overriding 'knownSuffixHandlers'.
    hookedPreProcessors :: [ PPSuffixHandler ],
    -- | These programs are detected at configure time.  Arguments for them are
    -- added to the configure command.
    hookedPrograms :: [Program],

    -- |Hook to run before configure command
    preConf  :: Args -> ConfigFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during configure.
    confHook :: (GenericPackageDescription, HookedBuildInfo)
            -> ConfigFlags -> IO LocalBuildInfo,
    -- |Hook to run after configure command
    postConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before build command.  Second arg indicates verbosity level.
    preBuild  :: Args -> BuildFlags -> IO HookedBuildInfo,

    -- |Over-ride this hook to get different behavior during build.
    buildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO (),
    -- |Hook to run after build command.  Second arg indicates verbosity level.
    postBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before repl command.  Second arg indicates verbosity level.
    preRepl  :: Args -> ReplFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during interpretation.
    replHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> ReplFlags -> [String] -> IO (),
    -- |Hook to run after repl command.  Second arg indicates verbosity level.
    postRepl :: Args -> ReplFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before clean command.  Second arg indicates verbosity level.
    preClean  :: Args -> CleanFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during clean.
    cleanHook :: PackageDescription -> () -> UserHooks -> CleanFlags -> IO (),
    -- |Hook to run after clean command.  Second arg indicates verbosity level.
    postClean :: Args -> CleanFlags -> PackageDescription -> () -> IO (),

    -- |Hook to run before copy command
    preCopy  :: Args -> CopyFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during copy.
    copyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO (),
    -- |Hook to run after copy command
    postCopy :: Args -> CopyFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before install command
    preInst  :: Args -> InstallFlags -> IO HookedBuildInfo,

    -- |Over-ride this hook to get different behavior during install.
    instHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO (),
    -- |Hook to run after install command.  postInst should be run
    -- on the target, not on the build machine.
    postInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before sdist command.  Second arg indicates verbosity level.
    preSDist  :: Args -> SDistFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during sdist.
    sDistHook :: PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> SDistFlags -> IO (),
    -- |Hook to run after sdist command.  Second arg indicates verbosity level.
    postSDist :: Args -> SDistFlags -> PackageDescription -> Maybe LocalBuildInfo -> IO (),

    -- |Hook to run before register command
    preReg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during registration.
    regHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO (),
    -- |Hook to run after register command
    postReg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before unregister command
    preUnreg  :: Args -> RegisterFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during unregistration.
    unregHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO (),
    -- |Hook to run after unregister command
    postUnreg :: Args -> RegisterFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before hscolour command.  Second arg indicates verbosity level.
    preHscolour  :: Args -> HscolourFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during hscolour.
    hscolourHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HscolourFlags -> IO (),
    -- |Hook to run after hscolour command.  Second arg indicates verbosity level.
    postHscolour :: Args -> HscolourFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before doctest command.  Second arg indicates verbosity level.
    preDoctest  :: Args -> DoctestFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during doctest.
    doctestHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> DoctestFlags -> IO (),
    -- |Hook to run after doctest command.  Second arg indicates verbosity level.
    postDoctest :: Args -> DoctestFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before haddock command.  Second arg indicates verbosity level.
    preHaddock  :: Args -> HaddockFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during haddock.
    haddockHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> HaddockFlags -> IO (),
    -- |Hook to run after haddock command.  Second arg indicates verbosity level.
    postHaddock :: Args -> HaddockFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before test command.
    preTest :: Args -> TestFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during test.
    testHook :: Args -> PackageDescription -> LocalBuildInfo -> UserHooks -> TestFlags -> IO (),
    -- |Hook to run after test command.
    postTest :: Args -> TestFlags -> PackageDescription -> LocalBuildInfo -> IO (),

    -- |Hook to run before bench command.
    preBench :: Args -> BenchmarkFlags -> IO HookedBuildInfo,
    -- |Over-ride this hook to get different behavior during bench.
    benchHook :: Args -> PackageDescription -> LocalBuildInfo -> UserHooks -> BenchmarkFlags -> IO (),
    -- |Hook to run after bench command.
    postBench :: Args -> BenchmarkFlags -> PackageDescription -> LocalBuildInfo -> IO ()
  }

{-# DEPRECATED runTests "Please use the new testing interface instead!" #-}

-- |Empty 'UserHooks' which do nothing.
emptyUserHooks :: UserHooks
emptyUserHooks
  = UserHooks {
      runTests  = ru,
      readDesc  = return Nothing,
      hookedPreProcessors = [],
      hookedPrograms      = [],
      preConf   = rn',
      confHook  = (\_ _ -> return (error "No local build info generated during configure. Over-ride empty configure hook.")),
      postConf  = ru,
      preBuild  = rn',
      buildHook = ru,
      postBuild = ru,
      preRepl   = \_ _ -> return emptyHookedBuildInfo,
      replHook  = \_ _ _ _ _ -> return (),
      postRepl  = ru,
      preClean  = rn,
      cleanHook = ru,
      postClean = ru,
      preCopy   = rn',
      copyHook  = ru,
      postCopy  = ru,
      preInst   = rn,
      instHook  = ru,
      postInst  = ru,
      preSDist  = rn,
      sDistHook = ru,
      postSDist = ru,
      preReg    = rn',
      regHook   = ru,
      postReg   = ru,
      preUnreg  = rn,
      unregHook = ru,
      postUnreg = ru,
      preHscolour  = rn,
      hscolourHook = ru,
      postHscolour = ru,
      preDoctest   = rn,
      doctestHook  = ru,
      postDoctest  = ru,
      preHaddock   = rn,
      haddockHook  = ru,
      postHaddock  = ru,
      preTest  = rn',
      testHook = \_ -> ru,
      postTest = ru,
      preBench = rn',
      benchHook = \_ -> ru,
      postBench = ru
    }
    where rn  args _ = noExtraFlags args >> return emptyHookedBuildInfo
          rn' _    _ = return emptyHookedBuildInfo
          ru _ _ _ _ = return ()
