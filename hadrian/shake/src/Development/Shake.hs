{-# LANGUAGE TypeFamilies, TypeOperators, ConstraintKinds, PatternSynonyms #-}

-- | This module is used for defining Shake build systems. As a simple example of a Shake build system,
--   let us build the file @result.tar@ from the files listed by @result.txt@:
--
-- @
-- import "Development.Shake"
-- import "Development.Shake.FilePath"
--
-- main = 'shakeArgs' 'shakeOptions' $ do
--     'want' [\"result.tar\"]
--     \"*.tar\" '%>' \\out -> do
--         contents \<- 'readFileLines' $ out 'Development.Shake.FilePath.-<.>' \"txt\"
--         'need' contents
--         'cmd' \"tar -cf\" [out] contents
-- @
--
--   We start by importing the modules defining both Shake and routines for manipulating 'FilePath' values.
--   We define @main@ to call 'shake' with the default 'shakeOptions'. As the second argument to
--   'shake', we provide a set of rules. There are two common forms of rules, 'want' to specify target files,
--   and '%>' to define a rule which builds a 'FilePattern'. We use 'want' to require that after the build
--   completes the file @result.tar@ should be ready.
--
--   The @*.tar@ rule describes how to build files with the extension @.tar@, including @result.tar@.
--   We 'readFileLines' on @result.txt@, after changing the @.tar@ extension to @.txt@. We read each line
--   into the variable @contents@ -- being a list of the files that should go into @result.tar@. Next, we
--   depend ('need') all the files in @contents@. If any of these files change, the rule will be repeated.
--   Finally we call the @tar@ program. If either @result.txt@ changes, or any of the files listed by @result.txt@
--   change, then @result.tar@ will be rebuilt.
--
--   To find out more:
--
-- * The user manual contains a longer example and background information on how to use Shake
--   <https://www.shakebuild.com/manual>.
--
-- * The home page has links to additional information <https://www.shakebuild.com/>, including
--   a mailing list.
--
-- * The theory behind Shake is described in an ICFP 2012 paper,
--   <https://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf Shake Before Building -- Replacing Make with Haskell>.
--   The <https://www.youtube.com/watch?v=xYCPpXVlqFM associated talk> forms a short overview of Shake.
module Development.Shake(
    -- * Writing a build system
    -- $writing

    -- * GHC build flags
    -- $flags

    -- * Other Shake modules
    -- $modules

    -- * Core
    shake,
    shakeOptions,
    Rules, action, withoutActions, alternatives, priority, versioned,
    Action, traced,
    liftIO, actionOnException, actionFinally, actionBracket, actionCatch, actionRetry, runAfter,
    ShakeException(..),
    -- * Configuration
    ShakeOptions(..), ShakeShuffle(..), Rebuild(..), Lint(..), Change(..),
    getShakeOptions, getShakeOptionsRules, getHashedShakeVersion,
    getShakeExtra, getShakeExtraRules, addShakeExtra,
    -- ** Command line
    shakeArgs, shakeArgsWith, shakeArgsOptionsWith, shakeOptDescrs, addHelpSuffix,
    -- ** Targets
    getTargets, addTarget, withTargetDocs, withoutTargets,
    -- ** Progress reporting
    Progress(..), progressSimple, progressDisplay, progressTitlebar, progressProgram, getProgress,
    -- ** Verbosity
    Verbosity(..), getVerbosity, putVerbose, putInfo, putWarn, putError, withVerbosity, quietly,
    -- * Running commands
    command, command_, cmd, cmd_, unit,
    Stdout(..), StdoutTrim(..), Stderr(..), Stdouterr(..), Exit(..), Process(..), CmdTime(..), CmdLine(..), FSATrace(..),
    CmdResult, CmdString, CmdOption(..),
    addPath, addEnv,
    -- * Explicit parallelism
    parallel, forP, par,
    -- * Utility functions
    copyFile', copyFileChanged,
    readFile', readFileLines,
    writeFile', writeFileLines, writeFileChanged,
    removeFiles, removeFilesAfter,
    withTempFile, withTempDir,
    withTempFileWithin, withTempDirWithin,
    -- * File rules
    need, want, (%>), (|%>), (?>), phony, (~>), phonys,
    (&%>), (&?>),
    orderOnly, orderOnlyAction,
    FilePattern, (?==), (<//>), filePattern,
    needed, trackRead, trackWrite, trackAllow,
    -- * Directory rules
    doesFileExist, doesDirectoryExist, getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    getDirectoryFilesIO,
    -- * Environment rules
    getEnv, getEnvWithDefault, getEnvError,
    -- * Oracle rules
    ShakeValue, RuleResult, addOracle, addOracleCache, addOracleHash, askOracle, askOracles,
    -- * Special rules
    alwaysRerun,
    -- * Resources
    Resource, newResource, newResourceIO, withResource, withResources,
    newThrottle, newThrottleIO,
    unsafeExtraThread,
    -- * Cache
    newCache, newCacheIO,
    historyDisable, produces,
    -- * Batching
    needHasChanged,
    resultHasChanged,
    batch,
    reschedule,
    -- * Deprecated
    askOracleWith,
    deprioritize,
    pattern Quiet, pattern Normal, pattern Loud, pattern Chatty,
    putLoud, putNormal, putQuiet
    ) where

-- I would love to use module export in the above export list, but alas Haddock
-- then shows all the things that are hidden in the docs, which is terrible.
import Control.Monad.IO.Class
import Development.Shake.Internal.Value
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Resource
import Development.Shake.Internal.Derived
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Progress
import Development.Shake.Internal.Args

import Development.Shake.Command
import Development.Shake.Internal.FilePattern
import Development.Shake.Internal.Rules.Directory
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.Rules.Files
import Development.Shake.Internal.Rules.Oracle
import Development.Shake.Internal.Rules.OrderOnly
import Development.Shake.Internal.Rules.Rerun

-- $writing
--
--   When writing a Shake build system, start by defining what you 'want', then write rules
--   with '%>' to produce the results. Before calling 'cmd' you should ensure that any files the command
--   requires are demanded with calls to 'need'. We offer the following advice to Shake users:
--
-- * If @ghc --make@ or @cabal@ is capable of building your project, use that instead. Custom build systems are
--   necessary for many complex projects, but many projects are not complex.
--
-- * The 'shakeArgs' function automatically handles command line arguments. To define non-file targets use 'phony'.
--
-- * Put all result files in a distinguished directory, for example @_make@. You can implement a @clean@
--   command by removing that directory, using @'removeFilesAfter' \"_make\" [\"\/\/\*\"]@.
--
-- * To obtain parallel builds set 'shakeThreads' to a number greater than 1.
--
-- * Lots of compilers produce @.o@ files. To avoid overlapping rules, use @.c.o@ for C compilers,
--   @.hs.o@ for Haskell compilers etc.
--
-- * Do not be afraid to mix Shake rules, system commands and other Haskell libraries -- use each for what
--   it does best.
--
-- * The more accurate the dependencies are, the better. Use additional rules like 'doesFileExist' and
--   'getDirectoryFiles' to track information other than just the contents of files. For information in the environment
--   that you suspect will change regularly (perhaps @ghc@ version number), either write the information to
--   a file with 'alwaysRerun' and 'writeFileChanged', or use 'addOracle'.

-- $flags
--
--   For large build systems the choice of GHC flags can have a significant impact. We recommend:
--
-- > ghc --make MyBuildSystem -threaded -rtsopts "-with-rtsopts=-I0 -qg"
--
--   * @-rtsopts@: Allow the setting of further GHC options at runtime.
--
--   * @-I0@: Disable idle garbage collection, to avoid frequent unnecessary garbage collection, see
--     <https://stackoverflow.com/questions/34588057/why-does-shake-recommend-disabling-idle-garbage-collection/ a full explanation>.
--
--   * You may add @-threaded@, and pass the options @-qg@ to @-with-rtsopts@
--     to disable parallel garbage collection. Parallel garbage collection in Shake
--     programs typically goes slower than sequential garbage collection, while occupying many cores that
--     could be used for running system commands.

-- $modules
--
--   The main Shake module is this one, "Development.Shake", which should be sufficient for most
--   people writing build systems using Shake. However, Shake provides some additional modules,
--
-- * "Development.Shake.Classes" provides convenience exports of the classes Shake relies on,
--   in particular 'Binary', 'Hashable' and 'NFData'. Useful for deriving these types using
--   @GeneralizedNewtypeDeriving@ without adding dependencies on the associated packages.
--
-- * "Development.Shake.Command" provides the command line wrappers. These are reexported by
--   "Development.Shake", but if you want to reuse just the command-line running functionality
--   in a non-Shake program you can import just that.
--
-- * "Development.Shake.Config" provides a way to write configuration files that are tracked.
--   The configuration files are in the Ninja format. Useful for users of bigger systems who
--   want to track the build rules not in Haskell.
--
-- * "Development.Shake.Database" provides lower level primitives to drive Shake, particularly
--   useful if you want to run multiple Shake runs in a row without reloading from the database.
--
-- * "Development.Shake.FilePath" is an extension of "System.FilePath" with a few additional
--   methods and safer extension manipulation code.
--
-- * "Development.Shake.Forward" is an alternative take on build systems, where you write the
--   rules as a script where steps are skipped, rather than as a set of dependencies. Only really
--   works if you use @fsatrace@.
--
-- * "Development.Shake.Rule" provides tools for writing your own types of Shake rules. Useful
--   if you need something new, like a rule that queries a database or similar.
--
-- * "Development.Shake.Util" has general utilities that are useful for build systems, e.g.
--   reading @Makefile@ syntax and alternative forms of argument parsing.


---------------------------------------------------------------------
-- DEPRECATED SINCE 0.16.1, NOV 2017

-- | /Deprecated:/ Replace @'askOracleWith' q a@ by @'askOracle' q@
--   since the 'RuleResult' type family now fixes the result type.
{-# DEPRECATED askOracleWith "Use 'askOracle q' instead of 'askOracleWith q a', the result value is now unnecessary" #-}
askOracleWith :: (RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> a -> Action a
askOracleWith question _ = askOracle question

---------------------------------------------------------------------
-- DEPRECATED SINCE 0.18.4, JUL 2019

-- | /Deprecated:/ Alias for 'reschedule'.
{-# DEPRECATED deprioritize "Use 'reschedule' instead" #-}
deprioritize :: Double -> Action ()
deprioritize = reschedule

-- | /Deprecated:/ A bidirectional pattern synonym for 'Error'.
pattern Quiet :: Verbosity
pattern Quiet  = Error
-- | /Deprecated:/ A bidirectional pattern synonym for 'Info'.
pattern Normal :: Verbosity
pattern Normal = Info
-- | /Deprecated:/ A bidirectional pattern synonym for 'Verbose'.
pattern Loud :: Verbosity
pattern Loud   = Verbose
-- | /Deprecated:/ A bidirectional pattern synonym for 'Verbose'.
pattern Chatty :: Verbosity
pattern Chatty = Verbose

putLoud, putNormal, putQuiet :: String -> Action ()
-- | /Deprecated:/ Alias for 'putVerbose'.
putLoud = putVerbose
-- | /Deprecated:/ Alias for 'putInfo'.
putNormal = putInfo
-- | /Deprecated:/ Alias for 'putError'.
putQuiet = putError
