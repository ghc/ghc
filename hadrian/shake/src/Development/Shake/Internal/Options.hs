{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}

-- | Types exposed to the user
module Development.Shake.Internal.Options(
    Progress(..), Verbosity(..), Rebuild(..), Lint(..), Change(..), ShakeShuffle(..),
    ShakeOptions(..), shakeOptions,
    -- Internal stuff
    shakeRebuildApply, shakeAbbreviationsApply, shakeOptionsFields,
    shakeOptionsResolve, shakeShuffleList
    ) where

import Data.Data
import Data.List.Extra
import Data.Tuple.Extra
import Data.Maybe
import Data.Dynamic
import Control.Monad
import General.Extra
import System.Time.Extra
import qualified Data.HashMap.Strict as Map
import Development.Shake.Internal.FilePattern
import Development.Shake.Internal.Errors
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import Development.Shake.Internal.CmdOption
import Data.Semigroup
import Prelude
import System.Random


-- | The current assumptions made by the build system, used by 'shakeRebuild'. These options
--   allow the end user to specify that any rules run are either to be treated as clean, or as
--   dirty, regardless of what the build system thinks.
--
--   These assumptions only operate on files reached by the current 'Development.Shake.action' commands. Any
--   other files in the database are left unchanged.
data Rebuild
    = RebuildNow
        -- ^ Assume these files are dirty and require rebuilding.
        --   for benchmarking rebuild speed and for rebuilding if untracked dependencies have changed.
        --   This flag is safe, but may cause more rebuilding than necessary.
    | RebuildNormal
        -- ^ Useful to reset the rebuild status to how it was before, equivalent to passing no 'Rebuild' flags.
    | RebuildLater
        -- ^ /This assumption is unsafe, and may lead to incorrect build results in this run/.
        --   Assume these files are clean in this run, but test them normally in future runs.
{-
    | RebuildNever
        -- Add to RebuildNow: Useful to undo the results of 'RebuildNever',
        -- ^ /This assumption is unsafe, and may lead to incorrect build results in this run, and in future runs/.
        --   Assume and record that these files are clean and do not require rebuilding, provided the file
        --   has been built before. Useful if you have modified a file in some
        --   inconsequential way, such as only the comments or whitespace, and wish to avoid a rebuild.
-}
      deriving (Eq,Ord,Show,Read,Typeable,Data,Enum,Bounded)


-- | Which lint checks to perform, used by 'shakeLint'.
data Lint
    = LintBasic
        -- ^ The most basic form of linting. Checks that the current directory does not change and that results do not change after they
        --   are first written. Any calls to 'needed' will assert that they do not cause a rule to be rebuilt.
    | LintFSATrace
        -- ^ Track which files are accessed by command line programs
        -- using <https://github.com/jacereda/fsatrace fsatrace>.
      deriving (Eq,Ord,Show,Read,Typeable,Data,Enum,Bounded)


-- | How should you determine if a file has changed, used by 'shakeChange'. The most common values are
--   'ChangeModtime' (the default, very fast, @touch@ causes files to rebuild) and 'ChangeModtimeAndDigestInput'
--   (slightly slower, @touch@ and switching @git@ branches does not cause input files to rebuild).
data Change
    = ChangeModtime
        -- ^ Compare equality of modification timestamps, a file has changed if its last modified time changes.
        --   A @touch@ will force a rebuild. This mode is fast and usually sufficiently accurate, so is the default.
    | ChangeDigest
        -- ^ Compare equality of file contents digests, a file has changed if its digest changes.
        --   A @touch@ will not force a rebuild. Use this mode if modification times on your file system are unreliable.
    | ChangeModtimeAndDigest
        -- ^ A file is rebuilt if both its modification time and digest have changed. For efficiency reasons, the modification
        --   time is checked first, and if that has changed, the digest is checked.
    | ChangeModtimeAndDigestInput
        -- ^ Use 'ChangeModtimeAndDigest' for input\/source files and 'ChangeModtime' for output files.
        --   An input file is one which is a dependency but is not built by Shake as it has no
        --   matching rule and already exists on the file system.
    | ChangeModtimeOrDigest
        -- ^ A file is rebuilt if either its modification time or its digest has changed. A @touch@ will force a rebuild,
        --   but even if a files modification time is reset afterwards, changes will also cause a rebuild.
      deriving (Eq,Ord,Show,Read,Typeable,Data,Enum,Bounded)


-- | How to perturb the order in which independent build work is scheduled.
--   Used by 'shakeShuffle'. The order of returned values and recorded
--   dependencies is not changed.
data ShakeShuffle
    = ShuffleNone
        -- ^ Do not perturb scheduling. This is the default.
    | ShuffleReverse
        -- ^ Reverse Shake's usual scheduling order.
    | ShuffleRandom (Maybe Int)
        -- ^ Randomise scheduling. 'Nothing' chooses and reports a fresh seed;
        --   'Just' supplies a seed which can be used to replay the ordering.
      deriving (Eq,Ord,Show,Read,Typeable,Data)


-- | Information about the current state of the build, obtained by either passing a callback function
--   to 'Development.Shake.shakeProgress' (asynchronous output) or 'Development.Shake.getProgress'
--   (synchronous output). Typically a build system will pass 'progressDisplay' to 'Development.Shake.shakeProgress',
--   which will poll this value and produce status messages.
data Progress = Progress
-- In retrospect shakeProgress should have been done differently, as a feature you turn on in Rules
-- but easiest way around that for now is put the Progress type in Options

    {isFailure :: !(Maybe String) -- ^ Starts out 'Nothing', becomes 'Just' a target name if a rule fails.
    ,countSkipped :: {-# UNPACK #-} !Int -- ^ Number of rules which were required, but were already in a valid state.
    ,countBuilt :: {-# UNPACK #-} !Int -- ^ Number of rules which were have been built in this run.
    ,countUnknown :: {-# UNPACK #-} !Int -- ^ Number of rules which have been built previously, but are not yet known to be required.
    ,countTodo :: {-# UNPACK #-} !Int -- ^ Number of rules which are currently required (ignoring dependencies that do not change), but not built.
    ,timeSkipped :: {-# UNPACK #-} !Double -- ^ Time spent building 'countSkipped' rules in previous runs.
    ,timeBuilt :: {-# UNPACK #-} !Double -- ^ Time spent building 'countBuilt' rules.
    ,timeUnknown :: {-# UNPACK #-} !Double -- ^ Time spent building 'countUnknown' rules in previous runs.
    ,timeTodo :: {-# UNPACK #-} !(Double,Int) -- ^ Time spent building 'countTodo' rules in previous runs, plus the number which have no known time (have never been built before).
    }
    deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Semigroup Progress where
    a <> b = Progress
        {isFailure = isFailure a `mplus` isFailure b
        ,countSkipped = countSkipped a + countSkipped b
        ,countBuilt = countBuilt a + countBuilt b
        ,countUnknown = countUnknown a + countUnknown b
        ,countTodo = countTodo a + countTodo b
        ,timeSkipped = timeSkipped a + timeSkipped b
        ,timeBuilt = timeBuilt a + timeBuilt b
        ,timeUnknown = timeUnknown a + timeUnknown b
        ,timeTodo = let (a1,a2) = timeTodo a; (b1,b2) = timeTodo b
                        x1 = a1 + b1; x2 = a2 + b2
                    in x1 `seq` x2 `seq` (x1,x2)
        }

instance Monoid Progress where
    mempty = Progress Nothing 0 0 0 0 0 0 0 (0,0)
    mappend = (<>)


-- | Options to control the execution of Shake, usually specified by overriding fields in
--   'shakeOptions':
--
--   @ 'shakeOptions'{'shakeThreads'=4, 'shakeReport'=[\"report.html\"]} @
--
--   The 'Data' instance for this type reports the 'shakeProgress' and 'shakeOutput' fields as having the abstract type 'Hidden',
--   because 'Data' cannot be defined for functions or 'TypeRep's.
data ShakeOptions = ShakeOptions
    {shakeFiles :: FilePath
        -- ^ Defaults to @.shake@. The directory used for storing Shake metadata files.
        --   All metadata files will be named @'shakeFiles'\/.shake./file-name/@, for some @/file-name/@.
        --   If the 'shakeFiles' directory does not exist it will be created.
        --   If set to @\"\/dev\/null\"@ then no shakeFiles are read or written (even on Windows).
    ,shakeThreads :: Int
        -- ^ Defaults to @1@. Maximum number of rules to run in parallel, similar to @make --jobs=/N/@.
        --   For many build systems, a number equal to or slightly less than the number of physical processors
        --   works well. Use @0@ to match the detected number of processors (when @0@, 'getShakeOptions' will
        --   return the number of threads used).
    ,shakeVersion :: String
        -- ^ Defaults to @"1"@. The version number of your build rules.
        --   Change the version number to force a complete rebuild, such as when making
        --   significant changes to the rules that require a wipe. The version number should be
        --   set in the source code, and not passed on the command line.
    ,shakeVerbosity :: Verbosity
        -- ^ Defaults to 'Info'. What level of messages should be printed out.
    ,shakeStaunch :: Bool
        -- ^ Defaults to 'False'. Operate in staunch mode, where building continues even after errors,
        --   similar to @make --keep-going@.
    ,shakeReport :: [FilePath]
        -- ^ Defaults to @[]@. Write a profiling report to a file, showing which rules rebuilt,
        --   why, and how much time they took. Useful for improving the speed of your build systems.
        --   If the file extension is @.json@ it will write JSON data; if @.js@ it will write Javascript;
        --   if @.trace@ it will write trace events (load into @about:\/\/tracing@ in Chrome);
        --   otherwise it will write HTML.
    ,shakeLint :: Maybe Lint
        -- ^ Defaults to 'Nothing'. Perform sanity checks during building, see 'Lint' for details.
    ,shakeLintInside :: [FilePath]
        -- ^ Directories in which the files will be tracked by the linter.
    ,shakeLintIgnore :: [FilePattern]
        -- ^ File patterns which are ignored from linter tracking, a bit like calling 'Development.Shake.trackAllow' in every rule.
    ,shakeLintWatch :: [FilePattern]
        -- ^ File patterns whose modification causes an error. Raises an error even if 'shakeLint' is 'Nothing'.
    ,shakeCommandOptions :: [CmdOption]
        -- ^ Defaults to @[]@. Additional options to be passed to all command invocations.
    ,shakeFlush :: Maybe Seconds
        -- ^ Defaults to @'Just' 10@. How often to flush Shake metadata files in seconds, or 'Nothing' to never flush explicitly.
        --   It is possible that on abnormal termination (not Haskell exceptions) any rules that completed in the last
        --   'shakeFlush' seconds will be lost.
    ,shakeRebuild :: [(Rebuild, FilePattern)]
        -- ^ What to rebuild
    ,shakeAbbreviations :: [(String,String)]
        -- ^ Defaults to @[]@. A list of substrings that should be abbreviated in status messages, and their corresponding abbreviation.
        --   Commonly used to replace the long paths (e.g. @.make\/i586-linux-gcc\/output@) with an abbreviation (e.g. @$OUT@).
    ,shakeStorageLog :: Bool
        -- ^ Defaults to 'False'. Write a message to @'shakeFiles'\/.shake.storage.log@ whenever a storage event happens which may impact
        --   on the current stored progress. Examples include database version number changes, database compaction or corrupt files.
    ,shakeLineBuffering :: Bool
        -- ^ Defaults to 'True'. Change 'stdout' and 'stderr' to line buffering while running Shake.
    ,shakeTimings :: Bool
        -- ^ Defaults to 'False'. Print timing information for each stage at the end.
    ,shakeRunCommands :: Bool
        -- ^ Default to 'True'. Should you run command line actions, set to 'False' to skip actions whose output streams and exit code
        --   are not used. Useful for profiling the non-command portion of the build system.
    ,shakeChange :: Change
        -- ^ Default to 'ChangeModtime'. How to check if a file has changed, see 'Change' for details.
    ,shakeCreationCheck :: Bool
        -- ^ Default to 'True'. After running a rule to create a file, is it an error if the file does not exist.
        --   Provided for compatibility with @make@ and @ninja@ (which have ugly file creation semantics).
    ,shakeLiveFiles :: [FilePath]
        -- ^ Default to @[]@. After the build system completes, write a list of all files which were /live/ in that run,
        --   i.e. those which Shake checked were valid or rebuilt. Produces best answers if nothing rebuilds.
    ,shakeVersionIgnore :: Bool
        -- ^ Defaults to 'False'. Ignore any differences in 'shakeVersion'.
    ,shakeColor :: Bool
        -- ^ Defaults to 'False'. Whether to colorize the output.
    ,shakeShare :: Maybe FilePath
        -- ^ Defaults to 'Nothing'. Whether to use and store outputs in a shared directory.
    ,shakeCloud :: [String]
        -- ^ Defaults to @[]@. Cloud servers to talk to forming a shared cache.
    ,shakeSymlink :: Bool
        -- ^ Defaults to @False@. Use symlinks for 'shakeShare' if they are available.
        --   If this setting is @True@ (even if symlinks are not available) then files will be
        --   made read-only to avoid inadvertantly poisoning the shared cache.
        --   Note the links are actually hard links, not symlinks.
    ,shakeNeedDirectory :: Bool
        -- ^ Defaults to @False@. Is depending on a directory an error (default), or it is permitted with
        --   undefined results. Provided for compatibility with @ninja@.
    ,shakeAllowRedefineRules :: Bool
        -- ^ Whether to allow calling addBuiltinRule for the same key more than once
    ,shakeShuffle :: ShakeShuffle
        -- ^ Defaults to 'ShuffleNone'. Perturb the scheduling order of independent
        --   dependencies to help find missing dependency declarations.
    ,shakeProgress :: IO Progress -> IO ()
        -- ^ Defaults to no action. A function called when the build starts, allowing progress to be reported.
        --   The function is called on a separate thread, and that thread is killed when the build completes.
        --   For applications that want to display progress messages, 'progressSimple' is often sufficient, but more advanced
        --   users should look at the 'Progress' data type.
    ,shakeOutput :: Verbosity -> String -> IO ()
        -- ^ Defaults to writing using 'putStrLn'. A function called to output messages from Shake, along with the 'Verbosity' at
        --   which that message should be printed. This function will be called atomically from all other 'shakeOutput' functions.
        --   The 'Verbosity' will always be greater than or higher than 'shakeVerbosity'.
    ,shakeTrace :: String -> String -> Bool -> IO ()
        -- ^ Defaults to doing nothing.
        --   Called for each call of 'Development.Shake.traced', with the key, the command and 'True' for starting, 'False' for stopping.
    ,shakeExtra :: Map.HashMap TypeRep Dynamic
        -- ^ This a map which can be used to store arbitrary extra information that a user may need when writing rules.
        --   The key of each entry must be the 'dynTypeRep' of the value.
        --   Insert values using 'addShakeExtra' and retrieve them using 'getShakeExtra'.
        --   The correct way to use this field is to define a hidden newtype for the key, so that conflicts cannot occur.
    }
    deriving Typeable

-- | The default set of 'ShakeOptions'.
shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions
    ".shake" 1 "1" Info False [] Nothing [] [] [] [] (Just 10) [] [] False True False
    True ChangeModtime True [] False False Nothing [] False False False ShuffleNone
    (const $ pure ())
    (const $ BS.putStrLn . UTF8.fromString) -- try and output atomically using BS
    (\_ _ _ -> pure ())
    Map.empty

-- | Resolve an automatically selected shuffle seed. Kept internal so every
--   entry point resolves the seed before constructing a build 'Global'.
shakeOptionsResolve :: ShakeOptions -> IO ShakeOptions
shakeOptionsResolve opts@ShakeOptions{shakeShuffle=ShuffleRandom Nothing} = do
    seed <- randomRIO (0, maxBound)
    when (shakeVerbosity opts >= Info) $
        shakeOutput opts Info $ "Shake shuffle seed: " ++ show seed ++
            " (use --shuffle=" ++ show seed ++ " to replay)"
    pure opts{shakeShuffle=ShuffleRandom $ Just seed}
shakeOptionsResolve opts = pure opts

-- | Perturb a list for scheduling. The salt distinguishes independent groups
--   while keeping a fixed seed replayable.
shakeShuffleList :: ShakeShuffle -> Int -> [a] -> [a]
shakeShuffleList ShuffleNone _ = id
shakeShuffleList ShuffleReverse _ = reverse
shakeShuffleList (ShuffleRandom seed) salt =
    map snd . sortOn fst . zip (randoms (mkStdGen $ fromMaybe 0 seed + salt) :: [Int])

fieldsShakeOptions =
    ["shakeFiles", "shakeThreads", "shakeVersion", "shakeVerbosity", "shakeStaunch", "shakeReport"
    ,"shakeLint", "shakeLintInside", "shakeLintIgnore", "shakeLintWatch", "shakeCommandOptions"
    ,"shakeFlush", "shakeRebuild", "shakeAbbreviations", "shakeStorageLog"
    ,"shakeLineBuffering", "shakeTimings", "shakeRunCommands", "shakeChange", "shakeCreationCheck"
    ,"shakeLiveFiles", "shakeVersionIgnore", "shakeColor", "shakeShare", "shakeCloud", "shakeSymlink"
    ,"shakeNeedDirectory", "shakeCanRedefineRules", "shakeShuffle"
    ,"shakeProgress", "shakeOutput", "shakeTrace", "shakeExtra"]
tyShakeOptions = mkDataType "Development.Shake.Types.ShakeOptions" [conShakeOptions]
conShakeOptions = mkConstr tyShakeOptions "ShakeOptions" fieldsShakeOptions Prefix
unhide x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 y1 y2 y3 y4 =
  ShakeOptions x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29
        (fromHidden y1) (fromHidden y2) (fromHidden y3) (fromHidden y4)

instance Data ShakeOptions where
    gfoldl k z (ShakeOptions x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 y1 y2 y3 y4) =
        z unhide `k` x1 `k` x2 `k` x3 `k` x4 `k` x5 `k` x6 `k` x7 `k` x8 `k` x9 `k` x10 `k` x11 `k`
        x12 `k` x13 `k` x14 `k` x15 `k` x16 `k` x17 `k` x18 `k` x19 `k` x20 `k` x21 `k` x22 `k` x23 `k` x24 `k` x25 `k` x26 `k` x27 `k` x28 `k` x29 `k`
        Hidden y1 `k` Hidden y2 `k` Hidden y3 `k` Hidden y4
    gunfold k z _ = k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ z unhide
    toConstr ShakeOptions{} = conShakeOptions
    dataTypeOf _ = tyShakeOptions

shakeOptionsFields :: ShakeOptions -> [(String, String)]
shakeOptionsFields = zipExact fieldsShakeOptions . gmapQ f
    where
        f x | Just x <- cast x = show (x :: Int)
            | Just x <- cast x = show (x :: FilePath)
            | Just x <- cast x = show (x :: Verbosity)
            | Just x <- cast x = show (x :: Change)
            | Just x <- cast x = show (x :: Bool)
            | Just x <- cast x = show (x :: [FilePath])
            | Just x <- cast x = show (x :: [(Rebuild, FilePattern)])
            | Just x <- cast x = show (x :: Maybe Lint)
            | Just x <- cast x = show (x :: Maybe Double)
            | Just x <- cast x = show (x :: Maybe String)
            | Just x <- cast x = show (x :: [(String,String)])
            | Just x <- cast x = show (x :: Hidden (IO Progress -> IO ()))
            | Just x <- cast x = show (x :: Hidden (Verbosity -> String -> IO ()))
            | Just x <- cast x = show (x :: Hidden (Map.HashMap TypeRep Dynamic))
            | Just x <- cast x = show (x :: Hidden (String -> String -> Bool -> IO ()))
            | Just x <- cast x = show (x :: [CmdOption])
            | Just x <- cast x = show (x :: ShakeShuffle)
            | otherwise = throwImpure $ errorInternal $ "Error while showing ShakeOptions, missing alternative for " ++ show (typeOf x)

instance Show ShakeOptions where
    show x = "ShakeOptions {" ++ intercalate ", " (map (\(a,b) -> a ++ " = " ++ b) $ shakeOptionsFields x) ++ "}"


-- | Internal type, copied from Hide in Uniplate
newtype Hidden a = Hidden {fromHidden :: a}
    deriving Typeable

instance Show (Hidden a) where show _ = "<hidden>"

instance Typeable a => Data (Hidden a) where
    gfoldl _ z = z
    gunfold _ _ _ = error "Development.Shake.Types.ShakeProgress: gunfold not implemented - data type has no constructors"
    toConstr _ = error "Development.Shake.Types.ShakeProgress: toConstr not implemented - data type has no constructors"
    dataTypeOf _ = tyHidden

tyHidden = mkDataType "Development.Shake.Types.Hidden" []


-- | The verbosity data type, used by 'shakeVerbosity'.
data Verbosity
    = Silent  -- ^ Don't print any messages.
    | Error     -- ^ Only print error messages.
    | Warn    -- ^ Print errors and warnings.
    | Info    -- ^ Print errors, warnings and @# /command-name/ (for /file-name/)@ when running a 'Development.Shake.traced' command.
    | Verbose -- ^ Print errors, warnings, full command lines when running a 'Development.Shake.command' or
              --   'Development.Shake.cmd' command and status messages when starting a rule.
    | Diagnostic -- ^ Print messages for virtually everything (mostly for debugging).
      deriving (Eq,Ord,Show,Read,Typeable,Data,Enum,Bounded)


-- | Apply the 'shakeRebuild' flags to a file, determining the desired behaviour
shakeRebuildApply :: ShakeOptions -> (FilePath -> Rebuild)
shakeRebuildApply ShakeOptions{shakeRebuild=rs}
    | null rs = const RebuildNormal
    | otherwise = \x -> fromMaybe RebuildNormal $ firstJust (\(r,pat) -> if pat x then Just r else Nothing) rs2
        where rs2 = map (second (?==)) $ reverse rs


shakeAbbreviationsApply :: ShakeOptions -> String -> String
shakeAbbreviationsApply ShakeOptions{shakeAbbreviations=abbrev}
    | null abbrev = id
    | otherwise = f
        where
            -- order so longer abbreviations are preferred
            ordAbbrev = sortOn (negate . length . fst) abbrev

            f [] = []
            f x | (to,rest):_ <- [(to,rest) | (from,to) <- ordAbbrev, Just rest <- [stripPrefix from x]] = to ++ f rest
            f (x:xs) = x : f xs
