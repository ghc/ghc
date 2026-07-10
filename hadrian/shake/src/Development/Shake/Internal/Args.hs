{-# LANGUAGE TupleSections #-}

-- | Command line parsing flags.
module Development.Shake.Internal.Args(
    shakeOptDescrs,
    shake,
    shakeArgs, shakeArgsWith, shakeArgsOptionsWith
    ) where

import Development.Shake.Internal.Paths
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Errors
import Development.Shake.Internal.CompactUI
import Development.Shake.Internal.Demo
import Development.Shake.Internal.Core.Action
import Development.Shake.FilePath
import Development.Shake.Internal.Rules.File
import Development.Shake.Internal.Progress
import Development.Shake.Database
import General.Timing
import General.Extra
import General.Thread
import General.GetOpt
import General.EscCodes

import Data.Tuple.Extra
import Control.DeepSeq
import Control.Exception.Extra
import Control.Monad
import Data.Either
import Data.List.Extra
import Data.Maybe
import System.Directory.Extra
import System.Environment
import System.Exit
import System.Time.Extra


-- | Main entry point for running Shake build systems. For an example see the top of the module "Development.Shake".
--   Use 'ShakeOptions' to specify how the system runs, and 'Rules' to specify what to build. The function will throw
--   an exception if the build fails.
--
--   To use command line flags to modify 'ShakeOptions' see 'shakeArgs'.
shake :: ShakeOptions -> Rules () -> IO ()
shake opts rules = do
    addTiming "Function shake"
    (_, after) <- shakeWithDatabase opts rules $ \db -> do
        shakeOneShotDatabase db
        shakeRunDatabase db []
    shakeRunAfter opts after


-- | Run a build system using command line arguments for configuration.
--   The available flags are those from 'shakeOptDescrs', along with a few additional
--   @make@ compatible flags that are not represented in 'ShakeOptions', such as @--print-directory@.
--   If there are no file arguments then the 'Rules' are used directly, otherwise the file arguments
--   are 'want'ed (after calling 'withoutActions'). As an example:
--
-- @
-- main = 'shakeArgs' 'shakeOptions'{'shakeFiles' = \"_make\", 'shakeProgress' = 'progressSimple'} $ do
--     'phony' \"clean\" $ 'Development.Shake.removeFilesAfter' \"_make\" [\"\/\/*\"]
--     'want' [\"_make\/neil.txt\",\"_make\/emily.txt\"]
--     \"_make\/*.txt\" '%>' \\out ->
--         ... build action here ...
-- @
--
--   This build system will default to building @neil.txt@ and @emily.txt@, while showing progress messages,
--   and putting the Shake files in locations such as @_make\/.database@. Some example command line flags:
--
-- * @main --no-progress@ will turn off progress messages.
--
-- * @main -j6@ will build on 6 threads.
--
-- * @main --help@ will display a list of supported flags.
--
-- * @main clean@ will not build anything, but will remove the @_make@ directory, including the
--   any 'shakeFiles'.
--
-- * @main _make/henry.txt@ will not build @neil.txt@ or @emily.txt@, but will instead build @henry.txt@.
shakeArgs :: ShakeOptions -> Rules () -> IO ()
shakeArgs opts rules = shakeArgsWith opts [] f
    where f _ files = pure $ Just $ if null files then rules else want files >> withoutActions rules


-- | A version of 'shakeArgs' with more flexible handling of command line arguments.
--   The caller of 'shakeArgsWith' can add additional flags (the second argument) and chose how to convert
--   the flags/arguments into rules (the third argument). Given:
--
-- @
-- 'shakeArgsWith' opts flags (\\flagValues argValues -> result)
-- @
--
-- * @opts@ is the initial 'ShakeOptions' value, which may have some fields overridden by command line flags.
--   This argument is usually 'shakeOptions', perhaps with a few fields overridden.
--
-- * @flags@ is a list of flag descriptions, which either produce a 'String' containing an error
--   message (typically for flags with invalid arguments, .e.g. @'Left' \"could not parse as int\"@), or a value
--   that is passed as @flagValues@. If you have no custom flags, pass @[]@.
--
-- * @flagValues@ is a list of custom flags that the user supplied. If @flags == []@ then this list will
--   be @[]@.
--
-- * @argValues@ is a list of non-flag arguments, which are often treated as files and passed to 'want'.
--   If arguments are specified then typically the 'want' calls from the rules are discarded using 'withoutActions'.
--
-- * @result@ should produce a 'Nothing' to indicate that no building needs to take place, or a 'Just'
--   providing the rules that should be used.
--
--   As an example of a build system that can use either @gcc@ or @distcc@ for compiling:
--
-- @
-- import System.Console.GetOpt
--
-- data Flags = DistCC deriving Eq
-- flags = [Option \"\" [\"distcc\"] (NoArg $ Right DistCC) \"Run distributed.\"]
--
-- main = 'shakeArgsWith' 'shakeOptions' flags $ \\flags targets -> pure $ Just $ do
--     let compiler = if DistCC \`elem\` flags then \"distcc\" else \"gcc\"
--     let rules = do
--         \"*.o\" '%>' \\out -> do
--             'need' ...
--             'cmd' compiler ...
--         'want' [\"target.exe\"]
--         ...
--     if null targets then rules else 'want' targets >> 'withoutActions' rules
-- @
--
--   Now you can pass @--distcc@ to use the @distcc@ compiler.
shakeArgsWith :: ShakeOptions -> [OptDescr (Either String a)] -> ([a] -> [String] -> IO (Maybe (Rules ()))) -> IO ()
shakeArgsWith opt args f = shakeArgsOptionsWith opt args $ \so a b -> fmap (so,) <$> f a b

-- | Like 'shakeArgsWith', but also lets you manipulate the 'ShakeOptions'.
shakeArgsOptionsWith
    :: ShakeOptions
    -> [OptDescr (Either String a)]
    -> (ShakeOptions -> [a] -> [String] -> IO (Maybe (ShakeOptions, Rules ())))
    -> IO ()
shakeArgsOptionsWith baseOpts userOptions rules = do
    addTiming "shakeArgsWith"
    let baseOpts2 = removeOverlap userOptions $ map snd shakeOptsEx
    args <- getArgs
    let (flag1,files,errs) = getOpt (baseOpts2 `mergeOptDescr` userOptions) args
        (self,user) = partitionEithers flag1
        (flagsExtra,flagsShake) = first concat $ unzip self
        progressReplays = [x | ProgressReplay x <- flagsExtra]
        progressRecords = [x | ProgressRecord x <- flagsExtra]
        changeDirectory = listToMaybe [x | ChangeDirectory x <- flagsExtra]
        printDirectory = lastDef False [x | PrintDirectory x <- flagsExtra]
        shareRemoves = [x | ShareRemove x <- flagsExtra]
        oshakeOpts = foldl' (flip ($)) baseOpts flagsShake
    lintInside <- mapM canonicalizePath $ shakeLintInside oshakeOpts
    let shakeOpts = oshakeOpts {shakeLintInside = map (toStandard . addTrailingPathSeparator) lintInside
                               ,shakeLintIgnore = map toStandard $ shakeLintIgnore oshakeOpts
                               ,shakeOutput     = if shakeColor oshakeOpts
                                                  then outputColor (shakeOutput oshakeOpts)
                                                  else shakeOutput oshakeOpts
                               }
    let putWhen v msg = when (shakeVerbosity oshakeOpts >= v) $ shakeOutput oshakeOpts v msg
    let putWhenLn v msg = putWhen v $ msg ++ "\n"
    let showHelp long = do
            progName <- getProgName
            (targets, helpSuffix) <- if not long then pure ([], []) else
                handleSynchronous (\e -> do putWhenLn Info $ "Failure to collect targets: " ++ show e; pure ([], [])) $ do
                    -- run the rules as simply as we can
                    rs <- rules shakeOpts [] []
                    case rs of
                        Just (_, rs) -> do
                            xs <- getTargets shakeOpts rs
                            helpSuffix <- getHelpSuffix shakeOpts rs
                            evaluate $ force (["  - " ++ a ++ maybe "" (" - " ++) b | (a,b) <- xs], helpSuffix)
                        _ -> pure ([], [])
            changes<- pure $
                let as = shakeOptionsFields baseOpts
                    bs = shakeOptionsFields oshakeOpts
                in ["  - " ++ lbl ++ ": " ++ v1 ++ " => " ++ v2 | long, ((lbl, v1), (_, v2)) <- zip as bs, v1 /= v2]

            putWhen Error $ unlines $
                ("Usage: " ++ progName ++ " [options] [target] ...") :
                (if null baseOpts2 then [] else "" : (if null userOptions then "Options:" else "Standard options:") : showOptDescr baseOpts2) ++
                (if null userOptions then [] else "" : "Extra options:" : showOptDescr userOptions) ++
                (if null changes then [] else "" : "Changed ShakeOptions:" : changes) ++
                (if null targets then [] else "" : "Targets:" : targets) ++
                (if null helpSuffix then [] else "" : helpSuffix)

    when (errs /= []) $ do
        putWhen Error $ unlines $ map ("shake: " ++) $ filter (not . null) $ lines $ unlines errs
        showHelp False
        exitFailure

    if Help `elem` flagsExtra then
        showHelp True
     else if Version `elem` flagsExtra then
        putWhenLn Info $ "Shake build system, version " ++ shakeVersionString
     else if NumericVersion `elem` flagsExtra then
        putWhenLn Info shakeVersionString
     else if Demo `elem` flagsExtra then
        demo $ shakeStaunch shakeOpts
     else if not $ null progressReplays then do
        dat <- forM progressReplays $ \file -> do
            src <- readFile file
            pure (file, map read $ lines src)
        forM_ (if null $ shakeReport shakeOpts then ["-"] else shakeReport shakeOpts) $ \file -> do
            putWhenLn Info $ "Writing report to " ++ file
            writeProgressReport file dat
     else do
        when (Sleep `elem` flagsExtra) $ sleep 1
        start <- offsetTime
        initDataDirectory -- must be done before we start changing directory
        let redir = maybe id withCurrentDirectory changeDirectory
        shakeOpts <- if null progressRecords then pure shakeOpts else do
            t <- offsetTime
            pure shakeOpts{shakeProgress = \p ->
                void $ withThreadsBoth (shakeProgress shakeOpts p) $
                    progressDisplay 1 (const $ pure ()) $ do
                        p <- p
                        t <- t
                        forM_ progressRecords $ \file ->
                            appendFile file $ show (t,p) ++ "\n"
                        pure p
            }
        (ran,shakeOpts,res) <- redir $ do
            when printDirectory $ do
                curdir <- getCurrentDirectory
                putWhenLn Info $ "shake: In directory `" ++ curdir ++ "'"
            (shakeOpts, ui) <- do
                let compact = lastDef No [x | Compact x <- flagsExtra]
                use <- if compact == Auto then checkEscCodes else pure $ compact == Yes
                if use
                    then second withThreadSlave <$> compactUI shakeOpts
                    else pure (shakeOpts, id)
            rules <- rules shakeOpts user files
            ui $ case rules of
                Nothing -> pure (False, shakeOpts, Right ())
                Just (shakeOpts, rules) -> do
                    res <- try_ $ shake shakeOpts $
                        if NoBuild `elem` flagsExtra then
                            withoutActions rules
                        else if ShareList `elem` flagsExtra ||
                                not (null shareRemoves) ||
                                ShareSanity `elem` flagsExtra then do
                            action $ do
                                unless (null shareRemoves) $
                                    actionShareRemove shareRemoves
                                when (ShareList `elem` flagsExtra)
                                    actionShareList
                                when (ShareSanity `elem` flagsExtra)
                                    actionShareSanity
                            withoutActions rules
                        else
                            rules
                    pure (True, shakeOpts, res)

        if not ran || shakeVerbosity shakeOpts < Info || NoTime `elem` flagsExtra then
            either throwIO pure res
         else
            let esc = if shakeColor shakeOpts then escape else \_ x -> x
            in case res of
                Left err ->
                    if Exception `elem` flagsExtra then
                        throwIO err
                    else do
                        putWhenLn Error $ esc Red $ show err
                        exitFailure
                Right () -> do
                    tot <- start
                    putWhenLn Info $ esc Green $ "Build completed in " ++ showDuration tot


-- | A list of command line options that can be used to modify 'ShakeOptions'. Each option returns
--   either an error message (invalid argument to the flag) or a function that changes some fields
--   in 'ShakeOptions'. The command line flags are @make@ compatible where possbile, but additional
--   flags have been added for the extra options Shake supports.
shakeOptDescrs :: [OptDescr (Either String (ShakeOptions -> ShakeOptions))]
shakeOptDescrs = [fmapFmapOptDescr snd o | (True, o) <- shakeOptsEx]

data Extra = ChangeDirectory FilePath
           | Version
           | NumericVersion
           | PrintDirectory Bool
           | Help
           | Sleep
           | NoTime
           | Exception
           | NoBuild
           | ProgressRecord FilePath
           | ProgressReplay FilePath
           | Demo
           | ShareList
           | ShareSanity
           | ShareRemove String
           | Compact Auto
             deriving Eq

data Auto = Yes | No | Auto
    deriving Eq

escape :: Color -> String -> String
escape color x = escForeground color ++ x ++ escNormal

outputColor :: (Verbosity -> String -> IO ()) -> Verbosity -> String -> IO ()
outputColor output v msg = output v $ color msg
  where color = case v of
            Silent -> id
            Error  -> escape Red
            Warn   -> escape Yellow
            _      -> escape Blue

-- | True if it has a potential effect on ShakeOptions
shakeOptsEx :: [(Bool, OptDescr (Either String ([Extra], ShakeOptions -> ShakeOptions)))]
shakeOptsEx =
    [opts $ Option "a" ["abbrev"] (reqArgPair "abbrev" "FULL=SHORT" $ \a s -> s{shakeAbbreviations=shakeAbbreviations s ++ [a]}) "Use abbreviation in status messages."
    ,opts $ Option ""  ["allow-redefine-rules"] (noArg $ \s -> s{shakeAllowRedefineRules = True}) "Allow redefining built-in rules"
    ,opts $ Option ""  ["no-allow-redefine-rules"] (noArg $ \s -> s{shakeAllowRedefineRules = False}) "Forbid redefining built-in rules (default)"
    ,extr $ Option ""  ["no-build"] (noArg [NoBuild]) "Don't build anything."
    ,extr $ Option "C" ["directory"] (reqArg "DIRECTORY" $ \x -> [ChangeDirectory x]) "Change to DIRECTORY before doing anything."
--    ,yes $ Option ""  ["cloud"] (reqArg "URL" $ \x s -> s{shakeCloud=shakeCloud s ++ [x]}) "HTTP server providing a cloud cache."
    ,opts $ Option ""  ["color","colour"] (noArg $ \s -> s{shakeColor=True}) "Colorize the output."
    ,opts $ Option ""  ["no-color","no-colour"] (noArg $ \s -> s{shakeColor=False}) "Don't colorize the output."
    ,extr $ Option ""  ["compact"] (optArgAuto "auto" "yes|no|auto" $ \x -> [Compact x]) "Use a compact Bazel/Buck style output."
    ,opts $ Option "d" ["debug"] (optArg "FILE" $ \x s -> s{shakeVerbosity=Diagnostic, shakeOutput=outputDebug (shakeOutput s) x}) "Print lots of debugging information."
    ,extr $ Option ""  ["demo"] (noArg [Demo]) "Run in demo mode."
    ,opts $ Option ""  ["digest"] (noArg $ \s -> s{shakeChange=ChangeDigest}) "Files change when digest changes."
    ,opts $ Option ""  ["digest-and"] (noArg $ \s -> s{shakeChange=ChangeModtimeAndDigest}) "Files change when modtime and digest change."
    ,opts $ Option ""  ["digest-and-input"] (noArg $ \s -> s{shakeChange=ChangeModtimeAndDigestInput}) "Files change on modtime (and digest for inputs)."
    ,opts $ Option ""  ["digest-or"] (noArg $ \s -> s{shakeChange=ChangeModtimeOrDigest}) "Files change when modtime or digest change."
    ,opts $ Option ""  ["digest-not"] (noArg $ \s -> s{shakeChange=ChangeModtime}) "Files change when modtime changes."
    ,extr $ Option ""  ["exception"] (noArg [Exception]) "Throw exceptions directly."
    ,opts $ Option ""  ["flush"] (reqIntArg 1 "flush" "N" (\i s -> s{shakeFlush=Just i})) "Flush metadata every N seconds."
    ,opts $ Option ""  ["never-flush"] (noArg $ \s -> s{shakeFlush=Nothing}) "Never explicitly flush metadata."
    ,extr $ Option "h" ["help"] (noArg [Help]) "Print this message and exit."
    ,opts $ Option "j" ["jobs"] (optArgInt 0 "jobs" "N" $ \i s -> s{shakeThreads=fromMaybe 0 i}) "Allow N jobs/threads at once [default CPUs]."
    ,opts $ Option "k" ["keep-going"] (noArg $ \s -> s{shakeStaunch=True}) "Keep going when some targets can't be made."
    ,opts $ Option "l" ["lint"] (noArg $ \s -> s{shakeLint=Just LintBasic}) "Perform limited validation after the run."
    ,opts $ Option ""  ["lint-watch"] (reqArg "PATTERN" $ \x s -> s{shakeLintWatch=shakeLintWatch s ++ [x]}) "Error if any of the patterns are created (expensive)."
    ,opts $ Option ""  ["lint-fsatrace"] (optArg "DIR" $ \x s -> s{shakeLint=Just LintFSATrace, shakeLintInside=shakeLintInside s ++ [fromMaybe "." x]}) "Use fsatrace to do validation [in current dir]."
    ,opts $ Option ""  ["lint-ignore"] (reqArg "PATTERN" $ \x s -> s{shakeLintIgnore=shakeLintIgnore s ++ [x]}) "Ignore any lint errors in these patterns."
    ,opts $ Option ""  ["no-lint"] (noArg $ \s -> s{shakeLint=Nothing}) "Turn off --lint."
    ,opts $ Option ""  ["live"] (optArg "FILE" $ \x s -> s{shakeLiveFiles=shakeLiveFiles s ++ [fromMaybe "live.txt" x]}) "List the files that are live [to live.txt]."
    ,opts $ Option "m" ["metadata"] (reqArg "PREFIX" $ \x s -> s{shakeFiles=x}) "Prefix for storing metadata files."
    ,extr $ Option ""  ["numeric-version"] (noArg [NumericVersion]) "Print just the version number and exit."
    ,opts $ Option ""  ["skip-commands"] (noArg $ \s -> s{shakeRunCommands=False}) "Try and avoid running external programs."
    ,opts $ Option "B" ["rebuild"] (optArg "PATTERN" $ \x s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNow, fromMaybe "**" x)]}) "If required, these files will rebuild even if nothing has changed."
    ,opts $ Option ""  ["no-rebuild"] (optArg "PATTERN" $ \x s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNormal, fromMaybe "**" x)]}) "If required, these files will rebuild only if things have changed (default)."
    ,opts $ Option ""  ["skip"] (optArg "PATTERN" $ \x s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildLater, fromMaybe "**" x)]}) "Don't rebuild matching files this run."
--    ,yes $ Option ""  ["skip-forever"] (OptArg (\x -> Right ([], \s -> s{shakeRebuild=shakeRebuild s ++ [(RebuildNever, fromMaybe "**" x)]})) "PATTERN") "Don't rebuild matching files until they change."
    ,opts $ Option "r" ["report","profile"] (optArg "FILE" $ \x s -> s{shakeReport=shakeReport s ++ [fromMaybe "report.html" x]}) "Write out profiling information [to report.html]."
    ,opts $ Option ""  ["no-reports"] (noArg $ \s -> s{shakeReport=[]}) "Turn off --report."
    ,opts $ Option ""  ["rule-version"] (reqArg "VERSION" $ \x s -> s{shakeVersion=x}) "Version of the build rules."
    ,opts $ Option ""  ["no-rule-version"] (noArg $ \s -> s{shakeVersionIgnore=True}) "Ignore the build rules version."
    ,opts $ Option ""  ["share"] (optArg "DIRECTORY" $ \x s -> s{shakeShare=Just $ fromMaybe "" x, shakeChange=ensureHash $ shakeChange s}) "Shared cache location."
    ,hide $ Option ""  ["share-list"] (noArg ([ShareList], ensureShare)) "List the shared cache files."
    ,hide $ Option ""  ["share-sanity"] (noArg ([ShareSanity], ensureShare)) "Sanity check the shared cache files."
    ,hide $ Option ""  ["share-remove"] (OptArg (\x -> Right ([ShareRemove $ fromMaybe "**" x], ensureShare)) "SUBSTRING") "Remove the shared cache keys."
    ,opts $ Option ""  ["share-copy"] (noArg $ \s -> s{shakeSymlink=False}) "Copy files into the cache."
    ,opts $ Option ""  ["share-symlink"] (noArg $ \s -> s{shakeSymlink=True}) "Symlink files into the cache."
    ,opts $ Option ""  ["shuffle"] (shuffleArg "{SEED|random|reverse|none}") "Shuffle independent build work [random]."
    ,opts $ Option "s" ["silent"] (noArg $ \s -> s{shakeVerbosity=Silent}) "Don't print anything."
    ,extr $ Option ""  ["sleep"] (noArg [Sleep]) "Sleep for a second before building."
    ,opts $ Option "S" ["no-keep-going","stop"] (noArg $ \s -> s{shakeStaunch=False}) "Turns off -k."
    ,opts $ Option ""  ["storage"] (noArg $ \s -> s{shakeStorageLog=True}) "Write a storage log."
    ,both $ Option "p" ["progress"] (progress $ optArgInt 1 "progress" "N" $ \i s -> s{shakeProgress=prog $ fromMaybe 5 i}) "Show progress messages [every N secs, default 5]."
    ,opts $ Option ""  ["no-progress"] (noArg $ \s -> s{shakeProgress=const $ pure ()}) "Don't show progress messages."
    ,opts $ Option "q" ["quiet"] (noArg $ \s -> s{shakeVerbosity=move (shakeVerbosity s) pred}) "Print less (pass repeatedly for even less)."
    ,extr $ Option ""  ["no-time"] (noArg [NoTime]) "Don't print build time."
    ,opts $ Option ""  ["timings"] (noArg $ \s -> s{shakeTimings=True}) "Print phase timings."
    ,opts $ Option "V" ["verbose","trace"] (noArg $ \s -> s{shakeVerbosity=move (shakeVerbosity s) succ}) "Print more (pass repeatedly for even more)."
    ,extr $ Option "v" ["version"] (noArg [Version]) "Print the version number and exit."
    ,extr $ Option "w" ["print-directory"] (noArg [PrintDirectory True]) "Print the current directory."
    ,extr $ Option ""  ["no-print-directory"] (noArg [PrintDirectory False]) "Turn off -w, even if it was turned on implicitly."
    ]
    where
        opts o = (True, fmapFmapOptDescr ([],) o)
        extr o = (False, fmapFmapOptDescr (,id) o)
        both o = (True, o)
        hide o = (False, o) -- I do modify the options, but not in a meaningful way

        move :: Verbosity -> (Int -> Int) -> Verbosity
        move x by = toEnum $ min (fromEnum mx) $ max (fromEnum mn) $ by $ fromEnum x
            where (mn,mx) = (asTypeOf minBound x, asTypeOf maxBound x)

        noArg = NoArg . Right
        reqArg a f = ReqArg (Right . f) a
        optArg a f = OptArg (Right . f) a

        reqIntArg mn flag a f = flip ReqArg a $ \x -> case reads x of
            [(i,"")] | i >= mn -> Right (f i)
            _ -> Left $ "the `--" ++ flag ++ "' option requires a number, " ++ show mn ++ " or above"

        optArgInt mn flag a f = flip OptArg a $ maybe (Right (f Nothing)) $ \x -> case reads x of
            [(i,"")] | i >= mn -> Right (f $ Just i)
            _ -> Left $ "the `--" ++ flag ++ "' option requires a number, " ++ show mn ++ " or above"

        optArgAuto flag a f = flip OptArg a $ maybe (Right (f Yes)) $ \x -> case x of
            "yes" -> Right $ f Yes
            "no" -> Right $ f No
            "auto" -> Right $ f Auto
            _ -> Left $ "the `--" ++ flag ++ "' option requires yes|no|auto, but got " ++ show x

        shuffleArg a = flip OptArg a $ \x -> case x of
            Nothing -> Right $ \s -> s{shakeShuffle=ShuffleRandom Nothing}
            Just "random" -> Right $ \s -> s{shakeShuffle=ShuffleRandom Nothing}
            Just "reverse" -> Right $ \s -> s{shakeShuffle=ShuffleReverse}
            Just "none" -> Right $ \s -> s{shakeShuffle=ShuffleNone}
            Just x -> case reads x of
                [(seed, "")] | seed >= 0 -> Right $ \s -> s{shakeShuffle=ShuffleRandom $ Just seed}
                _ -> Left $ "the `--shuffle' option requires a non-negative integer, random, reverse or none, but got " ++ show x

        reqArgPair flag a f = flip ReqArg a $ \x -> case break (== '=') x of
            (a,'=':b) -> Right $ f (a,b)
            _ -> Left $ "the `--" ++ flag ++ "' option requires an = in the argument"

        progress (OptArg func msg) = flip OptArg msg $ \x -> case break (== '=') `fmap` x of
            Just ("record",file) -> Right ([ProgressRecord $ if null file then "progress.txt" else tailErr file], id)
            Just ("replay",file) -> Right ([ProgressReplay $ if null file then "progress.txt" else tailErr file], id)
            _ -> ([],) <$> func x
        progress _ = throwImpure $ errorInternal "incomplete pattern, progress"

        outputDebug output Nothing = output
        outputDebug output (Just file) = \v msg -> do
            when (v /= Diagnostic) $ output v msg
            appendFile file $ removeEscCodes msg ++ "\n"

        prog i p = do
            program <- progressProgram
            progressDisplay i (\s -> progressTitlebar s >> program s) p

        -- ensure the file system always computes a hash, required for --share
        ensureHash ChangeModtime = ChangeModtimeAndDigest
        ensureHash ChangeModtimeAndDigestInput = ChangeModtimeAndDigest
        ensureHash x = x

        ensureShare s = s{shakeShare = Just $ fromMaybe "." $ shakeShare s}
