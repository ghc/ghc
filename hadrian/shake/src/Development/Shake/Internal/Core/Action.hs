{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables, ConstraintKinds, TupleSections #-}

module Development.Shake.Internal.Core.Action(
    actionOnException, actionFinally, actionBracket, actionCatch, actionRetry,
    getShakeOptions, getProgress, runAfter,
    lintTrackRead, lintTrackWrite, lintTrackAllow,
    getVerbosity, putWhen, putVerbose, putInfo, putWarn, putError, withVerbosity, quietly,
    orderOnlyAction,
    newCacheIO,
    unsafeExtraThread,
    parallel,
    batch,
    reschedule,
    historyDisable,
    traced,
    -- Internal only
    producesChecked, producesUnchecked, producesCheck, lintCurrentDirectory, lintWatch,
    blockApply, unsafeAllowApply, shakeException, lintTrackFinished,
    getCurrentKey, getLocal,
    actionShareList, actionShareRemove, actionShareSanity
    ) where

import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.DeepSeq
import Data.Typeable
import System.Directory
import System.FilePattern
import System.FilePattern.Directory
import System.Time.Extra
import Control.Concurrent.Extra
import Data.Maybe
import Data.Tuple.Extra
import Data.IORef.Extra
import Data.List.Extra
import Numeric.Extra
import General.Extra
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set

import Development.Shake.Classes
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.History.Shared
import General.Pool
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Value
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.FileName
import Development.Shake.Internal.Options
import Development.Shake.Internal.Errors
import General.Cleanup
import General.Fence


---------------------------------------------------------------------
-- RAW WRAPPERS

-- | Apply a modification, run an action, then run an undo action after.
--   Doesn't actually require exception handling because we don't have the ability to catch exceptions to the user.
actionThenUndoLocal :: (Local -> (Local, Local -> Local)) -> Action a -> Action a
actionThenUndoLocal f m = Action $ do
    s <- getRW
    let (s2,undo) = f s
    putRW s2
    res <- fromAction m
    modifyRW undo
    pure res


---------------------------------------------------------------------
-- EXCEPTION HANDLING

-- | Turn a normal exception into a ShakeException, giving it a stack and printing it out if in staunch mode.
--   If the exception is already a ShakeException (e.g. it's a child of ours who failed and we are rethrowing)
--   then do nothing with it.
shakeException :: Global -> Stack -> SomeException -> IO ShakeException
shakeException Global{globalOptions=ShakeOptions{..},..} stk e = case fromException e of
    Just (e :: ShakeException) -> pure e
    Nothing -> do
        e<- pure $ exceptionStack stk e
        when (shakeStaunch && shakeVerbosity >= Error) $
            globalOutput Error $ show e ++ "Continuing due to staunch mode"
        pure e


actionBracketEx :: Bool -> IO a -> (a -> IO b) -> (a -> Action c) -> Action c
actionBracketEx runOnSuccess alloc free act = do
    Global{..} <- Action getRO
    (v, key) <- liftIO $ mask_ $ do
        v <- alloc
        key <- liftIO $ register globalCleanup $ void $ free v
        pure (v, key)
    res <- Action $ catchRAW (fromAction $ act v) $ \e -> liftIO (release key) >> throwRAW e
    liftIO $ if runOnSuccess then release key else unprotect key
    pure res

-- | If an exception is raised by the 'Action', perform some 'IO' then reraise the exception.
--   This function is implemented using 'actionBracket'.
actionOnException :: Action a -> IO b -> Action a
actionOnException act free = actionBracketEx False (pure ()) (const free) (const act)

-- | After an 'Action', perform some 'IO', even if there is an exception.
--   This function is implemented using 'actionBracket'.
actionFinally :: Action a -> IO b -> Action a
actionFinally act free = actionBracket (pure ()) (const free) (const act)

-- | Like 'bracket', but where the inner operation is of type 'Action'. Usually used as
--   @'actionBracket' alloc free use@.
--
--   The @free@ action will be run masked. The cost of 'actionBracket' is _O(n log n)_
--   in the number of simultaneous 'actionBracket' calls active in the program.
actionBracket :: IO a -> (a -> IO b) -> (a -> Action c) -> Action c
actionBracket = actionBracketEx True


-- | If a syncronous exception is raised by the 'Action', perform some handler.
--   Note that there is no guarantee that the handler will run on shutdown (use 'actionFinally' for that),
--   and that 'actionCatch' /cannot/ catch exceptions thrown by dependencies, e.g. raised by 'need'
--   (to do so would allow untracked dependencies on failure conditions).
actionCatch :: Exception e => Action a -> (e -> Action a) -> Action a
actionCatch act hdl = Action $ catchRAW (fromAction act) $ \e ->
    case () of
        _ | not $ isAsyncException e
          , Nothing <- fromException e :: Maybe ShakeException
          , Just e <- fromException e
          -> fromAction $ hdl e
        _ -> throwRAW e


-- | Retry an 'Action' if it throws an exception, at most /n/ times (where /n/ must be positive).
--   If you need to call this function, you should probably try and fix the underlying cause (but you also probably know that).
actionRetry :: Int -> Action a -> Action a
actionRetry i act
    | i <= 0 = fail $ "actionRetry first argument must be positive, got " ++ show i
    | i == 1 = act
    | otherwise = Action $ catchRAW (fromAction act) $ \_ -> fromAction $ actionRetry (i-1) act


---------------------------------------------------------------------
-- QUERIES

-- | Get the initial 'ShakeOptions', these will not change during the build process.
getShakeOptions :: Action ShakeOptions
getShakeOptions = Action $ globalOptions <$> getRO


-- | Get the current 'Progress' structure, as would be returned by 'shakeProgress'.
getProgress :: Action Progress
getProgress = do
    Global{..} <- Action getRO
    liftIO globalProgress

-- | Specify an action to be run after the database has been closed, if building completes successfully.
runAfter :: IO () -> Action ()
runAfter op = do
    Global{..} <- Action getRO
    liftIO $ atomicModifyIORef_ globalAfter (op:)


---------------------------------------------------------------------
-- VERBOSITY

putWhen :: Verbosity -> String -> Action ()
putWhen v msg = do
    Global{..} <- Action getRO
    verb <- getVerbosity
    when (verb >= v) $
        liftIO $ globalOutput v msg


-- | Write an unimportant message to the output, only shown when 'shakeVerbosity' is higher than normal ('Verbose' or above).
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putVerbose :: String -> Action ()
putVerbose = putWhen Verbose

-- | Write a normal priority message to the output, only suppressed when 'shakeVerbosity' is 'Error', 'Warn' or 'Silent'.
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putInfo :: String -> Action ()
putInfo = putWhen Info

-- | Write a semi important message to the output, only suppressed when 'shakeVerbosity' is 'Error' or 'Silent'.
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putWarn :: String -> Action ()
putWarn = putWhen Warn

-- | Write an important message to the output, only suppressed when 'shakeVerbosity' is 'Silent'.
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putError :: String -> Action ()
putError = putWhen Error


-- | Get the current verbosity level, originally set by 'shakeVerbosity'. If you
--   want to output information to the console, you are recommended to use
--   'putVerbose' \/ 'putInfo' \/ 'putError', which ensures multiple messages are
--   not interleaved. The verbosity can be modified locally by 'withVerbosity'.
getVerbosity :: Action Verbosity
getVerbosity = Action $ localVerbosity <$> getRW


-- | Run an action with a particular verbosity level.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not have any impact on 'Diagnostic' tracing.
withVerbosity :: Verbosity -> Action a -> Action a
withVerbosity new = actionThenUndoLocal $ \s0 ->
    (s0{localVerbosity=new}, \s -> s{localVerbosity=localVerbosity s0})


-- | Run an action with 'Error' verbosity, in particular messages produced by 'traced'
--   (including from 'Development.Shake.cmd' or 'Development.Shake.command') will not be printed to the screen.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not turn off any 'Diagnostic' tracing.
quietly :: Action a -> Action a
quietly = withVerbosity Error


---------------------------------------------------------------------
-- BLOCK APPLY

unsafeAllowApply :: Action a -> Action a
unsafeAllowApply  = applyBlockedBy Nothing

blockApply :: String -> Action a -> Action a
blockApply = applyBlockedBy . Just

applyBlockedBy :: Maybe String -> Action a -> Action a
applyBlockedBy reason = actionThenUndoLocal $ \s0 ->
    (s0{localBlockApply=reason}, \s -> s{localBlockApply=localBlockApply s0})


---------------------------------------------------------------------
-- TRACING

-- | Write an action to the trace list, along with the start/end time of running the IO action.
--   The 'Development.Shake.cmd' and 'Development.Shake.command' functions automatically call 'traced'
--   with the name of the executable. The trace list is used for profile reports (see 'shakeReport').
--
--   By default 'traced' prints some useful extra context about what
--   Shake is building, e.g.:
--
-- > # traced message (for myobject.o)
--
--   To suppress the output of 'traced' (for example you want more control
--   over the message using 'putInfo'), use the 'quietly' combinator.
--
--   It is recommended that the string passed to 'traced' is short and that only a small number of unique strings
--   are used (makes profiling work better).
--   The string does not need to make sense on its own, only in conjunction with the target it is building.
traced :: String -> IO a -> Action a
traced msg act = do
    Global{..} <- Action getRO
    Local{localStack} <- Action getRW
    start <- liftIO globalTimestamp
    let key = showTopStack localStack
    putInfo $ "# " ++ msg ++ " (for " ++ key ++ ")"
    res <- liftIO $
        (shakeTrace globalOptions key msg True >> act)
            `finally` shakeTrace globalOptions key msg False
    stop <- liftIO globalTimestamp
    let trace = newTrace msg start stop
    liftIO $ evaluate $ rnf trace
    Action $ modifyRW $ \s -> s{localTraces = addTrace (localTraces s) trace}
    pure res


---------------------------------------------------------------------
-- TRACKING

-- | Track that a key has been used/read by the action preceding it when 'shakeLint' is active.
lintTrackRead :: ShakeValue key => [key] -> Action ()
-- One of the following must be true:
-- 1) you are the one building this key (e.g. key == topStack)
-- 2) you have already been used by apply, and are on the dependency list
-- 3) someone explicitly gave you permission with trackAllow
-- 4) at the end of the rule, a) you are now on the dependency list, and b) this key itself has no dependencies (is source file)
lintTrackRead ks = do
    Global{..} <- Action getRO
    when (isJust $ shakeLint globalOptions) $ do
        l@Local{..} <- Action getRW
        deps <- liftIO $ concatMapM (listDepends globalDatabase) $ enumerateDepends localDepends
        let top = topStack localStack

        let condition1 k = top == Just k
        let condition2 k = k `elem` deps
        let condition3 k = any ($ k) localTrackAllows
        let condition4 = filter (\k -> not $ condition1 k || condition2 k || condition3 k) $ map newKey ks
        unless (null condition4) $
            Action $ putRW l{localTrackRead = condition4 ++ localTrackRead}


-- | Track that a key has been changed/written by the action preceding it when 'shakeLint' is active.
lintTrackWrite :: ShakeValue key => [key] -> Action ()
-- One of the following must be true:
-- 1) you are the one building this key (e.g. key == topStack)
-- 2) someone explicitly gave you permission with trackAllow
-- 3) this file is never known to the build system, at the end it is not in the database
lintTrackWrite ks = do
    Global{..} <- Action getRO
    when (isJust $ shakeLint globalOptions) $ do
        l@Local{..} <- Action getRW
        let top = topStack localStack

        let condition1 k = Just k == top
        let condition2 k = any ($ k) localTrackAllows
        let condition3 = filter (\k -> not $ condition1 k || condition2 k) $ map newKey ks
        unless (null condition3) $
            Action $ putRW l{localTrackWrite = condition3 ++ localTrackWrite}


lintTrackFinished :: Action ()
lintTrackFinished = do
    -- only called when isJust shakeLint
    Global{..} <- Action getRO
    Local{..} <- Action getRW
    liftIO $ do
        let top = topStack localStack
        -- must apply the ignore at the end, because we might have merged in more ignores that
        -- apply to other branches
        let ignore k = any ($ k) localTrackAllows

        -- Read stuff
        deps <- concatMapM (listDepends globalDatabase) $ enumerateDepends localDepends
        let used = Set.filter (not . ignore) $ Set.fromList localTrackRead

        -- check Read 4a
        bad<- pure $ Set.toList $ used `Set.difference` Set.fromList deps
        unless (null bad) $ do
            let n = length bad
            throwM $ errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " used but not depended upon")
                [("Used", Just $ show x) | x <- bad]
                ""

        -- check Read 4b
        bad <- flip filterM (Set.toList used) $ \k -> not . null <$> lookupDependencies globalDatabase k
        unless (null bad) $ do
            let n = length bad
            throwM $ errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " depended upon after being used")
                [("Used", Just $ show x) | x <- bad]
                ""

        -- check Write 3
        bad<- pure $ filter (not . ignore) $ Set.toList $ Set.fromList localTrackWrite
        unless (null bad) $
            liftIO $ atomicModifyIORef_ globalTrackAbsent ([(fromMaybe k top, k) | k <- bad] ++)


-- | Allow any matching key recorded with 'lintTrackRead' or 'lintTrackWrite' in this action,
--   after this call, to violate the tracking rules.
lintTrackAllow :: ShakeValue key => (key -> Bool) -> Action ()
lintTrackAllow (test :: key -> Bool) = do
    Global{..} <- Action getRO
    when (isJust $ shakeLint globalOptions) $
        Action $ modifyRW $ \s -> s{localTrackAllows = f : localTrackAllows s}
    where
        tk = typeRep (Proxy :: Proxy key)
        f k = typeKey k == tk && test (fromKey k)


lintCurrentDirectory :: FilePath -> String -> IO ()
lintCurrentDirectory old msg = do
    now <- getCurrentDirectory
    when (old /= now) $ throwIO $ errorStructured
        "Lint checking error - current directory has changed"
        [("When", Just msg)
        ,("Wanted",Just old)
        ,("Got",Just now)]
        ""

lintWatch :: [FilePattern] -> IO (String -> IO ())
lintWatch [] = pure $ const $ pure ()
lintWatch pats = do
    let op = getDirectoryFiles "." pats -- cache parsing of the pats
    let record = do xs <- op; forM xs $ \x -> (x,) <$> getFileInfo False (fileNameFromString x)
    old <- record
    pure $ \msg -> do
        now <- record
        when (old /= now) $ throwIO $ errorStructured
            "Lint checking error - watched files have changed"
            (("When", Just msg) : changes (Map.fromList old) (Map.fromList now))
            ""
    where
        changes old now =
            [("Created", Just x) | x <- Map.keys $ Map.difference now old] ++
            [("Deleted", Just x) | x <- Map.keys $ Map.difference old now] ++
            [("Changed", Just x) | x <- Map.keys $ Map.filter id $ Map.intersectionWith (/=) old now]


listDepends :: Database -> Depends -> IO [Key]
listDepends db (Depends xs) = mapM (fmap (fst . fromJust) . getKeyValueFromId db) xs


lookupDependencies :: Database -> Key -> IO [Depends]
lookupDependencies db k = do
    Just (Ready r) <- getValueFromKey db k
    pure $ depends r


-- | This rule should not be saved to shared/cloud storage via 'shakeShare'.
--   There are usually two reasons to call this function:
--
--   1. It makes use of untracked dependencies that are specific to this machine,
--      e.g. files in a system directory or items on the @$PATH@.
--   2. The rule is trivial to compute locally, so there is no point sharing it.
--
--   If you want the rule to not be cached at all, use 'alwaysRerun'.
historyDisable :: Action ()
historyDisable = Action $ modifyRW $ \s -> s{localHistory = False}


-- | A version of 'produces' that checks the files actually exist
producesChecked :: [FilePath] -> Action ()
producesChecked xs = Action $ modifyRW $ \s -> s{localProduces = map (True,) (reverse xs) ++ localProduces s}

-- | A version of 'produces' that does not check.
producesUnchecked :: [FilePath] -> Action ()
producesUnchecked xs = Action $ modifyRW $ \s -> s{localProduces = map (False,) (reverse xs) ++ localProduces s}

producesCheck :: Action ()
producesCheck = do
    Local{localProduces} <- Action getRW
    missing <- liftIO $ filterM (notM . doesFileExist_) $ map snd $ filter fst localProduces
    when (missing /= []) $ throwM $ errorStructured
        "Files declared by 'produces' not produced"
        [("File " ++ show i, Just x) | (i,x) <- zipFrom 1 missing]
        ""


-- | Run an action but do not depend on anything the action uses.
--   A more general version of 'orderOnly'.
orderOnlyAction :: Action a -> Action a
orderOnlyAction act = Action $ do
    Local{localDepends=pre} <- getRW
    res <- fromAction act
    modifyRW $ \s -> s{localDepends=pre}
    pure res


---------------------------------------------------------------------
-- MORE COMPLEX

-- | A version of 'Development.Shake.newCache' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newCache' instead.
newCacheIO :: (Eq k, Hashable k) => (k -> Action v) -> IO (k -> Action v)
newCacheIO (act :: k -> Action v) = do
    var :: Var (Map.HashMap k (Fence IO (Either SomeException (DependsList,v)))) <- newVar Map.empty
    pure $ \key ->
        join $ liftIO $ modifyVar var $ \mp -> case Map.lookup key mp of
            Just bar -> pure $ (,) mp $ do
                (offset, (deps, v)) <- actionFenceRequeue bar
                Action $ modifyRW $ \s -> addDiscount offset $ s{localDepends = addDepends (localDepends s) deps}
                pure v
            Nothing -> do
                bar <- newFence
                pure $ (Map.insert key bar mp,) $ do
                    Local{localDepends=pre} <- Action getRW
                    Action $ modifyRW $ \s -> s{localDepends = newDepends []}
                    res <- Action $ tryRAW $ fromAction $ act key
                    case res of
                        Left err -> do
                            liftIO $ signalFence bar $ Left err
                            Action $ throwRAW err
                        Right v -> do
                            Local{localDepends=deps} <- Action getRW
                            Action $ modifyRW $ \s -> s{localDepends = addDepends pre deps}
                            liftIO $ signalFence bar $ Right (deps, v)
                            pure v


-- | Run an action without counting to the thread limit, typically used for actions that execute
--   on remote machines using barely any local CPU resources.
--   Unsafe as it allows the 'shakeThreads' limit to be exceeded.
--   You cannot depend on a rule (e.g. 'need') while the extra thread is executing.
--   If the rule blocks (e.g. calls 'withResource') then the extra thread may be used by some other action.
--   Only really suitable for calling 'cmd' / 'command'.
unsafeExtraThread :: Action a -> Action a
unsafeExtraThread act = do
    Global{..} <- Action getRO
    stop <- liftIO $ increasePool globalPool
    res <- Action $ tryRAW $ fromAction $ blockApply "Within unsafeExtraThread" act
    liftIO stop
    -- we start a new thread, giving up ours, to ensure the thread count goes down
    (wait, res) <- actionAlwaysRequeue res
    Action $ modifyRW $ addDiscount wait
    pure res


-- | Execute a list of actions in parallel. In most cases 'need' will be more appropriate to benefit from parallelism.
--   If the two types of 'Action' are different dependencies which ultimately boil down to 'apply',
--   using 'Applicative' operations will still ensure the dependencies occur in parallel.
--   The main use of this function is to run work that happens in an 'Action' in parallel.
parallel :: [Action a] -> Action [a]
-- Note: There is no parallel_ unlike sequence_ because there is no stack benefit to doing so
parallel [] = pure []
parallel [x] = pure <$> x
parallel acts = do
    Global{..} <- Action getRO

    done <- liftIO $ newIORef False
    waits <- forM acts $ \act ->
        addPoolWait PoolResume $ do
            whenM (liftIO $ readIORef done) $
                fail "parallel, one has already failed"
            Action $ modifyRW localClearMutable
            res <- act
            old <- Action getRW
            pure (old, res)
    (wait, res) <- actionFenceSteal =<< liftIO (exceptFence waits)
    liftIO $ atomicWriteIORef done True
    let (waits, locals, results) = unzip3 $ map (\(a,(b,c)) -> (a,b,c)) res
    Action $ modifyRW $ \root -> addDiscount (wait - sum waits) $ localMergeMutable root locals
    pure results


-- | Batch different outputs into a single 'Action', typically useful when a command has a high
--   startup cost - e.g. @apt-get install foo bar baz@ is a lot cheaper than three separate
--   calls to @apt-get install@. As an example, if we have a standard build rule:
--
-- @
-- \"*.out\" 'Development.Shake.%>' \\out -> do
--     'Development.Shake.need' [out '-<.>' \"in\"]
--     'Development.Shake.cmd' "build-multiple" [out '-<.>' \"in\"]
-- @
--
--   Assuming that @build-multiple@ can compile multiple files in a single run,
--   and that the cost of doing so is a lot less than running each individually,
--   we can write:
--
-- @
-- 'batch' 3 (\"*.out\" 'Development.Shake.%>')
--     (\\out -> do 'Development.Shake.need' [out '-<.>' \"in\"]; pure out)
--     (\\outs -> 'Development.Shake.cmd' "build-multiple" [out '-<.>' \"in\" | out \<- outs])
-- @
--
--   In constrast to the normal call, we have specified a maximum batch size of 3,
--   an action to run on each output individually (typically all the 'need' dependencies),
--   and an action that runs on multiple files at once. If we were to require lots of
--   @*.out@ files, they would typically be built in batches of 3.
--
--   If Shake ever has nothing else to do it will run batches before they are at the maximum,
--   so you may see much smaller batches, especially at high parallelism settings.
batch
    :: Int   -- ^ Maximum number to run in a single batch, e.g. @3@, must be positive.
    -> ((a -> Action ()) -> Rules ()) -- ^ Way to match an entry, e.g. @\"*.ext\" '%>'@.
    -> (a -> Action b)  -- ^ Preparation to run individually on each, e.g. using 'need'.
    -> ([b] -> Action ())  -- ^ Combination action to run on all, e.g. using 'cmd'.
    -> Rules ()
batch mx pred one many
    | mx <= 0 = error $ "Can't call batchable with <= 0, you used " ++ show mx
    | mx == 1 = pred $ \a -> do b <- one a; many [b]
    | otherwise = do
        todo :: IORef (Int, [(b, Local, Fence IO (Either SomeException (Seconds, Local)))]) <- liftIO $ newIORef (0, [])
        pred $ \a -> do
            b <- one a
            fence <- liftIO newFence
            -- add one to the batch
            local <- Action getRW
            count <- liftIO $ atomicModifyIORef todo $ \(count, bs) -> let i = count+1 in ((i, (b,local,fence):bs), i)
            requeue todo (==) count
            (wait, (cost, local2)) <- actionFenceRequeue fence
            Action $ modifyRW $ \root -> addDiscount (wait - cost) $ localMergeMutable root [local2]
    where
        -- When changing by one, only trigger on (==) so we don't have lots of waiting pool entries
        -- When changing by many, trigger on (>=) because we don't hit all edges
        requeue todo trigger count
            | count `trigger` mx = addPoolWait_ PoolResume $ go todo
            | count `trigger` 1  = addPoolWait_ PoolBatch  $ go todo
            | otherwise = pure ()

        go todo = do
            -- delete at most mx from the batch
            (now, count) <- liftIO $ atomicModifyIORef todo $ \(count, bs) ->
                let (now,later) = splitAt mx bs
                    count2 = if count > mx then count - mx else 0
                in ((count2, later), (now, count2))
            requeue todo (>=) count

            unless (null now) $ do
                res <- Action $ tryRAW $ do
                    -- make sure we are using one of the local's that we are computing
                    -- we things like stack, blockApply etc. work as expected
                    modifyRW $ const $ localClearMutable $ snd3 $ headErr now
                    start <- liftIO offsetTime
                    fromAction $ many $ map fst3 now
                    end <- liftIO start

                    -- accounting for time is tricky, we spend time T, over N jobs
                    -- so want to charge everyone for T / N time
                    -- but that also means we need to subtract localDiscount so we don't apply that to all
                    rw <- getRW
                    let t = end - localDiscount rw
                    let n = intToDouble (length now)
                    pure (t / n, rw{localDiscount = 0})
                liftIO $ mapM_ (flip signalFence res . thd3) now


-- | Given a running task, reschedule so it only continues after all other pending tasks,
--   and all rescheduled tasks with a higher pool priority. Note that due to parallelism there is no guarantee
--   that all actions of a higher pool priority will have /completed/ before the action resumes.
--   Only useful if the results are being interactively reported or consumed.
reschedule :: Double -> Action ()
reschedule x = do
    (wait, _) <- actionAlwaysRequeuePriority (PoolDeprioritize $ negate x) $ pure ()
    Action $ modifyRW $ addDiscount wait


getCurrentKey :: Action (Maybe Key)
getCurrentKey = Action $ topStack . localStack <$> getRW

getLocal :: Action Local
getLocal = Action getRW

-- | Hooked up to --share-remove
actionShareRemove :: [String] -> Action ()
actionShareRemove substrs = do
    Global{..} <- Action getRO
    case globalShared of
        Nothing -> throwM $ errorInternal "actionShareRemove with no shared"
        Just x -> liftIO $ removeShared x $ \k -> any (`isInfixOf` show k) substrs

-- | Hooked up to --share-list
actionShareList :: Action ()
actionShareList = do
    Global{..} <- Action getRO
    case globalShared of
        Nothing -> throwM $ errorInternal "actionShareList with no shared"
        Just x -> liftIO $ listShared x

-- | Hooked up to --share-sanity
actionShareSanity :: Action ()
actionShareSanity = do
    Global{..} <- Action getRO
    case globalShared of
        Nothing -> throwM $ errorInternal "actionShareSanity with no shared"
        Just x -> liftIO $ sanityShared x
