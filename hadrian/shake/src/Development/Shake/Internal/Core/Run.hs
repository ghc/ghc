{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE ConstraintKinds, TupleSections, ViewPatterns #-}
{-# LANGUAGE TypeFamilies, NamedFieldPuns #-}

module Development.Shake.Internal.Core.Run(
    RunState,
    open,
    reset,
    run,
    shakeRunAfter,
    liveFilesState,
    profileState,
    errorsState
    ) where

import Control.Exception
import Data.Tuple.Extra
import Control.Concurrent.Extra hiding (withNumCapabilities)
import Development.Shake.Internal.Core.Database
import Control.Monad.IO.Class
import General.Binary
import Development.Shake.Classes
import Development.Shake.Internal.Core.Storage
import Development.Shake.Internal.Core.Build
import Development.Shake.Internal.History.Shared
import Development.Shake.Internal.History.Cloud
import qualified General.TypeMap as TMap
import Control.Monad.Extra
import Data.Typeable
import Numeric.Extra
import Data.List.Extra
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Dynamic
import Data.Maybe
import Data.IORef.Extra
import System.Directory
import System.Time.Extra
import qualified Data.ByteString as BS

import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.Core.Rules
import General.Pool
import Development.Shake.Internal.Progress
import Development.Shake.Internal.Value
import Development.Shake.Internal.Profile
import Development.Shake.Internal.Options
import Development.Shake.Internal.Errors
import General.Timing
import General.Thread
import General.Extra
import General.Cleanup
import Data.Monoid
import Prelude


---------------------------------------------------------------------
-- MAKE

data RunState = RunState
    {opts :: ShakeOptions
    ,builtinRules :: Map.HashMap TypeRep BuiltinRule
    ,userRules :: TMap.Map UserRuleVersioned
    ,database :: Database
    ,curdir :: FilePath
    ,shared :: Maybe Shared
    ,cloud :: Maybe Cloud
    ,actions :: [(Stack, Action ())]
    }


open :: Cleanup -> ShakeOptions -> Rules () -> IO RunState
open cleanup opts rs = do
    opts <- shakeOptionsResolve opts
    withInit opts $ \opts@ShakeOptions{..} diagnostic _ -> do
        diagnostic $ pure "Starting run"
        SRules{actions, builtinRules, userRules} <- runRules opts rs

        diagnostic $ pure $ "Number of actions = " ++ show (length actions)
        diagnostic $ pure $ "Number of builtin rules = " ++ show (Map.size builtinRules) ++ " " ++ show (Map.keys builtinRules)
        diagnostic $ pure $ "Number of user rule types = " ++ show (TMap.size userRules)
        diagnostic $ pure $ "Number of user rules = " ++ show (sum (TMap.toList (userRuleSize . userRuleContents) userRules))

        checkShakeExtra shakeExtra
        curdir <- getCurrentDirectory

        database <- usingDatabase cleanup opts diagnostic builtinRules
        (shared, cloud) <- loadSharedCloud database opts builtinRules
        pure RunState{..}


-- Prepare for a fresh run by changing Result to Loaded
reset :: RunState -> IO ()
reset RunState{..} = runLocked database $
    modifyAllMem database f
    where
        f (Ready r) = Loaded (snd <$> r)
        f (Failed _ x) = maybe Missing Loaded x
        f (Running _ x) = maybe Missing Loaded x -- shouldn't ever happen, but Loaded is least worst
        f x = x


run :: RunState -> Bool -> [Action ()] -> IO [IO ()]
run RunState{..} oneshot actions2 =
    withInit opts $ \opts@ShakeOptions{..} diagnostic output -> do

        -- timings are a bit delicate, we want to make sure we clear them before we leave (so each run is fresh)
        -- but we also want to only print them if there is no exception, and have to caputre them before we clear them
        -- we use this variable to stash them away, then print after the exception handling block
        timingsToShow <- newIORef Nothing

        res <- withCleanup $ \cleanup -> do
            register cleanup $ do
                when (shakeTimings && shakeVerbosity >= Info) $
                    writeIORef timingsToShow . Just =<< getTimings
                resetTimings

            start <- offsetTime
            except <- newIORef (Nothing :: Maybe (String, ShakeException))
            let getFailure = fmap fst <$> readIORef except
            let raiseError err
                    | not shakeStaunch = throwIO err
                    | otherwise = do
                        let named = shakeAbbreviationsApply opts . shakeExceptionTarget
                        atomicModifyIORef except $ \v -> (Just $ fromMaybe (named err, err) v, ())
                        -- no need to print exceptions here, they get printed when they are wrapped

            after <- newIORef []
            absent <- newIORef []
            step <- incrementStep database
            getProgress <- usingProgress cleanup opts database step getFailure
            lintCurrentDirectory curdir "When running"

            watch <- lintWatch shakeLintWatch
            let ruleFinished
                    | isJust shakeLint = \k -> do
                        liftIO $ lintCurrentDirectory curdir $ show k
                        lintTrackFinished
                        liftIO $ watch $ show k
                    | otherwise = liftIO . watch . show

            addTiming "Running rules"
            locals <- newIORef []
            runPool (shakeThreads == 1 || shakeShuffle /= ShuffleNone) shakeThreads $ \pool -> do
                let global = Global applyKeyValue database pool cleanup start builtinRules output opts diagnostic ruleFinished after absent getProgress userRules shared cloud step oneshot
                -- give each action a stack to start with!
                let allActions = actions ++ map (emptyStack,) actions2
                forM_ (shakeShuffleList shakeShuffle (length allActions) allActions) $ \(stack, act) -> do
                    let local = newLocal stack shakeVerbosity
                    addPool PoolStart pool $ runAction global local (act >> getLocal) $ \case
                        Left e -> raiseError =<< shakeException global stack e
                        Right local -> atomicModifyIORef_ locals (local:)

            whenJustM (readIORef except) (throwIO . snd)
            assertFinishedDatabase database
            let putWhen lvl msg = when (shakeVerbosity >= lvl) $ output lvl msg

            locals <- readIORef locals
            end <- start
            if null actions && null actions2 then
                putWhen Info "Warning: No want/action statements, nothing to do"
             else
                recordRoot step locals end database

            when (isJust shakeLint) $ do
                addTiming "Lint checking"
                lintCurrentDirectory curdir "After completion"
                checkValid diagnostic database (runLint builtinRules) =<< readIORef absent
                putWhen Verbose "Lint checking succeeded"
            when (shakeReport /= []) $ do
                addTiming "Profile report"
                forM_ shakeReport $ \file -> do
                    putWhen Info $ "Writing report to " ++ file
                    writeProfile file database
            when (shakeLiveFiles /= []) $ do
                addTiming "Listing live"
                diagnostic $ pure "Listing live keys"
                xs <- liveFiles database
                forM_ shakeLiveFiles $ \file -> do
                    putWhen Info $ "Writing live list to " ++ file
                    (if file == "-" then putStr else writeFile file) $ unlines xs

            res <- readIORef after
            addTiming "Cleanup"
            pure res

        whenJustM (readIORef timingsToShow) $
            putStr . unlines
        pure res


-- | Run a set of IO actions, treated as \"after\" actions, typically returned from
--   'Development.Shake.Database.shakeRunDatabase'. The actions will be run with diagnostics
--   etc as specified in the 'ShakeOptions'.
shakeRunAfter :: ShakeOptions -> [IO ()] -> IO ()
shakeRunAfter _ [] = pure ()
shakeRunAfter opts after = withInit opts $ \ShakeOptions{..} diagnostic _ -> do
    let n = show $ length after
    diagnostic $ pure $ "Running " ++ n ++ " after actions"
    (time, _) <- duration $ sequence_ $ reverse after
    when (shakeTimings && shakeVerbosity >= Info) $
        putStrLn $ "(+ running " ++ show n ++ " after actions in " ++ showDuration time ++ ")"


withInit :: ShakeOptions -> (ShakeOptions -> (IO String -> IO ()) -> (Verbosity -> String -> IO ()) -> IO a) -> IO a
withInit opts act =
    withCleanup $ \cleanup -> do
        opts@ShakeOptions{..} <- usingShakeOptions cleanup opts
        (diagnostic, output) <- outputFunctions opts <$> newLock
        act opts diagnostic output


usingShakeOptions :: Cleanup -> ShakeOptions -> IO ShakeOptions
usingShakeOptions cleanup opts = do
    opts@ShakeOptions{..} <- if shakeThreads opts /= 0 then pure opts else do p <- getProcessorCount; pure opts{shakeThreads=p}
    when shakeLineBuffering $ usingLineBuffering cleanup
    usingNumCapabilities cleanup shakeThreads
    pure opts

outputFunctions :: ShakeOptions -> Lock -> (IO String -> IO (), Verbosity -> String -> IO ())
outputFunctions opts@ShakeOptions{..} outputLock = (diagnostic, output)
    where
        outputLocked v msg = withLock outputLock $ shakeOutput v msg

        diagnostic | shakeVerbosity < Diagnostic = const $ pure ()
                   | otherwise = \act -> do v <- act; outputLocked Diagnostic $ "% " ++ v
        output v = outputLocked v . shakeAbbreviationsApply opts


usingProgress :: Cleanup -> ShakeOptions -> Database -> Step -> IO (Maybe String) -> IO (IO Progress)
usingProgress cleanup ShakeOptions{..} database step getFailure = do
    let getProgress = do
            failure <- getFailure
            stats <- progress database step
            pure stats{isFailure=failure}
    allocateThread cleanup $ shakeProgress getProgress
    pure getProgress

checkShakeExtra :: Map.HashMap TypeRep Dynamic -> IO ()
checkShakeExtra mp = do
    let bad = [(k,t) | (k,v) <- Map.toList mp, let t = dynTypeRep v, t /= k]
    case bad of
        (k,t):xs -> throwIO $ errorStructured "Invalid Map in shakeExtra"
            [("Key",Just $ show k),("Value type",Just $ show t)]
            (if null xs then "" else "Plus " ++ show (length xs) ++ " other keys")
        _ -> pure ()


runLint :: Map.HashMap TypeRep BuiltinRule -> Key -> Value -> IO (Maybe String)
runLint mp k v = case Map.lookup (typeKey k) mp of
    Nothing -> pure Nothing
    Just BuiltinRule{..} -> builtinLint k v


assertFinishedDatabase :: Database -> IO ()
assertFinishedDatabase database = do
    -- if you have anyone Waiting, and are not exiting with an error, then must have a complex recursion (see #400)
    status <- getKeyValues database
    let bad = [key | (key, Running{}) <- status]
    when (bad /= []) $
        throwM $ errorComplexRecursion (map show bad)


liveFilesState :: RunState -> IO [FilePath]
liveFilesState RunState{..} = liveFiles database

profileState :: RunState -> FilePath -> IO ()
profileState RunState{..} file = writeProfile file database

liveFiles :: Database -> IO [FilePath]
liveFiles database = do
    status <- getKeyValues database
    let specialIsFileKey t = show (fst $ splitTyConApp t) == "FileQ"
    pure [show k | (k, Ready{}) <- status, specialIsFileKey $ typeKey k]

errorsState :: RunState -> IO [(String, SomeException)]
errorsState RunState{..} = do
    status <- getKeyValues database
    pure [(show k, e) | (k, Failed e _) <- status]


checkValid :: (IO String -> IO ()) -> Database -> (Key -> Value -> IO (Maybe String)) -> [(Key, Key)] -> IO ()
checkValid diagnostic db check absent = do
    status <- getKeyValues db
    diagnostic $ pure "Starting validity/lint checking"

    -- TEST 1: Have values changed since being depended on
    -- Do not use a forM here as you use too much stack space
    bad <- (\f -> foldM f [] status) $ \seen v -> case v of
        (key, Ready Result{..}) -> do
            good <- check key $ fst result
            diagnostic $ pure $ "Checking if " ++ show key ++ " is " ++ show result ++ ", " ++ if isNothing good then "passed" else "FAILED"
            pure $ [(key, result, now) | Just now <- [good]] ++ seen
        _ -> pure seen
    unless (null bad) $ do
        let n = length bad
        throwM $ errorStructured
            ("Lint checking error - " ++ (if n == 1 then "value has" else show n ++ " values have")  ++ " changed since being depended upon")
            (intercalate [("",Just "")] [ [("Key", Just $ show key),("Old", Just $ show result),("New", Just now)]
                                        | (key, result, now) <- bad])
            ""

    -- TEST 2: Is anything from lintTrackWrite which promised not to exist actually been created
    exists <- getIdFromKey db
    bad <- pure [(parent,key) | (parent, key) <- Set.toList $ Set.fromList absent, isJust $ exists key]
    unless (null bad) $ do
        let n = length bad
        throwM $ errorStructured
            ("Lint checking error - " ++ (if n == 1 then "value" else show n ++ " values") ++ " did not have " ++ (if n == 1 then "its" else "their") ++ " creation tracked")
            (intercalate [("",Just "")] [ [("Rule", Just $ show parent), ("Created", Just $ show key)] | (parent,key) <- bad])
            ""

    diagnostic $ pure "Validity/lint check passed"


---------------------------------------------------------------------
-- STORAGE

usingDatabase :: Cleanup -> ShakeOptions -> (IO String -> IO ()) -> Map.HashMap TypeRep BuiltinRule -> IO Database
usingDatabase cleanup opts diagnostic owitness = do
    let step = (typeRep (Proxy :: Proxy StepKey), (Ver 0, BinaryOp (const mempty) (const stepKey)))
    let root = (typeRep (Proxy :: Proxy Root), (Ver 0, BinaryOp (const mempty) (const rootKey)))
    witness<- pure $ Map.fromList
        [ (QTypeRep t, (version, BinaryOp (putDatabase putOp) (getDatabase getOp)))
        | (t,(version, BinaryOp{..})) <- step : root : Map.toList (Map.map (\BuiltinRule{..} -> (builtinVersion, builtinKey)) owitness)]
    (status, journal) <- usingStorage cleanup opts diagnostic witness
    journal<- pure $ \i k v -> journal (QTypeRep $ typeKey k) i (k, v)
    createDatabase status journal Missing


incrementStep :: Database -> IO Step
incrementStep db = runLocked db $ do
    stepId <- mkId db stepKey
    v <- liftIO $ getKeyValueFromId db stepId
    step <- liftIO $ evaluate $ case v of
        Just (_, Loaded r) -> incStep $ fromStepResult r
        _ -> Step 1
    let stepRes = toStepResult step
    setMem db stepId stepKey $ Ready stepRes
    liftIO $ setDisk db stepId stepKey $ Loaded $ fmap snd stepRes
    pure step

toStepResult :: Step -> Result (Value, BS_Store)
toStepResult i = Result (newValue i, runBuilder $ putEx i) i i [] 0 []

fromStepResult :: Result BS_Store -> Step
fromStepResult = getEx . result


recordRoot :: Step -> [Local] -> Seconds -> Database -> IO ()
recordRoot step locals (doubleToFloat -> end) db = runLocked db $ do
    rootId <- mkId db rootKey
    let local = localMergeMutable (newLocal emptyStack Info) locals
    let rootRes = Result
            {result = (newValue (), BS.empty)
            ,changed = step
            ,built = step
            ,depends = flattenDepends $ localDepends local
            ,execution = 0
            ,traces = flattenTraces $ addTrace (localTraces local) $ Trace BS.empty end end}
    setMem db rootId rootKey $ Ready rootRes
    liftIO $ setDisk db rootId rootKey $ Loaded $ fmap snd rootRes


loadSharedCloud :: DatabasePoly k v -> ShakeOptions -> Map.HashMap TypeRep BuiltinRule -> IO (Maybe Shared, Maybe Cloud)
loadSharedCloud var opts owitness = do
    let mp = Map.fromList $ map (first $ show . QTypeRep) $ Map.toList owitness
    let wit = binaryOpMap $ \a -> maybe (error $ "loadSharedCloud, couldn't find map for " ++ show a) builtinKey $ Map.lookup a mp
    let wit2 = BinaryOp (\k -> putOp wit (show $ QTypeRep $ typeKey k, k)) (snd . getOp wit)
    let keyVers = [(k, builtinVersion v) | (k,v) <- Map.toList owitness]
    let ver = makeVer $ shakeVersion opts

    shared <- case shakeShare opts of
        Nothing -> pure Nothing
        Just x -> Just <$> newShared (shakeSymlink opts) wit2 ver x
    cloud <- case newCloud (runLocked var) (Map.map builtinKey owitness) ver keyVers $ shakeCloud opts of
        _ | null $ shakeCloud opts -> pure Nothing
        Nothing -> fail "shakeCloud set but Shake not compiled for cloud operation"
        Just res -> Just <$> res
    pure (shared, cloud)


putDatabase :: (Key -> Builder) -> ((Key, Status) -> Builder)
putDatabase putKey (key, Loaded (Result x1 x2 x3 x4 x5 x6)) =
    putExN (putKey key) <> putExN (putEx x1) <> putEx x2 <> putEx x3 <> putEx x5 <> putExN (putEx x4) <> putEx x6
putDatabase _ (_, x) = throwImpure $ errorInternal $ "putWith, Cannot write Status with constructor " ++ statusType x


getDatabase :: (BS.ByteString -> Key) -> BS.ByteString -> (Key, Status)
getDatabase getKey bs
    | (key, bs) <- getExN bs
    , (x1, bs) <- getExN bs
    , (x2, x3, x5, bs) <- binarySplit3 bs
    , (x4, x6) <- getExN bs
    = (getKey key, Loaded (Result x1 x2 x3 (getEx x4) x5 (getEx x6)))
