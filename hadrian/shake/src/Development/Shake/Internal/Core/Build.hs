{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables, NamedFieldPuns, GADTs #-}
{-# LANGUAGE Rank2Types, ConstraintKinds, TypeOperators, TupleSections, ViewPatterns #-}

module Development.Shake.Internal.Core.Build(
    getDatabaseValue, getDatabaseValueGeneric,
    historyIsEnabled, historySave, historyLoad,
    applyKeyValue,
    apply, apply1,
    ) where

import Development.Shake.Classes
import General.Pool
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.Value
import Development.Shake.Internal.Errors
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Action
import Development.Shake.Internal.History.Shared
import Development.Shake.Internal.History.Cloud
import Development.Shake.Internal.Options
import Development.Shake.Internal.Core.Monad
import General.Wait
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class
import General.Extra
import General.Intern(Id)

import Control.Exception
import Control.Monad.Extra
import Numeric.Extra
import qualified Data.HashMap.Strict as Map
import Development.Shake.Internal.Core.Rules
import Data.Typeable
import Data.Maybe
import Data.List.Extra
import Data.Either.Extra
import System.Time.Extra


---------------------------------------------------------------------
-- LOW-LEVEL OPERATIONS ON THE DATABASE

setIdKeyStatus :: Global -> Database -> Id -> Key -> Status -> Locked ()
setIdKeyStatus Global{..} db i k v = do
    liftIO $ globalDiagnostic $ do
        -- actually safe because we only lose the Locked to enter the diagnostic context
        old <- getKeyValueFromId db i
        let changeStatus = maybe "Missing" (statusType . snd) old ++ " -> " ++ statusType v ++ ", " ++ maybe "<unknown>" (show . fst) old
        let changeValue = case v of
                Ready r -> Just $ "    = " ++ showBracket (result r) ++ " " ++
                    if changed r == globalStep then "(changed)"
                    else if built r == globalStep then "(unchanged)"
                    else "(didn't run)"
                _ -> Nothing
        pure $ changeStatus ++ maybe "" ("\n" ++) changeValue
    setMem db i k v


---------------------------------------------------------------------
-- QUERIES

getDatabaseValue :: (RuleResult key ~ value, ShakeValue key, Typeable value) => key -> Action (Maybe (Result (Either BS.ByteString value)))
getDatabaseValue k =
    fmap (fmap $ fmap $ fmap fromValue) $ getDatabaseValueGeneric $ newKey k

getDatabaseValueGeneric :: Key -> Action (Maybe (Result (Either BS.ByteString Value)))
getDatabaseValueGeneric k = do
    Global{..} <- Action getRO
    Just status <- liftIO $ getValueFromKey globalDatabase k
    pure $ getResult status


---------------------------------------------------------------------
-- NEW STYLE PRIMITIVES

-- | Lookup the value for a single Id, may need to spawn it
lookupOne :: Global -> Stack -> Database -> Id -> Wait Locked (Either SomeException (Result (Value, BS_Store)))
lookupOne global stack database i = do
    res <- quickly $ liftIO $ getKeyValueFromId database i
    case res of
        Nothing -> Now $ Left $ errorStructured "Shake Id no longer exists" [("Id", Just $ show i)] ""
        Just (k, s) -> case s of
            Ready r -> Now $ Right r
            Failed e _ -> Now $ Left e
            Running{} | Left e <- addStack i k stack -> Now $ Left e
            _ -> Later $ \continue -> do
                Just (_, s) <- liftIO $ getKeyValueFromId database i
                case s of
                    Ready r -> continue $ Right r
                    Failed e _ -> continue $ Left e
                    Running (NoShow w) r -> do
                        let w2 v = w v >> continue v
                        setMem database i k $ Running (NoShow w2) r
                    Loaded r -> buildOne global stack database i k (Just r) `fromLater` continue
                    Missing -> buildOne global stack database i k Nothing `fromLater` continue


-- | Build a key, must currently be either Loaded or Missing, changes to Waiting
buildOne :: Global -> Stack -> Database -> Id -> Key -> Maybe (Result BS.ByteString) -> Wait Locked (Either SomeException (Result (Value, BS_Store)))
buildOne global@Global{..} stack database i k r = case addStack i k stack of
    Left e -> do
        quickly $ setIdKeyStatus global database i k $ mkError e
        pure $ Left e
    Right stack -> Later $ \continue -> do
        setIdKeyStatus global database i k (Running (NoShow continue) r)
        let go = buildRunMode global stack database r
        fromLater go $ \mode -> liftIO $ addPool PoolStart globalPool $
            runKey global stack k r mode $ \res -> do
                runLocked database $ do
                    let val = fmap runValue res
                    res <- liftIO $ getKeyValueFromId database i
                    w <- case res of
                        Just (_, Running (NoShow w) _) -> pure w
                        -- We used to be able to hit here, but we fixed it by ensuring the thread pool workers are all
                        -- dead _before_ any exception bubbles up
                        _ -> throwM $ errorInternal $ "expected Waiting but got " ++ maybe "nothing" (statusType . snd) res ++ ", key " ++ show k
                    setIdKeyStatus global database i k $ either mkError Ready val
                    w val
                case res of
                    Right RunResult{..} | runChanged /= ChangedNothing -> setDisk database i k $ Loaded runValue{result=runStore}
                    _ -> pure ()
    where
        mkError e = Failed e $ if globalOneShot then Nothing else r


-- | Compute the value for a given RunMode and a restore function to run
buildRunMode :: Global -> Stack -> Database -> Maybe (Result a) -> Wait Locked RunMode
buildRunMode global stack database me = do
    changed <- case me of
        Nothing -> pure True
        Just me -> buildRunDependenciesChanged global stack database me
    pure $ if changed then RunDependenciesChanged else RunDependenciesSame


-- | Have the dependencies changed
buildRunDependenciesChanged :: Global -> Stack -> Database -> Result a -> Wait Locked Bool
buildRunDependenciesChanged global stack database me = isJust <$> firstJustM id
    [do x <- quickly $ shuffleDependencies global stack x
        firstJustWaitUnordered (fmap test . lookupOne global stack database) x
    | Depends x <- depends me]
    where
        test (Right dep) | changed dep <= built me = Nothing
        test _ = Just ()


-- Only perturb traversal. Callers retain their original lists for returned
-- values and dependency recording.
shuffleDependencies :: Global -> Stack -> [Id] -> Locked [Id]
shuffleDependencies Global{globalOptions=ShakeOptions{shakeShuffle=ShuffleNone}} _ xs = pure xs
shuffleDependencies Global{globalOptions=ShakeOptions{shakeShuffle=ShuffleReverse}} _ xs = pure $ reverse xs
shuffleDependencies Global{globalOptions=ShakeOptions{shakeShuffle}, globalDatabase} stack xs = do
    keys <- liftIO $ forM xs $ \i ->
        maybe "<missing>" (show . fst) <$> getKeyValueFromId globalDatabase i
    let salt = foldl' hashWithSalt (hashWithSalt (length xs) $ show $ topStack stack) keys
    pure $ shakeShuffleList shakeShuffle salt xs


---------------------------------------------------------------------
-- ACTUAL WORKERS

applyKeyValue :: [String] -> [Key] -> Action [Value]
applyKeyValue callStack ks = do
    -- this is the only place a user can inject a key into our world, so check they aren't throwing
    -- in unevaluated bottoms
    liftIO $ mapM_ (evaluate . rnf) ks

    global@Global{..} <- Action getRO
    Local{localStack, localBlockApply} <- Action getRW
    let stack = addCallStack callStack localStack

    let tk = typeKey $ headDef (newKey ()) ks -- always called at non-empty so never see () key
    whenJust localBlockApply $ throwM . errorNoApply tk (show <$> listToMaybe ks)

    let database = globalDatabase
    (is, wait) <- liftIO $ runLocked database $ do
        is <- mapM (mkId database) ks
        traversal <- shuffleDependencies global stack $ nubOrd is
        wait <- runWait $ do
            x <- firstJustWaitUnordered (fmap (either Just (const Nothing)) . lookupOne global stack database) traversal
            case x of
                Just e -> pure $ Left e
                Nothing -> quickly $ Right <$> mapM (fmap (\(Just (_, Ready r)) -> fst $ result r) . liftIO . getKeyValueFromId database) is
        pure (is, wait)
    Action $ modifyRW $ \s -> s{localDepends = addDepends1 (localDepends s) $ Depends is}

    case wait of
        Now vs -> either throwM pure vs
        _ -> do
            offset <- liftIO offsetTime
            vs <- Action $ captureRAW $ \continue ->
                runLocked globalDatabase $ fromLater wait $ \x ->
                    liftIO $ addPool (if isLeft x then PoolException else PoolResume) globalPool $ continue x
            offset <- liftIO offset
            Action $ modifyRW $ addDiscount offset
            pure vs


runKey
    :: Global
    -> Stack  -- Given the current stack with the key added on
    -> Key -- The key to build
    -> Maybe (Result BS.ByteString) -- A previous result, or Nothing if never been built before
    -> RunMode -- True if any of the children were dirty
    -> Capture (Either SomeException (RunResult (Result (Value, BS_Store))))
        -- Either an error, or a (the produced files, the result).
runKey global@Global{globalOptions=ShakeOptions{..},..} stack k r mode continue = do
    let tk = typeKey k
    BuiltinRule{..} <- case Map.lookup tk globalRules of
        Nothing -> throwM $ errorNoRuleToBuildType tk (Just $ show k) Nothing
        Just r -> pure r

    let s = (newLocal stack shakeVerbosity){localBuiltinVersion = builtinVersion}
    time <- offsetTime
    runAction global s (do
        res <- builtinRun k (fmap result r) mode
        liftIO $ evaluate $ rnf res

        -- completed, now track anything required afterwards
        when (runChanged res `elem` [ChangedRecomputeSame,ChangedRecomputeDiff]) $ do
            -- if the users code didn't run you don't have to check anything (we assume builtin rules are correct)
            globalRuleFinished k
            producesCheck

        Action $ fmap (res,) getRW) $ \case
            Left e ->
                continue . Left . toException =<< shakeException global stack e
            Right (RunResult{..}, Local{..})
                | runChanged == ChangedNothing || runChanged == ChangedStore, Just r <- r ->
                    continue $ Right $ RunResult runChanged runStore (r{result = mkResult runValue runStore})
                | otherwise -> do
                    dur <- time
                    let (cr, c) | Just r <- r, runChanged == ChangedRecomputeSame = (ChangedRecomputeSame, changed r)
                                | otherwise = (ChangedRecomputeDiff, globalStep)
                    continue $ Right $ RunResult cr runStore Result
                        {result = mkResult runValue runStore
                        ,changed = c
                        ,built = globalStep
                        ,depends = flattenDepends localDepends
                        ,execution = doubleToFloat $ dur - localDiscount
                        ,traces = flattenTraces localTraces}
            where
                mkResult value store = (value, if globalOneShot then BS.empty else store)

---------------------------------------------------------------------
-- USER key/value WRAPPERS

-- | Execute a rule, returning the associated values. If possible, the rules will be run in parallel.
--   This function requires that appropriate rules have been added with 'addBuiltinRule'.
--   All @key@ values passed to 'apply' become dependencies of the 'Action'.
apply :: (Partial, RuleResult key ~ value, ShakeValue key, Typeable value) => [key] -> Action [value]
apply [] =
    -- if they do [] then we don't test localBlockApply, but unclear if that should be an error or not
    pure []
apply ks =
    fmap (map fromValue) $ Action $ stepRAW (callStackFull, map newKey ks)


-- | Apply a single rule, equivalent to calling 'apply' with a singleton list. Where possible,
--   use 'apply' to allow parallelism.
apply1 :: (Partial, RuleResult key ~ value, ShakeValue key, Typeable value) => key -> Action value
apply1 = withFrozenCallStack $ fmap headErr . apply . pure



---------------------------------------------------------------------
-- HISTORY STUFF

-- | Load a value from the history. Given a version from any user rule
--   (or @0@), return the payload that was stored by 'historySave'.
--
--   If this function returns 'Just' it will also have restored any files that
--   were saved by 'historySave'.
historyLoad :: Int -> Action (Maybe BS.ByteString)
historyLoad (Ver -> ver) = do
    global@Global{..} <- Action getRO
    Local{localStack, localBuiltinVersion} <- Action getRW
    if isNothing globalShared && isNothing globalCloud then pure Nothing else do
        key <- liftIO $ evaluate $ fromMaybe (error "Can't call historyLoad outside a rule") $ topStack localStack
        let database = globalDatabase
        res <- liftIO $ runLocked database $ runWait $ do
            let ask k = do
                    i <- quickly $ mkId database k
                    let identify = runIdentify globalRules k . fst . result
                    either (const Nothing) identify <$> lookupOne global localStack database i
            x <- case globalShared of
                Nothing -> pure Nothing
                Just shared -> lookupShared shared ask key localBuiltinVersion ver
            x <- case x of
                Just res -> pure $ Just res
                Nothing -> case globalCloud of
                    Nothing -> pure Nothing
                    Just cloud -> lookupCloud cloud ask key localBuiltinVersion ver
            case x of
                Nothing -> pure Nothing
                Just (a,b,c) -> quickly $ Just . (a,,c) <$> mapM (mapM $ mkId database) b
        -- FIXME: If running with cloud and shared, and you got a hit in cloud, should also add it to shared
        res <- case res of
            Now x -> pure x
            _ -> do
                offset <- liftIO offsetTime
                res <- Action $ captureRAW $ \continue ->
                    runLocked globalDatabase $ fromLater res $ \x ->
                        liftIO $ addPool PoolResume globalPool $ continue $ Right x
                offset <- liftIO offset
                Action $ modifyRW $ addDiscount offset
                pure res
        case res of
            Nothing -> pure Nothing
            Just (res, deps, restore) -> do
                liftIO $ globalDiagnostic $ pure $ "History hit for " ++ show key
                liftIO restore
                Action $ modifyRW $ \s -> s{localDepends = newDepends $ map Depends deps}
                pure (Just res)


-- | Is the history enabled, returns 'True' if you have a 'shakeShare' or 'shakeCloud',
--   and haven't called 'historyDisable' so far in this rule.
historyIsEnabled :: Action Bool
historyIsEnabled = Action $ do
    Global{..} <- getRO
    Local{localHistory} <- getRW
    pure $ localHistory && (isJust globalShared || isJust globalCloud)


-- | Save a value to the history. Record the version of any user rule
--   (or @0@), and a payload. Must be run at the end of the rule, after
--   any dependencies have been captured. If history is enabled, stores the information
--   in a cache.
--
--   This function relies on 'produces' to have been called correctly to describe
--   which files were written during the execution of this rule.
historySave :: Int -> BS.ByteString -> Action ()
historySave (Ver -> ver) store = whenM historyIsEnabled $ Action $ do
    Global{..} <- getRO
    Local{localProduces, localDepends, localBuiltinVersion, localStack} <- getRW
    liftIO $ do
        -- make sure we throw errors before we get into the history
        evaluate ver
        evaluate store
        key <- evaluate $ fromMaybe (error "Can't call historySave outside a rule") $ topStack localStack

        let produced = reverse $ map snd localProduces
        deps <-
            -- can do this without the DB lock, since it reads things that are stable
            forNothingM (flattenDepends localDepends) $ \(Depends is) -> forNothingM is $ \i -> do
                Just (k, Ready r) <- getKeyValueFromId globalDatabase i
                pure $ (k,) <$> runIdentify globalRules k (fst $ result r)
        let k = topStack localStack
        case deps of
            Nothing -> liftIO $ globalDiagnostic $ pure $ "Dependency with no identity for " ++ show k
            Just deps -> do
                whenJust globalShared $ \shared -> addShared shared key localBuiltinVersion ver deps store produced
                whenJust globalCloud  $ \cloud  -> addCloud  cloud  key localBuiltinVersion ver deps store produced
                liftIO $ globalDiagnostic $ pure $ "History saved for " ++ show k


runIdentify :: Map.HashMap TypeRep BuiltinRule -> Key -> Value -> Maybe BS.ByteString
runIdentify mp k v
    | Just BuiltinRule{..} <- Map.lookup (typeKey k) mp = builtinIdentity k v
    | otherwise = throwImpure $ errorInternal "runIdentify can't find rule"
