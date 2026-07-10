{-# LANGUAGE ViewPatterns, TypeOperators #-}

module Model(props) where

import Data.Tuple.Extra
import Data.List.Extra
import Control.Monad.Extra
import Util
import Test

props :: [(String, Property)]
props = [("model" ++ show i ++ ": " ++ s, p) | (i,ps) <- zipFrom 0 [prop0, prop1, prop2], (s,p) <- ps]


-- Simple evaluation.
-- Prove (mostly proved in the Test module with assertions):
-- * assuming rule is a total order over the observable keys it doesn't crash
-- * assuming rule returns keys that respect some partial ordering it terminates
-- * prove Need always gets given the right key
model0 :: (k -> Action k v) -> k -> v
model0 rule = run
    where
        run = act run . rule

prop0 = ["always produces the right result" ~~
            \(ValidRule _ r) (Key k) -> model0 r k =%= k
        ]


-- model0 + transient caching
model1 :: Eq k => (k -> Action k v) -> k -> v
model1 rule = fst . flip runCache mempty . run
    where
        run k = cache k $ actM run $ rule k

prop1 = ["equivalent to model0" ~~
            \(ValidRule _ r) (Key k) -> model0 r k == model1 r k
        ,"rule is evaluated at most once per key" ~~
            \(ValidRuleOnce _ r) (Key k) -> model1 r k =%= k
        ]


type Database0 k v = k :-> v

-- model1 + persistent caching
-- Prove:
-- * assumption that rule never changes
-- * question: what changes can be made to rule that still preserve all these properties
model2 :: Eq k => (k -> Action k v) -> k -> Database0 k v -> (v, Database0 k v)
model2 rule = runCache . run
    where
        run k = cache k $ actM run $ rule k

prop2 = ["equivalent to model1 with no state" ~~
            \(ValidRule _ r) (Key k) -> model1 r k == fst (model2 r k mempty)
        ,"equivalent to running with empty database" ~~
            \(ValidRule _ r) (Key k1) (Key k2) ->
                fst (model2 r k2 (snd $ model2 r k1 mempty)) == fst (model2 r k2 mempty)
        ,"rule is run at most once" ~~
            \(ValidRuleOnce _ r) (Key k1) (Key k2) ->
                let res = model2 r k1 mempty in
                fst res =%= k1 && fst (model2 r k2 (snd res)) =%= k2
        ]


type Database1 k v = k :-> (v,[(k,v)])

-- | model2 + visibly changing world
--   A key may either produce a potentially changing value, or have a rule to generate it.
-- Prove:
-- * equivalent result to model1 with no state
-- * equivalent result to running with empty database
-- * rule is not run if all inputs are identical to last time round
-- * question: what changes can be made to rule that still preserve all these properties
model3 :: (Eq k, Eq v) => (k -> Either v (Action k v)) -> k -> Database1 k v -> (v, Database1 k v)
model3 rule k old = second (<> old) $ runCache (run k) mempty
    where
        run k = fmap fst $ cache k $
            case (old ! k, rule k) of
                (_, Left v ) -> pure (v, [])
                (Nothing, Right a) -> actMT run a
                (Just o@(_, ds), Right a) -> do
                    b <- andM [(== dv) <$> run dk | (dk,dv) <- ds]
                    if b then pure o else actMT run a


-- | model3 + computed values change
--   A key may either produce a potentially changing value, or have a rule to generate it.
-- Prove:
-- * equivalent result to running with empty database
-- * equivalent result to model1 with no state
-- * rule is not run if all inputs are identical to last time round
-- * question: what changes can be made to rule that still preserve all these properties
model4 :: (Eq k, Eq v) => (k -> (v, Action k v)) -> k -> Database1 k v -> (v, Database1 k v)
model4 rule k old = second (<> old) $ runCache (run k) mempty
    where
        run k = fmap fst $ cache k $
            case (old ! k, rule k) of
                (Nothing, (_, a)) -> actMT run a
                (Just o@(ov, ds), (v, a)) -> do
                    b <- andM $ pure (v == ov) : [(== dv) <$> run dk | (dk,dv) <- ds]
                    if b then pure o else actMT run a


data DB k v = DB {dbValue :: v, dbBuilt :: T, dbChanged :: T, dbDepends :: [k]}
type Database2 k v = (T, k :-> DB k v)

-- | model4 + optimised storage
-- Prove:
-- * identical to model4, provided values never change then change back
model5 :: (Eq k, Eq v) => (k -> (v, Action k v)) -> k -> Database2 k v -> (v, Database2 k v)
model5 rule k (succ -> t, old) =
        let (v,db) = runCache (run k) mempty
        in (dbValue v, (t, db <> old))
    where
        run k = cache k $
            case (old ! k, rule k) of
                (Nothing, (_, a)) -> do
                    (v,trace) <- actMT (fmap dbValue . run) a
                    pure $ DB v t t (map fst trace)
                (Just o, (v, a)) -> do
                    b <- andM $ pure (v == dbValue o) : [(<= dbBuilt o) . dbChanged <$> run dk | dk <- dbDepends o]
                    if b then pure o else do
                        (v,trace) <- actMT (fmap dbValue . run) a
                        pure $ DB v t (if v == dbValue o then dbChanged o else t) (map fst trace)


-- | model5 + more powerful action
-- Need to show what restrictions are required on rule to make Action1 safe.
-- What changes can be made to rule
-- True means you did need it, False means you used without needing it (so pick up existing state)
model6 :: (Eq k, Eq v) => (k -> (v, Action (Bool,k) v)) -> k -> Database2 k v -> (v, Database2 k v)
model6 rule k (succ -> t, old) =
        let (v,db) = runCache (run k) mempty
        in (dbValue v, (t, db <> old))
    where
        grab (False,k) =
            -- priority: what is in the cache, otherwise what is stored on disk
            maybe (fst $ rule k) dbValue <$> askCache k
        grab (True,k) = dbValue <$> run k

        run k = cache k $
            case (old ! k, rule k) of
                (Nothing, (_, a)) -> do
                    (v,trace) <- actMT grab a
                    pure $ DB v t t (map (snd . fst) trace)
                (Just o, (v, a)) -> do
                    b <- andM $ pure (v == dbValue o) : [(<= dbBuilt o) . dbChanged <$> run dk | dk <- dbDepends o]
                    if b then pure o else do
                        (v,trace) <- actMT grab a
                        pure $ DB v t (if v == dbValue o then dbChanged o else t) (map (snd . fst) trace)
