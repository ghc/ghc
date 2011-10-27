{-# LANGUAGE GADTs #-}
module Supercompile.Drive.Process2 (supercompile) where

import Supercompile.Drive.Match
import Supercompile.Drive.Split
import Supercompile.Drive.Process

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax

import Supercompile.Termination.Combinators

import Supercompile.Utilities

import Id         (mkLocalId)
import Name       (Name, mkSystemVarName)
import FastString (mkFastString)
import CoreUtils  (mkPiTypes)
import qualified State as State

import qualified Data.Map as M


data Stream a = a :< Stream a

listToStream :: [a] -> Stream a
listToStream []     = error "listToStream"
listToStream (x:xs) = x :< listToStream xs


data LeafTy a

data DelayStructure sh f where
    Leaf   :: f a -> DelayStructure (LeafTy a) f
    Branch :: DelayStructure sh1 f -> DelayStructure sh2 f -> DelayStructure (sh1, sh2) f


--newtype I a = I { unI :: a }
newtype QM m a = QM { unQM :: m (DelayM m a) }

-- If you don't want DelayM to have Monad structure, you can nuke the nested use of DelayM,
-- and make some of the consumers simpler. I actually want this generalisation, though.
data DelayM m r = Done r
                | forall sh. Delayed (DelayStructure sh (QM m)) (DelayStructure sh Identity -> DelayM m r)

instance Functor (DelayM m) where
    fmap f x = pure f <*> x

instance Applicative (DelayM m) where
    pure = return
    Done f         <*> Done x         = Done (f x)
    Delayed qs k   <*> Done x         = Delayed qs (\as -> k as <*> Done x)
    Done f         <*> Delayed qs k   = Delayed qs (\as -> Done f <*> k as)
    Delayed qs1 k1 <*> Delayed qs2 k2 = Delayed (Branch qs1 qs2) (\(Branch as1 as2) -> k1 as1 <*> k2 as2)

instance Monad (DelayM m) where
    return = Done
    Done x       >>= fxmy = fxmy x
    Delayed qs k >>= fxmy = Delayed qs (\as -> k as >>= fxmy)

delay :: m (DelayM m a) -> DelayM m a
delay q = Delayed (Leaf (QM q)) (\(Leaf (I a)) -> pure a)

runDelayM :: (Applicative m, Monad m)
          => (DelayM m r -> DelayM m r) -- ^ Chooses the evaluation strategy
          -> DelayM m r -> m r
runDelayM choose_some = go
  where
    go = go' . choose_some
    
    go' (Done x)       = pure x
    go' (Delayed qs k) = mungeDS qs >>= \mx -> go (mx >>= k)

fmapNT :: Applicative m
       => (forall a. f a -> m (g a))
       -> DelayStructure sh f
       -> m (DelayStructure sh g)
fmapNT f (Leaf x)         = fmap Leaf (f x)
fmapNT f (Branch qs1 qs2) = liftA2 Branch (fmapNT f qs1) (fmapNT f qs2)

mungeDS :: Applicative n
        => DelayStructure sh (QM n)
        -> n (DelayM n (DelayStructure sh Identity))
mungeDS = unComp . fmapNT (Comp . fmap (fmap I) . unQM)
{-
mungeDS (Leaf (QM mx))   = fmap (fmap (Leaf . I)) mx
mungeDS (Branch qs1 qs2) = liftA2 (liftA2 Branch) (mungeDS qs1) (mungeDS qs2)
-}

delayDS :: DelayStructure sh (QM n)
        -> DelayM n (DelayStructure sh Identity)
delayDS = fmapNT (fmap I . delay . unQM)
{-
delayDS (Leaf (QM mx))   = fmap (Leaf . I) (delay mx)
delayDS (Branch qs1 qs2) = liftA2 Branch (delayDS qs1) (delayDS qs2)
-}

depthFirst :: DelayM m r -> DelayM m r
depthFirst (Done x)       = Done x
depthFirst (Delayed qs k) = delayTail qs >>= k
  where
    delayTail :: DelayStructure sh (QM m) -> DelayM m (DelayStructure sh Identity)
    delayTail (Leaf (QM q))    = fmap (Leaf . I) (delay q)
    delayTail (Branch qs1 qs2) = liftM2 Branch (delayTail qs1) (delayDS qs2)

breadthFirst :: DelayM m r -> DelayM m r
breadthFirst = id


type ScpM = DelayM (MemoT HistoryThreadM)

traceRenderScpM :: (Outputable a, Monad m) => String -> a -> m ()
traceRenderScpM msg x = pprTraceSC msg (pPrint x) $ return () -- TODO: include depth, refine to ScpM monad only

runScpM :: MemoT HistoryThreadM (ScpM a) -> MemoT HistoryThreadM a
runScpM mx = mx >>= runDelayM eval_strat
  where
    -- Doing things this way prevents GHC bleating about depthFirst being unused
    eval_strat | False     = depthFirst
               | otherwise = breadthFirst


type ProcessHistory = History (State, RollbackScpM)

pROCESS_HISTORY :: ProcessHistory
pROCESS_HISTORY = mkHistory (cofmap fst wQO)

type HistoryEnvM = (->) ProcessHistory

runHistoryEnvM :: HistoryEnvM a -> a
runHistoryEnvM = flip ($) pROCESS_HISTORY

type HistoryThreadM = State.State ProcessHistory

withHistory :: (ProcessHistory -> (ProcessHistory, a)) -> HistoryThreadM a
withHistory f = State.state (swap . f)
  where swap = uncurry (flip (,))

runHistoryThreadM :: HistoryThreadM a -> a
runHistoryThreadM = flip State.evalState pROCESS_HISTORY


newtype StateT s m a = ST { unST :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f mx = ST $ \s -> fmap (first f) (unST mx s)

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (StateT s m) where
    return x = ST $ \s -> return (x, s)
    mx >>= fxmy = ST $ \s -> unST mx s >>= \(x, s) -> unST (fxmy x) s

{-
liftStateT :: Functor m => m a -> StateT s m a
liftStateT mx = ST $ \s -> fmap (flip (,) s) mx
-}

delayStateT :: Functor m => m (StateT s (DelayM m) a) -> StateT s (DelayM m) a
delayStateT mx = ST $ \s -> delay (fmap (($ s) . unST) mx)

{-
-- NB: you can't implement this for all monad transformers
-- (in particular the continuation monad transformer).
-- But if you can, we can derive a delayStateT equivalent from it:
fiddle :: (forall b. m b -> n b)
       -> StateT s m a -> StateT s n a
fiddle f mx = ST $ \s -> f (unST mx s)


lifty :: Monad m => m a -> DelayM m a
lifty = delay . liftM return

mx                                  :: m (StateT s (DelayM m) a)
liftStateT mx                       :: StateT s m (StateT s (DelayM m) a)
fiddle lifty                        :: forall a. Monad m => StateT s m a -> StateT s (DelayM m) a
fiddle lifty (liftStateT mx)        :: Monad m => StateT s (DelayM m) (StateT s (DelayM m) a)
join (fiddle lifty (liftStateT mx)) :: Monad m => StateT s (DelayM m) a

Therefore:

delayStateT :: Monad m => m (StateT s (DelayM m) a) -> StateT s (DelayM m) a
delayStateT = join . fiddle lifty . liftStateT
-}


data Promise = P {
    fun        :: Var,      -- Name assigned in output program
    abstracted :: [AbsVar], -- Abstracted over these variables
    meaning    :: State     -- Minimum adequate term
  }

data MemoState = MS {
    promises :: [Promise],
    hNames   :: Stream Name
  }

type MemoT = StateT MemoState

runMemoT :: Functor m => MemoT m a -> m a
runMemoT mx = fmap fst $ unST mx MS { promises = [], hNames = h_names }
  where h_names = listToStream $ zipWith (\i uniq -> mkSystemVarName uniq (mkFastString ('h' : show (i :: Int))))
                                         [1..] (uniqsFromSupply hFunctionsUniqSupply)


newtype FulfilmentState = FS {
    fulfilments :: [(Var, FVedTerm)]
  }

type FulfilmentT = StateT FulfilmentState

fulfill :: Monad m => Promise -> (Deeds, FVedTerm) -> FulfilmentT m (Deeds, FVedTerm)
fulfill p (deeds, e_body) = ST $ \fs -> return ((deeds, e_body), FS { fulfilments = (fun p, tyVarIdLambdas (abstracted p) e_body) : fulfilments fs })

runFulfilmentT :: Monad m => FulfilmentT m FVedTerm -> m FVedTerm
runFulfilmentT mx = liftM (\(e, fs) -> letRec (fulfilments fs) e) $ unST mx (FS { fulfilments = [] })


promise :: State -> MemoState -> (Promise, MemoState)
promise state ms = (p, ms')
  where vs_list = stateAbsVars state
        h_name :< h_names' = hNames ms
        x = mkLocalId h_name (vs_list `mkPiTypes` stateType state)
        p = P {
            fun        = x,
            abstracted = vs_list,
            meaning    = state
          }
        ms' = MS {
            promises = p : promises ms,
            hNames   = h_names'
          }

instance MonadStatics (FulfilmentT ScpM) where
    --bindCapturedFloats fvs mx | isEmptyVarSet fvs = liftM ((,) []) mx
    --                          | otherwise         = pprPanic "bindCapturedFloats: does not support statics" (ppr fvs)
    bindCapturedFloats _fvs mx = liftM ((,) []) mx -- FIXME: do something other than hope for the best
    monitorFVs = liftM ((,) emptyVarSet)

memo :: (Applicative t, Monad m)
     => (State -> t (FulfilmentT m (Deeds, Out FVedTerm)))
     -> State -> MemoT t (FulfilmentT m (Deeds, Out FVedTerm))
memo opt state = ST $ \ms ->
     -- NB: If tb contains a dead PureHeap binding (hopefully impossible) then it may have a free variable that
     -- I can't rename, so "rename" will cause an error. Not observed in practice yet.
    case [ (p, (releaseStateDeed state, var (fun p) `applyAbsVars` map (renameAbsVar rn_lr) (abstracted p)))
         | p <- promises ms
         , Just rn_lr <- [(\res -> if isNothing res then pprTraceSC "no match:" (ppr (fun p)) res else res) $
                          match (meaning p) state]
         ] of (p, res):_ -> pure $ (do { traceRenderScpM "=sc" (fun p, PrettyDoc (pPrintFullState True state), res)
                                       ; return res }, ms)
              _          -> flip fmap (opt state) $ \mres ->
                                   (do { traceRenderScpM ">sc" (fun p, PrettyDoc (pPrintFullState True state))
                                       ; res <- mres
                                       ; traceRenderScpM "<sc" (fun p, PrettyDoc (pPrintFullState False state), res)
                                       ; fulfill p res }, ms')
                where (p, ms') = promise state ms


type RollbackScpM = () -- Generaliser -> ScpBM (Deeds, Out FVedTerm)

sc' :: State -> HistoryThreadM (FulfilmentT ScpM (Deeds, Out FVedTerm))
sc' state = withHistory $ \hist -> case terminate hist (state, ()) of
              Continue hist'             -> (hist', split (snd $ reduce state) (delayStateT . sc))
              Stop (shallower_state, ()) -> (hist,  maybe (split state) id (generalise (mK_GENERALISER shallower_state state) state) (delayStateT . sc))

sc :: State -> MemoT HistoryThreadM (FulfilmentT ScpM (Deeds, Out FVedTerm))
sc = memo sc' . gc -- Garbage collection necessary because normalisation might have made some stuff dead


supercompile :: M.Map Var Term -> Term -> Term
supercompile unfoldings e = fVedTermToTerm $ runHistoryThreadM $ runMemoT $ runScpM $ liftM (runFulfilmentT . fmap snd) $ sc state
  where state = prepareTerm unfoldings e
