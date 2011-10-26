{-# LANGUAGE GADTs #-}
module Supercompile.Drive.Process2 () where

import Supercompile.Drive.Match
import Supercompile.Drive.Split
import Supercompile.Drive.Process

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
--import Supercompile.Core.Size
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.Evaluate
import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax

import Supercompile.Termination.Combinators
--import Supercompile.Termination.Extras
--import Supercompile.Termination.TagSet
import Supercompile.Termination.TagBag
--import Supercompile.Termination.TagGraph
import Supercompile.Termination.Generaliser

import Supercompile.StaticFlags
import Supercompile.Utilities hiding (Monad(..))

import Var        (isTyVar, isId)
import Id         (idType, mkLocalId, isDeadBinder, idOccInfo, setIdOccInfo, zapIdOccInfo, zapFragileIdInfo)
import Type       (isUnLiftedType, mkTyVarTy)
import Coercion   (isCoVar, mkCoVarCo)
import Name       (Name, mkSystemVarName)
import FastString (mkFastString)
import CoreUtils  (mkPiTypes)
import qualified State as State
import State hiding (State, mapAccumLM)
import BasicTypes (OccInfo(..))

import Text.XHtml hiding (text)

import qualified Control.Monad as Monad
import Control.Exception (handleJust, AsyncException(UserInterrupt))
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import qualified Data.Map as M
import Data.Monoid
import Data.Ord
import qualified Data.Set as S



import Control.Applicative (Applicative(..), (<$>), liftA2)
import Control.Monad (liftM2)

import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import qualified Data.Traversable as Traversable


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


type ScpM = DelayM (MemoT HistoryM)

traceRenderScpM :: (Outputable a, Monad m) => String -> a -> m ()
traceRenderScpM msg x = pprTraceSC msg (pPrint x) $ return () -- TODO: include depth, refine to ScpM monad only

runScpM :: MemoT HistoryM (ScpM a) -> MemoT HistoryM a
runScpM mx = mx >>= runDelayM eval_strat
  where
    --eval_strat = depthFirst
    eval_strat = breadthFirst


{--}
type HistoryM = (->) (History (State, RollbackScpM))

runHistoryM :: HistoryM a -> a
runHistoryM = flip ($) (mkHistory (cofmap fst wQO))
{--}

{-
type HistoryM = State.State (History (State, RollbackScpM))

runHistoryM :: HistoryM a -> a
runHistoryM = flip State.evalState (mkHistory (cofmap fst wQO))
-}

data Promise = P {
    fun        :: Var,      -- Name assigned in output program
    abstracted :: [AbsVar], -- Abstracted over these variables
    meaning    :: State     -- Minimum adequate term
  }

data MemoState = MS {
    promises :: [Promise], -- FIXME: fulfilments
    hNames   :: Stream Name
  }

newtype MemoT m a = MT { unMT :: MemoState -> m (a, MemoState) }

instance Functor m => Functor (MemoT m) where
    fmap f mx = MT $ \s -> fmap (first f) (unMT mx s)

instance (Functor m, Monad m) => Applicative (MemoT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (MemoT m) where
    return x = MT $ \s -> return (x, s)
    mx >>= fxmy = MT $ \s -> unMT mx s >>= \(x, s) -> unMT (fxmy x) s

runMemoT :: Functor m => MemoT m a -> m a
runMemoT mx = fmap fst $ unMT mx MS { promises = [], hNames = h_names }
  where h_names = listToStream $ zipWith (\i uniq -> mkSystemVarName uniq (mkFastString ('h' : show (i :: Int))))
                                         [1..] (uniqsFromSupply hFunctionsUniqSupply)


promise :: State -> MemoState -> (Promise, MemoState)
promise state ms = (p, ms)
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

instance MonadStatics (DelayM (MemoT HistoryM)) where
    -- FIXME

memo :: (State -> HistoryM (ScpM (Deeds, Out FVedTerm)))
     -> State -> MemoT HistoryM (ScpM (Deeds, Out FVedTerm))
memo opt state = MT $ \ms ->
     -- NB: If tb contains a dead PureHeap binding (hopefully impossible) then it may have a free variable that
     -- I can't rename, so "rename" will cause an error. Not observed in practice yet.
    case [ (p, (releaseStateDeed state, var (fun p) `applyAbsVars` map (renameAbsVar rn_lr) (abstracted p)))
         | p <- promises ms
         , Just rn_lr <- [(\res -> if isNothing res then pprTraceSC "no match:" (ppr (fun p)) res else res) $
                          match (meaning p) state]
         ] of (p, res):_ -> pure $ (do { traceRenderScpM "=sc" (fun p, PrettyDoc (pPrintFullState True state), res)
                                       ; pure res }, ms)
              _          -> opt state >>= \mres -> return
                                   (do { traceRenderScpM ">sc" (fun p, PrettyDoc (pPrintFullState True state))
                                       ; res <- mres
                                       ; traceRenderScpM "<sc" (fun p, PrettyDoc (pPrintFullState False state), res)
                                       ; pure res }, ms')
                where (p, ms') = promise state ms


type RollbackScpM = () -- Generaliser -> ScpBM (Deeds, Out FVedTerm)

sc' :: State -> HistoryM (ScpM (Deeds, Out FVedTerm))
sc' state = \hist -> case terminate hist (state, ()) of
                       Continue hist'             -> split (snd $ reduce state) (delay . sc) -- FIXME: use hist'
                       Stop (shallower_state, ()) -> maybe (split state) id (generalise (mK_GENERALISER shallower_state state) state) (delay . sc)

sc :: State -> MemoT HistoryM (ScpM (Deeds, Out FVedTerm))
sc = memo sc' . gc -- Garbage collection necessary because normalisation might have made some stuff dead


supercompile :: M.Map Var Term -> Term -> Term
supercompile unfoldings e = fVedTermToTerm $ snd $ runHistoryM $ runMemoT $ runScpM $ sc state
  where state = prepareTerm unfoldings e
