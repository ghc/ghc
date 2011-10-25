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



import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (liftM2)

import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import qualified Data.Traversable as Traversable


data Stream a = a :< Stream a

listToStream :: [a] -> Stream a
listToStream []     = error "listToStream"
listToStream (x:xs) = x :< listToStream xs


data DelayStructure sh a where
    Leaf :: a -> DelayStructure () a
    Branch :: DelayStructure sh1 a -> DelayStructure sh2 a -> DelayStructure (sh1, sh2) a

instance Show a => Show (DelayStructure sh a) where
    show (Leaf x) = "Leaf (" ++ show x ++ ")"
    show (Branch t1 t2) = "Branch (" ++ show t1 ++ ") (" ++ show t2 ++ ")"

instance Functor (DelayStructure sh) where
    fmap = Traversable.fmapDefault

instance Foldable (DelayStructure sh) where
    foldMap = Traversable.foldMapDefault

instance Traversable (DelayStructure sh) where
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Branch t1 t2) = Branch <$> traverse f t1 <*> traverse f t2


-- If you don't want DelayM to have Monad structure, you can nuke the nested use of DelayM,
-- and make some of the consumers simpler. I actually want this generalisation, though.
data DelayM q a r = Done r
                  | forall sh. Delayed (DelayStructure sh q) (DelayStructure sh a -> DelayM q a r)

instance Functor (DelayM q a) where
    fmap f x = pure f <*> x

instance Applicative (DelayM q a) where
    pure = return
    Done f         <*> Done x         = Done (f x)
    Delayed qs k   <*> Done x         = Delayed qs (\as -> k as <*> Done x)
    Done f         <*> Delayed qs k   = Delayed qs (\as -> Done f <*> k as)
    Delayed qs1 k1 <*> Delayed qs2 k2 = Delayed (Branch qs1 qs2) (\(Branch as1 as2) -> k1 as1 <*> k2 as2)

instance Monad (DelayM q a) where
    return = Done
    Done x       >>= fxmy = fxmy x
    Delayed qs k >>= fxmy = Delayed qs (\as -> k as >>= fxmy)

delay :: q -> DelayM q a a
delay q = Delayed (Leaf q) (\(Leaf a) -> pure a)

runDelayM :: (Applicative memom, Monad memom)
          => (DelayM q a r -> DelayM q a r) -- ^ Chooses the evaluation strategy
          -> (q -> memom (DelayM q a a))    -- ^ How to answer questions in the monad (possibly generating new requests in the process)
          -> DelayM q a r -> memom r
runDelayM choose_some sc = go
  where
    go = go' . choose_some
    
    go' (Done x)       = pure x
    go' (Delayed qs k) = traverse sc qs >>= \fs -> go (sequenceA fs >>= k)


depthFirst :: DelayM q a r -> DelayM q a r
depthFirst (Done x)       = Done x
depthFirst (Delayed qs k) = delayTail qs >>= k
  where
    delayTail :: DelayStructure sh q -> DelayM q a (DelayStructure sh a)
    delayTail (Leaf q)         = fmap Leaf (delay q)
    delayTail (Branch qs1 qs2) = liftM2 Branch (delayTail qs1) (traverse delay qs2)

breadthFirst :: DelayM q a r -> DelayM q a r
breadthFirst = id


type ScpM = DelayM State (Deeds, FVedTerm)

traceRenderScpM :: (Outputable a, Monad m) => String -> a -> m ()
traceRenderScpM msg x = pprTraceSC msg (pPrint x) $ return () -- TODO: include depth, refine to ScpM monad only

runScpM :: (Applicative m, Monad m)
        => (State -> m (ScpM (Deeds, FVedTerm)))
        -> m (ScpM a) -> m a
runScpM sc mx = mx >>= runDelayM eval_strat sc
  where
    --eval_strat = depthFirst
    eval_strat = breadthFirst


newtype HistoryT m a = HT { unHT :: History (State, RollbackScpM) -> m a }

instance Functor m => Functor (HistoryT m) where
    fmap f mx = HT $ \hist -> fmap f (unHT mx hist)

instance Applicative m => Applicative (HistoryT m) where
    pure x = HT $ \_ -> pure x
    mf <*> mx = HT $ \hist -> unHT mf hist <*> unHT mx hist

instance Monad m => Monad (HistoryT m) where
    return x = HT $ \_ -> return x
    mx >>= fxmy = HT $ \hist -> unHT mx hist >>= \x -> unHT (fxmy x) hist


data Promise = P {
    fun        :: Var,      -- Name assigned in output program
    abstracted :: [AbsVar], -- Abstracted over these variables
    meaning    :: State     -- Minimum adequate term
  }

data MemoState = MS {
    promises :: [Promise], -- FIXME: fulfilments
    hNames   :: Stream Name
  }

type MemoM = State.State MemoState

runMemoM :: MemoM a -> a
runMemoM = flip evalState $ MS { promises = [], hNames = h_names }
  where h_names = listToStream $ zipWith (\i uniq -> mkSystemVarName uniq (mkFastString ('h' : show (i :: Int))))
                                         [1..] (uniqsFromSupply hFunctionsUniqSupply)

{-
newtype MemoM a = MemoM { unMemoM :: MemoState -> (MemoState, a) }

instance Functor MemoM where
    fmap = liftM

instance Applicative MemoM where
    pure = return
    (<*>) = ap

instance Monad MemoM where
    return x = MemoM $ \s -> (s, x)
    MemoM xf >>= fxmy = MemoM $ \s -> case xf s of (s', x) -> unMemoM (fxmy x) s'

modify :: (MemoState -> (MemoState, a))
       -> MemoM a
modify = MemoM
-}

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

memo :: (State -> ScpM (Deeds, Out FVedTerm))
     -> State -> MemoM (ScpM (Deeds, Out FVedTerm))
memo opt state = State.state $ \ms ->
     -- NB: If tb contains a dead PureHeap binding (hopefully impossible) then it may have a free variable that
     -- I can't rename, so "rename" will cause an error. Not observed in practice yet.
    case [ (p, (releaseStateDeed state, var (fun p) `applyAbsVars` map (renameAbsVar rn_lr) (abstracted p)))
         | p <- promises ms
         , Just rn_lr <- [(\res -> if isNothing res then pprTraceSC "no match:" (ppr (fun p)) res else res) $
                          match (meaning p) state]
         ] of (p, res):_ -> (do { traceRenderScpM "=sc" (fun p, PrettyDoc (pPrintFullState True state), res)
                                ; pure res }, ms)
              _          -> (do { traceRenderScpM ">sc" (fun p, PrettyDoc (pPrintFullState True state))
                                ; res <- opt state
                                ; traceRenderScpM "<sc" (fun p, PrettyDoc (pPrintFullState False state), res)
                                ; pure res }, ms')
                where (p, ms') = promise state ms


type RollbackScpM = () -- Generaliser -> ScpBM (Deeds, Out FVedTerm)

sc' :: State -> HistoryT st ScpM (Deeds, Out FVedTerm)
sc' state = error "FIXME"

sc :: State -> HistoryT st MemoM (ScpM (Deeds, Out FVedTerm))
sc = memo sc' . gc -- Garbage collection necessary because normalisation might have made some stuff dead


supercompile :: M.Map Var Term -> Term -> Term
supercompile unfoldings e = fVedTermToTerm $ snd $ runMemoM $ runHistoryT $ runScpM sc $ sc state
  where state = prepareTerm unfoldings e
