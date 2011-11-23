{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Supercompile.Drive.Process3 (supercompile) where

import Supercompile.Drive.Match
import Supercompile.Drive.Split
import Supercompile.Drive.Process

import Supercompile.Core.FreeVars
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax

import Supercompile.Termination.TagBag (stateTags)
import Supercompile.Termination.Combinators hiding (generatedKey)
import qualified Supercompile.Termination.Combinators as Combinators

import Supercompile.Utilities

import Id         (mkLocalId)
import Name       (Name, mkSystemVarName)
import FastString (mkFastString)
import CoreUtils  (mkPiTypes)

import Control.Monad (join)

import qualified Data.Map as M
import Data.Monoid (mempty)


{--}
type ProcessHistory = GraphicalHistory (NodeKey, State)

pROCESS_HISTORY :: ProcessHistory
pROCESS_HISTORY = mkGraphicalHistory wQO

generatedKey :: ProcessHistory -> NodeKey
generatedKey = Combinators.generatedKey
{--}
{-
type ProcessHistory = LinearHistory (NodeKey, State)

pROCESS_HISTORY :: ProcessHistory
pROCESS_HISTORY = mkLinearHistory (cofmap snd wQO)

generatedKey :: ProcessHistory -> NodeKey
generatedKey _ = 0
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


newtype FulfilmentState = FS {
    fulfilments :: [(Var, FVedTerm)]
  }

fulfill :: Promise -> (Deeds, FVedTerm) -> FulfilmentState -> ((Deeds, FVedTerm), FulfilmentState)
fulfill p (deeds, e_body) fs = ((deeds, var (fun p) `applyAbsVars` abstracted p), FS { fulfilments = (fun p, tyVarIdLambdas (abstracted p) e_body) : fulfilments fs })


type StopCount = Int

newtype ScpM a = ScpM { unScpM :: StateT (MemoState, ProcessHistory, FulfilmentState)
                                         (ReaderT (StopCount, NodeKey, AlreadySpeculated) Identity) a }
               deriving (Functor, Applicative, Monad)

instance MonadStatics ScpM where
    bindCapturedFloats _fvs mx = liftM ((,) []) mx -- FIXME: do something other than hope for the best
    monitorFVs = liftM ((,) emptyVarSet)

runScpM :: ScpM FVedTerm -> FVedTerm
runScpM me = letRec (fulfilments fs') e
  where h_names = listToStream $ zipWith (\i uniq -> mkSystemVarName uniq (mkFastString ('h' : show (i :: Int))))
                                         [1..] (uniqsFromSupply hFunctionsUniqSupply)
        ms = MS { promises = [], hNames = h_names }
        hist = pROCESS_HISTORY
        fs = FS { fulfilments = [] }
        parent = generatedKey hist
        (e, (_ms', _hist', fs')) = unI $ unReaderT (unStateT (unScpM me) (ms, hist, fs)) (0, parent, nothingSpeculated)


traceRenderM :: Outputable a => String -> a -> ScpM ()
traceRenderM msg x = pprTraceSC msg (pPrint x) $ pure () -- TODO: include depth, refine to ScpM monad only

fulfillM :: Promise -> (Deeds, FVedTerm) -> ScpM (Deeds, FVedTerm)
fulfillM p res = ScpM $ StateT $ \(ms, hist, fs) -> case fulfill p res fs of (res', fs') -> return (res', (ms, hist, fs'))

terminateM :: State -> ScpM a -> (State -> ScpM a) -> ScpM a
terminateM state mcont mstop = ScpM $ StateT $ \(ms, hist, fs) -> ReaderT $ \(stops, parent, already) -> case terminate hist (parent, state) of
        Stop (_, shallow_state) -> trace ("stops: " ++ show stops) $
                                   unReaderT (unStateT (unScpM (mstop shallow_state)) (ms, hist,  fs)) (stops + 1, parent,             already) -- FIXME: prevent rollback?
        Continue hist'          -> unReaderT (unStateT (unScpM mcont)                 (ms, hist', fs)) (stops,     generatedKey hist', already)

speculateM :: State -> (State -> ScpM a) -> ScpM a
speculateM state mcont = ScpM $ StateT $ \s -> ReaderT $ \(stops, parent, already) -> case speculate already (mempty, state) of (already', (_stats, state')) -> unReaderT (unStateT (unScpM (mcont state')) s) (stops, parent, already')


sc, sc' :: State -> ScpM (Deeds, FVedTerm)
sc = memo sc' . gc -- Garbage collection necessary because normalisation might have made some stuff dead
sc' state = terminateM state (speculateM (reduce state) $ \state -> split state sc)
                             (\shallow_state -> maybe (trce "split" shallow_state $ split state)
                                                      (trce "gen" shallow_state)
                                                      (generalise (mK_GENERALISER shallow_state state) state)
                                                      sc)
  where
    trce how shallow_state = pprTraceSC ("sc-stop(" ++ how ++ ")") (ppr (stateTags shallow_state) <+> text "<|" <+> ppr (stateTags state) $$
                                                                    ppr shallow_state $$ pPrintFullState True shallow_state $$ ppr state $$ pPrintFullState True state)

-- Note [Prevent rollback loops by only rolling back when generalising]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- I tried to think about another way to fix rollback.
--  1. If we split <x |-> v[x] | \underbar{x} | > to < | v[x] | update x >
--     .. then we have the invariant that the children of a split are <= size of the parent
--        (we presently don't have this because we duplicate the bodies of heap-bound lambdas)
--  2. Then we can say that the children of any *generalise* have size strictly < that of the parent
--  3. As a result we can recover the termination argument by saying that:
--     a) For any potentially infinite final chain of states all related by "split"/"generalise..
--     b) We can chop it into segments of consecutive "split"s
--       i) Each segment must be of finite length (because of the alpha-renaming-tieback property and
--          the fact that each state in the chain is a syntactic subset of the initial one)
--       ii) There must be a finite number of segments, because each time we generalise we reduce size
--           by at least one, and the intervening splits don't increase it
--       iii) QED (The final chain is finite)
--
-- This is a really beautiful plan. The problem I've found with it is that we can't do 1) because of the
-- problem mentioned in the section about Arjan's idea in the splitter --- i.e. we can do:
--   1. SC (let x = v in x) 
--   2. Reduce to (let x = v in \underbar{x})
--   3. Split to (let x = v in x)
--   4. Tieback to 1), building a worthless loop
--
-- So we should probably work out why the existing supercompiler never builds dumb loops like this, so
-- we can carefully preserve that property when making the Arjan modification.

memo :: (State -> ScpM (Deeds, FVedTerm))
     ->  State -> ScpM (Deeds, FVedTerm) 
memo opt state = join $ ScpM $ StateT $ \(ms, hist, fs) ->
    -- NB: If tb contains a dead PureHeap binding (hopefully impossible) then it may have a free variable that
     -- I can't rename, so "rename" will cause an error. Not observed in practice yet.
    case [ (p, (releaseStateDeed state, var (fun p) `applyAbsVars` map (renameAbsVar rn_lr) (abstracted p)))
         | p <- promises ms
         , Just rn_lr <- [(\res -> if isNothing res then pprTraceSC "no match:" (ppr (fun p)) res else res) $
                          match (meaning p) state]
         ] of (p, res):_ -> pure (do { traceRenderM "=sc" (fun p, PrettyDoc (pPrintFullState True state), res)
                                     ; return res }, (ms, hist, fs))
              _          -> pure (do { traceRenderM ">sc" (fun p, PrettyDoc (pPrintFullState True state))
                                     ; res <- opt state
                                     ; traceRenderM "<sc" (fun p, PrettyDoc (pPrintFullState False state), res)
                                     ; fulfillM p res }, (ms', hist, fs))
                where (p, ms') = promise state ms

supercompile :: M.Map Var Term -> Term -> Term
supercompile unfoldings e = fVedTermToTerm $ runScpM $ liftM snd $ sc state
  where state = prepareTerm unfoldings e
