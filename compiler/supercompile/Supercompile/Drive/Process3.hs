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
import Supercompile.Evaluator.FreeVars

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

promise :: (State, State) -> MemoState -> (Promise, MemoState)
promise (state, reduced_state) ms = (p, ms')
  where vs_list = stateAbsVars state
        h_name :< h_names' = hNames ms
        x = mkLocalId h_name (vs_list `mkPiTypes` stateType state)
        p = P {
            fun        = x,
            -- We mark as dead any of those variables that are not in the stateLambdaBounders of
            -- the *reduced* state. This serves two purposes:
            --   1. The tieback we do right here can supply dummy values to those parameters rather
            --      than applying the free variables. This may make some bindings above us dead.
            --
            --   2. We can get rid of the code in renameAbsVar that downgrades live AbsVars to dead
            --      ones if they are not present in the renaming: only dead AbsVars are allowed to
            --      be absent in the renaming.
            abstracted = map (\v -> AbsVar { absVarDead = not (v `elemVarSet` stateLambdaBounders reduced_state), absVarVar = v }) vs_list,
            meaning    = reduced_state
          }
        ms' = MS {
            promises = p : promises ms,
            hNames   = h_names'
          }


newtype FulfilmentState = FS {
    fulfilments :: [(Var, FVedTerm)]
  }

fulfill :: Promise -> (Deeds, FVedTerm) -> FulfilmentState -> ((Deeds, FVedTerm), FulfilmentState)
fulfill p (deeds, e_body) fs = ((deeds, fun p `applyAbsVars` abstracted p), FS { fulfilments = (fun p, absVarLambdas (abstracted p) e_body) : fulfilments fs })


type Depth = Int
type StopCount = Int

newtype ScpM a = ScpM { unScpM :: StateT (MemoState, ProcessHistory, FulfilmentState)
                                         (ReaderT (Depth, StopCount, NodeKey, AlreadySpeculated) Identity) a }
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
        (e, (_ms', _hist', fs')) = unI $ unReaderT (unStateT (unScpM me) (ms, hist, fs)) (0, 0, parent, nothingSpeculated)


traceRenderM :: Outputable a => String -> a -> ScpM ()
traceRenderM msg x = ScpM $ StateT $ \s -> ReaderT $ \(depth, _, _, _) -> pprTraceSC (replicate depth ' ' ++ msg) (pPrint x) $ pure ((), s) -- TODO: include depth, refine to ScpM monad only

fulfillM :: Promise -> (Deeds, FVedTerm) -> ScpM (Deeds, FVedTerm)
fulfillM p res = ScpM $ StateT $ \(ms, hist, fs) -> case fulfill p res fs of (res', fs') -> return (res', (ms, hist, fs'))

terminateM :: State -> ScpM a -> (State -> ScpM a) -> ScpM a
terminateM state mcont mstop = ScpM $ StateT $ \(ms, hist, fs) -> ReaderT $ \(depth, stops, parent, already) -> trace ("depth: " ++ show depth) $ case terminate hist (parent, state) of
        Stop (_, shallow_state) -> trace ("stops: " ++ show stops) $
                                   unReaderT (unStateT (unScpM (mstop shallow_state)) (ms, hist,  fs)) (depth + 1, stops + 1, parent,             already) -- FIXME: prevent rollback?
        Continue hist'          -> unReaderT (unStateT (unScpM mcont)                 (ms, hist', fs)) (depth + 1, stops,     generatedKey hist', already)

speculateM :: (StepCount, State) -> ((StepCount, State) -> ScpM a) -> ScpM a
speculateM (steps, state) mcont = ScpM $ StateT $ \s -> ReaderT $ \(depth, stops, parent, already) -> case speculate already (steps, mempty, state) of (already', (steps', _stats, state')) -> unReaderT (unStateT (unScpM (mcont (steps', state'))) s) (depth, stops, parent, already')


sc, sc' :: State -> ScpM (Deeds, FVedTerm)
sc = memo sc' . gc -- Garbage collection necessary because normalisation might have made some stuff dead
sc' state = terminateM state (speculateM (reduceSteps state) $ \(_steps, state) -> split state sc)
                             (\shallow_state -> maybe (trce "split" shallow_state $ split state)
                                                      (trce "gen" shallow_state)
                                                      (generalise (mK_GENERALISER shallow_state state) state)
                                                      sc)
  where
    trce how shallow_state = pprTraceSC ("sc-stop(" ++ how ++ ")") ({- ppr (stateTags shallow_state) <+> text "<|" <+> ppr (stateTags state) $$ -}
                                                                    {- ppr shallow_state $$ -} pPrintFullState {-True-}False shallow_state $$ {- ppr state $$ -} pPrintFullState {-True-}False state)

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

    -- Note [Matching after reduction]
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    --
    -- If we match on States *after* reduction, we might get the following problem. Our initial state could be:
    --
    --  let y = 1; p = (x, y) in snd p
    --
    -- This state has the free variables {x}. However, after reduction+GC it does not have any free variables.
    -- Which set of free variables should we lambda-abstract the h-function over? Well, clearly we have to 
    -- lambda-abstract over the pre-reduction FVs in case that "opt" does not do any reduction and leaves x as a
    -- free variable.
    --
    -- A consequence of this decision is that we have to do something a bit weird at *tieback* site like this one:
    --
    --  let y = 1 in y
    --
    -- To tieback to the h-function for the initial state, we need to supply an x. Luckily, the act of reduction
    -- proves that x is a dead variable and hence we should just be able to supply "undefined".
    case [ (p, (releaseStateDeed state, fun p `applyAbsVars` map (renameAbsVar rn_lr) (abstracted p)))
         | p <- promises ms
         , Just rn_lr <- [-- (\res -> if isNothing res then pprTraceSC "no match:" (ppr (fun p)) res else pprTraceSC "match!" (ppr (fun p)) res) $
                          match (meaning p) reduced_state]
         ] of (p, res):_ -> pure (do { traceRenderM "=sc" (fun p, PrettyDoc (pPrintFullState {-True-}False state), res)
                                     ; return res }, (ms, hist, fs))
              _          -> pure (do { traceRenderM ">sc" (fun p, PrettyDoc (pPrintFullState {-True-}False state))
                                     ; res <- opt state
                                     ; traceRenderM "<sc" (fun p, PrettyDoc (pPrintFullState False state), res)
                                     ; fulfillM p res }, (ms', hist, fs))
                where (p, ms') = promise (state, reduced_state) ms
  where reduced_state = reduce state

supercompile :: M.Map Var Term -> Term -> Term
supercompile unfoldings e = fVedTermToTerm $ runScpM $ liftM snd $ sc state
  where state = prepareTerm unfoldings e
