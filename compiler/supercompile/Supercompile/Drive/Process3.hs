{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, ImpredicativeTypes #-}
module Supercompile.Drive.Process3 (supercompile) where

import Supercompile.Drive.Match
import Supercompile.Drive.Split
import Supercompile.Drive.Process

import Supercompile.Core.FreeVars
import Supercompile.Core.Size (fvedTermSize)
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax
import Supercompile.Evaluator.FreeVars

import Supercompile.Termination.Generaliser (Generaliser)
import Supercompile.Termination.TagBag (stateTags)
import Supercompile.Termination.Combinators hiding (generatedKey)
import qualified Supercompile.Termination.Combinators as Combinators

import Supercompile.StaticFlags
import Supercompile.Utilities

import Var        (varName)
import Id         (mkLocalId)
import Name       (Name, mkSystemVarName, getOccString)
import FastString (mkFastString)
import Util       (sndOf3)

import Control.Monad (join)

import Data.Function (on)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Monoid (mempty)


{--}
type ProcessHistory = GraphicalHistory (NodeKey, (String, State, Generaliser -> ScpM ()))

pROCESS_HISTORY :: ProcessHistory
pROCESS_HISTORY = mkGraphicalHistory (cofmap sndOf3 wQO)

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
    meaning    :: State,    -- Minimum adequate term
    dumped     :: Bool      -- Already rolled back, and hence inaccessible?
  }


appendHead :: [b] -> Train (a, [b]) [b] -> Train (a, [b]) [b]
appendHead ys1 (Car (x, ys2) zs) = (x, ys1 ++ ys2) `Car` zs
appendHead ys1 (Loco ys2)        = Loco (ys1 ++ ys2)

leftExtension :: Train (Promise, a) b -- ^ Longer list
              -> Train (Promise, a) b -- ^ Shorter list
              -> Maybe ([(Promise, a)], Train (Promise, a) b) -- Pair of the prefix present in the longer list and the common suffix (== shorter list)
leftExtension xs_train ys_train = case splitBy (trainCars ys_train) (reverse (trainCars xs_train)) of
    -- We can only roll back to direct ancestors, or we risk loops/other madness
    (prefix_rev, Right suffix_rev) | on (==) (map (fun . fst)) (trainCars ys_train) suffix_rev -> Just (reverse prefix_rev, ys_train)
    (_, _) -> Nothing -- pprPanic "leftExtension" (ppr (on (,) (map (fun . fst) . trainCars) xs_train ys_train))


data MemoState = MS {
    promises :: Train (Promise, [Promise]) [Promise], -- (parent, siblings) pairs, with those closest to current level first
    hNames   :: Stream Name
  }

promise :: MemoState -> (State, State) -> (MemoState, Promise)
promise ms (state, reduced_state) = (ms', p)
  where -- NB: because we stopped garbage-collecting in reduceForMatch, we need to garbage
        -- collect here to ensure we mark as dead any lambda binders we won't be able to
        -- determine a renaming for because they are dead.
        --
        -- If we don't do this then renameAbsVar will panic when it tries to lookup the renamed
        -- version of a live variable.
        (vs_list, h_ty) = stateAbsVars (Just (stateLambdaBounders (gc reduced_state))) state
        h_name :< h_names' = hNames ms
        x = mkLocalId h_name h_ty
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
            abstracted = vs_list,
            meaning    = reduced_state,
            dumped     = False
          }
        ms' = MS {
            promises = (p, []) `Car` promises ms, -- Establishes a new level in the process tree
            hNames   = h_names'
          }


newtype FulfilmentState = FS {
    fulfilments :: [(Var, FVedTerm)]
  }

fulfill :: (Deeds, FVedTerm) -> FulfilmentState -> MemoState -> ((Deeds, FVedTerm), FulfilmentState, MemoState)
fulfill (deeds, e_body) fs ms
  = ((deeds, applyAbsVars (fun p) Nothing (abstracted p)),
     FS { fulfilments = (fun p, absVarLambdas (abstracted p) e_body) : fulfilments fs },
     ms { promises = appendHead (p:children) promises' })
  where (p, children) `Car` promises' = promises ms


type StopCount = Int

data ScpState = ScpState {
    scpMemoState :: MemoState,
    scpProcessHistory :: ProcessHistory,
    scpFulfilmentState :: FulfilmentState,
    -- Debugging aids below this line:
    scpResidTags :: ResidTags,
    scpParentChildren :: ParentChildren
  }

data ScpEnv = ScpEnv {
    scpStopCount :: StopCount,
    scpNodeKey :: NodeKey,
    scpParents :: [Var],
    scpAlreadySpeculated :: AlreadySpeculated,
    -- Debugging aids below this line:
    scpTagAnnotations :: TagAnnotations
  }

type ScpResType = (FVedTerm, ScpState)

newtype ScpM a = ScpM { unScpM :: StateT ScpState
                                         (ReaderT ScpEnv (ContT ScpResType Identity)) a }
               deriving (Functor, Applicative, Monad)

instance MonadStatics ScpM where
    bindCapturedFloats _fvs mx = liftM ((,) []) mx -- FIXME: do something other than hope for the best
    monitorFVs = liftM ((,) emptyVarSet)

withScpEnv :: (ScpEnv -> ScpEnv) -> ScpM a -> ScpM a
withScpEnv f mx = ScpM $ StateT $ \s -> ReaderT $ \env -> unReaderT (unStateT (unScpM mx) s) (f env)

runScpM :: TagAnnotations -> ScpM FVedTerm -> FVedTerm
runScpM tag_anns me = fvedTermSize e' `seq` trace ("Deepest path:\n" ++ showSDoc (deepestPath fulfils (scpParentChildren s')) ++
                                                   "\nDepth histogram:\n" ++ showSDoc (depthHistogram (scpParentChildren s'))) e'
  where h_names = listToStream $ zipWith (\i uniq -> mkSystemVarName uniq (mkFastString ('h' : show (i :: Int))))
                                         [1..] (uniqsFromSupply hFunctionsUniqSupply)
        ms = MS { promises = Loco [], hNames = h_names }
        hist = pROCESS_HISTORY
        fs = FS { fulfilments = [] }
        parent = generatedKey hist
        (e, s') = unI $ runContT $ unReaderT (unStateT (unScpM me) (ScpState ms hist fs emptyResidTags emptyParentChildren)) (ScpEnv 0 parent [] nothingSpeculated tag_anns)
        fulfils = fulfilments (scpFulfilmentState s')
        e' = letRec fulfils e


callCCM :: ((a -> ScpM ()) -> ScpM a) -> ScpM a
callCCM act = ScpM $ StateT $ \s -> ReaderT $ \env -> callCC (\jump_back -> unReaderT (unStateT (unScpM (act (\a -> ScpM $ StateT $ \s' -> ReaderT $ \_ -> case s' `rolledBackTo` s of Just s'' -> jump_back (a, s''); Nothing -> trace "rollback failed" $ return ((), s')))) s) env)

catchM :: ((c -> ScpM ()) -> ScpM a) -- ^ Action to try: supplies a function than can be called to "raise an exception". Raising an exception restores the original ScpEnv and ScpState
       -> (c -> ScpM a)              -- ^ Handler deferred to if an exception is raised
       -> ScpM a                     -- ^ Result from either the main action or the handler
catchM try handler = do
    ei_exc_res <- callCCM $ \jump_back -> fmap Right (try (jump_back . Left))
    case ei_exc_res of
      Left exc  -> handler exc
      Right res -> return res

rolledBackTo :: ScpState -> ScpState -> Maybe ScpState
rolledBackTo s' s = flip fmap (on leftExtension (promises . scpMemoState) s' s) $ \(dangerous_promises, ok_promises) ->
  let -- We have to roll back any promise on the "stack" above us:
      (spine_rolled_back, possibly_rolled_back) = (second concat) $ unzip dangerous_promises
      -- NB: rolled_back includes names of both unfulfilled promises rolled back from the stack and fulfilled promises that have to be dumped as a result
      (rolled_fulfilments, rolled_back) = pruneFulfilments (scpFulfilmentState s') (mkVarSet (map fun spine_rolled_back))

      pruneFulfilments :: FulfilmentState -> VarSet -> (FulfilmentState, VarSet)
      pruneFulfilments (FS fulfilments) rolled_back
        | null dump = (if isEmptyVarSet rolled_back then id else pprTraceSC ("dumping " ++ show (sizeVarSet rolled_back) ++ " fulfilments:") (ppr rolled_back))
                      (FS fulfilments, rolled_back)
        | otherwise = pruneFulfilments (FS keep) (rolled_back `unionVarSet` mkVarSet (map fst dump))
        where (dump, keep) = partition (\(_, e) -> fvedTermFreeVars e `intersectsVarSet` rolled_back) fulfilments
  in ScpState {
      scpMemoState = MS {
          -- The most recent promise in s' always has no work done on it, so don't report dumping for it
          promises = appendHead [if fun p `elemVarSet` rolled_back then p { dumped = True } else p | p <- safeTail spine_rolled_back ++ possibly_rolled_back] ok_promises,
          hNames   = hNames (scpMemoState s')
        },
      scpProcessHistory = scpProcessHistory s,
      scpFulfilmentState = rolled_fulfilments,
      scpResidTags      = scpResidTags s', -- FIXME: not totally accurate
      scpParentChildren = scpParentChildren s'
    }

scpDepth :: ScpEnv -> Int
scpDepth = length . scpParents

traceRenderM :: Outputable a => String -> a -> ScpM ()
traceRenderM msg x
  | tRACE     = ScpM $ StateT $ \s -> ReaderT $ \env -> pprTraceSC (replicate (scpDepth env) ' ' ++ msg) (pPrint x) $ pure ((), s)
  | otherwise = return ()

addParentM :: Promise -> (State -> ScpM (Bool, (Deeds, FVedTerm))) -> State -> ScpM (Deeds, FVedTerm)
addParentM p opt state = ScpM $ StateT $ \s -> ReaderT $ add_parent s
  where
    add_parent s env
      | maybe False (scpDepth env >=) dEPTH_LIIMT
      , let (deeds, _statics, e, _gen) = residualiseState state
      = return ((deeds, e), s)
      | otherwise
      = trace ("depth: " ++ show (scpDepth env) ++ ' ' : showSDoc (parens (hsep (map ppr (scpParents env))))) $
        unReaderT (unStateT (unScpM (opt state)) s)
                  (env { scpParents = fun p : scpParents env }) >>= \((gen, res), s') -> return (res, s' { scpParentChildren = addChild (safeHead (scpParents env)) (fun p) (meaning p) gen (scpParentChildren s') })

fulfillM :: (Deeds, FVedTerm) -> ScpM (Deeds, FVedTerm)
fulfillM res = ScpM $ StateT $ \s -> case fulfill res (scpFulfilmentState s) (scpMemoState s) of (res', fs', ms') -> return (res', s { scpFulfilmentState = fs', scpMemoState = ms' })

terminateM :: String -> State -> (Generaliser -> ScpM ()) -> ScpM a -> (String -> State -> (Generaliser -> ScpM ()) -> ScpM a) -> ScpM a
terminateM h state rb mcont mstop = ScpM $ StateT $ \s -> ReaderT $ \env -> case ({-# SCC "terminate" #-} terminate (scpProcessHistory s) (scpNodeKey env, (h, state, rb))) of
        Stop (_, (shallow_h, shallow_state, shallow_rb))
          -> trace ("stops: " ++ show (scpStopCount env)) $
             unReaderT (unStateT (unScpM (mstop shallow_h shallow_state shallow_rb)) s)                                 (env { scpStopCount = scpStopCount env + 1}) -- FIXME: prevent rollback?
        Continue hist'
          -> unReaderT (unStateT (unScpM mcont)                                      (s { scpProcessHistory = hist' })) (env { scpNodeKey = generatedKey hist' })
  -- TODO: record the names of the h-functions on the way to the current one instead of a Int depth

speculateM :: State -> (State -> ScpM a) -> ScpM a
speculateM state mcont = ScpM $ StateT $ \s -> ReaderT $ \env -> case speculate (scpAlreadySpeculated env) (mempty, state) of (already', (_stats, state')) -> unReaderT (unStateT (unScpM (mcont state')) s) (env { scpAlreadySpeculated = already' })


sc :: State -> ScpM (Deeds, FVedTerm)
sc = memo sc' . gc -- Garbage collection necessary because normalisation might have made some stuff dead

sc' :: Maybe String -> State -> ScpM (Bool, (Deeds, FVedTerm))
sc' mb_h state = {-# SCC "sc'" #-} case mb_h of
  Nothing -> speculateM (reduce state) $ \state -> -- traceRenderM "!sc" (PrettyDoc (pPrintFullState quietStatePrettiness state)) >>
                                                   my_split state sc
  Just h  -> flip catchM try_generalise $ \rb ->
               terminateM h state rb
                 (speculateM (reduce state) $ \state -> my_split state sc)
                 (\shallow_h shallow_state shallow_rb -> trce shallow_h shallow_state $
                                                         (if sC_ROLLBACK then (\gen -> shallow_rb gen >> my_split state sc) else try_generalise) ({-# SCC "mK_GENERALISER'" #-} mK_GENERALISER shallow_state state))
  where
    try_generalise gen = maybe (trace "sc-stop(split)" $ my_split state)
                               (trace "sc-stop(gen)")
                               (my_generalise gen state)
                               sc


    -- FIXME: the "could have tied back" case is reachable (e.g. exp3_8 with unfoldings as internal bindings), and it doesn't appear to be
    -- because of dumped promises (no "dumped" in output). I'm reasonably sure this should not happen :(
    trce shallow_h shallow_state = pprTraceSC ("Embedding:" ++ shallow_h)
                                              ({- ppr (stateTags shallow_state) <+> text "<|" <+> ppr (stateTags state) $$ -}
                                               hang (text "Before:") 2 (trce1 shallow_state) $$
                                               hang (text "After:")  2 (trce1 state) $$
                                               (case unMatch (matchWithReason (snd (reduceForMatch shallow_state)) (snd (reduceForMatch state))) of Left why -> text why; Right _ -> text "!!! could have tied back"))
    trce1 state = pPrintFullState quietStatePrettiness state $$ pPrintFullState quietStatePrettiness (snd (reduceForMatch state))

    -- NB: we could try to generalise against all embedded things in the history, not just one. This might make a difference in rare cases.
    my_generalise gen = liftM (\splt -> liftM ((,) True)  . insertTagsM . splt) . generalise gen
    my_split      opt =                 liftM ((,) False) . insertTagsM . split opt

insertTagsM :: ScpM (ResidTags, a, b) -> ScpM (a, b)
insertTagsM mx = do
  (resid_tags, deeds, e') <- mx
  ScpM $ StateT $ \s -> ReaderT $ \env -> let resid_tags' = scpResidTags s `plusResidTags` resid_tags
                                          in trace (tagSummary (scpTagAnnotations env) 1 30 resid_tags' ++ "\n" ++ childrenSummary (scpParentChildren s)) $
                                             return ((), s { scpResidTags = resid_tags' })
  return (deeds, e')

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

memo :: (Maybe String -> State -> ScpM (Bool, (Deeds, FVedTerm)))
     ->  State -> ScpM (Deeds, FVedTerm)
memo opt init_state = {-# SCC "memo'" #-} memo_opt init_state
  where
    memo_opt state
      | Skip <- memo_how = liftM snd $ opt Nothing state
      | otherwise = join $ ScpM $ StateT $ \s ->
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
        --
        -- Note that:
        --  1. Terms that match *after* reduction may not match *before* reduction. This is obvious, and the reason
        --     that we match to reduce before matching in the first place
        --  2. Suprisingly, terms that match *before* reduction may not match *after* reduction! This occurs because
        --     two terms with different distributions of tag may match, but may roll back in different ways in reduce.
        case [ (p, instanceSplit (remaining_deeds, heap_inst, k_inst, applyAbsVars (fun p) (Just rn_lr) (abstracted p)) memo_opt)
             | let (parented_ps, unparented_ps) = trainToList (promises (scpMemoState s))
             , (p, is_ancestor, common_h_vars) <- [ (p_sibling, fun p_parent == fun p_sibling, common_h_vars)
                                                  | (p_parent, p_siblings) <- parented_ps
                                                  , let common_h_vars = case meaning p_parent of (_, Heap h _, _, _) -> M.keysSet h
                                                  , p_sibling <- p_parent:p_siblings ] ++
                                                  [ (p_root,    False,                         S.empty)
                                                  | p_root <- unparented_ps ]
             , let mm = MM { matchInstanceMatching = case () of () | not iNSTANCE_MATCHING -> NoInstances
                                                                   | is_ancestor           -> AllInstances
                                                                   | otherwise             -> InstancesOfGeneralised,
                             matchCommonHeapVars = common_h_vars }
             , Just (heap_inst, k_inst, rn_lr) <- [-- (\res -> if isNothing res then pprTraceSC "no match:" (ppr (fun p)) res   else   pprTraceSC "match!" (ppr (fun p)) res) $
                                                   match' mm (meaning p) reduced_state]
             , let -- This will always succeed because the state had deeds for everything in its heap/stack anyway:
                   Just remaining_deeds = claimDeeds (releaseStateDeed state) (heapSize heap_inst + stackSize k_inst)
               -- FIXME: prefer "more exact" matches
             , if dumped p
                then pprTraceSC "tieback-to-dumped" (ppr (fun p)) False
                else True
             ] of (p, res):_ -> pure (do { traceRenderM "=sc" (fun p, PrettyDoc (pPrintFullState quietStatePrettiness state), PrettyDoc (pPrintFullState quietStatePrettiness reduced_state), PrettyDoc (pPrintFullState quietStatePrettiness (meaning p)) {-, res-})
                                         ; insertTagsM res }, s)
                  _          | CheckOnly <- memo_how
                             -> pure (liftM snd $ opt Nothing state, s)
                             | otherwise
                             -> pure (do { traceRenderM ">sc {" (fun p, stateTags state, PrettyDoc (pPrintFullState quietStatePrettiness state))
                                         ; res <- addParentM p (opt (Just (getOccString (varName (fun p))))) state
                                         ; traceRenderM "<sc }" (fun p, PrettyDoc (pPrintFullState quietStatePrettiness state), res)
                                         ; fulfillM res }, s { scpMemoState = ms' })
                    where (ms', p) = promise (scpMemoState s) (state, reduced_state)
      where (state_did_reduce, reduced_state) = reduceForMatch state
            
            -- The idea here is to prevent the supercompiler from building loops when doing instance matching. Without
            -- this, we tend to do things like:
            --
            --  [a] h0 = D[let f = v in f] = let f = h1 in f
            --      h1 = D[let f = v in v] = h0
            --
            -- This fix is inspired by Peter's supercompiler where matching (and indeed termination checking) is only
            -- performed when the focus of evaluation is the name of a top level function. I haven't yet proved that
            -- this is safe in my setting (FIXME). This might be problematic because Peter does not have a private
            -- history for "reduce" like I do.
            --
            --
            -- Version 2 of this change **prevents** tieback when the focus is an indirection. I'm reasonably
            -- sure this is safe because of the way the splitter is defined and the fact that we have the invariant
            -- that indirections never occur in the stack, and only occur in the heap as the whole HB RHS. This,
            -- combined with the fact that they are acyclic seems to be enough to say that any sequence of splits
            -- only has an indirection in the focus a finite number of times. (FIXME: it's OK that we don't check
            -- the termination condition for those states where the stack is empty since reduce won't change the term.
            -- But it's less clear that it is actually OK to skip termination check when we skip tieback in the other case!!)
            --
            --
            -- Version 3 of this fix is to "eagerly" split values in the splitter: when creating a Bracket for a term,
            -- we split immediately if the term is a value. This is sufficient to fix the problem above, and it should
            -- save even *more* memoisations!
            --
            -- The reason this works is critically dependant on the fact that indirections always point to *manifest values*,
            -- and normalised states have "maximum update frames". If this wasn't the case, we could accidentally build a loop
            -- via this route:
            --      h0 = D[let f = e in f] = let f = h1 in f
            --      h1 = D[e] = h0
            --
            -- FIXME: in fact, generalisation can throw away an outer update frame like this! This doesn't cause problems
            -- because the matcher doesn't use update frames to do instance matching, but it *could* if we changed that...
            --
            --
            -- In version 4 I didn't check for tieback on unreducable states if we eagerly split values.
            -- Because if we don't eagerly split values this can lead to divergence with e.g.
            --   [b] let xs = x:xs in x:xs => let xs = x:xs in x:xs => ...
            -- (NB: this examples is an infinite chain which is entirely irreducible after normalisation)
            --
            -- But then I found that this diverged:
            --   [c] let xs = \k -> k x xs in \k -> k x xs => let xs = \k -> k x xs in k x xs => let xs = \k -> k x xs in \k -> k x xs => ...
            -- And with eager value splitting on, this diverges:
            --   [d] let xs = \k -> k x xs in k x xs => let xs = \k -> k x xs in k x xs => ...
            --
            -- We also have to be careful with examples where intermediate states are reducible, like:
            --   [e] let f = \x -> f x in f x => let f = \x -> f x in \x -> f x => let f = \x -> f x in f x => ...
            -- And with eager value splitting:
            --   [f] let f = \x -> f x in f x => let f = \x -> f x in f x => ...
            --
            -- So version 5 is as follows:
            --  1. If state is already an answer:
            --     a) If the answer is an indirection, skip (just like V2)
            --     b) Otherwise, check and remember (just like V3 and not like V4)
            --  2. If the state is not an answer then proceed like V5:
            --     a) If the state is irreducible, skip (it might make sense to CheckOnly in this case, not sure)
            --     b) If the state is reducible, check and remember
            --
            -- Now [a], [b], [c], and [e] above will converge even without eager value splitting.
            -- In fact, if we turn eager value splitting on the supercompiler will diverge because [d] won't be halted by this version!
            -- So version 5 is incompatible with eager value splitting.
            --
            --
            -- Version 5a is an alternative to version 5 that is usable when eager value splitting is on. In this case:
            --  1. If the state is already an answer, skip tieback (!)
            --  2. Otherwise, check and remember (we can't Skip if the state is reducible because otherwise [f] will diverge)
            --
            -- This can only be used when eagerly splitting values or examples like [b] will diverge (note that [c] will still converge)
            memo_how | dUPLICATE_VALUES_EVALUATOR || not iNSTANCE_MATCHING
                     = CheckAndRemember -- Do the simple thing in this case, it worked great until we introduced instance matching!
    
                     | (_, _, Loco _, qa) <- state -- NB: not safe to use reduced_state!
                     , Answer (_, (_, v)) <- annee qa
                     = case v of Indirect _ -> Skip; _ -> if eAGER_SPLIT_VALUES then Skip else CheckAndRemember
    
                     | otherwise
                     = if state_did_reduce || eAGER_SPLIT_VALUES then CheckAndRemember else Skip

data MemoHow = Skip | CheckOnly | CheckAndRemember

-- NB: don't garbage collect when reducing for a match!
--
-- If you do then you can start with this term:
--   [1] let $dNum = ww3 in * a $dNum
--
-- Looks like this after reduction+GC:
--   [2] case ww3 of Num ...
--
-- And if we reduce+split [1] instead we get:
--   [3] case $dNum of Num ...
--
-- Reducing+GCing [3] term gives us [3] again, and that is alpha equivalent to [2],
-- so we tie back to it rather than continuing. But that means our code is:
--   let h @a ww3 = let $dNum = ww3
--                  in h a $dNum
--
-- Which is a loop. So we need to do one of:
--  1. Not GC before matching
--  2. GC *after* reduction in the main codepath.
--  3. Not eliminate dead update frames when GCing
reduceForMatch :: State -> (Bool, State)
reduceForMatch state = {- second gc $ -} reduceWithFlag (case state of (_, h, k, e) -> (maxBound, h, k, e)) -- Reduce ignoring deeds for better normalisation

supercompile :: M.Map Var Term -> Term -> Term
supercompile unfoldings e = fVedTermToTerm $ start (liftM snd . sc)
  where (bvs_unfoldings, (to_bind, state), (preinit_with, preinit_state)) = prepareTerm unfoldings e
        start k | pREINITALIZE_MEMO_TABLE = run $ preinitalise preinit_with >> withScpEnv (\e -> e { scpAlreadySpeculated = bvs_unfoldings `S.union` scpAlreadySpeculated e }) (k preinit_state)
                | otherwise               = bindManyMixedLiftedness fvedTermFreeVars to_bind $ run $ k state
        run = runScpM (tagAnnotations state)

preinitalise :: [(State, FVedTerm)] -> ScpM ()
preinitalise states_fulfils = forM_ states_fulfils $ \(state, e') -> do
    ScpM $ StateT $ \s -> do
        let (ms', _p) = promise (scpMemoState s) (state, snd (reduceForMatch state))
        return ((), s { scpMemoState = ms' })
    fulfillM (emptyDeeds, e')
