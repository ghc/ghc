{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, ImpredicativeTypes #-}
module Supercompile.Drive.Process3 (supercompile) where

--import Supercompile.Drive.Match
import Supercompile.Drive.MSG
import Supercompile.Drive.Split2
import Supercompile.Drive.Process

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
import Supercompile.Core.Size (fvedTermSize)
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax
import Supercompile.Evaluator.FreeVars

--import Supercompile.Termination.Generaliser (Generaliser)
import Supercompile.Termination.TagBag (stateTags)
import Supercompile.Termination.Combinators hiding (generatedKey)
import qualified Supercompile.Termination.Combinators as Combinators

import Supercompile.StaticFlags
import Supercompile.Utilities

import Type       (typeSize, isTyVarTy)
import Coercion   (coercionSize, getCoVar_maybe)
import Var        (varName)
import Id         (Id, mkLocalId)
import MkId       (nullAddrId)
import Name       (Name, mkSystemVarName, getOccString)
import FastString (mkFastString)
import Util       (sndOf3)
import Pair
import VarEnv     (varEnvElts)

import Control.Monad (join)

import Data.Function (on)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Monoid (mempty)


{--}
type RollbackState = ScpM (ResidTags, Deeds, Out FVedTerm)
type ProcessHistory = GraphicalHistory (NodeKey, (String, State, RollbackState -> ScpM ()))

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
leftExtension = trainLeftExtensionBy (\orig@(p1, _) (p2, _) -> if fun p1 == fun p2 then Just orig else Nothing) (\b1 _b2 -> Just b1)
    -- We can only roll back to direct ancestors, or we risk loops/other madness


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
     refulfill e_body fs p,
     ms { promises = appendHead (p:children) promises' })
  where (p, children) `Car` promises' = promises ms

-- NB: we filter out from the existing fulfilments because if we are doing type generalisation during
-- matching then we will fulfill the same promise twice. Either:
--  1. We fulfil an on-stack promise from the type generalisation, and then later when unrolling the stack
--  2. OR we fulfil a promise from supercompilation, and then later overwrite it when we find a type generalisation
--
-- FIXME: should prefer the existing promise in case 1 for slightly better code.
refulfill :: FVedTerm -> FulfilmentState -> Promise -> FulfilmentState
refulfill e_body fs p = FS { fulfilments = (fun p, absVarLambdas (abstracted p) e_body) : filter ((/= fun p) . fst) (fulfilments fs) }


type StopCount = Int

data ScpState = ScpState {
    scpMemoState :: MemoState,
    scpProcessHistoryState :: ProcessHistory,
    scpFulfilmentState :: FulfilmentState,
    -- Debugging aids below this line:
    scpResidTags :: ResidTags,
    scpParentChildren :: ParentChildren
  }

data ScpEnv = ScpEnv {
    scpProcessHistoryEnv :: ProcessHistory,
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

{-
instance MonadStatics ScpM where
    bindCapturedFloats _fvs mx = liftM ((,) []) mx -- FIXME: do something other than hope for the best
    monitorFVs = liftM ((,) emptyVarSet)
-}

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
        (e, s') = unI $ runContT $ unReaderT (unStateT (unScpM me) (ScpState ms hist fs emptyResidTags emptyParentChildren)) (ScpEnv hist 0 parent [] nothingSpeculated tag_anns)
        fulfils = fulfilments (scpFulfilmentState s')
        e' = letRec fulfils e


outputFreeVars :: ScpM [Id]
outputFreeVars = ScpM $ StateT $ \s -> let (pss, ps) = trainToList (promises (scpMemoState s))
                                       in return (varSetElems extraOutputFvs ++ concatMap (\(p, ps) -> fun p : map fun ps) pss ++ map fun ps, s)

callCCM :: ((a -> ScpM ()) -> ScpM a) -> ScpM a
callCCM act = ScpM $ StateT $ \s -> ReaderT $ \env -> callCC (\jump_back -> unReaderT (unStateT (unScpM (act (\a -> ScpM $ StateT $ \s' -> ReaderT $ \_ -> case s' `rolledBackTo` s of Just s'' -> jump_back (a, s''); Nothing -> return ((), s')))) s) env)

catchM :: ((c -> ScpM ()) -> ScpM a) -- ^ Action to try: supplies a function than can be called to "raise an exception". Raising an exception restores the original ScpEnv and ScpState
       -> (c -> ScpM a)              -- ^ Handler deferred to if an exception is raised
       -> ScpM a                     -- ^ Result from either the main action or the handler
catchM try handler = do
    ei_exc_res <- callCCM $ \jump_back -> fmap Right (try (jump_back . Left))
    case ei_exc_res of
      Left exc  -> handler exc
      Right res -> return res

rolledBackTo :: ScpState -> ScpState -> Maybe ScpState
rolledBackTo s' s = case on leftExtension (promises . scpMemoState) s' s of
 Nothing -> pprTrace "rollback failed" (on (curry ppr) (fmapTrain (map fun . uncurry (:)) (map fun) . promises . scpMemoState) s' s) Nothing
 Just (dangerous_promises, ok_promises) -> Just $
  let -- We have to roll back any promise on the "stack" above us:
      (spine_rolled_back, possibly_rolled_back) = (second concat) $ unzip dangerous_promises
      -- NB: rolled_back includes names of both unfulfilled promises rolled back from the stack and fulfilled promises that have to be dumped as a result
      (rolled_fulfilments, rolled_back) = pruneFulfilments (scpFulfilmentState s') (mkVarSet (map fun spine_rolled_back))

      pruneFulfilments :: FulfilmentState -> VarSet -> (FulfilmentState, VarSet)
      pruneFulfilments (FS fulfilments) rolled_back
        | null dump = (if isEmptyVarSet rolled_back then id else pprTraceSC ("dumping " ++ show (sizeVarSet rolled_back) ++ " promises/fulfilments:") (ppr (map fun spine_rolled_back, rolled_back)))
                      (FS fulfilments, rolled_back)
        | otherwise = pruneFulfilments (FS keep) (rolled_back `unionVarSet` mkVarSet (map fst dump))
        where (dump, keep) = partition (\(_, e) -> fvedTermFreeVars e `intersectsVarSet` rolled_back) fulfilments
  in ScpState {
      scpMemoState = MS {
          -- The most recent promise in s' always has no work done on it, so don't report dumping for it
          promises = appendHead [if fun p `elemVarSet` rolled_back then p { dumped = True } else p | p <- safeTail spine_rolled_back ++ possibly_rolled_back] ok_promises,
          hNames   = hNames (scpMemoState s')
        },
      scpProcessHistoryState = scpProcessHistoryState s,
      scpFulfilmentState     = rolled_fulfilments,
      scpResidTags           = scpResidTags s', -- FIXME: not totally accurate
      scpParentChildren      = scpParentChildren s'
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

terminateM :: String -> State -> (RollbackState -> ScpM ()) -> ScpM a -> (String -> State -> (RollbackState -> ScpM ()) -> ScpM a) -> ScpM a
terminateM h state rb mcont mstop = ScpM $ StateT $ \s -> ReaderT $ \env -> case ({-# SCC "terminate" #-} terminate (if hISTORY_TREE then scpProcessHistoryEnv env else scpProcessHistoryState s) (scpNodeKey env, (h, state, rb))) of
        Stop (_, (shallow_h, shallow_state, shallow_rb))
          -> trace ("stops: " ++ show (scpStopCount env)) $
             unReaderT (unStateT (unScpM (mstop shallow_h shallow_state shallow_rb)) s)                                      (env { scpStopCount = scpStopCount env + 1}) -- FIXME: prevent rollback?
        Continue hist'
          -> unReaderT (unStateT (unScpM mcont)                                      (s { scpProcessHistoryState = hist' })) (env { scpNodeKey = generatedKey hist', scpProcessHistoryEnv = hist' })
  -- TODO: record the names of the h-functions on the way to the current one instead of a Int depth

speculateM :: State -> (State -> ScpM a) -> ScpM a
speculateM state mcont = ScpM $ StateT $ \s -> ReaderT $ \env -> case speculate (scpAlreadySpeculated env) (mempty, state) of (already', (_stats, state')) -> unReaderT (unStateT (unScpM (mcont state')) s) (env { scpAlreadySpeculated = already' })


sc :: State -> ScpM (Deeds, FVedTerm)
sc = memo sc' . gc -- Garbage collection necessary because normalisation might have made some stuff dead

sc' :: Maybe String -> State -> ScpM (Bool, (Deeds, FVedTerm)) -- Bool records whether generalisation occurred, for debug printing
sc' mb_h state = pprTrace "sc'" (trce1 state) $ {-# SCC "sc'" #-} case mb_h of
  Nothing -> speculateM (reduce state) $ \state -> -- traceRenderM "!sc" (PrettyDoc (pPrintFullState quietStatePrettiness state)) >>
                                                   my_split state
  Just h  -> flip catchM try_generalise $ \rb ->
               terminateM h state rb
                 (speculateM (reduce state) $ \state -> my_split state)
                 (\shallow_h shallow_state shallow_rb -> trce shallow_h shallow_state $ do
                                                           let gen = fmap (trace "sc-stop(msg)") (tryMSG sc shallow_state state) `mplus`
                                                                     fmap (trace "sc-stop(tag)") (tryTaG sc shallow_state state) `orElse`
                                                                          trace "sc-stop(split)" (split sc shallow_state, split sc state)
                                                           when sC_ROLLBACK $ shallow_rb (fst gen)
                                                           try_generalise (snd gen))
  where
    try_generalise gen = my_generalise gen

    -- FIXME: the "could have tied back" case is reachable (e.g. exp3_8 with unfoldings as internal bindings), and it doesn't appear to be
    -- because of dumped promises (no "dumped" in output). I'm reasonably sure this should not happen :(
    trce shallow_h shallow_state = pprTraceSC ("Embedding:" ++ shallow_h)
                                              ({- ppr (stateTags shallow_state) <+> text "<|" <+> ppr (stateTags state) $$ -}
                                               hang (text "Before:") 2 (trce1 shallow_state) $$
                                               hang (text "After:")  2 (trce1 state) $$
                                               (case msg (MSGMode { msgCommonHeapVars = emptyInScopeSet }) (snd (reduceForMatch shallow_state)) (snd (reduceForMatch state)) of Left why -> text why; Right res -> case msgMatch AllInstances res of Nothing -> text "msg, not instance"; Just _ -> text "!!! could have tied back?"))
    trce1 state = pPrintFullState quietStatePrettiness state $$ pPrintFullState quietStatePrettiness (snd (reduceForMatch state))

    -- NB: we could try to generalise against all embedded things in the history, not just one. This might make a difference in rare cases.
    my_generalise splt  = liftM ((,) True)  $ splt           >>= insertTagsM
    my_split      state = liftM ((,) False) $ split sc state >>= insertTagsM

tryTaG, tryMSG :: (State -> ScpM (Deeds, Out FVedTerm))
               -> State -> State
               -> Maybe (ScpM (ResidTags, Deeds, Out FVedTerm),
                         ScpM (ResidTags, Deeds, Out FVedTerm))
tryTaG opt shallow_state state = bothWays (\_ -> generaliseSplit opt gen) shallow_state state
  where gen = mK_GENERALISER shallow_state state

tryMSG opt = bothWays $ \shallow_state state -> do
  -- FIXME: better? In particular, could rollback and then MSG
  (Pair _ (deeds_r, heap_r@(Heap h_r ids_r), rn_r, k_r), (heap@(Heap _ ids), k, qa)) <- msgMaybe (MSGMode { msgCommonHeapVars = case shallow_state of (_, Heap _ ids, _, _) -> ids }) shallow_state state
  let [deeds, deeds_r'] = splitDeeds deeds_r [heapSize heap + stackSize k + annedSize qa, heapSize heap_r + stackSize k_r]
  pprTrace "MSG success" (pPrintFullState quietStatePrettiness (deeds, heap, k, qa) $$
                          pPrintFullState quietStatePrettiness (deeds_r', heap_r, k_r, fmap Question (annedVar (mkTag 0) nullAddrId))) $ Just $ do
    (deeds', e) <- sc (deeds, heap, k, qa)
    -- Just to suppress warnings from renameId (since output term may mention h functions). Alternatively, I could rename the State I pass to "sc"
    -- NB: adding some new bindings to h_r for the h functions is a bit of a hack because:
    --  1. It only serves to suppress errors from "split" which occur when e' refers to some variables not bound in the heap
    --  2. These new dummy bindings will never be passed down to any recursive invocation of opt
    (h_hs, e') <- renameSCResult ids (rn_r, e)
    instanceSplit opt (deeds' `plusDeeds` deeds_r', Heap (h_r `M.union` h_hs) ids_r, k_r, e')

renameSCResult :: InScopeSet -> In FVedTerm -> ScpM (PureHeap, FVedTerm)
renameSCResult ids (rn_r, e) = do
    hs <- outputFreeVars
    let rn_r' = foldr (\x rn -> insertIdRenaming rn x x) rn_r hs
        h_r'  = foldr (\x h  -> M.insert x lambdaBound h) M.empty hs
    return (h_r', renameFVedTerm ids rn_r' e)

-- NB: in the case of both callers, if "f" fails one way then it fails the other way as well (and likewise for success).
-- Therefore we can return a single Maybe rather than a pair of two Maybes.
bothWays :: (a -> a -> Maybe b)
         -> a -> a -> Maybe (b, b)
bothWays f shallow_state state = zipMaybeWithEqual "bothWays" (,) (f state shallow_state) (f shallow_state state)

insertTagsM :: (ResidTags, a, b) -> ScpM (a, b)
insertTagsM (resid_tags, deeds, e') =
  ScpM $ StateT $ \s -> ReaderT $ \env -> let resid_tags' = scpResidTags s `plusResidTags` resid_tags
                                          in trace (tagSummary (scpTagAnnotations env) 1 30 resid_tags' ++ "\n" ++ childrenSummary (scpParentChildren s)) $
                                             return ((deeds, e'), s { scpResidTags = resid_tags' })

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

-- FIXME: use MSG while matching to detect opportunities to do type despecialisation
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

        -- Note [Instance matching and loops]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        --
        -- If we aren't careful, instance matching can easily cause the supercompiler to loop. What if we have this tieback:
        --
        --   \_ -> f x
        --
        -- And we are asked to supercompile:
        --
        --   let a = \_ -> f a
        --   in \_ -> f a
        --
        -- If we aren't careful, we will detect an instance match, and hence recursively supercompile the "remaining" term:
        --
        --   let a = \_ -> f a
        --   in \_ -> f a
        --
        -- We ran into a very similar situation in practice with exp3_8, where the recursive structure was actually an instance
        -- dictionary which contained several methods (lambdas), each of which selected methods from that same dictionary. i.e.
        -- we had something like this in the memo table:
        --
        --  let $cnegate = \x y -> ...[$fNumNat]
        --      $c+ = \x y -> ...[$fNumNat]
        --      $fNumNat = D:Num Nat $cnegate $c+
        --  in $c+ x y
        --
        -- And we were compiling:
        --
        --  let $cnegate = \x y -> ...[$fNumNat]
        --      $c+ = \x y -> ...[$fNumNat]
        --      $fNumNat = D:Num Nat $cnegate $c+
        --      a = fromInteger Nat $fNumNat c
        --      b = negate Nat $fNumNat d
        --      c = __integer 0
        --  in $c+ a b
        --
        -- So one of the "remaining" terms was:
        --
        --  let $cnegate = \x y -> ...[$fNumNat]
        --      $c+ = \x y -> ...[$fNumNat]
        --      $fNumNat = D:Num Nat $cnegate $c+
        --  in negate Nat $fNumNat d
        --
        -- And after inlining of the negate selector+body we just get a call to ($c+) which is identical to what we began with,
        -- since in the class defaults (negate x) is defined as (0 - x) and (a - b) is defined as (a + negate b).
        --
        -- Of course, this particular program will most likely cause a runtime loop, but the compiler shouldn't diverge too!
        --
        -- I think a reasonable fix is twofold:
        --  1. Record instance matches in the memotable (!). Still no need to do it for truly *exact* matches.
        --  2. When tying back, always prefer exact matches over instance matches.
        --     This is something I want to do anyway because we should also be able to detect instance matches that are strictly
        --     better than other possible instance matches, and we should prefer those as well with the same code.
        --
        -- BUT we have to be so so careful if we do this that we don't introduce accidental extra loops.

        -- Note [Type generalisation and rollback]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        --
        -- When doing type generalisation, we can end up in the situation where a fulfilled promise in another part of the tree
        -- refers to an as-yet unfulfilled promise we place newly onto the stack. If we then rollback that promise, what do we
        -- do with that overwritten fulfilment?
        --
        -- This problem doesn't occur without type generalisation because the h functions mentioned freely by the fulfilments/
        -- partial fulfilments (latent on the stack) at the time we made the promise we are rolling back to can *only* mention
        -- promises that we already made at that time!
        --
        -- WITH type generalisation we go and modify those existing fulfilments to point into the portion of the stack which
        -- is in danger of rollback.
        -- (A similar problem would occur if we messed about with previous fulfilments when we detected a MSG opportunity.)
        --
        -- A formalisation of this idea is below:
        --
        -- Imagine there is a quantity "time" which ticks forward by at least 1 unit whenever a new promise{-/fulfilment-} occurs. Call:
        --  * tp(h) the time at which h was promised
        -- {-
        --  * tf(h) the time at which h was fulfilled
        -- -}
        --  * fvs(h)(t) the h-function FVs for the fulfilment for h as of time t
        --
        -- Then we have:
        --  1. P-before-F-use: if h' \elem fvs(h)(t) then tp(h') <= t
        -- {-
        --  2. Monotonicity: fvs(h)(t) `subVarSet` fvs(h)(t+1)
        --  3. Fix: if t >= tf(h) then fvs(h)(t) = fvs(h)(tf(h))
        --
        -- Due to monotonicity and fix, we can usefully define fvs(h) = fvs(h)(tf(h))
        -- where if h' \elem fvs(h)(t) then h' \elem fvs(h)
        --
        --  3. P-before-F: tp(h) < tf(h)
        --  4. Depth-first: if h' \elem fvs(h) and tf(h') > tf(h) then tp(h') < tp(h)
        -- 
        -- We can derive a simple lemma immediately:
        --  3. Dependent-P-before-F: if h' \elem fvs(h) then tp(h') < tf(h)
        --     Proof: by cases on tf(h') `compare` tf(h):
        --      Case (==): then h == h' and by P-before-F, (tp(h') = tp(h)) < tf(h)
        --      Case (<): using P-before-F, tp(h') < tf(h') < tf(h)
        --      Case (>): tf(h') > tf(h) so using depth-first and P-before-F, tp(h') < tp(h) < tf(h)
        -- -}
        --
        -- Rollback to tp(h) from time t (tp(h) < t, tf(h) > t) relies on:
        --  forall h'. tp(h') < tp(h) ==> if h'' \elem fvs(h')(t) then tp(h'') <= tp(h)
        -- Because h' is *not* on the top of the stack at t, we know that we don't *add* any new FVs to it:
        --  fvs(h')(t) `subVarSet` fvs(h')(tp(h))
        -- (In fact it will be completely invariant, but it is more general to insist only on shrinkage). It follows that since:
        --  h'' \elem fvs(h')(t)
        -- Then by the subset relation:
        --  h'' \elem fvs(h')(tp(h))
        -- And so by P-before-F-use:
        --  tp(h'') <= tp(h)
        -- Which is what we wanted.
        --
        --
        -- All that said, what are we going to do about this problem?
        --  * It would be safe to only do type generalisation if the older promise is on the stack, because in that case
        --    we could easily code it to only add normal fulfilments to the state if one isn't already present, and it wouldn't
        --    cause any rollback issues
        --  * But if it's on the stack anyway, we would get better results by just rolling back immediately with the normal MSG
        --    doing the work of dumping the type information for us (albeit this will likely immediately terminate b/c types aren't tagged)
        --
        -- I think the simplest thing to do is just treat it as a normal instance match, and not worry about overwriting the older code!

        -- FIXME: this code is all super-horrible how
        let remember what = (do { traceRenderM ">sc {" (fun p, stateTags state, PrettyDoc (pPrintFullState quietStatePrettiness state))
                                ; res <- addParentM p (what (Just (getOccString (varName (fun p))))) state
                                ; traceRenderM "<sc }" (fun p, PrettyDoc (pPrintFullState quietStatePrettiness state), res)
                                ; fulfillM res }, s { scpMemoState = ms' })
              where (ms', p) = promise (scpMemoState s) (state, reduced_state)
        in case fmap (\(exact, (p, mr)) -> (exact, case mr of
                       RightIsInstance (Heap h_inst ids_inst) rn_lr k_inst -> do { traceRenderM ("=sc" ++ if exact then "" else "(inst)") (fun p, PrettyDoc (pPrintFullState quietStatePrettiness state), PrettyDoc (pPrintFullState quietStatePrettiness reduced_state), PrettyDoc (pPrintFullState quietStatePrettiness (meaning p)) {-, res-})
                                                                                 ; stuff <- instanceSplit memo_opt (remaining_deeds, Heap (foldr (\x -> M.insert x lambdaBound) h_inst (fun p:varSetElems extraOutputFvs)) ids_inst, k_inst, applyAbsVars (fun p) (Just rn_lr) (abstracted p))
                                                                                 ; insertTagsM stuff }
                         where
                          -- This will always succeed because the state had deeds for everything in its heap/stack anyway:
                          Just remaining_deeds = claimDeeds (releaseStateDeed state) (pureHeapSize h_inst + stackSize k_inst)
                        -- FIXME: in rare cases (i.e when rollback is off OR when the state we are type-genning against is on the stack)
                        -- then this codepath could also overwrite the fulfilment for the old state to call into the generalised version:
                       RightGivesTypeGen _rn_l s rn_r -> trace "typegen" $ do { (deeds, e') <- memo_opt s; (_, e'') <- renameSCResult (case s of (_, Heap _ ids, _, _) -> ids) (rn_r, e'); return (deeds, e'') })) $
                     bestChoice [ (p, mr)
                                | let (parented_ps, unparented_ps) = trainToList (promises (scpMemoState s))
                                , (p, is_ancestor, common_h_vars) <- [ (p_sibling, fun p_parent == fun p_sibling, common_h_vars)
                                                                     | (p_parent, p_siblings) <- parented_ps
                                                                     , let common_h_vars = case meaning p_parent of (_, Heap _ ids, _, _) -> ids
                                                                     , p_sibling <- p_parent:p_siblings ] ++
                                                                     [ (p_root,    False,                         emptyInScopeSet)
                                                                     | p_root <- unparented_ps ]
                                , let inst_mtch | not iNSTANCE_MATCHING = NoInstances
                                                | is_ancestor           = AllInstances
                                                | otherwise             = InstancesOfGeneralised
                                      mm = MSGMode { msgCommonHeapVars = common_h_vars }
                                --      mm = MM { matchInstanceMatching = inst_mtch, matchCommonHeapVars = common_h_vars }
                                --, Just (heap_inst, k_inst, rn_lr) <- [-- (\res -> if isNothing res then pprTraceSC "no match:" (ppr (fun p)) res   else   pprTraceSC "match!" (ppr (fun p)) res) $
                                --                                      match' mm (meaning p) reduced_state]
                                , Just mr <- [msgMaybe mm (meaning p) reduced_state >>= msgMatch inst_mtch]
                                {-
                                , if dumped p
                                   then pprTraceSC "tieback-to-dumped" (ppr (fun p)) False
                                   else True
                                -}
                                ] of Just (exact, res) -> pure $ if exact then (res, s) else remember (\_ _ -> liftM ((,) False) res)
                                     Nothing           | CheckOnly <- memo_how
                                                       -> pure (liftM snd $ opt Nothing state, s)
                                                       | otherwise
                                                       -> pure (remember opt)
      where (_state_did_reduce, reduced_state) = reduceForMatch state

            -- Prefer more exact matches (see Note [Instance matching and loops]: it is essential to choose exact matches over instances)
            bestChoice :: [(Promise, MSGMatchResult)]
                       -> Maybe (Bool, (Promise, MSGMatchResult))
            bestChoice = go Nothing
              where go mb_best [] = fmap ((,) False) mb_best
                    go mb_best (this@(_, mr):rest)
                      | mostSpecific mr = Just (True, this) -- Stop early upon exact match (as an optimisation)
                      | otherwise       = go (Just (maybe this (\best@(_, best_mr) -> if mr `moreSpecific` best_mr then this else best) mb_best)) rest

                    mostSpecific :: MSGMatchResult -> Bool
                    mostSpecific (RightIsInstance (Heap h _) rn k) = isPureHeapEmpty h && isStackEmpty k && isEmptyRenaming rn
                    mostSpecific (RightGivesTypeGen _ _ _)         = False

                    moreSpecific :: MSGMatchResult -> MSGMatchResult -> Bool
                    -- Just a heuristic to decide between different instance matches: try to discard *least* stuff
                    moreSpecific (RightIsInstance (Heap h_l _) _ k_l) (RightIsInstance (Heap h_r _) _ k_r)
                      = (pureHeapSize h_l + stackSize k_l) <= (pureHeapSize h_r + stackSize k_r)
                    -- Another heuristic: try to discard *most* type information (this is only more specific in the sense that it is more specific about what can be thrown away)
                    moreSpecific (RightGivesTypeGen _ _ rn_l) (RightGivesTypeGen _ _ rn_r)
                      = renamingSize rn_r <= renamingSize rn_l
                    -- Prefer instance matches to type generalisation (don't have a good sense about what is best here):
                    moreSpecific (RightIsInstance _ _ _) _ = True
                    moreSpecific _ (RightIsInstance _ _ _) = False

                    renamingSize (_, tv_subst, co_subst) = sumMap typeSize (varEnvElts tv_subst) + sumMap coercionSize (varEnvElts co_subst)

                    -- TODO: it might be OK to insist the incoming renaming is invertible, but this should definitely work:
                    isEmptyRenaming (_, tv_subst, co_subst) = all isTyVarTy (varEnvElts tv_subst) && all isCoVarCo (varEnvElts co_subst)
                    isCoVarCo = isJust . getCoVar_maybe

            -- I do not currently know how to prevent the supercompiler from building loops when doing float-to-match
            -- (i.e. instance matching where we match var-to-term, not just var-to-var) is on. With it, we tend to do things like:
            --
            --  [a] h0 = D[let f = v in f] = let f = h1 in f
            --      h1 = D[let f = v in v] = h0
            --
            -- In standard supercompilation, we still need to skip some tieback attempts for reasons of correctness:
            -- our correctness proof does not work if we try to memo values. Luckily, this doesn't hurt us at all because
            -- it doesn't sacrifice termination of the supercompiler. See the thesis for details.
            --
            -- The main example we are worried about when we discuss correctness is this:
            --
            --  [a] h0 = D[let x = True in x] = let x = h1 in x
            --      h1 = D[True] = h0
            --
            -- (Note that the meaning of h1 is cost equivalent to the meaning of h0, so superficially this tieback is justified!
            --  If x bound a lambda instead then we would be able to make an argument that the tieback was not cost equivalent.)
            memo_how -- If eager value splitting is on, we'll always check, which
                     -- is certainly terminating (even without EVS) and is also safe (this is due to EVS).
                     --
                     -- Even with EVS, it is not safe to skip memoisation if the state in the focus is a value, an indirection, or stuck
                     -- on a free variable. Examples that prove each case incorrect are:
                     --  1. let f = \x -> C f in C f
                     --     (a value state, eager-splits to itself)
                     --  2. let f = \x -> f |> co in f |> co
                     --     (an indirection, eager-splits to itself)
                     --  3. let f = \x -> g f x in g f x
                     --     (blocked on FV, eager-splits to itself)
                     | eAGER_SPLIT_VALUES
                     = CheckAndRemember

                     -- If eager value splitting is off, we do this, which is sufficient to avoid non-correct output
                     -- but not enough to avoid non-termination (consider the input let xs = 1:xs in 1:xs, which splits to
                     -- itself and is always irreducible)
                     | isStateIrreducible state
                     = Skip

                     | otherwise
                     = CheckAndRemember

data MemoHow = Skip | CheckOnly | CheckAndRemember

-- NB: don't garbage collect when reducing for a match!
--
-- If you do then you can start with this term:
--   [1] let $dNum = ww3 in * a $dNum
--
-- Looks like this after reduction+GC (the update for $dNum is dead):
--   [2] case ww3 of Num ...
--
-- And if we reduce+split [1] instead we get (the update for $dNum is residualised):
--   [3] case $dNum of Num ...
--
-- Reducing+GCing [3] gives us [3] again, and that is alpha equivalent to [2],
-- so we tie back to it rather than continuing. But that means our code is:
--   let h @a ww3 = let $dNum = ww3
--                  in h a $dNum
--
-- Which is a loop. So we need to do one of:
--  1. Not GC before matching
--  2. GC *after* reduction in the main codepath.
--  3. Not eliminate dead update frames when GCing
reduceForMatch :: State -> (Bool, State)
reduceForMatch state | rEDUCE_BEFORE_MATCH = {- second gc $ -} reduceWithFlag (case state of (_, h, k, e) -> (maxBound, h, k, e)) -- Reduce ignoring deeds for better normalisation
                     | otherwise           = (False, state)

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
