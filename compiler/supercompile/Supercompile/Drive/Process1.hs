{-# LANGUAGE BangPatterns, RankNTypes, RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Supercompile.Drive.Process1 (supercompile) where

#include "HsVersions.h"

import Supercompile.Drive.Match
import Supercompile.Drive.Split
import Supercompile.Drive.Process

import Supercompile.Core.FreeVars
import Supercompile.Core.Renaming
--import Supercompile.Core.Size
import Supercompile.Core.Syntax
import Supercompile.Core.Tag

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Residualise
import Supercompile.Evaluator.Syntax

import Supercompile.Termination.Combinators
--import Supercompile.Termination.Extras
--import Supercompile.Termination.TagSet
--import Supercompile.Termination.TagGraph
import Supercompile.Termination.Generaliser

import Supercompile.StaticFlags
import Supercompile.Utilities hiding (Monad(..))

import Id         (mkLocalId, setIdOccInfo)
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

import Prelude hiding (Monad(..))


ifThenElse :: Bool -> a -> a -> a
ifThenElse True  x _ = x
ifThenElse False _ y = y


supercompile :: M.Map Var Term -> Term -> IO (SCStats, Term)
supercompile unfoldings e = liftM (second fVedTermToTerm) $ runScpM $ fmap snd $ sc (mkLinearHistory (cofmap fst wQO)) S.empty state
  where state = prepareTerm unfoldings e

--
-- == The drive loop ==
--

data Promise = P {
    fun        :: Var,      -- Name assigned in output program
    abstracted :: [AbsVar], -- Abstracted over these variables
    meaning    :: State,    -- Minimum adequate term
    embedded   :: Maybe SDoc
  }

instance MonadStatics ScpBM where
    bindCapturedFloats = bindFloats
    monitorFVs mx = ScpM $ \e s k -> unScpM mx e s (\x s' -> let (fss_delta, _fss_common) = splitByReverse (pTreeHole s) (pTreeHole s')
                                                             in k (unionVarSets [fvedTermFreeVars e' | (_, e') <- concatMap fulfilmentTreeFulfilments fss_delta], x) s')

-- Note [Floating h-functions past the let-bound variables to which they refer]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- This seems like a reasonable thing to do because some variables will become free after supercompilation.
-- However, there really isn't much point doing the float because I won't be able to tie back to the floated thing
-- in any other branch.
--
-- Indeed, allowing such tiebacks may be a source of bugs! Consider a term like:
--
--  x |-> <10>
--  x + 5
--
-- After supercompilation, we will have:
--
--  15
--
-- Since we check the *post supercompilation* free variables here, that h function could be floated
-- upwards, so it is visible to later supercompilations. But what if our context had looked like:
--
--   (let x = 10 in x + 5, let x = 11 in x + 5)
--
-- Since we only match phantoms by name, we are now in danger of tying back to this h-function when we
-- supercompile the second component of the pair!
--
-- Conclusion: don't bother with this rubbish.
--
-- Note [Variables reachable from let-bindings]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- TODO: we shouldn't lambda-abstract over any variables reachable via the let-bound thing. Doing so needlessly
-- passes them around via lambdas when they will always be available in the closure.
--
-- Consider this example:
--
--   \y -> let x = \z -> .. too big to inline ... y ...
---        in (... x ..., ... x ...)
--
-- When supercompliing each component of the pair we might feel tempted to generate h-functions lambda abstracted over
-- y, but doing so is pointless (just hides information from GHC) since the result will be trapped under the x binding anyway.
fulfilmentRefersTo :: FreeVars -> (Promise, FreeVars) -> Maybe (Out Var)
fulfilmentRefersTo extra_statics (p, fulfilment_fvs)
  = if Foldable.any (`elemVarSet` extra_statics) (fulfilment_fvs `unionVarSet` extra_fvs)
     then Just (fun p)
     else Nothing
  where
    -- We bind floats with phantoms bindings where those phantom bindings are bound.
    --
    -- For wrappers introduced by --refine-fvs, we still need to use (fvedTermFreeVars e') because that will include
    -- the wrapped h-function (e.g. the h83' wrapper for h83). This also applies (though more rarely) for non-wrappers
    -- because looking at the fvedTermFreeVars is the only way we can learn about what h-functions they require.
    extra_fvs = stateLetBounders (meaning p)

-- Used at the end of supercompilation to extract just those h functions that are actually referred to.
-- More often than not, this will be *all* the h functions, but if we don't discard h functions on rollback
-- then this is not necessarily the case!
fulfilmentReferredTo :: FreeVars -> (Promise, FreeVars) -> Maybe FreeVars
fulfilmentReferredTo fvs (p, fulfilment_fvs)
  = if fun p `elemVarSet` fvs
     then Just fulfilment_fvs
     else Nothing

-- We do need a fixed point here to identify the full set of h-functions to residualise.
-- The reason is that even if a static variable is not free in an output h-function, we might
-- have created (and make reference to) some h-function that *does* actually refer to one
-- of the static variables.
-- See also Note [Phantom variables and bindings introduced by scrutinisation]
partitionFulfilments :: forall t a b. Traversable t
                     => (a -> (Promise, FreeVars) -> Maybe b)              -- ^ Decide whether a fulfilment should be residualised given our current a, returning a new b if so
                     -> ([b] -> a)                                         -- ^ Combine bs of those fufilments being residualised into a new a
                     -> a                                                  -- ^ Used to decide whether the fufilments right here are suitable for residulising
                     -> t (Promise, Fulfilment)                            -- ^ Fulfilments to partition
                     -> ([(Promise, Fulfilment)], t (Promise, Fulfilment)) -- ^ Fulfilments that should be bound and those that should continue to float, respectively
partitionFulfilments check combine = go
  where
    go :: a -> t (Promise, Fulfilment) -> ([(Promise, Fulfilment)], t (Promise, Fulfilment))
    go x fs
      -- | traceRender ("partitionFulfilments", x, map (fun . fst) fs) False = undefined
      | null fs_now' = ([], fs)
      | otherwise    = first (fs_now' ++) $ go (combine xs') fs'
      where (fs', fs_now_xs') = runState (traverse one_captured fs) []
            (fs_now', xs') = unzip fs_now_xs'

            one_captured :: (Promise, Fulfilment) -> State.State [((Promise, Fulfilment), b)] (Promise, Fulfilment)
            one_captured (p, f)
              | Fulfilled e <- f
              , Just y <- check x (p, fvedTermFreeVars e)
              = modify (((p, f), y):) Monad.>> pure (p, Captured)
              | otherwise
              = pure (p, f)
            
-- Note [Where to residualise fulfilments with FVs]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Be careful of this subtle problem:
--
--  let h6 = D[e1]
--      residual = ...
--      h7 = D[... let residual = ...
--                 in Just residual]
--  in ...
--
-- If we first drive e1 and create a fulfilment for the h6 promise, then when driving h7 we will eventually come across a residual binding for the
-- "residual" variable. If we aren't careful, we will notice that "residual" is a FV of the h6 fulfilment and residualise it deep within h7. But
-- what if the body of the outermost let drove to something referring to h6? We have a FV - disaster!
--
-- The right thing to do is to make sure that fulfilments created in different "branches" of the process tree aren't eligible for early binding in
-- that manner, but we still want to tie back to them if possible. The bindFloats function achieves this by carefully shuffling information between the
-- fulfilments and promises parts of the monadic-carried state.
bindFloats :: FreeVars -> ScpBM a -> ScpBM ([(Var, FVedTerm)], a)
bindFloats extra_statics mx
  = ScpM $ \e s k -> unScpM mx (e { pTreeContext = BindCapturedFloats extra_statics (pTreeHole s) : pTreeContext e })
                               (s { pTreeHole = [] }) (kontinue s k)
  where
    kontinue s k x s' = -- traceRender ("bindFloats", [(fun p, fvedTermFreeVars e) | (p, e) <- fs_now], [(fun p, fvedTermFreeVars e) | (p, e) <- fs_later]) $
                        k (fulfilmentsToBinds fs_now, x) (s' { pTreeHole = unComp fs_later ++ pTreeHole s })
      where (fs_now, fs_later) = partitionFulfilments fulfilmentRefersTo mkVarSet extra_statics (Comp (pTreeHole s'))

fulfilmentsToBinds :: [(Promise, Fulfilment)] -> Out [(Var, FVedTerm)]
fulfilmentsToBinds fs = sortBy (comparing ((read :: String -> Int) . dropLastWhile (== '\'') . drop 1 . varString . fst)) [(fun p, e') | (p, Fulfilled e') <- fs]

freshHName :: ScpM f f Name
freshHName = ScpM $ \_e s k -> k (expectHead "freshHName" (names s)) (s { names = tail (names s) })


getPromises :: ScpM () () [Promise]
getPromises = ScpM $ \e s k -> k (pTreeContextPromises (pTreeContext e)) s

getPromiseNames :: ScpM FulfilmentTree FulfilmentTree [Var]
getPromiseNames = ScpM $ \e s k -> k (map (fun . fst) (fulfilmentTreeFulfilments (pTreeHole s)) ++ map fun (pTreeContextPromises (pTreeContext e))) s

promise :: Promise -> ScpBM (a, Out FVedTerm) -> ScpPM (a, Out FVedTerm)
promise p opt = ScpM $ \e s k -> {- traceRender ("promise", fun p, abstracted p) $ -} unScpM (mx p) (e { pTreeContext = Promise p : pTreeContext e, depth = 1 + depth e }) (s { pTreeHole = [] }) k
  where
    mx p = do
      (a, optimised_e) <- opt
      -- We have a little trick here: we can reduce the number of free variables our "h" functions abstract over if we discover that after supercompilation some
      -- variables become dead. This lets us get some of the good stuff from absence analysis: we can actually reduce the number of loop-carried vars like this.
      -- It is particularly important to do this trick when we have unfoldings, because functions get a ton more free variables in that case.
      --
      -- If some of the fufilments we have already generated refer to us, we need to fix them up because their application sites will apply more arguments than we
      -- actually need. We aren't able to do anything about the stuff they spuriously allocate as a result, but we can make generate a little wrapper that just discards
      -- those arguments. With luck, GHC will inline it and good things will happen.
      --
      -- We have to be careful when generating the wrapper: the *type variables* of the optimised_fvs must also be abstracted over!
      --
      -- TODO: we can generate the wrappers in a smarter way now that we can always see all possible fulfilments?
      let optimised_fvs_incomplete = fvedTermFreeVars optimised_e
          optimised_fvs = optimised_fvs_incomplete `unionVarSet` unionVarSets (map varBndrFreeVars (varSetElems optimised_fvs_incomplete))
          abstracted'
            | not rEFINE_FULFILMENT_FVS = abstracted p
            | otherwise = [x { absVarDead = absVarDead x || not (absVarVar x `elemVarSet` optimised_fvs) }
                          | x <- abstracted p]
      pprTrace "promise" (ppr optimised_fvs $$ ppr optimised_e) $
       ScpM $ \_e s k -> k () (s { pTreeHole = Split False [(p { abstracted = abstracted' },
                               Fulfilled (absVarLambdas abstracted' optimised_e))] (pTreeHole s) })
      
      fmap (((mkVarSet (map absVarVar abstracted') `unionVarSet` stateLetBounders (meaning p)) `unionVarSet`) . mkVarSet) getPromiseNames >>=
        \fvs -> ASSERT2(optimised_fvs `subVarSet` fvs, ppr (fun p, optimised_fvs `minusVarSet` fvs, fvs, optimised_e)) return ()
      
      return (a, fun p `applyAbsVars` abstracted')

-- No meaning, term: "legacy" term that can no longer be tied back to
-- No meaning, no term: rolled back while still a promise
-- Meaning, term: standard
-- Meaning, no term: rolled back after being fulfilled for some other reason
type FulfilmentTree = PTree (Promise, Fulfilment)

data Fulfilment = Captured                           -- ^ Already residualised because captured by a BV or similar
                | RolledBack (Maybe (Out FVedTerm))  -- ^ Rolled back so will never be residualised, but we might know what it is anyway (FIXME: always Nothing)
                | Fulfilled (Out FVedTerm)           -- ^ Not yet residualised: floated, eligible for further tiebacks

data PTree a = Tieback Var              -- ^ Didn't promise or drive extra stuff: just tied back
             | Split Bool [a] [PTree a] -- ^ Made a promise, fulfiling it like so (with 1 or 2 fulfilments..)
                                        --   and where the children are these
             | BoundCapturedFloats FreeVars [PTree a]
                                        -- ^ Produced these children within the context of these BVs

instance Functor PTree where fmap = Traversable.fmapDefault
instance Foldable PTree where foldMap = Traversable.foldMapDefault

instance Traversable PTree where
    traverse _ (Tieback n)  = pure (Tieback n)
    traverse f (Split rb x ts) = Split rb <$> traverse f x <*> traverse (traverse f) ts
    traverse f (BoundCapturedFloats bvs ts) = BoundCapturedFloats bvs <$> traverse (traverse f) ts

-- Fulfilments at each level and the free variables of bindCapturedFloats that caused them to pushed.
-- We guarantee that promises for each these are already present in the promises field.
--
-- I have to store these in the monad-carried information because catchScpM has to be able to restore
-- (a subset of) them when rollback is initiated. See also Note [Where to residualise fulfilments with FVs]
--
-- I have to store them in their full-blown tree format (rather than just a flat list of Fulfilment at each
-- level) for nice pretty-printed logging.
data PTreeContextItem = Promise Promise
                      | BindCapturedFloats FreeVars [FulfilmentTree]
type PTreeContext = [PTreeContextItem]

data ScpEnv = ScpEnv {
    pTreeContext :: PTreeContext, -- Zipper into the process tree "above" us
    depth        :: Int
  }

data ScpState f = ScpState {
    names     :: [Name],
    pTreeHole :: f, -- Work-in-progress on "this level" of the process tree
    stats     :: SCStats
  }

pTreeContextPromises :: PTreeContext -> [Promise]
pTreeContextPromises = foldMap $ \tci -> case tci of
    Promise p                -> [p]
    BindCapturedFloats _ fts -> map fst (concatMap fulfilmentTreeFulfilments fts)

-- Only returns those fulfilments that are still floating and eligible for tieback
fulfilmentTreeFulfilments :: FulfilmentTree -> [(Promise, Out FVedTerm)]
fulfilmentTreeFulfilments t = [(p, e') | (p, Fulfilled e') <- Foldable.toList t]

class IMonad m where
    return :: a -> m s s a
    (>>=) :: m s0 s1 a -> (a -> m s1 s2 b) -> m s0 s2 b
    (>>) :: m s0 s1 a -> m s1 s2 b -> m s0 s2 b
    fail :: String -> m s0 s1 a

    mx >> my = mx >>= \_ -> my
    fail = error

-- The IO monad is used to catch Ctrl+C for pretty-printing purposes
newtype ScpM f f' a = ScpM { unScpM :: ScpEnv -> ScpState f -> (a -> ScpState f' -> IO (SCStats, Out FVedTerm)) -> IO (SCStats, Out FVedTerm) }

type ScpPM = ScpM ()                FulfilmentTree
type ScpBM = ScpM [FulfilmentTree] [FulfilmentTree]

instance Functor (ScpM f f') where
    fmap f x = x >>= (return . f)

instance Monad.Monad (ScpM f f) where
    return = return
    (>>=) = (>>=)

instance IMonad ScpM where
    return x = ScpM $ \_e s k -> k x s
    (!mx) >>= fxmy = ScpM $ \e s k -> unScpM mx e s (\x s -> unScpM (fxmy x) e s k)

class Printable f where
    handlePrint :: ScpM f f' a -> ScpM f f' a

instance Printable FulfilmentTree where
    handlePrint = handlePrint' pprScpPM

instance Printable [FulfilmentTree] where
    handlePrint = handlePrint' pprScpBM

instance Printable () where
    handlePrint = id

handlePrint' :: ScpM f f String -> ScpM f f' a -> ScpM f f' a
handlePrint' prnt act = ScpM $ \e s k -> handleJust (\e -> case e of UserInterrupt -> Just (); _ -> Nothing) (\() -> unScpM prnt e s (\res _ -> writeFile "sc.htm" res Monad.>> error "Ctrl+C-ed")) (unScpM act e s k)

runScpM :: ScpPM (Out FVedTerm) -> IO (SCStats, Out FVedTerm)
runScpM me = unScpM (tracePprScpPM me) init_e init_s (\e' s -> Monad.return (stats s, bindManyMixedLiftedness fvedTermFreeVars (fulfilmentsToBinds $ fst $ partitionFulfilments fulfilmentReferredTo unionVarSets (fvedTermFreeVars e') (pTreeHole s)) e'))
  where
    init_e = ScpEnv { pTreeContext = [], depth = 0 }
    init_s = ScpState { names = h_names, pTreeHole = (), stats = mempty }
    
    -- We need to create a name supply with *pairs* of Names because if we refine the fulfilment FVs we will create two bindings for each h-function
    h_names = zipWith (\i uniq -> mkSystemVarName uniq (mkFastString ('h' : show (i :: Int))))
                      [1..] (uniqsFromSupply hFunctionsUniqSupply)

catchScpM :: ((forall b. c -> ScpBM b) -> ScpBM a) -- ^ Action to try: supplies a function than can be called to "raise an exception". Raising an exception restores the original ScpEnv and ScpState
          -> (c -> ScpBM a)                        -- ^ Handler deferred to if an exception is raised
          -> ScpBM a                               -- ^ Result from either the main action or the handler
catchScpM f_try f_abort = ScpM $ \e s k -> unScpM (f_try (\c -> ScpM $ \e' s' _k' ->
    unScpM (f_abort c) e (if False -- dISCARD_FULFILMENTS_ON_ROLLBACK
                          then s
                          else let (fss_candidates, _fss_common) = splitByReverse (pTreeContext e) (pTreeContext e')
                                   
                                   -- Since we are rolling back we need to float as many of the fulfilments created in between here and the rollback point
                                   -- upwards. This means that we don't lose the work that we already did to supercompile those bindings.
                                   --
                                   -- The approach is to accumulate a set of floating fulfilments that I try to move past each statics set one at a time,
                                   -- from inside (deeper in the tree) to the outside (closer to top level).
                                   go :: (VarSet, [FulfilmentTree]) -> PTreeContextItem -> (VarSet, [FulfilmentTree])
                                   go (partial_not_completed, fs_floating) (Promise p) = (partial_not_completed `extendVarSet` fun p, [Split True [(p, RolledBack Nothing)] fs_floating])
                                   go (partial_not_completed, fs_floating) (BindCapturedFloats extra_statics fs_pre_bind) = (partial_not_completed, fs_pre_bind ++ [BoundCapturedFloats extra_statics (unComp fs_ok)])
                                      where (_fs_discard, fs_ok) = partitionFulfilments fulfilmentRefersTo mkVarSet (not_completed `unionVarSet` extra_statics) (Comp fs_floating)

                                   (not_completed, fs_floating) = foldl' go (emptyVarSet, []) fss_candidates
                               in s' { pTreeHole = fs_floating ++ pTreeHole s })
                         k)) e s k

addStats :: SCStats -> ScpM f f ()
addStats scstats = ScpM $ \_e s k -> k () (let scstats' = stats s `mappend` scstats in scstats' `seqSCStats` s { stats = scstats' })

recordStopped :: State -> ScpBM a -> ScpBM a
recordStopped state mx = ScpM $ \e s k -> unScpM mx (e { pTreeContext = case pTreeContext e of (Promise p:rest) -> Promise p { embedded = Just (pPrintFullState False state) }:rest }) s k


type PrettyTree = PTree (Var, SDoc, Maybe SDoc, Maybe SDoc)

tracePprScpPM :: ScpPM a -> ScpPM a
tracePprScpPM = id
{-
tracePprScpPM mx = do
  x <- mx
  s <- pprScpPM
  unsafePerformIO (writeFile "sc.htm" s Monad.>> Monad.return (return x))
  -- pprTraceSC "tracePprScpM" (text s) $ return x
-}

pprScpBM :: ScpBM String
pprScpPM :: ScpM FulfilmentTree FulfilmentTree String
(pprScpBM, pprScpPM) = (go (map unwindTree), go (\t -> [unwindTree t]))
  where
    go :: forall f. (f -> [PrettyTree]) -> ScpM f f String
    go xtract = ScpM $ \e s k -> k (html (pTreeContext e) (xtract (pTreeHole s))) s

    html ctxt hole = show $ thehtml $ toHtmlFromList
      [header (toHtmlFromList [thetitle (stringToHtml "Supercompilation"),
                               script mempty ! [thetype "text/javascript", src "http://www.omega-prime.co.uk/files/chsc/jquery-1.6.3.min.js"],
                               script mempty ! [thetype "text/javascript", src "http://www.omega-prime.co.uk/files/chsc/jstree_pre1.0_fix_1/jquery.jstree.js"],
                               script js]),
       body (toHtmlFromList [thediv htm ! [identifier "tree"],
                             pre mempty ! [identifier "content-in"],
                             pre mempty ! [identifier "content-embed"],
                             pre mempty ! [identifier "content-out"]])]
      where htm = pprTrees (unwindContext ctxt hole)
            -- NB: must use primHtml so that entities in the script are not escaped
            js = primHtml "$(function () { $(\"#tree\").jstree({ \"plugins\" : [\"themes\", \"html_data\"], \"core\" : { \"animation\" : 0 } }).bind(\"loaded.jstree\", function (event, data) { data.inst.open_all(-1, false); }); });"

    unwindTree :: FulfilmentTree -> PrettyTree
    unwindTree = fmap unwindPromiseFulfilment

    unwindPromiseFulfilment (p, f) = (fun p, pPrintFullState False (meaning p), embedded p,
                                      case f of Captured         -> Nothing
                                                RolledBack mb_e' -> fmap ppr mb_e'
                                                Fulfilled e'     -> Just (ppr e'))

    unwindContext :: PTreeContext -> [PrettyTree] -> [PrettyTree]
    unwindContext = flip $ foldl (flip unwindContextItem)

    unwindContextItem :: PTreeContextItem -> [PrettyTree] -> [PrettyTree]
    unwindContextItem (Promise p)                  ts = [Split False [(fun p, pPrintFullState False (meaning p), embedded p, Nothing)] ts]
     -- NB: don't put (Split True) here because otherwise it looks like everything on the way back to the root has been rolled back, when it fact it is only "in progress"
    unwindContextItem (BindCapturedFloats fvs ts') ts = map unwindTree ts' ++ [BoundCapturedFloats fvs ts]

    pprTrees :: [PrettyTree] -> Html
    pprTrees ts = ulist $ toHtmlFromList $ map pprTree ts

    pprTree :: PrettyTree -> Html
    pprTree t = li $ case t of
      Tieback x                  -> anchor (stringToHtml ("Tieback " ++ show x)) ! [href ("#" ++ show x)]
      Split rb fs ts             -> toHtmlFromList ([thediv (anchor (stringToHtml (show x ++ (if isJust mb_emb_code then " (sc-stop)" else "") ++ if rb then " (rolled back)" else "")) !
                                                                [strAttr "onclick" $ "document.getElementById(\"content-in\").innerText=" ++ show (showSDoc in_code) ++
                                                                                    ";document.getElementById(\"content-out\").innerText=" ++ show (maybe "" showSDoc mb_out_code) ++
                                                                                    ";document.getElementById(\"content-embed\").innerText=" ++ show (maybe "" showSDoc mb_emb_code) ++
                                                                                    ";return false"]) !
                                                            [identifier (show x)]
                                                    | (x, in_code, mb_emb_code, mb_out_code) <- fs] ++ [pprTrees ts])
      BoundCapturedFloats fvs ts -> toHtmlFromList [stringToHtml ("Capture " ++ showSDoc (ppr fvs)), pprTrees ts]


type RollbackScpM = Generaliser -> ScpBM (Deeds, Out FVedTerm)
type ProcessHistory = LinearHistory (State, RollbackScpM)

liftPB :: ScpPM a -> ScpBM a
liftPB act = ScpM $ \e s k -> unScpM act e (s { pTreeHole = () }) (\x s' -> k x (s' { pTreeHole = pTreeHole s' : pTreeHole s }))

sc  :: ProcessHistory -> AlreadySpeculated -> State -> ScpPM (Deeds, Out FVedTerm)
sc' :: ProcessHistory -> AlreadySpeculated -> State -> ScpBM (Deeds, Out FVedTerm)
sc  hist = rollbackBig (memo (sc' hist))
sc' hist speculated state = handlePrint $ (\raise -> check raise) `catchScpM` \gen -> stop gen state hist -- TODO: I want to use the original history here, but I think doing so leads to non-term as it contains rollbacks from "below us" (try DigitsOfE2)
  where
    check this_rb = case terminate hist (state, this_rb) of
                      Continue hist' -> continue hist'
                      Stop (shallower_state, rb) -> recordStopped shallower_state $maybe (stop gen state hist) ($ gen) $ guard sC_ROLLBACK Monad.>> Just rb
                        where gen = mK_GENERALISER shallower_state state
    stop gen state hist = do addStats $ mempty { stat_sc_stops = 1 }
                             maybe (trace "sc-stop: no generalisation" $ split state) (trace "sc-stop: generalisation") (generalise gen state) (liftPB . sc hist speculated) -- Keep the trace exactly here or it gets floated out by GHC
    continue hist = do traceRenderScpM "reduce end (continue)" (PrettyDoc (pPrintFullState False state'))
                       addStats stats
                       split state' (liftPB . sc hist speculated')
      where (speculated', (_steps, stats, state')) = (if sPECULATION then speculate speculated else ((,) speculated)) $ reduce' state -- TODO: experiment with doing admissability-generalisation on reduced terms. My suspicion is that it won't help, though (such terms are already stuck or non-stuck but loopy: throwing stuff away does not necessarily remove loopiness).

memo :: (AlreadySpeculated -> State -> ScpBM (Deeds, Out FVedTerm))
     ->  AlreadySpeculated -> State -> ScpPM (Deeds, Out FVedTerm)
memo opt speculated state0 = do
    let state1 = gc state0 -- Necessary because normalisation might have made some stuff dead
    
    ps <- getPromises
    case [ (p, (releaseStateDeed state0, fun p `applyAbsVars` tb_dynamic_vs))
         | p <- ps
         , Just rn_lr <- [(\res -> if isNothing res then pprTraceSC "no match:" (ppr (fun p)) res else res) $
                           match (meaning p) state1]
          -- NB: because I can trim reduce the set of things abstracted over above, it's OK if the renaming derived from the meanings renames vars that aren't in the abstracted list, but NOT vice-versa
         -- , let bad_renames = S.fromList (abstracted p) S.\\ M.keysSet (unRenaming rn_lr) in ASSERT2(S.null bad_renames, text "Renaming was inexhaustive:" <+> pPrint bad_renames $$ pPrint (fun p) $$ pPrintFullState (unI (meaning p)) $$ pPrint rn_lr $$ pPrintFullState state3) True
          -- ("tieback: FVs for " ++ showSDoc (pPrint (fun p) $$ text "Us:" $$ pPrint state3 $$ text "Them:" $$ pPrint (meaning p)))
         , let rn_fv = renameAbsVar rn_lr
                       -- NB: If tb contains a dead PureHeap binding (hopefully impossible) then it may have a free variable that
                       -- I can't rename, so "rename" will cause an error. Not observed in practice yet.
               tb_dynamic_vs = map rn_fv (abstracted p)
         ] of
      (p, res):_ -> {- traceRender ("tieback", pPrintFullState state3, fst res) $ -} do
        traceRenderScpM "=sc" (fun p, PrettyDoc (pPrintFullState True state1), res)
        ScpM $ \_ s k -> k res (s { pTreeHole = Tieback (fun p) })
      [] -> {- traceRender ("new drive", pPrintFullState state3) $ -} do
        let vs_list = stateAbsVars state1

        -- NB: promises are lexically scoped because they may refer to FVs
        x <- freshHName
        promise (P { fun = mkLocalId x (vs_list `mkPiTypes` stateType state1), abstracted = map mkLiveAbsVar vs_list, meaning = state1, embedded = Nothing }) $
          do
            traceRenderScpM ">sc" (x, PrettyDoc (pPrintFullState True state1))
            -- FIXME: this is the site of the Dreadful Hack that makes it safe to match on reduced terms yet *drive* unreduced ones
            -- I only add non-internally bound junk to the input heap because:
            --  a) Thats the only stuff I *need* to add to make sure the FVs etc match up properly
            --  b) New InternallyBound stuff might be created by reduction and then swiftly become dead, and I don't want to push that down
            --     gratutiously. Furthermore, the Ids for that stuff might clash with those still-to-be-allocated in the state0 IdSupply.
            --
            -- Note that since the reducer only looks into non-internal *value* bindings doing this does not cause work duplication, only value duplication
            --
            -- FIXME: I'm not acquiring deeds for these....
            res <- opt speculated state1
            traceRenderScpM "<sc" (x, PrettyDoc (pPrintFullState False state1), res)
            return res

-- Several design choices here:
--
--  1. How to account for size of specialisations created during drive? Presumably ones that eventually get shared should be given a discount, but how?
--
--  2. How to continue if we do roll back. Currently I throw away any specialisations created in the process, but this seems uncool.
rollbackBig :: (AlreadySpeculated -> State -> ScpM f f' (Deeds, Out FVedTerm))
            ->  AlreadySpeculated -> State -> ScpM f f' (Deeds, Out FVedTerm)
rollbackBig opt speculated state
  -- | rOLLBACK_BIG = ScpM $ \e s k -> unScpM (opt speculated state) e s $ \(deeds', term') s' -> let too_big = fvedTermSize term' + sum [fvedTermSize term' | (p, term') <- pTreeHoles s', not (fun p `elem` map (fun . fst) (pTreeHoles s))] > bLOAT_FACTOR * stateSize state
  --                                                                                              in if too_big then k (case residualiseState state of (deeds, _, e') -> (deeds, e')) s else k (deeds', term') s'
  | otherwise = opt speculated state

traceRenderScpM :: Outputable a => String -> a -> ScpM f f ()
traceRenderScpM msg x = ScpM (\e s k -> k (depth e) s) >>= \depth -> pprTraceSC msg (nest depth $ pPrint x) $ return ()
