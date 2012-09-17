module Supercompile.Drive.MSG (
    MSGMode(..), msg, msgMaybe,

    InstanceMatching(..), MSGMatchResult(..), msgMatch
  ) where

#include "HsVersions.h"

import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Syntax

--import qualified Supercompile.GHC as GHC
import Supercompile.Utilities hiding (guard)
import Supercompile.StaticFlags

import qualified CoreSyn as Core

import Util
import Coercion
import CoreUtils  (hashCoercion, hashType {- , hashExpr -})
import Name       (mkSystemVarName)
import Var        (TyVar, mkTyVar, isTyVar, isId, varType, setVarType, tyVarKind, setTyVarKind, varName, idDetails, setIdDetails, idInfo, lazySetIdInfo)
import Id         (Id, idType, idName, realIdUnfolding, setIdUnfolding, idSpecialisation, setIdSpecialisation, mkSysLocal, mkLocalId)
import IdInfo     (SpecInfo(..))
import VarEnv
import Pair
import Type       (mkTyConApp, mkAppTy, splitAppTys, getTyVar_maybe, isKindTy)
import Kind
import TysWiredIn (pairTyCon {- , tupleCon -})
import TysPrim    (funTyCon)
import TypeRep    (Type(..))
import TrieMap    (TrieMap(..), CoercionMap, TypeMap)
import Rules      (mkSpecInfo, roughTopNames)
import Unique     (mkUniqueGrimily)
import FastString (fsLit)
--import BasicTypes (TupleSort(..))
import qualified State

import Id (mkTemplateLocals)
import Type (mkFunTy)
import TysWiredIn
import BasicTypes


import Control.Monad (join)
import Control.Monad.Fix
import qualified Data.Foldable as Foldable

import qualified Data.IntMap as IM
import qualified Data.Map as M
--import qualified Data.Set as S
--import qualified Data.Foldable as Foldable


pprTraceSC :: String -> SDoc -> a -> a
--pprTraceSC _ _ = id
--pprTraceSC = pprTrace
pprTraceSC msg doc a = traceSC (msg ++ ": " ++ showSDoc doc) a

traceSC :: String -> a -> a
traceSC _ = id
--traceSC = trace


-- Demonstrates rollback due to heap work duplication
example1 :: SDoc
example1 = either text pPrint $ msg' (MSGMode (mkInScopeSet emptyVarSet)) state1 state2
  where
    tg = mkTag 0

    state1 = (emptyDeeds, Heap (M.fromList [(f, lambdaBound), (g, lambdaBound), (x, internallyBound (renamedTerm $ annedTerm tg (annedTerm tg (Var g) `App` y)))]) (mkInScopeSet (mkVarSet [f, g, x, y])), Tagged tg (Apply x) `Car` Tagged tg (Update y) `Car` Loco False, (annedQA tg (Question f)))
    state2 = (emptyDeeds, Heap (M.fromList [(f, lambdaBound), (x, lambdaBound)]) (mkInScopeSet (mkVarSet [f, x, y])), Tagged tg (Apply x) `Car` Tagged tg (Update y) `Car` Loco False, (annedQA tg (Question f)))

    [f, g, x, y] = mkTemplateLocals [boolTy `mkFunTy` intTy, intTy `mkFunTy` boolTy, boolTy, intTy]

-- Demonstrates rollback due to stack "work duplication"
example2 :: SDoc
example2 = either text pPrint $ msg' (MSGMode (mkInScopeSet emptyVarSet)) state1 state2
  where
    expensive = internallyBound (renamedTerm $ annedTerm tg (annedTerm tg (Var f) `App` y))
    pairDC = tupleCon BoxedTuple 2
    tg = mkTag 0

    state1 = (emptyDeeds, Heap (M.fromList [(f, lambdaBound), (y, lambdaBound), (a, expensive)]) (mkInScopeSet (mkVarSet [f, y, a])), Loco False, annedQA tg $ Answer $ renamedValue $ Data pairDC [intTy, intTy] [] [a, a])
    state2 = (emptyDeeds, Heap (M.fromList [(f, lambdaBound), (y, lambdaBound), (a, expensive), (b, expensive)]) (mkInScopeSet (mkVarSet [f, y, a, b])), Loco False, annedQA tg $ Answer $ renamedValue $ Data pairDC [intTy, intTy] [] [a, b])

    [a, b, f, y] = mkTemplateLocals [intTy, intTy, boolTy `mkFunTy` intTy, boolTy]


rnBndr2' :: Applicative t => RnEnv2 -> Var -> Var -> MSGT t (RnEnv2, Var)
rnBndr2' rn2 x_l x_r = MSG $ \_ s -> (s, pure (rnBndr2'' (msgInScopeSet s) rn2 x_l x_r))
  -- The uniqAway is 1/2 of the story to ensure we don't get clashes between new rigid binders and the new common heap binders
  --
  -- (We don't want to inadvertently have {x} in both InScopeSets and an occurrence (\y -> x) where RnEnv2 makes y rename to x by pure chance.
  -- BUT I don't want to just put ids_l/ids_r in the RnEnv2 because that will force common occurrence sites to rename (in msgPend) which
  -- which will make things drastically worse! Instead, rnBndr2' does the uniqAway manually.)

-- NB: rigid variable occurrences do not get correct type/info.
-- This actually dosen't matter for internal binders since the supercompiler's normal renaming mechanism will
-- propagate binding-site info down to the use sites, but it means we can't reuse the rigid-binders mechanismfor stack binders
rnBndr2'' :: InScopeSet -> RnEnv2 -> Var -> Var -> (RnEnv2, Var)
rnBndr2'' extra_iss rn2 x_l x_r = (rn2', x')
  where rn2' = rn2 { envL     = extendVarEnv (envL rn2) x_l x'
                   , envR     = extendVarEnv (envR rn2) x_r x'
                   , in_scope = extendInScopeSet (in_scope rn2) x' }
        x' = uniqAway (extra_iss `unionInScope` in_scope rn2) x_r


{-
newtype MSG' a = MSG' { unMSG' :: MSGLR a }

instance Functor MSG' where
    fmap = liftM

instance Monad MSG' where
    return x = MSG' (M.empty, x, M.empty)
    (vs1a, x, vs1b) >>= fxmy = MSG' (vs1a `M.union` vs2a, y, vs1b `M.union` vs2b)
      where MSG' (vs2a, y, vs2b) = fxmy x
-}

type MSG' = Either String

-- Information on the context which we are currently in
data MSGEnv = MSGEnv {
    msgLostWorkSharing :: Bool,
    msgMode :: MSGMode
  }

msgLoseWorkSharing :: MSG a -> MSG a
msgLoseWorkSharing = msgLoseWorkSharing' True

msgLoseWorkSharing' :: Bool -> MSG a -> MSG a
msgLoseWorkSharing' lost mx = MSG $ \e -> unMSG mx (e { msgLostWorkSharing = lost })

{-
-- I'm not sure if we want to float arbitrary stuff out of lambdas. Doing so does reduce
-- worst case allocation behaviour, but it does so at the cost of increased upfront allocations
-- and GC lifetimes.
--
-- At the moment my poor compromise is to only float out cheap looking stuff.
msgCheckFloatable :: Bool -> Bool -> MSG ()
msgCheckFloatable cheap_l cheap_r
  | cheap_l, cheap_r = return ()
  | otherwise        = MSG $ \e s -> (s, if msgLostWorkSharing e
                                          then Left "msgCheckFloatable"
                                          else Right ())
-}

-- We want to create as few new "common" vars as possible when MSGing. This state helps to achieve this:
--  * When MSG encounters the same pair of two (heap or stack bound) things, we reuse the same "common" var
--    to refer to them.
--- * This is good because it doesn't make sense for a pair to match in one place but fail to match in another.
--    By reusing the same common var in both places we ensure the same generalisation decision applies to all
--    identical use sites.
--  * We ensure we only create a finite number of common variables because there are only finitely many possible
--    pairs.
--  * When MSGing *type* information it is critical for correctness that all similar type pairs get generalised
--    to a single type lambda. Consider:
--      (:) ((,) Int  Bool) x ([] ((,) Int  Bool)) `msg`
--      (:) ((,) Word Char) x ([] ((,) Word Char))
--    We *can't* generalise to this:
--      a |-> Int    a |-> Word
--      b |-> Bool   b |-> Char
--      c |-> Int    c |-> Word
--      d |-> Bool   d |-> Char
--      (:) ((,) a b) x ([] ((,) c d))
--    Since the generalised term wouldn't type-check. Instead we must generalise to:
--      a |-> Int    a |-> Word
--      b |-> Bool   b |-> Char
--      (:) ((,) a b) x ([] ((,) a b))
--    Now everything is cool!
--
-- INVARIANT: none of the stuff in Pending is bound rigidly
data Pending = -- INVARIANT: the Vars on both sides are either both TyVars or both Ids
               PendingVar      Var       Var
               -- INVARIANT: none of the following three have variables on *both* sides
             | PendingType     Type      Type
             | PendingCoercion Coercion  Coercion
             -- | PendingTerm     AnnedTerm AnnedTerm

instance Outputable Pending where
    ppr (PendingVar      x_l  x_r)  = ppr x_l  <+> text "<-x->" <+> ppr x_r
    ppr (PendingType     ty_l ty_r) = ppr ty_l <+> text "<-t->" <+> ppr ty_r
    ppr (PendingCoercion co_l co_r) = ppr co_l <+> text "<-c->" <+> ppr co_r
    --ppr (PendingTerm     e_l  e_r)  = ppr e_l  <+> text "<-e->" <+> ppr e_r

data MSGState = MSGState {
    msgInScopeSet     :: InScopeSet,                                      -- We have to ensure all new vars introduced by MSG are distinct from each other
    msgKnownVars      :: VarEnv (VarEnv Var), {- partial loop in range -} -- INVARIANT: the "known" maps are inverse to the pending list, except that PendingTerms are not recorded in
    msgKnownTypes     :: TypeMap (TypeMap TyVar),                         -- a "known" map at all. We don't *want* them in one because we don't mant MSGing to increase work sharing!
    msgKnownCoercions :: CoercionMap (CoercionMap CoVar),                 -- INVARIANT: all Vars in the range have extra information that has *already* been MSGed
    msgLR             :: Pair MSGLRState,
    -- May only shrink:
    msgSuckStack      :: IM.IntMap (MSGU ()),
    -- May grow and then shrink, a bit chaotically:
    msgCommonHeap     :: PureHeap,
    msgCommonStack    :: Stack
  }

data MSGLRState = MSGLRState {
    -- May only shrink:
    msgLRSuckVar        :: VarEnv (MSGU ()), -- Can be called to ensure that the given var is in the individual state
    msgLRAvailableHeap  :: PureHeap,         -- Available heap
    msgLRAvailableStack :: Stack,            -- Available stack
    -- May only grow:
    msgLRHeap           :: PureHeap,         -- Certainly-individual heap
    msgLRStack          :: Stack,            -- Certainly-individual stack
    msgLRRenaming       :: Renaming          -- Certainly-individual renaming
  }

type StackInitM = State.State (InScopeSet, VarEnv (VarEnv Var))

-- A lot of things depend on being able to suck out the unique of a variable,
-- so ensure that the looped variables I put in the stack only have loops in fields where it is unavoidable
unloopVar :: Var {- varUnique and idScope are correct, every other field may be wrong -} -> Var {- loop -} -> Var {- partially looped -}
unloopVar x x_looped
  | isTyVar x = x `setVarType` varType x_looped
  | isId x    = x `setVarType` varType x_looped `setIdDetails` idDetails x_looped `lazySetIdInfo` idInfo x_looped
  | otherwise = error "unloopVar: TcTyVar"

-- NB: derived by just specialising the msgPend code for the case of two stack binders in the "obvious" way
msgPendStackBinder :: Var {- looped -} -> Var -> Var -> StackInitM (Var {- partial loop -}, MSG Var)
msgPendStackBinder x_looped x_l x_r = State.state $ \(iss, known) -> let x0 = zapVarExtraInfo x_r
                                                                         x1 = uniqAway iss x0
                                                                         x = x1 `unloopVar` x_looped
                                                                     in ((x, msgBndrExtras top_rn2 x1 x_l x_r), (extendInScopeSet iss x1, extendVarEnv known x_l (unitVarEnv x_r x)))

-- INVARIANT: incoming base variable has *no* extra information beyond Name and Type/Kind (which will be anyway overwritten)
msgPend :: RnEnv2 -> Var -> Pending -> MSG Var {- partial loop -}
msgPend rn2 x0 pending = MSG $ \e s0 -> case lookupUpdatePending s0 of
    Right x                       -> (s0, pure x)
    Left (mb_eq, binderise, mk_s) -> res
      where -- The use of s here is necessary to ensure we only allocate a given common var once
            extra_iss | Just eq <- mb_eq
                      , eq `elemInScopeSet` msgCommonHeapVars (msgMode e)
                      = emptyInScopeSet
                      | otherwise
                      = msgInScopeSet s0
            -- This use of rn2 is 1/2 of the story necessary to ensure new common vars don't clash with rigid binders
            x1 = uniqAway (rnInScopeSet rn2 `unionInScope` extra_iss) x0
            -- We *don't* need to uniqAway with the set of common variables (in e) because the msgInScopeSet
            -- was initialized to contain them all.

            -- NB: we make use of lazy programming to ensure that we can see the currently-pended common variable
            -- in the state even as we are binderising it. This is important since the variable's IdInfo might mention itself,
            -- as x_l and x_r will in fact be bound by the top-level letrec.
            s1 = mk_s x
            s2 = s1 { msgInScopeSet = extendInScopeSet (msgInScopeSet s1) x1 } -- NB: binderization *never* changes the unique -- exploit that to avoid a loop
            res = unMSG (msgLoseWorkSharing' False (binderise x1)) e s2 -- This thing will be bound in the top letrec, outside any lambdas
            (_, Right x_looped) = res
            x = x1 `unloopVar` x_looped
  where
    lookupUpdatePending :: MSGState
                        -> Either (Maybe Var,       -- Are both sides equal vars, and if so what are they equal to?
                                   Var -> MSG Var,  -- Produce a version of the variable suitable for use as a heap binder (with generalised info/type/kind + any specialisation work done if reqd)
                                   Var -> MSGState) -- How to update the initial state with the variable for this pending item (only the "known" maps change)
                                  Var
    lookupUpdatePending s = case pending of
      -- TODO: binder matching can legitimately fail, in which case we might want to create a common "vanilla"
      -- binder with no non-MSGable info, leaving the non-unifiable specs/rules to the generalised versions?
      PendingVar      x_l  x_r  -> fmapLeft (\upd -> (if x_l == x_r then Just x_r else Nothing, binderiseVars      x_l  x_r,  \x -> s { msgKnownVars      = upd x })) $ lookupUpdateVE (msgKnownVars s)      x_l  x_r
      PendingType     ty_l ty_r -> fmapLeft (\upd -> (Nothing,                                  binderiseTypes     ty_l ty_r, \a -> s { msgKnownTypes     = upd a })) $ lookupUpdateTM (msgKnownTypes s)     ty_l ty_r
      PendingCoercion co_l co_r -> fmapLeft (\upd -> (Nothing,                                  binderiseCoercions co_l co_r, \q -> s { msgKnownCoercions = upd q })) $ lookupUpdateTM (msgKnownCoercions s) co_l co_r
      --PendingTerm     e_l  e_r  -> Left              (Nothing,                                  binderiseTerms     e_l  e_r,  \_ -> s)
      -- NB: the fact that we don't memoise multiple occurrences of identical (term, term) pairs is very delicate from a termination
      -- perspective. You might imagine that you can easily make the PureHeap matcher get into a loop where the work list continually
      -- adds as many or more PendingTerm work items than it discharges.
      --
      -- Interestingly this doesn't seem to happen in practice. Consider:
      --   x |-> 1 : y       \       / a |-> 1 : (1 : b)
      --   y |-> 1 : (1 : x) | `msg` | b |-> 1 : a
      --   x                 /       \ a
      --
      -- After MSGing we get:
      --   d |-> 1 : e (x `msg` a)
      --   e |-> 1 : f (y `msg` 1 : b)
      --   f |-> 1 : g (1 : x `msg` b)
      --   g |-> d     (x `msg` a)
      --   d
      --
      -- It looks like *every* possible loop always goes through a (Var, Var) pair eventually,
      -- so we don't get non-termination!  I'm not quite sure how to prove this yet, though.

    binderiseVars x_l x_r x1 = do
      x <- msgBndrExtras rn2 x1 x_l x_r
      msgu $ specGenVars x (Pair x_l x_r)
      return x

    binderiseTypes ty_l ty_r a1 = do
      a <- liftM (a1 `setTyVarKind`) $ msgKind rn2 (typeKind ty_l) (typeKind ty_r)
      msgu $ genTypes a (Pair ty_l ty_r)
      return a

    binderiseCoercions co_l co_r q1 = do
      q <- liftM (q1 `setVarType`) $ msgType rn2 (coercionType co_l) (coercionType co_r)
      msgu $ genCoercions q (Pair co_l co_r)
      return q

    --binderiseTerms e_l e_r x = liftM (x `setVarType`) $ msgType rn2 (termType e_l) (termType e_r)

    fmapLeft f = either (Left . f) Right

-- Assumption: both input types do not MSG, are floatable, and one of the two is not a var
genTypes :: TyVar -> Pair Type -> MSGU ()
genTypes a ty_lrs = do
  -- Extend the renaming
  modify_ $ \s -> s { msgCommonHeap = M.insert a generalisedLambdaBound (msgCommonHeap s)
                    , msgLR = liftA2 (\ty_lr s_lr -> s_lr { msgLRRenaming = insertTypeSubst (msgLRRenaming s_lr) a ty_lr }) ty_lrs (msgLR s) }
  -- Ensure FVs are bound
  sucks2 $ fmap tyVarsOfType ty_lrs

-- Assumption: both input coercions do not MSG, are floatable, and one of the two is not a var
-- TODO: I could try to avoid generalisation when co_l or co_r is just a heap-bound variable. We could do this (in the same way as PendingTerm) by floating
-- the non-variable into a new heap binding which looks just like it was in the initial heap on the left/right and then matching the variable pair we are
-- left with as normal.
--
-- The only problem with this plan is that co_l and co_r are untagged, so I wouldn't know how to tag the new heap bindings. The best I can do given the current
-- supercompiler data types is to tag the new coercion HeapBindings with the tags of the *thing that they coerce*, which doesn't seem cool at all!
--
-- Doing this may also suffer from the standard problems with instance matching (see PendingTerm)
genCoercions :: CoVar -> Pair Coercion -> MSGU ()
genCoercions q co_lrs = do
  -- Extend the renaming
  modify_ $ \s -> s { msgCommonHeap = M.insert q generalisedLambdaBound (msgCommonHeap s)
                    , msgLR = liftA2 (\co_lr s_lr -> s_lr { msgLRRenaming = insertCoercionSubst (msgLRRenaming s_lr) q co_lr }) co_lrs (msgLR s) }
  -- Ensure FVs are bound
  sucks2 $ fmap tyCoVarsOfCo co_lrs

-- Assumption: both input vars do not MSG, are flexi
genVars, genVars' :: Var {- partial loop -} -> Generalised -> Pair Var -> MSGU ()
genVars x gen x_lrs = do
  -- Extend the renaming
  genVars' x gen x_lrs
  -- Ensure solitary FV is bound
  suck2 x_lrs
genVars' x gen x_lrs = modify_ $ \s -> s { msgCommonHeap = M.insert x (if gen then generalisedLambdaBound else lambdaBound) (msgCommonHeap s)
                                         , msgLR = liftA2 (\x_lr s_lr -> s_lr { msgLRRenaming = insertVarRenaming (msgLRRenaming s_lr) x x_lr }) x_lrs (msgLR s) }

-- Assumption: both input vars MAY YET MSG, are flexi
specGenVars :: Var {- partial loop -} -> Pair Var -> MSGU ()
specGenVars x (Pair x_l x_r) = do
    Pair mb_hb_l mb_hb_r <- flip fmap get $ \s -> liftA2 (\x_lr lr_s -> M.lookup x_lr (msgLRAvailableHeap lr_s)) (Pair x_l x_r) (msgLR s)
    sucks <- liftA2 Pair (suck' pFst x_l) (suck' pSnd x_r)
    let hb_r_gen = maybe False heapBindingGeneralised mb_hb_r
        gen = do genVars' x hb_r_gen (Pair x_l x_r)
                 Foldable.sequenceA_ sucks
    case (mb_hb_l, mb_hb_r) of
      (Just hb_l, Just hb_r)
        | Right hb_l_inj <- inject hb_l
        , Right hb_r_inj <- inject hb_r
        -> flip recoverM gen $ void $ mfix $ \hb -> do
             -- If they match, we need to make a common heap binding. We insert it into the heap *before* recursing
             -- because we want "gen" to be able to delete it even if we somehow recursively invoke "gen"
             modify_ $ \s -> s { msgCommonHeap = M.insert x hb (msgCommonHeap s) }
             -- Actually check for a match
             case (hb_l_inj, hb_r_inj) of
               (Just (let_bound_l, in_e_l), Just (let_bound_r, in_e_r))
                 | let_bound_l == let_bound_r
                 , not let_bound_r || (x_l == x_r && x_r == x) -- Note [MSGing let-bounds] (we have to check this even if matching RHSs because we need to choose a common binder that will be in scope on both sides)
                 -> do -- Ensure that we can't attempt to MSG either side's term again (if the term is cheap)
                       -- Also overwrite the sucker so it generalises this "x" before doing normal sucking
                       modify_ $ \s -> s { msgLR = liftA3 (\x_lr in_e_lr s_lr -> if termIsCheap (snd in_e_lr) then s_lr else s_lr { msgLRAvailableHeap = M.delete x_lr (msgLRAvailableHeap s_lr)
                                                                                                                                  , msgLRSuckVar = extendVarEnv (msgLRSuckVar s_lr) x_lr gen })
                                                          (Pair x_l x_r) (Pair in_e_l in_e_r) (msgLR s) }
                       -- Finally, the point: MSG together the heap binding RHSes
                       in_e <- flip recoverM (msgIn renameAnnedTerm annedTermFreeVars msgTerm top_rn2 in_e_l in_e_r) $ do
                         -- Optimisation: attempt to match using the "common heap vars" trick.
                         -- If this fails we can always try to match in a legit manner, but I
                         -- expect this to shortcut the full term matching route almost all of
                         -- the time if this guard succeeds.
                         --
                         -- The reason that this hack should almost always work is because I
                         -- expect common-heap stuff to be mostly matched against *itself*
                         -- first, so the assigned "common" var will be the same as the *input*
                         -- variable. This allows us to safely use the *right hand* term as the
                         -- *common* HeapBinding without any sort of changes to variables.
                          guard "msgLoop(optimisation): LR binder mismatch" $ x_l == x_r
                          mm <- MSG $ \e s -> (s, pure (msgMode e))
                          guard "msgLoop(optimisation): LR binder fresh" $ x_r `elemInScopeSet` msgCommonHeapVars mm
                          let l_fvs = inFreeVars annedTermFreeVars in_e_l
                              r_fvs = inFreeVars annedTermFreeVars in_e_r
                          guard "msgLoop(optimisation): LR FV mismatch" $ l_fvs == r_fvs
                          Foldable.mapM_ (\x -> msgFlexiVar top_rn2 x x >>= \x' -> guard "msgLoop: shortcut" (x' == x) >> return ()) r_fvs
                          return in_e_r -- Right biased
                       return $ (if let_bound_r then letBound else internallyBound) in_e
               (Nothing, Nothing)
                 | x_l == x_r && x_r == x -- Note [MSGing let-bounds]
                 -> return hb_r -- Right biased
               _ -> fail "msgLoop: non-unifiable heap bindings"
      _ -> genVars x hb_r_gen (Pair x_l x_r)

lookupUpdateVE :: VarEnv (VarEnv Var) -> Var -> Var -> Either (Var -> VarEnv (VarEnv Var)) Var
lookupUpdateVE mp x_l x_r = case mb_x_l_map >>= flip lookupVarEnv x_r of
      Nothing -> Left $ \x -> extendVarEnv mp x_l (maybe (unitVarEnv x_r x) (\x_l_map -> extendVarEnv x_l_map x_r x) mb_x_l_map)
      Just x  -> Right x
    where mb_x_l_map = lookupVarEnv mp x_l

lookupUpdateTM :: TrieMap m => m (m Var) -> Key m -> Key m -> Either (Var -> m (m Var)) Var
lookupUpdateTM mp it_l it_r = case mb_it_l_map >>= lookupTM it_r of
      Nothing -> Left $ \a -> extendTM it_l (extendTM it_r a (mb_it_l_map `orElse` emptyTM)) mp
      Just a  -> Right a
    where mb_it_l_map = lookupTM it_l mp
          extendTM k v m = alterTM k (\_ -> Just v) m


class Applicative t => Error t where
    recover :: t a -> ((forall b. t b) -> a) -> a
    explain :: String -> t a

instance Error Identity where
    recover (I x) _ = x
    explain = error

instance Error Maybe where
    recover Nothing  k = k Nothing
    recover (Just x) _ = x
    explain _ = Nothing

instance Error (Either String) where
    recover (Left msg) k = k (Left msg)
    recover (Right x)  _ = x
    explain = Left


type MSG  = MSGT MSG'
type MSGU = MSGT Identity

newtype MSGT t a = MSG { unMSG :: MSGEnv -> MSGState -> (MSGState, t a) }

runMSG :: MSGEnv -> MSGState -> MSGT t a -> (MSGState, t a)
runMSG e s mx = unMSG mx e s

instance Error t => Functor (MSGT t) where
    fmap = liftM

instance Error t => Applicative (MSGT t) where
    pure = return
    (<*>) = ap

instance Error t => Monad (MSGT t) where
    return x = MSG $ \_ s -> (s, pure x)
    mx >>= fxmy = MSG $ \e s ->
      let (s', tx) = unMSG mx e s
      in liftA (\x -> unMSG (fxmy x) e s') tx `recover` \fl -> (s', fl)
    fail msg = MSG $ \_ s -> (s, explain msg)

instance Error t => MonadFix (MSGT t) where
    mfix fxmx = MSG $ \e s -> let ~(s', tx) = unMSG (fxmx x) e s
                                  x = tx `recover` \_ -> error "mfix"
                              in (s', tx)

recoverM :: (Error t, Applicative t') => MSGT t a -> MSGT t' a -> MSGT t' a
recoverM mx1 mx2 = MSG $ \e s0 -> let (s1, tx) = unMSG mx1 e s0
                                  in fmap (\x -> (s1, pure x)) tx `recover` \_ -> unMSG mx2 e s1


-- INVARIANT: neither incoming Var may be bound rigidly (rigid only matches against rigid)
msgFlexiVar :: RnEnv2 -> Var -> Var -> MSG Var
msgFlexiVar rn2 x_l x_r = msgPend rn2 (zapVarExtraInfo x_r) (PendingVar x_l x_r)

-- INVARIANT: neither incoming Type can refer to something bound rigidly (can't float out things that reference rigids)
msgGeneraliseType :: RnEnv2 -> Type -> Type -> MSG TyVar
msgGeneraliseType rn2 ty_l ty_r = msgPend rn2 a (PendingType ty_l ty_r)
  where 
    -- Unbiased choice of base variable: only one side may be a variable, kind is MSGed at binding site
    -- NB: TyVars have no extra information, so there is nothing to zap!
    a = (getTyVar_maybe ty_l `mplus` getTyVar_maybe ty_r) `orElse` mkTyVar (mkSystemVarName uniq (fsLit "genty")) (typeKind ty_r)
    uniq = mkUniqueGrimily (hashType (mkTyConApp pairTyCon [ty_l, ty_r])) -- NB: pair might not be kind correct, but who cares?

-- INVARIANT: neither incoming Coercion can refer to something bound rigidly (don't want to lambda-abstract to float out things that reference rigids)
msgGeneraliseCoercion :: RnEnv2 -> Coercion -> Coercion -> MSG CoVar
msgGeneraliseCoercion rn2 co_l co_r = msgPend rn2 q (PendingCoercion co_l co_r)
  where
    -- Unbiased choice of base variable: only one side may be a variable, type is MSGed at binding site
    q = (fmap zapIdExtraInfo (getCoVar_maybe co_l `mplus` getCoVar_maybe co_r)) `orElse` mkSysLocal (fsLit "genco") uniq (coercionType co_r)
    uniq = mkUniqueGrimily (hashCoercion (mkTyConAppCo pairTyCon [co_l, co_r])) -- NB: pair might not be type correct, but who cares?

{-
-- INVARIANT: neither incoming AnnedTerm can refer to something bound rigidly (don't want to lambda-abstract to float out things that reference rigids)
msgGeneraliseTerm :: RnEnv2 -> AnnedTerm -> AnnedTerm -> MSG Id
msgGeneraliseTerm rn2 e_l e_r = msgPend rn2 x (PendingTerm e_l e_r)
  where
    -- Unbiased choice of base variable: only one side may be a variable, type is MSGed at binding site
    x = (fmap zapIdExtraInfo (getVar_maybe e_l `mplus` getVar_maybe e_r)) `orElse` mkSysLocal (fsLit "genterm") uniq (termType e_r)
    uniq = mkUniqueGrimily (hashExpr (Core.mkConApp (tupleCon BoxedTuple 2) [GHC.termToCoreExpr e_l, GHC.termToCoreExpr e_r])) -- NB: pair might not be type correct, but who cares?

    getVar_maybe e = case extract e of
      Var x              -> Just x
      -- TODO: value generalisation (need to check for update frames at stack top)
      --Value (Indirect x) -> Just x -- Because we sneakily reuse msgGeneraliseTerm for values as well
      _                  -> Nothing
-}

zapVarExtraInfo :: Var -> Var
zapVarExtraInfo x | isId x    = zapIdExtraInfo x
                  | otherwise = x

zapIdExtraInfo :: Id -> Id
zapIdExtraInfo x = mkLocalId (varName x) (idType x)

{-
newtype MSG a = MSG { unMSG :: Either String (MSG' a) }
--newtype MSG a = MSG { unMSG :: Maybe a }

instance Functor MSG where
    fmap = liftM

instance Monad MSG where
    return = MSG . return
    mx >>= fxmy = MSG $ unMSG mx >>= (unMSG . fxmy)
    fail s = MSG $ Left s
    --fail s = MSG $ fail s

instance MonadFix MSG where
    mfix xmy = MSG (mfix (unMSG . xmy))
-}

guard :: String -> Bool -> MSG ()
guard _   True  = return ()
guard msg False = fail msg

runMSG' :: MSG' a -> Maybe a
runMSG' (Right x)   = Just x
runMSG' (Left _msg) = {- trace ("msg " ++ _msg) -} Nothing
--runMSG = unMSG

instance Error t => MonadPlus (MSGT t) where
    mzero = fail "mzero"
    mplus = recoverM


data MSGMode = MSGMode {
    msgCommonHeapVars :: InScopeSet
  }

type MSGResult = (Pair (Deeds, Heap, Renaming, Stack), (Heap, Stack, Anned QA))

-- Note [Renaming via MSG]
-- ~~~~~~~~~~~~~~~~~~~~~~~
--
-- If s1 can be renamed/instantiated to s2, then taking the MSG of the two should yield:
--  1. On the left:
--    a) An empty stack
--    b) A heap containing only lambdaBounds
--    c) A Renaming that maps TyVar->TyVar and CoVar->CoVar, and which is invertible
--       (we can't accept [x |-> z, y |-> z] because the right part of the MSG might
--        instantiate x and y to two different things)
--  2. On the right:
--    a) An empty stack, or one which is an allowed instantiation
--    b) A heap containing only lambdaBounds, or one which is an allowed instantiation
--    c) A Renaming. We can always allow TyVar->Type and CoVar->Coercion mappings because
--       they are not computationally relevant. We could optionally forbid non-invertible
--       renamings like [x |-> z, y |-> z] because it means that two variables are shared
--       here which weren't in the original term. However, this is likely not important in practice.
--
-- If s1 and s2 match to give type generalisation info, we expect this from the MSG:
--  1. On the left:
--    a) An empty stack
--    b) A heap containing only lambdaBounds
--  2. On the right:
--    a) An empty stack
--    b) A heap containing only lambdaBounds
--
-- The form of the Renaming is unimportant (as long as we don't care about whether 2 variables
-- are shared or not) because we are always happy to generalise away Type/Coercion info, and
-- we're going to satisfy the demand for the States on both sides by driving the (instantiable)
-- common State.

-- Note [MSG and common heap vars]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- MSG can easily break the "common heap vars" optimisation
-- To make sure that it continues to work we need the property that:
--  1. For every variable bound in the outgoing heap/stack of the {left,right} and common outgoing states..
--  2. ..IF it is in the {left,right} incoming InScopeSet..
--  3. ..then the thing it is bound in the {left,right} incoming state MUST have the same meaning as the new binding.
--
-- Things which are definitely safe in my current approach:
--  1. New left/right binders introduced for floated terms: they are chosen to be distinct from the existing left/right InScopeSets
--  2. Elements of the left/right heap copied into each individual heap: clearly these are already in scope
--  3. Suffixes of the left/right stack copied into each individual stack: clearly these are already in scope
--
-- Things which are problematic:
--  1. Common heap variables chosen for PendingVars/PendingTerms
--  2. Common stack variables resulting from coupling update frames
--
-- In both of the problematic cases, it is sufficient to ensure that either:
--  1. The common variable gets a name not present in *either* input InScopeSet, OR
--  2. If the common name *is* present in the left/right InScopeSet, then it is *identical* to the
--     left/right occurrence from which it was derived (because this is sufficient for us to be satisified
--     that the meanings of what they will get bound to will be the same)
--
-- So if I have {f, g, x} InScope in the left input and {f, g, a} on the right, and:
--   a) I (msgFlexiVar f f) to f, then everything is kosher
--   b) I (msgFlexiVar f g) to f/g, then I have to choose a brand new name instead (e.g. b)
--   c) I (msgFlexiVar x a) to x/a, then things are OK (because each of x and a are only InScope on one side)
--   d) msgFlexiVar returns anything outside the set {f, g, x, a}, then everything is OK
--
-- Basically we have to watch out for the case where the returned name is present in *both* input
-- InScopeSets *and* the left/right + common vars are not all identical.
--
-- (FIXME: there is still an outstanding problem: it could happen that a parent has x letBound but
-- generalises that away in a child. Then a grandchild makes it letBound again: this binding would compare equal
-- to the parent x even though the meaning may be totally different! But this is not relevant at the moment
-- because the set of letBound things is fixed once and for all at the beginning of supercompilation.)


-- Note [MSGing let-bounds]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We may never common up let-bound HeapBindings from two sides if those bindings:
--   a) Are referred to by different names on the two side (which means they actually bind to two different things), OR
--   b) Have a common binder that was renamed *at all*
--
-- So what we would really like to GUARANTEE is that if we EVER match a pair of variables (x_l, x_r) where x_l == x_r == x,
-- the MSGed variable is simply x with no extra faffing around. Note that this coincides with the goal we described
-- in Note [MSG and common heap vars], and we can achieve both simultaneously by adopting a new scheme:
--   * We behave as if we preinitialized the pending var map with (x, x) |-> x mappings for all x in the common InScopeSet.
--     We sort do this "lazily" because we don't want to have to discharge e.g. a (++) `msg` (++) obligation when the (++)
--     variable wasn't even mentioned in the term of interest.
--   * When name-matching let-bound stuff, we insist on (left, right, common) triples all being *exactly* the same variable.
--     So we can only name-match let-bounds if the let-bound name was in the input common variable set.
--
-- An interesting point here is that let-bounds are often *global* Ids, and the standard code path for msgFlexiVar always
-- produces a *local* Id. We need to avoid that happening, because in the case where the resulting common vars end up letBound
-- there will be no visible binding site to turn that local Id back into a global one! By preinitalizing, we can put the
-- global variable in the range of the mapping and avoid any problems.

msgMaybe :: MSGMode -- ^ How to match
         -> State   -- ^ Tieback semantics
         -> State   -- ^ This semantics
         -> Maybe MSGResult -- ^ Renaming from left to right
msgMaybe mm s_l s_r = {- s_l `seq` s_r `seq` (trace "msgMaybe" $ (\res -> res `seq` trace "msgMaybe'" res) $ -} runMSG' (msg mm s_l s_r)


data MSGMatchResult = RightIsInstance   Heap Renaming Stack
                    | RightGivesTypeGen Renaming State Renaming

msgMatch :: InstanceMatching -> MSGResult -> Maybe MSGMatchResult
msgMatch inst_mtch (Pair (_, Heap h_l _, rn_l, k_l) (deeds_r, heap_r@(Heap h_r _), rn_r, k_r), (heap@(Heap _ ids), k, qa))
  -- Try to detect instantiation first
  --  1) Is the left-hand renaming invertible?
  -- FIXME: trim the renaming to the FVs of the common state so that "dead" substitutions don't cause the inversion check to fail
  | Just rn_l_inv@(rn_l_inv_xs, rn_l_inv_as, rn_l_inv_qs) <- invertRenaming ids rn_l
  --  2) Is the left-hand stack empty, and if has been instantiated on the right, was that valid?
  , Loco gen_k_l <- k_l
  , case k_r of Loco _ -> True
                _      -> mayInstantiate inst_mtch gen_k_l
  --  3) Is the left-hand heap empty of anything except lambdaBounds, and if it has been instantiated on the right, was that valid?
  --     NB: we can safely ignore stack-bound variables because stack-bound vars are only matched against stack-bound vars, heap-bound
  --     ones are only matched against heap-bound ones, and we don't have any generalisation flag to check on update frames.
  , let k_r_bvs = stackBoundVars k_r
        heap_non_instantiating x_r = case M.lookup x_r h_r of
                         Nothing | x_r `elemVarSet` k_r_bvs -> False -- Instantiating with an update-frame bound thing is *not* OK, especially since this might be a leading update frame with a value in the focus!
                         Just hb_r -> isJust (heapBindingLambdaBoundness hb_r)
                         _ -> panic "msgMatch: variable unbound on right" -- (ppr rn_l_inv $$ ppr x_r $$ pPrintFullState fullStatePrettiness (maxBound, heap, k, qa) $$ ppr (h_l, rn_l, k_l) $$ ppr (h_r, rn_r, k_r))
  -- NB: just because we have (x_l, hb_l) in the heap *DOESN'T MEAN* there is a x/x_r corresponding to this x_l because we might have:
  --  x |-> 1
  --  y |-> Just x   `msg`  a |-> Just b
  --  Just y                Just a
  -- Note that the left-hand heap binder x_l will not be in the range of the rn_l (and hence not in the domain of the rn_l_inv).
  , flip all (M.toList h_l) $ \(x_l, hb_l) -> case heapBindingLambdaBoundness hb_l of
                                                Nothing -> False
                                                Just gen_l -> mayInstantiate inst_mtch gen_l || case () of
                                                                      () | isCoVar x_l, Just q <- lookupVarEnv rn_l_inv_qs x_l, let co_r = lookupCoVarSubst rn_r q -> tYPE_GEN || maybe False heap_non_instantiating (getCoVar_maybe co_r)
                                                                         | isId    x_l, Just x <- lookupVarEnv rn_l_inv_xs x_l, let x_r  = renameId rn_r x         -> heap_non_instantiating x_r
                                                                         | isTyVar x_l, Just a <- lookupVarEnv rn_l_inv_as x_l, let ty_r = lookupTyVarSubst rn_r a -> tYPE_GEN || isJust (getTyVar_maybe ty_r)
                                                                         -- This case occurs when a heap binding is pulled in on one side by "sucks" but without
                                                                         -- being paired with one on the other side (e.g. 'x' in the example above). If this happens
                                                                         -- we'll just say True, though we'll probably reject the instantiation eventuall when we come
                                                                         -- to the binding on this side that caused the pulling-in to happen.
                                                                         | otherwise -> True
                                                                         -- TODO: what if we "instantiate" with some IdInfo on the left? Should that be disallowed?
  = Just (RightIsInstance heap_r (composeRenamings rn_l_inv rn_r) k_r)

  -- Now look for type generalisation information
  --  1) Are both stacks empty?
  | isStackEmpty k_l
  , isStackEmpty k_r
  --  2) Do both heaps only contain lambdaBounds?
  , isPureHeapEmpty h_l
  , isPureHeapEmpty h_r
  --  3) Are both type substitutions non-trivial? If they are trivial then we risk not making any progress after we generalise away the type info
  , let state = (deeds_r, heap, k, qa)
        fvs = stateFreeVars state
  , isTypeRenamingNonTrivial rn_l fvs
  , isTypeRenamingNonTrivial rn_r fvs
  = Just (RightGivesTypeGen rn_l state rn_r)

  -- No information gained in this case :-(
  | otherwise
  = Nothing

isTypeRenamingNonTrivial :: Renaming -> FreeVars -> Bool
isTypeRenamingNonTrivial rn fvs = (\f -> foldVarSet f False fvs) $ \x rest -> (isTyVar x && isNothing (getTyVar_maybe (lookupTyVarSubst rn x))) || rest

msg :: MSGMode -> State -> State -> MSG' MSGResult
msg = {- pprTrace "examples" (example1 $$ example2) -} msg'
msg' mm (deeds_l, heap_l, k_l, qa_l) (deeds_r, heap_r, k_r, qa_r) = -- (\res -> traceRender ("msg", M.keysSet h_l, residualiseDriveState (Heap h_l prettyIdSupply, k_l, in_e_l), M.keysSet h_r, residualiseDriveState (Heap h_r prettyIdSupply, k_r, in_e_r), res) res) $
    liftM (first (liftA2 (\deeds (heap, rn, k) -> (deeds, heap, rn, k)) (Pair deeds_l deeds_r))) $ msgLoop mm (heap_l, heap_r) (qa_l, qa_r) (k_l, k_r)

msgAnned :: (Tag -> b -> Anned b) -> (Tag -> a -> Tag -> a -> MSG b)
         -> Anned a -> Anned a -> MSG (Anned b)
msgAnned anned f a_l a_r = liftM (anned (annedTag a_r)) $ f (annedTag a_l) (annee a_l) (annedTag a_r) (annee a_r) -- Right biased

msgQA, msgQA' :: RnEnv2 -> Tag -> QA -> Tag -> QA -> MSG QA
msgQA rn2 tg_l qa_l tg_r qa_r = msgQA' rn2 tg_l qa_l tg_r qa_r {- `mplus` do
    guardFloatable "msgQA" qaFreeVars rn2 qa_l qa_r
    -- NB: a QA is always cheap, so no floatability check
    liftM Question $ msgGeneraliseTerm rn2 (annedTerm tg_l (qaToAnnedTerm' (rnInScopeSet rn2) qa_l)) (annedTerm tg_r (qaToAnnedTerm' (rnInScopeSet rn2) qa_r)) -}

msgQA' rn2 _    (Question x_l') _    (Question x_r') = liftM Question $ msgVar rn2 x_l' x_r'
msgQA' rn2 tg_l (Answer in_v_l) tg_r (Answer in_v_r) = liftM Answer $ msgAnswer rn2 tg_l in_v_l tg_r in_v_r
msgQA' _ _ _ _ _ = fail "msgQA"

msgAnswer :: RnEnv2 -> Tag -> Answer -> Tag -> Answer -> MSG Answer
msgAnswer rn2 tg_l a_l tg_r a_r = msgIn renameAnnedValue' annedValueFreeVars' (\rn2 v_l v_r -> msgValue rn2 tg_l v_l tg_r v_r) rn2 a_l a_r

msgCoerced :: (RnEnv2 -> Tag -> a -> Tag -> a -> MSG b)
           -> RnEnv2 -> Tag -> Coerced a -> Tag -> Coerced a -> MSG (Coerced b)
msgCoerced f rn2  out_tg_l (Uncast,           x_l)  out_tg_r (Uncast,           x_r) = liftM ((,) Uncast) $ f rn2 out_tg_l x_l out_tg_r x_r
msgCoerced f rn2 _out_tg_l (CastBy co_l tg_l, x_l) _out_tg_r (CastBy co_r tg_r, x_r) = liftM2 (\co b -> (CastBy co tg_r, b)) (msgCoercion rn2 co_l co_r) (f rn2 tg_l x_l tg_r x_r) -- Right biased
msgCoerced _ _ _ _ _ _ = fail "msgCoerced"

-- Note [Kind MSG and subkinding]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Perhaps suprisingly, we have:
--
--  (\(x :: Int) -> x) `msg` (\(y :: Num Int) -> y)
-- =/=
-- [*/k,Int/a]  k :: BOX |-> \lambda  [Constraint/k, Num Int/a]
--              a :: k |-> \lambda
--              ((\x :: a) -> x)
--
-- In this case there is no code generation problem (both * and
-- Constraint are represented by a pointer) but Lint rightly rejects
-- it on the basis that the kind of the first argument to (->) must
-- be a subkind of OpenTypeKind, not a raw variable!
--
-- If there was a kind which was the lub of * and Constraint I
-- think we could solve this by returning that lub when they mismatch.
-- But in the absence of that we will have to just fail.
--
-- Not being able to MSG * and Constraint has far-reaching consequences.
-- Imagine that we want (* -> *) `msg` (* -> Constraint).
-- If we have that * `msg` Constraint fails then the result will be
--   [* -> */k] k [* -> Constraint/k]
-- But this is BAD since we might be doing:
--   /\(a :: * -> *) (b :: *) -> f @(a b -> Int)
--    `msg`
--   /\(a :: * -> Constraint) (b :: *) -> f @(a b -> Int)
-- And if the result is:
--   /\(k :: BOX) (a :: k) (b :: *) -> f @(a b -> Int)
-- Then the result will be ill-kinded!
--
-- CONCLUSION: kind MSG is just too hard to get right because subkinding
-- and the kind invariant screws everything up. Don't do it.
--
-- Note [Kind MSG and the kind invariant]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- It is illegal to MSG to a state that requires us to abstract over a
-- kind that does not satisfy the kind invariant (see TypeRep.lhs,
-- the Note [The kind invariant]). Examples would be:
--
--  (\(x :: Int#) -> x) `msg` (\(y :: Char#) -> y)
-- =/=
--  [Int#/a]  a :: # |-> \lambda  [Char#/a]
--            (\(x :: a) -> x)
--
--  (\(x :: State# Int) -> x) `msg` (\(y :: Array# Int) -> x)
-- =/=
--  [State#/a]  a :: * -> # |-> \lambda  [Array#/a]
--              ((\x :: a Int) -> x)
--
-- The above two examples don't even require kind abstraction to
-- screw things up. Things can get worse though:
--
--  (\(x :: Int#) -> x) `msg` (\(y :: (# #)) -> y)
-- =/=
--  [#/k,Int#/a]  k :: BOX |-> \lambda  [(#)/k,Char#/a]
--                a :: k |-> \lambda
--                ((\x :: a) -> x)
--
-- To solve this, I check in msgType that the MSG result satisfied the
-- predicate 'canInstantiateKindVarWithKind'. Furthermore, when I did
-- kind-MSG, I would check 'canAbstractOverTyVarOfKind' (a synonym) in
-- msgTyVarBndrExtras', to ensure that kind variables were never
-- instantiated with a bad kind.
--
-- Note [Kind MSG and coupling types]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We clearly cannot construct a generalisation that maps the same type
-- variable to two types which are "similar" at the root. For example:
--  (\(x :: Int -> Int) (y :: Int) -> x y) `msg` (\(x :: Bool -> Bool) (y :: Bool) -> x y)
-- =/=
--  [Int->Int/a,Int/b]  a :: * |-> \lambda  [Bool->Bool/a,Bool/b]
--                      b :: * |-> \lambda
--                      (\(x :: a) (y :: b) -> x y)
--
-- Since the resulting generalised term is not necessarily type correct.
-- According to my proof (see the thesis), these pairs of types are "similar":
--  (T, T)                         [for all type constructors T]
--  (ty1l ty2l, ty1r ty2r)         [for all types ty1l ty2l ty1r ty2r]
--  (forall a. tyl, forall b. tyr) [for all types tyl tyr]
--
-- You might think we can ensure that the first two pairs are never generated by simply
-- always including the tycon/tyapp in the generalised part of the term. However,
-- the last case screws everything up because we can't unconditionally include the "forall"
-- part of the type into the generalised part (for variable capture reasons), which
-- means that things like this must be true:
--  T (forall a. a) `msg` T (forall a. Int)
-- =/=
--  [forall a. a/b]  b :: * |-> \lambda  [forall a. Int/b]
--                   T b
-- =/= 
--  [T (forall a. a)/b]  b :: * |-> \lambda  [T (forall a. Int)/b]
--                       b
--
-- In order to ensure we don't inadvertently return the last possibility, we need to check
-- for coupling type applications etc before allows generalisation as well.
msgKind :: RnEnv2 -> Kind -> Kind -> MSG Kind
msgKind = msgType

canAbstractOverTyVarOfKind :: Kind -> Bool
canAbstractOverTyVarOfKind = ok
  where
    -- TODO: I'm not 100% sure of the correctness of this check
    -- In particular, I don't think we need to check for non-conforming
    -- kinds in "negative" positions since they would only appear if the
    -- definition site had erroneously abstracted over a non-conforming
    -- kind. For example, this *should* never be allowed:
    --   data Foo (a :: * -> #) = Bar (a Int)
    --   Foo :: (* -> #) -> *
    --   Bar :: forall (a :: * -> #). a Int -> Foo a
    ok k | isOpenTypeKind k || isUbxTupleKind k || isArgTypeKind k || isUnliftedTypeKind k = False
    ok (TyVarTy _)     = True -- This is OK because kinds dont get generalised, and we assume all incoming kind instantiations satisfy the kind invariant
    ok (AppTy k1 k2)   = ok k1 && ok k2
    ok (TyConApp _ ks) = all ok ks
    ok (FunTy k1 k2)   = ok k1 && ok k2
    ok (ForAllTy _ k)  = ok k
    ok (LitTy _)       = True

-- NB: we don't match FunTy and TyConApp literally because
-- we can do better MSG for combinations like:
--   Either Int Char `msg` Maybe Char
-- We get:
--   a |-> Either Int        a |-> Maybe
--   a Char
-- Rather than the trivial MSG we would otherwise get if we insisted
-- on matching TyConApp constructors exactly:
--   a |-> Either Int Char   a |-> Maybe Char
--   a
--
-- NB: we don't do anything stupid like introduce a TyVar due to
-- generalisation at the superkind level because the only term
-- at the superkind level is Box, which always MSGes into itself.
msgType :: RnEnv2 -> Type -> Type -> MSG Type
msgType rn2 ty_l ty_r = case checkEqual (isKindTy ty_l) (isKindTy ty_r) of
   -- Ensures that we don't try to match kinds vs types or vice-versa.
   -- Note that because of some punning, MSGing them together won't necessarily
   -- fail at the root. For example, MSGing (* -> *) and (Int -> Int) will give
   -- (a -> a) since FunTy is a pun (similar with promoted DataCons/TyCons).
  Nothing -> fail "msgType: mismatched kinds/superkinds"
   -- We can MSG as normal now: msgType' itself will decide when generalisation is
   -- allowable, making use of the boolean argument to prevent any kind generalisation.
   -- We decided to never generalise kinds, because it leads to various issues:
   -- see Note [Kind MSG and subkinding] and Note [Kind MSG and the kind invariant]
  Just are_kinds -> msgType' are_kinds rn2 ty_l ty_r


-- INVARIANT: both sides are at the same level (i.e. both types or both kinds)
msgType' :: Bool -> RnEnv2 -> Type -> Type -> MSG Type
msgType' _         rn2 (TyVarTy x_l)         (TyVarTy x_r)         = liftM TyVarTy $ msgVar rn2 x_l x_r -- NB: if this fails, one of the two sides is unfloatable, so don't try to generalise
msgType' are_kinds rn2 (AppTy ty1_l ty2_l)   (AppTy ty1_r ty2_r)   = liftM2 mkAppTy (msgType' are_kinds rn2 ty1_l ty1_r) (msgType rn2 ty2_l ty2_r) -- NB: arguments not necessarily at same level, but type constructor must be
msgType' _         _   (TyConApp tc_l [])    (TyConApp tc_r [])    | tc_l == tc_r = return (TyConApp tc_r [])
msgType' are_kinds rn2 (TyConApp tc_l tys_l) (TyConApp tc_r tys_r) | not (null tys_l) || not (null tys_r) = msgType' are_kinds rn2 (foldl AppTy (TyConApp tc_l []) tys_l) (foldl AppTy (TyConApp tc_r []) tys_r)
msgType' are_kinds rn2 (FunTy ty1_l ty2_l)   (FunTy ty1_r ty2_r)   = msgType' are_kinds rn2 ((TyConApp funTyCon [] `AppTy` ty1_l) `AppTy` ty2_l) ((TyConApp funTyCon [] `AppTy` ty1_r) `AppTy` ty2_r)
msgType' are_kinds rn2 (ForAllTy a_l ty_l)   (ForAllTy a_r ty_r)   = msgTyVarBndr ForAllTy rn2 a_l a_r $ \rn2 -> msgType' are_kinds rn2 ty_l ty_r
msgType' _         _   (LitTy l_l)           (LitTy l_r)           | l_l == l_r = return (LitTy l_r)
msgType' are_kinds rn2 ty_l ty_r
  | are_kinds = fail "msgType: no generalisation at kind level"
  | not (canAbstractOverTyVarOfKind (typeKind ty_l')) = fail "msgType: bad kind for type generalisation" -- NB: legit to just check left hand side because kinds are ungeneralised, so it will eventualy be checked that it exactly matches the right
  | otherwise = do guardFloatable "msgType" tyVarsOfType rn2 ty_l' ty_r'
                   liftM TyVarTy (msgGeneraliseType rn2 ty_l' ty_r')
  where ty_l' = repair ty_l
        ty_r' = repair ty_r
        -- I intentionally call msgType' with types that do not obey the Type invariants.
        -- However, if I let these broken types leak out Bad Things will happens!
        repair ty | (TyConApp tc [], tys) <- splitAppTys ty = mkTyConApp tc tys
                  | otherwise                               = ty

msgCoercion, msgCoercion' :: RnEnv2 -> Coercion -> Coercion -> MSG Coercion
msgCoercion rn2 co_l co_r = msgCoercion' rn2 co_l co_r `mplus` do
    guardFloatable "msgCoercion" tyCoVarsOfCo rn2 co_l co_r
    liftM CoVarCo $ msgGeneraliseCoercion rn2 co_l co_r

msgCoercion' rn2 (Refl ty_l)              (Refl ty_r)              = liftM Refl $ msgType rn2 ty_l ty_r
msgCoercion' _   (TyConAppCo tc_l [])     (TyConAppCo tc_r [])     = guard "msgCoercion: TyConAppCo" (tc_l == tc_r) >> return (TyConAppCo tc_r [])
msgCoercion' rn2 (TyConAppCo tc_l cos_l)  (TyConAppCo tc_r cos_r)  = msgCoercion rn2 (foldl AppCo (TyConAppCo tc_l []) cos_l) (foldl AppCo (TyConAppCo tc_r []) cos_r)
msgCoercion' rn2 (AppCo co1_l co2_l)      (AppCo co1_r co2_r)      = liftM2 mkAppCo (msgCoercion rn2 co1_l co1_r) (msgCoercion rn2 co2_l co2_r)
msgCoercion' rn2 (ForAllCo a_l co_l)      (ForAllCo a_r co_r)      = msgTyVarBndr ForAllCo rn2 a_l a_r $ \rn2 -> msgCoercion rn2 co_l co_r
msgCoercion' rn2 (CoVarCo a_l)            (CoVarCo a_r)            = liftM CoVarCo $ msgVar rn2 a_l a_r
msgCoercion' rn2 (AxiomInstCo ax_l cos_l) (AxiomInstCo ax_r cos_r) = guard "msgCoercion: AxiomInstCo" (ax_l == ax_r) >> liftM (AxiomInstCo ax_r) (zipWithEqualM (msgCoercion rn2) cos_l cos_r)
msgCoercion' rn2 (UnsafeCo ty1_l ty2_l)   (UnsafeCo ty1_r ty2_r)   = liftM2 UnsafeCo (msgType rn2 ty1_l ty1_r) (msgType rn2 ty2_l ty2_r)
msgCoercion' rn2 (SymCo co_l)             (SymCo co_r)             = liftM SymCo $ msgCoercion rn2 co_l co_r
msgCoercion' rn2 (TransCo co1_l co2_l)    (TransCo co1_r co2_r)    = liftM2 TransCo (msgCoercion rn2 co1_l co1_r) (msgCoercion rn2 co2_l co2_r)
msgCoercion' rn2 (NthCo i_l co_l)         (NthCo i_r co_r)         = guard "msgCoercion: NthCo" (i_l == i_r) >> liftM (NthCo i_r) (msgCoercion rn2 co_l co_r)
msgCoercion' rn2 (InstCo co_l ty_l)       (InstCo co_r ty_r)       = liftM2 InstCo (msgCoercion rn2 co_l co_r) (msgType rn2 ty_l ty_r)
msgCoercion' _ _ _ = fail "msgCoercion"

msgTerm :: RnEnv2 -> AnnedTerm -> AnnedTerm -> MSG AnnedTerm
msgTerm rn2 e_l e_r = msgAnned annedTerm (msgTerm' rn2) e_l e_r {- `mplus` do
    guardFloatable "msgTerm" annedTermFreeVars rn2 e_l e_r
    msgCheckFloatable (termIsCheap e_l) (termIsCheap e_r)
    liftM (fmap Var . annedVar (annedTag e_r)) $ msgGeneraliseTerm rn2 e_l e_r -- Right biased -}

-- TODO: allow lets on only one side? Useful for msging e.g. (let x = 2 in y + x) with (z + 2)
msgTerm' :: RnEnv2 -> Tag -> TermF Anned -> Tag -> TermF Anned -> MSG (TermF Anned)
msgTerm' rn2 _    (Var x_l)                  _    (Var x_r)                  = liftM Var $ msgVar rn2 x_l x_r
msgTerm' rn2 tg_l (Value v_l)                tg_r (Value v_r)                = liftM Value $ msgValue rn2 tg_l v_l tg_r v_r
msgTerm' rn2 _    (TyApp e_l ty_l)           _    (TyApp e_r ty_r)           = liftM2 TyApp (msgTerm rn2 e_l e_r) (msgType rn2 ty_l ty_r)
msgTerm' rn2 _    (App e_l x_l)              _    (App e_r x_r)              = liftM2 App   (msgTerm rn2 e_l e_r) (msgVar  rn2 x_l  x_r)
msgTerm' rn2 _    (PrimOp pop_l tys_l es_l)  _    (PrimOp pop_r tys_r es_r)  = guard "msgTerm: primop" (pop_l == pop_r) >> liftM2 (PrimOp pop_r) (zipWithEqualM (msgType rn2) tys_l tys_r) (zipWithEqualM (msgTerm rn2) es_l es_r)
msgTerm' rn2 _    (Case e_l x_l ty_l alts_l) _    (Case e_r x_r ty_r alts_r) = liftM3 (\e ty (x, alts) -> Case e x ty alts) (msgTerm rn2 e_l e_r) (msgType rn2 ty_l ty_r) (msgIdCoVarBndr (,) rn2 x_l x_r $ \rn2 -> msgAlts rn2 alts_l alts_r)
msgTerm' rn2 _    (Let x_l e1_l e2_l)        _    (Let x_r e1_r e2_r)        = liftM2 (\e1 (x, e2) -> Let x e1 e2) (msgTerm rn2 e1_l e1_r) $ msgIdCoVarBndr (,) rn2 x_l x_r $ \rn2 -> msgTerm rn2 e2_l e2_r
msgTerm' rn2 _    (LetRec xes_l e_l)         _    (LetRec xes_r e_r)         = msgIdCoVarBndrsRec (\xs (es, e) -> LetRec (zipEqual "msgTerm: letrec" xs es) e) rn2 xs_l xs_r $ \rn2 -> liftM2 (,) (zipWithEqualM (msgTerm rn2) es_l es_r) (msgTerm rn2 e_l e_r)
  where (xs_l, es_l) = unzip xes_l
        (xs_r, es_r) = unzip xes_r
msgTerm' rn2 _    (Cast e_l co_l)            _    (Cast e_r co_r)            = liftM2 Cast (msgTerm rn2 e_l e_r) (msgCoercion rn2 (co_l) (co_r))
msgTerm' _ _ _ _ _ = fail "msgTerm"

msgValue :: RnEnv2 -> Tag -> AnnedValue -> Tag -> AnnedValue -> MSG AnnedValue
msgValue rn2 tg_l v_l tg_r v_r = msgValue' rn2 v_l v_r {- `mplus` do
    guardFloatable "msgValue" annedValueFreeVars' rn2 v_l v_r
    -- NB: values are always cheap, so no floatability check
    liftM Indirect $ msgGeneraliseTerm rn2 (fmap Value $ annedValue tg_l v_l) (fmap Value $ annedValue tg_r v_r) -}
    -- TODO: value generalisation (need to check for update frames at stack top)

msgValue' :: RnEnv2 -> AnnedValue -> AnnedValue -> MSG AnnedValue
msgValue' rn2 (TyLambda a_l e_l)           (TyLambda a_r e_r)           = msgTyVarBndr TyLambda rn2 a_l a_r $ \rn2 ->                     msgTerm rn2 e_l e_r
msgValue' rn2 (Lambda x_l e_l)             (Lambda x_r e_r)             = msgIdCoVarBndr Lambda rn2 x_l x_r $ \rn2 -> msgLoseWorkSharing (msgTerm rn2 e_l e_r)
msgValue' rn2 (Data dc_l tys_l cos_l xs_l) (Data dc_r tys_r cos_r xs_r) = guard "msgValue: datacon" (dc_l == dc_r) >> liftM3 (Data dc_r) (zipWithEqualM (msgType rn2) tys_l tys_r) (zipWithEqualM (msgCoercion rn2) cos_l cos_r) (zipWithEqualM (msgVar rn2) xs_l xs_r)
msgValue' _   (Literal l_l)                (Literal l_r)                = guard "msgValue: literal" (l_l == l_r) >> return (Literal l_r)
msgValue' rn2 (Coercion co_l)              (Coercion co_r)              = liftM Coercion $ msgCoercion rn2 co_l co_r
msgValue' _ _ _ = fail "msgValue"

msgAlts :: RnEnv2 -> [AnnedAlt] -> [AnnedAlt] -> MSG [AnnedAlt]
msgAlts rn2 = zipWithEqualM (msgAlt rn2)

msgAlt :: RnEnv2 -> AnnedAlt -> AnnedAlt -> MSG AnnedAlt
msgAlt rn2 (alt_con_l, alt_e_l) (alt_con_r, alt_e_r) = msgAltCon rn2 alt_con_l alt_con_r $ \rn2 -> msgTerm rn2 alt_e_l alt_e_r

msgAltCon :: RnEnv2 -> AltCon -> AltCon -> (RnEnv2 -> MSG AnnedTerm) -> MSG AnnedAlt
msgAltCon rn2 (DataAlt dc_l as_l qs_l xs_l) (DataAlt dc_r as_r qs_r xs_r) k = guard "msgAltCon: datacon" (dc_l == dc_r) >> (msgTyVarBndrs (\as (qs, (xs, e)) -> (DataAlt dc_r as qs xs, e)) rn2 as_l as_r $ \rn2 -> msgIdCoVarBndrs (,) rn2 qs_l qs_r $ \rn2 -> msgIdCoVarBndrs (,) rn2 xs_l xs_r k)
msgAltCon rn2 (LiteralAlt l_l)              (LiteralAlt l_r)              k = guard "msgAltCon: literal" (l_l == l_r) >> liftM (\e -> (LiteralAlt l_r, e)) (k rn2)
msgAltCon rn2 DefaultAlt                    DefaultAlt                    k = liftM ((,) DefaultAlt) $ k rn2
msgAltCon _ _ _ _ = fail "msgAltCon"

msgVarBndr :: (Var -> b -> c) -> RnEnv2 -> Var -> Var -> (RnEnv2 -> MSG b) -> MSG c
msgVarBndr f rn2 v_l v_r k | isId      v_l = guard "msgVarBndr: Id"      (isId v_r)      >> msgIdCoVarBndr f rn2 v_l v_r k
                           | isKindVar v_l = guard "msgVarBndr: KindVar" (isKindVar v_r) >> msgTyVarBndr   f rn2 v_l v_r k
                           | isTyVar   v_l = guard "msgVarBndr: TyVar"   (isTyVar v_r)   >> msgTyVarBndr   f rn2 v_l v_r k
                           | otherwise     = panic "msgVarBndr"

msgTyVarBndr :: (TyVar -> b -> c) -> RnEnv2 -> TyVar -> TyVar -> (RnEnv2 -> MSG b) -> MSG c
msgTyVarBndr f rn2 a_l a_r k = do
    (rn2', a) <- rnBndr2' rn2 a_l a_r
    liftM2 f (msgTyVarBndrExtras rn2 a a_l a_r) $ k rn2'

msgIdCoVarBndr :: (Id -> b -> c) -> RnEnv2 -> Id -> Id -> (RnEnv2 -> MSG b) -> MSG c
msgIdCoVarBndr f rn2 x_l x_r k = msgIdCoVarBndrFlexible f rn2 x_l x_r (\rn2' -> liftM ((,) rn2) (k rn2'))

msgIdCoVarBndrFlexible :: (Id -> b -> c) -> RnEnv2 -> Id -> Id -> (RnEnv2 -> MSG (RnEnv2, b)) -> MSG c
msgIdCoVarBndrFlexible f rn2 x_l x_r k = do
    (rn2', x) <- rnBndr2' rn2 x_l x_r
    (chosen_rn2, b) <- k rn2'
    x <- msgIdCoVarBndrExtras chosen_rn2 x x_l x_r
    return (f x b)

{-
msgIdCoVarBndr' :: RnEnv2 -> Id -> Id -> (RnEnv2, RnEnv2 -> MSG Id)
msgIdCoVarBndr' init_rn2 x_l x_r = (pprTraceSC "msgIdCoVarBndr'" (ppr (x_l, x_r)) init_rn2', \rn2 -> msgIdCoVarBndrExtras rn2 x x_l x_r)
  where (init_rn2', x) = rnBndr2' init_rn2 x_l x_r
-}

msgBndrExtras :: RnEnv2 -> Var -> Var -> Var -> MSG Var
msgBndrExtras rn2 v v_l v_r
  | isId      v_l = guard "msgBndrExtras: Id"      (isId      v_r) >> msgIdCoVarBndrExtras rn2 v v_l v_r
  | isKindVar v_l = guard "msgBndrExtras: KindVar" (isKindVar v_r) >> msgTyVarBndrExtras   rn2 v v_l v_r
  | isTyVar   v_l = guard "msgBndrExtras: TyVar"   (isTyVar   v_r) >> msgTyVarBndrExtras   rn2 v v_l v_r
  | otherwise     = panic "msgBndrExtras"

msgTyVarBndrExtras :: RnEnv2 -> TyVar -> TyVar -> TyVar -> MSG TyVar
msgTyVarBndrExtras rn2 a a_l a_r = liftM (a `setTyVarKind`) $ msgKind rn2 (tyVarKind a_l) (tyVarKind a_r)


-- We have to be careful to msg the "fragile" IdInfo for binders as well as the obvious type information
msgIdCoVarBndrExtras :: RnEnv2 -> Id -> Id -> Id -> MSG Id
msgIdCoVarBndrExtras rn2 x x_l x_r = liftM3 (\unf spec ty -> x `setVarType` ty `setIdUnfolding` unf `setIdSpecialisation` spec)
                                            (msgUnfolding rn2 (realIdUnfolding x_l) (realIdUnfolding x_r))
                                            (msgSpecInfo rn2 x (idSpecialisation x_l) (idSpecialisation x_r))
                                            (msgType rn2 (idType x_l) (idType x_r)) -- NB: cheating a bit to use the same renaming for type info, but probably OK

msgSpecInfo :: RnEnv2 -> Id -> SpecInfo -> SpecInfo -> MSG SpecInfo
msgSpecInfo rn2 x (SpecInfo rules_l _) (SpecInfo rules_r _) = liftM mkSpecInfo $ zipWithEqualM (msgRule rn2 x) rules_l rules_r

msgRule :: RnEnv2 -> Id -> Core.CoreRule -> Core.CoreRule -> MSG Core.CoreRule
msgRule _   _ (Core.BuiltinRule { Core.ru_name = name1 }) rule2@(Core.BuiltinRule { Core.ru_name = name2 }) = guard "msgRule: BuiltinRule" (name1 == name2) >> return rule2 -- NB: assume builtin rules generate RHSes without any free vars!
msgRule rn2 x (Core.Rule { Core.ru_bndrs = vs_l, Core.ru_args = args_l, Core.ru_rhs = rhs_l, Core.ru_name = name1, Core.ru_act = act1, Core.ru_auto = auto1, Core.ru_local = local1 })
              (Core.Rule { Core.ru_bndrs = vs_r, Core.ru_args = args_r, Core.ru_rhs = rhs_r, Core.ru_name = name2, Core.ru_act = act2, Core.ru_auto = auto2, Core.ru_local = local2 })
              = guard "msgRule: Rule" (name1 == name2 && act1 == act2 && auto1 == auto2 && local1 == local2) >> msgVarBndrs (\vs (args, rhs) -> Core.Rule { Core.ru_bndrs = vs, Core.ru_args = args, Core.ru_rhs = rhs, Core.ru_rough = roughTopNames args, Core.ru_name = name2, Core.ru_act = act2, Core.ru_fn = idName x, Core.ru_auto = auto2, Core.ru_local = local2 }) rn2 vs_l vs_r (\rn2 -> liftM2 (,) (zipWithEqualM (msgCore rn2) args_l args_r) (msgCore rn2 rhs_l rhs_r))
msgRule _ _ _ _ = fail "msgRule"

msgUnfolding :: RnEnv2 -> Core.Unfolding -> Core.Unfolding -> MSG Core.Unfolding
msgUnfolding rn2 (Core.CoreUnfolding { Core.uf_tmpl = rhs1, Core.uf_src = src1 }) unf2@(Core.CoreUnfolding { Core.uf_tmpl = rhs2, Core.uf_src = src2 })
  | Core.isStableSource src1, Core.isStableSource src2 = liftM (\rhs -> unf2 { Core.uf_tmpl = rhs }) $ msgCore rn2 rhs1 rhs2 -- Right biased
msgUnfolding rn2 (Core.DFunUnfolding arity1 dc1 args1) (Core.DFunUnfolding arity2 dc2 args2) = guard "msgUnfolding: DFunUnfolding" (arity1 == arity2 && dc1 == dc2) >> liftM (Core.DFunUnfolding arity2 dc2) (zipWithEqualM (msgCore rn2) args1 args2)
-- It is OK to msg any *unstable* unfolding against any other one
msgUnfolding _ unf1 unf2 | not (Core.isStableUnfolding unf1), not (Core.isStableUnfolding unf2) = return Core.noUnfolding
msgUnfolding _ _ _ = fail "msgUnfolding"

msgTickish :: RnEnv2 -> Core.Tickish Id -> Core.Tickish Id -> MSG (Core.Tickish Id)
msgTickish rn2 (Core.Breakpoint { Core.breakpointId = id_l, Core.breakpointFVs = fvs_l }) ti_r@(Core.Breakpoint { Core.breakpointId = id_r, Core.breakpointFVs = fvs_r })
  = guard "msgTickish: Breakpoint" (id_l == id_r) >> liftM (\fvs -> ti_r { Core.breakpointFVs = fvs }) (zipWithEqualM (msgVar rn2) fvs_l fvs_r) -- Right biased
msgTickish _ (Core.Breakpoint {}) _ = fail "msgTickish: Breakpoint vs ?"
msgTickish _ _ (Core.Breakpoint {}) = fail "msgTickish: ? vs Breakpoint"
msgTickish _ ti_l ti_r = guard "msgTickish: non-Breakpoint not exactly equal" (ti_l == ti_r) >> return ti_r

-- TODO: msg instantiation within Core?
msgCore :: RnEnv2 -> Core.CoreExpr -> Core.CoreExpr -> MSG Core.CoreExpr
msgCore rn2 (Core.Var x_l)       (Core.Var x_r)       = liftM Core.Var $ msgVar rn2 x_l x_r
msgCore _   (Core.Lit l_l)       (Core.Lit l_r)       = guard "msgCore: Lit" (l_l == l_r) >> return (Core.Lit l_r)
msgCore rn2 (Core.App e1_l e2_l) (Core.App e1_r e2_r) = liftM2 Core.App (msgCore rn2 e1_l e1_r) (msgCore rn2 e2_l e2_r)
msgCore rn2 (Core.Lam x_l e_l)   (Core.Lam x_r e_r)   = msgVarBndr Core.Lam rn2 x_l x_r $ \rn2 -> (if isId x_r then msgLoseWorkSharing else id) $ msgCore rn2 e_l e_r
msgCore rn2 (Core.Let (Core.NonRec x_l e1_l) e2_l) (Core.Let (Core.NonRec x_r e1_r) e2_r)
  = liftM2 (\e1 (x, e2) -> Core.Let (Core.NonRec x e1) e2) (msgCore rn2 e1_l e1_r) $ msgVarBndr (,) rn2 x_l x_r $ \rn2 -> msgCore rn2 e2_l e2_r
msgCore rn2 (Core.Let (Core.Rec xes_l) e_l) (Core.Let (Core.Rec xes_r) e_r)
  = msgIdCoVarBndrsRec (\xs (es, e) -> Core.Let (Core.Rec (zipEqual "msgCore: LetRec" xs es)) e) rn2 xs_l xs_r $ \rn2 -> liftM2 (,) (zipWithEqualM (msgCore rn2) es_l es_r) (msgCore rn2 e_l e_r)
      where (xs_l, es_l) = unzip xes_l
            (xs_r, es_r) = unzip xes_r
msgCore rn2 (Core.Case e_l x_l ty_l alts_l) (Core.Case e_r x_r ty_r alts_r) = liftM3 (\e ty (x, alts) -> Core.Case e x ty alts) (msgCore rn2 e_l e_r) (msgType rn2 ty_l ty_r) (msgIdCoVarBndr (,) rn2 x_l x_r $ \rn2 -> msgCoreAlts rn2 alts_l alts_r)
msgCore rn2 (Core.Cast e_l co_l)            (Core.Cast e_r co_r)            = liftM2 Core.Cast (msgCore rn2 e_l e_r) (msgCoercion rn2 co_l co_r)
msgCore rn2 (Core.Tick ti_l e_l)            (Core.Tick ti_r e_r)            = liftM2 Core.Tick (msgTickish rn2 ti_l ti_r) (msgCore rn2 e_l e_r)
msgCore rn2 (Core.Type ty_l)                (Core.Type ty_r)                = liftM Core.Type $ msgType rn2 ty_l ty_r
msgCore rn2 (Core.Coercion co_l)            (Core.Coercion co_r)            = liftM Core.Coercion $ msgCoercion rn2 co_l co_r
msgCore _ _ _ = fail "msgCore"

msgCoreAlts :: RnEnv2 -> [Core.CoreAlt] -> [Core.CoreAlt] -> MSG [Core.CoreAlt]
msgCoreAlts rn2 = zipWithEqualM (msgCoreAlt rn2)

msgCoreAlt :: RnEnv2 -> Core.CoreAlt -> Core.CoreAlt -> MSG Core.CoreAlt
msgCoreAlt rn2 (alt_con_l, vs_l, alt_e_l) (alt_con_r, vs_r, alt_e_r) = guard "msgCoreAlt" (alt_con_l == alt_con_r) >> msgVarBndrs ((,,) alt_con_l) rn2 vs_l vs_r (\rn2 -> msgCore rn2 alt_e_l alt_e_r)

msgTyVarBndrs :: ([TyVar] -> a -> b) -> RnEnv2 -> [TyVar] -> [TyVar] -> (RnEnv2 -> MSG a) -> MSG b
msgTyVarBndrs = msgMany msgTyVarBndr

msgIdCoVarBndrs :: ([Id] -> a -> b) -> RnEnv2 -> [Id] -> [Id] -> (RnEnv2 -> MSG a) -> MSG b
msgIdCoVarBndrs = msgMany msgIdCoVarBndr

msgIdCoVarBndrsRec :: ([Id] -> a -> b) -> RnEnv2 -> [Id] -> [Id] -> (RnEnv2 -> MSG a) -> MSG b
msgIdCoVarBndrsRec = msgManyRec msgIdCoVarBndrFlexible

msgVarBndrs :: ([Var] -> a -> b) -> RnEnv2 -> [Var] -> [Var] -> (RnEnv2 -> MSG a) -> MSG b
msgVarBndrs = msgMany msgVarBndr

msgMany :: (forall b c. (Var -> b -> c) -> RnEnv2 -> v -> v -> (RnEnv2 -> MSG b) -> MSG c)
        -> ([Var] -> b -> c) -> RnEnv2 -> [v] -> [v] -> (RnEnv2 -> MSG b) -> MSG c
msgMany mtch f rn2 xs_l xs_r k = liftM (uncurry f) $ go rn2 xs_l xs_r
  where go rn2 []         []         = liftM ((,) []) $ k rn2
        go rn2 (x_l:xs_l) (x_r:xs_r) = liftM (\(x, (xs, b)) -> (x:xs, b)) $ mtch (,) rn2 x_l x_r $ \rn2 -> go rn2 xs_l xs_r
        go _ _ _ = fail "msgMany"

msgManyRec :: forall b c v.
              (forall b c. (Var -> b -> c) -> RnEnv2 -> v -> v -> (RnEnv2 -> MSG (RnEnv2, b)) -> MSG c)
           -> ([Var] -> b -> c) -> RnEnv2 -> [v] -> [v] -> (RnEnv2 -> MSG b) -> MSG c
msgManyRec mtch_flexi f rn2 xs_l xs_r k = liftM (uncurry f . snd) $ go rn2 xs_l xs_r
  where go :: RnEnv2 -> [v] -> [v] -> MSG (RnEnv2, ([Var], b))
        go rn2 []         []         = liftM ((,) rn2 . (,) []) $ k rn2
        go rn2 (x_l:xs_l) (x_r:xs_r) = liftM (\(x, (rn2, (xs, b))) -> (rn2, (x:xs, b))) $ mtch_flexi (,) rn2 x_l x_r $ \rn2 -> liftM (\(rn, res) -> (rn, (rn, res))) $ go rn2 xs_l xs_r
        go _ _ _ = fail "msgManyRec"

msgVar :: RnEnv2 -> Out Var -> Out Var -> MSG Var
msgVar rn2 x_l x_r = case (rnOccL_maybe rn2 x_l, rnOccR_maybe rn2 x_r) of
     -- Both rigidly bound: msg iff they rename to the same thing
    (Just x_l', Just x_r') -> pprTraceSC "msgVar_maybe(rigid)" (ppr (x_l, x_r)) $ guard "msgVar: rigid" (x_l' == x_r') >> return x_l'
     -- Both bound by let: defer decision about msging
    (Nothing, Nothing)     -> pprTraceSC "msgVar_maybe(flexi)" (ppr (x_l, x_r)) $ msgFlexiVar rn2 x_l x_r
     -- One bound by let and one bound rigidly: don't msg
    _                      -> fail "msgVar: mismatch"

{-
msgVarL :: RnEnv2 -> Out Id -> Out AnnedTerm -> MSG [MSGLR]
msgVarL rn2 x_l e_r = fmap maybeToList (msgVarL_maybe rn2 x_l e_r)

msgVarL_maybe :: RnEnv2 -> Out Id -> Out AnnedTerm -> MSG (Maybe MSGLR)
msgVarL_maybe rn2 x_l e_r = guard "msgVarL_maybe: no floating" fLOAT_TO_MATCH >> case rnOccL_maybe rn2 x_l of
     -- Left rigidly bound: msging is impossible (assume we already tried msgVar_maybe)
    Just _  -> fail "msgVar: rigid"
     -- Both bound by let: defer decision about msging
    Nothing -> return (Just (VarL x_l e_r))

msgVarR :: RnEnv2 -> Out AnnedTerm -> Out Id -> MSG [MSGLR]
msgVarR rn2 e_l x_r = fmap maybeToList (msgVarR_maybe rn2 e_l x_r)

msgVarR_maybe :: RnEnv2 -> Out AnnedTerm -> Out Id -> MSG (Maybe MSGLR)
msgVarR_maybe rn2 e_l x_r = guard "msgVarR_maybe: no floating" fLOAT_TO_MATCH >> case rnOccR_maybe rn2 x_r of
     -- Right rigidly bound: msging is impossible (assume we already tried msgVar_maybe)
    Just _  -> fail "msgVar: rigid"
     -- Both bound by let: defer decision about msging
    Nothing -> return (Just (VarR e_l x_r))
-}

msgIn :: (InScopeSet -> Renaming -> a -> a)
      -> (b -> FreeVars)
      -> (RnEnv2 -> a -> a -> MSG b)
      -> RnEnv2 -> In a -> In a -> MSG (In b)
msgIn rnm fvs mtch rn2 (rn_l, x_l) (rn_r, x_r) = liftM (\b -> (mkIdentityRenaming (fvs b), b)) $ mtch rn2 (rnm iss rn_l x_l) (rnm iss rn_r x_r)
  where iss = rnInScopeSet rn2 -- NB: this line is one of the few things that relies on the RnEnv2 InScopeSet being correct

heapBindingGeneralised :: HeapBinding -> Generalised
heapBindingGeneralised = maybe True id . heapBindingLambdaBoundness

stackGeneralised :: Stack -> Generalised
stackGeneralised k_lr = case k_lr of Loco gen -> gen; _ -> True

-- Achieves several things:
--  1) Sets up known_x entries for stack binder pairs
--  2) Initialises msgSuckStack
--  3) Initialises msgLRAvailableStack
initStack :: [Var] {- loop -} -> Int -> Stack -> Stack -> StackInitM ([MSG Var], Pair (Stack, Stack), IM.IntMap (MSGU ()))
initStack xs i (Car kf_l k_l) (Car kf_r k_r) = do
    (xs, mb_x, mb_mx) <- case (tagee kf_l, tagee kf_r) of
      (Update x_l, Update x_r) -> do
        -- This is all a bit tricky: we want to enter this binder pair into the known map,
        -- but instead of using the standard scheme where I look into the heap for definitions
        -- of the vars to MSG (and generalise if it is not found), I want to just assume that
        -- I'm going to be able to MSG up to the current stack frame and fix things up later
        -- if that is wrong.
        --
        -- Furthermore I don't want to MSG the binder's *extra* info until the known maps are complete, hence
        -- this lazy pattern match/return action list nonsense, which I'll need to fix up with mfix in the caller.
        let ~(x_looped:xs') = xs
        (x, mx) <- msgPendStackBinder x_looped x_l x_r
        return (xs', Just (x, Pair x_l x_r), Just mx)
      _ -> return (xs, Nothing, Nothing)
    let suck = initSuckStackFrame i mb_x kf_l kf_r
    liftM (\(mxs, k_lrs, sucks) -> (maybe id (:) mb_mx mxs, liftA2 (\kf_lr (k_avail_lr, k_lr) -> (kf_lr `Car` k_avail_lr, k_lr)) (Pair kf_l kf_r) k_lrs, IM.insert i suck sucks)) $ initStack xs (i + 1) k_l k_r
initStack _ _ k_l k_r = return ([], Pair (Loco (stackGeneralised k_l), k_r) (Loco (stackGeneralised k_r), k_r), IM.empty)

initSuckStackFrame :: Int -> Maybe (Var {- partial loop -}, Pair Var) -> Tagged StackFrame -> Tagged StackFrame -> MSGU ()
initSuckStackFrame i mb_x (Tagged tg_l kf_l) (Tagged tg_r kf_r) = do
    -- First things first, ensure we can't make a reentrant attempt to suck this stack frame
    modify_ $ \s -> s { msgSuckStack = IM.delete i (msgSuckStack s) }
    -- Rename the common update binder, if any
    maybe (return ()) (\(x, x_lrs) -> modify_ $ \s -> s { msgCommonHeap = M.insert x lambdaBound (msgCommonHeap s)
                                                        , msgLR = liftA2 (\x_lr s_lr -> s_lr { msgLRRenaming = insertVarRenaming (msgLRRenaming s_lr) x x_lr }) x_lrs (msgLR s) }) mb_x
    -- Suck in the following frames first to make the next logic easier to write
    suckStack (i + 1)
    -- Remove the frame from the common or available stacks..
    modify_ $ \s -> case fmap msgLRAvailableStack (msgLR s) of
                      -- Since the only way we can enter this sucker is if i is available at the start,
                      -- if the available stack is empty at this point then we know that the ith frame must be in the
                      -- "committed" common stack, so we need to dump it from there (it must be at the end):
                      (Pair (Loco _) (Loco _))            -> s { msgCommonStack = trainInit (\_ _ -> True) (msgCommonStack s) }
                      -- Otherwise, we must still be in the process of MSGing the stack, so let's remove the
                      -- sucked frame from the available frames (again, it must be at the end):
                      avail_ks@(Pair (Car _ _) (Car _ _)) -> s { msgLR = liftA2 (\avail_k_lr s_lr -> s_lr { msgLRAvailableStack = trainInit (\_ _ -> True) avail_k_lr }) avail_ks (msgLR s) }
                      _ -> panic "initSuckStackFrame: available stack not paired"
    -- .. and add the frame to the individual stack
    modify_ $ \s -> s { msgLR = liftA2 (\kf_lr s_lr -> s_lr { msgLRStack = kf_lr `Car` msgLRStack s_lr }) (Pair (Tagged tg_l kf_l) (Tagged tg_r kf_r)) (msgLR s) }
    -- Ensure that all FVs of the newly-individualised frame are bound
    sucks2 $ fmap stackFrameFreeVars (Pair kf_l kf_r)

msgStack :: Int -> MSGU ()
msgStack i = do
    k_avail_lrs <- flip fmap get $ \s -> fmap msgLRAvailableStack (msgLR s)
    flip recoverM (suckStack i) $ do
        Pair (kf_l `Car` k_avail_l') (kf_r `Car` k_avail_r') <- return k_avail_lrs
        modify_ $ \s -> s { msgLR = liftA2 (\k_avail_lr' s_lr -> s_lr { msgLRAvailableStack = k_avail_lr' }) (Pair k_avail_l' k_avail_r') (msgLR s) }
        void $ mfix $ \kf -> do modify_ $ \s -> s { msgCommonStack = trainAppend (msgCommonStack s) (\_ -> kf `Car` Loco (stackGeneralised k_avail_r')) } -- Right biased
                                msgStackFrame kf_l kf_r
        msgu $ msgStack (i + 1)

get :: Applicative t => MSGT t MSGState
get = MSG $ \_ s -> (s, pure s)

modify_ :: Applicative t => (MSGState -> MSGState) -> MSGT t ()
modify_ f = MSG $ \_ s -> (f s, pure ())

msgu :: MSGU a -> MSG a
msgu mx = MSG $ \e s -> second (Right . unI) $ unMSG mx e s

sucks2 :: Pair VarSet -> MSGU ()
sucks2 (Pair xs_l xs_r) = do { sucks pFst xs_l; sucks pSnd xs_r }

sucks :: (Pair MSGLRState -> MSGLRState) -> VarSet -> MSGU ()
sucks sel xs = foldVarSet (\x rest -> suck sel x >> rest) (return ()) xs

suck2 :: Pair Var -> MSGU ()
suck2 (Pair x_l x_r) = do { suck pFst x_l; suck pSnd x_r }

suck :: (Pair MSGLRState -> MSGLRState) -> Var -> MSGU ()
suck sel x = join $ suck' sel x

suck' :: (Pair MSGLRState -> MSGLRState) -> Var -> MSGU (MSGU ())
suck' sel x = {- trace ("suck':" ++ show x) $ -} flip fmap get $ \s -> lookupWithDefaultVarEnv (msgLRSuckVar (sel (msgLR s))) (return ()) x

suckStack :: Int -> MSGU ()
suckStack i = {- trace ("suckStack:" ++ show i) $ -} join $ flip fmap get $ \s -> IM.findWithDefault (return ()) i (msgSuckStack s)

msgStackFrame :: Tagged StackFrame -> Tagged StackFrame -> MSG (Tagged StackFrame)
msgStackFrame kf_l kf_r = liftM (Tagged (tag kf_r)) $ go (tagee kf_l) (tagee kf_r) -- Right biased
  where
    go :: StackFrame -> StackFrame -> MSG StackFrame
    go (Apply x_l')                          (Apply x_r')                          = liftM Apply $ msgVar top_rn2 x_l' x_r'
    go (TyApply ty_l')                       (TyApply ty_r')                       = liftM TyApply $ msgType top_rn2 ty_l' ty_r'
    go (Scrutinise x_l' ty_l' in_alts_l)     (Scrutinise x_r' ty_r' in_alts_r)     = liftM2 (\ty (x, in_alts) -> Scrutinise x ty in_alts) (msgType top_rn2 ty_l' ty_r') (msgIdCoVarBndr (,) top_rn2 x_l' x_r' $ \rn2 -> msgIn renameAnnedAlts annedAltsFreeVars msgAlts rn2 in_alts_l in_alts_r)
    go (PrimApply pop_l tys_l' as_l in_es_l) (PrimApply pop_r tys_r' as_r in_es_r) = guard "msgStackFrame: primop" (pop_l == pop_r) >> liftM3 (\tys as in_es -> PrimApply pop_r tys as in_es) (zipWithEqualM (msgType top_rn2) tys_l' tys_r') (zipWithEqualM (msgAnned annedCoercedAnswer (msgCoerced msgAnswer top_rn2)) as_l as_r) (zipWithEqualM (msgIn renameAnnedTerm annedTermFreeVars msgTerm top_rn2) in_es_l in_es_r)
    go (StrictLet x_l' in_e_l)               (StrictLet x_r' in_e_r)               = msgIdCoVarBndr StrictLet top_rn2 x_l' x_r' $ \rn2 -> msgIn renameAnnedTerm annedTermFreeVars msgTerm rn2 in_e_l in_e_r
    go (CastIt co_l')                        (CastIt co_r')                        = liftM CastIt $ msgCoercion top_rn2 co_l' co_r'
    go (Update x_l')                         (Update x_r')                         = liftM Update $ msgVar top_rn2 x_l' x_r'
      -- We treat update bindings exactly like an occurrence: this is guaranteed to work nicely because in initStack
      -- we took the time to extend the "known" mappings for every possible update pair we might come across
    go _ _ = fail "msgStackFrame"

-- NB: it is not necessary to include the ids_l/ids_r in these InScopeSets because the
-- binders allocated for rigid binders are *allowed* to shadow things bound in h_l/h_r.
-- The only things they may not shadow are things bound in the eventual common heap,
-- but we will ensure that when we are choosing those binders in msgPend.
--
-- Indeed it is better to not include ids_l/ids_r so that the new common heap/stack
-- binders often often have exactly the same uniques as their counterparts in the
-- incoming states. This aids debugging.
top_rn2 :: RnEnv2
top_rn2 = mkRnEnv2 emptyInScopeSet

-- NB: we must enforce invariant that stuff "outside" cannot refer to stuff bound "inside" (heap *and* stack)
msgLoop :: MSGMode -> (Heap, Heap) -> (Anned QA, Anned QA) -> (Stack, Stack)
        -> MSG' (Pair (Heap, Renaming, Stack), (Heap, Stack, Anned QA)) 
msgLoop mm (Heap init_h_l init_ids_l, Heap init_h_r init_ids_r) (qa_l, qa_r) (init_k_l, init_k_r)
  = flip fmap mb_res $ \(_, e) -> (liftA2 (\init_ids_lr msg_s_lr' -> (Heap (msgLRHeap msg_s_lr') init_ids_lr, msgLRRenaming msg_s_lr', msgLRStack msg_s_lr'))
                                          (Pair init_ids_l init_ids_r) (msgLR msg_s'),
                                   (Heap (msgCommonHeap msg_s') (msgInScopeSet msg_s'), msgCommonStack msg_s', e))
  where
    (msg_s', mb_res) = runMSG msg_e msg_s $ do
      xs <- sequence mxs
      msgu $ msgStack 0
      liftM ((,) xs) $ msgAnned annedQA (msgQA top_rn2) qa_l qa_r

    -- NB: knot-tied to feed the MSGed binders into the range of the "known" map generated by initStack
    Right (xs, _) = mb_res
    ((mxs, k_lrs, suck_stack), (ids, known_vars)) = State.runState (initStack xs 0 init_k_l init_k_r) (msgCommonHeapVars mm, emptyVarEnv)

    msg_e = MSGEnv False mm
    msg_s = MSGState {
        msgInScopeSet     = ids,
        msgKnownVars      = known_vars,
        msgKnownTypes     = emptyTM,
        msgKnownCoercions = emptyTM,
        msgLR             = msg_s_lrs,
        msgCommonHeap     = M.empty,
        msgCommonStack    = Loco (stackGeneralised init_k_r), -- Right biased
        msgSuckStack      = suck_stack
      }
    msg_s_lrs = (\x y z f -> liftA3 f x y z) (Pair (pFst, pairFirst) (pSnd, pairSecond)) (Pair (init_h_l, init_k_l) (init_h_r, init_k_r)) k_lrs $ \(xtrct_lr, mdfy_lr) (init_h_lr, init_k_lr) (k_avails_lr, k_lr) -> MSGLRState {
        msgLRAvailableHeap  = init_h_lr,
        msgLRAvailableStack = k_avails_lr,
        msgLRHeap           = M.empty,
        msgLRStack          = k_lr,
        msgLRRenaming       = emptyRenaming,
        msgLRSuckVar        = mkSuckVars xtrct_lr mdfy_lr init_h_lr init_k_lr
      }

    pairFirst  (Pair a b) f = Pair (f a) b
    pairSecond (Pair a b) f = Pair a (f b)

    mkSuckVars xtrct_lr mdfy_lr init_h_lr init_k_lr = suck2
      where suck0 = emptyVarEnv
            suck1 = (\f -> foldr f suck0 ([0..] `zip` trainCars init_k_lr)) $ \(i, Tagged _ kf_lr) suck -> case kf_lr of
                      Update x_lr -> extendVarEnv suck x_lr $ suckStack i
                      _           -> suck
            suck2 = (\f -> M.foldWithKey f suck1 init_h_lr) $ \x_lr hb_lr suck -> extendVarEnv suck x_lr $ do
                      modify_ $ \s -> s { msgLR = mdfy_lr (msgLR s) $ \s_lr -> s_lr { msgLRHeap = M.insert x_lr hb_lr (msgLRHeap s_lr)
                                                                                    , msgLRSuckVar = msgLRSuckVar s_lr `delVarEnv` x_lr
                                                                                    , msgLRAvailableHeap = if heapBindingCheap hb_lr then msgLRAvailableHeap s_lr else M.delete x_lr (msgLRAvailableHeap s_lr) } }
                      sucks xtrct_lr (heapBindingFreeVars hb_lr)

          {-
          -- NB: I can try to avoid generalisation when e_l or e_r is just a heap-bound variable. We do this by floating the non-variable into a new heap binding
          -- which looks just like it was in the inital heap on the left/right and then matching the variable pair we are left with as normal.
          --
          -- Note [Non-terminating instance matches]
          -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          --
          -- We need to carefully avoid termination issues that arise from terms like:
          --  x |-> y
          --  y |-> x  `msg`  a |-> True
          --  x               a
          --
          -- Where we might go:
          --  Does x match a?    It does if y matches True!
          --  Does y match True? It does if x matches True!
          --  Does x match True? It does if y matches True!
          --  Does y match True? ...
          --
          -- That would be an infinite loop. In fact, instead we are floating the expression
          -- (i.e. True) to a new (named) heap binding, but that *doesn't* mean that we benefit from
          -- the standard termination benefits of the PendingVar memoisation stuff. We get:
          --  Does x match a? It does if y matches True!
          --  Give True the name b.
          --  Does y match b? It does if x matches True!
          --  Give True the name c.
          --  Does x match c? It does if y matches True!
          --  Give True the name d
          --  Does y match d? ...
          --
          -- This is still an infinite loop :-(
          --
          -- You might think any loop like this must go through at least 1 cheap term, because for it to continue
          -- we have to repeatedly copy a heap binding to the common heap under many different names. This *is* true
          -- but it's not so easy to see -- don't forget that only a *portion* of a heap binding needs to be copied
          -- up to the common heap many times, so we might potentially have a loop where we repeatedly give distinct
          -- names to the (expensive) term (fib 100) from the heap-bound term (/\a -> fib 100). (Examples with a value
          -- lambda won't demonstrate this because we don't float expensive stuff from those). This is still OK though,
          -- because to float that portion out an infinite number of times we have to copy the enclosing cheap binding
          -- an infinite number of times too.
          --
          -- Since the loop *does* always go through at least 1 cheap term, you might think a suitable solution to the
          -- problem is to memoise PendingTerm pairs *iff* both sides of the pair are cheap (we CANNOT memoise these
          -- pairs in general or we may increase sharing). Although that solves the termination issue, it isn't much
          -- better. We get:
          --  Does x match a? It does if y matches True!
          --  Give True the name b.
          --  Does y match b? It does if x matches True!
          --  Give True the name c.
          --  Does x match c? It does if y matches True, AND we've seen this before - reuse that decision.
          --
          -- So the MSGed term is:
          --  d |-> e
          --  e |-> d
          --  d
          --
          -- This is clearly not instantiable to derive the RHS!
          --
          -- NB: this problem is not restricted to values/cheap stuff. We can also run into an infinite loop with e.g.:
          --  a |-> a  `msg`  fib 100
          --  a
          --
          -- We would get:
          --  Does a match fib 100? It does if a matches fib 100, AND we've seen this before - reuse that decision.
          --
          -- Or without renaming but with introducing new names:
          --  Does a match fib 100? It does if a matches fib 100!
          --  Give fib 100 the name b.
          --  Does a match b? It does if a matches fib 100!
          --  ...
          --
          -- TODO: I don't have a good solution at the moment. For now, I've patched it so we don't use these branches.
          -- FIXME: carefully document the basis (if any) by which I believe it is OK to MSG stuff out like this
          --        Naively it seems rather dangerous. Consider:
          --        1. Supercompile (let f = \x. x in f)
          --        2. Split to (\x. x)
          --        3. MSG against the simple variable b (with the same tag, presumably)
          --        4. Instance-split will drive (\x. x) as the next term, so we might tieback to get a loop :(
          {-
          | Just (x_l, e_r) <- varTermPair e_l e_r
          , let (ids_r', x_r) = uniqAway' ids_r x_common
          = go rn_l rn_r used_l used_r init_h_l (M.insert x_r (internallyBound (renamedTerm e_r)) init_h_r) (Heap h_l ids_l) (Heap h_r ids_r') h (msg_s { msgPending = (x_common, PendingVar x_l x_r):rest })
          
          | Just (x_r, e_l) <- varTermPair e_r e_l
          , let (ids_l', x_l) = uniqAway' ids_l x_common
          = go rn_l rn_r used_l used_r (M.insert x_l (internallyBound (renamedTerm e_l)) init_h_l) init_h_r (Heap h_l ids_l') (Heap h_r ids_r) h (msg_s { msgPending = (x_common, PendingVar x_l x_r):rest })
          -}
          Right (x_common, PendingTerm e_l e_r, msg_s)
          -- We already know the terms don't match so we are going to generalise
          --
          -- NB: we rely on the fact that x_common always has no extra info here, because we need to effectively apply the left/right
          -- hand side renaming to the Id extra info. If we don't then we end up having binders like (x :: genty) in the left/right heaps
          -- when the left/right renaming contains a mapping like (genty |-> Int).
          --
          -- We can just barely get away with this in the type/coercion cases because the types/coercions don't get bindings per-se, they are just added to the renaming
            -> sucks (annedTermFreeVars e_l `unionVarSet` varBndrFreeVars x_common_l) (sucks (annedTermFreeVars e_r `unionVarSet` varBndrFreeVars x_common_r) (go msg_s . swap) . swap)
                     (Pair ((init_h_l, used_l), l_to_r, (Heap (M.insert x_common_l (internallyBound (renamedTerm e_l)) h_l) ids_l', insertIdRenaming rn_l x_common x_common_l, k_l))
                           ((init_h_r, used_r), r_to_l, (Heap (M.insert x_common_r (internallyBound (renamedTerm e_r)) h_r) ids_r', insertIdRenaming rn_r x_common x_common_r, k_r)))
                     (M.insert x_common generalisedLambdaBound h) k
            where (ids_l', x_common_l) = uniqAway' ids_l (x_common `setVarType` termType e_l)
                  (ids_r', x_common_r) = uniqAway' ids_r (x_common `setVarType` termType e_r)
          -}

inject :: HeapBinding -> MSG' (Maybe (Bool, In AnnedTerm))
inject (HB { howBound = how_bound, heapBindingMeaning = meaning })
  | LambdaBound <- how_bound
  , Left _ <- meaning
  = Left "msgLoop: lambda-bounds must be generalised"
  | LetBound <- how_bound
  , Left _ <- meaning
  = return Nothing
  | Just let_bound <- case how_bound of
      LetBound        -> Just True
      InternallyBound -> Just False
      LambdaBound     -> Nothing
  , Right in_e <- meaning
  = return $ Just (let_bound, in_e)
  | otherwise
  = pprPanic "msgLoop: unhandled heap binding format" (ppr how_bound $$ (case meaning of Left _ -> text "Left"; Right _ -> text "Right"))

{-
findVar :: PureHeap -> S.Set Var
        -> Var -> (MSG' (S.Set Var, HeapBinding),
                     -- Nothing ==> unavailable for common heap/stack
                     -- Just    ==> available for common heap/stack with this usage
                   PureHeap -> Stack -> Maybe (VarSet, S.Set Var, PureHeap))
                     -- If this "adding" action returns Nothing, it means that we already added the corresponding thing
                     -- to the common part of the state, and the thing was not cheap. In this case we must use the information
                     -- recorded at the point we successfully added it to the common part in order to remove it from the common
                     -- part and add it to the individual parts
                     --
                     -- NB: if the individual part is Just, then this "adder" always succeeds
findVar init_h used x = case M.lookup x init_h of
     -- Variable bound by the heap (vastly common case):
    Just hb -> (common, individual)
      where (common, must_rollback)
              | S.notMember x used = (return (used', hb),               False)
              | otherwise          = (lft "used heap binding (common)", True)
            individual h _ | x `M.member` h = Just (emptyVarSet,                                            used,  h)
                           | must_rollback  = Nothing
                           | otherwise      = Just (varBndrFreeVars x `unionVarSet` heapBindingFreeVars hb, used', M.insert x hb h)
            used' | Just (_, e) <- heapBindingTerm hb
                  , not (termIsCheap e)
                  = S.insert x used
                  | otherwise
                  = used
     -- By a process of elimination, variables must be bound be the stack. We don't need to worry about returning a sensible answer
     -- in the "common" case because we eagerly discharge all matches related to the stack that can possibly succeed before entering
     -- the "go" loop. We do need to be careful about the individual portion because we may have to initiate rollback.
    Nothing -> (lft "used stack binding (common)", \h k -> case find (\kf -> case tagee kf of Update x_kf -> x_kf == x; _ -> False) (trainCars k) of
                                                             -- In this case we found x bound in the *common* part of the stack, must roll back
                                                             Nothing -> Nothing
                                                             -- In this case x was bound in the individual part and we are already done
                                                             Just _  -> Just (emptyVarSet, used, h))
  where lft msg = Left (msg ++ ": " ++ showPpr x)

suck :: Var -> Looper a -> Looper a
suck x_lr kont (Pair info_lr@((init_h_lr, used_lr), _, _) info_rl) h k
  = suck' (snd (findVar init_h_lr used_lr x_lr)) x_lr kont (Pair info_lr info_rl) h k

suck' :: (PureHeap -> Stack -> Maybe (VarSet, S.Set Var, PureHeap))
      -> Var -> Looper a -> Looper a
suck' find_res x_lr kont (Pair info_lr@((init_h_lr, _), lr_to_rl, (Heap h_lr ids_lr, rn_lr, k_lr)) info_rl) h k
  = case find_res h_lr k_lr of
      -- NB: this "rollback" action is guaranteed to suck x_lr into the individual heap/stack along with any
      -- partner x_rl or transitively needed x_lrs required to ensure that the newly lambda-bound x_common is instantiated correctl
      Nothing -> unRB rollback_lr kont (Pair info_lr info_rl) h k
        where Just rollback_lr = lookupVarEnv lr_to_rl x_lr
      Just (fvs_lr, used_lr', h_lr') -> sucks fvs_lr kont (Pair ((init_h_lr, used_lr'), lr_to_rl, (Heap h_lr' ids_lr, rn_lr, k_lr)) info_rl) h k

sucks :: VarSet -> Looper a -> Looper a
sucks xs_lr kont = foldVarSet suck kont xs_lr
-}

guardFloatable :: String -> (a -> FreeVars) -> RnEnv2 -> a -> a -> MSG ()
guardFloatable msg fvs rn2 x_l x_r = guard (msg ++ ": unfloatable") (canFloat fvs (rnOccL_maybe rn2) x_l && canFloat fvs (rnOccR_maybe rn2) x_r)

canFloat :: (b -> FreeVars) -> (Var -> Maybe a) -> b -> Bool
canFloat fvs lkup = foldVarSet (\a rest_ok -> maybe rest_ok (const False) (lkup a)) True . fvs

{-
uniqAway' :: InScopeSet -> Var -> (InScopeSet, Var)
uniqAway' ids x = (ids `extendInScopeSet` x', x')
  where x' = uniqAway ids x
-}