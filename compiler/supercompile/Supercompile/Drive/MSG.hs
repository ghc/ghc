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

import qualified Supercompile.GHC as GHC
import Supercompile.Utilities hiding (guard)

import qualified CoreSyn as Core

import Util
import Coercion
import CoreUtils  (hashCoercion, hashType, hashExpr)
import Name       (mkSystemVarName)
import Var        (TyVar, mkTyVar, isTyVar, isId, tyVarKind, setVarType, setTyVarKind, varName)
import Id         (Id, idType, idName, realIdUnfolding, setIdUnfolding, idSpecialisation, setIdSpecialisation, mkSysLocal, mkLocalId)
import IdInfo     (SpecInfo(..))
import VarEnv
import Type       (typeKind, mkTyConApp, mkAppTy, getTyVar_maybe)
import TysWiredIn (pairTyCon, tupleCon)
import TysPrim    (funTyCon)
import TypeRep    (Kind, Type(..), isKindVar)
import TrieMap    (TrieMap(..), CoercionMap, TypeMap)
import Rules      (mkSpecInfo, roughTopNames)
import Unique     (mkUniqueGrimily)
import FastString (fsLit)
import BasicTypes (TupleSort(..))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Foldable as Foldable


-- FIXME: we probably need to enforce the kind invariant explicitly


pprTraceSC :: String -> SDoc -> a -> a
--pprTraceSC _ _ = id
--pprTraceSC = pprTrace
pprTraceSC msg doc a = traceSC (msg ++ ": " ++ showSDoc doc) a

traceSC :: String -> a -> a
traceSC _ = id
--traceSC = trace


rnBndr2' :: RnEnv2 -> Var -> Var -> MSG (RnEnv2, Var)
rnBndr2' rn2 x_l x_r = MSG $ \_ s -> Right (s, rnBndr2'' (uniqAway (msgInScopeSet s)) rn2 x_l x_r)
  -- The uniqAway is 1/2 of the story to ensure we don't get clashes between new rigid binders and the new common heap binders
  --
  -- (We don't want to inadvertently have {x} in both InScopeSets and an occurrence (\y -> x) where RnEnv2 makes y rename to x by pure chance.
  -- BUT I don't want to just put ids_l/ids_r in the RnEnv2 because that will force common occurrence sites to rename (in msgPend) which
  -- which will make things drastically worse! Instead, rnBndr2' does the uniqAway manually.)

-- FIXME: rigid variable occurrences do not get correct type/info
rnBndr2'' :: (Var -> Var)
          -> RnEnv2 -> Var -> Var -> (RnEnv2, Var)
rnBndr2'' f rn2 x_l x_r = (rn2', x')
  where rn2' = rn2 { envL     = extendVarEnv (envL rn2) x_l x'
                   , envR     = extendVarEnv (envR rn2) x_r x'
                   , in_scope = extendInScopeSet (in_scope rn2) x' }
        x' = f (uniqAway (in_scope rn2) x_r)


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
msgLoseWorkSharing mx = MSG $ \e -> unMSG mx (e { msgLostWorkSharing = True })

-- I'm not sure if we want to float arbitrary stuff out of lambdas. Doing so does reduce
-- worst case allocation behaviour, but it does so at the cost of increased upfront allocations
-- and GC lifetimes.
--
-- At the moment my poor compromise is to only float out cheap looking stuff.
msgCheckFloatable :: Bool -> Bool -> MSG ()
msgCheckFloatable cheap_l cheap_r
  | cheap_l, cheap_r = return ()
  | otherwise        = MSG $ \e s -> if msgLostWorkSharing e
                                      then Left "msgCheckFloatable"
                                      else Right (s, ())

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
             | PendingTerm     AnnedTerm AnnedTerm

instance Outputable Pending where
    ppr (PendingVar      x_l  x_r)  = ppr x_l  <+> text "<-x->" <+> ppr x_r
    ppr (PendingType     ty_l ty_r) = ppr ty_l <+> text "<-t->" <+> ppr ty_r
    ppr (PendingCoercion co_l co_r) = ppr co_l <+> text "<-c->" <+> ppr co_r
    ppr (PendingTerm     e_l  e_r)  = ppr e_l  <+> text "<-e->" <+> ppr e_r

data MSGState = MSGState {
    msgInScopeSet            :: InScopeSet,                      -- We have to ensure all new vars introduced by MSG are distinct from each other
    msgKnownPendingVars      :: VarEnv (VarEnv Var),             -- INVARIANT: the "known" maps are inverse to the pending list, except that PendingTerms are not recorded in
    msgKnownPendingTypes     :: TypeMap (TypeMap TyVar),         -- a "known" map at all. We don't *want* them in one because we don't mant MSGing to increase work sharing!
    msgKnownPendingCoercions :: CoercionMap (CoercionMap CoVar), -- INVARIANT: all Vars in the range have extra information that has *already* been MSGed
    msgPending               :: [(Var, Pending)]                 -- The pending list
  }

-- INVARIANT: incoming base variable has *no* extra information beyond Name and Type/Kind (which will be anyway overwritten)
msgPend :: RnEnv2 -> Var -> Pending -> MSG Var
msgPend rn2 x0 pending = MSG $ \e s0 -> case lookupUpdatePending s0 of
    Right x                       -> Right (s0, x)
    Left (mb_eq, binderise, mk_s) -> res
      where -- This use of rn2 is 1/2 of the story necessary to ensure new common vars don't clash with rigid binders
            x1 = uniqAway (rnInScopeSet rn2) x0
            -- The use of s here is necessary to ensure we only allocate a given common var once
            x2 | Just eq <- mb_eq
               , eq `elemInScopeSet` msgCommonHeapVars (msgMode e)
               = eq
               | otherwise
               = uniqAway (msgInScopeSet s0) x1
            -- We *don't* need to uniqAway with the set of common variables (in e) because the msgInScopeSet
            -- was initialized to contain them all.

            -- NB: we make use of lazy programming to ensure that we can see the currently-pended common variable
            -- in the state even as we are binderising it. This is important since the variable's IdInfo might mention itself,
            -- as x_l and x_r will in fact be bound by the top-level letrec.
            s1 = mk_s x
            s2 = s1 { msgInScopeSet = extendInScopeSet (msgInScopeSet s1) x2, -- NB: binderization *never* changes the unique -- exploit that to avoid a loop
                      msgPending = (x, pending) : msgPending s1 }
            res = unMSG (binderise x2) (e { msgLostWorkSharing = False }) s2 -- This thing will be bound in the top letrec, outside any lambdas
            Right (_, x) = trace "forcing knot" res
  where
    lookupUpdatePending :: MSGState
                        -> Either (Maybe Var,       -- Are both sides equal vars, and if so what are they equal to?
                                   Var -> MSG Var,  -- Produce a version of the variable suitable for use as a heap binder (with generalised info/type/kind)
                                   Var -> MSGState) -- How to update the initial state with the variable for this pending item (only the "known" maps change)
                                  Var
    lookupUpdatePending s = case pending of
      -- TODO: binder matching can legitimately fail, in which case we might want to create a common "vanilla"
      -- binder with no non-MSGable info, leaving the non-unifiable specs/rules to the generalised versions?
      PendingVar      x_l  x_r  -> fmapLeft (\upd -> (if x_l == x_r then Just x_r else Nothing, \x -> msgBndrExtras rn2 x x_l x_r,                                                    \x -> s { msgKnownPendingVars      = upd x })) $ lookupUpdateVE (msgKnownPendingVars s)      x_l  x_r
      PendingType     ty_l ty_r -> fmapLeft (\upd -> (Nothing,                                  \a -> liftM (a `setTyVarKind`) $ msgKind rn2 (typeKind ty_l)     (typeKind ty_r),     \a -> s { msgKnownPendingTypes     = upd a })) $ lookupUpdateTM (msgKnownPendingTypes s)     ty_l ty_r
      PendingCoercion co_l co_r -> fmapLeft (\upd -> (Nothing,                                  \q -> liftM (q `setVarType`)   $ msgType rn2 (coercionType co_l) (coercionType co_r), \q -> s { msgKnownPendingCoercions = upd q })) $ lookupUpdateTM (msgKnownPendingCoercions s) co_l co_r
      PendingTerm     e_l  e_r  -> Left              (Nothing,                                  \x -> liftM (x `setVarType`)   $ msgType rn2 (termType e_l)      (termType e_r),      \_ -> s)
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

    fmapLeft f = either (Left . f) Right

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

newtype MSG a = MSG { unMSG :: MSGEnv -> MSGState -> MSG' (MSGState, a) }

runMSG :: MSGEnv -> MSGState -> MSG a -> MSG' (MSGState, a)
runMSG e s mx = unMSG mx e s

instance Functor MSG where
    fmap = liftM

instance Applicative MSG where
    pure = return
    (<*>) = ap

instance Monad MSG where
    return x = MSG $ \_ s -> return (s, x)
    mx >>= fxmy = MSG $ \e s -> do
      (s, x) <- unMSG mx e s
      unMSG (fxmy x) e s
    fail msg = MSG $ \_ _ -> Left msg

-- INVARIANT: neither incoming Var may be bound rigidly (rigid only matches against rigid)
msgFlexiVar :: RnEnv2 -> Var -> Var -> MSG Var
msgFlexiVar rn2 x_l x_r = msgPend rn2 (zapIdExtraInfo x_r) (PendingVar x_l x_r)

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

-- INVARIANT: neither incoming AnnedTerm can refer to something bound rigidly (don't want to lambda-abstract to float out things that reference rigids)
msgGeneraliseTerm :: RnEnv2 -> AnnedTerm -> AnnedTerm -> MSG Id
msgGeneraliseTerm rn2 e_l e_r = msgPend rn2 x (PendingTerm e_l e_r)
  where
    -- Unbiased choice of base variable: only one side may be a variable, type is MSGed at binding site
    x = (fmap zapIdExtraInfo (getVar_maybe e_l `mplus` getVar_maybe e_r)) `orElse` mkSysLocal (fsLit "genterm") uniq (termType e_r)
    uniq = mkUniqueGrimily (hashExpr (Core.mkConApp (tupleCon BoxedTuple 2) [GHC.termToCoreExpr e_l, GHC.termToCoreExpr e_r])) -- NB: pair might not be type correct, but who cares?

    getVar_maybe e = case extract e of
      Var x              -> Just x
      Value (Indirect x) -> Just x -- Because we sneakily reuse msgGeneraliseTerm for values as well
      _                  -> Nothing

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

instance MonadPlus MSG where
    mzero = fail "mzero"
    mx1 `mplus` mx2 = MSG $ \e s -> case unMSG mx1 e s of Right res -> Right res
                                                          Left _    -> unMSG mx2 e s


data MSGMode = MSGMode {
    msgCommonHeapVars :: InScopeSet
  }

type MSGResult = ((Deeds, Heap, Renaming, Stack), (Heap, Stack, Anned QA), (Deeds, Heap, Renaming, Stack))

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
msgMaybe mm s_l s_r = runMSG' (msg mm s_l s_r)


data MSGMatchResult = RightIsInstance   Heap Renaming Stack
                    | RightGivesTypeGen Renaming State Renaming

msgMatch :: InstanceMatching -> MSGResult -> Maybe MSGMatchResult
msgMatch inst_mtch ((_, Heap h_l _, rn_l, k_l), (heap@(Heap _ ids), k, qa), (deeds_r, heap_r@(Heap h_r _), rn_r, k_r))
  -- Try to detect instantiation first
  --  1) Is the left-hand renaming invertible?
  | Just rn_l_inv <- invertRenaming ids rn_l
  --  2) Is the left-hand stack empty, and if has been instantiated on the right, was that valid?
  , Loco gen_k_l <- k_l
  , case k_r of Loco _ -> True
                _      -> mayInstantiate inst_mtch gen_k_l
  --  3) Is the left-hand heap empty of anything except lambdaBounds, and if it has been instantiated on the right, was that valid?
  --     NB: we can safely ignore stack-bound variables because stack-bound vars are only matched against stack-bound vars, heap-bound
  --     ones are only matched against heap-bound ones, and we don't have any generalisation flag to check on update frames.
  , let k_r_bvs = stackBoundVars k_r
        heap_non_instantiating x_r = case M.lookup x_r h_r of
                         Nothing | x_r `elemVarSet` k_r_bvs -> True -- Instantiating with an update-frame bound thing is *probably* OK
                         Just hb_r -> isJust (heapBindingLambdaBoundness hb_r)
                         _ -> panic "msgMatch: variable unbound on right" -- (ppr rn_l $$ ppr rn_r $$ ppr x $$ ppr (renameId rn_l x) $$ ppr x_r)
  , flip all (M.toList h_l) $ \(x_l, hb_l) -> case heapBindingLambdaBoundness hb_l of
                                                Nothing -> False
                                                Just gen_l -> mayInstantiate inst_mtch gen_l || case () of
                                                                      () | isCoVar x_l, Just q <- getCoVar_maybe (lookupCoVarSubst rn_l_inv x_l), let co_r = lookupCoVarSubst rn_r q -> maybe False heap_non_instantiating (getCoVar_maybe co_r)
                                                                         | isId    x_l, let x = renameId rn_l_inv x_l,                            let x_r  = renameId rn_r x         -> heap_non_instantiating x_r
                                                                         | isTyVar x_l, Just a <- getTyVar_maybe (lookupTyVarSubst rn_l_inv x_l), let ty_r = lookupTyVarSubst rn_r a -> isJust (getTyVar_maybe ty_r)
                                                                         | otherwise   -> panic "msgMatch: impossible variable type/non-invertible renaming"
                                                                         -- TODO: perhaps type/covar instantiation should be unconditonally allowed?
  = Just (RightIsInstance heap_r (composeRenamings ids rn_l_inv rn_r) k_r)

  -- Now look for type generalisation information
  --  1) Are both stacks empty?
  | Loco _ <- k_l
  , Loco _ <- k_r
  --  2) Do both heaps only contain lambdaBounds?
  , Foldable.all (isJust . heapBindingLambdaBoundness) h_l
  , Foldable.all (isJust . heapBindingLambdaBoundness) h_r
  = Just (RightGivesTypeGen rn_l (deeds_r, heap, k, qa) rn_r)

  -- No information gained in this case :-(
  | otherwise
  = Nothing

msg :: MSGMode -> State -> State -> MSG' MSGResult
msg mm (deeds_l, heap_l, k_l, qa_l) (deeds_r, heap_r, k_r, qa_r) = -- (\res -> traceRender ("msg", M.keysSet h_l, residualiseDriveState (Heap h_l prettyIdSupply, k_l, in_e_l), M.keysSet h_r, residualiseDriveState (Heap h_r prettyIdSupply, k_r, in_e_r), res) res) $
    -- TODO: test for multiple solutions? Attempt to choose best?
    -- FIXME: use the deterministic algorithm w/ unmarking
    firstSuccess [ do ((qa, (k_l, k, k_r)), (rn_l, (heap_l, heap, heap_r), rn_r)) <- mres
                      return ((deeds_l, heap_l, rn_l, k_l), (heap, k, qa), (deeds_r, heap_r, rn_r, k_r))
                 | mrn2mk <- msgEC commons init_rn2 k_l k_r
                 , mres <- prod (do (rn2, mk) <- mrn2mk
                                    (msg_s, res@(_, (k_l, _, k_r))) <- runMSG msg_e msg_s (liftM2 (,) (msgAnned annedQA (msgQA rn2) qa_l qa_r) (mk rn2))
                                    return (map (liftM ((,) res)) $ msgPureHeap mm rn2 msg_e msg_s heap_l heap_r (stackOpenFreeVars k_l) (stackOpenFreeVars k_r)))
                 ]
  where
    -- NB: it is not necessary to include the ids_l/ids_r in these InScopeSets because the
    -- binders allocated for rigid binders are *allowed* to shadow things bound in h_l/h_r.
    -- The only things they may not shadow are things bound in the eventual common heap,
    -- but we will ensure that when we are choosing those binders in msgPend.
    --
    -- Indeed it is better to not include ids_l/ids_r so that the new common heap/stack
    -- binders often often have exactly the same uniques as their counterparts in the
    -- incoming states. This aids debugging.
    init_rn2 = mkRnEnv2 emptyInScopeSet
    commons = msgCommonHeapVars mm
    msg_e = MSGEnv False mm
    msg_s = MSGState commons emptyVarEnv emptyTM emptyTM []

    firstSuccess :: [MSG' a] -> MSG' a
    firstSuccess []   = Left "firstSuccess: no elements at all"
    firstSuccess (Left _:next:nexts) = firstSuccess (next:nexts)
    --firstSuccess [Left msg] | trace ("firstSuccess: " ++ msg) False = undefined
    firstSuccess (it:_) = it

msgAnned :: (Tag -> b -> Anned b) -> (Tag -> a -> Tag -> a -> MSG b)
         -> Anned a -> Anned a -> MSG (Anned b)
msgAnned anned f a_l a_r = liftM (anned (annedTag a_r)) $ f (annedTag a_l) (annee a_l) (annedTag a_r) (annee a_r) -- Right biased

msgQA, msgQA' :: RnEnv2 -> Tag -> QA -> Tag -> QA -> MSG QA
msgQA rn2 tg_l qa_l tg_r qa_r = msgQA' rn2 tg_l qa_l tg_r qa_r `mplus` do
    guardFloatable "msgQA" qaFreeVars rn2 qa_l qa_r
    -- NB: a QA is always cheap, so no floatability check
    liftM Question $ msgGeneraliseTerm rn2 (annedTerm tg_l (qaToAnnedTerm' (rnInScopeSet rn2) qa_l)) (annedTerm tg_r (qaToAnnedTerm' (rnInScopeSet rn2) qa_r))

msgQA' rn2 _    (Question x_l') _    (Question x_r') = liftM Question $ msgVar rn2 x_l' x_r'
msgQA' rn2 tg_l (Answer in_v_l) tg_r (Answer in_v_r) = liftM Answer $ msgAnswer rn2 tg_l in_v_l tg_r in_v_r
msgQA' _ _ _ _ _ = fail "msgQA"

msgAnswer :: RnEnv2 -> Tag -> Answer -> Tag -> Answer -> MSG Answer
msgAnswer = msgCoerced (\rn2 tg_l a_l tg_r a_r -> msgIn renameAnnedValue' annedValueFreeVars' (\rn2 v_l v_r -> msgValue rn2 tg_l v_l tg_r v_r) rn2 a_l a_r)

msgCoerced :: (RnEnv2 -> Tag -> a -> Tag -> a -> MSG b)
           -> RnEnv2 -> Tag -> Coerced a -> Tag -> Coerced a -> MSG (Coerced b)
msgCoerced f rn2  out_tg_l (Uncast,           x_l)  out_tg_r (Uncast,           x_r) = liftM ((,) Uncast) $ f rn2 out_tg_l x_l out_tg_r x_r
msgCoerced f rn2 _out_tg_l (CastBy co_l tg_l, x_l) _out_tg_r (CastBy co_r tg_r, x_r) = liftM2 (\co b -> (CastBy co tg_r, b)) (msgCoercion rn2 co_l co_r) (f rn2 tg_l x_l tg_r x_r) -- Right biased
msgCoerced _ _ _ _ _ _ = fail "msgCoerced"

-- TODO: I don't know how complete support for polykinds actually *is* in the supercompiler, so this is a bit speculative:
msgKind :: RnEnv2 -> Kind -> Kind -> MSG Kind
msgKind = msgType

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
msgType, msgType' :: RnEnv2 -> Type -> Type -> MSG Type
msgType rn2 ty_l ty_r = msgType' rn2 ty_l ty_r `mplus` do
    guardFloatable "msgType" tyVarsOfType rn2 ty_l ty_r
    liftM TyVarTy $ msgGeneraliseType rn2 ty_l ty_r

msgType' rn2 (TyVarTy x_l)         (TyVarTy x_r)         = liftM TyVarTy $ msgVar rn2 x_l x_r
msgType' rn2 (AppTy ty1_l ty2_l)   (AppTy ty1_r ty2_r)   = liftM2 mkAppTy (msgType rn2 ty1_l ty1_r) (msgType rn2 ty2_l ty2_r)
msgType' _   (TyConApp tc_l [])    (TyConApp tc_r [])    = guard "msgType: TyConApp" (tc_l == tc_r) >> return (TyConApp tc_r [])
msgType' rn2 (TyConApp tc_l tys_l) (TyConApp tc_r tys_r) = msgType rn2 (foldl AppTy (TyConApp tc_l []) tys_l) (foldl AppTy (TyConApp tc_r []) tys_r)
msgType' rn2 (FunTy ty1_l ty2_l)   (FunTy ty1_r ty2_r)   = msgType rn2 ((TyConApp funTyCon [] `AppTy` ty1_l) `AppTy` ty2_l) ((TyConApp funTyCon [] `AppTy` ty1_r) `AppTy` ty2_r)
msgType' rn2 (ForAllTy a_l ty_l)   (ForAllTy a_r ty_r)   = msgTyVarBndr ForAllTy rn2 a_l a_r $ \rn2 -> msgType rn2 ty_l ty_r
msgType' _ _ _ = fail "msgType"

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
msgTerm rn2 e_l e_r = msgAnned annedTerm (msgTerm' rn2) e_l e_r `mplus` do
    guardFloatable "msgTerm" annedTermFreeVars rn2 e_l e_r
    msgCheckFloatable (termIsCheap e_l) (termIsCheap e_r)
    liftM (fmap Var . annedVar (annedTag e_r)) $ msgGeneraliseTerm rn2 e_l e_r -- Right biased

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
msgValue rn2 tg_l v_l tg_r v_r = msgValue' rn2 v_l v_r `mplus` do
    guardFloatable "msgValue" annedValueFreeVars' rn2 v_l v_r
    -- NB: values are always cheap, so no floatability check
    liftM Indirect $ msgGeneraliseTerm rn2 (fmap Value $ annedValue tg_l v_l) (fmap Value $ annedValue tg_r v_r)

msgValue' :: RnEnv2 -> AnnedValue -> AnnedValue -> MSG AnnedValue
msgValue' rn2 (Indirect x_l)               (Indirect x_r)               = liftM Indirect $ msgVar rn2 x_l x_r
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
msgIdCoVarBndr f rn2 x_l x_r k = msgIdCoVarBndrFlexible f rn2 x_l x_r (\rn2' -> liftM ((,) rn2') (k rn2'))

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
msgVarL_maybe rn2 x_l e_r = guard "msgVarL_maybe: no instance msging"  iNSTANCE_MATCHING >> case rnOccL_maybe rn2 x_l of
     -- Left rigidly bound: msging is impossible (assume we already tried msgVar_maybe)
    Just _  -> fail "msgVar: rigid"
     -- Both bound by let: defer decision about msging
    Nothing -> return (Just (VarL x_l e_r))

msgVarR :: RnEnv2 -> Out AnnedTerm -> Out Id -> MSG [MSGLR]
msgVarR rn2 e_l x_r = fmap maybeToList (msgVarR_maybe rn2 e_l x_r)

msgVarR_maybe :: RnEnv2 -> Out AnnedTerm -> Out Id -> MSG (Maybe MSGLR)
msgVarR_maybe rn2 e_l x_r = guard "msgVarR_maybe: no instance msging" iNSTANCE_MATCHING >> case rnOccR_maybe rn2 x_r of
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

msgEC :: InScopeSet -> RnEnv2 -> Stack -> Stack -> [MSG' (RnEnv2, RnEnv2 -> MSG (Stack, Stack, Stack))]
msgEC commons init_rn2 = go init_rn2
  where
    splitCar _   (Car x xs) = return (x, xs)
    splitCar msg (Loco _)   = Left ("splitCar: " ++ msg)

    go rn2' k_l k_r
      = prod (do (kf_l, k_l) <- splitCar "left" k_l
                 (kf_r, k_r) <- splitCar "right" k_r
                 (rn2'', mkf') <- msgECFrame commons rn2' kf_l kf_r
                 return (map (liftM (second (\it rn2 -> liftM2 (\kf (k_l, k, k_r) -> (k_l, kf `Car` k, k_r)) (mkf' rn2) (it rn2)))) $ go rn2'' k_l k_r)) ++
        [return (rn2', \_ -> return (k_l, Loco (case k_r of Loco gen -> gen; _ -> True), k_r))] -- Right biased generalisation flag

msgECFrame :: InScopeSet -> RnEnv2 -> Tagged StackFrame -> Tagged StackFrame -> MSG' (RnEnv2, RnEnv2 -> MSG (Tagged StackFrame))
msgECFrame commons init_rn2 kf_l kf_r = liftM (second (liftM (Tagged (tag kf_r)) .)) $ go (tagee kf_l) (tagee kf_r) -- Right biased
  where
    go :: StackFrame -> StackFrame -> MSG' (RnEnv2, RnEnv2 -> MSG StackFrame)
    go (Apply x_l')                          (Apply x_r')                          = return (init_rn2,  \rn2 -> liftM Apply $ msgVar rn2 x_l' x_r')
    go (TyApply ty_l')                       (TyApply ty_r')                       = return (init_rn2,  \rn2 -> liftM TyApply $ msgType rn2 ty_l' ty_r')
    go (Scrutinise x_l' ty_l' in_alts_l)     (Scrutinise x_r' ty_r' in_alts_r)     = return (init_rn2,  \rn2 -> liftM2 (\ty (x, in_alts) -> Scrutinise x ty in_alts) (msgType rn2 ty_l' ty_r') (msgIdCoVarBndr (,) rn2 x_l' x_r' $ \rn2 -> msgIn renameAnnedAlts annedAltsFreeVars msgAlts rn2 in_alts_l in_alts_r))
    go (PrimApply pop_l tys_l' as_l in_es_l) (PrimApply pop_r tys_r' as_r in_es_r) = return (init_rn2,  \rn2 -> guard "msgECFrame: primop" (pop_l == pop_r) >> liftM3 (PrimApply pop_r) (zipWithEqualM (msgType rn2) tys_l' tys_r') (zipWithEqualM (msgAnned annedAnswer (msgAnswer rn2)) as_l as_r) (zipWithEqualM (msgIn renameAnnedTerm annedTermFreeVars msgTerm rn2) in_es_l in_es_r))
    go (StrictLet x_l' in_e_l)               (StrictLet x_r' in_e_r)               = return (init_rn2,  \rn2 -> msgIdCoVarBndr StrictLet rn2 x_l' x_r' $ \rn2 -> msgIn renameAnnedTerm annedTermFreeVars msgTerm rn2 in_e_l in_e_r)
    go (CastIt co_l')                        (CastIt co_r')                        = return (init_rn2,  \rn2 -> liftM CastIt $ msgCoercion rn2 co_l' co_r')
    go (Update x_l')                         (Update x_r')                         = return (init_rn2', \rn2 -> liftM Update $ msgIdCoVarBndrExtras rn2 x' x_l' x_r')
      where (init_rn2', x') = rnBndr2'' (if x_l' == x_r' && x_r' `elemInScopeSet` commons then const x_r' else uniqAway commons) init_rn2 x_l' x_r'
    go _ _ = Left "msgECFrame"

-- NB: we must enforce invariant that stuff "outside" cannot refer to stuff bound "inside" (heap *and* stack)
msgPureHeap :: MSGMode -> RnEnv2 -> MSGEnv -> MSGState -> Heap -> Heap -> (BoundVars, FreeVars) -> (BoundVars, FreeVars) -> [MSG' (Renaming, (Heap, Heap, Heap), Renaming)]
msgPureHeap mm rn2 msg_e msg_s (Heap init_h_l init_ids_l) (Heap init_h_r init_ids_r) (k_bvs_l, k_fvs_l) (k_bvs_r, k_fvs_r)
  = prod (do (used_l, h_l) <- sucks init_h_l k_bvs_l M.empty S.empty k_fvs_l
             (used_r, h_r) <- sucks init_h_r k_bvs_r M.empty S.empty k_fvs_r
             return $ go emptyRenaming emptyRenaming used_l used_r init_h_l init_h_r (Heap h_l init_ids_l) (Heap h_r init_ids_r) M.empty msg_s)
  where
    go :: Renaming  -- Renaming that should be applied to common stuff when instantiated on the left
       -> Renaming  -- Renaming that should be applied to common stuff when instantiated on the right
       -> S.Set Var -- Binding in left heap already copied into one of the heaps
       -> S.Set Var -- Binding in left heap already copied into one of the heaps
       -> PureHeap  -- Potentially uncopied left heap
       -> PureHeap  -- Potentially uncopied right heap
       -> Heap      -- Left heap
       -> Heap      -- Right heap
       -> PureHeap  -- Common heap
       -> MSGState  -- Pending matches etc. MSGState is also used to ensure we never match a var pair more than once (they get the same common name)
       -> [MSG' (Renaming, (Heap, Heap, Heap), Renaming)]
    go rn_l rn_r _ _ _ _ heap_l heap_r h (MSGState { msgPending = [], msgInScopeSet = ids })
      -- NB: it is very important that the final InScopeSet includes not only the InScopeSet from the MSGState (which
      -- will include all of the common *heap* bound variables) but also that from the RnEnv2 (which will include
      -- all of the common *stack* bound variables).
      = [return (rn_l, (heap_l, Heap h (ids `unionInScope` rnInScopeSet rn2), heap_r), rn_r)]
    {-
    go _ _ _ _ _ _ _ _ _ (MSGState { msgPending = ((x_common, next):_) })
      | pprTrace "msgPureHeap" (ppr x_common $$ ppr next) False
      = undefined
    -}
    go rn_l rn_r used_l used_r init_h_l init_h_r (Heap h_l ids_l) (Heap h_r ids_r) h msg_s@(MSGState { msgPending = ((x_common, PendingVar x_l x_r):rest) })
      -- Just like an internal binder, we have to be sure to match the binders themselves (for e.g. type variables)
      | msg_s <- msg_s { msgPending = rest }
      = prod (do (used_l, hb_l) <- mb_common_l
                 (used_r, hb_r) <- mb_common_r
                 (h_l, h_r, rn_l, rn_r, msg_s, hb) <- case (inject hb_l, inject hb_r) of
                   (Just (Just (let_bound_l, in_e_l)), Just (Just (let_bound_r, in_e_r)))
                     | let_bound_l == let_bound_r
                     , not let_bound_r || (x_l == x_r && x_r == x_common) -- Note [MSGing let-bounds] (we have to check this even if matching RHSs because we need to choose a common binder that will be in scope on both sides)
                     , Right (msg_s, in_e) <- case () of
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
                          _ | x_l == x_r, x_r `elemInScopeSet` msgCommonHeapVars mm
                            , let l_fvs = inFreeVars annedTermFreeVars in_e_l
                                  r_fvs = inFreeVars annedTermFreeVars in_e_r
                            , l_fvs == r_fvs
                            , Right res <- runMSG msg_e msg_s $ do Foldable.mapM_ (\x -> msgFlexiVar rn2 x x >>= \x' -> guard "msgPureHeap: shortcut" (x' == x) >> return ()) r_fvs
                                                                   return in_e_r -- Right biased
                            -> Right res
                            | otherwise
                            -> runMSG msg_e msg_s $ msgIn renameAnnedTerm annedTermFreeVars msgTerm rn2 in_e_l in_e_r
                     -> return (h_l, h_r, rn_l, rn_r, msg_s, (if let_bound_r then letBound else internallyBound) in_e)
                   (Just Nothing, Just Nothing)
                     | x_l == x_r && x_r == x_common -- Note [MSGing let-bounds]
                     -> return (h_l, h_r, rn_l, rn_r, msg_s, hb_r) -- Right biased
                   (Nothing, Nothing) -- NB: this branch is only different from *not* generalising because we don't generalise the binder IdInfo
                     -> return (M.insert x_l hb_l h_l, M.insert x_r hb_r h_r,
                                insertIdRenaming rn_l x_common x_l, insertIdRenaming rn_r x_common x_r,
                                msg_s, lambdaBound)
                   _ -> Left "msgPureHeap: non-unifiable heap bindings"
                 -- If they match, we need to make a common heap binding
                 return (go rn_l rn_r used_l used_r init_h_l init_h_r
                            (Heap h_l ids_l) (Heap h_r ids_r) (M.insert x_common hb h) msg_s)) ++
        -- If they don't match, we need to generalise
        prod (do -- Whenever we add a new "outside" binding like this we have to transitively copy in all the things
                 -- that binding refers to. If that is not possible, we have to fail.
                 (used_l', h_l') <- mb_individual_l >>= suck init_h_l k_bvs_l h_l x_l
                 (used_r', h_r') <- mb_individual_r >>= suck init_h_r k_bvs_r h_r x_r
                 return $ go (insertIdRenaming rn_l x_common x_l) (insertIdRenaming rn_r x_common x_r) used_l' used_r' init_h_l init_h_r
                             (Heap h_l' ids_l) (Heap h_r' ids_r) (M.insert x_common generalised h) msg_s) -- FIXME: only mark as generalised if *right hand side* was not e.g. a lambda bound
      where
        (mb_common_l, mb_individual_l) = find init_h_l k_bvs_l h_l used_l x_l
        (mb_common_r, mb_individual_r) = find init_h_r k_bvs_r h_r used_r x_r
    go rn_l rn_r used_l used_r init_h_l init_h_r (Heap h_l ids_l) (Heap h_r ids_r) h msg_s@(MSGState { msgPending = ((x_common, PendingTerm e_l e_r):rest) })
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

      -- Match binders themselves, but in this case we can't reuse msgIdCoVarBndrExtras, which is annoying :-(. We rely on the fact that x_common always has no extra info.
      -- NB: binder matching here never fails because type matching never fails, and x_common is guaranteed created with no extra info.
      | msg_s <- msg_s { msgPending = rest }
      -- We already know the terms don't match so we are going to generalise
      = prod (do let (ids_l', x_common_l) = uniqAway' ids_l x_common
                     (ids_r', x_common_r) = uniqAway' ids_r x_common
                 (used_l', h_l') <- sucks init_h_l k_bvs_l (M.insert x_common_l (internallyBound (renamedTerm e_l)) h_l) used_l (annedTermFreeVars e_l)
                 (used_r', h_r') <- sucks init_h_r k_bvs_l (M.insert x_common_r (internallyBound (renamedTerm e_r)) h_r) used_r (annedTermFreeVars e_r)
                 return (go (insertIdRenaming rn_l x_common x_common_l) (insertIdRenaming rn_r x_common x_common_r) used_l' used_r' init_h_l init_h_r
                            (Heap h_l' ids_l') (Heap h_r' ids_r') (M.insert x_common generalised h) msg_s))
    go rn_l rn_r used_l used_r init_h_l init_h_r (Heap h_l ids_l) (Heap h_r ids_r) h msg_s@(MSGState { msgPending = ((a_common, PendingType ty_l ty_r):rest) })
      -- NB: the heap only ever maps TyVars to lambdaBound/generalised, so there is no point trying to detect TyVars on either side
      | msg_s <- msg_s { msgPending = rest }
      -- We already know the types don't match so we are going to generalise. Note that these particular "sucks" can never fail:
      = prod (do (used_l', h_l') <- sucks init_h_l k_bvs_l h_l used_l (tyVarsOfType ty_l)
                 (used_r', h_r') <- sucks init_h_r k_bvs_l h_r used_r (tyVarsOfType ty_r)
                 return (go (insertTypeSubst rn_l a_common ty_l) (insertTypeSubst rn_r a_common ty_r) used_l' used_r' init_h_l init_h_r
                            (Heap h_l' ids_l) (Heap h_r' ids_r) (M.insert a_common generalised h) msg_s))
    go rn_l rn_r used_l used_r init_h_l init_h_r (Heap h_l ids_l) (Heap h_r ids_r) h msg_s@(MSGState { msgPending = ((q_common, PendingCoercion co_l co_r):rest) })
      -- TODO: I could try to avoid generalisation when co_l or co_r is just a heap-bound variable. We could do this (in the same way as PendingTerm) by floating
      -- the non-variable into a new heap binding which looks just like it was in the initial heap on the left/right and then matching the variable pair we are
      -- left with as normal.
      --
      -- The only problem with this plan is that co_l and co_r are untagged, so I wouldn't know how to tag the new heap bindings. The best I can do given the current
      -- supercompiler data types is to tag the new coercion HeapBindings with the tags of the *thing that they coerce*, which doesn't seem cool at all!
      --
      -- Doing this may also suffer from the standard problems with instance matching (see PendingTerm)
      | msg_s <- msg_s { msgPending = rest }
      -- We already know the coercions don't match so we are going to generalise
      = prod (do (used_l', h_l') <- sucks init_h_l k_bvs_l h_l used_l (tyCoVarsOfCo co_l)
                 (used_r', h_r') <- sucks init_h_r k_bvs_l h_r used_r (tyCoVarsOfCo co_r)
                 return (go (insertCoercionSubst rn_l q_common co_l) (insertCoercionSubst rn_r q_common co_r) used_l' used_r' init_h_l init_h_r
                            (Heap h_l' ids_l) (Heap h_r' ids_r) (M.insert q_common generalised h) msg_s))

    find :: PureHeap -> BoundVars -> PureHeap -> S.Set Var
         -> Var -> (MSG' (S.Set Var, HeapBinding),
                      -- Nothing ==> unavailable for common heap
                      -- Just    ==> available for common heap with this usage
                    MSG' (S.Set Var, Maybe HeapBinding))
                      -- Nothing      ==> unavailable for individual heap
                      -- Just Nothing ==> available for individual heap but already in it
                      -- Just Just    ==> available for individual heap and not in it yet
    find init_h k_bvs h used x = second (\individual -> if x `M.member` h then return (used, Nothing) else individual) $ case M.lookup x init_h of
         -- Variable bound by the heap (vastly common case):
        Just hb | S.notMember x used -> (return (used', hb),       return (used', Just hb))
                | otherwise          -> (lft "used heap binding (common)", lft "used heap binding (individual)")
          where used' | Just (_, e) <- heapBindingTerm hb
                      , not (termIsCheap e)
                      = S.insert x used
                      | otherwise
                      = used
         -- By a process of elimination, variable must be bound be the stack. Normally it will in fact be bound by the "instance" portion
         -- of the stack because matches involving the common portion variables either already failed or were discharged by RnEnv2, but
         -- if "find" is called by "sucks" then this may not necessarily be the case
        Nothing -> (lft "used stack binding (common)", if x `elemVarSet` k_bvs then return (used, Just lambdaBound) else lft "used stack binding (individual)")
     where lft msg = Left (msg ++ ": " ++ showPpr x)

    suck :: PureHeap -> BoundVars -> PureHeap -> Var -> (S.Set Var, Maybe HeapBinding) -> MSG' (S.Set Var, PureHeap)
    suck _      _     h _ (used, Nothing) = return (used, h)                                                                                   -- Already copied in
    suck init_h k_bvs h x (used, Just hb) = sucks init_h k_bvs (M.insert x hb h) used (heapBindingFreeVars hb `unionVarSet` varBndrFreeVars x) -- Not yet copied: put in and consider FVs

    sucks :: PureHeap -> BoundVars -> PureHeap -> S.Set Var -> FreeVars -> MSG' (S.Set Var, PureHeap)
    sucks init_h k_bvs h used fvs = foldM (\(used, h) x -> snd (find init_h k_bvs h used x) >>= suck init_h k_bvs h x) (used, h) (varSetElems fvs)

    {-
    varTermPair :: AnnedTerm -> AnnedTerm -> Maybe (Var, AnnedTerm)
    varTermPair e_l e_r = case extract e_l of
        Var   x_l                                     -> Just (x_l, e_r)
        Value (Indirect x_l) | Value _ <- extract e_r -> Just (x_l, e_r) -- We also (sneakily) use PendingTerm to deal with indirection/value mismatches
        _ -> Nothing
    -}

    inject :: HeapBinding -> Maybe (Maybe (Bool, In AnnedTerm))
    inject (HB { howBound = how_bound, heapBindingMeaning = meaning })
      | LambdaBound <- how_bound
      , Left _ <- meaning
      = Nothing
      | LetBound <- how_bound
      , Left _ <- meaning
      = Just Nothing
      | Just let_bound <- case how_bound of
          LetBound        -> Just True
          InternallyBound -> Just False
          LambdaBound     -> Nothing
      , Right in_e <- meaning
      = Just $ Just (let_bound, in_e)
      | otherwise
      = pprPanic "msgPureHeap: unhandled heap binding format" (ppr how_bound $$ (case meaning of Left _ -> text "Left"; Right _ -> text "Right"))

prod :: MSG' [MSG' a] -> [MSG' a]
prod (Left msg)  = [Left msg]
prod (Right mxs) = mxs

guardFloatable :: String -> (a -> FreeVars) -> RnEnv2 -> a -> a -> MSG ()
guardFloatable msg fvs rn2 x_l x_r = guard (msg ++ ": unfloatable") (canFloat fvs (rnOccL_maybe rn2) x_l && canFloat fvs (rnOccR_maybe rn2) x_r)

canFloat :: (b -> FreeVars) -> (Var -> Maybe a) -> b -> Bool
canFloat fvs lkup = foldVarSet (\a rest_ok -> maybe rest_ok (const False) (lkup a)) True . fvs

uniqAway' :: InScopeSet -> Var -> (InScopeSet, Var)
uniqAway' ids x = (ids `extendInScopeSet` x', x')
  where x' = uniqAway ids x
