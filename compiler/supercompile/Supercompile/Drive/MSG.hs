module Supercompile.Drive.MSG (
    msg
  ) where

#include "HsVersions.h"

import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.Evaluator.Deeds
import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Syntax

import Supercompile.Utilities hiding (guard)

import qualified CoreSyn as Core

import Util
import Coercion
import Var        (TyVar, isTyVar, isId, tyVarKind, setVarType, setTyVarKind)
import Id         (Id, idType, idName, realIdUnfolding, setIdUnfolding, idSpecialisation, setIdSpecialisation, isGlobalId)
import IdInfo     (SpecInfo(..))
import VarEnv
import TypeRep    (Kind, Type(..))
import Rules      (mkSpecInfo, roughTopNames)

import Control.Monad.Fix

import qualified Data.Map as M
import qualified Data.Set as S


pprTraceSC :: String -> SDoc -> a -> a
--pprTraceSC _ _ = id
--pprTraceSC = pprTrace
pprTraceSC msg doc a = traceSC (msg ++ ": " ++ showSDoc doc) a

traceSC :: String -> a -> a
traceSC _ = id
--traceSC = trace


rnBndr2' :: RnEnv2 -> Var -> Var -> (RnEnv2, Var)
rnBndr2' rn2 x_l x_r = (rn2', x)
  where rn2' = rnBndr2 rn2 x_l x_r
        Just x = rnOccL_maybe rn2' x_l


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

data MSGState = MSGState {
    msgInScopeSet      :: InScopeSet,           -- We have to ensure all new vars are distinct from *each other* AND *top level vars*
    msgKnownFlexiPairs :: M.Map (Var, Var) Var, -- INVARIANT: inverse
    msgPending         :: [(Var, (Var, Var))]   -- of each other
  }

-- INVARIANTs:
--   1. All the Vars are *not* rigidly bound (i.e. they are bound by a "let")
--   2. The terms *may* contain free variables of any kind, rigidly bound or not
--   3. The Vars in VarL/VarR are always CoVars/Ids, NEVER TyVars (might change this in the future)
newtype MSG a = MSG { unMSG :: MSGState -> MSG' (MSGState, a) }

instance Functor MSG where
    fmap = liftM

instance Applicative MSG where
    pure = return
    (<*>) = ap

instance Monad MSG where
    return x = MSG $ \s -> return (s, x)
    mx >>= fxmy = MSG $ \s -> do
      (s, x) <- unMSG mx s
      unMSG (fxmy x) s
    fail msg = MSG $ \_ -> Left msg

instance MonadFix MSG where
    mfix xmy = MSG $ \s -> let res = unMSG (xmy x) s
                               Right (_, x) = res
                           in res

msgFlexiVar :: Var -> Var -> MSG Var
msgFlexiVar x_l x_r = MSG $ \s -> Right $ case M.lookup (x_l, x_r) (msgKnownFlexiPairs s) of
  Nothing -> if x == x_r && not (isGlobalId x)
              then pprPanic "msgFlexiVar" (ppr x) -- FIXME: I don't know why global vars are showing up here, this indicates a bug elsewehre
              else (s { msgInScopeSet = extendInScopeSet (msgInScopeSet s) x,
                    msgKnownFlexiPairs = M.insert (x_l, x_r) x (msgKnownFlexiPairs s),
                    msgPending = (x, (x_l, x_r)) : msgPending s }, x)
    where x = uniqAway (msgInScopeSet s) x_r
  Just x  -> (s, x)

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

-- instance MonadPlus MSG where
--     mzero = fail "mzero"
--     mx1 `mplus` mx2 = MSG $ unMSG mx1 `mplus` unMSG mx2


{-
data MSGMode = MM {
    msgCommonHeapVars :: S.Set Var
  }
-}

type MSGResult = ((Deeds, Heap, Renaming, Stack), (Heap, Stack, Anned QA), (Deeds, Heap, Renaming, Stack))

msg :: {- MSGMode -- ^ How to match
    -> -} State   -- ^ Tieback semantics
    -> State   -- ^ This semantics
    -> Maybe MSGResult -- ^ Renaming from left to right
msg {- mm -} s_l s_r = runMSG' (msgWithReason {- mm -} s_l s_r)

msgWithReason :: {- MSGMode -> -} State -> State -> MSG' MSGResult
msgWithReason {- mm -} (deeds_l, Heap h_l ids_l, k_l, qa_l) (deeds_r, Heap h_r ids_r, k_r, qa_r) = -- (\res -> traceRender ("msg", M.keysSet h_l, residualiseDriveState (Heap h_l prettyIdSupply, k_l, in_e_l), M.keysSet h_r, residualiseDriveState (Heap h_r prettyIdSupply, k_r, in_e_r), res) res) $
  do
    -- It's very important that we don't just use the state free variables from both sides to construct the initial in scope set,
    -- because we use it to msg the stack and QA on each side *without* first extending it with variables bound by the PureHeap!
    --
    -- The InScopeSets from the Heap are guaranteed to take these into account (along with the stuff bound by the stack, but that
    -- doesn't matter too much) so we just use those instead.
    --
    -- This was the source of a very confusing bug :-(
    let ids = ids_l `unionInScope` ids_r
        init_rn2 = mkRnEnv2 ids
        msg_s = MSGState ids M.empty []
    case [ ((deeds_l, Heap h_l ids_l, mkRenaming (M.toList rn_l), k_l), (heap, k, qa), (deeds_r, Heap h_r ids_r, mkRenaming (M.toList rn_r), k_r))
         | (rn2, mk) <- mfix $ \(~(rn2, _)) -> msgEC init_rn2 rn2 k_l k_r
         , Right (msg_s, (qa, (k_l, k, k_r))) <- [unMSG (liftM2 (,) (msgAnned (msgQA rn2) qa_l qa_r) mk) msg_s]
         , (rn_l, (h_l, heap, h_r), rn_r) <- msgPureHeap {- mm -} rn2 msg_s h_l h_r (stackOpenFreeVars k_l) (stackOpenFreeVars k_r)
         ] of
      []      -> fail "msgWithReason: no acceptable combination"
      (res:_) -> return res -- TODO: test for multiple solutions? Attempt to choose best?

msgAnned :: (a -> a -> MSG b)
         -> Anned a -> Anned a -> MSG (Anned b)
msgAnned f a_l a_r = flip traverse a_r $ f (annee a_l) -- Right biased

msgQA :: RnEnv2 -> QA -> QA -> MSG QA
msgQA rn2 (Question x_l') (Question x_r') = liftM Question $ msgVar rn2 x_l' x_r'
msgQA rn2 (Answer in_v_l) (Answer in_v_r) = liftM Answer $ msgAnswer rn2 in_v_l in_v_r
msgQA _ _ _ = fail "msgQA"

msgAnswer :: RnEnv2 -> Answer -> Answer -> MSG Answer
msgAnswer = msgCoerced (msgIn renameAnnedValue' annedValueFreeVars' msgValue)

msgCoerced :: (RnEnv2 -> a -> a -> MSG b)
           -> RnEnv2 -> Coerced a -> Coerced a -> MSG (Coerced b)
msgCoerced f rn2 (Uncast,            x_l) (Uncast,           x_r) = liftM ((,) Uncast) $ f rn2 x_l x_r
msgCoerced f rn2 (CastBy co_l _tg_l, x_l) (CastBy co_r tg_r, x_r) = liftM2 (\co b -> (CastBy co tg_r, b)) (msgCoercion rn2 co_l co_r) (f rn2 x_l x_r) -- Right biased
msgCoerced _ _ _ _ = fail "msgCoerced"

-- TODO: I don't know how complete support for polykinds actually *is* in the supercompiler, so this is a bit speculative:
msgKind :: RnEnv2 -> Kind -> Kind -> MSG Kind
msgKind = msgType

-- TODO: msg type instantiation?
msgType :: RnEnv2 -> Type -> Type -> MSG Type
msgType rn2 (TyVarTy x_l)         (TyVarTy x_r)         = liftM TyVarTy $ msgVar rn2 x_l x_r
msgType rn2 (AppTy ty1_l ty2_l)   (AppTy ty1_r ty2_r)   = liftM2 AppTy (msgType rn2 ty1_l ty1_r) (msgType rn2 ty2_l ty2_r)
msgType rn2 (TyConApp tc_l tys_l) (TyConApp tc_r tys_r) = guard "msgType: TyConApp" (tc_l == tc_r) >> liftM (TyConApp tc_r) (zipWithEqualM (msgType rn2) tys_l tys_r)
msgType rn2 (FunTy ty1_l ty2_l)   (FunTy ty1_r ty2_r)   = liftM2 FunTy (msgType rn2 ty1_l ty1_r) (msgType rn2 ty2_l ty2_r)
msgType rn2 (ForAllTy a_l ty_l)   (ForAllTy a_r ty_r)   = msgTyVarBndr ForAllTy rn2 a_l a_r $ \rn2 -> msgType rn2 ty_l ty_r
msgType _ _ _ = fail "msgType"

-- TODO: msg coercion instantiation?
msgCoercion :: RnEnv2 -> Coercion -> Coercion -> MSG Coercion
msgCoercion rn2 (Refl ty_l)              (Refl ty_r)              = liftM Refl $ msgType rn2 ty_l ty_r
msgCoercion rn2 (TyConAppCo tc_l cos_l)  (TyConAppCo tc_r cos_r)  = guard "msgCoercion: TyConAppCo" (tc_l == tc_r) >> liftM (TyConAppCo tc_r) (zipWithEqualM (msgCoercion rn2) cos_l cos_r)
msgCoercion rn2 (AppCo co1_l co2_l)      (AppCo co1_r co2_r)      = liftM2 AppCo (msgCoercion rn2 co1_l co1_r) (msgCoercion rn2 co2_l co2_r)
msgCoercion rn2 (ForAllCo a_l co_l)      (ForAllCo a_r co_r)      = msgTyVarBndr ForAllCo rn2 a_l a_r $ \rn2 -> msgCoercion rn2 co_l co_r
msgCoercion rn2 (CoVarCo a_l)            (CoVarCo a_r)            = liftM CoVarCo $ msgVar rn2 a_l a_r
msgCoercion rn2 (AxiomInstCo ax_l cos_l) (AxiomInstCo ax_r cos_r) = guard "msgCoercion: AxiomInstCo" (ax_l == ax_r) >> liftM (AxiomInstCo ax_r) (zipWithEqualM (msgCoercion rn2) cos_l cos_r)
msgCoercion rn2 (UnsafeCo ty1_l ty2_l)   (UnsafeCo ty1_r ty2_r)   = liftM2 UnsafeCo (msgType rn2 ty1_l ty1_r) (msgType rn2 ty2_l ty2_r)
msgCoercion rn2 (SymCo co_l)             (SymCo co_r)             = liftM SymCo $ msgCoercion rn2 co_l co_r
msgCoercion rn2 (TransCo co1_l co2_l)    (TransCo co1_r co2_r)    = liftM2 TransCo (msgCoercion rn2 co1_l co1_r) (msgCoercion rn2 co2_l co2_r)
msgCoercion rn2 (NthCo i_l co_l)         (NthCo i_r co_r)         = guard "msgCoercion: NthCo" (i_l == i_r) >> liftM (NthCo i_r) (msgCoercion rn2 co_l co_r)
msgCoercion rn2 (InstCo co_l ty_l)       (InstCo co_r ty_r)       = liftM2 InstCo (msgCoercion rn2 co_l co_r) (msgType rn2 ty_l ty_r)
msgCoercion _ _ _ = fail "msgCoercion"

msgTerm :: RnEnv2 -> AnnedTerm -> AnnedTerm -> MSG AnnedTerm
msgTerm rn2 = msgAnned (msgTerm' rn2)

-- TODO: allow lets on only one side? Useful for msging e.g. (let x = 2 in y + x) with (z + 2)
msgTerm' :: RnEnv2 -> TermF Anned -> TermF Anned -> MSG (TermF Anned)
msgTerm' rn2 (Var x_l)                  (Var x_r)                  = liftM Var $ msgVar rn2 x_l x_r
msgTerm' rn2 (Value v_l)                (Value v_r)                = liftM Value $ msgValue rn2 v_l v_r
msgTerm' rn2 (TyApp e_l ty_l)           (TyApp e_r ty_r)           = liftM2 TyApp (msgTerm rn2 e_l e_r) (msgType rn2 ty_l ty_r)
msgTerm' rn2 (App e_l x_l)              (App e_r x_r)              = liftM2 App (msgTerm rn2 e_l e_r) (msgVar   rn2 x_l x_r)
msgTerm' rn2 (PrimOp pop_l tys_l es_l)  (PrimOp pop_r tys_r es_r)  = guard "msgTerm: primop" (pop_l == pop_r) >> liftM2 (PrimOp pop_r) (zipWithEqualM (msgType rn2) tys_l tys_r) (zipWithEqualM (msgTerm rn2) es_l es_r)
msgTerm' rn2 (Case e_l x_l ty_l alts_l) (Case e_r x_r ty_r alts_r) = liftM3 (\e ty (x, alts) -> Case e x ty alts) (msgTerm rn2 e_l e_r) (msgType rn2 ty_l ty_r) (msgIdCoVarBndr (,) rn2 x_l x_r $ \rn2 -> msgAlts rn2 alts_l alts_r)
msgTerm' rn2 (Let x_l e1_l e2_l)        (Let x_r e1_r e2_r)        = liftM2 (\e1 (x, e2) -> Let x e1 e2) (msgTerm rn2 e1_l e1_r) $ msgIdCoVarBndr (,) rn2 x_l x_r $ \rn2 -> msgTerm rn2 e2_l e2_r
msgTerm' rn2 (LetRec xes_l e_l)         (LetRec xes_r e_r)         = msgIdCoVarBndrs (\xs (es, e) -> LetRec (zipEqual "msgTerm: letrec" xs es) e) rn2 xs_l xs_r $ \rn2 -> liftM2 (,) (zipWithEqualM (msgTerm rn2) es_l es_r) (msgTerm rn2 e_l e_r)
  where (xs_l, es_l) = unzip xes_l
        (xs_r, es_r) = unzip xes_r
msgTerm' rn2 (Cast e_l co_l)            (Cast e_r co_r)            = liftM2 Cast (msgTerm rn2 (e_l) (e_r)) (msgCoercion rn2 (co_l) (co_r))
msgTerm' _ _ _ = fail "msgTerm"

msgValue :: RnEnv2 -> AnnedValue -> AnnedValue -> MSG AnnedValue
msgValue rn2 (Indirect x_l)               (Indirect x_r)               = liftM Indirect $ msgVar rn2 x_l x_r
msgValue rn2 (TyLambda a_l e_l)           (TyLambda a_r e_r)           = msgTyVarBndr TyLambda rn2 a_l a_r $ \rn2 -> msgTerm rn2 e_l e_r
msgValue rn2 (Lambda x_l e_l)             (Lambda x_r e_r)             = msgIdCoVarBndr Lambda rn2 x_l x_r $ \rn2 -> msgTerm rn2 e_l e_r
msgValue rn2 (Data dc_l tys_l cos_l xs_l) (Data dc_r tys_r cos_r xs_r) = guard "msgValue: datacon" (dc_l == dc_r) >> liftM3 (Data dc_r) (zipWithEqualM (msgType rn2) tys_l tys_r) (zipWithEqualM (msgCoercion rn2) cos_l cos_r) (zipWithEqualM (msgVar rn2) xs_l xs_r)
msgValue _   (Literal l_l)                (Literal l_r)                = guard "msgValue: literal" (l_l == l_r) >> return (Literal l_r)
msgValue rn2 (Coercion co_l)              (Coercion co_r)              = liftM Coercion $ msgCoercion rn2 co_l co_r
msgValue _ _ _ = fail "msgValue"

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
msgVarBndr f rn2 v_l v_r | isId v_l,    isId v_r    = msgIdCoVarBndr f rn2 v_l v_r
                         | isTyVar v_l, isTyVar v_r = msgTyVarBndr   f rn2 v_l v_r
                         | otherwise                = fail "msgVarBndr"

msgTyVarBndr :: (TyVar -> b -> c) -> RnEnv2 -> TyVar -> TyVar -> (RnEnv2 -> MSG b) -> MSG c
msgTyVarBndr f rn2 a_l a_r k = liftM2 f (msgTyVarBndrExtras rn2 a a_l a_r) $ k rn2'
  where (rn2', a) = rnBndr2' rn2 a_l a_r

msgIdCoVarBndr :: (Id -> b -> c) -> RnEnv2 -> Id -> Id -> (RnEnv2 -> MSG b) -> MSG c
msgIdCoVarBndr f rn2 x_l x_r k = liftM2 f mx $ k rn2'
  where (rn2', mx) = msgIdCoVarBndr' rn2 rn2' x_l x_r

{-
checkMSGLR :: Bool -> Id -> Id -> MSGLR -> MSG MSGLR
checkMSGLR lambdaish x_l x_r lr = case lr of
    VarL _ e_r | x_r `elemVarSet` annedTermFreeVars e_r -> fail "checkMSGLR: deferred term mentioned rigid right variable"
               | lambdaish, not (termIsCheap e_r)       -> fail "checkMSGLR: expensive deferred (right) term escaping lambda"
    VarR e_l _ | x_l `elemVarSet` annedTermFreeVars e_l -> fail "checkMSGLR: deferred term mentioned rigid left variable"
               | lambdaish, not (termIsCheap e_l)       -> fail "checkMSGLR: expensive deferred (left) term escaping lambda"
    _ -> return lr
-}

-- We have to be careful to msg the "fragile" IdInfo for binders as well as the obvious type information
-- (idSpecialisation :: Id -> SpecInfo, realIdUnfolding :: Id -> Unfolding)
msgIdCoVarBndr' :: RnEnv2 -> RnEnv2 {- knot-tied -} -> Id -> Id -> (RnEnv2, MSG Id)
msgIdCoVarBndr' init_rn2 rn2 x_l x_r = (pprTraceSC "msgIdCoVarBndr'" (ppr (x_l, x_r)) init_rn2', msgIdCoVarBndrExtras rn2 x x_l x_r)
  where (init_rn2', x) = rnBndr2' init_rn2 x_l x_r

msgBndrExtras :: RnEnv2 -> Var -> Var -> Var -> MSG Var
msgBndrExtras rn2 v v_l v_r
  | isId v_l,    isId v_r    = msgIdCoVarBndrExtras rn2 v v_l v_r
  | isTyVar v_l, isTyVar v_r = msgTyVarBndrExtras   rn2 v v_l v_r
  | otherwise                = fail "msgBndrExtras"

msgTyVarBndrExtras :: RnEnv2 -> TyVar -> TyVar -> TyVar -> MSG TyVar
msgTyVarBndrExtras rn2 a a_l a_r = liftM (a `setTyVarKind`) $ msgKind rn2 (tyVarKind a_l) (tyVarKind a_r)

msgIdCoVarBndrExtras :: RnEnv2 -> Id -> Id -> Id -> MSG Id
msgIdCoVarBndrExtras rn2 x x_l x_r = liftM3 (\unf spec ty -> x `setVarType` ty `setIdUnfolding` unf `setIdSpecialisation` spec)
                                            (msgUnfolding rn2 (realIdUnfolding x_l) (realIdUnfolding x_r))
                                            (msgSpecInfo rn2 x (idSpecialisation x_l) (idSpecialisation x_r))
                                            (msgType rn2 (idType x_l) (idType x_r))

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
msgCore rn2 (Core.Lam x_l e_l)   (Core.Lam x_r e_r)
  | isId x_l,    isId x_r    = msgIdCoVarBndr Core.Lam rn2 x_l x_r $ \rn2 -> msgCore rn2 e_l e_r
  | isTyVar x_l, isTyVar x_r = msgTyVarBndr   Core.Lam rn2 x_l x_r $ \rn2 -> msgCore rn2 e_l e_r
msgCore rn2 (Core.Let (Core.NonRec x_l e1_l) e2_l) (Core.Let (Core.NonRec x_r e1_r) e2_r)
  | isId x_l, isId x_r = liftM2 (\e1 (x, e2) -> Core.Let (Core.NonRec x e1) e2) (msgCore rn2 e1_l e1_r) $ msgIdCoVarBndr (,) rn2 x_l x_r $ \rn2 -> msgCore rn2 e2_l e2_r
msgCore rn2 (Core.Let (Core.Rec xes_l) e_l) (Core.Let (Core.Rec xes_r) e_r)
  = msgIdCoVarBndrs (\xs (es, e) -> Core.Let (Core.Rec (zipEqual "msgCore: LetRec" xs es)) e) rn2 xs_l xs_r $ \rn2 -> liftM2 (,) (zipWithEqualM (msgCore rn2) es_l es_r) (msgCore rn2 e_l e_r)
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

msgVarBndrs :: ([Var] -> a -> b) -> RnEnv2 -> [Var] -> [Var] -> (RnEnv2 -> MSG a) -> MSG b
msgVarBndrs = msgMany msgVarBndr

msgMany :: (forall b c. (Var -> b -> c) -> RnEnv2 -> v -> v -> (RnEnv2 -> MSG b) -> MSG c)
        -> ([Var] -> b -> c) -> RnEnv2 -> [v] -> [v] -> (RnEnv2 -> MSG b) -> MSG c
msgMany mtch f rn2 xs_l xs_r k = liftM (uncurry f) $ go rn2 xs_l xs_r
  where go rn2 []         []         = liftM ((,) []) $ k rn2
        go rn2 (x_l:xs_l) (x_r:xs_r) = liftM (\(x, (xs, b)) -> (x:xs, b)) $ mtch (,) rn2 x_l x_r $ \rn2 -> go rn2 xs_l xs_r
        go _ _ _ = fail "msgMany"

msgVar :: RnEnv2 -> Out Var -> Out Var -> MSG Var
msgVar rn2 x_l x_r = case (rnOccL_maybe rn2 x_l, rnOccR_maybe rn2 x_r) of
     -- Both rigidly bound: msg iff they rename to the same thing
    (Just x_l', Just x_r') -> pprTraceSC "msgVar_maybe(rigid)" (ppr (x_l, x_r)) $ guard "msgVar: rigid" (x_l' == x_r') >> return x_l'
     -- Both bound by let: defer decision about msging
    (Nothing, Nothing)     -> pprTraceSC "msgVar_maybe(flexi)" (ppr (x_l, x_r)) $ msgFlexiVar x_l x_r
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

msgEC :: RnEnv2 -> RnEnv2 {- knot-tied -} -> Stack -> Stack -> [(RnEnv2, MSG (Stack, Stack, Stack))]
msgEC init_rn2 rn2 = go init_rn2
  where
    go rn2' k_l k_r
      = (case () of
           _ | Car kf_l k_l <- k_l
             , Car kf_r k_r <- k_r
             , Just (rn2'', mkf') <- msgECFrame rn2' rn2 kf_l kf_r
             -> map (second (liftM2 (\kf (k_l, k, k_r) -> (k_l, kf `Car` k, k_r)) mkf')) $ go rn2'' k_l k_r
           _ -> []) ++
        [(rn2', return (k_l, Loco False, k_r))]

msgECFrame :: RnEnv2 -> RnEnv2 {- knot-tied -} -> Tagged StackFrame -> Tagged StackFrame -> Maybe (RnEnv2, MSG (Tagged StackFrame))
msgECFrame init_rn2 rn2 kf_l kf_r = liftM (second (liftM $ Tagged (tag kf_r))) $ go (tagee kf_l) (tagee kf_r) -- Right biased
  where
    go :: StackFrame -> StackFrame -> Maybe (RnEnv2, MSG StackFrame)
    go (Apply x_l')                          (Apply x_r')                          = Just (init_rn2, liftM Apply $ msgVar rn2 x_l' x_r')
    go (TyApply ty_l')                       (TyApply ty_r')                       = Just (init_rn2, liftM TyApply $ msgType rn2 ty_l' ty_r')
    go (Scrutinise x_l' ty_l' in_alts_l)     (Scrutinise x_r' ty_r' in_alts_r)     = Just (init_rn2, liftM2 (\ty (x, in_alts) -> Scrutinise x ty in_alts) (msgType rn2 ty_l' ty_r') (msgIdCoVarBndr (,) rn2 x_l' x_r' $ \rn2 -> msgIn renameAnnedAlts annedAltsFreeVars msgAlts rn2 in_alts_l in_alts_r))
    go (PrimApply pop_l tys_l' as_l in_es_l) (PrimApply pop_r tys_r' as_r in_es_r) = Just (init_rn2, guard "msgECFrame: primop" (pop_l == pop_r) >> liftM3 (PrimApply pop_r) (zipWithEqualM (msgType rn2) tys_l' tys_r') (zipWithEqualM (msgAnned (msgAnswer rn2)) as_l as_r) (zipWithEqualM (msgIn renameAnnedTerm annedTermFreeVars msgTerm rn2) in_es_l in_es_r))
    go (StrictLet x_l' in_e_l)               (StrictLet x_r' in_e_r)               = Just (init_rn2, msgIdCoVarBndr StrictLet rn2 x_l' x_r' $ \rn2 -> msgIn renameAnnedTerm annedTermFreeVars msgTerm rn2 in_e_l in_e_r)
    go (CastIt co_l')                        (CastIt co_r')                        = Just (init_rn2, liftM CastIt $ msgCoercion rn2 co_l' co_r')
    go (Update x_l')                         (Update x_r')                         = Just (second (liftM Update) $ msgIdCoVarBndr' init_rn2 rn2 x_l' x_r')
    go _ _ = Nothing

-- NB: we must enforce invariant that stuff "outside" cannot refer to stuff bound "inside" (heap *and* stack)
msgPureHeap :: {- MSGMode -> -} RnEnv2 -> MSGState -> PureHeap -> PureHeap -> (BoundVars, FreeVars) -> (BoundVars, FreeVars) -> [(M.Map Var Var, (PureHeap, Heap, PureHeap), M.Map Var Var)]
msgPureHeap {- mm -} rn2 msg_s init_h_l init_h_r (k_bvs_l, k_fvs_l) (k_bvs_r, k_fvs_r)
  = [ solution
    | Just (used_l, h_l) <- [sucks init_h_l k_bvs_l M.empty S.empty k_fvs_l]
    , Just (used_r, h_r) <- [sucks init_h_r k_bvs_r M.empty S.empty k_fvs_r]
    , solution <- go M.empty M.empty used_l used_r h_l h_r M.empty msg_s]
  where
    go :: M.Map Var Var        -- Renaming that should be applied to common stuff when instantiated on the left
       -> M.Map Var Var        -- Renaming that should be applied to common stuff when instantiated on the right
       -> S.Set Var            -- Binding in left heap already copied into one of the heaps
       -> S.Set Var            -- Binding in left heap already copied into one of the heaps
       -> PureHeap             -- Left heap
       -> PureHeap             -- Right heap
       -> PureHeap             -- Common heap
       -> MSGState             -- Pending matches etc. MSGState is also used to ensure we never match a var pair more than once (they get the same common name)
       -> [(M.Map Var Var, (PureHeap, Heap, PureHeap), M.Map Var Var)]
    go rn_l rn_r _      _      h_l h_r h       (MSGState { msgPending = [], msgInScopeSet = ids }) = [(rn_l, (h_l, Heap h ids, h_r), rn_r)]
    go rn_l rn_r used_l used_r h_l h_r h msg_s@(MSGState { msgPending = ((x_common, (x_l, x_r)):rest) })
      -- Just like an internal binder, we have to be sure to match the binders themselves (for e.g. type variables)
      | Right (msg_s, x_common) <- flip unMSG (msg_s { msgPending = rest }) (msgBndrExtras rn2 x_common x_l x_r)
      = (case () of
          _ | Just (used_l', hb_l) <- mb_common_l
            , Just (used_r', hb_r) <- mb_common_r
            , Just (msg_s, hb) <- case (inject hb_l, inject hb_r) of
                                    (Just (Just (let_bound_l, in_e_l)), Just (Just (let_bound_r, in_e_r)))
                                      | let_bound_l == let_bound_r
                                      , Right (msg_s, in_e) <- flip unMSG msg_s $ msgIn renameAnnedTerm annedTermFreeVars msgTerm rn2 in_e_l in_e_r
                                      -> Just (msg_s, (if let_bound_r then letBound else internallyBound) in_e)
                                    (Just Nothing, Just Nothing)
                                      | x_l == x_r
                                      -> Just (msg_s, hb_r) -- Right biased
                                    (Nothing, Nothing)
                                      -> Just (msg_s, lambdaBound)
                                    _ -> Nothing
            -- If they match, we need to make a common heap binding
            -> go rn_l rn_r used_l' used_r'
                  h_l h_r (M.insert x_common hb h) msg_s
          _ -> []) ++
        -- If they don't match, we need to generalise
        [ solution
        -- Whenever we add a new "outside" binding like this we have to transitively copy in all the things
        -- that binding refers to. If that is not possible, we have to fail.
        | Just (used_l', h_l') <- [mb_individual_l >>= suck init_h_l k_bvs_l h_l x_l]
        , Just (used_r', h_r') <- [mb_individual_r >>= suck init_h_r k_bvs_r h_r x_r]
        , solution <- go (M.insert x_common x_l rn_l) (M.insert x_common x_r rn_r) used_l' used_r'
                         h_l' h_r' (M.insert x_common lambdaBound h) msg_s
        ]
      | otherwise
      = []
      where
        (mb_common_l, mb_individual_l) = find init_h_l k_bvs_l h_l used_l x_l
        (mb_common_r, mb_individual_r) = find init_h_r k_bvs_r h_r used_r x_r

    find :: PureHeap -> BoundVars -> PureHeap -> S.Set Var
         -> Var -> (Maybe (S.Set Var, HeapBinding),
                      -- Nothing ==> unavailable for common heap
                      -- Just    ==> available for common heap with this usage
                    Maybe (S.Set Var, Maybe HeapBinding))
                      -- Nothing      ==> unavailable for individual heap
                      -- Just Nothing ==> available for individual heap but already in it
                      -- Just Just    ==> available for individual heap and not in it yet
    find init_h k_bvs h used x = case M.lookup x init_h of
         -- Variable bound by the heap (vastly common case):
        Just hb | S.notMember x used -> (Just (used', hb), if x `M.member` h then Just (used, Nothing) else Just (used', Just hb))
                | otherwise          -> (Nothing,          if x `M.member` h then Just (used, Nothing) else Nothing)
          where used' | Just (_, e) <- heapBindingTerm hb
                      , not (termIsCheap e)
                      = S.insert x used
                      | otherwise
                      = used
         -- By a process of elimination, variable must be bound be the stack. Normally it will in fact be bound by the "instance" portion
         -- of the stack because matches involving the common portion variables either already failed or were discharged by RnEnv2, but
         -- if "find" is called by "sucks" then this may not necessarily be the case
        Nothing -> (Nothing, if x `elemVarSet` k_bvs then Just (used, Nothing) else Nothing)

    suck :: PureHeap -> BoundVars -> PureHeap -> Var -> (S.Set Var, Maybe HeapBinding) -> Maybe (S.Set Var, PureHeap)
    suck _      _     h _ (used, Nothing) = Just (used, h)                                                                                     -- Already copied in
    suck init_h k_bvs h x (used, Just hb) = sucks init_h k_bvs (M.insert x hb h) used (heapBindingFreeVars hb `unionVarSet` varBndrFreeVars x) -- Not yet copied: put in and consider FVs

    sucks :: PureHeap -> BoundVars -> PureHeap -> S.Set Var -> FreeVars -> Maybe (S.Set Var, PureHeap)
    sucks init_h k_bvs h used fvs = foldM (\(used, h) x -> snd (find init_h k_bvs h used x) >>= suck init_h k_bvs h x) (used, h) (varSetElems fvs)

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
