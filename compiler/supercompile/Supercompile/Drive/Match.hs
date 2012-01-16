module Supercompile.Drive.Match (
    match,
    Match, unMatch, match'
  ) where

#include "HsVersions.h"

import Supercompile.Core.Renaming
import Supercompile.Core.Syntax

import Supercompile.Evaluator.FreeVars
import Supercompile.Evaluator.Syntax

import Supercompile.StaticFlags
import Supercompile.Utilities hiding (guard)

import qualified CoreSyn as Core

import Coercion
import Var        (TyVar, isTyVar, isId, tyVarKind)
import Id         (Id, idType, realIdUnfolding, idSpecialisation)
import IdInfo     (SpecInfo(..), emptySpecInfo)
import VarEnv
import TypeRep    (Kind, Type(..))
import MonadUtils (mapMaybeM)

import Control.Monad.Fix

import Data.Function (on)
import qualified Data.Map as M


pprTraceSC :: String -> SDoc -> a -> a
--pprTraceSC _ _ = id
--pprTraceSC = pprTrace
pprTraceSC msg doc a = traceSC (msg ++ ": " ++ showSDoc doc) a

traceSC :: String -> a -> a
traceSC _ = id
--traceSC = trace


-- Note [Instance matching]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Instance matching is very, very dangerous from a correctness standpoint. If we start with:
--
--  let x = v[x] in x
--
-- Then reducing and splitting causes us to drive:
--
--  let x = v[x] in v
--
-- Note that the new term matches against the previous one if we can do instance matching, so
-- if we are not careful we immediately build a loop. This really sucks.


newtype Match a = Match { unMatch :: Either String a }
--newtype Match a = Match { unMatch :: Maybe a }

instance Functor Match where
    fmap = liftM

instance Monad Match where
    return = Match . return
    mx >>= fxmy = Match $ unMatch mx >>= (unMatch . fxmy)
    fail s = Match $ Left s
    --fail s = Match $ fail s

instance MonadFix Match where
    mfix xmy = Match (mfix (unMatch . xmy))

guard :: String -> Bool -> Match ()
guard _   True  = return ()
guard msg False = fail msg

runMatch :: Match a -> Maybe a
runMatch (Match (Right x))   = Just x
runMatch (Match (Left _msg)) = {- trace ("match " ++ msg) -} Nothing
--runMatch = unMatch

matchRnEnv2 :: (a -> FreeVars) -> a -> a -> RnEnv2
matchRnEnv2 f x y = mkRnEnv2 (mkInScopeSet (f x `unionVarSet` f y))

-- instance MonadPlus Match where
--     mzero = fail "mzero"
--     mx1 `mplus` mx2 = Match $ unMatch mx1 `mplus` unMatch mx2


type MatchResult = M.Map Var Var

-- INVARIANTs:
--   1. All the Vars are *not* rigidly bound (i.e. they are bound by a "let")
--   2. The terms *may* contain free variables of any kind, rigidly bound or not
--   3. The Vars in VarL/VarR are always CoVars/Ids, NEVER TyVars (might change this in the future)
data MatchLR = VarL Var (Out (TermF Anned))
             | VarR (Out (TermF Anned)) Var
             | VarLR Var Var

-- Need exact equality for matchPureHeap loop
instance Eq MatchLR where
    VarL x1 e1      == VarL x2 e2      = x1 == x2            && e1 `eqAnnedTerm` e2
    VarR e1 x1      == VarR e2 x2      = e1 `eqAnnedTerm` e2 && x1 == x2
    VarLR x_l1 x_r1 == VarLR x_l2 x_r2 = x_l1 == x_l2        && x_r1 == x_r2
    _ == _ = False

eqAnnedTerm :: TermF Anned -> TermF Anned -> Bool
eqAnnedTerm e1 e2  = case runMatch (matchTerm' (matchRnEnv2 annedTermFreeVars' e1 e2) e1 e2) of
    Nothing  -> False
    Just lrs -> all (\lr -> case lr of VarLR x_l x_r | x_l == x_r -> True; _ -> False) lrs

instance Outputable MatchLR where
    pprPrec _ (VarL x e')   = ppr x  <+> text "<->" <+> ppr e'
    pprPrec _ (VarR e' x)   = ppr e' <+> text "<->" <+> ppr x
    pprPrec _ (VarLR x1 x2) = ppr x1 <+> text "<->" <+> ppr x2

match :: State -- ^ Tieback semantics
      -> State -- ^ This semantics
      -> Maybe MatchResult -- ^ Renaming from left to right
match s_l s_r = runMatch (match' s_l s_r)

match' :: State
      -> State
      -> Match MatchResult
match' (_deeds_l, Heap h_l ids_l, k_l, qa_l) (_deeds_r, Heap h_r ids_r, k_r, qa_r) = -- (\res -> traceRender ("match", M.keysSet h_l, residualiseDriveState (Heap h_l prettyIdSupply, k_l, in_e_l), M.keysSet h_r, residualiseDriveState (Heap h_r prettyIdSupply, k_r, in_e_r), res) res) $
  do
    -- It's very important that we don't just use the state free variables from both sides to construct the initial in scope set,
    -- because we use it to match the stack and QA on each side *without* first extending it with variables bound by the PureHeap!
    --
    -- The InScopeSets from the Heap are guaranteed to take these into account (along with the stuff bound by the stack, but that
    -- doesn't matter too much) so we just use those instead.
    --
    -- This was the source of a very confusing bug :-(
    let init_rn2 = mkRnEnv2 (ids_l `unionInScope` ids_r)
    (rn2, mfree_eqs2) <- mfix $ \(~(rn2, _)) -> matchEC init_rn2 rn2 k_l k_r
    free_eqs1 <- pprTraceSC "match0" (rn2 `seq` empty) $ matchAnned (matchQA rn2) qa_l qa_r
    free_eqs2 <- pprTraceSC "match1" empty $ mfree_eqs2
    pprTraceSC "match2" (ppr free_eqs1) $ matchPureHeap rn2 (free_eqs1 ++ free_eqs2) h_l h_r >>= safeMkMatchResult

matchAnned :: (a -> a -> b)
           -> Anned a -> Anned a -> b
matchAnned f = f `on` annee

matchQA :: RnEnv2 -> QA -> QA -> Match [MatchLR]
matchQA rn2 (Question x_l') (Question x_r') = matchVar rn2 x_l' x_r'
matchQA rn2 (Question x_l') (Answer in_v_r) = matchVarL rn2 x_l' (answerToAnnedTerm' (rnInScopeSet rn2) in_v_r) -- NB: these rely on the RnEnv2
matchQA rn2 (Answer in_v_l) (Question x_r') = matchVarR rn2 (answerToAnnedTerm' (rnInScopeSet rn2) in_v_l) x_r' -- InScopeSet just like matchIn does
matchQA rn2 (Answer in_v_l) (Answer in_v_r) = matchAnswer rn2 in_v_l in_v_r

matchAnswer :: RnEnv2 -> Answer -> Answer -> Match [MatchLR]
matchAnswer rn2 = matchCoerced (matchIn renameAnnedValue' matchValue) rn2

matchCoerced :: (RnEnv2 -> a -> a -> Match [MatchLR])
             -> RnEnv2 -> Coerced a -> Coerced a -> Match [MatchLR]
matchCoerced f rn2 (mb_co_l, x_l) (mb_co_r, x_r) = liftM2 (++) (matchMaybe (\co_l co_r -> matchCoercion rn2 co_l co_r) (castByCo mb_co_l) (castByCo mb_co_r)) (f rn2 x_l x_r) -- TODO: should match (Just Id) against Nothing

matchKind :: Kind -> Kind -> Match [MatchLR]
matchKind k_l k_r = guard "matchKind" (k_l `isSubKind` k_r && k_r `isSubKind` k_l) >> return []

-- TODO: match type instantiation?
matchType :: RnEnv2 -> Type -> Type -> Match [MatchLR]
matchType rn2 (TyVarTy x_l)         (TyVarTy x_r)         = matchVar rn2 x_l x_r
matchType rn2 (AppTy ty1_l ty2_l)   (AppTy ty1_r ty2_r)   = liftM2 (++) (matchType rn2 ty1_l ty1_r) (matchType rn2 ty2_l ty2_r)
matchType rn2 (TyConApp tc_l tys_l) (TyConApp tc_r tys_r) = guard "matchType: TyConApp" (tc_l == tc_r) >> matchList (matchType rn2) tys_l tys_r
matchType rn2 (FunTy ty1_l ty2_l)   (FunTy ty1_r ty2_r)   = liftM2 (++) (matchType rn2 ty1_l ty1_r) (matchType rn2 ty2_l ty2_r)
matchType rn2 (ForAllTy a_l ty_l)   (ForAllTy a_r ty_r)   = matchTyVarBndr rn2 a_l a_r $ \rn2 -> matchType rn2 ty_l ty_r
matchType _ _ _ = fail "matchType"

-- TODO: match coercion instantiation?
matchCoercion :: RnEnv2 -> Coercion -> Coercion -> Match [MatchLR]
matchCoercion rn2 (Refl ty_l)              (Refl ty_r)              = matchType rn2 (ty_l) (ty_r)
matchCoercion rn2 (TyConAppCo tc_l cos_l)  (TyConAppCo tc_r cos_r)  = guard "matchCoercion: TyConAppCo" (tc_l == tc_r) >> matchList (matchCoercion rn2) (cos_l) (cos_r)
matchCoercion rn2 (AppCo co1_l co2_l)      (AppCo co1_r co2_r)      = liftM2 (++) (matchCoercion rn2 (co1_l) (co1_r)) (matchCoercion rn2 (co2_l) (co2_r))
matchCoercion rn2 (ForAllCo a_l co_l)      (ForAllCo a_r co_r)      = matchTyVarBndr rn2 a_l a_r $ \rn2 -> matchCoercion rn2 co_l co_r
matchCoercion rn2 (CoVarCo a_l)            (CoVarCo a_r)            = matchVar rn2 a_l a_r
matchCoercion rn2 (AxiomInstCo ax_l cos_l) (AxiomInstCo ax_r cos_r) = guard "matchCoercion: AxiomInstCo" (ax_l == ax_r) >> matchList (matchCoercion rn2) (cos_l) (cos_r)
matchCoercion rn2 (UnsafeCo ty1_l ty2_l)   (UnsafeCo ty1_r ty2_r)   = liftM2 (++) (matchType rn2 (ty1_l) (ty1_r)) (matchType rn2 (ty2_l) (ty2_r))
matchCoercion rn2 (SymCo co_l)             (SymCo co_r)             = matchCoercion rn2 (co_l) (co_r)
matchCoercion rn2 (TransCo co1_l co2_l)    (TransCo co1_r co2_r)    = liftM2 (++) (matchCoercion rn2 (co1_l) (co1_r)) (matchCoercion rn2 (co2_l) (co2_r))
matchCoercion rn2 (NthCo i_l co_l)         (NthCo i_r co_r)         = guard "matchCoercion: NthCo" (i_l == i_r) >> matchCoercion rn2 (co_l) (co_r)
matchCoercion rn2 (InstCo co_l ty_l)       (InstCo co_r ty_r)       = liftM2 (++) (matchCoercion rn2 (co_l) (co_r)) (matchType rn2 (ty_l) (ty_r))
matchCoercion _ _ _ = fail "matchCoercion"

matchTerm :: RnEnv2 -> AnnedTerm -> AnnedTerm -> Match [MatchLR]
matchTerm rn2 = matchAnned (matchTerm' rn2)

-- TODO: allow lets on only one side? Useful for matching e.g. (let x = 2 in y + x) with (z + 2)
matchTerm' :: RnEnv2 -> TermF Anned -> TermF Anned -> Match [MatchLR]
matchTerm' rn2 (Var x_l)                  (Var x_r)                  = matchVar rn2 x_l x_r
matchTerm' rn2 (Var x_l)                  e_r                        = matchVarL rn2 x_l e_r
matchTerm' rn2 e_l                        (Var x_r)                  = matchVarR rn2 e_l x_r
matchTerm' rn2 (Value v_l)                (Value v_r)                = matchValue rn2 v_l v_r
matchTerm' rn2 (TyApp e_l ty_l)           (TyApp e_r ty_r)           = liftM2 (++) (matchTerm rn2 e_l e_r) (matchType rn2 ty_l ty_r)
matchTerm' rn2 (App e_l x_l)              (App e_r x_r)              = liftM2 (++) (matchTerm rn2 e_l e_r) (matchVar   rn2 x_l x_r)
matchTerm' rn2 (PrimOp pop_l tys_l es_l)  (PrimOp pop_r tys_r es_r)  = guard "matchTerm: primop" (pop_l == pop_r) >> liftM2 (++) (matchList (matchType rn2) tys_l tys_r) (matchList (matchTerm rn2) es_l es_r)
matchTerm' rn2 (Case e_l x_l ty_l alts_l) (Case e_r x_r ty_r alts_r) = liftM3 app3 (matchTerm rn2 e_l e_r) (matchType rn2 ty_l ty_r) (matchIdCoVarBndr rn2 x_l x_r $ \rn2 -> matchAlts rn2 alts_l alts_r)
matchTerm' rn2 (Let x_l e1_l e2_l)        (Let x_r e1_r e2_r)        = liftM2 (++) (matchTerm rn2 e1_l e1_r) $ matchIdCoVarBndr rn2 x_l x_r $ \rn2 -> matchTerm rn2 e2_l e2_r
matchTerm' rn2 (LetRec xes_l e_l)         (LetRec xes_r e_r)         = matchIdCoVarBndrs rn2 xs_l xs_r $ \rn2 -> liftM2 (++) (matchList (matchTerm rn2) es_l es_r) (matchTerm rn2 e_l e_r)
  where (xs_l, es_l) = unzip xes_l
        (xs_r, es_r) = unzip xes_r
matchTerm' rn2 (Cast e_l co_l)            (Cast e_r co_r)            = liftM2 (++) (matchTerm rn2 (e_l) (e_r)) (matchCoercion rn2 (co_l) (co_r))
matchTerm' _ _ _ = fail "matchTerm"

matchValue :: RnEnv2 -> AnnedValue -> AnnedValue -> Match [MatchLR]
matchValue rn2 (Indirect x_l)               (Indirect x_r)               = matchVar rn2 x_l x_r
matchValue rn2 (Indirect x_l)               v_r                          = matchVarL rn2 x_l (Value v_r)
matchValue rn2 v_l                          (Indirect x_r)               = matchVarR rn2 (Value v_l) x_r
matchValue rn2 (TyLambda a_l e_l)           (TyLambda a_r e_r)           = matchTyVarBndr rn2 a_l a_r $ \rn2 -> matchTerm rn2 e_l e_r
matchValue rn2 (Lambda x_l e_l)             (Lambda x_r e_r)             = matchIdCoVarBndr rn2 x_l x_r $ \rn2 -> matchTerm rn2 e_l e_r
matchValue rn2 (Data dc_l tys_l cos_l xs_l) (Data dc_r tys_r cos_r xs_r) = guard "matchValue: datacon" (dc_l == dc_r) >> liftM3 app3 (matchList (matchType rn2) tys_l tys_r) (matchList (matchCoercion rn2) cos_l cos_r) (matchList (matchVar rn2) xs_l xs_r)
matchValue _   (Literal l_l)                (Literal l_r)                = guard "matchValue: literal" (l_l == l_r) >> return []
matchValue rn2 (Coercion co_l)              (Coercion co_r)              = matchCoercion rn2 (co_l) (co_r)
matchValue _ _ _ = fail "matchValue"

matchAlts :: RnEnv2 -> [AnnedAlt] -> [AnnedAlt] -> Match [MatchLR]
matchAlts rn2 = matchList (matchAlt rn2)

matchAlt :: RnEnv2 -> AnnedAlt -> AnnedAlt -> Match [MatchLR]
matchAlt rn2 (alt_con_l, alt_e_l) (alt_con_r, alt_e_r) = matchAltCon rn2 alt_con_l alt_con_r $ \rn2 -> matchTerm rn2 alt_e_l alt_e_r

matchAltCon :: RnEnv2 -> AltCon -> AltCon -> (RnEnv2 -> Match [MatchLR]) -> Match [MatchLR]
matchAltCon rn2 (DataAlt dc_l as_l qs_l xs_l) (DataAlt dc_r as_r qs_r xs_r) k = guard "matchAltCon: datacon" (dc_l == dc_r) >> (matchTyVarBndrs rn2 as_l as_r $ \rn2 -> matchIdCoVarBndrs rn2 qs_l qs_r $ \rn2 -> matchIdCoVarBndrs rn2 xs_l xs_r k)
matchAltCon rn2 (LiteralAlt l_l)              (LiteralAlt l_r)              k = guard "matchAltCon: literal" (l_l == l_r) >> k rn2
matchAltCon rn2 DefaultAlt                    DefaultAlt                    k = k rn2
matchAltCon _ _ _ _ = fail "matchAltCon"

matchVarBndr :: RnEnv2 -> Var -> Var -> (RnEnv2 -> Match [MatchLR]) -> Match [MatchLR]
matchVarBndr rn2 v_l v_r | isId v_l,    isId v_r    = matchIdCoVarBndr rn2 v_l v_r
                         | isTyVar v_l, isTyVar v_r = matchTyVarBndr   rn2 v_l v_r
                         | otherwise                = fail "matchVarBndr"

matchTyVarBndr :: RnEnv2 -> TyVar -> TyVar -> (RnEnv2 -> Match [MatchLR]) -> Match [MatchLR]
matchTyVarBndr rn2 a_l a_r k = liftM2 (++) (matchKind (tyVarKind a_l) (tyVarKind a_r)) (k (rnBndr2 rn2 a_l a_r))

matchIdCoVarBndr :: RnEnv2 -> Id -> Id -> (RnEnv2 -> Match [MatchLR]) -> Match [MatchLR]
matchIdCoVarBndr rn2 x_l x_r k = liftM2 (++) match_x $ k rn2' >>= if iNSTANCE_MATCHING then mapM (checkMatchLR x_l x_r) else return
  where (rn2', match_x) = matchIdCoVarBndr' rn2 rn2' x_l x_r

checkMatchLR :: Id -> Id -> MatchLR -> Match MatchLR
checkMatchLR x_l x_r lr = case lr of
    VarL _ e_r | x_r `elemVarSet` annedTermFreeVars' e_r -> fail "checkMatchLR: deferred term mentioned rigid right variable"
               | not (termIsCheap' e_r)                  -> fail "checkMatchLR: expensive deferred (right) term escaping lambda"
    VarR e_l _ | x_l `elemVarSet` annedTermFreeVars' e_l -> fail "checkMatchLR: deferred term mentioned rigid left variable"
               | not (termIsCheap' e_l)                  -> fail "checkMatchLR: expensive deferred (left) term escaping lambda"
    _ -> return lr

-- We have to be careful to match the "fragile" IdInfo for binders as well as the obvious type information
-- (idSpecialisation :: Id -> SpecInfo, realIdUnfolding :: Id -> Unfolding)
matchIdCoVarBndr' :: RnEnv2 -> RnEnv2 {- knot-tied -} -> Id -> Id -> (RnEnv2, Match [MatchLR])
matchIdCoVarBndr' init_rn2 rn2 x_l x_r = (pprTraceSC "matchIdCoVarBndr'" (ppr (x_l, x_r)) $ rnBndr2 init_rn2 x_l x_r, matchIdCoVarBndrExtras rn2 x_l x_r)

matchBndrExtras :: RnEnv2 -> Var -> Var -> Match [MatchLR]
matchBndrExtras rn2 v_l v_r
  | isId v_l,    isId v_r    = matchIdCoVarBndrExtras rn2 v_l v_r
  | isTyVar v_l, isTyVar v_r = matchTyVarBndrExtras   rn2 v_l v_r
  | otherwise                = fail "matchBndrExtras"

matchTyVarBndrExtras :: RnEnv2 -> TyVar -> TyVar -> Match [MatchLR]
matchTyVarBndrExtras _ a_l a_r = matchKind (tyVarKind a_l) (tyVarKind a_r)

matchIdCoVarBndrExtras :: RnEnv2 -> Id -> Id -> Match [MatchLR]
matchIdCoVarBndrExtras rn2 x_l x_r = liftM3 app3 (matchUnfolding rn2 (realIdUnfolding x_l) (realIdUnfolding x_r)) (matchSpecInfo rn2 (idSpecialisation x_l) (idSpecialisation x_r)) (matchType rn2 (idType x_l) (idType x_r))

-- TODO: currently insists that the LHS has no unfolding/RULES. (This is not as bad as it seems since unstable unfoldings match). Can we do better?
matchIdCoVarBndrExtrasL :: RnEnv2 -> Id -> TermF Anned -> Match [MatchLR]
matchIdCoVarBndrExtrasL rn2 x_l e_r = liftM3 app3 (matchUnfolding rn2 (realIdUnfolding x_l) Core.noUnfolding) (matchSpecInfo rn2 (idSpecialisation x_l) emptySpecInfo) (matchType rn2 (idType x_l) (termType' e_r))

-- TODO: currently insists that the LHS has no unfolding/RULES. (This is not as bad as it seems since unstable unfoldings match). Can we do better?
matchIdCoVarBndrExtrasR :: RnEnv2 -> TermF Anned -> Id -> Match [MatchLR]
matchIdCoVarBndrExtrasR rn2 e_l x_r = liftM3 app3 (matchUnfolding rn2 Core.noUnfolding (realIdUnfolding x_r)) (matchSpecInfo rn2 emptySpecInfo (idSpecialisation x_r)) (matchType rn2 (termType' e_l) (idType x_r))

matchSpecInfo :: RnEnv2 -> SpecInfo -> SpecInfo -> Match [MatchLR]
matchSpecInfo rn2 (SpecInfo rules_l _) (SpecInfo rules_r _) = matchList (matchRule rn2) rules_l rules_r

matchRule :: RnEnv2 -> Core.CoreRule -> Core.CoreRule -> Match [MatchLR]
matchRule _   (Core.BuiltinRule { Core.ru_name = name1 }) (Core.BuiltinRule { Core.ru_name = name2 }) = guard "matchRule: BuiltinRule" (name1 == name2) >> return [] -- NB: assume builtin rules generate RHSes without any free vars!
matchRule rn2 (Core.Rule { Core.ru_bndrs = vs_l, Core.ru_args = args_l, Core.ru_rhs = rhs_l }) (Core.Rule { Core.ru_bndrs = vs_r, Core.ru_args = args_r, Core.ru_rhs = rhs_r }) = matchMany matchVarBndr rn2 vs_l vs_r $ \rn2 -> liftM2 (++) (matchList (matchCore rn2) args_l args_r) (matchCore rn2 rhs_l rhs_r)
matchRule _ _ _ = fail "matchRule"

matchUnfolding :: RnEnv2 -> Core.Unfolding -> Core.Unfolding -> Match [MatchLR]
matchUnfolding rn2 (Core.CoreUnfolding { Core.uf_tmpl = rhs1, Core.uf_src = src1 }) (Core.CoreUnfolding { Core.uf_tmpl = rhs2, Core.uf_src = src2 })
  | Core.isStableSource src1, Core.isStableSource src2 = matchCore rn2 rhs1 rhs2
matchUnfolding rn2 (Core.DFunUnfolding _ _ args1) (Core.DFunUnfolding _ _ args2) = matchList (matchCore rn2) args1 args2
-- It is OK to match any *unstable* unfolding against any other one
matchUnfolding _ unf1 unf2 | not (Core.isStableUnfolding unf1), not (Core.isStableUnfolding unf2) = return []
matchUnfolding _ _ _ = fail "matchUnfolding"

-- TODO: match instantiation within Core?
matchCore :: RnEnv2 -> Core.CoreExpr -> Core.CoreExpr -> Match [MatchLR]
matchCore rn2 (Core.Var x_l)       (Core.Var x_r)       = matchVar rn2 x_l x_r
matchCore _   (Core.Lit l_l)       (Core.Lit l_r)       = guard "matchCore: Lit" (l_l == l_r) >> return []
matchCore rn2 (Core.App e1_l e2_l) (Core.App e1_r e2_r) = liftM2 (++) (matchCore rn2 e1_l e1_r) (matchCore rn2 e2_l e2_r)
matchCore rn2 (Core.Lam x_l e_l)   (Core.Lam x_r e_r)
  | isId x_l,    isId x_r    = matchIdCoVarBndr rn2 x_l x_r $ \rn2 -> matchCore rn2 e_l e_r
  | isTyVar x_l, isTyVar x_r = matchTyVarBndr   rn2 x_l x_r $ \rn2 -> matchCore rn2 e_l e_r
matchCore rn2 (Core.Let (Core.NonRec x_l e1_l) e2_l) (Core.Let (Core.NonRec x_r e1_r) e2_r)
  | isId x_l, isId x_r = liftM2 (++) (matchCore rn2 e1_l e1_r) $ matchIdCoVarBndr rn2 x_l x_r $ \rn2 -> matchCore rn2 e2_l e2_r
matchCore rn2 (Core.Let (Core.Rec xes_l) e_l) (Core.Let (Core.Rec xes_r) e_r)
  = matchIdCoVarBndrs rn2 xs_l xs_r $ \rn2 -> liftM2 (++) (matchList (matchCore rn2) es_l es_r) (matchCore rn2 e_l e_r)
      where (xs_l, es_l) = unzip xes_l
            (xs_r, es_r) = unzip xes_r
matchCore rn2 (Core.Case e_l x_l ty_l alts_l) (Core.Case e_r x_r ty_r alts_r) = liftM3 app3 (matchCore rn2 e_l e_r) (matchType rn2 ty_l ty_r) (matchIdCoVarBndr rn2 x_l x_r $ \rn2 -> matchCoreAlts rn2 alts_l alts_r)
matchCore rn2 (Core.Cast e_l co_l)            (Core.Cast e_r co_r)            = liftM2 (++) (matchCore rn2 e_l e_r) (matchCoercion rn2 co_l co_r)
matchCore rn2 (Core.Note no_l e_l)            (Core.Note no_r e_r)            = guard "matchCore: Note" (no_l == no_r) >> matchCore rn2 e_l e_r
matchCore rn2 (Core.Type ty_l)                (Core.Type ty_r)                = matchType rn2 ty_l ty_r
matchCore rn2 (Core.Coercion co_l)            (Core.Coercion co_r)            = matchCoercion rn2 co_l co_r
matchCore _ _ _ = fail "matchCore"

matchCoreAlts :: RnEnv2 -> [Core.CoreAlt] -> [Core.CoreAlt] -> Match [MatchLR]
matchCoreAlts rn2 = matchList (matchCoreAlt rn2)

matchCoreAlt :: RnEnv2 -> Core.CoreAlt -> Core.CoreAlt -> Match [MatchLR]
matchCoreAlt rn2 (alt_con_l, vs_l, alt_e_l) (alt_con_r, vs_r, alt_e_r) = guard "matchCoreAlt" (alt_con_l == alt_con_r) >> matchMany matchVarBndr rn2 vs_l vs_r (\rn2 -> matchCore rn2 alt_e_l alt_e_r)

matchTyVarBndrs :: RnEnv2 -> [TyVar] -> [TyVar] -> (RnEnv2 -> Match [MatchLR]) -> Match [MatchLR]
matchTyVarBndrs = matchMany matchTyVarBndr

matchIdCoVarBndrs :: RnEnv2 -> [Id] -> [Id] -> (RnEnv2 -> Match [MatchLR]) -> Match [MatchLR]
matchIdCoVarBndrs = matchMany matchIdCoVarBndr

matchMany :: (RnEnv2 -> v -> v -> (RnEnv2 -> Match b) -> Match b)
          -> RnEnv2 -> [v] -> [v] -> (RnEnv2 -> Match b) -> Match b
matchMany _    rn2 []         []         k = k rn2
matchMany mtch rn2 (x_l:xs_l) (x_r:xs_r) k = mtch rn2 x_l x_r $ \rn2 -> matchMany mtch rn2 xs_l xs_r k
matchMany _ _ _ _ _ = fail "matchMany"

matchVar :: RnEnv2 -> Out Id -> Out Id -> Match [MatchLR]
matchVar rn2 x_l x_r = fmap maybeToList (matchVar_maybe rn2 x_l x_r)

matchVar_maybe :: RnEnv2 -> Out Id -> Out Id -> Match (Maybe MatchLR)
matchVar_maybe rn2 x_l x_r = case (rnOccL_maybe rn2 x_l, rnOccR_maybe rn2 x_r) of
     -- Both rigidly bound: match iff they rename to the same thing
    (Just x_l', Just x_r') -> pprTraceSC "matchVar_maybe(rigid)" (ppr (x_l, x_r)) $ guard "matchVar: rigid" (x_l' == x_r') >> return Nothing
     -- Both bound by let: defer decision about matching
    (Nothing, Nothing)     -> pprTraceSC "matchVar_maybe(flexi)" (ppr (x_l, x_r)) $ return (Just (VarLR x_l x_r))
     -- One bound by let and one bound rigidly: don't match
    _                      -> fail "matchVar: mismatch"

matchVarL :: RnEnv2 -> Out Id -> Out (TermF Anned) -> Match [MatchLR]
matchVarL rn2 x_l e_r = fmap maybeToList (matchVarL_maybe rn2 x_l e_r)

matchVarL_maybe :: RnEnv2 -> Out Id -> Out (TermF Anned) -> Match (Maybe MatchLR)
matchVarL_maybe rn2 x_l e_r = guard "matchVarL_maybe: no instance matching"  iNSTANCE_MATCHING >> case rnOccL_maybe rn2 x_l of
     -- Left rigidly bound: matching is impossible (assume we already tried matchVar_maybe)
    Just _  -> fail "matchVar: rigid"
     -- Both bound by let: defer decision about matching
    Nothing -> return (Just (VarL x_l e_r))

matchVarR :: RnEnv2 -> Out (TermF Anned) -> Out Id -> Match [MatchLR]
matchVarR rn2 e_l x_r = fmap maybeToList (matchVarR_maybe rn2 e_l x_r)

matchVarR_maybe :: RnEnv2 -> Out (TermF Anned) -> Out Id -> Match (Maybe MatchLR)
matchVarR_maybe rn2 e_l x_r = guard "matchVarR_maybe: no instance matching" iNSTANCE_MATCHING >> case rnOccR_maybe rn2 x_r of
     -- Right rigidly bound: matching is impossible (assume we already tried matchVar_maybe)
    Just _  -> fail "matchVar: rigid"
     -- Both bound by let: defer decision about matching
    Nothing -> return (Just (VarR e_l x_r))

matchList :: (a -> a -> Match [MatchLR])
          -> [a] -> [a] -> Match [MatchLR]
matchList mtch xs_l xs_r = fmap concat (zipWithEqualM mtch xs_l xs_r)

matchMaybe :: (a -> a -> Match [MatchLR])
           -> Maybe a -> Maybe a -> Match [MatchLR]
matchMaybe _ Nothing    Nothing    = return []
matchMaybe f (Just x_l) (Just x_r) = f x_l x_r
matchMaybe _ _ _ = fail "matchMaybe"

matchIn :: (InScopeSet -> Renaming -> a -> a)
        -> (RnEnv2 -> a -> a -> Match b)
        -> RnEnv2 -> In a -> In a -> Match b
matchIn rnm mtch rn2 (rn_l, x_l) (rn_r, x_r) = mtch rn2 (rnm iss rn_l x_l) (rnm iss rn_r x_r)
  where iss = rnInScopeSet rn2 -- NB: this line is one of the few things that relies on the RnEnv2 InScopeSet being correct

matchEC :: RnEnv2 -> RnEnv2 {- knot-tied -} -> Stack -> Stack -> Match (RnEnv2, Match [MatchLR])
matchEC init_rn2 rn2 k_l k_r = foldZipEqualM (\(init_rn2', meqs) kf_l kf_r -> fmap (second (liftM2 (++) meqs)) $ matchECFrame init_rn2' rn2 kf_l kf_r) (init_rn2, return []) k_l k_r

matchECFrame :: RnEnv2 -> RnEnv2 {- knot-tied -} -> Tagged StackFrame -> Tagged StackFrame -> Match (RnEnv2, Match [MatchLR])
matchECFrame init_rn2 rn2 kf_l kf_r = go (tagee kf_l) (tagee kf_r)
  where
    go :: StackFrame -> StackFrame -> Match (RnEnv2, Match [MatchLR])
    go (Apply x_l')                          (Apply x_r')                          = return (init_rn2, matchVar rn2 x_l' x_r')
    go (TyApply ty_l')                       (TyApply ty_r')                       = return (init_rn2, matchType rn2 ty_l' ty_r')
    go (Scrutinise x_l' ty_l' in_alts_l)     (Scrutinise x_r' ty_r' in_alts_r)     = return (init_rn2, liftM2 (++) (matchType rn2 ty_l' ty_r') (matchIdCoVarBndr rn2 x_l' x_r' $ \rn2 -> matchIn renameAnnedAlts matchAlts rn2 in_alts_l in_alts_r))
    go (PrimApply pop_l tys_l' as_l in_es_l) (PrimApply pop_r tys_r' as_r in_es_r) = return (init_rn2, guard "matchECFrame: primop" (pop_l == pop_r) >> liftM3 (\x y z -> x ++ y ++ z) (matchList (matchType rn2) tys_l' tys_r') (matchList (matchAnned (matchAnswer rn2)) as_l as_r) (matchList (matchIn renameAnnedTerm matchTerm rn2) in_es_l in_es_r))
    go (StrictLet x_l' in_e_l)               (StrictLet x_r' in_e_r)               = return (init_rn2, matchIdCoVarBndr rn2 x_l' x_r' $ \rn2 -> matchIn renameAnnedTerm matchTerm rn2 in_e_l in_e_r)
    go (CastIt co_l')                        (CastIt co_r')                        = return (init_rn2, matchCoercion rn2 co_l' co_r')
    go (Update x_l')                         (Update x_r')                         = return (matchIdCoVarBndr' init_rn2 rn2 x_l' x_r')
    go _ _ = fail "matchECFrame"

--- Returns a renaming from the list only if the list maps a "left" variable to a unique "right" variable
-- If the left side var was free, we might have assumed two different corresponding rights for it. This is not necessarily a problem:
--      a |-> True; ()<(a, a)> `match` c |-> True; d |-> True; ()<(c, d)>
--      a |-> True; ()<(a, a)> `match` c |-> True; d |-> c; ()<(c, d)>
-- However, I'm going to reject this for now (simpler).
--
-- TODO: arguably I should check that this is actually a true *bijection* not just a *function* because a renaming like
-- {x |-> a, y |-> a} means that if we carrried on supercompiling here we could exploit more equalities (via positive information
-- propagation - imagine we scrutinise x and later scrutinise y) and potentially get better code that at the tieback site. I need to
-- check how important this is in practice.
safeMkMatchResult :: [(Var, Var)] -> Match MatchResult
safeMkMatchResult eqs = guard "safeMkRenaming" (all (\(x_l, x_r) -> M.lookup x_l eqs_map == Just x_r) eqs) >> return eqs_map
  where eqs_map = M.fromList eqs


trimBounds :: BoundVars -> BoundVars -> [MatchLR] -> Match [(Var, Var)]
trimBounds internally_bound_l internally_bound_r eqs
  = flip mapMaybeM eqs $ \lr -> case lr of VarLR x_l x_r -> case (x_l `elemVarSet` internally_bound_l, x_r `elemVarSet` internally_bound_r) of
                                                              (True,  True)  -> return Nothing               -- Both local, don't want it in the output
                                                              (False, False) -> return (Just (x_l, x_r))     -- Both free
                                                              _              -> fail "trimBounds: boundness" -- Mismatch
                                           VarL x_l _ | x_l `elemVarSet` internally_bound_l -> return Nothing
                                           VarR _ x_r | x_r `elemVarSet` internally_bound_r -> return Nothing
                                           _ -> fail "trimBounds: instantiation"

-- FIXME: match binders in heap
matchPureHeap :: RnEnv2 -> [MatchLR] -> PureHeap -> PureHeap -> Match [(Var, Var)]
matchPureHeap rn2 init_free_eqs h_l h_r
  = matchLoop [] init_free_eqs emptyVarSet emptyVarSet >>=
    trimBounds (fst (pureHeapVars h_l) InternallyBound) (fst (pureHeapVars h_r) InternallyBound)
    -- NB: if there are dead bindings in the left PureHeap then the output Renaming will not contain a renaming for their binders.
    --
    -- NB: The resulting equalities must only relate local vars to local vars (in which case we can discard them, because
    -- matchLoop would have ensured that they were fulfilled) or free vars to free vars (in which case we propagate them upward).
    --
    -- NB: We already know there are no eqs that relate update-frame bound (rigid) variables.
  where
    -- NB: must respect work-sharing for non-values
    --  x |-> e1, y |-> e1; (x, y) `match` x |-> e1; (x, x) == Nothing
    --  x |-> e1; (x, x) `match` x |-> e1; y |-> e1; (x, y) == Nothing (though this is more questionable, it seems a consistent choice)
    -- NB: treat equal values as equal regardless of duplication
    --  x |-> v, y |-> v; (x, y) `match` x |-> v; (x, x) /= Nothing
    -- TODO: look through variables on both sides
    --  x |-> e1; (x, x) `match` x |-> e1; y |-> x `match` (x, y) /= Nothing
    --  x |-> e1, y |-> x; (x, y) `match` x |-> e1 `match` (x, x) /= Nothing
    --
    -- It used to be important to allow instantiatation of a dynamic variable with a static *variable*.
    -- This was so because if we didn't tie back to a situation where all that had changed was that one more
    -- variable was static, we would immediately whistle because the tagbags would be the same.
    --
    -- In the new world, we record staticness as phantom heap bindings, so this just doesn't figure in at all.
    -- We can account for staticness using the standard generalisation mechanism, and there is no need for the
    -- matcher to have hacks like that (though we still have to be careful about how we match phantoms).
    
    matchLoop known [] _ _ = return known
    matchLoop known (lr:free_eqs) used_l used_r
       -- Perhaps we have already assumed this equality is true?
       -- NB: it is OK to do exact syntactic equality on VarL/VarR here because we always rename new equalities generated in
       -- this loop using the same InScopeSet (that from rn2) so only a finite number of distinct binders will be generated.
      | lr `elem` known = matchLoop known free_eqs used_l used_r
      | otherwise = pprTraceSC "matchLoop" (ppr lr) $
                    case (case lr of VarLR x_l x_r -> (go_template (matchBndrExtras rn2 x_l x_r)    ,     lookupUsed used_l x_l h_l,                         lookupUsed used_r x_r h_r)
                                     VarL  x_l e_r -> (go_template (matchIdCoVarBndrExtrasL rn2 x_l e_r), lookupUsed used_l x_l h_l,                         Just (InternallyBound, Just (Just (used_r, e_r))))
                                     VarR  e_l x_r -> (go_template (matchIdCoVarBndrExtrasR rn2 e_l x_r), Just (InternallyBound, Just (Just (used_l, e_l))), lookupUsed used_r x_r h_r)) of
           -- If matching an internal let, it is possible that variables occur free. Insist that free-ness matches:
           -- TODO: actually I'm pretty sure that the heap binds *everything* now. These cases could probably be removed,
           -- though they don't do any particular harm.
          (go, Nothing, Nothing) -> go [] used_l used_r
          (_,  Just _,  Nothing) -> failLoop "matching binding on left not present in the right"
          (_,  Nothing, Just _)  -> failLoop "matching binding on right not present in the left"
          (go, Just hb_l, Just hb_r) -> case (hb_l, hb_r) of
               -- If the template provably doesn't use this heap binding, we can match it against anything at all
              ((InternallyBound, Nothing), _) -> go [] used_l used_r
               -- If the template internalises a binding of this form, check that the matchable semantics is the same.
               -- If the matchable doesn't have a corresponding binding tieback is impossible because we have less info this time.
              ((InternallyBound, Just (Just (used_l', e_l'))), (how_r, mb_e_r')) -> case mb_e_r' of
                  Just (Just (used_r', e_r')) -> matchTerm' rn2 e_l' e_r' >>= \extra_free_eqs -> go extra_free_eqs used_l' used_r'
                  Just Nothing                -> failLoop "right side of InternallyBound already used"
                  Nothing                     -> failLoop $ "can only match a termful InternallyBound on left against an actual term, not a termless " ++ show how_r ++ " binding"
               -- If the template has no information but exposes a lambda, we can rename to tie back.
               -- If there is a corresponding binding in the matchable we can't tieback because we have more info this time.
               --
               -- NB: this may cause us to instantiate a lambda-bound var with one that is presently let-bound. The alternative
               -- (almost certainly an sc-stop) is worse, though... Doing this really matters; see for example the Bernouilli benchmark.
               --
               -- TODO: give let-bound nothings tags and generalise to get the same effect?
              ((LambdaBound, Nothing), (_how_r, mb_e_r')) -> case mb_e_r' of
                  Nothing -> (if _how_r == LetBound then pprTraceSC "Downgrading" empty else id) $
                             go [] used_l used_r
                  Just _ -> failLoop "cannot match termless LambdaBound on left against an actual term"
               -- If the template has an unfolding, we must do lookthrough
              ((LambdaBound, Just (Just (used_l', e_l'))), (_how_r, mb_e_r')) -> case mb_e_r' of
                  Just (Just (used_r', e_r')) -> matchTerm' rn2 e_l' e_r' >>= \extra_free_eqs -> go extra_free_eqs used_l' used_r'
                  Just Nothing                -> failLoop "right side of LambdaBound already used"
                  Nothing                     -> failLoop "can only match a termful LambdaBound on left against an actual term"
               -- We assume the supercompiler gives us no-shadowing for let-bound names, so if two names are the same they must refer to the same thing
               -- NB: because I include this case, we may not include a renaming for some lambda-bound variables in the final knowns (if they are bound
               -- above the let-bound thing)
               --
               -- Interestingly, doing this matching here also improves matching in the case where a previous state had a more-or-less
               -- evaluated version of this heap binding in place. We "know" that we can match them since they originated from the same
               -- heap binding, even though evaluation may have changed their shape.
               --
               -- Of course, we still need to match the FVs on both sides. For example, the LHS could be {x |-> Just y} with the RHS
               -- {x |-> Just y, y |-> True} -- better not tie back in this situation, so we validate that the y bindings still match.
               -- This also ensures that the outgoing knowns can be used to build a renaming that includes the RHS of these bindings.
               --
               -- OK, I don't think this is safe in the case where either side is not LetBound. The reason is that we might have:
               --     D[(let x = e1 in x, let x = e2 in x)]
               -- ==> (D[let x = e1 in x], D[let x = e2 in x])
               --
               -- Which floats to:
               --     let h0 = D[let x = e1 in x]
               --     in in (h0, D[let x = e2 in x])
               --
               -- We better not tieback the second tuple component to h0 on the basis that the two x binders match!
               -- They are only guaranteed to match if the are **Let bound**, because in that case those binders must have been
               -- created by a common ancestor and hence we can just match the uniques to determine whether the binders are the "same".
               -- It is NOT safe to do this is both/either sides are LambdaBound, because we have no guarantee of a common ancestor in that case.
              ((LetBound, mb_e_l'), (_how_r, mb_e_r'))
                | VarLR x_l x_r <- lr, x_l == x_r -> case (mb_e_l', mb_e_r') of -- TODO: even match LetBounds against non-VarLR
                  (Nothing,                     Nothing)                     -> go [] used_l used_r
                  (Just (Just (used_l', e_l')), Just (Just (used_r', e_r'))) -> ASSERT2(annedTermFreeVars' e_r' `subVarSet` annedTermFreeVars' e_l', text "match" <+> ppr (x_l, e_l', x_r, _how_r, e_r'))
                                                                                go [VarLR x x | x <- varSetElems (annedTermFreeVars' e_l')] used_l' used_r'
                  _                          -> failLoop "insane LetBounds"
               -- If the template doesn't lambda abstract, we can't rename. Only tieback if we have an exact *name* match.
               --
               -- You might think that we could do better than this if both the LHS and RHS had unfoldings, by matching them.
               -- However, this is very dangerous because we don't want to match the template {x |-> let {Just y}, y |-> lam {}}
               -- against the matchable {x' |-> let {Just y'}, y' |-> lam {}}, since the template may still be able to reach y via the binding
               -- for x (we renamed the lambda-abstracted y to y' so there is nothing to fear from there).
               --
               -- NB: we can treat this *almost* exactly like the LambdaBound+unfolding case now since we have the invariant that LetBound things never
               -- refer to LambdaBound things. *However* we anticipate that doing so would almost always fail to tieback, so we elect to just stick with
               -- the "cheap-but-inaccurate" name-matching heuristic.
                | otherwise -> failLoop "LetBound"
              _ -> failLoop "left side of InternallyBound/LambdaBound already used"
      where -- First Maybe: whether or not the var is bound in the heap
            -- Second Maybe: whether or not the HeapBinding actually has a term
            -- Third Maybe: whether it is safe for work duplication to make use of that term
            lookupUsed :: VarSet -> Var -> PureHeap -> Maybe (HowBound, Maybe (Maybe (VarSet, Out (TermF Anned))))
            lookupUsed used x h = case M.lookup x h of
              Nothing -> Nothing
              Just hb -> Just (howBound hb, flip fmap (heapBindingTerm hb) $ \in_e -> let e' = renameIn (renameAnnedTerm (rnInScopeSet rn2)) in_e
                                                                                      in case () of
                                                                                              () | termIsCheap e'      -> Just (used, annee e')
                                                                                                 | x `elemVarSet` used -> Nothing
                                                                                                 | otherwise           -> Just (used `extendVarSet` x, annee e'))

            failLoop rest = fail $ "matchLoop: " ++ showPpr lr ++ ": " ++ rest
            go_template mextras extra_free_eqs used_l' used_r' = do
                -- Don't forget to match types/unfoldings of binders as well:
                bndr_free_eqs <- mextras
                matchLoop (lr : known) (bndr_free_eqs ++ extra_free_eqs ++ free_eqs) used_l' used_r'

app3 :: [a] -> [a] -> [a] -> [a]
app3 x y z = x ++ y ++ z
