{-# OPTIONS_GHC -Woverlapping-patterns -Wincomplete-patterns #-}

module T11195 where

import GHC.Core.TyCo.Rep
import GHC.Core.Coercion
import GHC.Core.Type hiding( substTyVarBndr, substTy, extendTCvSubst )
import GHC.Core.TyCo.Compare
import GHC.Core.InstEnv
import GHC.Core.Coercion.Axiom
import GHC.Tc.Utils.TcType       ( exactTyCoVarsOfType )
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Data.Pair

type NormalCo    = Coercion
type NormalNonIdCo = NormalCo  -- Extra invariant: not the identity
type SymFlag = Bool
type ReprFlag = Bool

chooseRole :: ReprFlag -> Role -> Role
chooseRole = undefined

wrapRole :: ReprFlag -> Role -> Coercion -> Coercion
wrapRole = undefined

wrapSym :: SymFlag -> Coercion -> Coercion
wrapSym = undefined

optForAllCoBndr :: LiftingContext -> Bool
                -> TyVar -> Coercion -> (LiftingContext, TyVar, Coercion)
optForAllCoBndr = undefined

opt_trans :: InScopeSet -> NormalCo -> NormalCo -> NormalCo
opt_trans = undefined

opt_univ :: LiftingContext -> SymFlag -> UnivCoProvenance -> Role
         -> Type -> Type -> Coercion
opt_univ = undefined

opt_co3 :: LiftingContext -> SymFlag -> Maybe Role
        -> Role -> Coercion -> NormalCo
opt_co3 = undefined

opt_co2 :: LiftingContext -> SymFlag -> Role -> Coercion -> NormalCo
opt_co2 = undefined

compatible_co :: Coercion -> Coercion -> Bool
compatible_co = undefined

etaTyConAppCo_maybe = undefined
etaAppCo_maybe = undefined
etaForAllCo_maybe = undefined

matchAxiom = undefined
checkAxInstCo = undefined

isAxiomCo_maybe :: Coercion -> Maybe (SymFlag, CoAxiomRule, [Coercion])
isAxiomCo_maybe co1 = undefined

isCohLeft_maybe = undefined
isCohRight_maybe = undefined
matchNewtypeBranch = undefined

opt_transList :: InScopeSet -> [NormalCo] -> [NormalCo] -> [NormalCo]
opt_transList is = zipWith (opt_trans is)

opt_trans_rule :: InScopeSet -> NormalNonIdCo -> NormalNonIdCo -> Maybe NormalCo
opt_trans_rule is in_co1@(SelCo sel1 co1) in_co2@(SelCo sel2 co2)
  | sel1 == sel2
  , co1 `compatible_co` co2 = undefined

opt_trans_rule is in_co1@(LRCo d1 co1) in_co2@(LRCo d2 co2)
  | d1 == d2
  , co1 `compatible_co` co2 = undefined

-- Push transitivity inside instantiation
opt_trans_rule is in_co1@(InstCo co1 ty1) in_co2@(InstCo co2 ty2)
  | ty1 `eqCoercion` ty2
  , co1 `compatible_co` co2 = undefined

opt_trans_rule is (UnivCo { uco_prov = p1 })
                  (UnivCo { uco_prov = p2 })
  | p1 == p2 = undefined
    -- if the provenances are different, opt'ing will be very confusing

-- Push transitivity down through matching top-level constructors.
opt_trans_rule is in_co1@(TyConAppCo r1 tc1 cos1)
                  in_co2@(TyConAppCo r2 tc2 cos2)
  | tc1 == tc2 = undefined

opt_trans_rule is in_co1@(AppCo co1a co1b) in_co2@(AppCo co2a co2b)
  = undefined

-- Eta rules
opt_trans_rule is co1@(TyConAppCo r tc cos1) co2
  | Just cos2 <- etaTyConAppCo_maybe tc co2 = undefined

opt_trans_rule is co1 co2@(TyConAppCo r tc cos2)
  | Just cos1 <- etaTyConAppCo_maybe tc co1 = undefined

opt_trans_rule is co1@(AppCo co1a co1b) co2
  | Just (co2a,co2b) <- etaAppCo_maybe co2 = undefined

opt_trans_rule is co1 co2@(AppCo co2a co2b)
  | Just (co1a,co1b) <- etaAppCo_maybe co1 = undefined

-- Push transitivity inside forall
opt_trans_rule is co1 co2
  | ForAllCo tv1 vl1 vr1 eta1 r1 <- co1
  , Just (tv2,vl2,vr2,eta2,r2) <- etaForAllCo_maybe co2 = undefined

  | ForAllCo tv2 vl2 vr1 eta2 r2 <- co2
  , Just (tv1,vl1,vr2,eta1,r1) <- etaForAllCo_maybe co1 = undefined

-- Push transitivity inside axioms
opt_trans_rule is co1 co2
  | Just (sym1, axr1, cos1) <- isAxiomCo_maybe co1
  , Just (sym2, axr2, cos2) <- isAxiomCo_maybe co2
  , axr1 == axr2
  , sym1 == not sym2
  , Just (tc, role, branch) <- coAxiomRuleBranch_maybe axr1
  , let qtvs   = coAxBranchTyVars branch ++ coAxBranchCoVars branch
        lhs    = mkTyConApp tc (coAxBranchLHS branch)
        rhs    = coAxBranchRHS branch
        pivot_tvs = exactTyCoVarsOfType (if sym2 then rhs else lhs)
  , all (`elemVarSet` pivot_tvs) qtvs
  = Just $ if sym2
       -- TrPushAxSym
    then liftCoSubstWith role qtvs (opt_transList is cos1 (map mkSymCo cos2)) lhs
       -- TrPushSymAx
    else liftCoSubstWith role qtvs (opt_transList is (map mkSymCo cos1) cos2) rhs

  -- See Note [Push transitivity inside axioms] and
  -- Note [Push transitivity inside newtype axioms only]
  -- TrPushSymAxR
  | Just (sym, axr, cos1) <- isAxiomCo_maybe co1
  , True <- sym
  , Just cos2 <- matchNewtypeBranch sym axr co2
  , let newAxInst = AxiomCo axr (opt_transList is (map mkSymCo cos2) cos1)
  = Just (SymCo newAxInst)

  -- TrPushAxR
  | Just (sym, axr, cos1) <- isAxiomCo_maybe co1
  , False <- sym
  , Just cos2 <- matchNewtypeBranch sym axr co2
  , let newAxInst = AxiomCo axr (opt_transList is cos1 cos2)
  = Just newAxInst

  -- TrPushSymAxL
  | Just (sym, axr, cos2) <- isAxiomCo_maybe co2
  , True <- sym
  , Just cos1 <- matchNewtypeBranch (not sym) axr co1
  , let newAxInst :: Coercion
        newAxInst = AxiomCo axr (opt_transList is cos2 (map mkSymCo cos1))
  = Just (SymCo newAxInst)

  -- TrPushAxL
  | Just (sym, axr, cos2) <- isAxiomCo_maybe co2
  , False <- sym
  , Just cos1 <- matchNewtypeBranch (not sym) axr co1
  , let newAxInst = AxiomCo axr (opt_transList is cos1 cos2)
  = Just newAxInst

opt_trans_rule is co1 co2
  | Just (lco, lh) <- isCohRight_maybe co1
  , Just (rco, rh) <- isCohLeft_maybe co2
  , (coercionType lh) `eqType` (coercionType rh)
  = undefined

opt_trans_rule _ co1 co2        -- Identity rule
  | (Pair ty1 _, r) <- coercionKindRole co1
  , Pair _ ty2 <- coercionKind co2
  , ty1 `eqType` ty2
  = undefined

opt_trans_rule _ _ _ = Nothing
