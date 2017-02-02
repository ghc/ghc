-- (c) The University of Glasgow 2006

{-# LANGUAGE CPP #-}

-- The default iteration limit is a bit too low for the definitions
-- in this module.
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fmax-pmcheck-iterations=10000000 #-}
#endif

module OptCoercion ( optCoercion, checkAxInstCo ) where

#include "HsVersions.h"

import DynFlags
import TyCoRep
import Coercion
import Type hiding( substTyVarBndr, substTy )
import TcType       ( exactTyCoVarsOfType )
import TyCon
import CoAxiom
import VarSet
import VarEnv
import Outputable
import FamInstEnv ( flattenTys )
import Pair
import ListSetOps ( getNth )
import Util
import Unify
import InstEnv
import Control.Monad   ( zipWithM )

{-
%************************************************************************
%*                                                                      *
                 Optimising coercions
%*                                                                      *
%************************************************************************

Note [Optimising coercion optimisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Looking up a coercion's role or kind is linear in the size of the
coercion. Thus, doing this repeatedly during the recursive descent
of coercion optimisation is disastrous. We must be careful to avoid
doing this if at all possible.

Because it is generally easy to know a coercion's components' roles
from the role of the outer coercion, we pass down the known role of
the input in the algorithm below. We also keep functions opt_co2
and opt_co3 separate from opt_co4, so that the former two do Phantom
checks that opt_co4 can avoid. This is a big win because Phantom coercions
rarely appear within non-phantom coercions -- only in some TyConAppCos
and some AxiomInstCos. We handle these cases specially by calling
opt_co2.

Note [Optimising InstCo]
~~~~~~~~~~~~~~~~~~~~~~~~
When we have (InstCo (ForAllCo tv h g) g2), we want to optimise.

Let's look at the typing rules.

h : k1 ~ k2
tv:k1 |- g : t1 ~ t2
-----------------------------
ForAllCo tv h g : (all tv:k1.t1) ~ (all tv:k2.t2[tv |-> tv |> sym h])

g1 : (all tv:k1.t1') ~ (all tv:k2.t2')
g2 : s1 ~ s2
--------------------
InstCo g1 g2 : t1'[tv |-> s1] ~ t2'[tv |-> s2]

We thus want some coercion proving this:

  (t1[tv |-> s1]) ~ (t2[tv |-> s2 |> sym h])

If we substitute the *type* tv for the *coercion*
(g2 `mkCoherenceRightCo` sym h) in g, we'll get this result exactly.
This is bizarre,
though, because we're substituting a type variable with a coercion. However,
this operation already exists: it's called *lifting*, and defined in Coercion.
We just need to enhance the lifting operation to be able to deal with
an ambient substitution, which is why a LiftingContext stores a TCvSubst.

-}

optCoercion :: TCvSubst -> Coercion -> NormalCo
-- ^ optCoercion applies a substitution to a coercion,
--   *and* optimises it to reduce its size
optCoercion env co
  | hasNoOptCoercion unsafeGlobalDynFlags = substCo env co
  | debugIsOn
  = let out_co = opt_co1 lc False co
        (Pair in_ty1  in_ty2,  in_role)  = coercionKindRole co
        (Pair out_ty1 out_ty2, out_role) = coercionKindRole out_co
    in
    ASSERT2( substTyUnchecked env in_ty1 `eqType` out_ty1 &&
             substTyUnchecked env in_ty2 `eqType` out_ty2 &&
             in_role == out_role
           , text "optCoercion changed types!"
             $$ hang (text "in_co:") 2 (ppr co)
             $$ hang (text "in_ty1:") 2 (ppr in_ty1)
             $$ hang (text "in_ty2:") 2 (ppr in_ty2)
             $$ hang (text "out_co:") 2 (ppr out_co)
             $$ hang (text "out_ty1:") 2 (ppr out_ty1)
             $$ hang (text "out_ty2:") 2 (ppr out_ty2)
             $$ hang (text "subst:") 2 (ppr env) )
    out_co

  | otherwise         = opt_co1 lc False co
  where
    lc = mkSubstLiftingContext env

type NormalCo    = Coercion
  -- Invariants:
  --  * The substitution has been fully applied
  --  * For trans coercions (co1 `trans` co2)
  --       co1 is not a trans, and neither co1 nor co2 is identity

type NormalNonIdCo = NormalCo  -- Extra invariant: not the identity

-- | Do we apply a @sym@ to the result?
type SymFlag = Bool

-- | Do we force the result to be representational?
type ReprFlag = Bool

-- | Optimize a coercion, making no assumptions. All coercions in
-- the lifting context are already optimized (and sym'd if nec'y)
opt_co1 :: LiftingContext
        -> SymFlag
        -> Coercion -> NormalCo
opt_co1 env sym co = opt_co2 env sym (coercionRole co) co

-- See Note [Optimising coercion optimisation]
-- | Optimize a coercion, knowing the coercion's role. No other assumptions.
opt_co2 :: LiftingContext
        -> SymFlag
        -> Role   -- ^ The role of the input coercion
        -> Coercion -> NormalCo
opt_co2 env sym Phantom co = opt_phantom env sym co
opt_co2 env sym r       co = opt_co3 env sym Nothing r co

-- See Note [Optimising coercion optimisation]
-- | Optimize a coercion, knowing the coercion's non-Phantom role.
opt_co3 :: LiftingContext -> SymFlag -> Maybe Role -> Role -> Coercion -> NormalCo
opt_co3 env sym (Just Phantom)          _ co = opt_phantom env sym co
opt_co3 env sym (Just Representational) r co = opt_co4_wrap env sym True  r co
  -- if mrole is Just Nominal, that can't be a downgrade, so we can ignore
opt_co3 env sym _                       r co = opt_co4_wrap env sym False r co

-- See Note [Optimising coercion optimisation]
-- | Optimize a non-phantom coercion.
opt_co4, opt_co4_wrap :: LiftingContext -> SymFlag -> ReprFlag -> Role -> Coercion -> NormalCo

opt_co4_wrap = opt_co4
{-
opt_co4_wrap env sym rep r co
  = pprTrace "opt_co4_wrap {"
    ( vcat [ text "Sym:" <+> ppr sym
           , text "Rep:" <+> ppr rep
           , text "Role:" <+> ppr r
           , text "Co:" <+> ppr co ]) $
    ASSERT( r == coercionRole co )
    let result = opt_co4 env sym rep r co in
    pprTrace "opt_co4_wrap }" (ppr co $$ text "---" $$ ppr result) $
    result
-}

opt_co4 env _   rep r (Refl _r ty)
  = ASSERT2( r == _r, text "Expected role:" <+> ppr r $$
                      text "Found role:" <+> ppr _r   $$
                      text "Type:" <+> ppr ty )
    liftCoSubst (chooseRole rep r) env ty

opt_co4 env sym rep r (SymCo co)  = opt_co4_wrap env (not sym) rep r co
  -- surprisingly, we don't have to do anything to the env here. This is
  -- because any "lifting" substitutions in the env are tied to ForAllCos,
  -- which treat their left and right sides differently. We don't want to
  -- exchange them.

opt_co4 env sym rep r g@(TyConAppCo _r tc cos)
  = ASSERT( r == _r )
    case (rep, r) of
      (True, Nominal) ->
        mkTyConAppCo Representational tc
                     (zipWith3 (opt_co3 env sym)
                               (map Just (tyConRolesRepresentational tc))
                               (repeat Nominal)
                               cos)
      (False, Nominal) ->
        mkTyConAppCo Nominal tc (map (opt_co4_wrap env sym False Nominal) cos)
      (_, Representational) ->
                      -- must use opt_co2 here, because some roles may be P
                      -- See Note [Optimising coercion optimisation]
        mkTyConAppCo r tc (zipWith (opt_co2 env sym)
                                   (tyConRolesRepresentational tc)  -- the current roles
                                   cos)
      (_, Phantom) -> pprPanic "opt_co4 sees a phantom!" (ppr g)

opt_co4 env sym rep r (AppCo co1 co2)
  = mkAppCo (opt_co4_wrap env sym rep r co1)
            (opt_co4_wrap env sym False Nominal co2)

opt_co4 env sym rep r (ForAllCo tv k_co co)
  = case optForAllCoBndr env sym tv k_co of
      (env', tv', k_co') -> mkForAllCo tv' k_co' $
                            opt_co4_wrap env' sym rep r co
     -- Use the "mk" functions to check for nested Refls

opt_co4 env sym rep r (FunCo _r co1 co2)
  = ASSERT( r == _r )
    if rep
    then mkFunCo Representational co1' co2'
    else mkFunCo r co1' co2'
  where
    co1' = opt_co4_wrap env sym rep r co1
    co2' = opt_co4_wrap env sym rep r co2

opt_co4 env sym rep r (CoVarCo cv)
  | Just co <- lookupCoVar (lcTCvSubst env) cv
  = opt_co4_wrap (zapLiftingContext env) sym rep r co

  | Just cv1 <- lookupInScope (lcInScopeSet env) cv
  = ASSERT( isCoVar cv1 ) wrapRole rep r $ wrapSym sym (CoVarCo cv1)
                -- cv1 might have a substituted kind!

  | otherwise = WARN( True, text "opt_co: not in scope:" <+> ppr cv $$ ppr env)
                ASSERT( isCoVar cv )
                wrapRole rep r $ wrapSym sym (CoVarCo cv)

opt_co4 env sym rep r (AxiomInstCo con ind cos)
    -- Do *not* push sym inside top-level axioms
    -- e.g. if g is a top-level axiom
    --   g a : f a ~ a
    -- then (sym (g ty)) /= g (sym ty) !!
  = ASSERT( r == coAxiomRole con )
    wrapRole rep (coAxiomRole con) $
    wrapSym sym $
                       -- some sub-cos might be P: use opt_co2
                       -- See Note [Optimising coercion optimisation]
    AxiomInstCo con ind (zipWith (opt_co2 env False)
                                 (coAxBranchRoles (coAxiomNthBranch con ind))
                                 cos)
      -- Note that the_co does *not* have sym pushed into it

opt_co4 env sym rep r (UnivCo prov _r t1 t2)
  = ASSERT( r == _r )
    opt_univ env sym prov (chooseRole rep r) t1 t2

opt_co4 env sym rep r (TransCo co1 co2)
                      -- sym (g `o` h) = sym h `o` sym g
  | sym       = opt_trans in_scope co2' co1'
  | otherwise = opt_trans in_scope co1' co2'
  where
    co1' = opt_co4_wrap env sym rep r co1
    co2' = opt_co4_wrap env sym rep r co2
    in_scope = lcInScopeSet env


opt_co4 env sym rep r co@(NthCo {}) = opt_nth_co env sym rep r co

opt_co4 env sym rep r (LRCo lr co)
  | Just pr_co <- splitAppCo_maybe co
  = ASSERT( r == Nominal )
    opt_co4_wrap env sym rep Nominal (pick_lr lr pr_co)
  | Just pr_co <- splitAppCo_maybe co'
  = ASSERT( r == Nominal )
    if rep
    then opt_co4_wrap (zapLiftingContext env) False True Nominal (pick_lr lr pr_co)
    else pick_lr lr pr_co
  | otherwise
  = wrapRole rep Nominal $ LRCo lr co'
  where
    co' = opt_co4_wrap env sym False Nominal co

    pick_lr CLeft  (l, _) = l
    pick_lr CRight (_, r) = r

-- See Note [Optimising InstCo]
opt_co4 env sym rep r (InstCo co1 arg)
    -- forall over type...
  | Just (tv, kind_co, co_body) <- splitForAllCo_maybe co1
  = opt_co4_wrap (extendLiftingContext env tv
                    (arg' `mkCoherenceRightCo` mkSymCo kind_co))
                 sym rep r co_body

    -- See if it is a forall after optimization
    -- If so, do an inefficient one-variable substitution, then re-optimize

    -- forall over type...
  | Just (tv', kind_co', co_body') <- splitForAllCo_maybe co1'
  = opt_co4_wrap (extendLiftingContext (zapLiftingContext env) tv'
                    (arg' `mkCoherenceRightCo` mkSymCo kind_co'))
            False False r' co_body'

  | otherwise = InstCo co1' arg'
  where
    co1' = opt_co4_wrap env sym rep r co1
    r'   = chooseRole rep r
    arg' = opt_co4_wrap env sym False Nominal arg

opt_co4 env sym rep r (CoherenceCo co1 co2)
  | TransCo col1 cor1 <- co1
  = opt_co4_wrap env sym rep r (mkTransCo (mkCoherenceCo col1 co2) cor1)

  | TransCo col1' cor1' <- co1'
  = if sym then opt_trans in_scope col1'
                  (optCoercion (zapTCvSubst (lcTCvSubst env))
                               (mkCoherenceRightCo cor1' co2'))
           else opt_trans in_scope (mkCoherenceCo col1' co2') cor1'

  | otherwise
  = wrapSym sym $ mkCoherenceCo (opt_co4_wrap env False rep r co1) co2'
  where co1' = opt_co4_wrap env sym   rep   r       co1
        co2' = opt_co4_wrap env False False Nominal co2
        in_scope = lcInScopeSet env

opt_co4 env sym _rep r (KindCo co)
  = ASSERT( r == Nominal )
    let kco' = promoteCoercion co in
    case kco' of
      KindCo co' -> promoteCoercion (opt_co1 env sym co')
      _          -> opt_co4_wrap env sym False Nominal kco'
  -- This might be able to be optimized more to do the promotion
  -- and substitution/optimization at the same time

opt_co4 env sym _ r (SubCo co)
  = ASSERT( r == Representational )
    opt_co4_wrap env sym True Nominal co

-- This could perhaps be optimized more.
opt_co4 env sym rep r (AxiomRuleCo co cs)
  = ASSERT( r == coaxrRole co )
    wrapRole rep r $
    wrapSym sym $
    AxiomRuleCo co (zipWith (opt_co2 env False) (coaxrAsmpRoles co) cs)

-------------
-- | Optimize a phantom coercion. The input coercion may not necessarily
-- be a phantom, but the output sure will be.
opt_phantom :: LiftingContext -> SymFlag -> Coercion -> NormalCo
opt_phantom env sym co
  = opt_univ env sym (PhantomProv (mkKindCo co)) Phantom ty1 ty2
  where
    Pair ty1 ty2 = coercionKind co

opt_univ :: LiftingContext -> SymFlag -> UnivCoProvenance -> Role
         -> Type -> Type -> Coercion
opt_univ env sym (PhantomProv h) _r ty1 ty2
  | sym       = mkPhantomCo h' ty2' ty1'
  | otherwise = mkPhantomCo h' ty1' ty2'
  where
    h' = opt_co4 env sym False Nominal h
    ty1' = substTy (lcSubstLeft  env) ty1
    ty2' = substTy (lcSubstRight env) ty2

opt_univ env sym prov role oty1 oty2
  | Just (tc1, tys1) <- splitTyConApp_maybe oty1
  , Just (tc2, tys2) <- splitTyConApp_maybe oty2
  , tc1 == tc2
      -- NB: prov must not be the two interesting ones (ProofIrrel & Phantom);
      -- Phantom is already taken care of, and ProofIrrel doesn't relate tyconapps
  = let roles    = tyConRolesX role tc1
        arg_cos  = zipWith3 (mkUnivCo prov) roles tys1 tys2
        arg_cos' = zipWith (opt_co4 env sym False) roles arg_cos
    in
    mkTyConAppCo role tc1 arg_cos'

  -- can't optimize the AppTy case because we can't build the kind coercions.

  | Just (tv1, ty1) <- splitForAllTy_maybe oty1
  , Just (tv2, ty2) <- splitForAllTy_maybe oty2
      -- NB: prov isn't interesting here either
  = let k1   = tyVarKind tv1
        k2   = tyVarKind tv2
        eta  = mkUnivCo prov Nominal k1 k2
          -- eta gets opt'ed soon, but not yet.
        ty2' = substTyWith [tv2] [TyVarTy tv1 `mkCastTy` eta] ty2

        (env', tv1', eta') = optForAllCoBndr env sym tv1 eta
    in
    mkForAllCo tv1' eta' (opt_univ env' sym prov role ty1 ty2')

  | otherwise
  = let ty1 = substTyUnchecked (lcSubstLeft  env) oty1
        ty2 = substTyUnchecked (lcSubstRight env) oty2
        (a, b) | sym       = (ty2, ty1)
               | otherwise = (ty1, ty2)
    in
    mkUnivCo prov' role a b

  where
    prov' = case prov of
      UnsafeCoerceProv   -> prov
      PhantomProv kco    -> PhantomProv $ opt_co4_wrap env sym False Nominal kco
      ProofIrrelProv kco -> ProofIrrelProv $ opt_co4_wrap env sym False Nominal kco
      PluginProv _       -> prov
      HoleProv h         -> pprPanic "opt_univ fell into a hole" (ppr h)


-------------
-- NthCo must be handled separately, because it's the one case where we can't
-- tell quickly what the component coercion's role is from the containing
-- coercion. To avoid repeated coercionRole calls as opt_co1 calls opt_co2,
-- we just look for nested NthCo's, which can happen in practice.
opt_nth_co :: LiftingContext -> SymFlag -> ReprFlag -> Role -> Coercion -> NormalCo
opt_nth_co env sym rep r = go []
  where
    go ns (NthCo n co) = go (n:ns) co
      -- previous versions checked if the tycon is decomposable. This
      -- is redundant, because a non-decomposable tycon under an NthCo
      -- is entirely bogus. See docs/core-spec/core-spec.pdf.
    go ns co
      = opt_nths ns co

      -- try to resolve 1 Nth
    push_nth n (Refl r1 ty)
      | Just (tc, args) <- splitTyConApp_maybe ty
      = Just (Refl (nthRole r1 tc n) (args `getNth` n))
      | n == 0
      , Just (tv, _) <- splitForAllTy_maybe ty
      = Just (Refl Nominal (tyVarKind tv))
    push_nth n (TyConAppCo _ _ cos)
      = Just (cos `getNth` n)
    push_nth 0 (ForAllCo _ eta _)
      = Just eta
    push_nth _ _ = Nothing

      -- input coercion is *not* yet sym'd or opt'd
    opt_nths [] co = opt_co4_wrap env sym rep r co
    opt_nths (n:ns) co
      | Just co' <- push_nth n co
      = opt_nths ns co'

      -- here, the co isn't a TyConAppCo, so we opt it, hoping to get
      -- a TyConAppCo as output. We don't know the role, so we use
      -- opt_co1. This is slightly annoying, because opt_co1 will call
      -- coercionRole, but as long as we don't have a long chain of
      -- NthCo's interspersed with some other coercion former, we should
      -- be OK.
    opt_nths ns co = opt_nths' ns (opt_co1 env sym co)

      -- input coercion *is* sym'd and opt'd
    opt_nths' [] co
      = if rep && (r == Nominal)
            -- propagate the SubCo:
        then opt_co4_wrap (zapLiftingContext env) False True r co
        else co
    opt_nths' (n:ns) co
      | Just co' <- push_nth n co
      = opt_nths' ns co'
    opt_nths' ns co = wrapRole rep r (mk_nths ns co)

    mk_nths [] co = co
    mk_nths (n:ns) co = mk_nths ns (mkNthCo n co)

-------------
opt_transList :: InScopeSet -> [NormalCo] -> [NormalCo] -> [NormalCo]
opt_transList is = zipWith (opt_trans is)

opt_trans :: InScopeSet -> NormalCo -> NormalCo -> NormalCo
opt_trans is co1 co2
  | isReflCo co1 = co2
  | otherwise    = opt_trans1 is co1 co2

opt_trans1 :: InScopeSet -> NormalNonIdCo -> NormalCo -> NormalCo
-- First arg is not the identity
opt_trans1 is co1 co2
  | isReflCo co2 = co1
  | otherwise    = opt_trans2 is co1 co2

opt_trans2 :: InScopeSet -> NormalNonIdCo -> NormalNonIdCo -> NormalCo
-- Neither arg is the identity
opt_trans2 is (TransCo co1a co1b) co2
    -- Don't know whether the sub-coercions are the identity
  = opt_trans is co1a (opt_trans is co1b co2)

opt_trans2 is co1 co2
  | Just co <- opt_trans_rule is co1 co2
  = co

opt_trans2 is co1 (TransCo co2a co2b)
  | Just co1_2a <- opt_trans_rule is co1 co2a
  = if isReflCo co1_2a
    then co2b
    else opt_trans1 is co1_2a co2b

opt_trans2 _ co1 co2
  = mkTransCo co1 co2

------
-- Optimize coercions with a top-level use of transitivity.
opt_trans_rule :: InScopeSet -> NormalNonIdCo -> NormalNonIdCo -> Maybe NormalCo

-- Push transitivity through matching destructors
opt_trans_rule is in_co1@(NthCo d1 co1) in_co2@(NthCo d2 co2)
  | d1 == d2
  , co1 `compatible_co` co2
  = fireTransRule "PushNth" in_co1 in_co2 $
    mkNthCo d1 (opt_trans is co1 co2)

opt_trans_rule is in_co1@(LRCo d1 co1) in_co2@(LRCo d2 co2)
  | d1 == d2
  , co1 `compatible_co` co2
  = fireTransRule "PushLR" in_co1 in_co2 $
    mkLRCo d1 (opt_trans is co1 co2)

-- Push transitivity inside instantiation
opt_trans_rule is in_co1@(InstCo co1 ty1) in_co2@(InstCo co2 ty2)
  | ty1 `eqCoercion` ty2
  , co1 `compatible_co` co2
  = fireTransRule "TrPushInst" in_co1 in_co2 $
    mkInstCo (opt_trans is co1 co2) ty1

opt_trans_rule is in_co1@(UnivCo p1 r1 tyl1 _tyr1)
                  in_co2@(UnivCo p2 r2 _tyl2 tyr2)
  | Just prov' <- opt_trans_prov p1 p2
  = ASSERT( r1 == r2 )
    fireTransRule "UnivCo" in_co1 in_co2 $
    mkUnivCo prov' r1 tyl1 tyr2
  where
    -- if the provenances are different, opt'ing will be very confusing
    opt_trans_prov UnsafeCoerceProv      UnsafeCoerceProv      = Just UnsafeCoerceProv
    opt_trans_prov (PhantomProv kco1)    (PhantomProv kco2)
      = Just $ PhantomProv $ opt_trans is kco1 kco2
    opt_trans_prov (ProofIrrelProv kco1) (ProofIrrelProv kco2)
      = Just $ ProofIrrelProv $ opt_trans is kco1 kco2
    opt_trans_prov (PluginProv str1)     (PluginProv str2)     | str1 == str2 = Just p1
    opt_trans_prov _ _ = Nothing

-- Push transitivity down through matching top-level constructors.
opt_trans_rule is in_co1@(TyConAppCo r1 tc1 cos1) in_co2@(TyConAppCo r2 tc2 cos2)
  | tc1 == tc2
  = ASSERT( r1 == r2 )
    fireTransRule "PushTyConApp" in_co1 in_co2 $
    mkTyConAppCo r1 tc1 (opt_transList is cos1 cos2)

opt_trans_rule is in_co1@(AppCo co1a co1b) in_co2@(AppCo co2a co2b)
  = fireTransRule "TrPushApp" in_co1 in_co2 $
    mkAppCo (opt_trans is co1a co2a)
            (opt_trans is co1b co2b)

-- Eta rules
opt_trans_rule is co1@(TyConAppCo r tc cos1) co2
  | Just cos2 <- etaTyConAppCo_maybe tc co2
  = ASSERT( length cos1 == length cos2 )
    fireTransRule "EtaCompL" co1 co2 $
    mkTyConAppCo r tc (opt_transList is cos1 cos2)

opt_trans_rule is co1 co2@(TyConAppCo r tc cos2)
  | Just cos1 <- etaTyConAppCo_maybe tc co1
  = ASSERT( length cos1 == length cos2 )
    fireTransRule "EtaCompR" co1 co2 $
    mkTyConAppCo r tc (opt_transList is cos1 cos2)

opt_trans_rule is co1@(AppCo co1a co1b) co2
  | Just (co2a,co2b) <- etaAppCo_maybe co2
  = fireTransRule "EtaAppL" co1 co2 $
    mkAppCo (opt_trans is co1a co2a)
            (opt_trans is co1b co2b)

opt_trans_rule is co1 co2@(AppCo co2a co2b)
  | Just (co1a,co1b) <- etaAppCo_maybe co1
  = fireTransRule "EtaAppR" co1 co2 $
    mkAppCo (opt_trans is co1a co2a)
            (opt_trans is co1b co2b)

-- Push transitivity inside forall
opt_trans_rule is co1 co2
  | ForAllCo tv1 eta1 r1 <- co1
  , Just (tv2,eta2,r2) <- etaForAllCo_maybe co2
  = push_trans tv1 eta1 r1 tv2 eta2 r2

  | ForAllCo tv2 eta2 r2 <- co2
  , Just (tv1,eta1,r1) <- etaForAllCo_maybe co1
  = push_trans tv1 eta1 r1 tv2 eta2 r2

  where
  push_trans tv1 eta1 r1 tv2 eta2 r2
    = fireTransRule "EtaAllTy" co1 co2 $
      mkForAllCo tv1 (opt_trans is eta1 eta2) (opt_trans is' r1 r2')
    where
      is' = is `extendInScopeSet` tv1
      r2' = substCoWithUnchecked [tv2] [TyVarTy tv1] r2

-- Push transitivity inside axioms
opt_trans_rule is co1 co2

  -- See Note [Why call checkAxInstCo during optimisation]
  -- TrPushSymAxR
  | Just (sym, con, ind, cos1) <- co1_is_axiom_maybe
  , True <- sym
  , Just cos2 <- matchAxiom sym con ind co2
  , let newAxInst = AxiomInstCo con ind (opt_transList is (map mkSymCo cos2) cos1)
  , Nothing <- checkAxInstCo newAxInst
  = fireTransRule "TrPushSymAxR" co1 co2 $ SymCo newAxInst

  -- TrPushAxR
  | Just (sym, con, ind, cos1) <- co1_is_axiom_maybe
  , False <- sym
  , Just cos2 <- matchAxiom sym con ind co2
  , let newAxInst = AxiomInstCo con ind (opt_transList is cos1 cos2)
  , Nothing <- checkAxInstCo newAxInst
  = fireTransRule "TrPushAxR" co1 co2 newAxInst

  -- TrPushSymAxL
  | Just (sym, con, ind, cos2) <- co2_is_axiom_maybe
  , True <- sym
  , Just cos1 <- matchAxiom (not sym) con ind co1
  , let newAxInst = AxiomInstCo con ind (opt_transList is cos2 (map mkSymCo cos1))
  , Nothing <- checkAxInstCo newAxInst
  = fireTransRule "TrPushSymAxL" co1 co2 $ SymCo newAxInst

  -- TrPushAxL
  | Just (sym, con, ind, cos2) <- co2_is_axiom_maybe
  , False <- sym
  , Just cos1 <- matchAxiom (not sym) con ind co1
  , let newAxInst = AxiomInstCo con ind (opt_transList is cos1 cos2)
  , Nothing <- checkAxInstCo newAxInst
  = fireTransRule "TrPushAxL" co1 co2 newAxInst

  -- TrPushAxSym/TrPushSymAx
  | Just (sym1, con1, ind1, cos1) <- co1_is_axiom_maybe
  , Just (sym2, con2, ind2, cos2) <- co2_is_axiom_maybe
  , con1 == con2
  , ind1 == ind2
  , sym1 == not sym2
  , let branch = coAxiomNthBranch con1 ind1
        qtvs = coAxBranchTyVars branch ++ coAxBranchCoVars branch
        lhs  = coAxNthLHS con1 ind1
        rhs  = coAxBranchRHS branch
        pivot_tvs = exactTyCoVarsOfType (if sym2 then rhs else lhs)
  , all (`elemVarSet` pivot_tvs) qtvs
  = fireTransRule "TrPushAxSym" co1 co2 $
    if sym2
       -- TrPushAxSym
    then liftCoSubstWith role qtvs (opt_transList is cos1 (map mkSymCo cos2)) lhs
       -- TrPushSymAx
    else liftCoSubstWith role qtvs (opt_transList is (map mkSymCo cos1) cos2) rhs
  where
    co1_is_axiom_maybe = isAxiom_maybe co1
    co2_is_axiom_maybe = isAxiom_maybe co2
    role = coercionRole co1 -- should be the same as coercionRole co2!

opt_trans_rule is co1 co2
  | Just (lco, lh) <- isCohRight_maybe co1
  , Just (rco, rh) <- isCohLeft_maybe co2
  , (coercionType lh) `eqType` (coercionType rh)
  = opt_trans_rule is lco rco

opt_trans_rule _ co1 co2        -- Identity rule
  | (Pair ty1 _, r) <- coercionKindRole co1
  , Pair _ ty2 <- coercionKind co2
  , ty1 `eqType` ty2
  = fireTransRule "RedTypeDirRefl" co1 co2 $
    Refl r ty2

opt_trans_rule _ _ _ = Nothing

fireTransRule :: String -> Coercion -> Coercion -> Coercion -> Maybe Coercion
fireTransRule _rule _co1 _co2 res
  = -- pprTrace ("Trans rule fired: " ++ _rule) (vcat [ppr _co1, ppr _co2, ppr res]) $
    Just res

{-
Note [Conflict checking with AxiomInstCo]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the following type family and axiom:

type family Equal (a :: k) (b :: k) :: Bool
type instance where
  Equal a a = True
  Equal a b = False
--
Equal :: forall k::*. k -> k -> Bool
axEqual :: { forall k::*. forall a::k. Equal k a a ~ True
           ; forall k::*. forall a::k. forall b::k. Equal k a b ~ False }

We wish to disallow (axEqual[1] <*> <Int> <Int). (Recall that the index is
0-based, so this is the second branch of the axiom.) The problem is that, on
the surface, it seems that (axEqual[1] <*> <Int> <Int>) :: (Equal * Int Int ~
False) and that all is OK. But, all is not OK: we want to use the first branch
of the axiom in this case, not the second. The problem is that the parameters
of the first branch can unify with the supplied coercions, thus meaning that
the first branch should be taken. See also Note [Apartness] in
types/FamInstEnv.hs.

Note [Why call checkAxInstCo during optimisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is possible that otherwise-good-looking optimisations meet with disaster
in the presence of axioms with multiple equations. Consider

type family Equal (a :: *) (b :: *) :: Bool where
  Equal a a = True
  Equal a b = False
type family Id (a :: *) :: * where
  Id a = a

axEq :: { [a::*].       Equal a a ~ True
        ; [a::*, b::*]. Equal a b ~ False }
axId :: [a::*]. Id a ~ a

co1 = Equal (axId[0] Int) (axId[0] Bool)
  :: Equal (Id Int) (Id Bool) ~  Equal Int Bool
co2 = axEq[1] <Int> <Bool>
  :: Equal Int Bool ~ False

We wish to optimise (co1 ; co2). We end up in rule TrPushAxL, noting that
co2 is an axiom and that matchAxiom succeeds when looking at co1. But, what
happens when we push the coercions inside? We get

co3 = axEq[1] (axId[0] Int) (axId[0] Bool)
  :: Equal (Id Int) (Id Bool) ~ False

which is bogus! This is because the type system isn't smart enough to know
that (Id Int) and (Id Bool) are Surely Apart, as they're headed by type
families. At the time of writing, I (Richard Eisenberg) couldn't think of
a way of detecting this any more efficient than just building the optimised
coercion and checking.
-}

-- | Check to make sure that an AxInstCo is internally consistent.
-- Returns the conflicting branch, if it exists
-- See Note [Conflict checking with AxiomInstCo]
checkAxInstCo :: Coercion -> Maybe CoAxBranch
-- defined here to avoid dependencies in Coercion
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in CoreLint
checkAxInstCo (AxiomInstCo ax ind cos)
  = let branch       = coAxiomNthBranch ax ind
        tvs          = coAxBranchTyVars branch
        cvs          = coAxBranchCoVars branch
        incomps      = coAxBranchIncomps branch
        (tys, cotys) = splitAtList tvs (map (pFst . coercionKind) cos)
        co_args      = map stripCoercionTy cotys
        subst        = zipTvSubst tvs tys `composeTCvSubst`
                       zipCvSubst cvs co_args
        target   = Type.substTys subst (coAxBranchLHS branch)
        in_scope = mkInScopeSet $
                   unionVarSets (map (tyCoVarsOfTypes . coAxBranchLHS) incomps)
        flattened_target = flattenTys in_scope target in
    check_no_conflict flattened_target incomps
  where
    check_no_conflict :: [Type] -> [CoAxBranch] -> Maybe CoAxBranch
    check_no_conflict _    [] = Nothing
    check_no_conflict flat (b@CoAxBranch { cab_lhs = lhs_incomp } : rest)
         -- See Note [Apartness] in FamInstEnv
      | SurelyApart <- tcUnifyTysFG instanceBindFun flat lhs_incomp
      = check_no_conflict flat rest
      | otherwise
      = Just b
checkAxInstCo _ = Nothing


-----------
wrapSym :: SymFlag -> Coercion -> Coercion
wrapSym sym co | sym       = SymCo co
               | otherwise = co

-- | Conditionally set a role to be representational
wrapRole :: ReprFlag
         -> Role         -- ^ current role
         -> Coercion -> Coercion
wrapRole False _       = id
wrapRole True  current = downgradeRole Representational current

-- | If we require a representational role, return that. Otherwise,
-- return the "default" role provided.
chooseRole :: ReprFlag
           -> Role    -- ^ "default" role
           -> Role
chooseRole True _ = Representational
chooseRole _    r = r

-----------
isAxiom_maybe :: Coercion -> Maybe (Bool, CoAxiom Branched, Int, [Coercion])
isAxiom_maybe (SymCo co)
  | Just (sym, con, ind, cos) <- isAxiom_maybe co
  = Just (not sym, con, ind, cos)
isAxiom_maybe (AxiomInstCo con ind cos)
  = Just (False, con, ind, cos)
isAxiom_maybe _ = Nothing

matchAxiom :: Bool -- True = match LHS, False = match RHS
           -> CoAxiom br -> Int -> Coercion -> Maybe [Coercion]
matchAxiom sym ax@(CoAxiom { co_ax_tc = tc }) ind co
  | CoAxBranch { cab_tvs = qtvs
               , cab_cvs = []   -- can't infer these, so fail if there are any
               , cab_roles = roles
               , cab_lhs = lhs
               , cab_rhs = rhs } <- coAxiomNthBranch ax ind
  , Just subst <- liftCoMatch (mkVarSet qtvs)
                              (if sym then (mkTyConApp tc lhs) else rhs)
                              co
  , all (`isMappedByLC` subst) qtvs
  = zipWithM (liftCoSubstTyVar subst) roles qtvs

  | otherwise
  = Nothing

-------------
-- destruct a CoherenceCo
isCohLeft_maybe :: Coercion -> Maybe (Coercion, Coercion)
isCohLeft_maybe (CoherenceCo co1 co2) = Just (co1, co2)
isCohLeft_maybe _                     = Nothing

-- destruct a (sym (co1 |> co2)).
-- if isCohRight_maybe co = Just (co1, co2), then (sym co1) `mkCohRightCo` co2 = co
isCohRight_maybe :: Coercion -> Maybe (Coercion, Coercion)
isCohRight_maybe (SymCo (CoherenceCo co1 co2)) = Just (mkSymCo co1, co2)
isCohRight_maybe _                             = Nothing

-------------
compatible_co :: Coercion -> Coercion -> Bool
-- Check whether (co1 . co2) will be well-kinded
compatible_co co1 co2
  = x1 `eqType` x2
  where
    Pair _ x1 = coercionKind co1
    Pair x2 _ = coercionKind co2

-------------
{-
etaForAllCo_maybe
~~~~~~~~~~~~~~~~~
Suppose we have

  g : all a1:k1.t1  ~  all a2:k2.t2

but g is *not* a ForAllCo. We want to eta-expand it. So, we do this:

  g' = all a1:(ForAllKindCo g).(InstCo g (a1 `mkCoherenceRightCo` ForAllKindCo g))

Call the kind coercion h1 and the body coercion h2. We can see that

  h2 : t1 ~ t2[a2 |-> (a1 |> h2)]

According to the typing rule for ForAllCo, we get that

  g' : all a1:k1.t1  ~  all a1:k2.(t2[a2 |-> (a1 |> h2)][a1 |-> a1 |> sym h2])

or

  g' : all a1:k1.t1  ~  all a1:k2.(t2[a2 |-> a1])

as desired.
-}
etaForAllCo_maybe :: Coercion -> Maybe (TyVar, Coercion, Coercion)
-- Try to make the coercion be of form (forall tv:kind_co. co)
etaForAllCo_maybe co
  | ForAllCo tv kind_co r <- co
  = Just (tv, kind_co, r)

  | Pair ty1 ty2  <- coercionKind co
  , Just (tv1, _) <- splitForAllTy_maybe ty1
  , isForAllTy ty2
  , let kind_co = mkNthCo 0 co
  = Just ( tv1, kind_co
         , mkInstCo co (mkNomReflCo (TyVarTy tv1) `mkCoherenceRightCo` kind_co) )

  | otherwise
  = Nothing

etaAppCo_maybe :: Coercion -> Maybe (Coercion,Coercion)
-- If possible, split a coercion
--   g :: t1a t1b ~ t2a t2b
-- into a pair of coercions (left g, right g)
etaAppCo_maybe co
  | Just (co1,co2) <- splitAppCo_maybe co
  = Just (co1,co2)
  | (Pair ty1 ty2, Nominal) <- coercionKindRole co
  , Just (_,t1) <- splitAppTy_maybe ty1
  , Just (_,t2) <- splitAppTy_maybe ty2
  , let isco1 = isCoercionTy t1
  , let isco2 = isCoercionTy t2
  , isco1 == isco2
  = Just (LRCo CLeft co, LRCo CRight co)
  | otherwise
  = Nothing

etaTyConAppCo_maybe :: TyCon -> Coercion -> Maybe [Coercion]
-- If possible, split a coercion
--       g :: T s1 .. sn ~ T t1 .. tn
-- into [ Nth 0 g :: s1~t1, ..., Nth (n-1) g :: sn~tn ]
etaTyConAppCo_maybe tc (TyConAppCo _ tc2 cos2)
  = ASSERT( tc == tc2 ) Just cos2

etaTyConAppCo_maybe tc co
  | mightBeUnsaturatedTyCon tc
  , (Pair ty1 ty2, r) <- coercionKindRole co
  , Just (tc1, tys1)  <- splitTyConApp_maybe ty1
  , Just (tc2, tys2)  <- splitTyConApp_maybe ty2
  , tc1 == tc2
  , isInjectiveTyCon tc r  -- See Note [NthCo and newtypes] in TyCoRep
  , let n = length tys1
  = ASSERT( tc == tc1 )
    ASSERT( n == length tys2 )
    Just (decomposeCo n co)
    -- NB: n might be <> tyConArity tc
    -- e.g.   data family T a :: * -> *
    --        g :: T a b ~ T c d

  | otherwise
  = Nothing

{-
Note [Eta for AppCo]
~~~~~~~~~~~~~~~~~~~~
Suppose we have
   g :: s1 t1 ~ s2 t2

Then we can't necessarily make
   left  g :: s1 ~ s2
   right g :: t1 ~ t2
because it's possible that
   s1 :: * -> *         t1 :: *
   s2 :: (*->*) -> *    t2 :: * -> *
and in that case (left g) does not have the same
kind on either side.

It's enough to check that
  kind t1 = kind t2
because if g is well-kinded then
  kind (s1 t2) = kind (s2 t2)
and these two imply
  kind s1 = kind s2

-}

optForAllCoBndr :: LiftingContext -> Bool
                -> TyVar -> Coercion -> (LiftingContext, TyVar, Coercion)
optForAllCoBndr env sym
  = substForAllCoBndrCallbackLC sym (opt_co4_wrap env sym False Nominal) env
