-- (c) The University of Glasgow 2006

{-# LANGUAGE CPP #-}

module GHC.Core.Coercion.Opt
   ( optCoercion
   , OptCoercionOpts (..)
   )
where

import GHC.Prelude

import GHC.Tc.Utils.TcType   ( exactTyCoVarsOfType )

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Subst
import GHC.Core.TyCo.Compare( eqType, eqForAllVis )
import GHC.Core.Coercion
import GHC.Core.Type as Type hiding( substTyVarBndr, substTy )
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.Unify

import GHC.Types.Basic( SwapFlag(..), flipSwap, isSwapped, pickSwap, notSwapped )
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Var.Env

import GHC.Data.Pair

import GHC.Utils.Outputable
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Misc
import GHC.Utils.Panic

import Control.Monad   ( zipWithM )

{-
%************************************************************************
%*                                                                      *
                 Optimising coercions
%*                                                                      *
%************************************************************************

This module does coercion optimisation.  See the paper

   Evidence normalization in Systtem FV (RTA'13)
   https://simon.peytonjones.org/evidence-normalization/

The paper is also in the GHC repo, in docs/opt-coercion.

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
Optimising InstCo is pretty subtle: #15725, #25387.

(1) tv is a type variable. We want to optimise

  InstCo (ForAllCo tv kco g) g2  -->   S(g)

where S is some substitution. Let's look at the typing rules.

    kco : k1 ~ k2
    tv:k1 |- g : t1 ~ t2
    -----------------------------
    ForAllCo tv kco g : (all tv:k1.t1) ~ (all tv:k2.t2[tv |-> tv |> sym kco])

    g1 : (all tv:k1.t1') ~ (all tv:k2.t2')
    g2 : (s1:k1) ~ (s2:k2)
    --------------------
    InstCo g1 g2 : t1'[tv |-> s1] ~ t2'[tv |-> s2]

Putting these two together

    kco : k1 ~ k2
    tv:k1 |- g : t1 ~ t2
    g2 : (s1:k1) ~ (s2:k2)
    --------------------
    InstCo (ForAllCo tv kco g) g2 : t1[tv |-> s1] ~ t2[tv |-> s2 |> sym kco]

We thus want S(g) to have kind

  S(g) :: (t1[tv |-> s1]) ~ (t2[tv |-> s2 |> sym kco])

All we need do is to substitute the coercion tv_co for tv:
  S = [tv :-> tv_co]
where
  tv_co : s1 ~ (s2 |> sym kco)
This looks bizarre, because we're substituting a /type variable/ with a
/coercion/. However, this operation already exists: it's called *lifting*, and
defined in GHC.Core.Coercion.  We just need to enhance the lifting operation to
be able to deal with an ambient substitution, which is why a LiftingContext
stores a TCvSubst.

In general if
  S = [tv :-> tv_co]
  tv_co : r1 ~ r2
  g     : t1 ~ t2
then
  S(g) : t1[tv :-> r1] ~ t2[tv :-> r2]

The substitution S is embodied in the LiftingContext argument of `opt_co4`;
See Note [The LiftingContext in optCoercion]

(2) cv is a coercion variable
Now consider we have (InstCo (ForAllCo cv h g) g2), we want to optimise.

h : (t1 ~r t2) ~N (t3 ~r t4)
cv : t1 ~r t2 |- g : t1' ~r2 t2'
n1 = nth r 2 (downgradeRole r N h) :: t1 ~r t3
n2 = nth r 3 (downgradeRole r N h) :: t2 ~r t4
------------------------------------------------
ForAllCo cv h g : (all cv:t1 ~r t2. t1') ~r2
                  (all cv:t3 ~r t4. t2'[cv |-> n1 ; cv ; sym n2])

g1 : (all cv:t1 ~r t2. t1') ~ (all cv: t3 ~r t4. t2')
g2 : h1 ~N h2
h1 : t1 ~r t2
h2 : t3 ~r t4
------------------------------------------------
InstCo g1 g2 : t1'[cv |-> h1] ~ t2'[cv |-> h2]

We thus want some coercion proving this:

  t1'[cv |-> h1] ~ t2'[cv |-> n1 ; h2; sym n2]

So we substitute the coercion variable c for the coercion
(h1 ~N (n1; h2; sym n2)) in g.

Note [The LiftingContext in optCoercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To suppport Note [Optimising InstCo] the coercion optimiser carries a
GHC.Core.Coercion.LiftingContext, which comprises
  * An ordinary Subst
  * The `lc_env`: a mapping from /type variables/ to /coercions/

We don't actually have a separate function
   liftCoSubstCo :: LiftingContext -> Coercion -> Coercion
The substitution of a type variable by a coercion is done by the calls to
`liftCoSubst` (on a type) in the Refl and GRefl cases of `opt_co4`.

We use the following invariants:
 (LC1) The coercions in the range of `lc_env` have already had all substitutions
       applied; they are "OutCoercions".  If you re-optimise these coercions, you
       must zap the LiftingContext first.

 (LC2) However they have /not/ had the "ambient sym" (the second argument of
       `opt_co4`) applied.  The ambient sym applies to the entire coercion not
       to the little bits being substituted.
-}

-- | Coercion optimisation options
newtype OptCoercionOpts = OptCoercionOpts
   { optCoercionEnabled :: Bool  -- ^ Enable coercion optimisation (reduce its size)
   }

optCoercion :: OptCoercionOpts -> Subst -> Coercion -> NormalCo
-- ^ optCoercion applies a substitution to a coercion,
--   *and* optimises it to reduce its size
optCoercion opts env co
  | optCoercionEnabled opts
  = optCoercion' env co

{-
  = pprTrace "optCoercion {" (text "Co:" <> ppr (coercionSize co)) $
    let result = optCoercion' env co in
    pprTrace "optCoercion }"
       (vcat [ text "Co:"    <+> ppr (coercionSize co)
             , text "Optco:" <+> ppWhen (isReflCo result) (text "(refl)")
                             <+> ppr (coercionSize result) ]) $
    result
-}

  | otherwise
  = substCo env co

optCoercion' :: Subst -> Coercion -> NormalCo
optCoercion' env co
  | debugIsOn
  = let out_co = opt_co1 lc NotSwapped co
        (Pair in_ty1  in_ty2,  in_role)  = coercionKindRole co
        (Pair out_ty1 out_ty2, out_role) = coercionKindRole out_co

        details = vcat [ text "in_co:" <+> ppr co
                       , text "in_ty1:" <+> ppr in_ty1
                       , text "in_ty2:" <+> ppr in_ty2
                       , text "out_co:" <+> ppr out_co
                       , text "out_ty1:" <+> ppr out_ty1
                       , text "out_ty2:" <+> ppr out_ty2
                       , text "in_role:" <+> ppr in_role
                       , text "out_role:" <+> ppr out_role
                       ]
    in
    warnPprTrace (not (isReflCo out_co) && isReflexiveCo out_co)
                 "optCoercion: reflexive but not refl" details $
--    assertPpr (substTyUnchecked env in_ty1 `eqType` out_ty1 &&
--               substTyUnchecked env in_ty2 `eqType` out_ty2 &&
--               in_role == out_role)
--              (hang (text "optCoercion changed types!") 2 details) $
    out_co

  | otherwise
  = opt_co1 lc NotSwapped co
  where
    lc = mkSubstLiftingContext env
--    ppr_one cv = ppr cv <+> dcolon <+> ppr (coVarKind cv)


type NormalCo    = Coercion
  -- Invariants:
  --  * The substitution has been fully applied
  --  * For trans coercions (co1 `trans` co2)
  --       co1 is not a trans, and neither co1 nor co2 is identity

type NormalNonIdCo = NormalCo  -- Extra invariant: not the identity

-- | Do we force the result to be representational?
type ReprFlag = Bool

-- | Optimize a coercion, making no assumptions. All coercions in
-- the lifting context are already optimized (and sym'd if nec'y)
opt_co1 :: LiftingContext
        -> SwapFlag   -- IsSwapped => apply Sym to the result
        -> Coercion -> NormalCo
opt_co1 env sym co = opt_co2 env sym (coercionRole co) co

-- See Note [Optimising coercion optimisation]
-- | Optimize a coercion, knowing the coercion's role. No other assumptions.
opt_co2 :: LiftingContext
        -> SwapFlag   -- ^IsSwapped => apply Sym to the result
        -> Role       -- ^ The role of the input coercion
        -> Coercion -> NormalCo
opt_co2 env sym Phantom co = opt_phantom env sym co
opt_co2 env sym r       co = opt_co4 env sym False r co

-- See Note [Optimising coercion optimisation]
-- | Optimize a coercion, knowing the coercion's non-Phantom role,
--   and with an optional downgrade
opt_co3 :: LiftingContext -> SwapFlag -> Maybe Role -> Role -> Coercion -> NormalCo
opt_co3 env sym (Just Phantom)          _ co = opt_phantom env sym co
opt_co3 env sym (Just Representational) r co = opt_co4 env sym True  r co
  -- if mrole is Just Nominal, that can't be a downgrade, so we can ignore
opt_co3 env sym _                       r co = opt_co4 env sym False r co

-- See Note [Optimising coercion optimisation]
-- | Optimize a non-phantom coercion.
opt_co4, opt_co4' :: LiftingContext -> SwapFlag -> ReprFlag
                  -> Role -> Coercion -> NormalCo
-- Precondition:  In every call (opt_co4 lc sym rep role co)
--                we should have role = coercionRole co
-- Precondition:  role is not Phantom
-- Postcondition: The resulting coercion is equivalant to
--                     wrapsub (wrapsym (mksub co)
--                 where wrapsym is SymCo if sym=True
--                       wrapsub is SubCo if rep=True

-- opt_co4 is there just to support tracing, when debugging
-- Usually it just goes straight to opt_co4'
opt_co4 = opt_co4'

{-
opt_co4 env sym rep r co
  = pprTrace "opt_co4 {"
   ( vcat [ text "Sym:" <+> ppr sym
          , text "Rep:" <+> ppr rep
          , text "Role:" <+> ppr r
          , text "Co:" <+> ppr co ]) $
   assert (r == coercionRole co )    $
   let result = opt_co4' env sym rep r co in
   pprTrace "opt_co4 }" (ppr co $$ text "---" $$ ppr result) $
   assertPpr (res_role == coercionRole result)
             (vcat [ text "Role:" <+> ppr r
                   , text "Result: " <+>  ppr result
                   , text "Result type:" <+> ppr (coercionType result) ]) $
   result

  where
    res_role | rep       = Representational
             | otherwise = r
-}

opt_co4' env sym rep r (Refl ty)
  = assertPpr (r == Nominal)
              (text "Expected role:" <+> ppr r    $$
               text "Found role:" <+> ppr Nominal $$
               text "Type:" <+> ppr ty) $
    wrapSym sym $ liftCoSubst (chooseRole rep r) env ty
        -- wrapSym: see (LC2) of Note [The LiftingContext in optCoercion]

opt_co4' env sym rep r (GRefl _r ty MRefl)
  = assertPpr (r == _r)
              (text "Expected role:" <+> ppr r $$
               text "Found role:" <+> ppr _r   $$
               text "Type:" <+> ppr ty) $
    wrapSym sym $ liftCoSubst (chooseRole rep r) env ty
        -- wrapSym: see (LC2) of Note [The LiftingContext in optCoercion]

opt_co4' env sym  rep r (GRefl _r ty (MCo kco))
  = assertPpr (r == _r)
              (text "Expected role:" <+> ppr r $$
               text "Found role:" <+> ppr _r   $$
               text "Type:" <+> ppr ty) $
    if isGReflCo kco || isGReflCo kco'
    then wrapSym sym ty_co
    else wrapSym sym $ mk_coherence_right_co r' (coercionRKind ty_co) kco' ty_co
            -- ty :: k1
            -- kco :: k1 ~ k2
            -- Desired result coercion:   ty ~ ty |> co
  where
    r'    = chooseRole rep r
    ty_co = liftCoSubst r' env ty
    kco'  = opt_co4 env NotSwapped False Nominal kco

opt_co4' env sym rep r (SymCo co)  = opt_co4 env (flipSwap sym) rep r co
  -- surprisingly, we don't have to do anything to the env here. This is
  -- because any "lifting" substitutions in the env are tied to ForAllCos,
  -- which treat their left and right sides differently. We don't want to
  -- exchange them.

opt_co4' env sym rep r g@(TyConAppCo _r tc cos)
  = assert (r == _r) $
    case (rep, r) of
      (True, Nominal) ->
        mkTyConAppCo Representational tc
                     (zipWith3 (opt_co3 env sym)
                               (map Just (tyConRoleListRepresentational tc))
                               (repeat Nominal)
                               cos)
      (False, Nominal) ->
        mkTyConAppCo Nominal tc (map (opt_co4 env sym False Nominal) cos)
      (_, Representational) ->
                      -- must use opt_co2 here, because some roles may be P
                      -- See Note [Optimising coercion optimisation]
        mkTyConAppCo r tc (zipWith (opt_co2 env sym)
                                   (tyConRoleListRepresentational tc)  -- the current roles
                                   cos)
      (_, Phantom) -> pprPanic "opt_co4 sees a phantom!" (ppr g)

opt_co4' env sym rep r (AppCo co1 co2)
  = mkAppCo (opt_co4 env sym rep r co1)
            (opt_co4 env sym False Nominal co2)

opt_co4' env sym rep r (ForAllCo { fco_tcv = tv, fco_visL = visL, fco_visR = visR
                                , fco_kind = k_co, fco_body = co })
  = case optForAllCoBndr env sym tv k_co of
      (env', tv', k_co') -> mkForAllCo tv' visL' visR' k_co' $
                            opt_co4 env' sym rep r co
     -- Use the "mk" functions to check for nested Refls
  where
    !(visL', visR') = swapSym sym (visL, visR)

opt_co4' env sym rep r (FunCo _r afl afr cow co1 co2)
  = assert (r == _r) $
    mkFunCo2 r' afl' afr' cow' co1' co2'
  where
    co1' = opt_co4 env sym rep r co1
    co2' = opt_co4 env sym rep r co2
    cow' = opt_co1 env sym cow
    !r' | rep       = Representational
        | otherwise = r
    !(afl', afr') = swapSym sym (afl, afr)

opt_co4' env sym rep r (CoVarCo cv)
  | Just co <- lcLookupCoVar env cv   -- see Note [Forall over coercion] for why
                                      -- this is the right thing here
  = -- pprTrace "CoVarCo" (ppr cv $$ ppr co) $
    opt_co4 (zapLiftingContext env) sym rep r co

  | ty1 `eqType` ty2   -- See Note [Optimise CoVarCo to Refl]
  = mkReflCo (chooseRole rep r) ty1

  | otherwise
  = assert (isCoVar cv1) $
    wrapRole rep r $ wrapSym sym $
    CoVarCo cv1

  where
    Pair ty1 ty2 = coVarTypes cv1

    cv1 = case lookupInScope (lcInScopeSet env) cv of
             Just cv1 -> cv1
             Nothing  -> warnPprTrace True
                          "opt_co: not in scope"
                          (ppr cv $$ ppr env)
                          cv
          -- cv1 might have a substituted kind!

opt_co4' _ _ _ _ (HoleCo h)
  = pprPanic "opt_univ fell into a hole" (ppr h)

opt_co4' env sym rep r (AxiomCo con cos)
    -- Do *not* push sym inside top-level axioms
    -- e.g. if g is a top-level axiom
    --   g a : f a ~ a
    -- then (sym (g ty)) /= g (sym ty) !!
  = assert (r == coAxiomRuleRole con )
    wrapRole rep (coAxiomRuleRole con) $
    wrapSym sym $
                       -- some sub-cos might be P: use opt_co2
                       -- See Note [Optimising coercion optimisation]
    AxiomCo con (zipWith (opt_co2 env NotSwapped)
                         (coAxiomRuleArgRoles con)
                         cos)
      -- Note that the_co does *not* have sym pushed into it

opt_co4' env sym rep r (UnivCo { uco_prov = prov, uco_lty = t1
                              , uco_rty = t2, uco_deps = deps })
  = opt_univ env sym prov deps (chooseRole rep r) t1 t2

opt_co4' env sym rep r (TransCo co1 co2)
  -- sym (g `o` h) = sym h `o` sym g
  | isSwapped sym = opt_trans in_scope co2' co1'
  | otherwise     = opt_trans in_scope co1' co2'
  where
    co1' = opt_co4 env sym rep r co1
    co2' = opt_co4 env sym rep r co2
    in_scope = lcInScopeSet env

opt_co4' env sym rep r (SelCo cs co)
  -- Historical note 1: we used to check `co` for Refl, TyConAppCo etc
  -- before optimising `co`; but actually the SelCo will have been built
  -- with mkSelCo, so these tests always fail.

  -- Historical note 2: if rep=True and r=Nominal, we used to recursively
  -- call opt_co4 to re-optimse the result. But (a) that is inefficient
  -- and (b) wrapRole uses mkSubCo which does much the same job
  = wrapRole rep r $ mkSelCo cs $ opt_co1 env sym co

opt_co4' env sym rep r (LRCo lr co)
  | Just pr_co <- splitAppCo_maybe co
  = assert (r == Nominal )
    opt_co4 env sym rep Nominal (pick_lr lr pr_co)
  | Just pr_co <- splitAppCo_maybe co'
  = assert (r == Nominal) $
    if rep
    then opt_co4 (zapLiftingContext env) NotSwapped True Nominal (pick_lr lr pr_co)
    else pick_lr lr pr_co
  | otherwise
  = wrapRole rep Nominal $ LRCo lr co'
  where
    co' = opt_co4 env sym False Nominal co

    pick_lr CLeft  (l, _) = l
    pick_lr CRight (_, r) = r

{-
Note [Forall over coercion]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example:
  type (:~:) :: forall k. k -> k -> Type
  Refl :: forall k (a :: k) (b :: k). forall (cv :: (~#) k k a b). (:~:) k a b
  k1,k2,k3,k4 :: Type
  eta :: (k1 ~# k2) ~# (k3 ~# k4)    ==    ((~#) Type Type k1 k2) ~# ((~#) Type Type k3 k4)
  co1_3 :: k1 ~# k3
  co2_4 :: k2 ~# k4
  nth 2 eta :: k1 ~# k3
  nth 3 eta :: k2 ~# k4
  co11_31 :: <k1> ~# (sym co1_3)
  co22_24 :: <k2> ~# co2_4
  (forall (cv :: eta). Refl <Type> co1_3 co2_4 (co11_31 ;; cv ;; co22_24)) ::
    (forall (cv :: k1 ~# k2). Refl Type k1 k2 (<k1> ;; cv ;; <k2>) ~#
    (forall (cv :: k3 ~# k4). Refl Type k3 k4
       (sym co1_3 ;; nth 2 eta ;; cv ;; sym (nth 3 eta) ;; co2_4))
  co1_2 :: k1 ~# k2
  co3_4 :: k3 ~# k4
  co5 :: co1_2 ~# co3_4
  InstCo (forall (cv :: eta). Refl <Type> co1_3 co2_4 (co11_31 ;; cv ;; co22_24)) co5 ::
   (Refl Type k1 k2 (<k1> ;; cv ;; <k2>))[cv |-> co1_2] ~#
   (Refl Type k3 k4 (sym co1_3 ;; nth 2 eta ;; cv ;; sym (nth 3 eta) ;; co2_4))[cv |-> co3_4]
      ==
   (Refl Type k1 k2 (<k1> ;; co1_2 ;; <k2>)) ~#
    (Refl Type k3 k4 (sym co1_3 ;; nth 2 eta ;; co3_4 ;; sym (nth 3 eta) ;; co2_4))
      ==>
   Refl <Type> co1_3 co2_4 (co11_31 ;; co1_2 ;; co22_24)
Conclusion: Because of the way this all works, we want to put in the *left-hand*
coercion in co5's type. (In the code, co5 is called `arg`.)
So we extend the environment binding cv to arg's left-hand type.
-}

-- See Note [Optimising InstCo]
opt_co4' env sym rep r (InstCo fun_co arg_co)
    -- forall over type...
  | Just (tv, _visL, _visR, k_co, body_co) <- splitForAllCo_ty_maybe fun_co
    -- tv      :: k1
    -- k_co    :: k1 ~ k2
    -- body_co :: t1 ~ t2
    -- arg_co  :: (s1:k1) ~ (s2:k2)
  , let arg_co'  = opt_co4 env NotSwapped False Nominal arg_co
                  -- Do /not/ push Sym into the arg_co, hence sym=False
                  -- see (LC2) of Note [The LiftingContext in optCoercion]
        k_co' = opt_co4 env NotSwapped False Nominal k_co
        s2'   = coercionRKind arg_co'
        tv_co = mk_coherence_right_co Nominal s2' (mkSymCo k_co') arg_co'
                   -- mkSymCo kind_co :: k2 ~ k1
                   -- tv_co   :: (s1 :: k1) ~ (((s2 :: k2) |> (sym kind_co)) :: k1)
  = opt_co4 (extendLiftingContext env tv tv_co) sym rep r body_co

    -- See Note [Forall over coercion]
  | Just (cv, _visL, _visR, _kind_co, body_co) <- splitForAllCo_co_maybe fun_co
  , CoercionTy h1 <- coercionLKind arg_co
  , let h1' = opt_co4 env NotSwapped False Nominal h1
  = opt_co4 (extendLiftingContextCvSubst env cv h1') sym rep r body_co

  -- OK so those cases didn't work.  See if it is a forall /after/ optimization
  -- If so, do an inefficient one-variable substitution, then re-optimize

    -- forall over type...
  | Just (tv', _visL, _visR, k_co', body_co') <- splitForAllCo_ty_maybe fun_co'
  , let s2'   = coercionRKind arg_co'
        tv_co = mk_coherence_right_co Nominal s2' (mkSymCo k_co') arg_co'
        env'  = extendLiftingContext (zapLiftingContext env) tv' tv_co
  = opt_co4 env' NotSwapped False r' body_co'

    -- See Note [Forall over coercion]
  | Just (cv', _visL, _visR, _kind_co', body_co') <- splitForAllCo_co_maybe fun_co'
  , CoercionTy h1' <- coercionLKind arg_co'
  , let env' = extendLiftingContextCvSubst (zapLiftingContext env) cv' h1'
  = opt_co4 env' NotSwapped False r' body_co'

  -- Those cases didn't work either, so rebuild the InstCo
  -- Push Sym into /both/ function /and/ arg_coument
  | otherwise = InstCo fun_co' arg_co'

  where
    -- fun_co' arg_co' are both optimised, /and/ we have pushed `sym` into both
    -- So no more sym'ing on th results of fun_co' arg_co'
    fun_co' = opt_co4 env sym rep r fun_co
    arg_co' = opt_co4 env sym False Nominal arg_co
    r'   = chooseRole rep r

opt_co4' env sym _rep r (KindCo co)
  = assert (r == Nominal) $
    let kco' = promoteCoercion co in
    case kco' of
      KindCo co' -> promoteCoercion (opt_co1 env sym co')
      _          -> opt_co4 env sym False Nominal kco'
  -- This might be able to be optimized more to do the promotion
  -- and substitution/optimization at the same time

opt_co4' env sym _ r (SubCo co)
  = assert (r == Representational) $
    opt_co4 env sym True Nominal co

{- Note [Optimise CoVarCo to Refl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have (c :: t~t) we can optimise it to Refl. That increases the
chances of floating the Refl upwards; e.g. Maybe c --> Refl (Maybe t)

We do so here in optCoercion, not in mkCoVarCo; see Note [mkCoVarCo]
in GHC.Core.Coercion.
-}

-------------
-- | Optimize a phantom coercion. The input coercion may not necessarily
-- be a phantom, but the output sure will be.
opt_phantom :: LiftingContext -> SwapFlag -> Coercion -> NormalCo
opt_phantom env sym (UnivCo { uco_prov = prov, uco_lty = t1
                            , uco_rty = t2, uco_deps = deps })
  = opt_univ env sym prov deps Phantom t1 t2

opt_phantom env sym co
  = opt_univ env sym PhantomProv [mkKindCo co] Phantom ty1 ty2
  where
    Pair ty1 ty2 = coercionKind co

{- Note [Differing kinds]
   ~~~~~~~~~~~~~~~~~~~~~~
The two types may not have the same kind (although that would be very unusual).
But even if they have the same kind, and the same type constructor, the number
of arguments in a `CoTyConApp` can differ. Consider

  Any :: forall k. k

  Any @Type Int                :: Type
  Any @(Type->Type) Maybe Int  :: Type

Hence the need to compare argument lengths; see #13658

Note [opt_univ needs injectivity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If opt_univ sees a coercion between `T a1 a2` and `T b1 b2` it will optimize it
by producing a TyConAppCo for T, and pushing the UnivCo into the arguments.  But
this works only if T is injective. Otherwise we can have something like

  type family F x where
    F Int  = Int
    F Bool = Int

where `UnivCo :: F Int ~ F Bool` is reasonable (it is effectively just an
alternative representation for a couple of uses of AxiomInstCos) but we do not
want to produce `F (UnivCo :: Int ~ Bool)` where the inner coercion is clearly
inconsistent.  Hence the opt_univ case for TyConApps checks isInjectiveTyCon.
See #19509.

 -}

opt_univ :: LiftingContext -> SwapFlag -> UnivCoProvenance
         -> [Coercion]
         -> Role -> Type -> Type -> Coercion
opt_univ env sym prov deps role ty1 ty2
  = let ty1'  = substTyUnchecked (lcSubstLeft  env) ty1
        ty2'  = substTyUnchecked (lcSubstRight env) ty2
        deps' = map (opt_co1 env sym) deps
        (ty1'', ty2'') = swapSym sym (ty1', ty2')
    in
    mkUnivCo prov deps' role ty1'' ty2''

{-
opt_univ env PhantomProv cvs _r ty1 ty2
  = mkUnivCo PhantomProv cvs Phantom ty1' ty2'
  where
    ty1' = substTy (lcSubstLeft  env) ty1
    ty2' = substTy (lcSubstRight env) ty2

opt_univ1 env prov cvs' role oty1 oty2
  | Just (tc1, tys1) <- splitTyConApp_maybe oty1
  , Just (tc2, tys2) <- splitTyConApp_maybe oty2
  , tc1 == tc2
  , isInjectiveTyCon tc1 role  -- see Note [opt_univ needs injectivity]
  , equalLength tys1 tys2 -- see Note [Differing kinds]
      -- NB: prov must not be the two interesting ones (ProofIrrel & Phantom);
      -- Phantom is already taken care of, and ProofIrrel doesn't relate tyconapps
  = let roles    = tyConRoleListX role tc1
        arg_cos  = zipWith3 (mkUnivCo prov cvs') roles tys1 tys2
        arg_cos' = zipWith (opt_co4 env False False) roles arg_cos
    in
    mkTyConAppCo role tc1 arg_cos'

  -- can't optimize the AppTy case because we can't build the kind coercions.

  | Just (Bndr tv1 vis1, ty1) <- splitForAllForAllTyBinder_maybe oty1
  , isTyVar tv1
  , Just (Bndr tv2 vis2, ty2) <- splitForAllForAllTyBinder_maybe oty2
  , isTyVar tv2
      -- NB: prov isn't interesting here either
  = let k1   = tyVarKind tv1
        k2   = tyVarKind tv2
        eta  = mkUnivCo prov cvs' Nominal k1 k2
          -- eta gets opt'ed soon, but not yet.
        ty2' = substTyWith [tv2] [TyVarTy tv1 `mkCastTy` eta] ty2

        (env', tv1', eta') = optForAllCoBndr env False tv1 eta
    in
    mkForAllCo tv1' vis1 vis2 eta' (opt_univ1 env' prov cvs' role ty1 ty2')

  | Just (Bndr cv1 vis1, ty1) <- splitForAllForAllTyBinder_maybe oty1
  , isCoVar cv1
  , Just (Bndr cv2 vis2, ty2) <- splitForAllForAllTyBinder_maybe oty2
  , isCoVar cv2
      -- NB: prov isn't interesting here either
  = let k1    = varType cv1
        k2    = varType cv2
        r'    = coVarRole cv1
        eta   = mkUnivCo prov cvs' Nominal k1 k2
        eta_d = downgradeRole r' Nominal eta
          -- eta gets opt'ed soon, but not yet.
        n_co  = (mkSymCo $ mkSelCo (SelTyCon 2 r') eta_d) `mkTransCo`
                (mkCoVarCo cv1) `mkTransCo`
                (mkSelCo (SelTyCon 3 r') eta_d)
        ty2'  = substTyWithCoVars [cv2] [n_co] ty2

        (env', cv1', eta') = optForAllCoBndr env False cv1 eta
    in
    mkForAllCo cv1' vis1 vis2 eta' (opt_univ1 env' prov cvs' role ty1 ty2')

  | otherwise
  = let ty1 = substTyUnchecked (lcSubstLeft  env) oty1
        ty2 = substTyUnchecked (lcSubstRight env) oty2
    in
    mkUnivCo prov cvs' role ty1 ty2
-}

-------------
opt_transList :: HasDebugCallStack => InScopeSet -> [NormalCo] -> [NormalCo] -> [NormalCo]
opt_transList is = zipWithEqual (opt_trans is)
  -- The input lists must have identical length.

opt_trans :: HasDebugCallStack => InScopeSet -> NormalCo -> NormalCo -> NormalCo

-- opt_trans just allows us to add some debug tracing
-- Usually it just goes to opt_trans'
opt_trans is co1 co2
  = -- (if coercionRKind co1 `eqType` coercionLKind co2
    --  then (\x -> x) else
    --  pprTrace "opt_trans" (vcat [ text "co1" <+> ppr co1
    --                             , text "co2" <+> ppr co2
    --                             , text "co1 kind" <+> ppr (coercionKind co1)
    --                             , text "co2 kind" <+> ppr (coercionKind co2)
    --                             , callStackDoc ])) $
    opt_trans' is co1 co2

{-
opt_trans is co1 co2
  = assertPpr (r1==r2) (vcat [ ppr r1 <+> ppr co1, ppr r2 <+> ppr co2]) $
    assertPpr (rres == r1) (vcat [ ppr r1 <+> ppr co1, ppr r2 <+> ppr co2, text "res" <+> ppr rres <+> ppr res ]) $
    res
  where
    res = opt_trans' is co1 co2
    rres = coercionRole res
    r1 = coercionRole co1
    r2 = coercionRole co1
-}

opt_trans' :: HasDebugCallStack => InScopeSet -> NormalCo -> NormalCo -> NormalCo
opt_trans' is co1 co2
  | isReflCo co1 = co2
    -- optimize when co1 is a Refl Co
  | otherwise    = opt_trans1 is co1 co2

opt_trans1 :: HasDebugCallStack => InScopeSet -> NormalNonIdCo -> NormalCo -> NormalCo
-- First arg is not the identity
opt_trans1 is co1 co2
  | isReflCo co2 = co1
    -- optimize when co2 is a Refl Co
  | otherwise    = opt_trans2 is co1 co2

opt_trans2 :: HasDebugCallStack => InScopeSet -> NormalNonIdCo -> NormalNonIdCo -> NormalCo
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
  = mk_trans_co co1 co2


------
-- Optimize coercions with a top-level use of transitivity.
opt_trans_rule :: HasDebugCallStack => InScopeSet -> NormalNonIdCo -> NormalNonIdCo -> Maybe NormalCo

opt_trans_rule _ in_co1 in_co2
  | assertPpr (coercionRKind in_co1 `eqType` coercionLKind in_co2)
              (vcat [ text "in_co1" <+> ppr in_co1
                   , text "in_co2" <+> ppr in_co2
                   , text "in_co1 kind" <+> ppr (coercionKind in_co1)
                   , text "in_co2 kind" <+> ppr (coercionKind in_co2)
                   , callStackDoc ]) $
    False
  = panic "opt_trans_rule"  -- This entire equation is purely assertion checking

opt_trans_rule is in_co1@(GRefl r1 t1 (MCo co1)) in_co2@(GRefl r2 _t2 (MCo co2))
  = assert (r1 == r2) $
    fireTransRule "GRefl" in_co1 in_co2 $
    mk_grefl_right_co r1 t1 (opt_trans is co1 co2)

-- Push transitivity through matching destructors
opt_trans_rule is in_co1@(SelCo d1 co1) in_co2@(SelCo d2 co2)
  | d1 == d2
  , coercionRole co1 == coercionRole co2
  , co1 `compatible_co` co2
  = fireTransRule "PushNth" in_co1 in_co2 $
    mkSelCo d1 (opt_trans is co1 co2)

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

opt_trans_rule _
    in_co1@(UnivCo { uco_prov = p1, uco_role = r1, uco_lty = tyl1, uco_deps = deps1 })
    in_co2@(UnivCo { uco_prov = p2, uco_role = r2, uco_rty = tyr2, uco_deps = deps2 })
  | p1 == p2    -- If the provenances are different, opt'ing will be very confusing
  = assert (r1 == r2) $
    fireTransRule "UnivCo" in_co1 in_co2 $
    mkUnivCo p1 (deps1 ++ deps2) r1 tyl1 tyr2

-- Push transitivity down through matching top-level constructors.
opt_trans_rule is in_co1@(TyConAppCo r1 tc1 cos1) in_co2@(TyConAppCo r2 tc2 cos2)
  | tc1 == tc2
  = assert (r1 == r2) $
    fireTransRule "PushTyConApp" in_co1 in_co2 $
    mkTyConAppCo r1 tc1 (opt_transList is cos1 cos2)

opt_trans_rule is in_co1@(FunCo r1 afl1 afr1 w1 co1a co1b)
                  in_co2@(FunCo r2 afl2 afr2 w2 co2a co2b)
  = assert (r1 == r2)     $     -- Just like the TyConAppCo/TyConAppCo case
    assert (afr1 == afl2) $
    fireTransRule "PushFun" in_co1 in_co2 $
    mkFunCo2 r1 afl1 afr2 (opt_trans is w1 w2)
                          (opt_trans is co1a co2a)
                          (opt_trans is co1b co2b)

opt_trans_rule is in_co1@(AppCo co1a co1b) in_co2@(AppCo co2a co2b)
  -- Must call opt_trans_rule_app; see Note [EtaAppCo]
  = opt_trans_rule_app is in_co1 in_co2 co1a [co1b] co2a [co2b]

-- Eta rules
opt_trans_rule is co1@(TyConAppCo r tc cos1) co2
  | Just cos2 <- etaTyConAppCo_maybe tc co2
  = fireTransRule "EtaCompL" co1 co2 $
    mkTyConAppCo r tc (opt_transList is cos1 cos2)

opt_trans_rule is co1 co2@(TyConAppCo r tc cos2)
  | Just cos1 <- etaTyConAppCo_maybe tc co1
  = fireTransRule "EtaCompR" co1 co2 $
    mkTyConAppCo r tc (opt_transList is cos1 cos2)

opt_trans_rule is co1@(AppCo co1a co1b) co2
  | Just (co2a,co2b) <- etaAppCo_maybe co2
  = opt_trans_rule_app is co1 co2 co1a [co1b] co2a [co2b]

opt_trans_rule is co1 co2@(AppCo co2a co2b)
  | Just (co1a,co1b) <- etaAppCo_maybe co1
  = opt_trans_rule_app is co1 co2 co1a [co1b] co2a [co2b]

-- Push transitivity inside forall
-- forall over types.
opt_trans_rule is co1 co2
  | Just (tv1, visL1, _visR1, eta1, r1) <- splitForAllCo_ty_maybe co1
  , Just (tv2, _visL2, visR2, eta2, r2) <- etaForAllCo_ty_maybe co2
  = push_trans tv1 eta1 r1 tv2 eta2 r2 visL1 visR2

  | Just (tv2, _visL2, visR2, eta2, r2) <- splitForAllCo_ty_maybe co2
  , Just (tv1, visL1, _visR1, eta1, r1) <- etaForAllCo_ty_maybe co1
  = push_trans tv1 eta1 r1 tv2 eta2 r2 visL1 visR2

  where
  push_trans tv1 eta1 r1 tv2 eta2 r2 visL visR
    -- Given:
    --   co1 = /\ tv1 : eta1 <visL, visM>. r1
    --   co2 = /\ tv2 : eta2 <visM, visR>. r2
    -- Wanted:
    --   /\tv1 : (eta1;eta2) <visL, visR>.  (r1; r2[tv2 |-> tv1 |> eta1])
    = fireTransRule "EtaAllTy_ty" co1 co2 $
      mkForAllCo tv1 visL visR (opt_trans is eta1 eta2) (opt_trans is' r1 r2')
    where
      is' = is `extendInScopeSet` tv1
      r2' = substCoWithUnchecked [tv2] [mkCastTy (TyVarTy tv1) eta1] r2

-- Push transitivity inside forall
-- forall over coercions.
opt_trans_rule is co1 co2
  | Just (cv1, visL1, _visR1, eta1, r1) <- splitForAllCo_co_maybe co1
  , Just (cv2, _visL2, visR2, eta2, r2) <- etaForAllCo_co_maybe co2
  = push_trans cv1 eta1 r1 cv2 eta2 r2 visL1 visR2

  | Just (cv2, _visL2, visR2, eta2, r2) <- splitForAllCo_co_maybe co2
  , Just (cv1, visL1, _visR1, eta1, r1) <- etaForAllCo_co_maybe co1
  = push_trans cv1 eta1 r1 cv2 eta2 r2 visL1 visR2

  where
  push_trans cv1 eta1 r1 cv2 eta2 r2 visL visR
    -- Given:
    --   co1 = /\ (cv1 : eta1) <visL, visM>. r1
    --   co2 = /\ (cv2 : eta2) <visM, visR>. r2
    -- Wanted:
    --   n1 = nth 2 eta1
    --   n2 = nth 3 eta1
    --   nco = /\ cv1 : (eta1;eta2). (r1; r2[cv2 |-> (sym n1);cv1;n2])
    = fireTransRule "EtaAllTy_co" co1 co2 $
      mkForAllCo cv1 visL visR (opt_trans is eta1 eta2) (opt_trans is' r1 r2')
    where
      is'  = is `extendInScopeSet` cv1
      role = coVarRole cv1
      eta1' = downgradeRole role Nominal eta1
      n1   = mkSelCo (SelTyCon 2 role) eta1'
      n2   = mkSelCo (SelTyCon 3 role) eta1'
      r2'  = substCo (zipCvSubst [cv2] [(mkSymCo n1) `mk_trans_co`
                                        (mkCoVarCo cv1) `mk_trans_co` n2])
                    r2

-- Push transitivity inside axioms
opt_trans_rule is co1 co2

  -- TrPushAxSym/TrPushSymAx
  -- Put this first!  Otherwise (#23619) we get
  --    newtype N a = MkN a
  --    axN :: forall a. N a ~ a
  -- Now consider (axN ty ; sym (axN ty))
  -- If we put TrPushSymAxR first, we'll get
  --    (axN ty ; sym (axN ty)) :: N ty ~ N ty -- Obviously Refl
  --    --> axN (sym (axN ty))  :: N ty ~ N ty -- Very stupid
  | Just (sym1, axr1, cos1) <- isAxiomCo_maybe co1
  , Just (sym2, axr2, cos2) <- isAxiomCo_maybe co2
  , axr1 == axr2
  , sym1 == flipSwap sym2
  , Just (tc, role, branch) <- coAxiomRuleBranch_maybe axr1
  , let qtvs   = coAxBranchTyVars branch ++ coAxBranchCoVars branch
        lhs    = mkTyConApp tc (coAxBranchLHS branch)
        rhs    = coAxBranchRHS branch
        pivot_tvs = exactTyCoVarsOfType (pickSwap sym2 lhs rhs)
  , all (`elemVarSet` pivot_tvs) qtvs
  = fireTransRule "TrPushAxSym" co1 co2 $
    if isSwapped sym2
       -- TrPushAxSym
    then liftCoSubstWith role qtvs (opt_transList is cos1 (map mkSymCo cos2)) lhs
       -- TrPushSymAx
    else liftCoSubstWith role qtvs (opt_transList is (map mkSymCo cos1) cos2) rhs

  -- See Note [Push transitivity inside axioms] and
  -- Note [Push transitivity inside newtype axioms only]
  -- TrPushSymAxR
  | Just (sym, axr, cos1) <- isAxiomCo_maybe co1
  , isSwapped sym
  , Just cos2 <- matchNewtypeBranch sym axr co2
  , let newAxInst = AxiomCo axr (opt_transList is (map mkSymCo cos2) cos1)
  = fireTransRule "TrPushSymAxR" co1 co2 $ SymCo newAxInst

  -- TrPushAxR
  | Just (sym, axr, cos1) <- isAxiomCo_maybe co1
  , notSwapped sym
  , Just cos2 <- matchNewtypeBranch sym axr co2
  , let newAxInst = AxiomCo axr (opt_transList is cos1 cos2)
  = fireTransRule "TrPushAxR" co1 co2 newAxInst

  -- TrPushSymAxL
  | Just (sym, axr, cos2) <- isAxiomCo_maybe co2
  , isSwapped sym
  , Just cos1 <- matchNewtypeBranch (flipSwap sym) axr co1
  , let newAxInst = AxiomCo axr (opt_transList is cos2 (map mkSymCo cos1))
  = fireTransRule "TrPushSymAxL" co1 co2 $ SymCo newAxInst

  -- TrPushAxL
  | Just (sym, axr, cos2) <- isAxiomCo_maybe co2
  , notSwapped sym
  , Just cos1 <- matchNewtypeBranch (flipSwap sym) axr co1
  , let newAxInst = AxiomCo axr (opt_transList is cos1 cos2)
  = fireTransRule "TrPushAxL" co1 co2 newAxInst


opt_trans_rule _ co1 co2        -- Identity rule
  | let ty1 = coercionLKind co1
        r   = coercionRole co1
        ty2 = coercionRKind co2
  , ty1 `eqType` ty2
  = fireTransRule "RedTypeDirRefl" co1 co2 $
    mkReflCo r ty2

opt_trans_rule _ _ _ = Nothing

-- See Note [EtaAppCo]
opt_trans_rule_app :: InScopeSet
                   -> Coercion   -- original left-hand coercion (printing only)
                   -> Coercion   -- original right-hand coercion (printing only)
                   -> Coercion   -- left-hand coercion "function"
                   -> [Coercion] -- left-hand coercion "args"
                   -> Coercion   -- right-hand coercion "function"
                   -> [Coercion] -- right-hand coercion "args"
                   -> Maybe Coercion
opt_trans_rule_app is orig_co1 orig_co2 co1a co1bs co2a co2bs
  | AppCo co1aa co1ab <- co1a
  , Just (co2aa, co2ab) <- etaAppCo_maybe co2a
  = opt_trans_rule_app is orig_co1 orig_co2 co1aa (co1ab:co1bs) co2aa (co2ab:co2bs)

  | AppCo co2aa co2ab <- co2a
  , Just (co1aa, co1ab) <- etaAppCo_maybe co1a
  = opt_trans_rule_app is orig_co1 orig_co2 co1aa (co1ab:co1bs) co2aa (co2ab:co2bs)

  | otherwise
  = assert (co1bs `equalLength` co2bs) $
    fireTransRule ("EtaApps:" ++ show (length co1bs)) orig_co1 orig_co2 $
    let rt1a = coercionRKind co1a

        lt2a = coercionLKind co2a
        rt2a = coercionRole  co2a

        rt1bs = map coercionRKind co1bs
        lt2bs = map coercionLKind co2bs
        rt2bs = map coercionRole co2bs

        kcoa = mkKindCo $ buildCoercion lt2a rt1a
        kcobs = map mkKindCo $ zipWith buildCoercion lt2bs rt1bs

        co2a'   = mkCoherenceLeftCo rt2a lt2a kcoa co2a
        co2bs'  = zipWith3 mkGReflLeftCo rt2bs lt2bs kcobs
        co2bs'' = zipWith mk_trans_co co2bs' co2bs
    in
    mkAppCos (opt_trans is co1a co2a')
             (zipWith (opt_trans is) co1bs co2bs'')

fireTransRule :: String -> Coercion -> Coercion -> Coercion -> Maybe Coercion
fireTransRule _rule _co1 _co2 res
  = Just res

{-
Note [Push transitivity inside axioms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
opt_trans_rule tries to push transitivity inside axioms to deal with cases like
the following:

    newtype N a = MkN a

    axN :: N a ~R# a

    covar :: a ~R# b
    co1 = axN <a> :: N a ~R# a
    co2 = axN <b> :: N b ~R# b

    co :: a ~R# b
    co = sym co1 ; N covar ; co2

When we are optimising co, we want to notice that the two axiom instantiations
cancel out. This is implemented by rules such as TrPushSymAxR, which transforms
    sym (axN <a>) ; N covar
into
    sym (axN covar)
so that TrPushSymAx can subsequently transform
    sym (axN covar) ; axN <b>
into
    covar
which is much more compact. In some perf test cases this kind of pattern can be
generated repeatedly during simplification, so it is very important we squash it
to stop coercions growing exponentially.  For more details see the paper:

    Evidence normalisation in System FC
    Dimitrios Vytiniotis and Simon Peyton Jones
    RTA'13, 2013
    https://www.microsoft.com/en-us/research/publication/evidence-normalization-system-fc-2/


Note [Push transitivity inside newtype axioms only]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The optimization described in Note [Push transitivity inside axioms] is possible
for both newtype and type family axioms.  However, for type family axioms it is
relatively common to have transitive sequences of axioms instantiations, for
example:

    data Nat = Zero | Suc Nat

    type family Index (n :: Nat) (xs :: [Type]) :: Type where
      Index Zero    (x : xs) = x
      Index (Suc n) (x : xs) = Index n xs

    axIndex :: { forall x::Type. forall xs::[Type]. Index Zero (x : xs) ~ x
               ; forall n::Nat. forall x::Type. forall xs::[Type]. Index (Suc n) (x : xs) ~ Index n xs }

    co :: Index (Suc (Suc Zero)) [a, b, c] ~ c
    co = axIndex[1] <Suc Zero> <a> <[b, c]>
       ; axIndex[1] <Zero> <b> <[c]>
       ; axIndex[0] <c> <[]>

Not only are there no cancellation opportunities here, but calling matchAxiom
repeatedly down the transitive chain is very expensive.  Hence we do not attempt
to push transitivity inside type family axioms.  See #8095, !9210 and related tickets.

This is implemented by opt_trans_rule checking that the axiom is for a newtype
constructor (i.e. not a type family).  Adding these guards substantially
improved performance (reduced bytes allocated by more than 10%) for the tests
CoOpt_Singletons, LargeRecord, T12227, T12545, T13386, T15703, T5030, T8095.

A side benefit is that we do not risk accidentally creating an ill-typed
coercion; see Note [Why call checkAxInstCo during optimisation].

There may exist programs that previously relied on pushing transitivity inside
type family axioms to avoid creating huge coercions, which will regress in
compile time performance as a result of this change.  We do not currently know
of any examples, but if any come to light we may need to reconsider this
behaviour.


Note [Why call checkAxInstCo during optimisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: The following is no longer relevant, because we no longer push transitivity
into type family axioms (Note [Push transitivity inside newtype axioms only]).
It is retained for reference in case we change this behaviour in the future.

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
that (Id Int) and (Id Bool) are SurelyApart, as they're headed by type
families. At the time of writing, I (Richard Eisenberg) couldn't think of
a way of detecting this any more efficient than just building the optimised
coercion and checking.

Note [EtaAppCo]
~~~~~~~~~~~~~~~
Suppose we're trying to optimize (co1a co1b ; co2a co2b). Ideally, we'd
like to rewrite this to (co1a ; co2a) (co1b ; co2b). The problem is that
the resultant coercions might not be well kinded. Here is an example (things
labeled with x don't matter in this example):

  k1 :: Type
  k2 :: Type

  a :: k1 -> Type
  b :: k1

  h :: k1 ~ k2

  co1a :: x1 ~ (a |> (h -> <Type>)
  co1b :: x2 ~ (b |> h)

  co2a :: a ~ x3
  co2b :: b ~ x4

First, convince yourself of the following:

  co1a co1b :: x1 x2 ~ (a |> (h -> <Type>)) (b |> h)
  co2a co2b :: a b   ~ x3 x4

  (a |> (h -> <Type>)) (b |> h) `eqType` a b

That last fact is due to Note [Non-trivial definitional equality] in GHC.Core.TyCo.Rep,
where we ignore coercions in types as long as two types' kinds are the same.
In our case, we meet this last condition, because

  (a |> (h -> <Type>)) (b |> h) :: Type
    and
  a b :: Type

So the input coercion (co1a co1b ; co2a co2b) is well-formed. But the
suggested output coercions (co1a ; co2a) and (co1b ; co2b) are not -- the
kinds don't match up.

The solution here is to twiddle the kinds in the output coercions. First, we
need to find coercions

  ak :: kind(a |> (h -> <Type>)) ~ kind(a)
  bk :: kind(b |> h)             ~ kind(b)

This can be done with mkKindCo and buildCoercion. The latter assumes two
types are identical modulo casts and builds a coercion between them.

Then, we build (co1a ; co2a |> sym ak) and (co1b ; co2b |> sym bk) as the
output coercions. These are well-kinded.

Also, note that all of this is done after accumulated any nested AppCo
parameters. This step is to avoid quadratic behavior in calling coercionKind.

The problem described here was first found in dependent/should_compile/dynamic-paper.

-}

-----------
swapSym :: SwapFlag -> (a,a) -> (a,a)
swapSym IsSwapped  (x,y) = (y,x)
swapSym NotSwapped (x,y) = (x,y)

wrapSym :: SwapFlag -> Coercion -> Coercion
wrapSym IsSwapped  co = mkSymCo co
wrapSym NotSwapped co = co

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
isAxiomCo_maybe :: Coercion -> Maybe (SwapFlag, CoAxiomRule, [Coercion])
-- We don't expect to see nested SymCo; and that lets us write a simple,
-- non-recursive function. (If we see a nested SymCo we'll just fail,
-- which is ok.)
isAxiomCo_maybe (SymCo (AxiomCo ax cos)) = Just (IsSwapped,  ax, cos)
isAxiomCo_maybe (AxiomCo ax cos)         = Just (NotSwapped, ax, cos)
isAxiomCo_maybe _                        = Nothing

matchNewtypeBranch :: SwapFlag -- IsSwapped = match LHS, NotSwapped = match RHS
                   -> CoAxiomRule
                   -> Coercion -> Maybe [Coercion]
matchNewtypeBranch sym axr co
  | Just (tc,branch) <- isNewtypeAxiomRule_maybe axr
  , CoAxBranch { cab_tvs = qtvs
               , cab_cvs = []   -- can't infer these, so fail if there are any
               , cab_roles = roles
               , cab_lhs = lhs
               , cab_rhs = rhs } <- branch
  , Just subst <- liftCoMatch (mkVarSet qtvs)
                              (pickSwap sym rhs (mkTyConApp tc lhs))
                              co
  , all (`isMappedByLC` subst) qtvs
  = zipWithM (liftCoSubstTyVar subst) roles qtvs

  | otherwise
  = Nothing

-------------
compatible_co :: Coercion -> Coercion -> Bool
-- Check whether (co1 . co2) will be well-kinded
compatible_co co1 co2
  = x1 `eqType` x2
  where
    x1 = coercionRKind co1
    x2 = coercionLKind co2

-------------
{-
etaForAllCo
~~~~~~~~~~~~~~~~~
(1) etaForAllCo_ty_maybe
Suppose we have

  g : all a1:k1.t1  ~  all a2:k2.t2

but g is *not* a ForAllCo. We want to eta-expand it. So, we do this:

  g' = all a1:(ForAllKindCo g).(InstCo g (a1 ~ a1 |> ForAllKindCo g))

Call the kind coercion h1 and the body coercion h2. We can see that

  h2 : t1 ~ t2[a2 |-> (a1 |> h1)]

According to the typing rule for ForAllCo, we get that

  g' : all a1:k1.t1  ~  all a1:k2.(t2[a2 |-> (a1 |> h1)][a1 |-> a1 |> sym h1])

or

  g' : all a1:k1.t1  ~  all a1:k2.(t2[a2 |-> a1])

as desired.

(2) etaForAllCo_co_maybe
Suppose we have

  g : all c1:(s1~s2). t1 ~ all c2:(s3~s4). t2

Similarly, we do this

  g' = all c1:h1. h2
     : all c1:(s1~s2). t1 ~ all c1:(s3~s4). t2[c2 |-> (sym eta1;c1;eta2)]
                                              [c1 |-> eta1;c1;sym eta2]

Here,

  h1   = mkSelCo Nominal 0 g       :: (s1~s2)~(s3~s4)
  eta1 = mkSelCo (SelTyCon 2 r) h1 :: (s1 ~ s3)
  eta2 = mkSelCo (SelTyCon 3 r) h1 :: (s2 ~ s4)
  h2   = mkInstCo g (cv1 ~ (sym eta1;c1;eta2))
-}
etaForAllCo_ty_maybe :: Coercion -> Maybe (TyVar, ForAllTyFlag, ForAllTyFlag, Coercion, Coercion)
-- Try to make the coercion be of form (forall tv:kind_co. co)
etaForAllCo_ty_maybe co
  | Just (tv, visL, visR, kind_co, r) <- splitForAllCo_ty_maybe co
  = Just (tv, visL, visR, kind_co, r)

  | (Pair ty1 ty2, role)  <- coercionKindRole co
  , Just (Bndr tv1 vis1, _) <- splitForAllForAllTyBinder_maybe ty1
  , isTyVar tv1
  , Just (Bndr tv2 vis2, _) <- splitForAllForAllTyBinder_maybe ty2
  , isTyVar tv2
  -- can't eta-expand at nominal role unless visibilities match
  , (role /= Nominal) || (vis1 `eqForAllVis` vis2)
  , let kind_co = mkSelCo SelForAll co
  = Just ( tv1, vis1, vis2, kind_co
         , mkInstCo co (mk_grefl_right_co Nominal (TyVarTy tv1) kind_co))

  | otherwise
  = Nothing

etaForAllCo_co_maybe :: Coercion -> Maybe (CoVar, ForAllTyFlag, ForAllTyFlag, Coercion, Coercion)
-- Try to make the coercion be of form (forall cv:kind_co. co)
etaForAllCo_co_maybe co
  | Just (cv, visL, visR, kind_co, r) <- splitForAllCo_co_maybe co
  = Just (cv, visL, visR, kind_co, r)

  | (Pair ty1 ty2, role)  <- coercionKindRole co
  , Just (Bndr cv1 vis1, _) <- splitForAllForAllTyBinder_maybe ty1
  , isCoVar cv1
  , Just (Bndr cv2 vis2, _) <- splitForAllForAllTyBinder_maybe ty2
  , isCoVar cv2
  -- can't eta-expand at nominal role unless visibilities match
  , (role /= Nominal)
  = let kind_co  = mkSelCo SelForAll co
        r        = coVarRole cv1
        l_co     = mkCoVarCo cv1
        kind_co' = downgradeRole r Nominal kind_co
        r_co     = mkSymCo (mkSelCo (SelTyCon 2 r) kind_co')
                   `mk_trans_co` l_co
                   `mk_trans_co` mkSelCo (SelTyCon 3 r) kind_co'
    in Just ( cv1, vis1, vis2, kind_co
            , mkInstCo co (mkProofIrrelCo Nominal kind_co l_co r_co))

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
-- into [ SelCo (SelTyCon 0)     g :: s1~t1
--      , ...
--      , SelCo (SelTyCon (n-1)) g :: sn~tn ]
etaTyConAppCo_maybe tc (TyConAppCo _ tc2 cos2)
  = assert (tc == tc2) $ Just cos2

etaTyConAppCo_maybe tc co
  | not (tyConMustBeSaturated tc)
  , (Pair ty1 ty2, r) <- coercionKindRole co
  , Just (tc1, tys1)  <- splitTyConApp_maybe ty1
  , Just (tc2, tys2)  <- splitTyConApp_maybe ty2
  , tc1 == tc2
  , isInjectiveTyCon tc r  -- See Note [SelCo and newtypes] in GHC.Core.TyCo.Rep
  , let n = length tys1
  , tys2 `lengthIs` n      -- This can fail in an erroneous program
                           -- E.g. T a ~# T a b
                           -- #14607
  = assert (tc == tc1) $
    Just (decomposeCo n co (tyConRolesX r tc1))
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

optForAllCoBndr :: LiftingContext -> SwapFlag
                -> TyCoVar -> Coercion -> (LiftingContext, TyCoVar, Coercion)
optForAllCoBndr env sym
  = substForAllCoBndrUsingLC sym (opt_co4 env sym False Nominal) env


{- **********************************************************************
%*                                                                      *
       Assertion-checking versions of functions in Coercion.hs
%*                                                                      *
%********************************************************************* -}

-- We can't check the assertions in the "main" functions of these
-- functions, because the assertions don't hold during zonking.
-- But they are fantastically helpful in finding bugs in the coercion
-- optimiser itself, so I have copied them here with assertions.

mk_trans_co :: HasDebugCallStack => Coercion -> Coercion -> Coercion
-- Do assertion checking in mk_trans_co
mk_trans_co co1 co2
  = assertPpr (coercionRKind co1 `eqType` coercionLKind co2)
              (vcat [ text "co1" <+> ppr co1
                    , text "co2" <+> ppr co2
                    , text "co1 kind" <+> ppr (coercionKind co1)
                    , text "co2 kind" <+> ppr (coercionKind co2)
                    , callStackDoc ]) $
    mkTransCo co1 co2

mk_coherence_right_co :: HasDebugCallStack => Role -> Type -> CoercionN -> Coercion -> Coercion
mk_coherence_right_co r ty co co2
  = assertGRefl ty co $
    mkCoherenceRightCo r ty co co2

assertGRefl :: HasDebugCallStack => Type -> Coercion -> r -> r
assertGRefl ty co res
  = assertPpr (typeKind ty `eqType` coercionLKind co)
              (vcat [ pp_ty "ty" ty
                    , pp_co "co" co
                    , callStackDoc ]) $
    res

mk_grefl_right_co :: Role -> Type -> CoercionN -> Coercion
mk_grefl_right_co r ty co
  = assertGRefl ty co $
    mkGReflRightCo r ty co

pp_co :: String -> Coercion -> SDoc
pp_co s co = text s <+> hang (ppr co) 2 (dcolon <+> ppr (coercionKind co))

pp_ty :: String -> Type -> SDoc
pp_ty s ty = text s <+> hang (ppr ty) 2 (dcolon <+> ppr (typeKind ty))

