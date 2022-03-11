-- (c) The University of Glasgow 2006

{-# LANGUAGE CPP #-}

module GHC.Core.Coercion.Opt
   ( optCoercion
   , checkAxInstCo
   , OptCoercionOpts (..)
   )
where

import GHC.Prelude

import GHC.Tc.Utils.TcType   ( exactTyCoVarsOfType )

import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Subst
import GHC.Core.Coercion
import GHC.Core.Type as Type hiding( substTyVarBndr, substTy )
import GHC.Core.TyCon
import GHC.Core.Coercion.Axiom
import GHC.Core.Unify

import GHC.Types.Var.Set
import GHC.Types.Var.Env

import GHC.Data.Pair
import GHC.Data.List.SetOps ( getNth )

import GHC.Utils.Outputable
import GHC.Utils.Constants (debugIsOn)
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Trace

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
(1) tv is a type variable
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
(g2 ; t2 ~ t2 |> sym h) in g, we'll get this result exactly.
This is bizarre,
though, because we're substituting a type variable with a coercion. However,
this operation already exists: it's called *lifting*, and defined in GHC.Core.Coercion.
We just need to enhance the lifting operation to be able to deal with
an ambient substitution, which is why a LiftingContext stores a TCvSubst.

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
-}

-- | Coercion optimisation options
newtype OptCoercionOpts = OptCoercionOpts
   { optCoercionEnabled :: Bool  -- ^ Enable coercion optimisation (reduce its size)
   }

optCoercion :: OptCoercionOpts -> TCvSubst -> Coercion -> NormalCo
-- ^ optCoercion applies a substitution to a coercion,
--   *and* optimises it to reduce its size
optCoercion opts env co
  | optCoercionEnabled opts = optCoercion' env co
  | otherwise               = substCo env co

optCoercion' :: TCvSubst -> Coercion -> NormalCo
optCoercion' env co
  | debugIsOn
  = let out_co = opt_co1 lc False co
        (Pair in_ty1  in_ty2,  in_role)  = coercionKindRole co
        (Pair out_ty1 out_ty2, out_role) = coercionKindRole out_co
    in
    assertPpr (substTyUnchecked env in_ty1 `eqType` out_ty1 &&
               substTyUnchecked env in_ty2 `eqType` out_ty2 &&
               in_role == out_role)
              ( text "optCoercion changed types!"
                 $$ hang (text "in_co:") 2 (ppr co)
                 $$ hang (text "in_ty1:") 2 (ppr in_ty1)
                 $$ hang (text "in_ty2:") 2 (ppr in_ty2)
                 $$ hang (text "out_co:") 2 (ppr out_co)
                 $$ hang (text "out_ty1:") 2 (ppr out_ty1)
                 $$ hang (text "out_ty2:") 2 (ppr out_ty2)
                 $$ hang (text "subst:") 2 (ppr env))
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
    assert (r == coercionRole co )
    let result = opt_co4 env sym rep r co in
    pprTrace "opt_co4_wrap }" (ppr co $$ text "---" $$ ppr result) $
    result
-}

opt_co4 env _   rep r (Refl ty)
  = assertPpr (r == Nominal)
              (text "Expected role:" <+> ppr r    $$
               text "Found role:" <+> ppr Nominal $$
               text "Type:" <+> ppr ty) $
    liftCoSubst (chooseRole rep r) env ty

opt_co4 env _   rep r (GRefl _r ty MRefl)
  = assertPpr (r == _r)
              (text "Expected role:" <+> ppr r $$
               text "Found role:" <+> ppr _r   $$
               text "Type:" <+> ppr ty) $
    liftCoSubst (chooseRole rep r) env ty

opt_co4 env sym  rep r (GRefl _r ty (MCo co))
  = assertPpr (r == _r)
              (text "Expected role:" <+> ppr r $$
               text "Found role:" <+> ppr _r   $$
               text "Type:" <+> ppr ty) $
    if isGReflCo co || isGReflCo co'
    then liftCoSubst r' env ty
    else wrapSym sym $ mkCoherenceRightCo r' ty' co' (liftCoSubst r' env ty)
  where
    r'  = chooseRole rep r
    ty' = substTy (lcSubstLeft env) ty
    co' = opt_co4 env False False Nominal co

opt_co4 env sym rep r (SymCo co)  = opt_co4_wrap env (not sym) rep r co
  -- surprisingly, we don't have to do anything to the env here. This is
  -- because any "lifting" substitutions in the env are tied to ForAllCos,
  -- which treat their left and right sides differently. We don't want to
  -- exchange them.

opt_co4 env sym rep r g@(TyConAppCo _r tc cos)
  = assert (r == _r) $
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

opt_co4 env sym rep r (FunCo _r cow co1 co2)
  = assert (r == _r) $
    if rep
    then mkFunCo Representational cow' co1' co2'
    else mkFunCo r cow' co1' co2'
  where
    co1' = opt_co4_wrap env sym rep r co1
    co2' = opt_co4_wrap env sym rep r co2
    cow' = opt_co1 env sym cow

opt_co4 env sym rep r (CoVarCo cv)
  | Just co <- lookupCoVar (lcTCvSubst env) cv
  = opt_co4_wrap (zapLiftingContext env) sym rep r co

  | ty1 `eqType` ty2   -- See Note [Optimise CoVarCo to Refl]
  = mkReflCo (chooseRole rep r) ty1

  | otherwise
  = assert (isCoVar cv1 )
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

opt_co4 _ _ _ _ (HoleCo h)
  = pprPanic "opt_univ fell into a hole" (ppr h)

opt_co4 env sym rep r (AxiomInstCo con ind cos)
    -- Do *not* push sym inside top-level axioms
    -- e.g. if g is a top-level axiom
    --   g a : f a ~ a
    -- then (sym (g ty)) /= g (sym ty) !!
  = assert (r == coAxiomRole con )
    wrapRole rep (coAxiomRole con) $
    wrapSym sym $
                       -- some sub-cos might be P: use opt_co2
                       -- See Note [Optimising coercion optimisation]
    AxiomInstCo con ind (zipWith (opt_co2 env False)
                                 (coAxBranchRoles (coAxiomNthBranch con ind))
                                 cos)
      -- Note that the_co does *not* have sym pushed into it

opt_co4 env sym rep r (UnivCo prov _r t1 t2)
  = assert (r == _r )
    opt_univ env sym prov (chooseRole rep r) t1 t2

opt_co4 env sym rep r (TransCo co1 co2)
                      -- sym (g `o` h) = sym h `o` sym g
  | sym       = opt_trans in_scope co2' co1'
  | otherwise = opt_trans in_scope co1' co2'
  where
    co1' = opt_co4_wrap env sym rep r co1
    co2' = opt_co4_wrap env sym rep r co2
    in_scope = lcInScopeSet env

opt_co4 env _sym rep r (NthCo _r n co)
  | Just (ty, _) <- isReflCo_maybe co
  , Just (_tc, args) <- assert (r == _r )
                        splitTyConApp_maybe ty
  = liftCoSubst (chooseRole rep r) env (args `getNth` n)

  | Just (ty, _) <- isReflCo_maybe co
  , n == 0
  , Just (tv, _) <- splitForAllTyCoVar_maybe ty
      -- works for both tyvar and covar
  = liftCoSubst (chooseRole rep r) env (varType tv)

opt_co4 env sym rep r (NthCo r1 n (TyConAppCo _ _ cos))
  = assert (r == r1 )
    opt_co4_wrap env sym rep r (cos `getNth` n)

-- see the definition of GHC.Builtin.Types.Prim.funTyCon
opt_co4 env sym rep r (NthCo r1 n (FunCo _r2 w co1 co2))
  = assert (r == r1 )
    opt_co4_wrap env sym rep r (mkNthCoFunCo n w co1 co2)

opt_co4 env sym rep r (NthCo _r n (ForAllCo _ eta _))
      -- works for both tyvar and covar
  = assert (r == _r )
    assert (n == 0 )
    opt_co4_wrap env sym rep Nominal eta

opt_co4 env sym rep r (NthCo _r n co)
  | Just nth_co <- case co' of
      TyConAppCo _ _ cos -> Just (cos `getNth` n)
      FunCo _ w co1 co2  -> Just (mkNthCoFunCo n w co1 co2)
      ForAllCo _ eta _   -> Just eta
      _                  -> Nothing
  = if rep && (r == Nominal)
      -- keep propagating the SubCo
    then opt_co4_wrap (zapLiftingContext env) False True Nominal nth_co
    else nth_co

  | otherwise
  = wrapRole rep r $ NthCo r n co'
  where
    co' = opt_co1 env sym co

opt_co4 env sym rep r (LRCo lr co)
  | Just pr_co <- splitAppCo_maybe co
  = assert (r == Nominal )
    opt_co4_wrap env sym rep Nominal (pick_lr lr pr_co)
  | Just pr_co <- splitAppCo_maybe co'
  = assert (r == Nominal) $
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
  | Just (tv, kind_co, co_body) <- splitForAllCo_ty_maybe co1
  = opt_co4_wrap (extendLiftingContext env tv
                    (mkCoherenceRightCo Nominal t2 (mkSymCo kind_co) sym_arg))
                   -- mkSymCo kind_co :: k1 ~ k2
                   -- sym_arg :: (t1 :: k1) ~ (t2 :: k2)
                   -- tv |-> (t1 :: k1) ~ (((t2 :: k2) |> (sym kind_co)) :: k1)
                 sym rep r co_body

    -- forall over coercion...
  | Just (cv, kind_co, co_body) <- splitForAllCo_co_maybe co1
  , CoercionTy h1 <- t1
  , CoercionTy h2 <- t2
  = let new_co = mk_new_co cv (opt_co4_wrap env sym False Nominal kind_co) h1 h2
    in opt_co4_wrap (extendLiftingContext env cv new_co) sym rep r co_body

    -- See if it is a forall after optimization
    -- If so, do an inefficient one-variable substitution, then re-optimize

    -- forall over type...
  | Just (tv', kind_co', co_body') <- splitForAllCo_ty_maybe co1'
  = opt_co4_wrap (extendLiftingContext (zapLiftingContext env) tv'
                    (mkCoherenceRightCo Nominal t2' (mkSymCo kind_co') arg'))
            False False r' co_body'

    -- forall over coercion...
  | Just (cv', kind_co', co_body') <- splitForAllCo_co_maybe co1'
  , CoercionTy h1' <- t1'
  , CoercionTy h2' <- t2'
  = let new_co = mk_new_co cv' kind_co' h1' h2'
    in opt_co4_wrap (extendLiftingContext (zapLiftingContext env) cv' new_co)
                    False False r' co_body'

  | otherwise = InstCo co1' arg'
  where
    co1'    = opt_co4_wrap env sym rep r co1
    r'      = chooseRole rep r
    arg'    = opt_co4_wrap env sym False Nominal arg
    sym_arg = wrapSym sym arg'

    -- Performance note: don't be alarmed by the two calls to coercionKind
    -- here, as only one call to coercionKind is actually demanded per guard.
    -- t1/t2 are used when checking if co1 is a forall, and t1'/t2' are used
    -- when checking if co1' (i.e., co1 post-optimization) is a forall.
    --
    -- t1/t2 must come from sym_arg, not arg', since it's possible that arg'
    -- might have an extra Sym at the front (after being optimized) that co1
    -- lacks, so we need to use sym_arg to balance the number of Syms. (#15725)
    Pair t1  t2  = coercionKind sym_arg
    Pair t1' t2' = coercionKind arg'

    mk_new_co cv kind_co h1 h2
      = let -- h1 :: (t1 ~ t2)
            -- h2 :: (t3 ~ t4)
            -- kind_co :: (t1 ~ t2) ~ (t3 ~ t4)
            -- n1 :: t1 ~ t3
            -- n2 :: t2 ~ t4
            -- new_co = (h1 :: t1 ~ t2) ~ ((n1;h2;sym n2) :: t1 ~ t2)
            r2  = coVarRole cv
            kind_co' = downgradeRole r2 Nominal kind_co
            n1 = mkNthCo r2 2 kind_co'
            n2 = mkNthCo r2 3 kind_co'
         in mkProofIrrelCo Nominal (Refl (coercionType h1)) h1
                           (n1 `mkTransCo` h2 `mkTransCo` (mkSymCo n2))

opt_co4 env sym _rep r (KindCo co)
  = assert (r == Nominal) $
    let kco' = promoteCoercion co in
    case kco' of
      KindCo co' -> promoteCoercion (opt_co1 env sym co')
      _          -> opt_co4_wrap env sym False Nominal kco'
  -- This might be able to be optimized more to do the promotion
  -- and substitution/optimization at the same time

opt_co4 env sym _ r (SubCo co)
  = assert (r == Representational) $
    opt_co4_wrap env sym True Nominal co

-- This could perhaps be optimized more.
opt_co4 env sym rep r (AxiomRuleCo co cs)
  = assert (r == coaxrRole co) $
    wrapRole rep r $
    wrapSym sym $
    AxiomRuleCo co (zipWith (opt_co2 env False) (coaxrAsmpRoles co) cs)

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
opt_phantom :: LiftingContext -> SymFlag -> Coercion -> NormalCo
opt_phantom env sym co
  = opt_univ env sym (PhantomProv (mkKindCo co)) Phantom ty1 ty2
  where
    Pair ty1 ty2 = coercionKind co

{- Note [Differing kinds]
   ~~~~~~~~~~~~~~~~~~~~~~
The two types may not have the same kind (although that would be very unusual).
But even if they have the same kind, and the same type constructor, the number
of arguments in a `CoTyConApp` can differ. Consider

  Any :: forall k. k

  Any * Int                      :: *
  Any (*->*) Maybe Int  :: *

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
  , isInjectiveTyCon tc1 role  -- see Note [opt_univ needs injectivity]
  , equalLength tys1 tys2 -- see Note [Differing kinds]
      -- NB: prov must not be the two interesting ones (ProofIrrel & Phantom);
      -- Phantom is already taken care of, and ProofIrrel doesn't relate tyconapps
  = let roles    = tyConRolesX role tc1
        arg_cos  = zipWith3 (mkUnivCo prov') roles tys1 tys2
        arg_cos' = zipWith (opt_co4 env sym False) roles arg_cos
    in
    mkTyConAppCo role tc1 arg_cos'

  -- can't optimize the AppTy case because we can't build the kind coercions.

  | Just (tv1, ty1) <- splitForAllTyVar_maybe oty1
  , Just (tv2, ty2) <- splitForAllTyVar_maybe oty2
      -- NB: prov isn't interesting here either
  = let k1   = tyVarKind tv1
        k2   = tyVarKind tv2
        eta  = mkUnivCo prov' Nominal k1 k2
          -- eta gets opt'ed soon, but not yet.
        ty2' = substTyWith [tv2] [TyVarTy tv1 `mkCastTy` eta] ty2

        (env', tv1', eta') = optForAllCoBndr env sym tv1 eta
    in
    mkForAllCo tv1' eta' (opt_univ env' sym prov' role ty1 ty2')

  | Just (cv1, ty1) <- splitForAllCoVar_maybe oty1
  , Just (cv2, ty2) <- splitForAllCoVar_maybe oty2
      -- NB: prov isn't interesting here either
  = let k1    = varType cv1
        k2    = varType cv2
        r'    = coVarRole cv1
        eta   = mkUnivCo prov' Nominal k1 k2
        eta_d = downgradeRole r' Nominal eta
          -- eta gets opt'ed soon, but not yet.
        n_co  = (mkSymCo $ mkNthCo r' 2 eta_d) `mkTransCo`
                (mkCoVarCo cv1) `mkTransCo`
                (mkNthCo r' 3 eta_d)
        ty2'  = substTyWithCoVars [cv2] [n_co] ty2

        (env', cv1', eta') = optForAllCoBndr env sym cv1 eta
    in
    mkForAllCo cv1' eta' (opt_univ env' sym prov' role ty1 ty2')

  | otherwise
  = let ty1 = substTyUnchecked (lcSubstLeft  env) oty1
        ty2 = substTyUnchecked (lcSubstRight env) oty2
        (a, b) | sym       = (ty2, ty1)
               | otherwise = (ty1, ty2)
    in
    mkUnivCo prov' role a b

  where
    prov' = case prov of
#if __GLASGOW_HASKELL__ < 901
-- This alt is redundant with the first match of the FunDef
      PhantomProv kco    -> PhantomProv $ opt_co4_wrap env sym False Nominal kco
#endif
      ProofIrrelProv kco -> ProofIrrelProv $ opt_co4_wrap env sym False Nominal kco
      PluginProv _       -> prov
      CorePrepProv _     -> prov

-------------
opt_transList :: HasDebugCallStack => InScopeSet -> [NormalCo] -> [NormalCo] -> [NormalCo]
opt_transList is = zipWithEqual "opt_transList" (opt_trans is)
  -- The input lists must have identical length.

opt_trans :: InScopeSet -> NormalCo -> NormalCo -> NormalCo
opt_trans is co1 co2
  | isReflCo co1 = co2
    -- optimize when co1 is a Refl Co
  | otherwise    = opt_trans1 is co1 co2

opt_trans1 :: InScopeSet -> NormalNonIdCo -> NormalCo -> NormalCo
-- First arg is not the identity
opt_trans1 is co1 co2
  | isReflCo co2 = co1
    -- optimize when co2 is a Refl Co
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

opt_trans_rule is in_co1@(GRefl r1 t1 (MCo co1)) in_co2@(GRefl r2 _ (MCo co2))
  = assert (r1 == r2) $
    fireTransRule "GRefl" in_co1 in_co2 $
    mkGReflRightCo r1 t1 (opt_trans is co1 co2)

-- Push transitivity through matching destructors
opt_trans_rule is in_co1@(NthCo r1 d1 co1) in_co2@(NthCo r2 d2 co2)
  | d1 == d2
  , coercionRole co1 == coercionRole co2
  , co1 `compatible_co` co2
  = assert (r1 == r2) $
    fireTransRule "PushNth" in_co1 in_co2 $
    mkNthCo r1 d1 (opt_trans is co1 co2)

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
  = assert (r1 == r2) $
    fireTransRule "UnivCo" in_co1 in_co2 $
    mkUnivCo prov' r1 tyl1 tyr2
  where
    -- if the provenances are different, opt'ing will be very confusing
    opt_trans_prov (PhantomProv kco1)    (PhantomProv kco2)
      = Just $ PhantomProv $ opt_trans is kco1 kco2
    opt_trans_prov (ProofIrrelProv kco1) (ProofIrrelProv kco2)
      = Just $ ProofIrrelProv $ opt_trans is kco1 kco2
    opt_trans_prov (PluginProv str1)     (PluginProv str2)     | str1 == str2 = Just p1
    opt_trans_prov _ _ = Nothing

-- Push transitivity down through matching top-level constructors.
opt_trans_rule is in_co1@(TyConAppCo r1 tc1 cos1) in_co2@(TyConAppCo r2 tc2 cos2)
  | tc1 == tc2
  = assert (r1 == r2) $
    fireTransRule "PushTyConApp" in_co1 in_co2 $
    mkTyConAppCo r1 tc1 (opt_transList is cos1 cos2)

opt_trans_rule is in_co1@(FunCo r1 w1 co1a co1b) in_co2@(FunCo r2 w2 co2a co2b)
  = assert (r1 == r2) $   -- Just like the TyConAppCo/TyConAppCo case
    fireTransRule "PushFun" in_co1 in_co2 $
    mkFunCo r1 (opt_trans is w1 w2) (opt_trans is co1a co2a) (opt_trans is co1b co2b)

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
  | Just (tv1, eta1, r1) <- splitForAllCo_ty_maybe co1
  , Just (tv2, eta2, r2) <- etaForAllCo_ty_maybe co2
  = push_trans tv1 eta1 r1 tv2 eta2 r2

  | Just (tv2, eta2, r2) <- splitForAllCo_ty_maybe co2
  , Just (tv1, eta1, r1) <- etaForAllCo_ty_maybe co1
  = push_trans tv1 eta1 r1 tv2 eta2 r2

  where
  push_trans tv1 eta1 r1 tv2 eta2 r2
    -- Given:
    --   co1 = /\ tv1 : eta1. r1
    --   co2 = /\ tv2 : eta2. r2
    -- Wanted:
    --   /\tv1 : (eta1;eta2).  (r1; r2[tv2 |-> tv1 |> eta1])
    = fireTransRule "EtaAllTy_ty" co1 co2 $
      mkForAllCo tv1 (opt_trans is eta1 eta2) (opt_trans is' r1 r2')
    where
      is' = is `extendInScopeSet` tv1
      r2' = substCoWithUnchecked [tv2] [mkCastTy (TyVarTy tv1) eta1] r2

-- Push transitivity inside forall
-- forall over coercions.
opt_trans_rule is co1 co2
  | Just (cv1, eta1, r1) <- splitForAllCo_co_maybe co1
  , Just (cv2, eta2, r2) <- etaForAllCo_co_maybe co2
  = push_trans cv1 eta1 r1 cv2 eta2 r2

  | Just (cv2, eta2, r2) <- splitForAllCo_co_maybe co2
  , Just (cv1, eta1, r1) <- etaForAllCo_co_maybe co1
  = push_trans cv1 eta1 r1 cv2 eta2 r2

  where
  push_trans cv1 eta1 r1 cv2 eta2 r2
    -- Given:
    --   co1 = /\ cv1 : eta1. r1
    --   co2 = /\ cv2 : eta2. r2
    -- Wanted:
    --   n1 = nth 2 eta1
    --   n2 = nth 3 eta1
    --   nco = /\ cv1 : (eta1;eta2). (r1; r2[cv2 |-> (sym n1);cv1;n2])
    = fireTransRule "EtaAllTy_co" co1 co2 $
      mkForAllCo cv1 (opt_trans is eta1 eta2) (opt_trans is' r1 r2')
    where
      is'  = is `extendInScopeSet` cv1
      role = coVarRole cv1
      eta1' = downgradeRole role Nominal eta1
      n1   = mkNthCo role 2 eta1'
      n2   = mkNthCo role 3 eta1'
      r2'  = substCo (zipCvSubst [cv2] [(mkSymCo n1) `mkTransCo`
                                        (mkCoVarCo cv1) `mkTransCo` n2])
                    r2

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
        co2bs'' = zipWith mkTransCo co2bs' co2bs
    in
    mkAppCos (opt_trans is co1a co2a')
             (zipWith (opt_trans is) co1bs co2bs'')

fireTransRule :: String -> Coercion -> Coercion -> Coercion -> Maybe Coercion
fireTransRule _rule _co1 _co2 res
  = Just res

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
"GHC.Core.FamInstEnv".

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

-- | Check to make sure that an AxInstCo is internally consistent.
-- Returns the conflicting branch, if it exists
-- See Note [Conflict checking with AxiomInstCo]
checkAxInstCo :: Coercion -> Maybe CoAxBranch
-- defined here to avoid dependencies in GHC.Core.Coercion
-- If you edit this function, you may need to update the GHC formalism
-- See Note [GHC Formalism] in GHC.Core.Lint
checkAxInstCo (AxiomInstCo ax ind cos)
  = let branch       = coAxiomNthBranch ax ind
        tvs          = coAxBranchTyVars branch
        cvs          = coAxBranchCoVars branch
        incomps      = coAxBranchIncomps branch
        (tys, cotys) = splitAtList tvs (map coercionLKind cos)
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
         -- See Note [Apartness] in GHC.Core.FamInstEnv
      | SurelyApart <- tcUnifyTysFG alwaysBindFun flat lhs_incomp
      = check_no_conflict flat rest
      | otherwise
      = Just b
checkAxInstCo _ = Nothing


-----------
wrapSym :: SymFlag -> Coercion -> Coercion
wrapSym sym co | sym       = mkSymCo co
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

  h1   = mkNthCo Nominal 0 g :: (s1~s2)~(s3~s4)
  eta1 = mkNthCo r 2 h1      :: (s1 ~ s3)
  eta2 = mkNthCo r 3 h1      :: (s2 ~ s4)
  h2   = mkInstCo g (cv1 ~ (sym eta1;c1;eta2))
-}
etaForAllCo_ty_maybe :: Coercion -> Maybe (TyVar, Coercion, Coercion)
-- Try to make the coercion be of form (forall tv:kind_co. co)
etaForAllCo_ty_maybe co
  | Just (tv, kind_co, r) <- splitForAllCo_ty_maybe co
  = Just (tv, kind_co, r)

  | Pair ty1 ty2  <- coercionKind co
  , Just (tv1, _) <- splitForAllTyVar_maybe ty1
  , isForAllTy_ty ty2
  , let kind_co = mkNthCo Nominal 0 co
  = Just ( tv1, kind_co
         , mkInstCo co (mkGReflRightCo Nominal (TyVarTy tv1) kind_co))

  | otherwise
  = Nothing

etaForAllCo_co_maybe :: Coercion -> Maybe (CoVar, Coercion, Coercion)
-- Try to make the coercion be of form (forall cv:kind_co. co)
etaForAllCo_co_maybe co
  | Just (cv, kind_co, r) <- splitForAllCo_co_maybe co
  = Just (cv, kind_co, r)

  | Pair ty1 ty2  <- coercionKind co
  , Just (cv1, _) <- splitForAllCoVar_maybe ty1
  , isForAllTy_co ty2
  = let kind_co  = mkNthCo Nominal 0 co
        r        = coVarRole cv1
        l_co     = mkCoVarCo cv1
        kind_co' = downgradeRole r Nominal kind_co
        r_co     = (mkSymCo (mkNthCo r 2 kind_co')) `mkTransCo`
                   l_co `mkTransCo`
                   (mkNthCo r 3 kind_co')
    in Just ( cv1, kind_co
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
-- into [ Nth 0 g :: s1~t1, ..., Nth (n-1) g :: sn~tn ]
etaTyConAppCo_maybe tc (TyConAppCo _ tc2 cos2)
  = assert (tc == tc2) $ Just cos2

etaTyConAppCo_maybe tc co
  | not (mustBeSaturated tc)
  , (Pair ty1 ty2, r) <- coercionKindRole co
  , Just (tc1, tys1)  <- splitTyConApp_maybe ty1
  , Just (tc2, tys2)  <- splitTyConApp_maybe ty2
  , tc1 == tc2
  , isInjectiveTyCon tc r  -- See Note [NthCo and newtypes] in GHC.Core.TyCo.Rep
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

optForAllCoBndr :: LiftingContext -> Bool
                -> TyCoVar -> Coercion -> (LiftingContext, TyCoVar, Coercion)
optForAllCoBndr env sym
  = substForAllCoBndrUsingLC sym (opt_co4_wrap env sym False Nominal) env
