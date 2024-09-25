{-# LANGUAGE FlexibleContexts, RecursiveDo #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

module GHC.Tc.Utils.Instantiate (
     topSkolemise, skolemiseRequired,
     topInstantiate,
     instantiateSigma,
     instCall, instDFunType, instStupidTheta, instTyVarsWith,
     newWanted, newWanteds,

     tcInstType, tcInstTypeBndrs,
     tcSkolemiseInvisibleBndrs,
     tcInstSkolTyVars, tcInstSkolTyVarsX, tcInstSkolTyVarBndrsX,
     tcSkolDFunType, tcSuperSkolTyVars, tcInstSuperSkolTyVarsX,

     freshenTyVarBndrs, freshenCoVarBndrsX,

     tcInstInvisibleTyBindersN, tcInstInvisibleTyBinders, tcInstInvisibleTyBinder,

     newOverloadedLit, mkOverLit,

     newClsInst, newFamInst,
     tcGetInsts, tcGetInstEnvs, getOverlapFlag,
     tcExtendLocalInstEnv,
     instCallConstraints, newMethodFromName,
     tcSyntaxName,

     -- Simple functions over evidence variables
     tyCoVarsOfWC,
     tyCoVarsOfCt, tyCoVarsOfCts,
  ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env

import GHC.Builtin.Types  ( integerTyConName )
import GHC.Builtin.Names

import GHC.Hs
import GHC.Hs.Syn.Type   ( hsLitType )

import GHC.Core.InstEnv
import GHC.Core.FamInstEnv
import GHC.Core ( isOrphan ) -- For the Coercion constructor
import GHC.Core.Type
import GHC.Core.TyCo.Ppr ( debugPprType )
import GHC.Core.Class( Class )
import GHC.Core.Coercion.Axiom

import {-# SOURCE #-}   GHC.Tc.Gen.Expr( tcCheckPolyExpr, tcSyntaxOp )
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Env
import GHC.Tc.Types.Evidence
import GHC.Tc.Instance.FunDeps
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep_syntactic )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.TcType
import GHC.Tc.Errors.Types
import GHC.Tc.Zonk.Monad ( ZonkM )

import GHC.Types.Id.Make( mkDictFunId )
import GHC.Types.Basic ( TypeOrKind(..), Arity, VisArity )
import GHC.Types.Error
import GHC.Types.SourceText
import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Var.Env
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Var
import qualified GHC.LanguageExtensions as LangExt

import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Utils.Outputable
import GHC.Utils.Unique (sameUnique)

import GHC.Unit.State
import GHC.Unit.External
import GHC.Unit.Module.Warnings

import Data.List ( mapAccumL )
import qualified Data.List.NonEmpty as NE
import Control.Monad( when, unless )
import Data.Function ( on )

{-
************************************************************************
*                                                                      *
                Creating and emitting constraints
*                                                                      *
************************************************************************
-}

newMethodFromName
  :: CtOrigin              -- ^ why do we need this?
  -> Name                  -- ^ name of the method
  -> [TcRhoType]           -- ^ types with which to instantiate the class
  -> TcM (HsExpr GhcTc)
-- ^ Used when 'Name' is the wired-in name for a wired-in class method,
-- so the caller knows its type for sure, which should be of form
--
-- > forall a. C a => <blah>
--
-- 'newMethodFromName' is supposed to instantiate just the outer
-- type variable and constraint

newMethodFromName origin name ty_args
  = do { id <- tcLookupId name
              -- Use tcLookupId not tcLookupGlobalId; the method is almost
              -- always a class op, but with -XRebindableSyntax GHC is
              -- meant to find whatever thing is in scope, and that may
              -- be an ordinary function.

       ; let ty = piResultTys (idType id) ty_args
             (theta, _caller_knows_this) = tcSplitPhiTy ty
       ; wrap <- assert (not (isForAllTy ty) && isSingleton theta) $
                 instCall origin ty_args theta

       ; return (mkHsWrap wrap (HsVar noExtField (noLocA id))) }

{-
************************************************************************
*                                                                      *
         Instantiation and skolemisation
*                                                                      *
************************************************************************

Note [Skolemisation]
~~~~~~~~~~~~~~~~~~~~
topSkolemise decomposes and skolemises a type, returning a type
with no top level foralls or (=>).

Examples:

  topSkolemise (forall a. Ord a => a -> a)
    =  ( wp, [a], [d:Ord a], a->a )
    where
      wp = /\a. \(d:Ord a). <hole> a d

For nested foralls, see Note [Skolemisation en-bloc]

In general,
  if      topSkolemise ty = (wrap, tvs, evs, rho)
    and   e :: rho
  then    wrap e :: ty
    and   'wrap' binds {tvs, evs}

Note [Skolemisation en-bloc]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this case:

  topSkolemise  (forall a. Ord a => forall b. Eq b => a->b->b)

We /could/ return just
  (wp, [a], [d:Ord a, forall b. Eq b => a -> b -> b)

But in fact we skolemise "en-bloc", looping around (in `topSkolemise` for
example) to skolemise the (forall b. Eq b =>).  So in fact

  topSkolemise  (forall a. Ord a => forall b. Eq b => a->b->b)
    =  ( wp, [a,b], [d1:Ord a,d2:Eq b], a->b->b )
    where
      wp = /\a.\(d1:Ord a)./\b.\(d2:Ord b). <hole> a d1 b d2

This applies regardless of DeepSubsumption.

Why do we do this "en-bloc" loopy thing?  It is /nearly/ just an optimisation.
But not quite!  At the call site of `topSkolemise` (and its cousins) we
use `checkConstraints` to gather constraints and build an implication
constraint.   So skolemising just one level at a time would lead to nested
implication constraints. That is a bit less efficient, but there is /also/ a small
user-visible effect: see Note [Let-bound skolems] in GHC.Tc.Solver.InertSet.
Specifically, consider

   forall a. Eq a => forall b. (a ~ [b]) => blah

If we skolemise en-bloc, the equality (a~[b]) is like a let-binding and we
don't treat it like a GADT pattern match, limiting unification. With nested
implications, the inner one would be treated as having-given-equalities.

This is also relevant when Required foralls are involved; see #24810, and
the loop in `skolemiseRequired`.
-}

topSkolemise :: SkolemInfo
             -> TcSigmaType
             -> TcM ( HsWrapper
                    , [(Name,TcInvisTVBinder)]     -- All skolemised variables
                    , [EvVar]                      -- All "given"s
                    , TcRhoType )
-- See Note [Skolemisation]
topSkolemise skolem_info ty
  = go init_subst idHsWrapper [] [] ty
  where
    init_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType ty))

    -- Why recursive?  See Note [Skolemisation en-bloc]
    go subst wrap tv_prs ev_vars ty
      | (bndrs, theta, inner_ty) <- tcSplitSigmaTyBndrs ty
      , let tvs = binderVars bndrs
      , not (null tvs && null theta)
      = do { (subst', bndrs1) <- tcInstSkolTyVarBndrsX skolem_info subst bndrs
           ; let tvs1 = binderVars bndrs1
           ; traceTc "topSkol" (vcat [ ppr tvs <+> vcat (map (ppr . getSrcSpan) tvs)
                                     , ppr tvs1 <+> vcat (map (ppr . getSrcSpan) tvs1) ])
           ; ev_vars1 <- newEvVars (substTheta subst' theta)
           ; go subst'
                (wrap <.> mkWpTyLams tvs1 <.> mkWpEvLams ev_vars1)
                (tv_prs ++ (map tyVarName tvs `zip` bndrs1))
                (ev_vars ++ ev_vars1)
                inner_ty }

      | otherwise
      = return (wrap, tv_prs, ev_vars, substTy subst ty)
        -- substTy is a quick no-op on an empty substitution

skolemiseRequired :: SkolemInfo -> VisArity -> TcSigmaType
                  -> TcM (VisArity, HsWrapper, [Name], [ForAllTyBinder], [EvVar], TcRhoType)
-- Skolemise up to N required (visible) binders,
--    plus any invisible ones "in the way",
--    /and/ any trailing invisible ones.
-- So the result has no top-level invisible quantifiers.
-- Return the depleted arity.
skolemiseRequired skolem_info n_req sigma
  = go n_req init_subst idHsWrapper [] [] [] sigma
  where
    init_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType sigma))

    -- Why recursive?  See Note [Skolemisation en-bloc]
    go n_req subst wrap acc_nms acc_bndrs ev_vars ty
      | (n_req', bndrs, inner_ty) <- tcSplitForAllTyVarsReqTVBindersN n_req ty
      , not (null bndrs)
      = do { (subst', bndrs1) <- tcInstSkolTyVarBndrsX skolem_info subst bndrs
           ; let tvs1 = binderVars bndrs1
                 -- fix_up_vis: see Note [Required foralls in Core]
                 --             in GHC.Core.TyCo.Rep
                 fix_up_vis | n_req == n_req'
                            = idHsWrapper
                            | otherwise
                            = mkWpForAllCast bndrs1 (substTy subst' inner_ty)
           ; go n_req' subst'
                (wrap <.> fix_up_vis <.> mkWpTyLams tvs1)
                (acc_nms   ++ map (tyVarName . binderVar) bndrs)
                (acc_bndrs ++ bndrs1)
                ev_vars
                inner_ty }

      | (theta, inner_ty) <- tcSplitPhiTy ty
      , not (null theta)
      = do { ev_vars1 <- newEvVars (substTheta subst theta)
           ; go n_req subst
                (wrap <.> mkWpEvLams ev_vars1)
                acc_nms
                acc_bndrs
                (ev_vars ++ ev_vars1)
                inner_ty }

      | otherwise
      = return (n_req, wrap, acc_nms, acc_bndrs, ev_vars, substTy subst ty)
        -- substTy is a quick no-op on an empty substitution

topInstantiate :: CtOrigin -> TcSigmaType -> TcM (HsWrapper, TcRhoType)
-- Instantiate outer invisible binders (both Inferred and Specified)
-- If    top_instantiate ty = (wrap, inner_ty)
-- then  wrap :: inner_ty "->" ty
-- NB: returns a type with no (=>),
--     and no invisible forall at the top
topInstantiate orig sigma
  | (tvs,   body1) <- tcSplitSomeForAllTyVars isInvisibleForAllTyFlag sigma
  , (theta, body2) <- tcSplitPhiTy body1
  , not (null tvs && null theta)
  = do { (_, wrap1, body3) <- instantiateSigma orig noConcreteTyVars tvs theta body2
           -- Why 'noConcreteTyVars' here?
           -- See Note [Representation-polymorphism checking built-ins]
           -- in GHC.Tc.Utils.Concrete.

       -- Loop, to account for types like
       --       forall a. Num a => forall b. Ord b => ...
       ; (wrap2, body4) <- topInstantiate orig body3

       ; return (wrap2 <.> wrap1, body4) }

  | otherwise = return (idHsWrapper, sigma)

instantiateSigma :: CtOrigin
                 -> ConcreteTyVars -- ^ concreteness information
                 -> [TyVar]
                 -> TcThetaType -> TcSigmaType
                 -> TcM ([TcTyVar], HsWrapper, TcSigmaType)
-- (instantiate orig tvs theta ty)
-- instantiates the type variables tvs, emits the (instantiated)
-- constraints theta, and returns the (instantiated) type ty
instantiateSigma orig concs tvs theta body_ty
  = do { rec (subst, inst_tvs) <- mapAccumLM (new_meta subst) empty_subst tvs
       ; let inst_theta  = substTheta subst theta
             inst_body   = substTy subst body_ty
             inst_tv_tys = mkTyVarTys inst_tvs

       ; wrap <- instCall orig inst_tv_tys inst_theta
       ; traceTc "Instantiating"
                 (vcat [ text "origin" <+> pprCtOrigin orig
                       , text "tvs"   <+> ppr tvs
                       , text "theta" <+> ppr theta
                       , text "type" <+> debugPprType body_ty
                       , text "with" <+> vcat (map debugPprType inst_tv_tys)
                       , text "theta:" <+> ppr inst_theta ])

      ; return (inst_tvs, wrap, inst_body) }
  where
    in_scope = mkInScopeSet (tyCoVarsOfType (mkSpecSigmaTy tvs theta body_ty))
               -- mkSpecSigmaTy: Inferred vs Specified is not important here;
               --                We just want an accurate free-var set
    empty_subst = mkEmptySubst in_scope
    new_meta :: Subst -> Subst -> TyVar -> TcM (Subst, TcTyVar)
    new_meta final_subst subst tv
      -- Is this a type variable that must be instantiated to a concrete type?
      -- If so, create a ConcreteTv metavariable instead of a plain TauTv.
      -- See Note [Representation-polymorphism checking built-ins] in GHC.Tc.Utils.Concrete.
      | Just conc_orig0 <- lookupNameEnv concs (tyVarName tv)
      , let conc_orig = substConcreteTvOrigin final_subst body_ty conc_orig0
      -- See Note [substConcreteTvOrigin].
      = newConcreteTyVarX conc_orig subst tv
      | otherwise
      = newMetaTyVarX subst tv

instTyVarsWith :: CtOrigin -> [TyVar] -> [TcType] -> TcM Subst
-- Use this when you want to instantiate (forall a b c. ty) with
-- types [ta, tb, tc], but when the kinds of 'a' and 'ta' might
-- not yet match (perhaps because there are unsolved constraints; #14154)
-- If they don't match, emit a kind-equality to promise that they will
-- eventually do so, and thus make a kind-homogeneous substitution.
instTyVarsWith orig tvs tys
  = go emptySubst tvs tys
  where
    go subst [] []
      = return subst
    go subst (tv:tvs) (ty:tys)
      | tv_kind `tcEqType` ty_kind
      = go (extendTvSubstAndInScope subst tv ty) tvs tys
      | otherwise
      = do { co <- emitWantedEq orig KindLevel Nominal ty_kind tv_kind
           ; go (extendTvSubstAndInScope subst tv (ty `mkCastTy` co)) tvs tys }
      where
        tv_kind = substTy subst (tyVarKind tv)
        ty_kind = typeKind ty

    go _ _ _ = pprPanic "instTysWith" (ppr tvs $$ ppr tys)


{-
************************************************************************
*                                                                      *
            Instantiating a call
*                                                                      *
************************************************************************

Note [Handling boxed equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The solver deals entirely in terms of unboxed (primitive) equality.
There should never be a boxed Wanted equality. Ever. But, what if
we are calling `foo :: forall a. (F a ~ Bool) => ...`? That equality
is boxed, so naive treatment here would emit a boxed Wanted equality.

So we simply check for this case and make the right boxing of evidence.

-}

----------------
instCall :: CtOrigin -> [TcType] -> TcThetaType -> TcM HsWrapper
-- Instantiate the constraints of a call
--      (instCall o tys theta)
-- (a) Makes fresh dictionaries as necessary for the constraints (theta)
-- (b) Throws these dictionaries into the LIE
-- (c) Returns an HsWrapper ([.] tys dicts)

instCall orig tys theta
  = do  { dict_app <- instCallConstraints orig theta
        ; return (dict_app <.> mkWpTyApps tys) }

----------------
instCallConstraints :: CtOrigin -> TcThetaType -> TcM HsWrapper
-- Instantiates the TcTheta, puts all constraints thereby generated
-- into the LIE, and returns a HsWrapper to enclose the call site.

instCallConstraints orig preds
  | null preds
  = return idHsWrapper
  | otherwise
  = do { evs <- mapM (emitWanted orig) preds
                -- See Note [Possible fast path for equality constraints]
       ; traceTc "instCallConstraints" (ppr evs)
       ; return (mkWpEvApps evs) }

{- Note [Possible fast path for equality constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given  f :: forall a b. (a ~ [b]) => a -> b -> blah
rather than emitting ([W] alpha ~ [beta]) we could imagine calling unifyType
right here. But note

* Often such constraints look like (F a ~ G b), in which case unification would end up
  spitting out a wanted-equality anyway.

* So perhaps the main fast-path would be where the LHS or RHS was an instantiation
  variable. But note that this could, perhaps, impact on Quick Look:

  - The first arg of `f` changes from the naked `a` to the guarded `[b]` (or would do so
    if we zonked it).  That might affect typing under Quick Look.

  - We might imagine using the let-bound skolems trick:
         g :: forall a b. (a ~ forall c. c->c) => a -> [a] -> [a]
    Here we are just using `a` as a local abreviation for (forall c. c->c)
    See Note [Let-bound skolems] in GHC.Tc.Solver.InertSet.

    If we substitute aggressively (including zonking) that abbreviation could work.  But
    again it affects what is typeable.  And we don't support equalities over polytypes,
    currently, anyway.

* There is little point in trying to optimise for
   - (s ~# t), because this has kind Constraint#, not Constraint, and so will not be
               in the theta instantiated in instCall
   - (s ~~ t), becaues heterogeneous equality is rare, and more complicated.

Anyway, for now we don't take advantage of these potential effects.
-}

instDFunType :: DFunId -> [DFunInstType]
             -> TcM ( [TcType]      -- instantiated argument types
                    , TcThetaType ) -- instantiated constraint
-- See Note [DFunInstType: instantiating types] in GHC.Core.InstEnv
instDFunType dfun_id dfun_inst_tys
  = do { (subst, inst_tys) <- go empty_subst dfun_tvs dfun_inst_tys
       ; return (inst_tys, substTheta subst dfun_theta) }
  where
    dfun_ty = idType dfun_id
    (dfun_tvs, dfun_theta, _) = tcSplitSigmaTy dfun_ty
    empty_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType dfun_ty))
                  -- With quantified constraints, the
                  -- type of a dfun may not be closed

    go :: Subst -> [TyVar] -> [DFunInstType] -> TcM (Subst, [TcType])
    go subst [] [] = return (subst, [])
    go subst (tv:tvs) (Just ty : mb_tys)
      = do { (subst', tys) <- go (extendTvSubstAndInScope subst tv ty)
                                 tvs
                                 mb_tys
           ; return (subst', ty : tys) }
    go subst (tv:tvs) (Nothing : mb_tys)
      = do { (subst', tv') <- newMetaTyVarX subst tv
           ; (subst'', tys) <- go subst' tvs mb_tys
           ; return (subst'', mkTyVarTy tv' : tys) }
    go _ _ _ = pprPanic "instDFunTypes" (ppr dfun_id $$ ppr dfun_inst_tys)

----------------
instStupidTheta :: CtOrigin -> TcThetaType -> TcM ()
-- Similar to instCall, but only emit the constraints in the LIE
-- Used exclusively for the 'stupid theta' of a data constructor
instStupidTheta orig theta
  = do  { _co <- instCallConstraints orig theta -- Discard the coercion
        ; return () }


{- *********************************************************************
*                                                                      *
         Instantiating Kinds
*                                                                      *
********************************************************************* -}

-- | Given ty::forall k1 k2. k, instantiate all the invisible forall-binders
--   returning ty @kk1 @kk2 :: k[kk1/k1, kk2/k1]
-- Called only to instantiate kinds, in user-written type signatures
tcInstInvisibleTyBinders :: TcType -> TcKind -> TcM (TcType, TcKind)
tcInstInvisibleTyBinders ty kind
  = do { (extra_args, kind') <- tcInstInvisibleTyBindersN n_invis kind
       ; return (mkAppTys ty extra_args, kind') }
  where
    n_invis = invisibleTyBndrCount kind

tcInstInvisibleTyBindersN :: Int -> TcKind -> TcM ([TcType], TcKind)
-- Called only to instantiate kinds, in user-written type signatures
tcInstInvisibleTyBindersN 0 kind
  = return ([], kind)
tcInstInvisibleTyBindersN n ty
  = go n empty_subst ty
  where
    empty_subst = mkEmptySubst (mkInScopeSet (tyCoVarsOfType ty))

    go n subst kind
      | n > 0
      , Just (bndr, body) <- tcSplitForAllTyVarBinder_maybe kind
      , isInvisibleForAllTyFlag (binderFlag bndr)
      = do { (subst', arg) <- tcInstInvisibleTyBinder subst (binderVar bndr)
           ; (args, inner_ty) <- go (n-1) subst' body
           ; return (arg:args, inner_ty) }
      | otherwise
      = return ([], substTy subst kind)

tcInstInvisibleTyBinder :: Subst -> TyVar -> TcM (Subst, TcType)
-- Called only to instantiate kinds, in user-written type signatures

tcInstInvisibleTyBinder subst tv
  = do { (subst', tv') <- newMetaTyVarX subst tv
       ; return (subst', mkTyVarTy tv') }

{- *********************************************************************
*                                                                      *
        SkolemTvs (immutable)
*                                                                      *
********************************************************************* -}

tcInstType :: ([TyVar] -> TcM (Subst, [TcTyVar]))
                   -- ^ How to instantiate the type variables
           -> Id                                           -- ^ Type to instantiate
           -> TcM ([(Name, TcTyVar)], TcThetaType, TcType) -- ^ Result
                -- (type vars, preds (incl equalities), rho)
tcInstType inst_tyvars id
  | null tyvars   -- There may be overloading despite no type variables;
                  --      (?x :: Int) => Int -> Int
  = return ([], theta, tau)
  | otherwise
  = do { (subst, tyvars') <- inst_tyvars tyvars
       ; let tv_prs  = map tyVarName tyvars `zip` tyvars'
             subst'  = extendSubstInScopeSet subst (tyCoVarsOfType rho)
       ; return (tv_prs, substTheta subst' theta, substTy subst' tau) }
  where
    (tyvars, rho) = tcSplitForAllInvisTyVars (idType id)
    (theta, tau)  = tcSplitPhiTy rho

tcInstTypeBndrs :: Type -> TcM ([(Name, InvisTVBinder)], TcThetaType, TcType)
                     -- (type vars, preds (incl equalities), rho)
-- Instantiate the binders of a type signature with TyVarTvs
tcInstTypeBndrs poly_ty
  | null tyvars   -- There may be overloading despite no type variables;
                  --      (?x :: Int) => Int -> Int
  = return ([], theta, tau)
  | otherwise
  = do { (subst, tyvars') <- mapAccumLM inst_invis_bndr emptySubst tyvars
       ; let tv_prs  = map (tyVarName . binderVar) tyvars `zip` tyvars'
             subst'  = extendSubstInScopeSet subst (tyCoVarsOfType rho)
       ; return (tv_prs, substTheta subst' theta, substTy subst' tau) }
  where
    (tyvars, rho) = tcSplitForAllInvisTVBinders poly_ty
    (theta, tau)  = tcSplitPhiTy rho

    inst_invis_bndr :: Subst -> InvisTVBinder
                    -> TcM (Subst, InvisTVBinder)
    inst_invis_bndr subst (Bndr tv spec)
      = do { (subst', tv') <- newMetaTyVarTyVarX subst tv
           ; return (subst', Bndr tv' spec) }

--------------------------
tcSkolDFunType :: Type -> TcM (SkolemInfoAnon, [TcTyVar], TcThetaType, Class, [TcType])
-- Instantiate a type signature with skolem constants.
-- This freshens the names, but no need to do so
tcSkolDFunType dfun_ty
  = do { let (tvs, theta, cls, tys) = tcSplitDFunTy dfun_ty

         -- rec {..}: see Note [Keeping SkolemInfo inside a SkolemTv]
         --           in GHC.Tc.Utils.TcType
       ; rec { skol_info <- mkSkolemInfo skol_info_anon
             ; (subst, inst_tvs) <- tcInstSuperSkolTyVars skol_info tvs
                     -- We instantiate the dfun_tyd with superSkolems.
                     -- See Note [Subtle interaction of recursion and overlap]
                     -- and Note [Binding when looking up instances]
             ; let inst_tys = substTys subst tys
                   skol_info_anon = mkClsInstSkol cls inst_tys }

       ; let inst_theta = substTheta subst theta
       ; return (skol_info_anon, inst_tvs, inst_theta, cls, inst_tys) }

tcSuperSkolTyVars :: TcLevel -> SkolemInfo -> [TyVar] -> (Subst, [TcTyVar])
-- Make skolem constants, but do *not* give them new names, as above
-- As always, allocate them one level in
-- Moreover, make them "super skolems"; see GHC.Core.InstEnv
--    Note [Binding when looking up instances]
-- See Note [Kind substitution when instantiating]
-- Precondition: tyvars should be ordered by scoping
tcSuperSkolTyVars tc_lvl skol_info = mapAccumL do_one emptySubst
  where
    details = SkolemTv skol_info (pushTcLevel tc_lvl)
                       True   -- The "super" bit
    do_one subst tv = (extendTvSubstWithClone subst tv new_tv, new_tv)
      where
        kind   = substTyUnchecked subst (tyVarKind tv)
        new_tv = mkTcTyVar (tyVarName tv) kind details

-- | Given a list of @['TyVar']@, skolemize the type variables,
-- returning a substitution mapping the original tyvars to the
-- skolems, and the list of newly bound skolems.
tcInstSkolTyVars :: SkolemInfo -> [TyVar] -> TcM (Subst, [TcTyVar])
-- See Note [Skolemising type variables]
tcInstSkolTyVars skol_info = tcInstSkolTyVarsX skol_info emptySubst

tcInstSkolTyVarsX :: SkolemInfo -> Subst -> [TyVar] -> TcM (Subst, [TcTyVar])
-- See Note [Skolemising type variables]
tcInstSkolTyVarsX skol_info = tcInstSkolTyVarsPushLevel skol_info False

tcInstSkolTyVarBndrsX :: SkolemInfo -> Subst -> [VarBndr TyCoVar vis] -> TcM (Subst, [VarBndr TyCoVar vis])
tcInstSkolTyVarBndrsX skol_info subs bndrs = do
  (subst', bndrs') <- tcInstSkolTyVarsX skol_info subs (binderVars bndrs)
  pure (subst', zipWith mkForAllTyBinder flags bndrs')
  where
    flags = binderFlags bndrs

tcInstSuperSkolTyVars :: SkolemInfo -> [TyVar] -> TcM (Subst, [TcTyVar])
-- See Note [Skolemising type variables]
-- This version freshens the names and creates "super skolems";
--    see comments around superSkolemTv.
-- Must be lazy in skol_info:
--   see Note [Keeping SkolemInfo inside a SkolemTv] in GHC.Tc.Utils.TcType
tcInstSuperSkolTyVars skol_info = tcInstSuperSkolTyVarsX skol_info emptySubst

tcInstSuperSkolTyVarsX :: SkolemInfo -> Subst -> [TyVar] -> TcM (Subst, [TcTyVar])
-- See Note [Skolemising type variables]
-- This version freshens the names and creates "super skolems";
-- see comments around superSkolemTv.
tcInstSuperSkolTyVarsX skol_info subst = tcInstSkolTyVarsPushLevel skol_info True subst

tcInstSkolTyVarsPushLevel :: SkolemInfo -> Bool  -- True <=> make "super skolem"
                          -> Subst -> [TyVar]
                          -> TcM (Subst, [TcTyVar])
-- Skolemise one level deeper, hence pushTcLevel
-- See Note [Skolemising type variables]
tcInstSkolTyVarsPushLevel skol_info overlappable subst tvs
  = do { tc_lvl <- getTcLevel
       -- Do not retain the whole TcLclEnv
       ; let !pushed_lvl = pushTcLevel tc_lvl
       ; tcInstSkolTyVarsAt skol_info pushed_lvl overlappable subst tvs }

tcInstSkolTyVarsAt :: SkolemInfo -> TcLevel -> Bool
                   -> Subst -> [TyVar]
                   -> TcM (Subst, [TcTyVar])
tcInstSkolTyVarsAt skol_info lvl overlappable subst tvs
  = freshenTyCoVarsX new_skol_tv subst tvs
  where
    sk_details = SkolemTv skol_info lvl overlappable
    new_skol_tv name kind = mkTcTyVar name kind sk_details

tcSkolemiseInvisibleBndrs :: SkolemInfoAnon -> Type -> TcM ([TcTyVar], TcType)
-- Skolemise the outer invisible binders of a type
-- Do /not/ freshen them, because their scope is broader than
-- just this type.  It's a bit dubious, but used in very limited ways.
tcSkolemiseInvisibleBndrs skol_info ty
  = do { let (tvs, body_ty) = tcSplitForAllInvisTyVars ty
       ; lvl           <- getTcLevel
       ; skol_info     <- mkSkolemInfo skol_info
       ; let details = SkolemTv skol_info lvl False
             mk_skol_tv name kind = return (mkTcTyVar name kind details)  -- No freshening
       ; (subst, tvs') <- instantiateTyVarsX mk_skol_tv emptySubst tvs
       ; return (tvs', substTy subst body_ty) }

instantiateTyVarsX :: (Name -> Kind -> TcM TcTyVar)
                   -> Subst -> [TyVar]
                   -> TcM (Subst, [TcTyVar])
-- Instantiate each type variable in turn with the specified function
instantiateTyVarsX mk_tv subst tvs
  = case tvs of
      []       -> return (subst, [])
      (tv:tvs) -> do { let kind1 = substTyUnchecked subst (tyVarKind tv)
                     ; tv' <- mk_tv (tyVarName tv) kind1
                     ; let subst1 = extendTCvSubstWithClone subst tv tv'
                     ; (subst', tvs') <- instantiateTyVarsX mk_tv subst1 tvs
                     ; return (subst', tv':tvs') }

------------------
freshenTyVarBndrs :: [TyVar] -> TcM (Subst, [TyVar])
-- ^ Give fresh uniques to a bunch of TyVars, but they stay
--   as TyVars, rather than becoming TcTyVars
-- Used in 'GHC.Tc.Instance.Family.newFamInst', and 'GHC.Tc.Utils.Instantiate.newClsInst'
freshenTyVarBndrs = freshenTyCoVars mkTyVar

freshenCoVarBndrsX :: Subst -> [CoVar] -> TcM (Subst, [CoVar])
-- ^ Give fresh uniques to a bunch of CoVars
-- Used in "GHC.Tc.Instance.Family.newFamInst"
freshenCoVarBndrsX subst = freshenTyCoVarsX mkCoVar subst

------------------
freshenTyCoVars :: (Name -> Kind -> TyCoVar)
                -> [TyVar] -> TcM (Subst, [TyCoVar])
freshenTyCoVars mk_tcv = freshenTyCoVarsX mk_tcv emptySubst

freshenTyCoVarsX :: (Name -> Kind -> TyCoVar)
                 -> Subst -> [TyCoVar]
                 -> TcM (Subst, [TyCoVar])
-- This a complete freshening operation:
-- the skolems have a fresh unique, and a location from the monad
-- See Note [Skolemising type variables]
freshenTyCoVarsX mk_tcv
  = instantiateTyVarsX freshen_tcv
  where
    freshen_tcv :: Name -> Kind -> TcM TcTyVar
    freshen_tcv name kind
      = do { loc  <- getSrcSpanM
           ; uniq <- newUnique
           ; let !occ_name = getOccName name
                    -- Force so we don't retain reference to the old
                    -- name and id.   See (#19619) for more discussion
                 new_name = mkInternalName uniq occ_name loc
           ; return (mk_tcv new_name kind) }

{- Note [Skolemising type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The tcInstSkolTyVars family of functions instantiate a list of TyVars
to fresh skolem TcTyVars. Important notes:

a) Level allocation. We generally skolemise /before/ calling
   pushLevelAndCaptureConstraints.  So we want their level to the level
   of the soon-to-be-created implication, which has a level ONE HIGHER
   than the current level.  Hence the pushTcLevel.  It feels like a
   slight hack.

b) The [TyVar] should be ordered (kind vars first)
   See Note [Kind substitution when instantiating]

c) Clone the variable to give a fresh unique.  This is essential.
   Consider (tc160)
       type Foo x = forall a. a -> x
   And typecheck the expression
       (e :: Foo (Foo ())
   We will skolemise the signature, but after expanding synonyms it
   looks like
        forall a. a -> forall a. a -> x
   We don't want to make two big-lambdas with the same unique!

d) We retain locations. Because the location of the variable is the correct
   location to report in errors (e.g. in the signature). We don't want the
   location to change to the body of the function, which does *not* explicitly
   bind the variable.

e) The resulting skolems are
        non-overlappable for tcInstSkolTyVars,
   but overlappable for tcInstSuperSkolTyVars
   See GHC.Tc.Deriv.Infer Note [Overlap and deriving] for an example
   of where this matters.

Note [Kind substitution when instantiating]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we instantiate a bunch of kind and type variables, first we
expect them to be topologically sorted.
Then we have to instantiate the kind variables, build a substitution
from old variables to the new variables, then instantiate the type
variables substituting the original kind.

Example: If we want to instantiate
  [(k1 :: *), (k2 :: *), (a :: k1 -> k2), (b :: k1)]
we want
  [(?k1 :: *), (?k2 :: *), (?a :: ?k1 -> ?k2), (?b :: ?k1)]
instead of the bogus
  [(?k1 :: *), (?k2 :: *), (?a :: k1 -> k2), (?b :: k1)]
-}

{- *********************************************************************
*                                                                      *
                Literals
*                                                                      *
********************************************************************* -}

{-
In newOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).

-}

newOverloadedLit :: HsOverLit GhcRn
                 -> ExpRhoType
                 -> TcM (HsOverLit GhcTc)
newOverloadedLit lit res_ty
  = do { mb_lit' <- tcShortCutLit lit res_ty
       ; case mb_lit' of
            Just lit' -> return lit'
            Nothing   -> newNonTrivialOverloadedLit lit res_ty }

-- Does not handle things that 'shortCutLit' can handle. See also
-- newOverloadedLit in GHC.Tc.Utils.Unify
newNonTrivialOverloadedLit :: HsOverLit GhcRn
                           -> ExpRhoType
                           -> TcM (HsOverLit GhcTc)
newNonTrivialOverloadedLit
  lit@(OverLit { ol_val = val, ol_ext = OverLitRn rebindable (L loc meth_name) })
  res_ty
  = do  { hs_lit <- mkOverLit val
        ; let lit_ty = hsLitType hs_lit
        ; (_, fi') <- tcSyntaxOp orig (mkRnSyntaxExpr meth_name)
                                      [synKnownType lit_ty] res_ty $
                      \_ _ -> return ()
        ; let L _ witness = mkHsSyntaxApps (l2l loc) fi' [nlHsLit hs_lit]
        ; res_ty <- readExpType res_ty
        ; return (lit { ol_ext = OverLitTc { ol_rebindable = rebindable
                                           , ol_witness = witness
                                           , ol_type = res_ty } }) }
  where
    orig = LiteralOrigin lit

------------
mkOverLit :: OverLitVal -> TcM (HsLit (GhcPass p))
mkOverLit (HsIntegral i)
  = do  { integer_ty <- tcMetaTy integerTyConName
        ; return (HsInteger (il_text i)
                            (il_value i) integer_ty) }

mkOverLit (HsFractional r)
  = do  { rat_ty <- tcMetaTy rationalTyConName
        ; return (HsRat noExtField r rat_ty) }

mkOverLit (HsIsString src s) = return (HsString src s)

{-
************************************************************************
*                                                                      *
                Re-mappable syntax

     Used only for arrow syntax -- find a way to nuke this
*                                                                      *
************************************************************************

Suppose we are doing the -XRebindableSyntax thing, and we encounter
a do-expression.  We have to find (>>) in the current environment, which is
done by the rename. Then we have to check that it has the same type as
Control.Monad.(>>).  Or, more precisely, a compatible type. One 'customer' had
this:

  (>>) :: HB m n mn => m a -> n b -> mn b

So the idea is to generate a local binding for (>>), thus:

        let then72 :: forall a b. m a -> m b -> m b
            then72 = ...something involving the user's (>>)...
        in
        ...the do-expression...

Now the do-expression can proceed using then72, which has exactly
the expected type.

In fact tcSyntaxName just generates the RHS for then72, because we only
want an actual binding in the do-expression case. For literals, we can
just use the expression inline.
-}

tcSyntaxName :: CtOrigin
             -> TcType                  -- ^ Type to instantiate it at
             -> (Name, HsExpr GhcRn)    -- ^ (Standard name, user name)
             -> TcM (Name, HsExpr GhcTc)
                                        -- ^ (Standard name, suitable expression)
-- USED ONLY FOR CmdTop (sigh) ***
-- See Note [CmdSyntaxTable] in "GHC.Hs.Expr"

tcSyntaxName orig ty (std_nm, HsVar _ (L _ user_nm))
  | std_nm == user_nm
  = do rhs <- newMethodFromName orig std_nm [ty]
       return (std_nm, rhs)

tcSyntaxName orig ty (std_nm, user_nm_expr) = do
    std_id <- tcLookupId std_nm
    let
        ([tv], _, tau) = tcSplitSigmaTy (idType std_id)
        sigma1         = substTyWith [tv] [ty] tau
        -- Actually, the "tau-type" might be a sigma-type in the
        -- case of locally-polymorphic methods.

    span <- getSrcSpanM
    addErrCtxtM (syntaxNameCtxt user_nm_expr orig sigma1 span) $ do

        -- Check that the user-supplied thing has the
        -- same type as the standard one.
        -- Tiresome jiggling because tcCheckSigma takes a located expression
     expr <- tcCheckPolyExpr (L (noAnnSrcSpan span) user_nm_expr) sigma1
     hasFixedRuntimeRepRes std_nm user_nm_expr sigma1
     return (std_nm, unLoc expr)

syntaxNameCtxt :: HsExpr GhcRn -> CtOrigin -> Type -> SrcSpan -> TidyEnv
               -> ZonkM (TidyEnv, SDoc)
syntaxNameCtxt name orig ty loc tidy_env = return (tidy_env, msg)
  where
    msg = vcat [ text "When checking that" <+> quotes (ppr name)
                          <+> text "(needed by a syntactic construct)"
               , nest 2 (text "has the required type:"
                         <+> ppr (tidyType tidy_env ty))
               , nest 2 (sep [ppr orig, text "at" <+> ppr loc])]

{-
************************************************************************
*                                                                      *
                FixedRuntimeRep
*                                                                      *
************************************************************************
-}

-- | Check that the result type of an expression has a fixed runtime representation.
--
-- Used only for arrow operations such as 'arr', 'first', etc.
hasFixedRuntimeRepRes :: Name -> HsExpr GhcRn -> TcSigmaType -> TcM ()
hasFixedRuntimeRepRes std_nm user_expr ty = mapM_ do_check mb_arity
  where
   do_check :: Arity -> TcM ()
   do_check arity =
     let res_ty = nTimes arity (snd . splitPiTy) ty
     in hasFixedRuntimeRep_syntactic (FRRArrow $ ArrowFun user_expr) res_ty
   mb_arity :: Maybe Arity
   mb_arity -- arity of the arrow operation, counting type-level arguments
     | sameUnique std_nm arrAName     -- result used as an argument in, e.g., do_premap
     = Just 3
     | sameUnique std_nm composeAName -- result used as an argument in, e.g., dsCmdStmt/BodyStmt
     = Just 5
     | sameUnique std_nm firstAName   -- result used as an argument in, e.g., dsCmdStmt/BodyStmt
     = Just 4
     | sameUnique std_nm appAName     -- result used as an argument in, e.g., dsCmd/HsCmdArrApp/HsHigherOrderApp
     = Just 2
     | sameUnique std_nm choiceAName  -- result used as an argument in, e.g., HsCmdIf
     = Just 5
     | sameUnique std_nm loopAName    -- result used as an argument in, e.g., HsCmdIf
     = Just 4
     | otherwise
     = Nothing

{-
************************************************************************
*                                                                      *
                Class instances
*                                                                      *
************************************************************************
-}

getOverlapFlag :: Maybe OverlapMode   -- User pragma if any
               -> TcM OverlapFlag
-- Construct the OverlapFlag from the global module flags,
-- but if the overlap_mode argument is (Just m),
--     set the OverlapMode to 'm'
--
-- The overlap_mode argument comes from a user pragma on the instance decl:
--    Pragma                      overlap_mode_prag
--    -----------------------------------------
--    {-# OVERLAPPABLE #-}        Overlappable
--    {-# OVERLAPPING #-}         Overlapping
--    {-# OVERLAPS #-}            Overlaps
--    {-# INCOHERENT #-}          Incoherent   -- if -fspecialise-incoherent (on by default)
--    {-# INCOHERENT #-}          NonCanonical -- if -fno-specialise-incoherent
-- See Note [Rules for instance lookup] in GHC.Core.InstEnv

getOverlapFlag overlap_mode_prag
  = do  { dflags <- getDynFlags
        ; let overlap_ok               = xopt LangExt.OverlappingInstances dflags
              incoherent_ok            = xopt LangExt.IncoherentInstances  dflags
              noncanonical_incoherence = not $ gopt Opt_SpecialiseIncoherents dflags

              overlap_mode
                | Just m <- overlap_mode_prag = m
                | incoherent_ok               = Incoherent NoSourceText
                | overlap_ok                  = Overlaps   NoSourceText
                | otherwise                   = NoOverlap  NoSourceText

              -- final_overlap_mode: the `-fspecialise-incoherents` flag controls the
              -- meaning of the `Incoherent` overlap mode: as either an Incoherent overlap
              -- flag, or a NonCanonical overlap flag.
              -- See GHC.Core.InstEnv Note [Coherence and specialisation: overview]
              final_overlap_mode
                | Incoherent s <- overlap_mode
                , noncanonical_incoherence       = NonCanonical s
                | otherwise                      = overlap_mode

        ; return (OverlapFlag { isSafeOverlap = safeLanguageOn dflags
                              , overlapMode   = final_overlap_mode }) }


tcGetInsts :: TcM [ClsInst]
-- Gets the local class instances.
tcGetInsts = fmap tcg_insts getGblEnv

newClsInst :: Maybe OverlapMode   -- User pragma
           -> Name -> [TyVar] -> ThetaType
           -> Class -> [Type] -> Maybe (WarningTxt GhcRn) -> TcM ClsInst
newClsInst overlap_mode dfun_name tvs theta clas tys warn
  = do { (subst, tvs') <- freshenTyVarBndrs tvs
             -- Be sure to freshen those type variables,
             -- so they are sure not to appear in any lookup
       ; let tys' = substTys subst tys

             dfun = mkDictFunId dfun_name tvs theta clas tys
             -- The dfun uses the original 'tvs' because
             -- (a) they don't need to be fresh
             -- (b) they may be mentioned in the ib_binds field of
             --     an InstInfo, and in GHC.Tc.Utils.Env.pprInstInfoDetails it's
             --     helpful to use the same names

       ; oflag <- getOverlapFlag overlap_mode
       ; let cls_inst = mkLocalClsInst dfun oflag tvs' clas tys' warn

       ; when (isOrphan (is_orphan cls_inst)) $
         addDiagnostic (TcRnOrphanInstance $ Left cls_inst)

       ; return cls_inst }

tcExtendLocalInstEnv :: [ClsInst] -> TcM a -> TcM a
  -- Add new locally-defined instances
tcExtendLocalInstEnv dfuns thing_inside
 = do { traceDFuns dfuns
      ; env <- getGblEnv
      -- Force the access to the TcgEnv so it isn't retained.
      -- During auditing it is much easier to observe in -hi profiles if
      -- there are a very small number of TcGblEnv. Keeping a TcGblEnv
      -- alive is quite dangerous because it contains reference to many
      -- large data structures.
      ; let !init_inst_env = tcg_inst_env env
            !init_insts = tcg_insts env
      ; (inst_env', cls_insts') <- foldlM addLocalInst
                                          (init_inst_env, init_insts)
                                          dfuns
      ; let env' = env { tcg_insts    = cls_insts'
                       , tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }

addLocalInst :: (InstEnv, [ClsInst]) -> ClsInst -> TcM (InstEnv, [ClsInst])
-- Check that the proposed new instance is OK,
-- and then add it to the home inst env
-- If overwrite_inst, then we can overwrite a direct match
addLocalInst (home_ie, my_insts) ispec
   = do {
             -- Load imported instances, so that we report
             -- duplicates correctly

             -- 'matches'  are existing instance declarations that are less
             --            specific than the new one
             -- 'dups'     are those 'matches' that are equal to the new one
         ; isGHCi <- getIsGHCi
         ; eps    <- getEps
         ; tcg_env <- getGblEnv

           -- In GHCi, we *override* any identical instances
           -- that are also defined in the interactive context
           -- See Note [Override identical instances in GHCi]
         ; let home_ie'
                 | isGHCi    = deleteFromInstEnv home_ie ispec
                 | otherwise = home_ie

               global_ie = eps_inst_env eps
               inst_envs = InstEnvs { ie_global  = global_ie
                                    , ie_local   = home_ie'
                                    , ie_visible = tcVisibleOrphanMods tcg_env }

             -- Check for inconsistent functional dependencies
         ; let inconsistent_ispecs = checkFunDeps inst_envs ispec
         ; unless (null inconsistent_ispecs) $
           funDepErr ispec inconsistent_ispecs

             -- Check for duplicate instance decls.
         ; let (_tvs, cls, tys) = instanceHead ispec
               (matches, _, _)  = lookupInstEnv False inst_envs cls tys
               dups             = filter (identicalClsInstHead ispec) (map fst matches)
         ; unless (null dups) $
           dupInstErr ispec (head dups)

         ; return (extendInstEnv home_ie' ispec, ispec : my_insts) }


{- Note [Signature files and type class instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instances in signature files do not have an effect when compiling:
when you compile a signature against an implementation, you will
see the instances WHETHER OR NOT the instance is declared in
the file (this is because the signatures go in the EPS and we
can't filter them out easily.)  This is also why we cannot
place the instance in the hi file: it would show up as a duplicate,
and we don't have instance reexports anyway.

However, you might find them useful when typechecking against
a signature: the instance is a way of indicating to GHC that
some instance exists, in case downstream code uses it.

Implementing this is a little tricky.  Consider the following
situation (sigof03):

 module A where
     instance C T where ...

 module ASig where
     instance C T

When compiling ASig, A.hi is loaded, which brings its instances
into the EPS.  When we process the instance declaration in ASig,
we should ignore it for the purpose of doing a duplicate check,
since it's not actually a duplicate. But don't skip the check
entirely, we still want this to fail (tcfail221):

 module ASig where
     instance C T
     instance C T

Note that in some situations, the interface containing the type
class instances may not have been loaded yet at all.  The usual
situation when A imports another module which provides the
instances (sigof02m):

 module A(module B) where
     import B

See also Note [Signature lazy interface loading].  We can't
rely on this, however, since sometimes we'll have spurious
type class instances in the EPS, see #9422 (sigof02dm)

************************************************************************
*                                                                      *
                Family instances
*                                                                      *
************************************************************************
-}

-- All type variables in a FamInst must be fresh. This function
-- creates the fresh variables and applies the necessary substitution
-- It is defined here to avoid a dependency from FamInstEnv on the monad
-- code.

newFamInst :: FamFlavor -> CoAxiom Unbranched -> TcM FamInst
-- Freshen the type variables of the FamInst branches
newFamInst flavor axiom
  | CoAxBranch { cab_tvs = tvs
               , cab_cvs = cvs
               , cab_lhs = lhs
               , cab_rhs = rhs } <- coAxiomSingleBranch axiom
  = do { -- Freshen the type variables
         (subst, tvs') <- freshenTyVarBndrs tvs
       ; (subst, cvs') <- freshenCoVarBndrsX subst cvs
       ; let lhs'     = substTys subst lhs
             rhs'     = substTy  subst rhs

       ; let fam_inst = mkLocalFamInst flavor axiom tvs' cvs' lhs' rhs'
       ; when (isOrphan (fi_orphan fam_inst)) $
         addDiagnostic (TcRnOrphanInstance $ Right fam_inst)

       ; return fam_inst }


{- *********************************************************************
*                                                                      *
        Errors and tracing
*                                                                      *
********************************************************************* -}

traceDFuns :: [ClsInst] -> TcRn ()
traceDFuns ispecs
  = traceTc "Adding instances:" (vcat (map pp ispecs))
  where
    pp ispec = hang (ppr (instanceDFunId ispec) <+> colon)
                  2 (ppr ispec)
        -- Print the dfun name itself too

funDepErr :: ClsInst -> [ClsInst] -> TcRn ()
funDepErr ispec ispecs
  = addClsInstsErr TcRnFunDepConflict (ispec NE.:| ispecs)

dupInstErr :: ClsInst -> ClsInst -> TcRn ()
dupInstErr ispec dup_ispec
  = addClsInstsErr TcRnDupInstanceDecls (ispec NE.:| [dup_ispec])

addClsInstsErr :: (UnitState -> NE.NonEmpty ClsInst -> TcRnMessage)
               -> NE.NonEmpty ClsInst
               -> TcRn ()
addClsInstsErr mkErr ispecs = do
   unit_state <- hsc_units <$> getTopEnv
   setSrcSpan (getSrcSpan (NE.head sorted)) $
      addErr $ mkErr unit_state sorted
 where
   sorted = NE.sortBy (SrcLoc.leftmost_smallest `on` getSrcSpan) ispecs
   -- The sortBy just arranges that instances are displayed in order
   -- of source location, which reduced wobbling in error messages,
   -- and is better for users
