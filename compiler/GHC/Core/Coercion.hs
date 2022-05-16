{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
(c) The University of Glasgow 2006
-}

-- | Module for (a) type kinds and (b) type coercions,
-- as used in System FC. See 'GHC.Core.Expr' for
-- more on System FC and how coercions fit into it.
--
module GHC.Core.Coercion (
        -- * Main data type
        Coercion, CoercionN, CoercionR, CoercionP, MCoercion(..), MCoercionN, MCoercionR,
        UnivCoProvenance, CoercionHole(..),
        coHoleCoVar, setCoHoleCoVar,
        LeftOrRight(..),
        Var, CoVar, TyCoVar,
        Role(..), ltRole,

        -- ** Functions over coercions
        coVarRType, coVarLType, coVarTypes,
        coVarKind, coVarKindsTypesRole, coVarRole,
        coercionType, mkCoercionType,
        coercionKind, coercionLKind, coercionRKind,coercionKinds,
        coercionRole, coercionKindRole,

        -- ** Constructing coercions
        mkGReflCo, mkReflCo, mkRepReflCo, mkNomReflCo,
        mkCoVarCo, mkCoVarCos,
        mkAxInstCo, mkUnbranchedAxInstCo,
        mkAxInstRHS, mkUnbranchedAxInstRHS,
        mkAxInstLHS, mkUnbranchedAxInstLHS,
        mkPiCo, mkPiCos, mkCoCast,
        mkSymCo, mkTransCo,
        mkNthCo, mkNthCoFunCo, nthCoRole, mkLRCo,
        mkInstCo, mkAppCo, mkAppCos, mkTyConAppCo, mkFunCo, mkFunResCo,
        mkForAllCo, mkForAllCos, mkHomoForAllCos,
        mkPhantomCo,
        mkHoleCo, mkUnivCo, mkSubCo,
        mkAxiomInstCo, mkProofIrrelCo,
        downgradeRole, mkAxiomRuleCo,
        mkGReflRightCo, mkGReflLeftCo, mkCoherenceLeftCo, mkCoherenceRightCo,
        mkKindCo,
        castCoercionKind, castCoercionKind1, castCoercionKind2,
        mkFamilyTyConAppCo,

        mkHeteroCoercionType,
        mkPrimEqPred, mkReprPrimEqPred, mkPrimEqPredRole,
        mkHeteroPrimEqPred, mkHeteroReprPrimEqPred,

        -- ** Decomposition
        instNewTyCon_maybe,

        NormaliseStepper, NormaliseStepResult(..), composeSteppers,
        mapStepResult, unwrapNewTypeStepper,
        topNormaliseNewType_maybe, topNormaliseTypeX,

        decomposeCo, decomposeFunCo, decomposePiCos, getCoVar_maybe,
        splitTyConAppCo_maybe,
        splitAppCo_maybe,
        splitFunCo_maybe,
        splitForAllCo_maybe,
        splitForAllCo_ty_maybe, splitForAllCo_co_maybe,

        nthRole, tyConRolesX, tyConRolesRepresentational, setNominalRole_maybe,

        pickLR,

        isGReflCo, isReflCo, isReflCo_maybe, isGReflCo_maybe, isReflexiveCo, isReflexiveCo_maybe,
        isReflCoVar_maybe, isGReflMCo, mkGReflLeftMCo, mkGReflRightMCo,
        mkCoherenceRightMCo,

        coToMCo, mkTransMCo, mkTransMCoL, mkTransMCoR, mkCastTyMCo, mkSymMCo,
        mkHomoForAllMCo, mkFunResMCo, mkPiMCos,
        isReflMCo, checkReflexiveMCo,

        -- ** Coercion variables
        mkCoVar, isCoVar, coVarName, setCoVarName, setCoVarUnique,
        isCoVar_maybe,

        -- ** Free variables
        tyCoVarsOfCo, tyCoVarsOfCos, coVarsOfCo,
        tyCoFVsOfCo, tyCoFVsOfCos, tyCoVarsOfCoDSet,
        coercionSize, anyFreeVarsOfCo,

        -- ** Substitution
        CvSubstEnv, emptyCvSubstEnv,
        lookupCoVar,
        substCo, substCos, substCoVar, substCoVars, substCoWith,
        substCoVarBndr,
        extendTvSubstAndInScope, getCvSubstEnv,

        -- ** Lifting
        liftCoSubst, liftCoSubstTyVar, liftCoSubstWith, liftCoSubstWithEx,
        emptyLiftingContext, extendLiftingContext, extendLiftingContextAndInScope,
        liftCoSubstVarBndrUsing, isMappedByLC,

        mkSubstLiftingContext, zapLiftingContext,
        substForAllCoBndrUsingLC, lcTCvSubst, lcInScopeSet,

        LiftCoEnv, LiftingContext(..), liftEnvSubstLeft, liftEnvSubstRight,
        substRightCo, substLeftCo, swapLiftCoEnv, lcSubstLeft, lcSubstRight,

        -- ** Comparison
        eqCoercion, eqCoercionX,

        -- ** Forcing evaluation of coercions
        seqCo,

        -- * Pretty-printing
        pprCo, pprParendCo,
        pprCoAxiom, pprCoAxBranch, pprCoAxBranchLHS,
        pprCoAxBranchUser, tidyCoAxBndrsForUser,
        etaExpandCoAxBranch,

        -- * Tidying
        tidyCo, tidyCos,

        -- * Other
        promoteCoercion, buildCoercion,

        multToCo,

        hasCoercionHoleTy, hasCoercionHoleCo, hasThisCoercionHoleTy,

        setCoHoleType
       ) where

import {-# SOURCE #-} GHC.CoreToIface (toIfaceTyCon, tidyToIfaceTcArgs)

import GHC.Prelude

import GHC.Iface.Type
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.FVs
import GHC.Core.TyCo.Ppr
import GHC.Core.TyCo.Subst
import GHC.Core.TyCo.Tidy
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.TyCon.RecWalk
import GHC.Core.Coercion.Axiom
import {-# SOURCE #-} GHC.Core.Utils ( mkFunctionType )
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Name hiding ( varName )
import GHC.Types.Basic
import GHC.Types.Unique
import GHC.Data.Pair
import GHC.Types.SrcLoc
import GHC.Builtin.Names
import GHC.Builtin.Types.Prim
import GHC.Data.List.SetOps
import GHC.Data.Maybe
import GHC.Types.Unique.FM

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain

import Control.Monad (foldM, zipWithM)
import Data.Function ( on )
import Data.Char( isDigit )
import qualified Data.Monoid as Monoid

{-
%************************************************************************
%*                                                                      *
     -- The coercion arguments always *precisely* saturate
     -- arity of (that branch of) the CoAxiom.  If there are
     -- any left over, we use AppCo.  See
     -- See [Coercion axioms applied to coercions] in GHC.Core.TyCo.Rep

\subsection{Coercion variables}
%*                                                                      *
%************************************************************************
-}

coVarName :: CoVar -> Name
coVarName = varName

setCoVarUnique :: CoVar -> Unique -> CoVar
setCoVarUnique = setVarUnique

setCoVarName :: CoVar -> Name -> CoVar
setCoVarName   = setVarName

{-
%************************************************************************
%*                                                                      *
                   Pretty-printing CoAxioms
%*                                                                      *
%************************************************************************

Defined here to avoid module loops. CoAxiom is loaded very early on.

-}

etaExpandCoAxBranch :: CoAxBranch -> ([TyVar], [Type], Type)
-- Return the (tvs,lhs,rhs) after eta-expanding,
-- to the way in which the axiom was originally written
-- See Note [Eta reduction for data families] in GHC.Core.Coercion.Axiom
etaExpandCoAxBranch (CoAxBranch { cab_tvs = tvs
                                , cab_eta_tvs = eta_tvs
                                , cab_lhs = lhs
                                , cab_rhs = rhs })
  -- ToDo: what about eta_cvs?
  = (tvs ++ eta_tvs, lhs ++ eta_tys, mkAppTys rhs eta_tys)
 where
    eta_tys = mkTyVarTys eta_tvs

pprCoAxiom :: CoAxiom br -> SDoc
-- Used in debug-printing only
pprCoAxiom ax@(CoAxiom { co_ax_tc = tc, co_ax_branches = branches })
  = hang (text "axiom" <+> ppr ax <+> dcolon)
       2 (vcat (map (pprCoAxBranchUser tc) (fromBranches branches)))

pprCoAxBranchUser :: TyCon -> CoAxBranch -> SDoc
-- Used when printing injectivity errors (FamInst.reportInjectivityErrors)
-- and inaccessible branches (GHC.Tc.Validity.inaccessibleCoAxBranch)
-- This happens in error messages: don't print the RHS of a data
--   family axiom, which is meaningless to a user
pprCoAxBranchUser tc br
  | isDataFamilyTyCon tc = pprCoAxBranchLHS tc br
  | otherwise            = pprCoAxBranch    tc br

pprCoAxBranchLHS :: TyCon -> CoAxBranch -> SDoc
-- Print the family-instance equation when reporting
--   a conflict between equations (FamInst.conflictInstErr)
-- For type families the RHS is important; for data families not so.
--   Indeed for data families the RHS is a mysterious internal
--   type constructor, so we suppress it (#14179)
-- See FamInstEnv Note [Family instance overlap conflicts]
pprCoAxBranchLHS = ppr_co_ax_branch pp_rhs
  where
    pp_rhs _ _ = empty

pprCoAxBranch :: TyCon -> CoAxBranch -> SDoc
pprCoAxBranch = ppr_co_ax_branch ppr_rhs
  where
    ppr_rhs env rhs = equals <+> pprPrecTypeX env topPrec rhs

ppr_co_ax_branch :: (TidyEnv -> Type -> SDoc)
                 -> TyCon -> CoAxBranch -> SDoc
ppr_co_ax_branch ppr_rhs fam_tc branch
  = foldr1 (flip hangNotEmpty 2)
    [ pprUserForAll (mkTyCoVarBinders Inferred bndrs')
         -- See Note [Printing foralls in type family instances] in GHC.Iface.Type
    , pp_lhs <+> ppr_rhs tidy_env ee_rhs
    , text "-- Defined" <+> pp_loc ]
  where
    loc = coAxBranchSpan branch
    pp_loc | isGoodSrcSpan loc = text "at" <+> ppr (srcSpanStart loc)
           | otherwise         = text "in" <+> ppr loc

    -- Eta-expand LHS and RHS types, because sometimes data family
    -- instances are eta-reduced.
    -- See Note [Eta reduction for data families] in GHC.Core.Coercion.Axiom.
    (ee_tvs, ee_lhs, ee_rhs) = etaExpandCoAxBranch branch

    pp_lhs = pprIfaceTypeApp topPrec (toIfaceTyCon fam_tc)
                             (tidyToIfaceTcArgs tidy_env fam_tc ee_lhs)

    (tidy_env, bndrs') = tidyCoAxBndrsForUser emptyTidyEnv ee_tvs

tidyCoAxBndrsForUser :: TidyEnv -> [Var] -> (TidyEnv, [Var])
-- Tidy wildcards "_1", "_2" to "_", and do not return them
-- in the list of binders to be printed
-- This is so that in error messages we see
--     forall a. F _ [a] _ = ...
-- rather than
--     forall a _1 _2. F _1 [a] _2 = ...
--
-- This is a rather disgusting function
-- See Note [Wildcard names] in GHC.Tc.Gen.HsType
tidyCoAxBndrsForUser init_env tcvs
  = (tidy_env, reverse tidy_bndrs)
  where
    (tidy_env, tidy_bndrs) = foldl tidy_one (init_env, []) tcvs

    tidy_one (env@(occ_env, subst), rev_bndrs') bndr
      | is_wildcard bndr = (env_wild, rev_bndrs')
      | otherwise        = (env',     bndr' : rev_bndrs')
      where
        (env', bndr') = tidyVarBndr env bndr
        env_wild = (occ_env, extendVarEnv subst bndr wild_bndr)
        wild_bndr = setVarName bndr $
                    tidyNameOcc (varName bndr) (mkTyVarOcc "_")
                    -- Tidy the binder to "_"

    is_wildcard :: Var -> Bool
    is_wildcard tv = case occNameString (getOccName tv) of
                       ('_' : rest) -> all isDigit rest
                       _            -> False


{- *********************************************************************
*                                                                      *
              MCoercion
*                                                                      *
********************************************************************* -}

coToMCo :: Coercion -> MCoercion
-- Convert a coercion to a MCoercion,
-- It's not clear whether or not isReflexiveCo would be better here
--    See #19815 for a bit of data and dicussion on this point
coToMCo co | isReflCo co = MRefl
           | otherwise   = MCo co

checkReflexiveMCo :: MCoercion -> MCoercion
checkReflexiveMCo MRefl                       = MRefl
checkReflexiveMCo (MCo co) | isReflexiveCo co = MRefl
                           | otherwise        = MCo co

-- | Tests if this MCoercion is obviously generalized reflexive
-- Guaranteed to work very quickly.
isGReflMCo :: MCoercion -> Bool
isGReflMCo MRefl = True
isGReflMCo (MCo co) | isGReflCo co = True
isGReflMCo _ = False

-- | Make a generalized reflexive coercion
mkGReflCo :: Role -> Type -> MCoercionN -> Coercion
mkGReflCo r ty mco
  | isGReflMCo mco = if r == Nominal then Refl ty
                     else GRefl r ty MRefl
  | otherwise    = GRefl r ty mco

-- | Compose two MCoercions via transitivity
mkTransMCo :: MCoercion -> MCoercion -> MCoercion
mkTransMCo MRefl     co2       = co2
mkTransMCo co1       MRefl     = co1
mkTransMCo (MCo co1) (MCo co2) = MCo (mkTransCo co1 co2)

mkTransMCoL :: MCoercion -> Coercion -> MCoercion
mkTransMCoL MRefl     co2 = coToMCo co2
mkTransMCoL (MCo co1) co2 = MCo (mkTransCo co1 co2)

mkTransMCoR :: Coercion -> MCoercion -> MCoercion
mkTransMCoR co1 MRefl     = coToMCo co1
mkTransMCoR co1 (MCo co2) = MCo (mkTransCo co1 co2)

-- | Get the reverse of an 'MCoercion'
mkSymMCo :: MCoercion -> MCoercion
mkSymMCo MRefl    = MRefl
mkSymMCo (MCo co) = MCo (mkSymCo co)

-- | Cast a type by an 'MCoercion'
mkCastTyMCo :: Type -> MCoercion -> Type
mkCastTyMCo ty MRefl    = ty
mkCastTyMCo ty (MCo co) = ty `mkCastTy` co

mkHomoForAllMCo :: TyCoVar -> MCoercion -> MCoercion
mkHomoForAllMCo _   MRefl    = MRefl
mkHomoForAllMCo tcv (MCo co) = MCo (mkHomoForAllCos [tcv] co)

mkPiMCos :: [Var] -> MCoercion -> MCoercion
mkPiMCos _ MRefl = MRefl
mkPiMCos vs (MCo co) = MCo (mkPiCos Representational vs co)

mkFunResMCo :: Scaled Type -> MCoercionR -> MCoercionR
mkFunResMCo _      MRefl    = MRefl
mkFunResMCo arg_ty (MCo co) = MCo (mkFunResCo Representational arg_ty co)

mkGReflLeftMCo :: Role -> Type -> MCoercionN -> Coercion
mkGReflLeftMCo r ty MRefl    = mkReflCo r ty
mkGReflLeftMCo r ty (MCo co) = mkGReflLeftCo r ty co

mkGReflRightMCo :: Role -> Type -> MCoercionN -> Coercion
mkGReflRightMCo r ty MRefl    = mkReflCo r ty
mkGReflRightMCo r ty (MCo co) = mkGReflRightCo r ty co

-- | Like 'mkCoherenceRightCo', but with an 'MCoercion'
mkCoherenceRightMCo :: Role -> Type -> MCoercionN -> Coercion -> Coercion
mkCoherenceRightMCo _ _  MRefl    co2 = co2
mkCoherenceRightMCo r ty (MCo co) co2 = mkCoherenceRightCo r ty co co2

isReflMCo :: MCoercion -> Bool
isReflMCo MRefl = True
isReflMCo _     = False

{-
%************************************************************************
%*                                                                      *
        Destructing coercions
%*                                                                      *
%************************************************************************

Note [Function coercions]
~~~~~~~~~~~~~~~~~~~~~~~~~
Remember that
  (->) :: forall {r1} {r2}. TYPE r1 -> TYPE r2 -> TYPE LiftedRep
whose `RuntimeRep' arguments are intentionally marked inferred to
avoid type application.

Hence
  FunCo r mult co1 co2 :: (s1->t1) ~r (s2->t2)
is short for
  TyConAppCo (->) mult co_rep1 co_rep2 co1 co2
where co_rep1, co_rep2 are the coercions on the representations.
-}


-- | This breaks a 'Coercion' with type @T A B C ~ T D E F@ into
-- a list of 'Coercion's of kinds @A ~ D@, @B ~ E@ and @E ~ F@. Hence:
--
-- > decomposeCo 3 c [r1, r2, r3] = [nth r1 0 c, nth r2 1 c, nth r3 2 c]
decomposeCo :: Arity -> Coercion
            -> [Role]  -- the roles of the output coercions
                       -- this must have at least as many
                       -- entries as the Arity provided
            -> [Coercion]
decomposeCo arity co rs
  = [mkNthCo r n co | (n,r) <- [0..(arity-1)] `zip` rs ]
           -- Remember, Nth is zero-indexed

decomposeFunCo :: HasDebugCallStack
               => Role      -- Role of the input coercion
               -> Coercion  -- Input coercion
               -> (CoercionN, Coercion, Coercion)
-- Expects co :: (s1 -> t1) ~ (s2 -> t2)
-- Returns (co1 :: s1~s2, co2 :: t1~t2)
-- See Note [Function coercions] for the "3" and "4"

decomposeFunCo _ (FunCo _ w co1 co2) = (w, co1, co2)
   -- Short-circuits the calls to mkNthCo

decomposeFunCo r co = assertPpr all_ok (ppr co)
                      (mkNthCo Nominal 0 co, mkNthCo r 3 co, mkNthCo r 4 co)
  where
    Pair s1t1 s2t2 = coercionKind co
    all_ok = isFunTy s1t1 && isFunTy s2t2

{- Note [Pushing a coercion into a pi-type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have this:
    (f |> co) t1 .. tn
Then we want to push the coercion into the arguments, so as to make
progress. For example of why you might want to do so, see Note
[Respecting definitional equality] in GHC.Core.TyCo.Rep.

This is done by decomposePiCos.  Specifically, if
    decomposePiCos co [t1,..,tn] = ([co1,...,cok], cor)
then
    (f |> co) t1 .. tn   =   (f (t1 |> co1) ... (tk |> cok)) |> cor) t(k+1) ... tn

Notes:

* k can be smaller than n! That is decomposePiCos can return *fewer*
  coercions than there are arguments (ie k < n), if the kind provided
  doesn't have enough binders.

* If there is a type error, we might see
       (f |> co) t1
  where co :: (forall a. ty) ~ (ty1 -> ty2)
  Here 'co' is insoluble, but we don't want to crash in decoposePiCos.
  So decomposePiCos carefully tests both sides of the coercion to check
  they are both foralls or both arrows.  Not doing this caused #15343.
-}

decomposePiCos :: HasDebugCallStack
               => CoercionN -> Pair Type  -- Coercion and its kind
               -> [Type]
               -> ([CoercionN], CoercionN)
-- See Note [Pushing a coercion into a pi-type]
decomposePiCos orig_co (Pair orig_k1 orig_k2) orig_args
  = go [] (orig_subst,orig_k1) orig_co (orig_subst,orig_k2) orig_args
  where
    orig_subst = mkEmptyTCvSubst $ mkInScopeSet $
                 tyCoVarsOfTypes orig_args `unionVarSet` tyCoVarsOfCo orig_co

    go :: [CoercionN]      -- accumulator for argument coercions, reversed
       -> (TCvSubst,Kind)  -- Lhs kind of coercion
       -> CoercionN        -- coercion originally applied to the function
       -> (TCvSubst,Kind)  -- Rhs kind of coercion
       -> [Type]           -- Arguments to that function
       -> ([CoercionN], Coercion)
    -- Invariant:  co :: subst1(k1) ~ subst2(k2)

    go acc_arg_cos (subst1,k1) co (subst2,k2) (ty:tys)
      | Just (a, t1) <- splitForAllTyCoVar_maybe k1
      , Just (b, t2) <- splitForAllTyCoVar_maybe k2
        -- know     co :: (forall a:s1.t1) ~ (forall b:s2.t2)
        --    function :: forall a:s1.t1   (the function is not passed to decomposePiCos)
        --           a :: s1
        --           b :: s2
        --          ty :: s2
        -- need arg_co :: s2 ~ s1
        --      res_co :: t1[ty |> arg_co / a] ~ t2[ty / b]
      = let arg_co  = mkNthCo Nominal 0 (mkSymCo co)
            res_co  = mkInstCo co (mkGReflLeftCo Nominal ty arg_co)
            subst1' = extendTCvSubst subst1 a (ty `CastTy` arg_co)
            subst2' = extendTCvSubst subst2 b ty
        in
        go (arg_co : acc_arg_cos) (subst1', t1) res_co (subst2', t2) tys

      | Just (_w1, _s1, t1) <- splitFunTy_maybe k1
      , Just (_w1, _s2, t2) <- splitFunTy_maybe k2
        -- know     co :: (s1 -> t1) ~ (s2 -> t2)
        --    function :: s1 -> t1
        --          ty :: s2
        -- need arg_co :: s2 ~ s1
        --      res_co :: t1 ~ t2
      = let (_, sym_arg_co, res_co) = decomposeFunCo Nominal co
            -- It should be fine to ignore the multiplicity bit of the coercion
            -- for a Nominal coercion.
            arg_co               = mkSymCo sym_arg_co
        in
        go (arg_co : acc_arg_cos) (subst1,t1) res_co (subst2,t2) tys

      | not (isEmptyTCvSubst subst1) || not (isEmptyTCvSubst subst2)
      = go acc_arg_cos (zapTCvSubst subst1, substTy subst1 k1)
                       co
                       (zapTCvSubst subst2, substTy subst1 k2)
                       (ty:tys)

      -- tys might not be empty, if the left-hand type of the original coercion
      -- didn't have enough binders
    go acc_arg_cos _ki1 co _ki2 _tys = (reverse acc_arg_cos, co)

-- | Attempts to obtain the type variable underlying a 'Coercion'
getCoVar_maybe :: Coercion -> Maybe CoVar
getCoVar_maybe (CoVarCo cv) = Just cv
getCoVar_maybe _            = Nothing

-- | Attempts to tease a coercion apart into a type constructor and the application
-- of a number of coercion arguments to that constructor
splitTyConAppCo_maybe :: Coercion -> Maybe (TyCon, [Coercion])
splitTyConAppCo_maybe co
  | Just (ty, r) <- isReflCo_maybe co
  = do { (tc, tys) <- splitTyConApp_maybe ty
       ; let args = zipWith mkReflCo (tyConRolesX r tc) tys
       ; return (tc, args) }
splitTyConAppCo_maybe (TyConAppCo _ tc cos) = Just (tc, cos)
splitTyConAppCo_maybe (FunCo _ w arg res)     = Just (funTyCon, cos)
  where cos = [w, mkRuntimeRepCo arg, mkRuntimeRepCo res, arg, res]
splitTyConAppCo_maybe _                     = Nothing

multToCo :: Mult -> Coercion
multToCo r = mkNomReflCo r

-- first result has role equal to input; third result is Nominal
splitAppCo_maybe :: Coercion -> Maybe (Coercion, Coercion)
-- ^ Attempt to take a coercion application apart.
splitAppCo_maybe (AppCo co arg) = Just (co, arg)
splitAppCo_maybe (TyConAppCo r tc args)
  | args `lengthExceeds` tyConArity tc
  , Just (args', arg') <- snocView args
  = Just ( mkTyConAppCo r tc args', arg' )

  | not (mustBeSaturated tc)
    -- Never create unsaturated type family apps!
  , Just (args', arg') <- snocView args
  , Just arg'' <- setNominalRole_maybe (nthRole r tc (length args')) arg'
  = Just ( mkTyConAppCo r tc args', arg'' )
       -- Use mkTyConAppCo to preserve the invariant
       --  that identity coercions are always represented by Refl

splitAppCo_maybe co
  | Just (ty, r) <- isReflCo_maybe co
  , Just (ty1, ty2) <- splitAppTy_maybe ty
  = Just (mkReflCo r ty1, mkNomReflCo ty2)
splitAppCo_maybe _ = Nothing

-- Only used in specialise/Rules
splitFunCo_maybe :: Coercion -> Maybe (Coercion, Coercion)
splitFunCo_maybe (FunCo _ _ arg res) = Just (arg, res)
splitFunCo_maybe _ = Nothing

splitForAllCo_maybe :: Coercion -> Maybe (TyCoVar, Coercion, Coercion)
splitForAllCo_maybe (ForAllCo tv k_co co) = Just (tv, k_co, co)
splitForAllCo_maybe _                     = Nothing

-- | Like 'splitForAllCo_maybe', but only returns Just for tyvar binder
splitForAllCo_ty_maybe :: Coercion -> Maybe (TyVar, Coercion, Coercion)
splitForAllCo_ty_maybe (ForAllCo tv k_co co)
  | isTyVar tv = Just (tv, k_co, co)
splitForAllCo_ty_maybe _ = Nothing

-- | Like 'splitForAllCo_maybe', but only returns Just for covar binder
splitForAllCo_co_maybe :: Coercion -> Maybe (CoVar, Coercion, Coercion)
splitForAllCo_co_maybe (ForAllCo cv k_co co)
  | isCoVar cv = Just (cv, k_co, co)
splitForAllCo_co_maybe _ = Nothing

-------------------------------------------------------
-- and some coercion kind stuff

coVarLType, coVarRType :: HasDebugCallStack => CoVar -> Type
coVarLType cv | (_, _, ty1, _, _) <- coVarKindsTypesRole cv = ty1
coVarRType cv | (_, _, _, ty2, _) <- coVarKindsTypesRole cv = ty2

coVarTypes :: HasDebugCallStack => CoVar -> Pair Type
coVarTypes cv
  | (_, _, ty1, ty2, _) <- coVarKindsTypesRole cv
  = Pair ty1 ty2

coVarKindsTypesRole :: HasDebugCallStack => CoVar -> (Kind,Kind,Type,Type,Role)
coVarKindsTypesRole cv
 | Just (tc, [k1,k2,ty1,ty2]) <- splitTyConApp_maybe (varType cv)
 = (k1, k2, ty1, ty2, eqTyConRole tc)
 | otherwise
 = pprPanic "coVarKindsTypesRole, non coercion variable"
            (ppr cv $$ ppr (varType cv))

coVarKind :: CoVar -> Type
coVarKind cv
  = assert (isCoVar cv )
    varType cv

coVarRole :: CoVar -> Role
coVarRole cv
  = eqTyConRole (case tyConAppTyCon_maybe (varType cv) of
                   Just tc0 -> tc0
                   Nothing  -> pprPanic "coVarRole: not tyconapp" (ppr cv))

eqTyConRole :: TyCon -> Role
-- Given (~#) or (~R#) return the Nominal or Representational respectively
eqTyConRole tc
  | tc `hasKey` eqPrimTyConKey
  = Nominal
  | tc `hasKey` eqReprPrimTyConKey
  = Representational
  | otherwise
  = pprPanic "eqTyConRole: unknown tycon" (ppr tc)

-- | Given a coercion @co1 :: (a :: TYPE r1) ~ (b :: TYPE r2)@,
-- produce a coercion @rep_co :: r1 ~ r2@.
mkRuntimeRepCo :: HasDebugCallStack => Coercion -> Coercion
mkRuntimeRepCo co
  = mkNthCo Nominal 0 kind_co
  where
    kind_co = mkKindCo co  -- kind_co :: TYPE r1 ~ TYPE r2
                           -- (up to silliness with Constraint)

isReflCoVar_maybe :: Var -> Maybe Coercion
-- If cv :: t~t then isReflCoVar_maybe cv = Just (Refl t)
-- Works on all kinds of Vars, not just CoVars
isReflCoVar_maybe cv
  | isCoVar cv
  , Pair ty1 ty2 <- coVarTypes cv
  , ty1 `eqType` ty2
  = Just (mkReflCo (coVarRole cv) ty1)
  | otherwise
  = Nothing

-- | Tests if this coercion is obviously a generalized reflexive coercion.
-- Guaranteed to work very quickly.
isGReflCo :: Coercion -> Bool
isGReflCo (GRefl{}) = True
isGReflCo (Refl{})  = True -- Refl ty == GRefl N ty MRefl
isGReflCo _         = False

-- | Tests if this coercion is obviously reflexive. Guaranteed to work
-- very quickly. Sometimes a coercion can be reflexive, but not obviously
-- so. c.f. 'isReflexiveCo'
isReflCo :: Coercion -> Bool
isReflCo (Refl{}) = True
isReflCo (GRefl _ _ mco) | isGReflMCo mco = True
isReflCo _ = False

-- | Returns the type coerced if this coercion is a generalized reflexive
-- coercion. Guaranteed to work very quickly.
isGReflCo_maybe :: Coercion -> Maybe (Type, Role)
isGReflCo_maybe (GRefl r ty _) = Just (ty, r)
isGReflCo_maybe (Refl ty)      = Just (ty, Nominal)
isGReflCo_maybe _ = Nothing

-- | Returns the type coerced if this coercion is reflexive. Guaranteed
-- to work very quickly. Sometimes a coercion can be reflexive, but not
-- obviously so. c.f. 'isReflexiveCo_maybe'
isReflCo_maybe :: Coercion -> Maybe (Type, Role)
isReflCo_maybe (Refl ty) = Just (ty, Nominal)
isReflCo_maybe (GRefl r ty mco) | isGReflMCo mco = Just (ty, r)
isReflCo_maybe _ = Nothing

-- | Slowly checks if the coercion is reflexive. Don't call this in a loop,
-- as it walks over the entire coercion.
isReflexiveCo :: Coercion -> Bool
isReflexiveCo = isJust . isReflexiveCo_maybe

-- | Extracts the coerced type from a reflexive coercion. This potentially
-- walks over the entire coercion, so avoid doing this in a loop.
isReflexiveCo_maybe :: Coercion -> Maybe (Type, Role)
isReflexiveCo_maybe (Refl ty) = Just (ty, Nominal)
isReflexiveCo_maybe (GRefl r ty mco) | isGReflMCo mco = Just (ty, r)
isReflexiveCo_maybe co
  | ty1 `eqType` ty2
  = Just (ty1, r)
  | otherwise
  = Nothing
  where (Pair ty1 ty2, r) = coercionKindRole co


{-
%************************************************************************
%*                                                                      *
            Building coercions
%*                                                                      *
%************************************************************************

These "smart constructors" maintain the invariants listed in the definition
of Coercion, and they perform very basic optimizations.

Note [Role twiddling functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a plethora of functions for twiddling roles:

mkSubCo: Requires a nominal input coercion and always produces a
representational output. This is used when you (the programmer) are sure you
know exactly that role you have and what you want.

downgradeRole_maybe: This function takes both the input role and the output role
as parameters. (The *output* role comes first!) It can only *downgrade* a
role -- that is, change it from N to R or P, or from R to P. This one-way
behavior is why there is the "_maybe". If an upgrade is requested, this
function produces Nothing. This is used when you need to change the role of a
coercion, but you're not sure (as you're writing the code) of which roles are
involved.

This function could have been written using coercionRole to ascertain the role
of the input. But, that function is recursive, and the caller of downgradeRole_maybe
often knows the input role. So, this is more efficient.

downgradeRole: This is just like downgradeRole_maybe, but it panics if the
conversion isn't a downgrade.

setNominalRole_maybe: This is the only function that can *upgrade* a coercion.
The result (if it exists) is always Nominal. The input can be at any role. It
works on a "best effort" basis, as it should never be strictly necessary to
upgrade a coercion during compilation. It is currently only used within GHC in
splitAppCo_maybe. In order to be a proper inverse of mkAppCo, the second
coercion that splitAppCo_maybe returns must be nominal. But, it's conceivable
that splitAppCo_maybe is operating over a TyConAppCo that uses a
representational coercion. Hence the need for setNominalRole_maybe.
splitAppCo_maybe, in turn, is used only within coercion optimization -- thus,
it is not absolutely critical that setNominalRole_maybe be complete.

Note that setNominalRole_maybe will never upgrade a phantom UnivCo. Phantom
UnivCos are perfectly type-safe, whereas representational and nominal ones are
not. (Nominal ones are no worse than representational ones, so this function *will*
change a UnivCo Representational to a UnivCo Nominal.)

Conal Elliott also came across a need for this function while working with the
GHC API, as he was decomposing Core casts. The Core casts use representational
coercions, as they must, but his use case required nominal coercions (he was
building a GADT). So, that's why this function is exported from this module.

One might ask: shouldn't downgradeRole_maybe just use setNominalRole_maybe as
appropriate? I (Richard E.) have decided not to do this, because upgrading a
role is bizarre and a caller should have to ask for this behavior explicitly.

-}

-- | Make a reflexive coercion
mkReflCo :: Role -> Type -> Coercion
mkReflCo Nominal ty = Refl ty
mkReflCo r       ty = GRefl r ty MRefl

-- | Make a representational reflexive coercion
mkRepReflCo :: Type -> Coercion
mkRepReflCo ty = GRefl Representational ty MRefl

-- | Make a nominal reflexive coercion
mkNomReflCo :: Type -> Coercion
mkNomReflCo = Refl

-- | Apply a type constructor to a list of coercions. It is the
-- caller's responsibility to get the roles correct on argument coercions.
mkTyConAppCo :: HasDebugCallStack => Role -> TyCon -> [Coercion] -> Coercion
mkTyConAppCo r tc cos
  | [w, _rep1, _rep2, co1, co2] <- cos   -- See Note [Function coercions]
  , isFunTyCon tc
  = -- (a :: TYPE ra) -> (b :: TYPE rb)  ~  (c :: TYPE rc) -> (d :: TYPE rd)
    -- rep1 :: ra  ~  rc        rep2 :: rb  ~  rd
    -- co1  :: a   ~  c         co2  :: b   ~  d
    mkFunCo r w co1 co2

               -- Expand type synonyms
  | Just (tv_co_prs, rhs_ty, leftover_cos) <- expandSynTyCon_maybe tc cos
  = mkAppCos (liftCoSubst r (mkLiftingContext tv_co_prs) rhs_ty) leftover_cos

  | Just tys_roles <- traverse isReflCo_maybe cos
  = mkReflCo r (mkTyConApp tc (map fst tys_roles))
  -- See Note [Refl invariant]

  | otherwise = TyConAppCo r tc cos

-- | Build a function 'Coercion' from two other 'Coercion's. That is,
-- given @co1 :: a ~ b@ and @co2 :: x ~ y@ produce @co :: (a -> x) ~ (b -> y)@.
mkFunCo :: Role -> CoercionN -> Coercion -> Coercion -> Coercion
mkFunCo r w co1 co2
    -- See Note [Refl invariant]
  | Just (ty1, _) <- isReflCo_maybe co1
  , Just (ty2, _) <- isReflCo_maybe co2
  , Just (w, _) <- isReflCo_maybe w
  = mkReflCo r (mkVisFunTy w ty1 ty2)
  | otherwise = FunCo r w co1 co2

-- | Apply a 'Coercion' to another 'Coercion'.
-- The second coercion must be Nominal, unless the first is Phantom.
-- If the first is Phantom, then the second can be either Phantom or Nominal.
mkAppCo :: Coercion     -- ^ :: t1 ~r t2
        -> Coercion     -- ^ :: s1 ~N s2, where s1 :: k1, s2 :: k2
        -> Coercion     -- ^ :: t1 s1 ~r t2 s2
mkAppCo co arg
  | Just (ty1, r) <- isReflCo_maybe co
  , Just (ty2, _) <- isReflCo_maybe arg
  = mkReflCo r (mkAppTy ty1 ty2)

  | Just (ty1, r) <- isReflCo_maybe co
  , Just (tc, tys) <- splitTyConApp_maybe ty1
    -- Expand type synonyms; a TyConAppCo can't have a type synonym (#9102)
  = mkTyConAppCo r tc (zip_roles (tyConRolesX r tc) tys)
  where
    zip_roles (r1:_)  []            = [downgradeRole r1 Nominal arg]
    zip_roles (r1:rs) (ty1:tys)     = mkReflCo r1 ty1 : zip_roles rs tys
    zip_roles _       _             = panic "zip_roles" -- but the roles are infinite...

mkAppCo (TyConAppCo r tc args) arg
  = case r of
      Nominal          -> mkTyConAppCo Nominal tc (args ++ [arg])
      Representational -> mkTyConAppCo Representational tc (args ++ [arg'])
        where new_role = (tyConRolesRepresentational tc) !! (length args)
              arg'     = downgradeRole new_role Nominal arg
      Phantom          -> mkTyConAppCo Phantom tc (args ++ [toPhantomCo arg])
mkAppCo co arg = AppCo co  arg
-- Note, mkAppCo is careful to maintain invariants regarding
-- where Refl constructors appear; see the comments in the definition
-- of Coercion and the Note [Refl invariant] in GHC.Core.TyCo.Rep.

-- | Applies multiple 'Coercion's to another 'Coercion', from left to right.
-- See also 'mkAppCo'.
mkAppCos :: Coercion
         -> [Coercion]
         -> Coercion
mkAppCos co1 cos = foldl' mkAppCo co1 cos

{- Note [Unused coercion variable in ForAllCo]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Note [Unused coercion variable in ForAllTy] in GHC.Core.TyCo.Rep for the
motivation for checking coercion variable in types.
To lift the design choice to (ForAllCo cv kind_co body_co), we have two options:

(1) In mkForAllCo, we check whether cv is a coercion variable
    and whether it is not used in body_co. If so we construct a FunCo.
(2) We don't do this check in mkForAllCo.
    In coercionKind, we use mkTyCoForAllTy to perform the check and construct
    a FunTy when necessary.

We chose (2) for two reasons:

* for a coercion, all that matters is its kind, So ForAllCo or FunCo does not
  make a difference.
* even if cv occurs in body_co, it is possible that cv does not occur in the kind
  of body_co. Therefore the check in coercionKind is inevitable.

The last wrinkle is that there are restrictions around the use of the cv in the
coercion, as described in Section 5.8.5.2 of Richard's thesis. The idea is that
we cannot prove that the type system is consistent with unrestricted use of this
cv; the consistency proof uses an untyped rewrite relation that works over types
with all coercions and casts removed. So, we can allow the cv to appear only in
positions that are erased. As an approximation of this (and keeping close to the
published theory), we currently allow the cv only within the type in a Refl node
and under a GRefl node (including in the Coercion stored in a GRefl). It's
possible other places are OK, too, but this is a safe approximation.

Sadly, with heterogeneous equality, this restriction might be able to be violated;
Richard's thesis is unable to prove that it isn't. Specifically, the liftCoSubst
function might create an invalid coercion. Because a violation of the
restriction might lead to a program that "goes wrong", it is checked all the time,
even in a production compiler and without -dcore-list. We *have* proved that the
problem does not occur with homogeneous equality, so this check can be dropped
once ~# is made to be homogeneous.
-}


-- | Make a Coercion from a tycovar, a kind coercion, and a body coercion.
-- The kind of the tycovar should be the left-hand kind of the kind coercion.
-- See Note [Unused coercion variable in ForAllCo]
mkForAllCo :: TyCoVar -> CoercionN -> Coercion -> Coercion
mkForAllCo v kind_co co
  | assert (varType v `eqType` (pFst $ coercionKind kind_co)) True
  , assert (isTyVar v || almostDevoidCoVarOfCo v co) True
  , Just (ty, r) <- isReflCo_maybe co
  , isGReflCo kind_co
  = mkReflCo r (mkTyCoInvForAllTy v ty)
  | otherwise
  = ForAllCo v kind_co co

-- | Like 'mkForAllCo', but the inner coercion shouldn't be an obvious
-- reflexive coercion. For example, it is guaranteed in 'mkForAllCos'.
-- The kind of the tycovar should be the left-hand kind of the kind coercion.
mkForAllCo_NoRefl :: TyCoVar -> CoercionN -> Coercion -> Coercion
mkForAllCo_NoRefl v kind_co co
  | assert (varType v `eqType` (pFst $ coercionKind kind_co)) True
  , assert (isTyVar v || almostDevoidCoVarOfCo v co) True
  , assert (not (isReflCo co)) True
  , isCoVar v
  , not (v `elemVarSet` tyCoVarsOfCo co)
  = FunCo (coercionRole co) (multToCo Many) kind_co co
      -- Functions from coercions are always unrestricted
  | otherwise
  = ForAllCo v kind_co co

-- | Make nested ForAllCos
mkForAllCos :: [(TyCoVar, CoercionN)] -> Coercion -> Coercion
mkForAllCos bndrs co
  | Just (ty, r ) <- isReflCo_maybe co
  = let (refls_rev'd, non_refls_rev'd) = span (isReflCo . snd) (reverse bndrs) in
    foldl' (flip $ uncurry mkForAllCo_NoRefl)
           (mkReflCo r (mkTyCoInvForAllTys (reverse (map fst refls_rev'd)) ty))
           non_refls_rev'd
  | otherwise
  = foldr (uncurry mkForAllCo_NoRefl) co bndrs

-- | Make a Coercion quantified over a type/coercion variable;
-- the variable has the same type in both sides of the coercion
mkHomoForAllCos :: [TyCoVar] -> Coercion -> Coercion
mkHomoForAllCos vs co
  | Just (ty, r) <- isReflCo_maybe co
  = mkReflCo r (mkTyCoInvForAllTys vs ty)
  | otherwise
  = mkHomoForAllCos_NoRefl vs co

-- | Like 'mkHomoForAllCos', but the inner coercion shouldn't be an obvious
-- reflexive coercion. For example, it is guaranteed in 'mkHomoForAllCos'.
mkHomoForAllCos_NoRefl :: [TyCoVar] -> Coercion -> Coercion
mkHomoForAllCos_NoRefl vs orig_co
  = assert (not (isReflCo orig_co))
    foldr go orig_co vs
  where
    go v co = mkForAllCo_NoRefl v (mkNomReflCo (varType v)) co

mkCoVarCo :: CoVar -> Coercion
-- cv :: s ~# t
-- See Note [mkCoVarCo]
mkCoVarCo cv = CoVarCo cv

mkCoVarCos :: [CoVar] -> [Coercion]
mkCoVarCos = map mkCoVarCo

{- Note [mkCoVarCo]
~~~~~~~~~~~~~~~~~~~
In the past, mkCoVarCo optimised (c :: t~t) to (Refl t).  That is
valid (although see Note [Unbound RULE binders] in GHC.Core.Rules), but
it's a relatively expensive test and perhaps better done in
optCoercion.  Not a big deal either way.
-}

-- | Extract a covar, if possible. This check is dirty. Be ashamed
-- of yourself. (It's dirty because it cares about the structure of
-- a coercion, which is morally reprehensible.)
isCoVar_maybe :: Coercion -> Maybe CoVar
isCoVar_maybe (CoVarCo cv) = Just cv
isCoVar_maybe _            = Nothing

mkAxInstCo :: Role -> CoAxiom br -> BranchIndex -> [Type] -> [Coercion]
           -> Coercion
-- mkAxInstCo can legitimately be called over-staturated;
-- i.e. with more type arguments than the coercion requires
mkAxInstCo role ax index tys cos
  | arity == n_tys = downgradeRole role ax_role $
                     mkAxiomInstCo ax_br index (rtys `chkAppend` cos)
  | otherwise      = assert (arity < n_tys) $
                     downgradeRole role ax_role $
                     mkAppCos (mkAxiomInstCo ax_br index
                                             (ax_args `chkAppend` cos))
                              leftover_args
  where
    n_tys         = length tys
    ax_br         = toBranchedAxiom ax
    branch        = coAxiomNthBranch ax_br index
    tvs           = coAxBranchTyVars branch
    arity         = length tvs
    arg_roles     = coAxBranchRoles branch
    rtys          = zipWith mkReflCo (arg_roles ++ repeat Nominal) tys
    (ax_args, leftover_args)
                  = splitAt arity rtys
    ax_role       = coAxiomRole ax

-- worker function
mkAxiomInstCo :: CoAxiom Branched -> BranchIndex -> [Coercion] -> Coercion
mkAxiomInstCo ax index args
  = assert (args `lengthIs` coAxiomArity ax index) $
    AxiomInstCo ax index args

-- to be used only with unbranched axioms
mkUnbranchedAxInstCo :: Role -> CoAxiom Unbranched
                     -> [Type] -> [Coercion] -> Coercion
mkUnbranchedAxInstCo role ax tys cos
  = mkAxInstCo role ax 0 tys cos

mkAxInstRHS :: CoAxiom br -> BranchIndex -> [Type] -> [Coercion] -> Type
-- Instantiate the axiom with specified types,
-- returning the instantiated RHS
-- A companion to mkAxInstCo:
--    mkAxInstRhs ax index tys = snd (coercionKind (mkAxInstCo ax index tys))
mkAxInstRHS ax index tys cos
  = assert (tvs `equalLength` tys1) $
    mkAppTys rhs' tys2
  where
    branch       = coAxiomNthBranch ax index
    tvs          = coAxBranchTyVars branch
    cvs          = coAxBranchCoVars branch
    (tys1, tys2) = splitAtList tvs tys
    rhs'         = substTyWith tvs tys1 $
                   substTyWithCoVars cvs cos $
                   coAxBranchRHS branch

mkUnbranchedAxInstRHS :: CoAxiom Unbranched -> [Type] -> [Coercion] -> Type
mkUnbranchedAxInstRHS ax = mkAxInstRHS ax 0

-- | Return the left-hand type of the axiom, when the axiom is instantiated
-- at the types given.
mkAxInstLHS :: CoAxiom br -> BranchIndex -> [Type] -> [Coercion] -> Type
mkAxInstLHS ax index tys cos
  = assert (tvs `equalLength` tys1) $
    mkTyConApp fam_tc (lhs_tys `chkAppend` tys2)
  where
    branch       = coAxiomNthBranch ax index
    tvs          = coAxBranchTyVars branch
    cvs          = coAxBranchCoVars branch
    (tys1, tys2) = splitAtList tvs tys
    lhs_tys      = substTysWith tvs tys1 $
                   substTysWithCoVars cvs cos $
                   coAxBranchLHS branch
    fam_tc       = coAxiomTyCon ax

-- | Instantiate the left-hand side of an unbranched axiom
mkUnbranchedAxInstLHS :: CoAxiom Unbranched -> [Type] -> [Coercion] -> Type
mkUnbranchedAxInstLHS ax = mkAxInstLHS ax 0

-- | Make a coercion from a coercion hole
mkHoleCo :: CoercionHole -> Coercion
mkHoleCo h = HoleCo h

-- | Make a universal coercion between two arbitrary types.
mkUnivCo :: UnivCoProvenance
         -> Role       -- ^ role of the built coercion, "r"
         -> Type       -- ^ t1 :: k1
         -> Type       -- ^ t2 :: k2
         -> Coercion   -- ^ :: t1 ~r t2
mkUnivCo prov role ty1 ty2
  | ty1 `eqType` ty2 = mkReflCo role ty1
  | otherwise        = UnivCo prov role ty1 ty2

-- | Create a symmetric version of the given 'Coercion' that asserts
--   equality between the same types but in the other "direction", so
--   a kind of @t1 ~ t2@ becomes the kind @t2 ~ t1@.
mkSymCo :: Coercion -> Coercion

-- Do a few simple optimizations, but don't bother pushing occurrences
-- of symmetry to the leaves; the optimizer will take care of that.
mkSymCo co | isReflCo co          = co
mkSymCo    (SymCo co)             = co
mkSymCo    (SubCo (SymCo co))     = SubCo co
mkSymCo co                        = SymCo co

-- | Create a new 'Coercion' by composing the two given 'Coercion's transitively.
--   (co1 ; co2)
mkTransCo :: Coercion -> Coercion -> Coercion
mkTransCo co1 co2 | isReflCo co1 = co2
                  | isReflCo co2 = co1
mkTransCo (GRefl r t1 (MCo co1)) (GRefl _ _ (MCo co2))
  = GRefl r t1 (MCo $ mkTransCo co1 co2)
mkTransCo co1 co2                = TransCo co1 co2

mkNthCo :: HasDebugCallStack
        => Role  -- The role of the coercion you're creating
        -> Int   -- Zero-indexed
        -> Coercion
        -> Coercion
mkNthCo r n co
  = assertPpr good_call bad_call_msg $
    go n co
  where
    Pair ty1 ty2 = coercionKind co

    go 0 co
      | Just (ty, _) <- isReflCo_maybe co
      , Just (tv, _) <- splitForAllTyCoVar_maybe ty
      = -- works for both tyvar and covar
        assert (r == Nominal) $
        mkNomReflCo (varType tv)

    go n co
      | Just (ty, r0) <- isReflCo_maybe co
      , let tc = tyConAppTyCon ty
      = assertPpr (ok_tc_app ty n) (ppr n $$ ppr ty) $
        assert (nthRole r0 tc n == r) $
        mkReflCo r (tyConAppArgN n ty)
      where ok_tc_app :: Type -> Int -> Bool
            ok_tc_app ty n
              | Just (_, tys) <- splitTyConApp_maybe ty
              = tys `lengthExceeds` n
              | isForAllTy ty  -- nth:0 pulls out a kind coercion from a hetero forall
              = n == 0
              | otherwise
              = False

    go 0 (ForAllCo _ kind_co _)
      = assert (r == Nominal)
        kind_co
      -- If co :: (forall a1:k1. t1) ~ (forall a2:k2. t2)
      -- then (nth 0 co :: k1 ~N k2)
      -- If co :: (forall a1:t1 ~ t2. t1) ~ (forall a2:t3 ~ t4. t2)
      -- then (nth 0 co :: (t1 ~ t2) ~N (t3 ~ t4))

    go n (FunCo _ w arg res)
      = mkNthCoFunCo n w arg res

    go n (TyConAppCo r0 tc arg_cos) = assertPpr (r == nthRole r0 tc n)
                                                  (vcat [ ppr tc
                                                        , ppr arg_cos
                                                        , ppr r0
                                                        , ppr n
                                                        , ppr r ]) $
                                             arg_cos `getNth` n

    go n (SymCo co)  -- Recurse, hoping to get to a TyConAppCo or FunCo
      = mkSymCo (go n co)

    go n co
      = NthCo r n co

    -- Assertion checking
    bad_call_msg = vcat [ text "Coercion =" <+> ppr co
                        , text "LHS ty =" <+> ppr ty1
                        , text "RHS ty =" <+> ppr ty2
                        , text "n =" <+> ppr n, text "r =" <+> ppr r
                        , text "coercion role =" <+> ppr (coercionRole co) ]
    good_call
      -- If the Coercion passed in is between forall-types, then the Int must
      -- be 0 and the role must be Nominal.
      | Just (_tv1, _) <- splitForAllTyCoVar_maybe ty1
      , Just (_tv2, _) <- splitForAllTyCoVar_maybe ty2
      = n == 0 && r == Nominal

      -- If the Coercion passed in is between T tys and T tys', then the Int
      -- must be less than the length of tys/tys' (which must be the same
      -- lengths).
      --
      -- If the role of the Coercion is nominal, then the role passed in must
      -- be nominal. If the role of the Coercion is representational, then the
      -- role passed in must be tyConRolesRepresentational T !! n. If the role
      -- of the Coercion is Phantom, then the role passed in must be Phantom.
      --
      -- See also Note [NthCo Cached Roles] if you're wondering why it's
      -- blaringly obvious that we should be *computing* this role instead of
      -- passing it in.
      | Just (tc1, tys1) <- splitTyConApp_maybe ty1
      , Just (tc2, tys2) <- splitTyConApp_maybe ty2
      , tc1 == tc2
      = let len1 = length tys1
            len2 = length tys2
            good_role = case coercionRole co of
                          Nominal -> r == Nominal
                          Representational -> r == (tyConRolesRepresentational tc1 !! n)
                          Phantom -> r == Phantom
        in len1 == len2 && n < len1 && good_role

      | otherwise
      = True

-- | Extract the nth field of a FunCo
mkNthCoFunCo :: Int         -- ^ "n"
             -> CoercionN   -- ^ multiplicity coercion
             -> Coercion    -- ^ argument coercion
             -> Coercion    -- ^ result coercion
             -> Coercion    -- ^ nth coercion from a FunCo
-- See Note [Function coercions]
-- If FunCo _ mult arg_co res_co ::   (s1:TYPE sk1 :mult-> s2:TYPE sk2)
--                                  ~ (t1:TYPE tk1 :mult-> t2:TYPE tk2)
-- Then we want to behave as if co was
--    TyConAppCo mult argk_co resk_co arg_co res_co
-- where
--    argk_co :: sk1 ~ tk1  =  mkNthCo 0 (mkKindCo arg_co)
--    resk_co :: sk2 ~ tk2  =  mkNthCo 0 (mkKindCo res_co)
--                             i.e. mkRuntimeRepCo
mkNthCoFunCo n w co1 co2 = case n of
  0 -> w
  1 -> mkRuntimeRepCo co1
  2 -> mkRuntimeRepCo co2
  3 -> co1
  4 -> co2
  _ -> pprPanic "mkNthCo(FunCo)" (ppr n $$ ppr w $$ ppr co1 $$ ppr co2)

-- | If you're about to call @mkNthCo r n co@, then @r@ should be
-- whatever @nthCoRole n co@ returns.
nthCoRole :: Int -> Coercion -> Role
nthCoRole n co
  | Just (tc, _) <- splitTyConApp_maybe lty
  = nthRole r tc n

  | Just _ <- splitForAllTyCoVar_maybe lty
  = Nominal

  | otherwise
  = pprPanic "nthCoRole" (ppr co)

  where
    lty = coercionLKind co
    r   = coercionRole co

mkLRCo :: LeftOrRight -> Coercion -> Coercion
mkLRCo lr co
  | Just (ty, eq) <- isReflCo_maybe co
  = mkReflCo eq (pickLR lr (splitAppTy ty))
  | otherwise
  = LRCo lr co

-- | Instantiates a 'Coercion'.
mkInstCo :: Coercion -> Coercion -> Coercion
mkInstCo (ForAllCo tcv _kind_co body_co) co
  | Just (arg, _) <- isReflCo_maybe co
      -- works for both tyvar and covar
  = substCoUnchecked (zipTCvSubst [tcv] [arg]) body_co
mkInstCo co arg = InstCo co arg

-- | Given @ty :: k1@, @co :: k1 ~ k2@,
-- produces @co' :: ty ~r (ty |> co)@
mkGReflRightCo :: Role -> Type -> CoercionN -> Coercion
mkGReflRightCo r ty co
  | isGReflCo co = mkReflCo r ty
    -- the kinds of @k1@ and @k2@ are the same, thus @isGReflCo@
    -- instead of @isReflCo@
  | otherwise = GRefl r ty (MCo co)

-- | Given @ty :: k1@, @co :: k1 ~ k2@,
-- produces @co' :: (ty |> co) ~r ty@
mkGReflLeftCo :: Role -> Type -> CoercionN -> Coercion
mkGReflLeftCo r ty co
  | isGReflCo co = mkReflCo r ty
    -- the kinds of @k1@ and @k2@ are the same, thus @isGReflCo@
    -- instead of @isReflCo@
  | otherwise    = mkSymCo $ GRefl r ty (MCo co)

-- | Given @ty :: k1@, @co :: k1 ~ k2@, @co2:: ty ~r ty'@,
-- produces @co' :: (ty |> co) ~r ty'
-- It is not only a utility function, but it saves allocation when co
-- is a GRefl coercion.
mkCoherenceLeftCo :: Role -> Type -> CoercionN -> Coercion -> Coercion
mkCoherenceLeftCo r ty co co2
  | isGReflCo co = co2
  | otherwise    = (mkSymCo $ GRefl r ty (MCo co)) `mkTransCo` co2

-- | Given @ty :: k1@, @co :: k1 ~ k2@, @co2:: ty' ~r ty@,
-- produces @co' :: ty' ~r (ty |> co)
-- It is not only a utility function, but it saves allocation when co
-- is a GRefl coercion.
mkCoherenceRightCo :: Role -> Type -> CoercionN -> Coercion -> Coercion
mkCoherenceRightCo r ty co co2
  | isGReflCo co = co2
  | otherwise    = co2 `mkTransCo` GRefl r ty (MCo co)

-- | Given @co :: (a :: k) ~ (b :: k')@ produce @co' :: k ~ k'@.
mkKindCo :: Coercion -> Coercion
mkKindCo co | Just (ty, _) <- isReflCo_maybe co = Refl (typeKind ty)
mkKindCo (GRefl _ _ (MCo co)) = co
mkKindCo (UnivCo (PhantomProv h) _ _ _)    = h
mkKindCo (UnivCo (ProofIrrelProv h) _ _ _) = h
mkKindCo co
  | Pair ty1 ty2 <- coercionKind co
       -- generally, calling coercionKind during coercion creation is a bad idea,
       -- as it can lead to exponential behavior. But, we don't have nested mkKindCos,
       -- so it's OK here.
  , let tk1 = typeKind ty1
        tk2 = typeKind ty2
  , tk1 `eqType` tk2
  = Refl tk1
  | otherwise
  = KindCo co

mkSubCo :: HasDebugCallStack => Coercion -> Coercion
-- Input coercion is Nominal, result is Representational
-- see also Note [Role twiddling functions]
mkSubCo (Refl ty) = GRefl Representational ty MRefl
mkSubCo (GRefl Nominal ty co) = GRefl Representational ty co
mkSubCo (TyConAppCo Nominal tc cos)
  = TyConAppCo Representational tc (applyRoles tc cos)
mkSubCo (FunCo Nominal w arg res)
  = FunCo Representational w
          (downgradeRole Representational Nominal arg)
          (downgradeRole Representational Nominal res)
mkSubCo co = assertPpr (coercionRole co == Nominal) (ppr co <+> ppr (coercionRole co)) $
             SubCo co

-- | Changes a role, but only a downgrade. See Note [Role twiddling functions]
downgradeRole_maybe :: Role   -- ^ desired role
                    -> Role   -- ^ current role
                    -> Coercion -> Maybe Coercion
-- In (downgradeRole_maybe dr cr co) it's a precondition that
--                                   cr = coercionRole co

downgradeRole_maybe Nominal          Nominal          co = Just co
downgradeRole_maybe Nominal          _                _  = Nothing

downgradeRole_maybe Representational Nominal          co = Just (mkSubCo co)
downgradeRole_maybe Representational Representational co = Just co
downgradeRole_maybe Representational Phantom          _  = Nothing

downgradeRole_maybe Phantom          Phantom          co = Just co
downgradeRole_maybe Phantom          _                co = Just (toPhantomCo co)

-- | Like 'downgradeRole_maybe', but panics if the change isn't a downgrade.
-- See Note [Role twiddling functions]
downgradeRole :: Role  -- desired role
              -> Role  -- current role
              -> Coercion -> Coercion
downgradeRole r1 r2 co
  = case downgradeRole_maybe r1 r2 co of
      Just co' -> co'
      Nothing  -> pprPanic "downgradeRole" (ppr co)

mkAxiomRuleCo :: CoAxiomRule -> [Coercion] -> Coercion
mkAxiomRuleCo = AxiomRuleCo

-- | Make a "coercion between coercions".
mkProofIrrelCo :: Role       -- ^ role of the created coercion, "r"
               -> CoercionN  -- ^ :: phi1 ~N phi2
               -> Coercion   -- ^ g1 :: phi1
               -> Coercion   -- ^ g2 :: phi2
               -> Coercion   -- ^ :: g1 ~r g2

-- if the two coercion prove the same fact, I just don't care what
-- the individual coercions are.
mkProofIrrelCo r co g  _ | isGReflCo co  = mkReflCo r (mkCoercionTy g)
  -- kco is a kind coercion, thus @isGReflCo@ rather than @isReflCo@
mkProofIrrelCo r kco        g1 g2 = mkUnivCo (ProofIrrelProv kco) r
                                             (mkCoercionTy g1) (mkCoercionTy g2)

{-
%************************************************************************
%*                                                                      *
   Roles
%*                                                                      *
%************************************************************************
-}

-- | Converts a coercion to be nominal, if possible.
-- See Note [Role twiddling functions]
setNominalRole_maybe :: Role -- of input coercion
                     -> Coercion -> Maybe Coercion
setNominalRole_maybe r co
  | r == Nominal = Just co
  | otherwise = setNominalRole_maybe_helper co
  where
    setNominalRole_maybe_helper (SubCo co)  = Just co
    setNominalRole_maybe_helper co@(Refl _) = Just co
    setNominalRole_maybe_helper (GRefl _ ty co) = Just $ GRefl Nominal ty co
    setNominalRole_maybe_helper (TyConAppCo Representational tc cos)
      = do { cos' <- zipWithM setNominalRole_maybe (tyConRolesX Representational tc) cos
           ; return $ TyConAppCo Nominal tc cos' }
    setNominalRole_maybe_helper (FunCo Representational w co1 co2)
      = do { co1' <- setNominalRole_maybe Representational co1
           ; co2' <- setNominalRole_maybe Representational co2
           ; return $ FunCo Nominal w co1' co2'
           }
    setNominalRole_maybe_helper (SymCo co)
      = SymCo <$> setNominalRole_maybe_helper co
    setNominalRole_maybe_helper (TransCo co1 co2)
      = TransCo <$> setNominalRole_maybe_helper co1 <*> setNominalRole_maybe_helper co2
    setNominalRole_maybe_helper (AppCo co1 co2)
      = AppCo <$> setNominalRole_maybe_helper co1 <*> pure co2
    setNominalRole_maybe_helper (ForAllCo tv kind_co co)
      = ForAllCo tv kind_co <$> setNominalRole_maybe_helper co
    setNominalRole_maybe_helper (NthCo _r n co)
      -- NB, this case recurses via setNominalRole_maybe, not
      -- setNominalRole_maybe_helper!
      = NthCo Nominal n <$> setNominalRole_maybe (coercionRole co) co
    setNominalRole_maybe_helper (InstCo co arg)
      = InstCo <$> setNominalRole_maybe_helper co <*> pure arg
    setNominalRole_maybe_helper (UnivCo prov _ co1 co2)
      | case prov of PhantomProv _    -> False  -- should always be phantom
                     ProofIrrelProv _ -> True   -- it's always safe
                     PluginProv _     -> False  -- who knows? This choice is conservative.
                     CorePrepProv _   -> True
      = Just $ UnivCo prov Nominal co1 co2
    setNominalRole_maybe_helper _ = Nothing

-- | Make a phantom coercion between two types. The coercion passed
-- in must be a nominal coercion between the kinds of the
-- types.
mkPhantomCo :: Coercion -> Type -> Type -> Coercion
mkPhantomCo h t1 t2
  = mkUnivCo (PhantomProv h) Phantom t1 t2

-- takes any coercion and turns it into a Phantom coercion
toPhantomCo :: Coercion -> Coercion
toPhantomCo co
  = mkPhantomCo (mkKindCo co) ty1 ty2
  where Pair ty1 ty2 = coercionKind co

-- Convert args to a TyConAppCo Nominal to the same TyConAppCo Representational
applyRoles :: TyCon -> [Coercion] -> [Coercion]
applyRoles tc cos
  = zipWith (\r -> downgradeRole r Nominal) (tyConRolesRepresentational tc) cos

-- the Role parameter is the Role of the TyConAppCo
-- defined here because this is intimately concerned with the implementation
-- of TyConAppCo
-- Always returns an infinite list (with a infinite tail of Nominal)
tyConRolesX :: Role -> TyCon -> [Role]
tyConRolesX Representational tc = tyConRolesRepresentational tc
tyConRolesX role             _  = repeat role

-- Returns the roles of the parameters of a tycon, with an infinite tail
-- of Nominal
tyConRolesRepresentational :: TyCon -> [Role]
tyConRolesRepresentational tc = tyConRoles tc ++ repeat Nominal

nthRole :: Role -> TyCon -> Int -> Role
nthRole Nominal _ _ = Nominal
nthRole Phantom _ _ = Phantom
nthRole Representational tc n
  = (tyConRolesRepresentational tc) `getNth` n

ltRole :: Role -> Role -> Bool
-- Is one role "less" than another?
--     Nominal < Representational < Phantom
ltRole Phantom          _       = False
ltRole Representational Phantom = True
ltRole Representational _       = False
ltRole Nominal          Nominal = False
ltRole Nominal          _       = True

-------------------------------

-- | like mkKindCo, but aggressively & recursively optimizes to avoid using
-- a KindCo constructor. The output role is nominal.
promoteCoercion :: Coercion -> CoercionN

-- First cases handles anything that should yield refl.
promoteCoercion co = case co of

    _ | ki1 `eqType` ki2
      -> mkNomReflCo (typeKind ty1)
     -- no later branch should return refl
     --    The assert (False )s throughout
     -- are these cases explicitly, but they should never fire.

    Refl _ -> assert False $
              mkNomReflCo ki1

    GRefl _ _ MRefl -> assert False $
                       mkNomReflCo ki1

    GRefl _ _ (MCo co) -> co

    TyConAppCo _ tc args
      | Just co' <- instCoercions (mkNomReflCo (tyConKind tc)) args
      -> co'
      | otherwise
      -> mkKindCo co

    AppCo co1 arg
      | Just co' <- instCoercion (coercionKind (mkKindCo co1))
                                 (promoteCoercion co1) arg
      -> co'
      | otherwise
      -> mkKindCo co

    ForAllCo tv _ g
      | isTyVar tv
      -> promoteCoercion g

    ForAllCo _ _ _
      -> assert False $
         mkNomReflCo liftedTypeKind
      -- See Note [Weird typing rule for ForAllTy] in GHC.Core.TyCo.Rep

    FunCo _ _ _ _
      -> assert False $
         mkNomReflCo liftedTypeKind

    CoVarCo {}     -> mkKindCo co
    HoleCo {}      -> mkKindCo co
    AxiomInstCo {} -> mkKindCo co
    AxiomRuleCo {} -> mkKindCo co

    UnivCo (PhantomProv kco)    _ _ _ -> kco
    UnivCo (ProofIrrelProv kco) _ _ _ -> kco
    UnivCo (PluginProv _)       _ _ _ -> mkKindCo co
    UnivCo (CorePrepProv _)     _ _ _ -> mkKindCo co

    SymCo g
      -> mkSymCo (promoteCoercion g)

    TransCo co1 co2
      -> mkTransCo (promoteCoercion co1) (promoteCoercion co2)

    NthCo _ n co1
      | Just (_, args) <- splitTyConAppCo_maybe co1
      , args `lengthExceeds` n
      -> promoteCoercion (args !! n)

      | Just _ <- splitForAllCo_maybe co
      , n == 0
      -> assert False $ mkNomReflCo liftedTypeKind

      | otherwise
      -> mkKindCo co

    LRCo lr co1
      | Just (lco, rco) <- splitAppCo_maybe co1
      -> case lr of
           CLeft  -> promoteCoercion lco
           CRight -> promoteCoercion rco

      | otherwise
      -> mkKindCo co

    InstCo g _
      | isForAllTy_ty ty1
      -> assert (isForAllTy_ty ty2) $
         promoteCoercion g
      | otherwise
      -> assert False $
         mkNomReflCo liftedTypeKind
           -- See Note [Weird typing rule for ForAllTy] in GHC.Core.TyCo.Rep

    KindCo _
      -> assert False $
         mkNomReflCo liftedTypeKind

    SubCo g
      -> promoteCoercion g

  where
    Pair ty1 ty2 = coercionKind co
    ki1 = typeKind ty1
    ki2 = typeKind ty2

-- | say @g = promoteCoercion h@. Then, @instCoercion g w@ yields @Just g'@,
-- where @g' = promoteCoercion (h w)@.
-- fails if this is not possible, if @g@ coerces between a forall and an ->
-- or if second parameter has a representational role and can't be used
-- with an InstCo.
instCoercion :: Pair Type -- g :: lty ~ rty
             -> CoercionN  -- ^  must be nominal
             -> Coercion
             -> Maybe CoercionN
instCoercion (Pair lty rty) g w
  | (isForAllTy_ty lty && isForAllTy_ty rty)
  || (isForAllTy_co lty && isForAllTy_co rty)
  , Just w' <- setNominalRole_maybe (coercionRole w) w
    -- g :: (forall t1. t2) ~ (forall t1. t3)
    -- w :: s1 ~ s2
    -- returns mkInstCo g w' :: t2 [t1 |-> s1 ] ~ t3 [t1 |-> s2]
  = Just $ mkInstCo g w'
  | isFunTy lty && isFunTy rty
    -- g :: (t1 -> t2) ~ (t3 -> t4)
    -- returns t2 ~ t4
  = Just $ mkNthCo Nominal 4 g -- extract result type, which is the 5th argument to (->)
  | otherwise -- one forall, one funty...
  = Nothing

-- | Repeated use of 'instCoercion'
instCoercions :: CoercionN -> [Coercion] -> Maybe CoercionN
instCoercions g ws
  = let arg_ty_pairs = map coercionKind ws in
    snd <$> foldM go (coercionKind g, g) (zip arg_ty_pairs ws)
  where
    go :: (Pair Type, Coercion) -> (Pair Type, Coercion)
       -> Maybe (Pair Type, Coercion)
    go (g_tys, g) (w_tys, w)
      = do { g' <- instCoercion g_tys g w
           ; return (piResultTy <$> g_tys <*> w_tys, g') }

-- | Creates a new coercion with both of its types casted by different casts
-- @castCoercionKind2 g r t1 t2 h1 h2@, where @g :: t1 ~r t2@,
-- has type @(t1 |> h1) ~r (t2 |> h2)@.
-- @h1@ and @h2@ must be nominal.
castCoercionKind2 :: Coercion -> Role -> Type -> Type
                 -> CoercionN -> CoercionN -> Coercion
castCoercionKind2 g r t1 t2 h1 h2
  = mkCoherenceRightCo r t2 h2 (mkCoherenceLeftCo r t1 h1 g)

-- | @castCoercionKind1 g r t1 t2 h@ = @coercionKind g r t1 t2 h h@
-- That is, it's a specialised form of castCoercionKind, where the two
--          kind coercions are identical
-- @castCoercionKind1 g r t1 t2 h@, where @g :: t1 ~r t2@,
-- has type @(t1 |> h) ~r (t2 |> h)@.
-- @h@ must be nominal.
-- See Note [castCoercionKind1]
castCoercionKind1 :: Coercion -> Role -> Type -> Type
                  -> CoercionN -> Coercion
castCoercionKind1 g r t1 t2 h
  = case g of
      Refl {} -> assert (r == Nominal) $ -- Refl is always Nominal
                 mkNomReflCo (mkCastTy t2 h)
      GRefl _ _ mco -> case mco of
           MRefl       -> mkReflCo r (mkCastTy t2 h)
           MCo kind_co -> GRefl r (mkCastTy t1 h) $
                          MCo (mkSymCo h `mkTransCo` kind_co `mkTransCo` h)
      _ -> castCoercionKind2 g r t1 t2 h h

-- | Creates a new coercion with both of its types casted by different casts
-- @castCoercionKind g h1 h2@, where @g :: t1 ~r t2@,
-- has type @(t1 |> h1) ~r (t2 |> h2)@.
-- @h1@ and @h2@ must be nominal.
-- It calls @coercionKindRole@, so it's quite inefficient (which 'I' stands for)
-- Use @castCoercionKind2@ instead if @t1@, @t2@, and @r@ are known beforehand.
castCoercionKind :: Coercion -> CoercionN -> CoercionN -> Coercion
castCoercionKind g h1 h2
  = castCoercionKind2 g r t1 t2 h1 h2
  where
    (Pair t1 t2, r) = coercionKindRole g

mkFamilyTyConAppCo :: TyCon -> [CoercionN] -> CoercionN
-- ^ Given a family instance 'TyCon' and its arg 'Coercion's, return the
-- corresponding family 'Coercion'.  E.g:
--
-- > data family T a
-- > data instance T (Maybe b) = MkT b
--
-- Where the instance 'TyCon' is :RTL, so:
--
-- > mkFamilyTyConAppCo :RTL (co :: a ~# Int) = T (Maybe a) ~# T (Maybe Int)
--
-- cf. 'mkFamilyTyConApp'
mkFamilyTyConAppCo tc cos
  | Just (fam_tc, fam_tys) <- tyConFamInst_maybe tc
  , let tvs = tyConTyVars tc
        fam_cos = assertPpr (tvs `equalLength` cos) (ppr tc <+> ppr cos) $
                  map (liftCoSubstWith Nominal tvs cos) fam_tys
  = mkTyConAppCo Nominal fam_tc fam_cos
  | otherwise
  = mkTyConAppCo Nominal tc cos

-- See Note [Newtype coercions] in GHC.Core.TyCon

mkPiCos :: Role -> [Var] -> Coercion -> Coercion
mkPiCos r vs co = foldr (mkPiCo r) co vs

-- | Make a forall 'Coercion', where both types related by the coercion
-- are quantified over the same variable.
mkPiCo  :: Role -> Var -> Coercion -> Coercion
mkPiCo r v co | isTyVar v = mkHomoForAllCos [v] co
              | isCoVar v = assert (not (v `elemVarSet` tyCoVarsOfCo co)) $
                  -- We didn't call mkForAllCo here because if v does not appear
                  -- in co, the argement coercion will be nominal. But here we
                  -- want it to be r. It is only called in 'mkPiCos', which is
                  -- only used in GHC.Core.Opt.Simplify.Utils, where we are sure for
                  -- now (Aug 2018) v won't occur in co.
                            mkFunResCo r scaled_ty co
              | otherwise = mkFunResCo r scaled_ty co
              where
                scaled_ty = Scaled (varMult v) (varType v)

mkFunResCo :: Role -> Scaled Type -> Coercion -> Coercion
-- Given res_co :: res1 -> res2,
--   mkFunResCo r m arg res_co :: (arg -> res1) ~r (arg -> res2)
-- Reflexive in the multiplicity argument
mkFunResCo role (Scaled mult arg_ty) res_co
  = mkFunCo role (multToCo mult) (mkReflCo role arg_ty) res_co

-- mkCoCast (c :: s1 ~?r t1) (g :: (s1 ~?r t1) ~#R (s2 ~?r t2)) :: s2 ~?r t2
-- The first coercion might be lifted or unlifted; thus the ~? above
-- Lifted and unlifted equalities take different numbers of arguments,
-- so we have to make sure to supply the right parameter to decomposeCo.
-- Also, note that the role of the first coercion is the same as the role of
-- the equalities related by the second coercion. The second coercion is
-- itself always representational.
mkCoCast :: Coercion -> CoercionR -> Coercion
mkCoCast c g
  | (g2:g1:_) <- reverse co_list
  = mkSymCo g1 `mkTransCo` c `mkTransCo` g2

  | otherwise
  = pprPanic "mkCoCast" (ppr g $$ ppr (coercionKind g))
  where
    -- g  :: (s1 ~# t1) ~# (s2 ~# t2)
    -- g1 :: s1 ~# s2
    -- g2 :: t1 ~# t2
    (tc, _) = splitTyConApp (coercionLKind g)
    co_list = decomposeCo (tyConArity tc) g (tyConRolesRepresentational tc)

{- Note [castCoercionKind1]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
castCoercionKind1 deals with the very important special case of castCoercionKind2
where the two kind coercions are identical.  In that case we can exploit the
situation where the main coercion is reflexive, via the special cases for Refl
and GRefl.

This is important when rewriting  (ty |> co). We rewrite ty, yielding
   fco :: ty ~ ty'
and now we want a coercion xco between
   xco :: (ty |> co) ~ (ty' |> co)
That's exactly what castCoercionKind1 does.  And it's very very common for
fco to be Refl.  In that case we do NOT want to get some terrible composition
of mkLeftCoherenceCo and mkRightCoherenceCo, which is what castCoercionKind2
has to do in its full generality.  See #18413.
-}

{-
%************************************************************************
%*                                                                      *
            Newtypes
%*                                                                      *
%************************************************************************
-}

-- | If `instNewTyCon_maybe T ts = Just (rep_ty, co)`
--   then `co :: T ts ~R# rep_ty`
--
-- Checks for a newtype, and for being saturated
instNewTyCon_maybe :: TyCon -> [Type] -> Maybe (Type, Coercion)
instNewTyCon_maybe tc tys
  | Just (tvs, ty, co_tc) <- unwrapNewTyConEtad_maybe tc  -- Check for newtype
  , tvs `leLength` tys                                    -- Check saturated enough
  = Just (applyTysX tvs ty tys, mkUnbranchedAxInstCo Representational co_tc tys [])
  | otherwise
  = Nothing

{-
************************************************************************
*                                                                      *
         Type normalisation
*                                                                      *
************************************************************************
-}

-- | A function to check if we can reduce a type by one step. Used
-- with 'topNormaliseTypeX'.
type NormaliseStepper ev = RecTcChecker
                         -> TyCon     -- tc
                         -> [Type]    -- tys
                         -> NormaliseStepResult ev

-- | The result of stepping in a normalisation function.
-- See 'topNormaliseTypeX'.
data NormaliseStepResult ev
  = NS_Done   -- ^ Nothing more to do
  | NS_Abort  -- ^ Utter failure. The outer function should fail too.
  | NS_Step RecTcChecker Type ev    -- ^ We stepped, yielding new bits;
                                    -- ^ ev is evidence;
                                    -- Usually a co :: old type ~ new type

instance Outputable ev => Outputable (NormaliseStepResult ev) where
  ppr NS_Done           = text "NS_Done"
  ppr NS_Abort          = text "NS_Abort"
  ppr (NS_Step _ ty ev) = sep [text "NS_Step", ppr ty, ppr ev]

mapStepResult :: (ev1 -> ev2)
              -> NormaliseStepResult ev1 -> NormaliseStepResult ev2
mapStepResult f (NS_Step rec_nts ty ev) = NS_Step rec_nts ty (f ev)
mapStepResult _ NS_Done                 = NS_Done
mapStepResult _ NS_Abort                = NS_Abort

-- | Try one stepper and then try the next, if the first doesn't make
-- progress.
-- So if it returns NS_Done, it means that both steppers are satisfied
composeSteppers :: NormaliseStepper ev -> NormaliseStepper ev
                -> NormaliseStepper ev
composeSteppers step1 step2 rec_nts tc tys
  = case step1 rec_nts tc tys of
      success@(NS_Step {}) -> success
      NS_Done              -> step2 rec_nts tc tys
      NS_Abort             -> NS_Abort

-- | A 'NormaliseStepper' that unwraps newtypes, careful not to fall into
-- a loop. If it would fall into a loop, it produces 'NS_Abort'.
unwrapNewTypeStepper :: NormaliseStepper Coercion
unwrapNewTypeStepper rec_nts tc tys
  | Just (ty', co) <- instNewTyCon_maybe tc tys
  = -- pprTrace "unNS" (ppr tc <+> ppr (getUnique tc) <+> ppr tys $$ ppr ty' $$ ppr rec_nts) $
    case checkRecTc rec_nts tc of
      Just rec_nts' -> NS_Step rec_nts' ty' co
      Nothing       -> NS_Abort

  | otherwise
  = NS_Done

-- | A general function for normalising the top-level of a type. It continues
-- to use the provided 'NormaliseStepper' until that function fails, and then
-- this function returns. The roles of the coercions produced by the
-- 'NormaliseStepper' must all be the same, which is the role returned from
-- the call to 'topNormaliseTypeX'.
--
-- Typically ev is Coercion.
--
-- If topNormaliseTypeX step plus ty = Just (ev, ty')
-- then ty ~ev1~ t1 ~ev2~ t2 ... ~evn~ ty'
-- and ev = ev1 `plus` ev2 `plus` ... `plus` evn
-- If it returns Nothing then no newtype unwrapping could happen
topNormaliseTypeX :: NormaliseStepper ev
                  -> (ev -> ev -> ev)
                  -> Type -> Maybe (ev, Type)
topNormaliseTypeX stepper plus ty
 | Just (tc, tys) <- splitTyConApp_maybe ty
 -- SPJ: The default threshold for initRecTc is 100 which is extremely dangerous
 --      for certain type synonyms, we should think about reducing it (see #20990)
 , NS_Step rec_nts ty' ev <- stepper initRecTc tc tys
 = go rec_nts ev ty'
 | otherwise
 = Nothing
 where
    go rec_nts ev ty
      | Just (tc, tys) <- splitTyConApp_maybe ty
      = case stepper rec_nts tc tys of
          NS_Step rec_nts' ty' ev' -> go rec_nts' (ev `plus` ev') ty'
          NS_Done  -> Just (ev, ty)
          NS_Abort -> Nothing

      | otherwise
      = Just (ev, ty)

topNormaliseNewType_maybe :: Type -> Maybe (Coercion, Type)
-- ^ Sometimes we want to look through a @newtype@ and get its associated coercion.
-- This function strips off @newtype@ layers enough to reveal something that isn't
-- a @newtype@.  Specifically, here's the invariant:
--
-- > topNormaliseNewType_maybe rec_nts ty = Just (co, ty')
--
-- then (a)  @co : ty ~ ty'@.
--      (b)  ty' is not a newtype.
--
-- The function returns @Nothing@ for non-@newtypes@,
-- or unsaturated applications
--
-- This function does *not* look through type families, because it has no access to
-- the type family environment. If you do have that at hand, consider to use
-- topNormaliseType_maybe, which should be a drop-in replacement for
-- topNormaliseNewType_maybe
-- If topNormliseNewType_maybe ty = Just (co, ty'), then co : ty ~R ty'
topNormaliseNewType_maybe ty
  = topNormaliseTypeX unwrapNewTypeStepper mkTransCo ty

{-
%************************************************************************
%*                                                                      *
                   Comparison of coercions
%*                                                                      *
%************************************************************************
-}

-- | Syntactic equality of coercions
eqCoercion :: Coercion -> Coercion -> Bool
eqCoercion = eqType `on` coercionType

-- | Compare two 'Coercion's, with respect to an RnEnv2
eqCoercionX :: RnEnv2 -> Coercion -> Coercion -> Bool
eqCoercionX env = eqTypeX env `on` coercionType

{-
%************************************************************************
%*                                                                      *
                   "Lifting" substitution
           [(TyCoVar,Coercion)] -> Type -> Coercion
%*                                                                      *
%************************************************************************

Note [Lifting coercions over types: liftCoSubst]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The KPUSH rule deals with this situation
   data T a = K (a -> Maybe a)
   g :: T t1 ~ T t2
   x :: t1 -> Maybe t1

   case (K @t1 x) |> g of
     K (y:t2 -> Maybe t2) -> rhs

We want to push the coercion inside the constructor application.
So we do this

   g' :: t1~t2  =  Nth 0 g

   case K @t2 (x |> g' -> Maybe g') of
     K (y:t2 -> Maybe t2) -> rhs

The crucial operation is that we
  * take the type of K's argument: a -> Maybe a
  * and substitute g' for a
thus giving *coercion*.  This is what liftCoSubst does.

In the presence of kind coercions, this is a bit
of a hairy operation. So, we refer you to the paper introducing kind coercions,
available at www.cis.upenn.edu/~sweirich/papers/fckinds-extended.pdf

Note [extendLiftingContextEx]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider we have datatype
  K :: \/k. \/a::k. P -> T k  -- P be some type
  g :: T k1 ~ T k2

  case (K @k1 @t1 x) |> g of
    K y -> rhs

We want to push the coercion inside the constructor application.
We first get the coercion mapped by the universal type variable k:
   lc = k |-> Nth 0 g :: k1~k2

Here, the important point is that the kind of a is coerced, and P might be
dependent on the existential type variable a.
Thus we first get the coercion of a's kind
   g2 = liftCoSubst lc k :: k1 ~ k2

Then we store a new mapping into the lifting context
   lc2 = a |-> (t1 ~ t1 |> g2), lc

So later when we can correctly deal with the argument type P
   liftCoSubst lc2 P :: P [k|->k1][a|->t1] ~ P[k|->k2][a |-> (t1|>g2)]

This is exactly what extendLiftingContextEx does.
* For each (tyvar:k, ty) pair, we product the mapping
    tyvar |-> (ty ~ ty |> (liftCoSubst lc k))
* For each (covar:s1~s2, ty) pair, we produce the mapping
    covar |-> (co ~ co')
    co' = Sym (liftCoSubst lc s1) ;; covar ;; liftCoSubst lc s2 :: s1'~s2'

This follows the lifting context extension definition in the
"FC with Explicit Kind Equality" paper.
-}

-- ----------------------------------------------------
-- See Note [Lifting coercions over types: liftCoSubst]
-- ----------------------------------------------------

data LiftingContext = LC TCvSubst LiftCoEnv
  -- in optCoercion, we need to lift when optimizing InstCo.
  -- See Note [Optimising InstCo] in GHC.Core.Coercion.Opt
  -- We thus propagate the substitution from GHC.Core.Coercion.Opt here.

instance Outputable LiftingContext where
  ppr (LC _ env) = hang (text "LiftingContext:") 2 (ppr env)

type LiftCoEnv = VarEnv Coercion
     -- Maps *type variables* to *coercions*.
     -- That's the whole point of this function!
     -- Also maps coercion variables to ProofIrrelCos.

-- like liftCoSubstWith, but allows for existentially-bound types as well
liftCoSubstWithEx :: Role          -- desired role for output coercion
                  -> [TyVar]       -- universally quantified tyvars
                  -> [Coercion]    -- coercions to substitute for those
                  -> [TyCoVar]     -- existentially quantified tycovars
                  -> [Type]        -- types and coercions to be bound to ex vars
                  -> (Type -> Coercion, [Type]) -- (lifting function, converted ex args)
liftCoSubstWithEx role univs omegas exs rhos
  = let theta = mkLiftingContext (zipEqual "liftCoSubstWithExU" univs omegas)
        psi   = extendLiftingContextEx theta (zipEqual "liftCoSubstWithExX" exs rhos)
    in (ty_co_subst psi role, substTys (lcSubstRight psi) (mkTyCoVarTys exs))

liftCoSubstWith :: Role -> [TyCoVar] -> [Coercion] -> Type -> Coercion
liftCoSubstWith r tvs cos ty
  = liftCoSubst r (mkLiftingContext $ zipEqual "liftCoSubstWith" tvs cos) ty

-- | @liftCoSubst role lc ty@ produces a coercion (at role @role@)
-- that coerces between @lc_left(ty)@ and @lc_right(ty)@, where
-- @lc_left@ is a substitution mapping type variables to the left-hand
-- types of the mapped coercions in @lc@, and similar for @lc_right@.
liftCoSubst :: HasDebugCallStack => Role -> LiftingContext -> Type -> Coercion
{-# INLINE liftCoSubst #-}
-- Inlining this function is worth 2% of allocation in T9872d,
liftCoSubst r lc@(LC subst env) ty
  | isEmptyVarEnv env = mkReflCo r (substTy subst ty)
  | otherwise         = ty_co_subst lc r ty

emptyLiftingContext :: InScopeSet -> LiftingContext
emptyLiftingContext in_scope = LC (mkEmptyTCvSubst in_scope) emptyVarEnv

mkLiftingContext :: [(TyCoVar,Coercion)] -> LiftingContext
mkLiftingContext pairs
  = LC (mkEmptyTCvSubst $ mkInScopeSet $ tyCoVarsOfCos (map snd pairs))
       (mkVarEnv pairs)

mkSubstLiftingContext :: TCvSubst -> LiftingContext
mkSubstLiftingContext subst = LC subst emptyVarEnv

-- | Extend a lifting context with a new mapping.
extendLiftingContext :: LiftingContext  -- ^ original LC
                     -> TyCoVar         -- ^ new variable to map...
                     -> Coercion        -- ^ ...to this lifted version
                     -> LiftingContext
    -- mappings to reflexive coercions are just substitutions
extendLiftingContext (LC subst env) tv arg
  | Just (ty, _) <- isReflCo_maybe arg
  = LC (extendTCvSubst subst tv ty) env
  | otherwise
  = LC subst (extendVarEnv env tv arg)

-- | Extend a lifting context with a new mapping, and extend the in-scope set
extendLiftingContextAndInScope :: LiftingContext  -- ^ Original LC
                               -> TyCoVar         -- ^ new variable to map...
                               -> Coercion        -- ^ to this coercion
                               -> LiftingContext
extendLiftingContextAndInScope (LC subst env) tv co
  = extendLiftingContext (LC (extendTCvInScopeSet subst (tyCoVarsOfCo co)) env) tv co

-- | Extend a lifting context with existential-variable bindings.
-- See Note [extendLiftingContextEx]
extendLiftingContextEx :: LiftingContext    -- ^ original lifting context
                       -> [(TyCoVar,Type)]  -- ^ ex. var / value pairs
                       -> LiftingContext
-- Note that this is more involved than extendLiftingContext. That function
-- takes a coercion to extend with, so it's assumed that the caller has taken
-- into account any of the kind-changing stuff worried about here.
extendLiftingContextEx lc [] = lc
extendLiftingContextEx lc@(LC subst env) ((v,ty):rest)
-- This function adds bindings for *Nominal* coercions. Why? Because it
-- works with existentially bound variables, which are considered to have
-- nominal roles.
  | isTyVar v
  = let lc' = LC (subst `extendTCvInScopeSet` tyCoVarsOfType ty)
                 (extendVarEnv env v $
                  mkGReflRightCo Nominal
                                 ty
                                 (ty_co_subst lc Nominal (tyVarKind v)))
    in extendLiftingContextEx lc' rest
  | CoercionTy co <- ty
  = -- co      :: s1 ~r s2
    -- lift_s1 :: s1 ~r s1'
    -- lift_s2 :: s2 ~r s2'
    -- kco     :: (s1 ~r s2) ~N (s1' ~r s2')
    assert (isCoVar v) $
    let (_, _, s1, s2, r) = coVarKindsTypesRole v
        lift_s1 = ty_co_subst lc r s1
        lift_s2 = ty_co_subst lc r s2
        kco     = mkTyConAppCo Nominal (equalityTyCon r)
                               [ mkKindCo lift_s1, mkKindCo lift_s2
                               , lift_s1         , lift_s2          ]
        lc'     = LC (subst `extendTCvInScopeSet` tyCoVarsOfCo co)
                     (extendVarEnv env v
                        (mkProofIrrelCo Nominal kco co $
                          (mkSymCo lift_s1) `mkTransCo` co `mkTransCo` lift_s2))
    in extendLiftingContextEx lc' rest
  | otherwise
  = pprPanic "extendLiftingContextEx" (ppr v <+> text "|->" <+> ppr ty)


-- | Erase the environments in a lifting context
zapLiftingContext :: LiftingContext -> LiftingContext
zapLiftingContext (LC subst _) = LC (zapTCvSubst subst) emptyVarEnv

-- | Like 'substForAllCoBndr', but works on a lifting context
substForAllCoBndrUsingLC :: Bool
                            -> (Coercion -> Coercion)
                            -> LiftingContext -> TyCoVar -> Coercion
                            -> (LiftingContext, TyCoVar, Coercion)
substForAllCoBndrUsingLC sym sco (LC subst lc_env) tv co
  = (LC subst' lc_env, tv', co')
  where
    (subst', tv', co') = substForAllCoBndrUsing sym sco subst tv co

-- | The \"lifting\" operation which substitutes coercions for type
--   variables in a type to produce a coercion.
--
--   For the inverse operation, see 'liftCoMatch'
ty_co_subst :: LiftingContext -> Role -> Type -> Coercion
ty_co_subst !lc role ty
    -- !lc: making this function strict in lc allows callers to
    -- pass its two components separately, rather than boxing them.
    -- Unfortunately, Boxity Analysis concludes that we need lc boxed
    -- because it's used that way in liftCoSubstTyVarBndrUsing.
  = go role ty
  where
    go :: Role -> Type -> Coercion
    go r ty                | Just ty' <- coreView ty
                           = go r ty'
    go Phantom ty          = lift_phantom ty
    go r (TyVarTy tv)      = expectJust "ty_co_subst bad roles" $
                             liftCoSubstTyVar lc r tv
    go r (AppTy ty1 ty2)   = mkAppCo (go r ty1) (go Nominal ty2)
    go r (TyConApp tc tys) = mkTyConAppCo r tc (zipWith go (tyConRolesX r tc) tys)
    go r (FunTy _ w ty1 ty2) = mkFunCo r (go Nominal w) (go r ty1) (go r ty2)
    go r t@(ForAllTy (Bndr v _) ty)
       = let (lc', v', h) = liftCoSubstVarBndr lc v
             body_co = ty_co_subst lc' r ty in
         if isTyVar v' || almostDevoidCoVarOfCo v' body_co
           -- Lifting a ForAllTy over a coercion variable could fail as ForAllCo
           -- imposes an extra restriction on where a covar can appear. See last
           -- wrinkle in Note [Unused coercion variable in ForAllCo].
           -- We specifically check for this and panic because we know that
           -- there's a hole in the type system here, and we'd rather panic than
           -- fall into it.
         then mkForAllCo v' h body_co
         else pprPanic "ty_co_subst: covar is not almost devoid" (ppr t)
    go r ty@(LitTy {})     = assert (r == Nominal) $
                             mkNomReflCo ty
    go r (CastTy ty co)    = castCoercionKind (go r ty) (substLeftCo lc co)
                                                        (substRightCo lc co)
    go r (CoercionTy co)   = mkProofIrrelCo r kco (substLeftCo lc co)
                                                  (substRightCo lc co)
      where kco = go Nominal (coercionType co)

    lift_phantom ty = mkPhantomCo (go Nominal (typeKind ty))
                                  (substTy (lcSubstLeft  lc) ty)
                                  (substTy (lcSubstRight lc) ty)

{-
Note [liftCoSubstTyVar]
~~~~~~~~~~~~~~~~~~~~~~~~~
This function can fail if a coercion in the environment is of too low a role.

liftCoSubstTyVar is called from two places: in liftCoSubst (naturally), and
also in matchAxiom in GHC.Core.Coercion.Opt. From liftCoSubst, the so-called lifting
lemma guarantees that the roles work out. If we fail in this
case, we really should panic -- something is deeply wrong. But, in matchAxiom,
failing is fine. matchAxiom is trying to find a set of coercions
that match, but it may fail, and this is healthy behavior.
-}

-- See Note [liftCoSubstTyVar]
liftCoSubstTyVar :: LiftingContext -> Role -> TyVar -> Maybe Coercion
liftCoSubstTyVar (LC subst env) r v
  | Just co_arg <- lookupVarEnv env v
  = downgradeRole_maybe r (coercionRole co_arg) co_arg

  | otherwise
  = Just $ mkReflCo r (substTyVar subst v)

{- Note [liftCoSubstVarBndr]
   ~~~~~~~~~~~~~~~~~~~~~~~~~
callback:
  'liftCoSubstVarBndrUsing' needs to be general enough to work in two
  situations:

    - in this module, which manipulates 'Coercion's, and
    - in GHC.Core.FamInstEnv, where we work with 'Reduction's, which contain
      a coercion as well as a type.

  To achieve this, we require that the return type of the 'callback' function
  contain a coercion within it. This is witnessed by the first argument
  to 'liftCoSubstVarBndrUsing': a getter, which allows us to retrieve
  the coercion inside the return type. Thus:

    - in this module, we simply pass 'id' as the getter,
    - in GHC.Core.FamInstEnv, we pass 'reductionCoercion' as the getter.

liftCoSubstTyVarBndrUsing:
  Given
    forall tv:k. t
  We want to get
    forall (tv:k1) (kind_co :: k1 ~ k2) body_co

  We lift the kind k to get the kind_co
    kind_co = ty_co_subst k :: k1 ~ k2

  Now in the LiftingContext, we add the new mapping
    tv |-> (tv :: k1) ~ ((tv |> kind_co) :: k2)

liftCoSubstCoVarBndrUsing:
  Given
    forall cv:(s1 ~ s2). t
  We want to get
    forall (cv:s1'~s2') (kind_co :: (s1'~s2') ~ (t1 ~ t2)) body_co

  We lift s1 and s2 respectively to get
    eta1 :: s1' ~ t1
    eta2 :: s2' ~ t2
  And
    kind_co = TyConAppCo Nominal (~#) eta1 eta2

  Now in the liftingContext, we add the new mapping
    cv |-> (cv :: s1' ~ s2') ~ ((sym eta1;cv;eta2) :: t1 ~ t2)
-}

-- See Note [liftCoSubstVarBndr]
liftCoSubstVarBndr :: LiftingContext -> TyCoVar
                   -> (LiftingContext, TyCoVar, Coercion)
liftCoSubstVarBndr lc tv
  = liftCoSubstVarBndrUsing id callback lc tv
  where
    callback lc' ty' = ty_co_subst lc' Nominal ty'

-- the callback must produce a nominal coercion
liftCoSubstVarBndrUsing :: (r -> CoercionN)              -- ^ coercion getter
                        -> (LiftingContext -> Type -> r) -- ^ callback
                        -> LiftingContext -> TyCoVar
                        -> (LiftingContext, TyCoVar, r)
liftCoSubstVarBndrUsing view_co fun lc old_var
  | isTyVar old_var
  = liftCoSubstTyVarBndrUsing view_co fun lc old_var
  | otherwise
  = liftCoSubstCoVarBndrUsing view_co fun lc old_var

-- Works for tyvar binder
liftCoSubstTyVarBndrUsing :: (r -> CoercionN)              -- ^ coercion getter
                          -> (LiftingContext -> Type -> r) -- ^ callback
                          -> LiftingContext -> TyVar
                          -> (LiftingContext, TyVar, r)
liftCoSubstTyVarBndrUsing view_co fun lc@(LC subst cenv) old_var
  = assert (isTyVar old_var) $
    ( LC (subst `extendTCvInScope` new_var) new_cenv
    , new_var, stuff )
  where
    old_kind = tyVarKind old_var
    stuff    = fun lc old_kind
    eta      = view_co stuff
    k1       = coercionLKind eta
    new_var  = uniqAway (getTCvInScope subst) (setVarType old_var k1)

    lifted   = mkGReflRightCo Nominal (TyVarTy new_var) eta
               -- :: new_var ~ new_var |> eta
    new_cenv = extendVarEnv cenv old_var lifted

-- Works for covar binder
liftCoSubstCoVarBndrUsing :: (r -> CoercionN)              -- ^ coercion getter
                          -> (LiftingContext -> Type -> r) -- ^ callback
                          -> LiftingContext -> CoVar
                          -> (LiftingContext, CoVar, r)
liftCoSubstCoVarBndrUsing view_co fun lc@(LC subst cenv) old_var
  = assert (isCoVar old_var) $
    ( LC (subst `extendTCvInScope` new_var) new_cenv
    , new_var, stuff )
  where
    old_kind = coVarKind old_var
    stuff    = fun lc old_kind
    eta      = view_co stuff
    k1       = coercionLKind eta
    new_var  = uniqAway (getTCvInScope subst) (setVarType old_var k1)

    -- old_var :: s1  ~r s2
    -- eta     :: (s1' ~r s2') ~N (t1 ~r t2)
    -- eta1    :: s1' ~r t1
    -- eta2    :: s2' ~r t2
    -- co1     :: s1' ~r s2'
    -- co2     :: t1  ~r t2
    -- lifted  :: co1 ~N co2

    role   = coVarRole old_var
    eta'   = downgradeRole role Nominal eta
    eta1   = mkNthCo role 2 eta'
    eta2   = mkNthCo role 3 eta'

    co1     = mkCoVarCo new_var
    co2     = mkSymCo eta1 `mkTransCo` co1 `mkTransCo` eta2
    lifted  = mkProofIrrelCo Nominal eta co1 co2

    new_cenv = extendVarEnv cenv old_var lifted

-- | Is a var in the domain of a lifting context?
isMappedByLC :: TyCoVar -> LiftingContext -> Bool
isMappedByLC tv (LC _ env) = tv `elemVarEnv` env

-- If [a |-> g] is in the substitution and g :: t1 ~ t2, substitute a for t1
-- If [a |-> (g1, g2)] is in the substitution, substitute a for g1
substLeftCo :: LiftingContext -> Coercion -> Coercion
substLeftCo lc co
  = substCo (lcSubstLeft lc) co

-- Ditto, but for t2 and g2
substRightCo :: LiftingContext -> Coercion -> Coercion
substRightCo lc co
  = substCo (lcSubstRight lc) co

-- | Apply "sym" to all coercions in a 'LiftCoEnv'
swapLiftCoEnv :: LiftCoEnv -> LiftCoEnv
swapLiftCoEnv = mapVarEnv mkSymCo

lcSubstLeft :: LiftingContext -> TCvSubst
lcSubstLeft (LC subst lc_env) = liftEnvSubstLeft subst lc_env

lcSubstRight :: LiftingContext -> TCvSubst
lcSubstRight (LC subst lc_env) = liftEnvSubstRight subst lc_env

liftEnvSubstLeft :: TCvSubst -> LiftCoEnv -> TCvSubst
liftEnvSubstLeft = liftEnvSubst pFst

liftEnvSubstRight :: TCvSubst -> LiftCoEnv -> TCvSubst
liftEnvSubstRight = liftEnvSubst pSnd

liftEnvSubst :: (forall a. Pair a -> a) -> TCvSubst -> LiftCoEnv -> TCvSubst
liftEnvSubst selector subst lc_env
  = composeTCvSubst (TCvSubst emptyInScopeSet tenv cenv) subst
  where
    pairs            = nonDetUFMToList lc_env
                       -- It's OK to use nonDetUFMToList here because we
                       -- immediately forget the ordering by creating
                       -- a VarEnv
    (tpairs, cpairs) = partitionWith ty_or_co pairs
    tenv             = mkVarEnv_Directly tpairs
    cenv             = mkVarEnv_Directly cpairs

    ty_or_co :: (Unique, Coercion) -> Either (Unique, Type) (Unique, Coercion)
    ty_or_co (u, co)
      | Just equality_co <- isCoercionTy_maybe equality_ty
      = Right (u, equality_co)
      | otherwise
      = Left (u, equality_ty)
      where
        equality_ty = selector (coercionKind co)

-- | Extract the underlying substitution from the LiftingContext
lcTCvSubst :: LiftingContext -> TCvSubst
lcTCvSubst (LC subst _) = subst

-- | Get the 'InScopeSet' from a 'LiftingContext'
lcInScopeSet :: LiftingContext -> InScopeSet
lcInScopeSet (LC subst _) = getTCvInScope subst

{-
%************************************************************************
%*                                                                      *
            Sequencing on coercions
%*                                                                      *
%************************************************************************
-}

seqMCo :: MCoercion -> ()
seqMCo MRefl    = ()
seqMCo (MCo co) = seqCo co

seqCo :: Coercion -> ()
seqCo (Refl ty)                 = seqType ty
seqCo (GRefl r ty mco)          = r `seq` seqType ty `seq` seqMCo mco
seqCo (TyConAppCo r tc cos)     = r `seq` tc `seq` seqCos cos
seqCo (AppCo co1 co2)           = seqCo co1 `seq` seqCo co2
seqCo (ForAllCo tv k co)        = seqType (varType tv) `seq` seqCo k
                                                       `seq` seqCo co
seqCo (FunCo r w co1 co2)       = r `seq` seqCo w `seq` seqCo co1 `seq` seqCo co2
seqCo (CoVarCo cv)              = cv `seq` ()
seqCo (HoleCo h)                = coHoleCoVar h `seq` ()
seqCo (AxiomInstCo con ind cos) = con `seq` ind `seq` seqCos cos
seqCo (UnivCo p r t1 t2)
  = seqProv p `seq` r `seq` seqType t1 `seq` seqType t2
seqCo (SymCo co)                = seqCo co
seqCo (TransCo co1 co2)         = seqCo co1 `seq` seqCo co2
seqCo (NthCo r n co)            = r `seq` n `seq` seqCo co
seqCo (LRCo lr co)              = lr `seq` seqCo co
seqCo (InstCo co arg)           = seqCo co `seq` seqCo arg
seqCo (KindCo co)               = seqCo co
seqCo (SubCo co)                = seqCo co
seqCo (AxiomRuleCo _ cs)        = seqCos cs

seqProv :: UnivCoProvenance -> ()
seqProv (PhantomProv co)    = seqCo co
seqProv (ProofIrrelProv co) = seqCo co
seqProv (PluginProv _)      = ()
seqProv (CorePrepProv _)    = ()

seqCos :: [Coercion] -> ()
seqCos []       = ()
seqCos (co:cos) = seqCo co `seq` seqCos cos

{-
%************************************************************************
%*                                                                      *
             The kind of a type, and of a coercion
%*                                                                      *
%************************************************************************
-}

-- | Apply 'coercionKind' to multiple 'Coercion's
coercionKinds :: [Coercion] -> Pair [Type]
coercionKinds tys = sequenceA $ map coercionKind tys

-- | Get a coercion's kind and role.
coercionKindRole :: Coercion -> (Pair Type, Role)
coercionKindRole co = (coercionKind co, coercionRole co)

coercionType :: Coercion -> Type
coercionType co = case coercionKindRole co of
  (Pair ty1 ty2, r) -> mkCoercionType r ty1 ty2

------------------
-- | If it is the case that
--
-- > c :: (t1 ~ t2)
--
-- i.e. the kind of @c@ relates @t1@ and @t2@, then @coercionKind c = Pair t1 t2@.

coercionKind :: Coercion -> Pair Type
coercionKind co = Pair (coercionLKind co) (coercionRKind co)

coercionLKind :: Coercion -> Type
coercionLKind co
  = go co
  where
    go (Refl ty)                = ty
    go (GRefl _ ty _)           = ty
    go (TyConAppCo _ tc cos)    = mkTyConApp tc (map go cos)
    go (AppCo co1 co2)          = mkAppTy (go co1) (go co2)
    go (ForAllCo tv1 _ co1)     = mkTyCoInvForAllTy tv1 (go co1)
    go (FunCo _ w co1 co2)      = mkFunctionType (go w) (go co1) (go co2)
    go (CoVarCo cv)             = coVarLType cv
    go (HoleCo h)               = coVarLType (coHoleCoVar h)
    go (UnivCo _ _ ty1 _)       = ty1
    go (SymCo co)               = coercionRKind co
    go (TransCo co1 _)          = go co1
    go (LRCo lr co)             = pickLR lr (splitAppTy (go co))
    go (InstCo aco arg)         = go_app aco [go arg]
    go (KindCo co)              = typeKind (go co)
    go (SubCo co)               = go co
    go (NthCo _ d co)           = go_nth d (go co)
    go (AxiomInstCo ax ind cos) = go_ax_inst ax ind (map go cos)
    go (AxiomRuleCo ax cos)     = pFst $ expectJust "coercionKind" $
                                  coaxrProves ax $ map coercionKind cos

    go_ax_inst ax ind tys
      | CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                   , cab_lhs = lhs } <- coAxiomNthBranch ax ind
      , let (tys1, cotys1) = splitAtList tvs tys
            cos1           = map stripCoercionTy cotys1
      = assert (tys `equalLength` (tvs ++ cvs)) $
                  -- Invariant of AxiomInstCo: cos should
                  -- exactly saturate the axiom branch
        substTyWith tvs tys1       $
        substTyWithCoVars cvs cos1 $
        mkTyConApp (coAxiomTyCon ax) lhs

    go_app :: Coercion -> [Type] -> Type
    -- Collect up all the arguments and apply all at once
    -- See Note [Nested InstCos]
    go_app (InstCo co arg) args = go_app co (go arg:args)
    go_app co              args = piResultTys (go co) args

go_nth :: Int -> Type -> Type
go_nth d ty
  | Just args <- tyConAppArgs_maybe ty
  = assert (args `lengthExceeds` d) $
    args `getNth` d

  | d == 0
  , Just (tv,_) <- splitForAllTyCoVar_maybe ty
  = tyVarKind tv

  | otherwise
  = pprPanic "coercionLKind:nth" (ppr d <+> ppr ty)

coercionRKind :: Coercion -> Type
coercionRKind co
  = go co
  where
    go (Refl ty)                = ty
    go (GRefl _ ty MRefl)       = ty
    go (GRefl _ ty (MCo co1))   = mkCastTy ty co1
    go (TyConAppCo _ tc cos)    = mkTyConApp tc (map go cos)
    go (AppCo co1 co2)          = mkAppTy (go co1) (go co2)
    go (CoVarCo cv)             = coVarRType cv
    go (HoleCo h)               = coVarRType (coHoleCoVar h)
    go (FunCo _ w co1 co2)      = mkFunctionType (go w) (go co1) (go co2)
    go (UnivCo _ _ _ ty2)       = ty2
    go (SymCo co)               = coercionLKind co
    go (TransCo _ co2)          = go co2
    go (LRCo lr co)             = pickLR lr (splitAppTy (go co))
    go (InstCo aco arg)         = go_app aco [go arg]
    go (KindCo co)              = typeKind (go co)
    go (SubCo co)               = go co
    go (NthCo _ d co)           = go_nth d (go co)
    go (AxiomInstCo ax ind cos) = go_ax_inst ax ind (map go cos)
    go (AxiomRuleCo ax cos)     = pSnd $ expectJust "coercionKind" $
                                  coaxrProves ax $ map coercionKind cos

    go co@(ForAllCo tv1 k_co co1) -- works for both tyvar and covar
       | isGReflCo k_co           = mkTyCoInvForAllTy tv1 (go co1)
         -- kind_co always has kind @Type@, thus @isGReflCo@
       | otherwise                = go_forall empty_subst co
       where
         empty_subst = mkEmptyTCvSubst (mkInScopeSet $ tyCoVarsOfCo co)

    go_ax_inst ax ind tys
      | CoAxBranch { cab_tvs = tvs, cab_cvs = cvs
                   , cab_rhs = rhs } <- coAxiomNthBranch ax ind
      , let (tys2, cotys2) = splitAtList tvs tys
            cos2           = map stripCoercionTy cotys2
      = assert (tys `equalLength` (tvs ++ cvs)) $
                  -- Invariant of AxiomInstCo: cos should
                  -- exactly saturate the axiom branch
        substTyWith tvs tys2 $
        substTyWithCoVars cvs cos2 rhs

    go_app :: Coercion -> [Type] -> Type
    -- Collect up all the arguments and apply all at once
    -- See Note [Nested InstCos]
    go_app (InstCo co arg) args = go_app co (go arg:args)
    go_app co              args = piResultTys (go co) args

    go_forall subst (ForAllCo tv1 k_co co)
      -- See Note [Nested ForAllCos]
      | isTyVar tv1
      = mkInfForAllTy tv2 (go_forall subst' co)
      where
        k2  = coercionRKind k_co
        tv2 = setTyVarKind tv1 (substTy subst k2)
        subst' | isGReflCo k_co = extendTCvInScope subst tv1
                 -- kind_co always has kind @Type@, thus @isGReflCo@
               | otherwise      = extendTvSubst (extendTCvInScope subst tv2) tv1 $
                                  TyVarTy tv2 `mkCastTy` mkSymCo k_co

    go_forall subst (ForAllCo cv1 k_co co)
      | isCoVar cv1
      = mkTyCoInvForAllTy cv2 (go_forall subst' co)
      where
        k2 = coercionRKind k_co
        r         = coVarRole cv1
        eta1      = mkNthCo r 2 (downgradeRole r Nominal k_co)
        eta2      = mkNthCo r 3 (downgradeRole r Nominal k_co)

        -- k_co :: (t1 ~r t2) ~N (s1 ~r s2)
        -- k1    = t1 ~r t2
        -- k2    = s1 ~r s2
        -- cv1  :: t1 ~r t2
        -- cv2  :: s1 ~r s2
        -- eta1 :: t1 ~r s1
        -- eta2 :: t2 ~r s2
        -- n_subst  = (eta1 ; cv2 ; sym eta2) :: t1 ~r t2

        cv2     = setVarType cv1 (substTy subst k2)
        n_subst = eta1 `mkTransCo` (mkCoVarCo cv2) `mkTransCo` (mkSymCo eta2)
        subst'  | isReflCo k_co = extendTCvInScope subst cv1
                | otherwise     = extendCvSubst (extendTCvInScope subst cv2)
                                                cv1 n_subst

    go_forall subst other_co
      -- when other_co is not a ForAllCo
      = substTy subst (go other_co)

{-

Note [Nested ForAllCos]
~~~~~~~~~~~~~~~~~~~~~~~

Suppose we need `coercionKind (ForAllCo a1 (ForAllCo a2 ... (ForAllCo an
co)...) )`.   We do not want to perform `n` single-type-variable
substitutions over the kind of `co`; rather we want to do one substitution
which substitutes for all of `a1`, `a2` ... simultaneously.  If we do one
at a time we get the performance hole reported in #11735.

Solution: gather up the type variables for nested `ForAllCos`, and
substitute for them all at once.  Remarkably, for #11735 this single
change reduces /total/ compile time by a factor of more than ten.

-}

-- | Retrieve the role from a coercion.
coercionRole :: Coercion -> Role
coercionRole = go
  where
    go (Refl _) = Nominal
    go (GRefl r _ _) = r
    go (TyConAppCo r _ _) = r
    go (AppCo co1 _) = go co1
    go (ForAllCo _ _ co) = go co
    go (FunCo r _ _ _) = r
    go (CoVarCo cv) = coVarRole cv
    go (HoleCo h)   = coVarRole (coHoleCoVar h)
    go (AxiomInstCo ax _ _) = coAxiomRole ax
    go (UnivCo _ r _ _)  = r
    go (SymCo co) = go co
    go (TransCo co1 _co2) = go co1
    go (NthCo r _d _co) = r
    go (LRCo {}) = Nominal
    go (InstCo co _) = go co
    go (KindCo {}) = Nominal
    go (SubCo _) = Representational
    go (AxiomRuleCo ax _) = coaxrRole ax

{-
Note [Nested InstCos]
~~~~~~~~~~~~~~~~~~~~~
In #5631 we found that 70% of the entire compilation time was
being spent in coercionKind!  The reason was that we had
   (g @ ty1 @ ty2 .. @ ty100)    -- The "@s" are InstCos
where
   g :: forall a1 a2 .. a100. phi
If we deal with the InstCos one at a time, we'll do this:
   1.  Find the kind of (g @ ty1 .. @ ty99) : forall a100. phi'
   2.  Substitute phi'[ ty100/a100 ], a single tyvar->type subst
But this is a *quadratic* algorithm, and the blew up #5631.
So it's very important to do the substitution simultaneously;
cf Type.piResultTys (which in fact we call here).

-}

-- | Makes a coercion type from two types: the types whose equality
-- is proven by the relevant 'Coercion'
mkCoercionType :: Role -> Type -> Type -> Type
mkCoercionType Nominal          = mkPrimEqPred
mkCoercionType Representational = mkReprPrimEqPred
mkCoercionType Phantom          = \ty1 ty2 ->
  let ki1 = typeKind ty1
      ki2 = typeKind ty2
  in
  TyConApp eqPhantPrimTyCon [ki1, ki2, ty1, ty2]

mkHeteroCoercionType :: Role -> Kind -> Kind -> Type -> Type -> Type
mkHeteroCoercionType Nominal          = mkHeteroPrimEqPred
mkHeteroCoercionType Representational = mkHeteroReprPrimEqPred
mkHeteroCoercionType Phantom          = panic "mkHeteroCoercionType"

-- | Creates a primitive type equality predicate.
-- Invariant: the types are not Coercions
mkPrimEqPred :: Type -> Type -> Type
mkPrimEqPred ty1 ty2
  = mkTyConApp eqPrimTyCon [k1, k2, ty1, ty2]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

-- | Makes a lifted equality predicate at the given role
mkPrimEqPredRole :: Role -> Type -> Type -> PredType
mkPrimEqPredRole Nominal          = mkPrimEqPred
mkPrimEqPredRole Representational = mkReprPrimEqPred
mkPrimEqPredRole Phantom          = panic "mkPrimEqPredRole phantom"

-- | Creates a primitive type equality predicate with explicit kinds
mkHeteroPrimEqPred :: Kind -> Kind -> Type -> Type -> Type
mkHeteroPrimEqPred k1 k2 ty1 ty2 = mkTyConApp eqPrimTyCon [k1, k2, ty1, ty2]

-- | Creates a primitive representational type equality predicate
-- with explicit kinds
mkHeteroReprPrimEqPred :: Kind -> Kind -> Type -> Type -> Type
mkHeteroReprPrimEqPred k1 k2 ty1 ty2
  = mkTyConApp eqReprPrimTyCon [k1, k2, ty1, ty2]

mkReprPrimEqPred :: Type -> Type -> Type
mkReprPrimEqPred ty1  ty2
  = mkTyConApp eqReprPrimTyCon [k1, k2, ty1, ty2]
  where
    k1 = typeKind ty1
    k2 = typeKind ty2

-- | Assuming that two types are the same, ignoring coercions, find
-- a nominal coercion between the types. This is useful when optimizing
-- transitivity over coercion applications, where splitting two
-- AppCos might yield different kinds. See Note [EtaAppCo] in
-- "GHC.Core.Coercion.Opt".
buildCoercion :: Type -> Type -> CoercionN
buildCoercion orig_ty1 orig_ty2 = go orig_ty1 orig_ty2
  where
    go ty1 ty2 | Just ty1' <- coreView ty1 = go ty1' ty2
               | Just ty2' <- coreView ty2 = go ty1 ty2'

    go (CastTy ty1 co) ty2
      = let co' = go ty1 ty2
            r = coercionRole co'
        in  mkCoherenceLeftCo r ty1 co co'

    go ty1 (CastTy ty2 co)
      = let co' = go ty1 ty2
            r = coercionRole co'
        in  mkCoherenceRightCo r ty2 co co'

    go ty1@(TyVarTy tv1) _tyvarty
      = assert (case _tyvarty of
                  { TyVarTy tv2 -> tv1 == tv2
                  ; _           -> False      }) $
        mkNomReflCo ty1

    go (FunTy { ft_mult = w1, ft_arg = arg1, ft_res = res1 })
       (FunTy { ft_mult = w2, ft_arg = arg2, ft_res = res2 })
      = mkFunCo Nominal (go w1 w2) (go arg1 arg2) (go res1 res2)

    go (TyConApp tc1 args1) (TyConApp tc2 args2)
      = assert (tc1 == tc2) $
        mkTyConAppCo Nominal tc1 (zipWith go args1 args2)

    go (AppTy ty1a ty1b) ty2
      | Just (ty2a, ty2b) <- repSplitAppTy_maybe ty2
      = mkAppCo (go ty1a ty2a) (go ty1b ty2b)

    go ty1 (AppTy ty2a ty2b)
      | Just (ty1a, ty1b) <- repSplitAppTy_maybe ty1
      = mkAppCo (go ty1a ty2a) (go ty1b ty2b)

    go (ForAllTy (Bndr tv1 _flag1) ty1) (ForAllTy (Bndr tv2 _flag2) ty2)
      | isTyVar tv1
      = assert (isTyVar tv2) $
        mkForAllCo tv1 kind_co (go ty1 ty2')
      where kind_co  = go (tyVarKind tv1) (tyVarKind tv2)
            in_scope = mkInScopeSet $ tyCoVarsOfType ty2 `unionVarSet` tyCoVarsOfCo kind_co
            ty2'     = substTyWithInScope in_scope [tv2]
                         [mkTyVarTy tv1 `mkCastTy` kind_co]
                         ty2

    go (ForAllTy (Bndr cv1 _flag1) ty1) (ForAllTy (Bndr cv2 _flag2) ty2)
      = assert (isCoVar cv1 && isCoVar cv2) $
        mkForAllCo cv1 kind_co (go ty1 ty2')
      where s1 = varType cv1
            s2 = varType cv2
            kind_co = go s1 s2

            -- s1 = t1 ~r t2
            -- s2 = t3 ~r t4
            -- kind_co :: (t1 ~r t2) ~N (t3 ~r t4)
            -- eta1 :: t1 ~r t3
            -- eta2 :: t2 ~r t4

            r    = coVarRole cv1
            kind_co' = downgradeRole r Nominal kind_co
            eta1 = mkNthCo r 2 kind_co'
            eta2 = mkNthCo r 3 kind_co'

            subst = mkEmptyTCvSubst $ mkInScopeSet $
                      tyCoVarsOfType ty2 `unionVarSet` tyCoVarsOfCo kind_co
            ty2'  = substTy (extendCvSubst subst cv2 $ mkSymCo eta1 `mkTransCo`
                                                       mkCoVarCo cv1 `mkTransCo`
                                                       eta2)
                            ty2

    go ty1@(LitTy lit1) _lit2
      = assert (case _lit2 of
                  { LitTy lit2 -> lit1 == lit2
                  ; _          -> False        }) $
        mkNomReflCo ty1

    go (CoercionTy co1) (CoercionTy co2)
      = mkProofIrrelCo Nominal kind_co co1 co2
      where
        kind_co = go (coercionType co1) (coercionType co2)

    go ty1 ty2
      = pprPanic "buildKindCoercion" (vcat [ ppr orig_ty1, ppr orig_ty2
                                           , ppr ty1, ppr ty2 ])


{-
%************************************************************************
%*                                                                      *
       Coercion holes
%*                                                                      *
%************************************************************************
-}

has_co_hole_ty :: Type -> Monoid.Any
has_co_hole_co :: Coercion -> Monoid.Any
(has_co_hole_ty, _, has_co_hole_co, _)
  = foldTyCo folder ()
  where
    folder = TyCoFolder { tcf_view  = noView
                        , tcf_tyvar = const2 (Monoid.Any False)
                        , tcf_covar = const2 (Monoid.Any False)
                        , tcf_hole  = const2 (Monoid.Any True)
                        , tcf_tycobinder = const2
                        }

-- | Is there a coercion hole in this type?
hasCoercionHoleTy :: Type -> Bool
hasCoercionHoleTy = Monoid.getAny . has_co_hole_ty

-- | Is there a coercion hole in this coercion?
hasCoercionHoleCo :: Coercion -> Bool
hasCoercionHoleCo = Monoid.getAny . has_co_hole_co

hasThisCoercionHoleTy :: Type -> CoercionHole -> Bool
hasThisCoercionHoleTy ty hole = Monoid.getAny (f ty)
  where
    (f, _, _, _) = foldTyCo folder ()

    folder = TyCoFolder { tcf_view  = noView
                        , tcf_tyvar = const2 (Monoid.Any False)
                        , tcf_covar = const2 (Monoid.Any False)
                        , tcf_hole  = \ _ h -> Monoid.Any (getUnique h == getUnique hole)
                        , tcf_tycobinder = const2
                        }

-- | Set the type of a 'CoercionHole'
setCoHoleType :: CoercionHole -> Type -> CoercionHole
setCoHoleType h t = setCoHoleCoVar h (setVarType (coHoleCoVar h) t)
