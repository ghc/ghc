{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE CPP, MultiWayIf, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Tc.Utils.Instantiate (
       topSkolemise,
       topInstantiate, topInstantiateInferred,
       instCall, instDFunType, instStupidTheta, instTyVarsWith,
       newWanted, newWanteds,

       tcInstInvisibleTyBinders, tcInstInvisibleTyBinder,

       newOverloadedLit, mkOverLit,

       newClsInst,
       tcGetInsts, tcGetInstEnvs, getOverlapFlag,
       tcExtendLocalInstEnv,
       instCallConstraints, newMethodFromName,
       tcSyntaxName,

       -- Simple functions over evidence variables
       tyCoVarsOfWC,
       tyCoVarsOfCt, tyCoVarsOfCts,
    ) where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-}   GHC.Tc.Gen.Expr( tcCheckPolyExpr, tcSyntaxOp )
import {-# SOURCE #-}   GHC.Tc.Utils.Unify( unifyType, unifyKind )

import GHC.Types.Basic ( IntegralLit(..), SourceText(..) )
import GHC.Hs
import GHC.Tc.Utils.Zonk
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Constraint
import GHC.Core.Predicate
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.Env
import GHC.Tc.Types.Evidence
import GHC.Core.InstEnv
import GHC.Builtin.Types  ( heqDataCon, eqDataCon )
import GHC.Core    ( isOrphan )
import GHC.Tc.Instance.FunDeps
import GHC.Tc.Utils.TcMType
import GHC.Core.Type
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Ppr ( debugPprType )
import GHC.Tc.Utils.TcType
import GHC.Driver.Types
import GHC.Core.Class( Class )
import GHC.Types.Id.Make( mkDictFunId )
import GHC.Core( Expr(..) )  -- For the Coercion constructor
import GHC.Types.Id
import GHC.Types.Name
import GHC.Types.Var ( EvVar, tyVarName, VarBndr(..) )
import GHC.Core.DataCon
import GHC.Types.Var.Env
import GHC.Builtin.Names
import GHC.Types.SrcLoc as SrcLoc
import GHC.Driver.Session
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Types.Basic ( TypeOrKind(..) )
import qualified GHC.LanguageExtensions as LangExt

import Data.List ( sortBy )
import Control.Monad( unless )
import Data.Function ( on )

{-
************************************************************************
*                                                                      *
                Creating and emittind constraints
*                                                                      *
************************************************************************
-}

newMethodFromName
  :: CtOrigin              -- ^ why do we need this?
  -> Name                  -- ^ name of the method
  -> [TcRhoType]           -- ^ types with which to instantiate the class
  -> TcM (HsExpr GhcTcId)
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
       ; wrap <- ASSERT( not (isForAllTy ty) && isSingleton theta )
                 instCall origin ty_args theta

       ; return (mkHsWrap wrap (HsVar noExtField (noLoc id))) }

{-
************************************************************************
*                                                                      *
         Instantiation and skolemisation
*                                                                      *
************************************************************************

Note [Skolemisation]
~~~~~~~~~~~~~~~~~~~~
topSkolemise decomposes and skolemises a type, returning a type
with no top level foralls or (=>)

Examples:

  topSkolemise (forall a. Ord a => a -> a)
    =  ( wp, [a], [d:Ord a], a->a )
    where wp = /\a. \(d:Ord a). <hole> a d

  topSkolemise  (forall a. Ord a => forall b. Eq b => a->b->b)
    =  ( wp, [a,b], [d1:Ord a,d2:Eq b], a->b->b )
    where wp = /\a.\(d1:Ord a)./\b.\(d2:Ord b). <hole> a d1 b d2

This second example is the reason for the recursive 'go'
function in topSkolemise: we must remove successive layers
of foralls and (=>).

In general,
  if      topSkolemise ty = (wrap, tvs, evs, rho)
    and   e :: rho
  then    wrap e :: ty
    and   'wrap' binds {tvs, evs}

-}

topSkolemise :: TcSigmaType
             -> TcM ( HsWrapper
                    , [(Name,TyVar)]     -- All skolemised variables
                    , [EvVar]            -- All "given"s
                    , TcRhoType )
-- See Note [Skolemisation]
topSkolemise ty
  = go init_subst idHsWrapper [] [] ty
  where
    init_subst = mkEmptyTCvSubst (mkInScopeSet (tyCoVarsOfType ty))

    -- Why recursive?  See Note [Skolemisation]
    go subst wrap tv_prs ev_vars ty
      | (tvs, theta, inner_ty) <- tcSplitSigmaTy ty
      , not (null tvs && null theta)
      = do { (subst', tvs1) <- tcInstSkolTyVarsX subst tvs
           ; ev_vars1       <- newEvVars (substTheta subst' theta)
           ; go subst'
                (wrap <.> mkWpTyLams tvs1 <.> mkWpLams ev_vars1)
                (tv_prs ++ (map tyVarName tvs `zip` tvs1))
                (ev_vars ++ ev_vars1)
                inner_ty }

      | otherwise
      = return (wrap, tv_prs, ev_vars, substTy subst ty)
        -- substTy is a quick no-op on an empty substitution

-- | Instantiate all outer type variables
-- and any context. Never looks through arrows.
topInstantiate :: CtOrigin -> TcSigmaType -> TcM (HsWrapper, TcRhoType)
-- if    topInstantiate ty = (wrap, rho)
-- and   e :: ty
-- then  wrap e :: rho  (that is, wrap :: ty "->" rho)
-- NB: always returns a rho-type, with no top-level forall or (=>)
topInstantiate = top_instantiate True

-- | Instantiate all outer 'Inferred' binders
-- and any context. Never looks through arrows or specified type variables.
-- Used for visible type application.
topInstantiateInferred :: CtOrigin -> TcSigmaType
                       -> TcM (HsWrapper, TcSigmaType)
-- if    topInstantiate ty = (wrap, rho)
-- and   e :: ty
-- then  wrap e :: rho
-- NB: may return a sigma-type
topInstantiateInferred = top_instantiate False

top_instantiate :: Bool   -- True  <=> instantiate *all* variables
                          -- False <=> instantiate only the inferred ones
                -> CtOrigin -> TcSigmaType -> TcM (HsWrapper, TcRhoType)
top_instantiate inst_all orig ty
  | (binders, phi) <- tcSplitForAllVarBndrs ty
  , (theta, rho)   <- tcSplitPhiTy phi
  , not (null binders && null theta)
  = do { let (inst_bndrs, leave_bndrs) = span should_inst binders
             (inst_theta, leave_theta)
               | null leave_bndrs = (theta, [])
               | otherwise        = ([], theta)
             in_scope    = mkInScopeSet (tyCoVarsOfType ty)
             empty_subst = mkEmptyTCvSubst in_scope
             inst_tvs    = binderVars inst_bndrs
       ; (subst, inst_tvs') <- mapAccumLM newMetaTyVarX empty_subst inst_tvs
       ; let inst_theta' = substTheta subst inst_theta
             sigma'      = substTy subst (mkForAllTys leave_bndrs $
                                          mkPhiTy leave_theta rho)
             inst_tv_tys' = mkTyVarTys inst_tvs'

       ; wrap1 <- instCall orig inst_tv_tys' inst_theta'
       ; traceTc "Instantiating"
                 (vcat [ text "all tyvars?" <+> ppr inst_all
                       , text "origin" <+> pprCtOrigin orig
                       , text "type" <+> debugPprType ty
                       , text "theta" <+> ppr theta
                       , text "leave_bndrs" <+> ppr leave_bndrs
                       , text "with" <+> vcat (map debugPprType inst_tv_tys')
                       , text "theta:" <+>  ppr inst_theta' ])

       ; (wrap2, rho2) <-
           if null leave_bndrs   -- NB: if inst_all is True then leave_bndrs = []

         -- account for types like forall a. Num a => forall b. Ord b => ...
           then top_instantiate inst_all orig sigma'

         -- but don't loop if there were any un-inst'able tyvars
           else return (idHsWrapper, sigma')

       ; return (wrap2 <.> wrap1, rho2) }

  | otherwise = return (idHsWrapper, ty)
  where

    should_inst bndr
      | inst_all  = True
      | otherwise = binderArgFlag bndr == Inferred

instTyVarsWith :: CtOrigin -> [TyVar] -> [TcType] -> TcM TCvSubst
-- Use this when you want to instantiate (forall a b c. ty) with
-- types [ta, tb, tc], but when the kinds of 'a' and 'ta' might
-- not yet match (perhaps because there are unsolved constraints; #14154)
-- If they don't match, emit a kind-equality to promise that they will
-- eventually do so, and thus make a kind-homongeneous substitution.
instTyVarsWith orig tvs tys
  = go emptyTCvSubst tvs tys
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
        ty_kind = tcTypeKind ty

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
  = do { evs <- mapM go preds
       ; traceTc "instCallConstraints" (ppr evs)
       ; return (mkWpEvApps evs) }
  where
    go :: TcPredType -> TcM EvTerm
    go pred
     | Just (Nominal, ty1, ty2) <- getEqPredTys_maybe pred -- Try short-cut #1
     = do  { co <- unifyType Nothing ty1 ty2
           ; return (evCoercion co) }

       -- Try short-cut #2
     | Just (tc, args@[_, _, ty1, ty2]) <- splitTyConApp_maybe pred
     , tc `hasKey` heqTyConKey
     = do { co <- unifyType Nothing ty1 ty2
          ; return (evDFunApp (dataConWrapId heqDataCon) args [Coercion co]) }

     | otherwise
     = emitWanted orig pred

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
    empty_subst = mkEmptyTCvSubst (mkInScopeSet (tyCoVarsOfType dfun_ty))
                  -- With quantified constraints, the
                  -- type of a dfun may not be closed

    go :: TCvSubst -> [TyVar] -> [DFunInstType] -> TcM (TCvSubst, [TcType])
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

-- | Instantiates up to n invisible binders
-- Returns the instantiating types, and body kind
tcInstInvisibleTyBinders :: Int -> TcKind -> TcM ([TcType], TcKind)

tcInstInvisibleTyBinders 0 kind
  = return ([], kind)
tcInstInvisibleTyBinders n ty
  = go n empty_subst ty
  where
    empty_subst = mkEmptyTCvSubst (mkInScopeSet (tyCoVarsOfType ty))

    go n subst kind
      | n > 0
      , Just (bndr, body) <- tcSplitPiTy_maybe kind
      , isInvisibleBinder bndr
      = do { (subst', arg) <- tcInstInvisibleTyBinder subst bndr
           ; (args, inner_ty) <- go (n-1) subst' body
           ; return (arg:args, inner_ty) }
      | otherwise
      = return ([], substTy subst kind)

-- | Used only in *types*
tcInstInvisibleTyBinder :: TCvSubst -> TyBinder -> TcM (TCvSubst, TcType)
tcInstInvisibleTyBinder subst (Named (Bndr tv _))
  = do { (subst', tv') <- newMetaTyVarX subst tv
       ; return (subst', mkTyVarTy tv') }

tcInstInvisibleTyBinder subst (Anon af ty)
  | Just (mk, k1, k2) <- get_eq_tys_maybe (substTy subst ty)
    -- Equality is the *only* constraint currently handled in types.
    -- See Note [Constraints in kinds] in GHC.Core.TyCo.Rep
  = ASSERT( af == InvisArg )
    do { co <- unifyKind Nothing k1 k2
       ; arg' <- mk co
       ; return (subst, arg') }

  | otherwise  -- This should never happen
               -- See GHC.Core.TyCo.Rep Note [Constraints in kinds]
  = pprPanic "tcInvisibleTyBinder" (ppr ty)

-------------------------------
get_eq_tys_maybe :: Type
                 -> Maybe ( Coercion -> TcM Type
                             -- given a coercion proving t1 ~# t2, produce the
                             -- right instantiation for the TyBinder at hand
                          , Type  -- t1
                          , Type  -- t2
                          )
-- See Note [Constraints in kinds] in GHC.Core.TyCo.Rep
get_eq_tys_maybe ty
  -- Lifted heterogeneous equality (~~)
  | Just (tc, [_, _, k1, k2]) <- splitTyConApp_maybe ty
  , tc `hasKey` heqTyConKey
  = Just (\co -> mkHEqBoxTy co k1 k2, k1, k2)

  -- Lifted homogeneous equality (~)
  | Just (tc, [_, k1, k2]) <- splitTyConApp_maybe ty
  , tc `hasKey` eqTyConKey
  = Just (\co -> mkEqBoxTy co k1 k2, k1, k2)

  | otherwise
  = Nothing

-- | This takes @a ~# b@ and returns @a ~~ b@.
mkHEqBoxTy :: TcCoercion -> Type -> Type -> TcM Type
-- monadic just for convenience with mkEqBoxTy
mkHEqBoxTy co ty1 ty2
  = return $
    mkTyConApp (promoteDataCon heqDataCon) [k1, k2, ty1, ty2, mkCoercionTy co]
  where k1 = tcTypeKind ty1
        k2 = tcTypeKind ty2

-- | This takes @a ~# b@ and returns @a ~ b@.
mkEqBoxTy :: TcCoercion -> Type -> Type -> TcM Type
mkEqBoxTy co ty1 ty2
  = return $
    mkTyConApp (promoteDataCon eqDataCon) [k, ty1, ty2, mkCoercionTy co]
  where k = tcTypeKind ty1

{-
************************************************************************
*                                                                      *
                Literals
*                                                                      *
************************************************************************

-}

{-
In newOverloadedLit we convert directly to an Int or Integer if we
know that's what we want.  This may save some time, by not
temporarily generating overloaded literals, but it won't catch all
cases (the rest are caught in lookupInst).

-}

newOverloadedLit :: HsOverLit GhcRn
                 -> ExpRhoType
                 -> TcM (HsOverLit GhcTcId)
newOverloadedLit
  lit@(OverLit { ol_val = val, ol_ext = rebindable }) res_ty
  | not rebindable
    -- all built-in overloaded lits are tau-types, so we can just
    -- tauify the ExpType
  = do { res_ty <- expTypeToType res_ty
       ; dflags <- getDynFlags
       ; let platform = targetPlatform dflags
       ; case shortCutLit platform val res_ty of
        -- Do not generate a LitInst for rebindable syntax.
        -- Reason: If we do, tcSimplify will call lookupInst, which
        --         will call tcSyntaxName, which does unification,
        --         which tcSimplify doesn't like
           Just expr -> return (lit { ol_witness = expr
                                    , ol_ext = OverLitTc False res_ty })
           Nothing   -> newNonTrivialOverloadedLit orig lit
                                                   (mkCheckExpType res_ty) }

  | otherwise
  = newNonTrivialOverloadedLit orig lit res_ty
  where
    orig = LiteralOrigin lit

-- Does not handle things that 'shortCutLit' can handle. See also
-- newOverloadedLit in GHC.Tc.Utils.Unify
newNonTrivialOverloadedLit :: CtOrigin
                           -> HsOverLit GhcRn
                           -> ExpRhoType
                           -> TcM (HsOverLit GhcTcId)
newNonTrivialOverloadedLit orig
  lit@(OverLit { ol_val = val, ol_witness = HsVar _ (L _ meth_name)
               , ol_ext = rebindable }) res_ty
  = do  { hs_lit <- mkOverLit val
        ; let lit_ty = hsLitType hs_lit
        ; (_, fi') <- tcSyntaxOp orig (mkRnSyntaxExpr meth_name)
                                      [synKnownType lit_ty] res_ty $
                      \_ -> return ()
        ; let L _ witness = nlHsSyntaxApps fi' [nlHsLit hs_lit]
        ; res_ty <- readExpType res_ty
        ; return (lit { ol_witness = witness
                      , ol_ext = OverLitTc rebindable res_ty }) }
newNonTrivialOverloadedLit _ lit _
  = pprPanic "newNonTrivialOverloadedLit" (ppr lit)

------------
mkOverLit ::OverLitVal -> TcM (HsLit GhcTc)
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
             -> TcType                 -- ^ Type to instantiate it at
             -> (Name, HsExpr GhcRn)   -- ^ (Standard name, user name)
             -> TcM (Name, HsExpr GhcTcId)
                                       -- ^ (Standard name, suitable expression)
-- USED ONLY FOR CmdTop (sigh) ***
-- See Note [CmdSyntaxTable] in GHC.Hs.Expr

tcSyntaxName orig ty (std_nm, HsVar _ (L _ user_nm))
  | std_nm == user_nm
  = do rhs <- newMethodFromName orig std_nm [ty]
       return (std_nm, rhs)

tcSyntaxName orig ty (std_nm, user_nm_expr) = do
    std_id <- tcLookupId std_nm
    let
        -- C.f. newMethodAtLoc
        ([tv], _, tau) = tcSplitSigmaTy (idType std_id)
        sigma1         = substTyWith [tv] [ty] tau
        -- Actually, the "tau-type" might be a sigma-type in the
        -- case of locally-polymorphic methods.

    addErrCtxtM (syntaxNameCtxt user_nm_expr orig sigma1) $ do

        -- Check that the user-supplied thing has the
        -- same type as the standard one.
        -- Tiresome jiggling because tcCheckSigma takes a located expression
     span <- getSrcSpanM
     expr <- tcCheckPolyExpr (L span user_nm_expr) sigma1
     return (std_nm, unLoc expr)

syntaxNameCtxt :: HsExpr GhcRn -> CtOrigin -> Type -> TidyEnv
               -> TcRn (TidyEnv, SDoc)
syntaxNameCtxt name orig ty tidy_env
  = do { inst_loc <- getCtLocM orig (Just TypeLevel)
       ; let msg = vcat [ text "When checking that" <+> quotes (ppr name)
                          <+> text "(needed by a syntactic construct)"
                        , nest 2 (text "has the required type:"
                                  <+> ppr (tidyType tidy_env ty))
                        , nest 2 (pprCtLoc inst_loc) ]
       ; return (tidy_env, msg) }

{-
************************************************************************
*                                                                      *
                Instances
*                                                                      *
************************************************************************
-}

getOverlapFlag :: Maybe OverlapMode -> TcM OverlapFlag
-- Construct the OverlapFlag from the global module flags,
-- but if the overlap_mode argument is (Just m),
--     set the OverlapMode to 'm'
getOverlapFlag overlap_mode
  = do  { dflags <- getDynFlags
        ; let overlap_ok    = xopt LangExt.OverlappingInstances dflags
              incoherent_ok = xopt LangExt.IncoherentInstances  dflags
              use x = OverlapFlag { isSafeOverlap = safeLanguageOn dflags
                                  , overlapMode   = x }
              default_oflag | incoherent_ok = use (Incoherent NoSourceText)
                            | overlap_ok    = use (Overlaps NoSourceText)
                            | otherwise     = use (NoOverlap NoSourceText)

              final_oflag = setOverlapModeMaybe default_oflag overlap_mode
        ; return final_oflag }

tcGetInsts :: TcM [ClsInst]
-- Gets the local class instances.
tcGetInsts = fmap tcg_insts getGblEnv

newClsInst :: Maybe OverlapMode -> Name -> [TyVar] -> ThetaType
           -> Class -> [Type] -> TcM ClsInst
newClsInst overlap_mode dfun_name tvs theta clas tys
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
       ; let inst = mkLocalInstance dfun oflag tvs' clas tys'
       ; warnIfFlag Opt_WarnOrphans
                    (isOrphan (is_orphan inst))
                    (instOrphWarn inst)
       ; return inst }

instOrphWarn :: ClsInst -> SDoc
instOrphWarn inst
  = hang (text "Orphan instance:") 2 (pprInstanceHdr inst)
    $$ text "To avoid this"
    $$ nest 4 (vcat possibilities)
  where
    possibilities =
      text "move the instance declaration to the module of the class or of the type, or" :
      text "wrap the type with a newtype and declare the instance on the new type." :
      []

tcExtendLocalInstEnv :: [ClsInst] -> TcM a -> TcM a
  -- Add new locally-defined instances
tcExtendLocalInstEnv dfuns thing_inside
 = do { traceDFuns dfuns
      ; env <- getGblEnv
      ; (inst_env', cls_insts') <- foldlM addLocalInst
                                          (tcg_inst_env env, tcg_insts env)
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

{-
Note [Signature files and type class instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
        Errors and tracing
*                                                                      *
************************************************************************
-}

traceDFuns :: [ClsInst] -> TcRn ()
traceDFuns ispecs
  = traceTc "Adding instances:" (vcat (map pp ispecs))
  where
    pp ispec = hang (ppr (instanceDFunId ispec) <+> colon)
                  2 (ppr ispec)
        -- Print the dfun name itself too

funDepErr :: ClsInst -> [ClsInst] -> TcRn ()
funDepErr ispec ispecs
  = addClsInstsErr (text "Functional dependencies conflict between instance declarations:")
                    (ispec : ispecs)

dupInstErr :: ClsInst -> ClsInst -> TcRn ()
dupInstErr ispec dup_ispec
  = addClsInstsErr (text "Duplicate instance declarations:")
                    [ispec, dup_ispec]

addClsInstsErr :: SDoc -> [ClsInst] -> TcRn ()
addClsInstsErr herald ispecs
  = setSrcSpan (getSrcSpan (head sorted)) $
    addErr (hang herald 2 (pprInstances sorted))
 where
   sorted = sortBy (SrcLoc.leftmost_smallest `on` getSrcSpan) ispecs
   -- The sortBy just arranges that instances are displayed in order
   -- of source location, which reduced wobbling in error messages,
   -- and is better for users
