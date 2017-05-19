{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


The @Inst@ type: dictionaries or method instances
-}

{-# LANGUAGE CPP, MultiWayIf, TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Inst (
       deeplySkolemise,
       topInstantiate, topInstantiateInferred, deeplyInstantiate,
       instCall, instDFunType, instStupidTheta,
       newWanted, newWanteds,

       tcInstBinders, tcInstBindersX, tcInstBinderX,

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

import {-# SOURCE #-}   TcExpr( tcPolyExpr, tcSyntaxOp )
import {-# SOURCE #-}   TcUnify( unifyType, unifyKind, noThing )

import BasicTypes ( IntegralLit(..), SourceText(..) )
import FastString
import HsSyn
import TcHsSyn
import TcRnMonad
import TcEnv
import TcEvidence
import InstEnv
import TysWiredIn  ( heqDataCon, coercibleDataCon )
import CoreSyn     ( isOrphan )
import FunDeps
import TcMType
import Type
import TyCoRep     ( TyBinder(..) )
import TcType
import HscTypes
import Class( Class )
import MkId( mkDictFunId )
import Id
import Name
import Var      ( EvVar, mkTyVar, tyVarName, TyVarBndr(..) )
import DataCon
import TyCon
import VarEnv
import PrelNames
import SrcLoc
import DynFlags
import Util
import Outputable
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad( unless )

{-
************************************************************************
*                                                                      *
                Creating and emittind constraints
*                                                                      *
************************************************************************
-}

newMethodFromName :: CtOrigin -> Name -> TcRhoType -> TcM (HsExpr GhcTcId)
-- Used when Name is the wired-in name for a wired-in class method,
-- so the caller knows its type for sure, which should be of form
--    forall a. C a => <blah>
-- newMethodFromName is supposed to instantiate just the outer
-- type variable and constraint

newMethodFromName origin name inst_ty
  = do { id <- tcLookupId name
              -- Use tcLookupId not tcLookupGlobalId; the method is almost
              -- always a class op, but with -XRebindableSyntax GHC is
              -- meant to find whatever thing is in scope, and that may
              -- be an ordinary function.

       ; let ty = piResultTy (idType id) inst_ty
             (theta, _caller_knows_this) = tcSplitPhiTy ty
       ; wrap <- ASSERT( not (isForAllTy ty) && isSingleton theta )
                 instCall origin [inst_ty] theta

       ; return (mkHsWrap wrap (HsVar (noLoc id))) }

{-
************************************************************************
*                                                                      *
        Deep instantiation and skolemisation
*                                                                      *
************************************************************************

Note [Deep skolemisation]
~~~~~~~~~~~~~~~~~~~~~~~~~
deeplySkolemise decomposes and skolemises a type, returning a type
with all its arrows visible (ie not buried under foralls)

Examples:

  deeplySkolemise (Int -> forall a. Ord a => blah)
    =  ( wp, [a], [d:Ord a], Int -> blah )
    where wp = \x:Int. /\a. \(d:Ord a). <hole> x

  deeplySkolemise  (forall a. Ord a => Maybe a -> forall b. Eq b => blah)
    =  ( wp, [a,b], [d1:Ord a,d2:Eq b], Maybe a -> blah )
    where wp = /\a.\(d1:Ord a).\(x:Maybe a)./\b.\(d2:Ord b). <hole> x

In general,
  if      deeplySkolemise ty = (wrap, tvs, evs, rho)
    and   e :: rho
  then    wrap e :: ty
    and   'wrap' binds tvs, evs

ToDo: this eta-abstraction plays fast and loose with termination,
      because it can introduce extra lambdas.  Maybe add a `seq` to
      fix this
-}

deeplySkolemise :: TcSigmaType
                -> TcM ( HsWrapper
                       , [(Name,TyVar)]     -- All skolemised variables
                       , [EvVar]            -- All "given"s
                       , TcRhoType )

deeplySkolemise ty
  = go init_subst ty
  where
    init_subst = mkEmptyTCvSubst (mkInScopeSet (tyCoVarsOfType ty))

    go subst ty
      | Just (arg_tys, tvs, theta, ty') <- tcDeepSplitSigmaTy_maybe ty
      = do { let arg_tys' = substTys subst arg_tys
           ; ids1           <- newSysLocalIds (fsLit "dk") arg_tys'
           ; (subst', tvs1) <- tcInstSkolTyVarsX subst tvs
           ; ev_vars1       <- newEvVars (substTheta subst' theta)
           ; (wrap, tvs_prs2, ev_vars2, rho) <- go subst' ty'
           ; let tv_prs1 = map tyVarName tvs `zip` tvs1
           ; return ( mkWpLams ids1
                      <.> mkWpTyLams tvs1
                      <.> mkWpLams ev_vars1
                      <.> wrap
                      <.> mkWpEvVarApps ids1
                    , tv_prs1  ++ tvs_prs2
                    , ev_vars1 ++ ev_vars2
                    , mkFunTys arg_tys' rho ) }

      | otherwise
      = return (idHsWrapper, [], [], substTy subst ty)
        -- substTy is a quick no-op on an empty substitution

-- | Instantiate all outer type variables
-- and any context. Never looks through arrows.
topInstantiate :: CtOrigin -> TcSigmaType -> TcM (HsWrapper, TcRhoType)
-- if    topInstantiate ty = (wrap, rho)
-- and   e :: ty
-- then  wrap e :: rho  (that is, wrap :: ty "->" rho)
topInstantiate = top_instantiate True

-- | Instantiate all outer 'Inferred' binders
-- and any context. Never looks through arrows or specified type variables.
-- Used for visible type application.
topInstantiateInferred :: CtOrigin -> TcSigmaType
                       -> TcM (HsWrapper, TcSigmaType)
-- if    topInstantiate ty = (wrap, rho)
-- and   e :: ty
-- then  wrap e :: rho
topInstantiateInferred = top_instantiate False

top_instantiate :: Bool   -- True  <=> instantiate *all* variables
                          -- False <=> instantiate only the inferred ones
                -> CtOrigin -> TcSigmaType -> TcM (HsWrapper, TcRhoType)
top_instantiate inst_all orig ty
  | not (null binders && null theta)
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
                                          mkFunTys leave_theta rho)

       ; wrap1 <- instCall orig (mkTyVarTys inst_tvs') inst_theta'
       ; traceTc "Instantiating"
                 (vcat [ text "all tyvars?" <+> ppr inst_all
                       , text "origin" <+> pprCtOrigin orig
                       , text "type" <+> ppr ty
                       , text "theta" <+> ppr theta
                       , text "leave_bndrs" <+> ppr leave_bndrs
                       , text "with" <+> ppr inst_tvs'
                       , text "theta:" <+>  ppr inst_theta' ])

       ; (wrap2, rho2) <-
           if null leave_bndrs

         -- account for types like forall a. Num a => forall b. Ord b => ...
           then top_instantiate inst_all orig sigma'

         -- but don't loop if there were any un-inst'able tyvars
           else return (idHsWrapper, sigma')

       ; return (wrap2 <.> wrap1, rho2) }

  | otherwise = return (idHsWrapper, ty)
  where
    (binders, phi) = tcSplitForAllTyVarBndrs ty
    (theta, rho)   = tcSplitPhiTy phi

    should_inst bndr
      | inst_all  = True
      | otherwise = binderArgFlag bndr == Inferred

deeplyInstantiate :: CtOrigin -> TcSigmaType -> TcM (HsWrapper, TcRhoType)
--   Int -> forall a. a -> a  ==>  (\x:Int. [] x alpha) :: Int -> alpha
-- In general if
-- if    deeplyInstantiate ty = (wrap, rho)
-- and   e :: ty
-- then  wrap e :: rho
-- That is, wrap :: ty ~> rho
--
-- If you don't need the HsWrapper returned from this function, consider
-- using tcSplitNestedSigmaTys in TcType, which is a pure alternative that
-- only computes the returned TcRhoType.

deeplyInstantiate orig ty =
  deeply_instantiate orig
                     (mkEmptyTCvSubst (mkInScopeSet (tyCoVarsOfType ty)))
                     ty

deeply_instantiate :: CtOrigin
                   -> TCvSubst
                   -> TcSigmaType -> TcM (HsWrapper, TcRhoType)
-- Internal function to deeply instantiate that builds on an existing subst.
-- It extends the input substitution and applies the final subtitution to
-- the types on return.  See #12549.

deeply_instantiate orig subst ty
  | Just (arg_tys, tvs, theta, rho) <- tcDeepSplitSigmaTy_maybe ty
  = do { (subst', tvs') <- newMetaTyVarsX subst tvs
       ; ids1  <- newSysLocalIds (fsLit "di") (substTys subst' arg_tys)
       ; let theta' = substTheta subst' theta
       ; wrap1 <- instCall orig (mkTyVarTys tvs') theta'
       ; traceTc "Instantiating (deeply)" (vcat [ text "origin" <+> pprCtOrigin orig
                                                , text "type" <+> ppr ty
                                                , text "with" <+> ppr tvs'
                                                , text "args:" <+> ppr ids1
                                                , text "theta:" <+>  ppr theta'
                                                , text "subst:" <+> ppr subst'])
       ; (wrap2, rho2) <- deeply_instantiate orig subst' rho
       ; return (mkWpLams ids1
                    <.> wrap2
                    <.> wrap1
                    <.> mkWpEvVarApps ids1,
                 mkFunTys arg_tys rho2) }

  | otherwise
  = do { let ty' = substTy subst ty
       ; traceTc "deeply_instantiate final subst"
                 (vcat [ text "origin:"   <+> pprCtOrigin orig
                       , text "type:"     <+> ppr ty
                       , text "new type:" <+> ppr ty'
                       , text "subst:"    <+> ppr subst ])
      ; return (idHsWrapper, ty') }

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
    go pred
     | Just (Nominal, ty1, ty2) <- getEqPredTys_maybe pred -- Try short-cut #1
     = do  { co <- unifyType noThing ty1 ty2
           ; return (EvCoercion co) }

       -- Try short-cut #2
     | Just (tc, args@[_, _, ty1, ty2]) <- splitTyConApp_maybe pred
     , tc `hasKey` heqTyConKey
     = do { co <- unifyType noThing ty1 ty2
          ; return (EvDFunApp (dataConWrapId heqDataCon) args [EvCoercion co]) }

     | otherwise
     = emitWanted orig pred

instDFunType :: DFunId -> [DFunInstType]
             -> TcM ( [TcType]      -- instantiated argument types
                    , TcThetaType ) -- instantiated constraint
-- See Note [DFunInstType: instantiating types] in InstEnv
instDFunType dfun_id dfun_inst_tys
  = do { (subst, inst_tys) <- go emptyTCvSubst dfun_tvs dfun_inst_tys
       ; return (inst_tys, substTheta subst dfun_theta) }
  where
    (dfun_tvs, dfun_theta, _) = tcSplitSigmaTy (idType dfun_id)

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

{-
************************************************************************
*                                                                      *
         Instantiating Kinds
*                                                                      *
************************************************************************

-}

---------------------------
-- | This is used to instantiate binders when type-checking *types* only.
-- See also Note [Bidirectional type checking]
tcInstBinders :: [TyBinder] -> TcM (TCvSubst, [TcType])
tcInstBinders = tcInstBindersX emptyTCvSubst Nothing

-- | This is used to instantiate binders when type-checking *types* only.
-- The @VarEnv Kind@ gives some known instantiations.
-- See also Note [Bidirectional type checking]
tcInstBindersX :: TCvSubst -> Maybe (VarEnv Kind)
               -> [TyBinder] -> TcM (TCvSubst, [TcType])
tcInstBindersX subst mb_kind_info bndrs
  = do { (subst, args) <- mapAccumLM (tcInstBinderX mb_kind_info) subst bndrs
       ; traceTc "instantiating tybinders:"
           (vcat $ zipWith (\bndr arg -> ppr bndr <+> text ":=" <+> ppr arg)
                           bndrs args)
       ; return (subst, args) }

-- | Used only in *types*
tcInstBinderX :: Maybe (VarEnv Kind)
              -> TCvSubst -> TyBinder -> TcM (TCvSubst, TcType)
tcInstBinderX mb_kind_info subst (Named (TvBndr tv _))
  = case lookup_tv tv of
      Just ki -> return (extendTvSubstAndInScope subst tv ki, ki)
      Nothing -> do { (subst', tv') <- newMetaTyVarX subst tv
                    ; return (subst', mkTyVarTy tv') }
  where
    lookup_tv tv = do { env <- mb_kind_info   -- `Maybe` monad
                      ; lookupVarEnv env tv }


tcInstBinderX _ subst (Anon ty)
     -- This is the *only* constraint currently handled in types.
  | Just (mk, role, k1, k2) <- get_pred_tys_maybe substed_ty
  = do { let origin = TypeEqOrigin { uo_actual   = k1
                                   , uo_expected = k2
                                   , uo_thing    = Nothing }
       ; co <- case role of
                 Nominal          -> unifyKind noThing k1 k2
                 Representational -> emitWantedEq origin KindLevel role k1 k2
                 Phantom          -> pprPanic "tcInstBinderX Phantom" (ppr ty)
       ; arg' <- mk co k1 k2
       ; return (subst, arg') }

  | isPredTy substed_ty
  = do { let (env, tidy_ty) = tidyOpenType emptyTidyEnv substed_ty
       ; addErrTcM (env, text "Illegal constraint in a type:" <+> ppr tidy_ty)

         -- just invent a new variable so that we can continue
       ; u <- newUnique
       ; let name = mkSysTvName u (fsLit "dict")
       ; return (subst, mkTyVarTy $ mkTyVar name substed_ty) }


  | otherwise
  = do { tv_ty <- newFlexiTyVarTy substed_ty
       ; return (subst, tv_ty) }

  where
    substed_ty = substTy subst ty

      -- handle boxed equality constraints, because it's so easy
    get_pred_tys_maybe ty
      | Just (r, k1, k2) <- getEqPredTys_maybe ty
      = Just (\co _ _ -> return $ mkCoercionTy co, r, k1, k2)
      | Just (tc, [_, _, k1, k2]) <- splitTyConApp_maybe ty
      = if | tc `hasKey` heqTyConKey
             -> Just (mkHEqBoxTy, Nominal, k1, k2)
           | otherwise
             -> Nothing
      | Just (tc, [_, k1, k2]) <- splitTyConApp_maybe ty
      = if | tc `hasKey` eqTyConKey
             -> Just (mkEqBoxTy, Nominal, k1, k2)
           | tc `hasKey` coercibleTyConKey
             -> Just (mkCoercibleBoxTy, Representational, k1, k2)
           | otherwise
             -> Nothing
      | otherwise
      = Nothing

-------------------------------
-- | This takes @a ~# b@ and returns @a ~~ b@.
mkHEqBoxTy :: TcCoercion -> Type -> Type -> TcM Type
-- monadic just for convenience with mkEqBoxTy
mkHEqBoxTy co ty1 ty2
  = return $
    mkTyConApp (promoteDataCon heqDataCon) [k1, k2, ty1, ty2, mkCoercionTy co]
  where k1 = typeKind ty1
        k2 = typeKind ty2

-- | This takes @a ~# b@ and returns @a ~ b@.
mkEqBoxTy :: TcCoercion -> Type -> Type -> TcM Type
mkEqBoxTy co ty1 ty2
  = do { eq_tc <- tcLookupTyCon eqTyConName
       ; let [datacon] = tyConDataCons eq_tc
       ; hetero <- mkHEqBoxTy co ty1 ty2
       ; return $ mkTyConApp (promoteDataCon datacon) [k, ty1, ty2, hetero] }
  where k = typeKind ty1

-- | This takes @a ~R# b@ and returns @Coercible a b@.
mkCoercibleBoxTy :: TcCoercion -> Type -> Type -> TcM Type
-- monadic just for convenience with mkEqBoxTy
mkCoercibleBoxTy co ty1 ty2
  = do { return $
         mkTyConApp (promoteDataCon coercibleDataCon)
                    [k, ty1, ty2, mkCoercionTy co] }
  where k = typeKind ty1

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
  lit@(OverLit { ol_val = val, ol_rebindable = rebindable }) res_ty
  | not rebindable
    -- all built-in overloaded lits are tau-types, so we can just
    -- tauify the ExpType
  = do { res_ty <- expTypeToType res_ty
       ; dflags <- getDynFlags
       ; case shortCutLit dflags val res_ty of
        -- Do not generate a LitInst for rebindable syntax.
        -- Reason: If we do, tcSimplify will call lookupInst, which
        --         will call tcSyntaxName, which does unification,
        --         which tcSimplify doesn't like
           Just expr -> return (lit { ol_witness = expr, ol_type = res_ty
                                    , ol_rebindable = False })
           Nothing   -> newNonTrivialOverloadedLit orig lit
                                                   (mkCheckExpType res_ty) }

  | otherwise
  = newNonTrivialOverloadedLit orig lit res_ty
  where
    orig = LiteralOrigin lit

-- Does not handle things that 'shortCutLit' can handle. See also
-- newOverloadedLit in TcUnify
newNonTrivialOverloadedLit :: CtOrigin
                           -> HsOverLit GhcRn
                           -> ExpRhoType
                           -> TcM (HsOverLit GhcTcId)
newNonTrivialOverloadedLit orig
  lit@(OverLit { ol_val = val, ol_witness = HsVar (L _ meth_name)
               , ol_rebindable = rebindable }) res_ty
  = do  { hs_lit <- mkOverLit val
        ; let lit_ty = hsLitType hs_lit
        ; (_, fi') <- tcSyntaxOp orig (mkRnSyntaxExpr meth_name)
                                      [synKnownType lit_ty] res_ty $
                      \_ -> return ()
        ; let L _ witness = nlHsSyntaxApps fi' [nlHsLit hs_lit]
        ; res_ty <- readExpType res_ty
        ; return (lit { ol_witness = witness
                      , ol_type = res_ty
                      , ol_rebindable = rebindable }) }
newNonTrivialOverloadedLit _ lit _
  = pprPanic "newNonTrivialOverloadedLit" (ppr lit)

------------
mkOverLit ::(HasDefaultX p, SourceTextX p) => OverLitVal -> TcM (HsLit p)
mkOverLit (HsIntegral i)
  = do  { integer_ty <- tcMetaTy integerTyConName
        ; return (HsInteger (setSourceText $ il_text i)
                            (il_value i) integer_ty) }

mkOverLit (HsFractional r)
  = do  { rat_ty <- tcMetaTy rationalTyConName
        ; return (HsRat def r rat_ty) }

mkOverLit (HsIsString src s) = return (HsString (setSourceText src) s)

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
-- See Note [CmdSyntaxTable] in HsExpr

tcSyntaxName orig ty (std_nm, HsVar (L _ user_nm))
  | std_nm == user_nm
  = do rhs <- newMethodFromName orig std_nm ty
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
     expr <- tcPolyExpr (L span user_nm_expr) sigma1
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
             --     an InstInfo, and in TcEnv.pprInstInfoDetails it's
             --     helpful to use the same names

       ; oflag <- getOverlapFlag overlap_mode
       ; let inst = mkLocalInstance dfun oflag tvs' clas tys'
       ; warnIf (Reason Opt_WarnOrphans)
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
   sorted = sortWith getSrcLoc ispecs
   -- The sortWith just arranges that instances are dislayed in order
   -- of source location, which reduced wobbling in error messages,
   -- and is better for users
