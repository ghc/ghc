
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE UndecidableInstances #-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998

-}

-- | Zonking types.
module GHC.Tc.Zonk.Type where

import GHC.Prelude

import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Unique.Set

import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence
import GHC.Tc.Errors.Types ()
import GHC.Tc.Zonk.Monad
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Constraint
--import GHC.Tc.Utils.Monad        -- TcType, amongst others
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr

import GHC.Core.InstEnv (ClsInst(is_tys))
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.Predicate

import GHC.Utils.Constants
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Monad ( mapAccumLM )
import GHC.Utils.Panic

import GHC.Data.Bag
import GHC.Data.Pair

import Control.Monad.IO.Class
import Data.IORef -- SLD TODO: this should be removed in favour
                  -- of using some kind of TcRef definitions

{- *********************************************************************
*                                                                      *
                         Zonking types
*                                                                      *
************************************************************************
-}

{- Note [Sharing when zonking to Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Problem:

    In GHC.Tc.Utils.TcMType.zonkTcTyVar, we short-circuit (Indirect ty) to
    (Indirect zty), see Note [Sharing in zonking] in GHC.Tc.Utils.TcMType. But we
    /can't/ do this when zonking a TcType to a Type (#15552, esp
    comment:3).  Suppose we have

       alpha -> alpha
         where
            alpha is already unified:
             alpha := T{tc-tycon} Int -> Int
         and T is knot-tied

    By "knot-tied" I mean that the occurrence of T is currently a TcTyCon,
    but the global env contains a mapping "T" :-> T{knot-tied-tc}. See
    Note [Type checking recursive type and class declarations] in
    GHC.Tc.TyCl.

    Now we call zonkTcTypeToType on that (alpha -> alpha). If we follow
    the same path as Note [Sharing in zonking] in GHC.Tc.Utils.TcMType, we'll
    update alpha to
       alpha := T{knot-tied-tc} Int -> Int

    But alas, if we encounter alpha for a /second/ time, we end up
    looking at T{knot-tied-tc} and fall into a black hole. The whole
    point of zonkTcTypeToType is that it produces a type full of
    knot-tied tycons, and you must not look at the result!!

    To put it another way (zonkTcTypeToType . zonkTcTypeToType) is not
    the same as zonkTcTypeToType. (If we distinguished TcType from
    Type, this issue would have been a type error!)

Solutions: (see #15552 for other variants)

One possible solution is simply not to do the short-circuiting.
That has less sharing, but maybe sharing is rare. And indeed,
that usually turns out to be viable from a perf point of view

But zonkTyVarOcc implements something a bit better

* ZonkEnv contains ze_meta_tv_env, which maps
      from a MetaTyVar (unification variable)
      to a Type (not a TcType)

* In zonkTyVarOcc, we check this map to see if we have zonked
  this variable before. If so, use the previous answer; if not
  zonk it, and extend the map.

* The map is of course stateful, held in a TcRef. (That is unlike
  the treatment of lexically-scoped variables in ze_tv_env and
  ze_id_env.)

* In zonkTyVarOcc we read the TcRef to look up the unification
  variable:
    - if we get a hit we use the zonked result;
    - if not, in zonk_meta we see if the variable is `Indirect ty`,
      zonk that, and update the map (in finish_meta)
  But Nota Bene that the "update map" step must re-read the TcRef
  (or, more precisely, use updTcRef) because the zonking of the
  `Indirect ty` may have added lots of stuff to the map.  See
  #19668 for an example where this made an asymptotic difference!

Is it worth the extra work of carrying ze_meta_tv_env? Some
non-systematic perf measurements suggest that compiler allocation is
reduced overall (by 0.5% or so) but compile time really doesn't
change.  But in some cases it makes a HUGE difference: see test
T9198 and #19668.  So yes, it seems worth it.
-}

--------------------
-- Works with both type and kind variables
writeMetaTyVar :: HasDebugCallStack => TcTyVar -> TcType -> ZonkM ()
-- Write into a currently-empty MetaTyVar

writeMetaTyVar tyvar ty
  | not debugIsOn
  = writeMetaTyVarRef tyvar (metaTyVarRef tyvar) ty

-- Everything from here on only happens if DEBUG is on
  | not (isTcTyVar tyvar)
  = massertPpr False (text "Writing to non-tc tyvar" <+> ppr tyvar)

  | MetaTv { mtv_ref = ref } <- tcTyVarDetails tyvar
  = writeMetaTyVarRef tyvar ref ty

  | otherwise
  = massertPpr False (text "Writing to non-meta tyvar" <+> ppr tyvar)

--------------------
writeMetaTyVarRef :: HasDebugCallStack => TcTyVar -> IORef MetaDetails -> TcType -> ZonkM ()
-- Here the tyvar is for error checking only;
-- the ref cell must be for the same tyvar
writeMetaTyVarRef tyvar ref ty
  | not debugIsOn
  = do { traceZonk "writeMetaTyVar" (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)
                                     <+> text ":=" <+> ppr ty)
       ; writeTcRefZ ref (Indirect ty) }

  -- Everything from here on only happens if DEBUG is on
  -- Need to zonk 'ty' because we may only recently have promoted
  -- its free meta-tyvars (see Solver.Interact.tryToSolveByUnification)
  | otherwise
  = do { meta_details <- readTcRefZ ref;
       -- Zonk kinds to allow the error check to work
       ; zonked_tv_kind <- zonkTcType tv_kind
       ; zonked_ty      <- zonkTcType ty
       ; let zonked_ty_kind = typeKind zonked_ty
             zonked_ty_lvl  = tcTypeLevel zonked_ty
             level_check_ok  = not (zonked_ty_lvl `strictlyDeeperThan` tv_lvl)
             level_check_msg = ppr zonked_ty_lvl $$ ppr tv_lvl $$ ppr tyvar $$ ppr ty $$ ppr zonked_ty
             kind_check_ok = zonked_ty_kind `eqType` zonked_tv_kind
             -- Note [Extra-constraint holes in partial type signatures] in GHC.Tc.Gen.HsType

             kind_msg = hang (text "Ill-kinded update to meta tyvar")
                           2 (    ppr tyvar <+> text "::" <+> (ppr tv_kind $$ ppr zonked_tv_kind)
                              <+> text ":="
                              <+> ppr ty <+> text "::" <+> (ppr zonked_ty_kind) )

       ; traceZonk "writeMetaTyVar" (ppr tyvar <+> text ":=" <+> ppr ty)

       -- Check for double updates
       ; massertPpr (isFlexi meta_details) (double_upd_msg meta_details)

       -- Check for level OK
       ; massertPpr level_check_ok level_check_msg

       -- Check Kinds ok
       ; massertPpr kind_check_ok kind_msg

       -- Do the write
       ; writeTcRefZ ref (Indirect ty) }
  where
    tv_kind = tyVarKind tyvar

    tv_lvl = tcTyVarLevel tyvar


    double_upd_msg details = hang (text "Double update of meta tyvar")
                                2 (ppr tyvar $$ ppr details)


-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> ZonkM TcId
zonkId id = updateIdTypeAndMultM zonkTcType id

zonkCoVar :: CoVar -> ZonkM CoVar
zonkCoVar = zonkId

--------------------------------------------------------------------------------

{-
************************************************************************
*                                                                      *
     Zonking -- the main work-horses: zonkTcType, zonkTcTyVar
*                                                                      *
************************************************************************
-}

-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--      type variable and zonks the kind too
zonkTcType  :: TcType   -> ZonkM TcType
zonkTcTypes :: [TcType] -> ZonkM [TcType]
zonkCo      :: Coercion -> ZonkM Coercion
(zonkTcType, zonkTcTypes, zonkCo, _)
  = mapTyCo zonkTcTypeMapper

-- | A suitable TyCoMapper for zonking a type during type-checking,
-- before all metavars are filled in.
zonkTcTypeMapper :: TyCoMapper () ZonkM
zonkTcTypeMapper = TyCoMapper
  { tcm_tyvar = const zonkTcTyVar
  , tcm_covar = const (\cv -> mkCoVarCo <$> zonkTyCoVarKind cv)
  , tcm_hole  = hole
  , tcm_tycobinder = \_env tv _vis -> ((), ) <$> zonkTyCoVarKind tv
  , tcm_tycon      = zonkTcTyCon }
  where
    hole :: () -> CoercionHole -> ZonkM Coercion
    hole _ hole@(CoercionHole { ch_ref = ref, ch_co_var = cv })
      = do { contents <- liftIO $ readIORef ref
           ; case contents of
               Just co -> do { co' <- zonkCo co
                             ; checkCoercionHole cv co' }
               Nothing -> do { cv' <- zonkCoVar cv
                             ; return $ HoleCo (hole { ch_co_var = cv' }) } }

zonkTcTyCon :: TcTyCon -> ZonkM TcTyCon
-- Only called on TcTyCons
-- A non-poly TcTyCon may have unification
-- variables that need zonking, but poly ones cannot
zonkTcTyCon tc
 | isMonoTcTyCon tc = do { tck' <- zonkTcType (tyConKind tc)
                         ; return (setTcTyConKind tc tck') }
 | otherwise        = return tc

zonkTcTyVar :: TcTyVar -> ZonkM TcType
-- Simply look through all Flexis
zonkTcTyVar tv
  | isTcTyVar tv
  = case tcTyVarDetails tv of
      SkolemTv {}   -> zonk_kind_and_return
      RuntimeUnk {} -> zonk_kind_and_return
      MetaTv { mtv_ref = ref }
         -> do { cts <- liftIO $ readIORef ref
               ; case cts of
                    Flexi       -> zonk_kind_and_return
                    Indirect ty -> do { zty <- zonkTcType ty
                                      ; liftIO $ writeIORef ref (Indirect zty)
                                        -- See Note [Sharing in zonking]
                                      ; return zty } }

  | otherwise -- coercion variable
  = zonk_kind_and_return
  where
    zonk_kind_and_return = do { z_tv <- zonkTyCoVarKind tv
                              ; return (mkTyVarTy z_tv) }

-- Variant that assumes that any result of zonking is still a TyVar.
-- Should be used only on skolems and TyVarTvs
zonkTcTyVarsToTcTyVars :: HasDebugCallStack => [TcTyVar] -> ZonkM [TcTyVar]
zonkTcTyVarsToTcTyVars = mapM zonkTcTyVarToTcTyVar

zonkTcTyVarToTcTyVar :: HasDebugCallStack => TcTyVar -> ZonkM TcTyVar
zonkTcTyVarToTcTyVar tv
  = do { ty <- zonkTcTyVar tv
       ; let tv' = case getTyVar_maybe ty of
                     Just tv' -> tv'
                     Nothing  -> pprPanic "zonkTcTyVarToTcTyVar"
                                          (ppr tv $$ ppr ty)
       ; return tv' }

zonkInvisTVBinder :: VarBndr TcTyVar spec -> ZonkM (VarBndr TcTyVar spec)
zonkInvisTVBinder (Bndr tv spec) = do { tv' <- zonkTcTyVarToTcTyVar tv
                                      ; return (Bndr tv' spec) }

{- *********************************************************************
*                                                                      *
              Zonking types
*                                                                      *
********************************************************************* -}

zonkTcTypeAndFV :: TcType -> ZonkM DTyCoVarSet
-- Zonk a type and take its free variables
-- With kind polymorphism it can be essential to zonk *first*
-- so that we find the right set of free variables.  Eg
--    forall k1. forall (a:k2). a
-- where k2:=k1 is in the substitution.  We don't want
-- k2 to look free in this type!
zonkTcTypeAndFV ty
  = tyCoVarsOfTypeDSet <$> zonkTcType ty

zonkTyCoVar :: TyCoVar -> ZonkM TcType
-- Works on TyVars and TcTyVars
zonkTyCoVar tv | isTcTyVar tv = zonkTcTyVar tv
               | isTyVar   tv = mkTyVarTy <$> zonkTyCoVarKind tv
               | otherwise    = assertPpr (isCoVar tv) (ppr tv) $
                                mkCoercionTy . mkCoVarCo <$> zonkTyCoVarKind tv
   -- Hackily, when typechecking type and class decls
   -- we have TyVars in scope added (only) in
   -- GHC.Tc.Gen.HsType.bindTyClTyVars, but it seems
   -- painful to make them into TcTyVars there

zonkTyCoVarsAndFV :: TyCoVarSet -> ZonkM TyCoVarSet
zonkTyCoVarsAndFV tycovars
  = tyCoVarsOfTypes <$> mapM zonkTyCoVar (nonDetEltsUniqSet tycovars)
  -- It's OK to use nonDetEltsUniqSet here because we immediately forget about
  -- the ordering by turning it into a nondeterministic set and the order
  -- of zonking doesn't matter for determinism.

zonkDTyCoVarSetAndFV :: DTyCoVarSet -> ZonkM DTyCoVarSet
zonkDTyCoVarSetAndFV tycovars
  = mkDVarSet <$> (zonkTyCoVarsAndFVList $ dVarSetElems tycovars)

-- Takes a list of TyCoVars, zonks them and returns a
-- deterministically ordered list of their free variables.
zonkTyCoVarsAndFVList :: [TyCoVar] -> ZonkM [TyCoVar]
zonkTyCoVarsAndFVList tycovars
  = tyCoVarsOfTypesList <$> mapM zonkTyCoVar tycovars

zonkTcTyVars :: [TcTyVar] -> ZonkM [TcType]
zonkTcTyVars tyvars = mapM zonkTcTyVar tyvars

-----------------  Types
zonkTyCoVarKind :: TyCoVar -> ZonkM TyCoVar
zonkTyCoVarKind tv = do { kind' <- zonkTcType (tyVarKind tv)
                        ; return (setTyVarKind tv kind') }

{-
************************************************************************
*                                                                      *
        Coercion holes
*                                                                      *
************************************************************************
-}

-- | Check that a coercion is appropriate for filling a hole. (The hole
-- itself is needed only for printing.)
-- Always returns the checked coercion, but this return value is necessary
-- so that the input coercion is forced only when the output is forced.
checkCoercionHole :: CoVar -> Coercion -> ZonkM Coercion
checkCoercionHole cv co
  | debugIsOn
  = do { cv_ty <- zonkTcType (varType cv)
                  -- co is already zonked, but cv might not be
       ; return $
         assertPpr (ok cv_ty)
                   (text "Bad coercion hole" <+>
                    ppr cv <> colon <+> vcat [ ppr t1, ppr t2, ppr role
                                             , ppr cv_ty ])
         co }
  | otherwise
  = return co

  where
    (Pair t1 t2, role) = coercionKindRole co
    ok cv_ty | EqPred cv_rel cv_t1 cv_t2 <- classifyPredType cv_ty
             =  t1 `eqType` cv_t1
             && t2 `eqType` cv_t2
             && role == eqRelRole cv_rel
             | otherwise
             = False


{-
************************************************************************
*                                                                      *
              Zonking constraints
*                                                                      *
************************************************************************
-}

zonkImplication :: Implication -> ZonkM Implication
zonkImplication implic@(Implic { ic_skols  = skols
                               , ic_given  = given
                               , ic_wanted = wanted
                               , ic_info   = info })
  = do { skols'  <- mapM zonkTyCoVarKind skols  -- Need to zonk their kinds!
                                                -- as #7230 showed
       ; given'  <- mapM zonkEvVar given
       ; info'   <- zonkSkolemInfoAnon info
       ; wanted' <- zonkWCRec wanted
       ; return (implic { ic_skols  = skols'
                        , ic_given  = given'
                        , ic_wanted = wanted'
                        , ic_info   = info' }) }

zonkEvVar :: EvVar -> ZonkM EvVar
zonkEvVar var = updateIdTypeAndMultM zonkTcType var


zonkWC :: WantedConstraints -> ZonkM WantedConstraints
zonkWC wc = zonkWCRec wc

zonkWCRec :: WantedConstraints -> ZonkM WantedConstraints
zonkWCRec (WC { wc_simple = simple, wc_impl = implic, wc_errors = errs })
  = do { simple' <- zonkSimples simple
       ; implic' <- mapBagM zonkImplication implic
       ; errs'   <- mapBagM zonkDelayedError errs
       ; return (WC { wc_simple = simple', wc_impl = implic', wc_errors = errs' }) }

zonkSimples :: Cts -> ZonkM Cts
zonkSimples cts = do { cts' <- mapBagM zonkCt cts
                     ; traceZonk "zonkSimples done:" (ppr cts')
                     ; return cts' }

zonkDelayedError :: DelayedError -> ZonkM DelayedError
zonkDelayedError (DE_Hole hole)
  = DE_Hole <$> zonkHole hole
zonkDelayedError (DE_NotConcrete err)
  = DE_NotConcrete <$> zonkNotConcreteError err

zonkHole :: Hole -> ZonkM Hole
zonkHole hole@(Hole { hole_ty = ty })
  = do { ty' <- zonkTcType ty
       ; return (hole { hole_ty = ty' }) }

zonkNotConcreteError :: NotConcreteError -> ZonkM NotConcreteError
zonkNotConcreteError err@(NCE_FRR { nce_frr_origin = frr_orig })
  = do { frr_orig  <- zonkFRROrigin frr_orig
       ; return $ err { nce_frr_origin = frr_orig  } }

zonkFRROrigin :: FixedRuntimeRepOrigin -> ZonkM FixedRuntimeRepOrigin
zonkFRROrigin (FixedRuntimeRepOrigin ty orig)
  = do { ty' <- zonkTcType ty
       ; return $ FixedRuntimeRepOrigin ty' orig }

{- Note [zonkCt behaviour]
~~~~~~~~~~~~~~~~~~~~~~~~~~
zonkCt tries to maintain the canonical form of a Ct.  For example,
  - a CDictCan should stay a CDictCan;
  - a CIrredCan should stay a CIrredCan with its cc_reason flag intact

Why?, for example:
- For CDictCan, the @GHC.Tc.Solver.expandSuperClasses@ step, which runs after the
  simple wanted and plugin loop, looks for @CDictCan@s. If a plugin is in use,
  constraints are zonked before being passed to the plugin. This means if we
  don't preserve a canonical form, @expandSuperClasses@ fails to expand
  superclasses. This is what happened in #11525.

- For CIrredCan we want to see if a constraint is insoluble with insolubleWC

On the other hand, we change CEqCan to CNonCanonical, because of all of
CEqCan's invariants, which can break during zonking. (Example: a ~R alpha, where
we have alpha := N Int, where N is a newtype.) Besides, the constraint
will be canonicalised again, so there is little benefit in keeping the
CEqCan structure.

NB: Constraints are always rewritten etc by the canonicaliser in
@GHC.Tc.Solver.Canonical@ even if they come in as CDictCan. Only canonical constraints that
are actually in the inert set carry all the guarantees. So it is okay if zonkCt
creates e.g. a CDictCan where the cc_tyars are /not/ fully reduced.
-}

zonkCt :: Ct -> ZonkM Ct
-- See Note [zonkCt behaviour]
zonkCt ct@(CDictCan { cc_ev = ev, cc_tyargs = args })
  = do { ev'   <- zonkCtEvidence ev
       ; args' <- mapM zonkTcType args
       ; return $ ct { cc_ev = ev', cc_tyargs = args' } }

zonkCt (CEqCan (EqCt { eq_ev = ev }))
  = mkNonCanonical <$> zonkCtEvidence ev

zonkCt ct@(CIrredCan { cc_ev = ev }) -- Preserve the cc_reason flag
  = do { ev' <- zonkCtEvidence ev
       ; return (ct { cc_ev = ev' }) }

zonkCt ct
  = do { fl' <- zonkCtEvidence (ctEvidence ct)
       ; return (mkNonCanonical fl') }

zonkCtEvidence :: CtEvidence -> ZonkM CtEvidence
zonkCtEvidence ctev
  = do { let pred = ctev_pred ctev
       ; pred' <- zonkTcType pred
       ; return (setCtEvPredType ctev pred') }

zonkSkolemInfo :: SkolemInfo -> ZonkM SkolemInfo
zonkSkolemInfo (SkolemInfo u sk) = SkolemInfo u <$> zonkSkolemInfoAnon sk

zonkSkolemInfoAnon :: SkolemInfoAnon -> ZonkM SkolemInfoAnon
zonkSkolemInfoAnon (SigSkol cx ty tv_prs) = do { ty' <- zonkTcType ty
                                               ; return (SigSkol cx ty' tv_prs) }
zonkSkolemInfoAnon (InferSkol ntys) = do { ntys' <- mapM do_one ntys
                                     ; return (InferSkol ntys') }
  where
    do_one (n, ty) = do { ty' <- zonkTcType ty; return (n, ty') }
zonkSkolemInfoAnon skol_info = return skol_info

{- Note [Sharing in zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   alpha :-> beta :-> gamma :-> ty
where the ":->" means that the unification variable has been
filled in with Indirect. Then when zonking alpha, it'd be nice
to short-circuit beta too, so we end up with
   alpha :-> zty
   beta  :-> zty
   gamma :-> zty
where zty is the zonked version of ty.  That way, if we come across
beta later, we'll have less work to do.  (And indeed the same for
alpha.)

This is easily achieved: just overwrite (Indirect ty) with (Indirect
zty).  Non-systematic perf comparisons suggest that this is a modest
win.

But c.f Note [Sharing when zonking to Type] in GHC.Tc.Utils.Zonk.

%************************************************************************
%*                                                                      *
                 Tidying
*                                                                      *
************************************************************************
-}

zonkTidyTcType :: TidyEnv -> TcType -> ZonkM (TidyEnv, TcType)
zonkTidyTcType env ty = do { ty' <- zonkTcType ty
                           ; return (tidyOpenType env ty') }

zonkTidyTcTypes :: TidyEnv -> [TcType] -> ZonkM (TidyEnv, [TcType])
zonkTidyTcTypes = zonkTidyTcTypes' []
  where zonkTidyTcTypes' zs env [] = return (env, reverse zs)
        zonkTidyTcTypes' zs env (ty:tys)
          = do { (env', ty') <- zonkTidyTcType env ty
               ; zonkTidyTcTypes' (ty':zs) env' tys }

zonkTidyOrigin :: TidyEnv -> CtOrigin -> ZonkM (TidyEnv, CtOrigin)
zonkTidyOrigin env (GivenOrigin skol_info)
  = do { skol_info1 <- zonkSkolemInfoAnon skol_info
       ; let skol_info2 = tidySkolemInfoAnon env skol_info1
       ; return (env, GivenOrigin skol_info2) }
zonkTidyOrigin env (GivenSCOrigin skol_info sc_depth blocked)
  = do { skol_info1 <- zonkSkolemInfoAnon skol_info
       ; let skol_info2 = tidySkolemInfoAnon env skol_info1
       ; return (env, GivenSCOrigin skol_info2 sc_depth blocked) }
zonkTidyOrigin env orig@(TypeEqOrigin { uo_actual   = act
                                      , uo_expected = exp })
  = do { (env1, act') <- zonkTidyTcType env  act
       ; (env2, exp') <- zonkTidyTcType env1 exp
       ; return ( env2, orig { uo_actual   = act'
                             , uo_expected = exp' }) }
zonkTidyOrigin env (KindEqOrigin ty1 ty2 orig t_or_k)
  = do { (env1, ty1')  <- zonkTidyTcType env  ty1
       ; (env2, ty2')  <- zonkTidyTcType env1 ty2
       ; (env3, orig') <- zonkTidyOrigin env2 orig
       ; return (env3, KindEqOrigin ty1' ty2' orig' t_or_k) }
zonkTidyOrigin env (FunDepOrigin1 p1 o1 l1 p2 o2 l2)
  = do { (env1, p1') <- zonkTidyTcType env  p1
       ; (env2, o1') <- zonkTidyOrigin env1 o1
       ; (env3, p2') <- zonkTidyTcType env2 p2
       ; (env4, o2') <- zonkTidyOrigin env3 o2
       ; return (env4, FunDepOrigin1 p1' o1' l1 p2' o2' l2) }
zonkTidyOrigin env (FunDepOrigin2 p1 o1 p2 l2)
  = do { (env1, p1') <- zonkTidyTcType env  p1
       ; (env2, p2') <- zonkTidyTcType env1 p2
       ; (env3, o1') <- zonkTidyOrigin env2 o1
       ; return (env3, FunDepOrigin2 p1' o1' p2' l2) }
zonkTidyOrigin env (InjTFOrigin1 pred1 orig1 loc1 pred2 orig2 loc2)
  = do { (env1, pred1') <- zonkTidyTcType env  pred1
       ; (env2, orig1') <- zonkTidyOrigin env1 orig1
       ; (env3, pred2') <- zonkTidyTcType env2 pred2
       ; (env4, orig2') <- zonkTidyOrigin env3 orig2
       ; return (env4, InjTFOrigin1 pred1' orig1' loc1 pred2' orig2' loc2) }
zonkTidyOrigin env (CycleBreakerOrigin orig)
  = do { (env1, orig') <- zonkTidyOrigin env orig
       ; return (env1, CycleBreakerOrigin orig') }
zonkTidyOrigin env (InstProvidedOrigin mod cls_inst)
  = do { (env1, is_tys') <- mapAccumLM zonkTidyTcType env (is_tys cls_inst)
       ; return (env1, InstProvidedOrigin mod (cls_inst {is_tys = is_tys'})) }
zonkTidyOrigin env (WantedSuperclassOrigin pty orig)
  = do { (env1, pty')  <- zonkTidyTcType env pty
       ; (env2, orig') <- zonkTidyOrigin env1 orig
       ; return (env2, WantedSuperclassOrigin pty' orig') }
zonkTidyOrigin env orig = return (env, orig)

zonkTidyOrigins :: TidyEnv -> [CtOrigin] -> ZonkM (TidyEnv, [CtOrigin])
zonkTidyOrigins = mapAccumLM zonkTidyOrigin

zonkTidyFRRInfos :: TidyEnv
                 -> [FixedRuntimeRepErrorInfo]
                 -> ZonkM (TidyEnv, [FixedRuntimeRepErrorInfo])
zonkTidyFRRInfos = go []
  where
    go zs env [] = return (env, reverse zs)
    go zs env (FRR_Info { frr_info_origin = FixedRuntimeRepOrigin ty orig
                        , frr_info_not_concrete = mb_not_conc } : tys)
      = do { (env, ty) <- zonkTidyTcType env ty
           ; (env, mb_not_conc) <- go_mb_not_conc env mb_not_conc
           ; let info = FRR_Info { frr_info_origin = FixedRuntimeRepOrigin ty orig
                                 , frr_info_not_concrete = mb_not_conc }
           ; go (info:zs) env tys }

    go_mb_not_conc env Nothing = return (env, Nothing)
    go_mb_not_conc env (Just (tv, ty))
      = do { (env, tv) <- return $ tidyOpenTyCoVar env tv
           ; (env, ty) <- zonkTidyTcType env ty
           ; return (env, Just (tv, ty)) }

----------------
tidyCt :: TidyEnv -> Ct -> Ct
-- Used only in error reporting
tidyCt env = modifyCtEvidence (tidyCtEvidence env)

tidyCtEvidence :: TidyEnv -> CtEvidence -> CtEvidence
     -- NB: we do not tidy the ctev_evar field because we don't
     --     show it in error messages
tidyCtEvidence env ctev = ctev { ctev_pred = tidyType env ty }
  where
    ty  = ctev_pred ctev

tidyHole :: TidyEnv -> Hole -> Hole
tidyHole env h@(Hole { hole_ty = ty }) = h { hole_ty = tidyType env ty }

tidyDelayedError :: TidyEnv -> DelayedError -> DelayedError
tidyDelayedError env (DE_Hole hole)
  = DE_Hole $ tidyHole env hole
tidyDelayedError env (DE_NotConcrete err)
  = DE_NotConcrete $ tidyConcreteError env err

tidyConcreteError :: TidyEnv -> NotConcreteError -> NotConcreteError
tidyConcreteError env err@(NCE_FRR { nce_frr_origin = frr_orig })
  = err { nce_frr_origin = tidyFRROrigin env frr_orig }

tidyFRROrigin :: TidyEnv -> FixedRuntimeRepOrigin -> FixedRuntimeRepOrigin
tidyFRROrigin env (FixedRuntimeRepOrigin ty orig)
  = FixedRuntimeRepOrigin (tidyType env ty) orig

----------------
tidyEvVar :: TidyEnv -> EvVar -> EvVar
tidyEvVar env var = updateIdTypeAndMult (tidyType env) var



