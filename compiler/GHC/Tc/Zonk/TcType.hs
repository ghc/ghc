{-# LANGUAGE DuplicateRecordFields #-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998
-}

-- | Zonking types within the typechecker.
--
-- Distinct from the final zonking pass in "GHC.Tc.Zonk.Type";
-- see Note [Module structure for zonking] in GHC.Tc.Zonk.Type.
module GHC.Tc.Zonk.TcType
  ( -- * Zonking (within the typechecker)

    -- ** The 'ZonkM' monad and 'ZonkGblEnv'
    module GHC.Tc.Zonk.Monad

    -- ** Zonking types
  , zonkTcType, zonkTcTypes, zonkScaledTcType
  , zonkTcTyVar, zonkTcTyVars
  , zonkTcTyVarToTcTyVar, zonkTcTyVarsToTcTyVars
  , zonkInvisTVBinder
  , zonkCo

    -- ** Zonking 'TyCon's
  , zonkTcTyCon

    -- *** FreeVars
  , zonkTcTypeAndFV, zonkTyCoVarsAndFV, zonkTyCoVarsAndFVList
  , zonkDTyCoVarSetAndFV

    -- ** Zonking 'CoVar's and 'Id's
  , zonkId, zonkCoVar, zonkTyCoVar, zonkTyCoVarKind, zonkTyCoVarBndrKind

    -- ** Zonking skolem info
  , zonkSkolemInfo, zonkSkolemInfoAnon

    -- ** Zonking constraints
  , zonkCt, zonkWC, zonkSimples, zonkImplication

    -- * Rewriter sets
  , zonkRewriterSet, zonkCtRewriterSet, zonkCtEvRewriterSet

    -- * Coercion holes
  , isFilledCoercionHole, unpackCoercionHole, unpackCoercionHole_maybe


    -- * Tidying
  , tcInitTidyEnv, tcInitOpenTidyEnv
  , tidyCt, tidyEvVar, tidyDelayedError

    -- ** Zonk & tidy
  , zonkTidyTcType, zonkTidyTcTypes
  , zonkTidyOrigin, zonkTidyOrigins
  , zonkTidyFRRInfos

    -- * Writing to metavariables
  , writeMetaTyVar, writeMetaTyVarRef

    -- * Handling coercion holes
  , checkCoercionHole

  )
  where

import GHC.Prelude

import GHC.Types.Name
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Types.Unique.Set

import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Types.TcRef
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.BasicTypes
import GHC.Tc.Utils.TcType
import GHC.Tc.Zonk.Monad

import GHC.Core.InstEnv (ClsInst(is_tys))
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Tidy
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.Predicate

import GHC.Utils.Constants
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Misc
import GHC.Utils.Monad ( mapAccumLM )
import GHC.Utils.Panic

import GHC.Data.Bag
import GHC.Data.Pair

import Data.Semigroup
import Data.Maybe

{- *********************************************************************
*                                                                      *
                    Writing to metavariables
*                                                                      *
************************************************************************
-}

-- | Write into a currently-empty MetaTyVar.
--
-- Works with both type and kind variables.
writeMetaTyVar :: HasDebugCallStack
               => TcTyVar -- ^ the type varfiable to write to
               -> TcType  -- ^ the type to write into the mutable reference
               -> ZonkM ()
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
{-# INLINE writeMetaTyVar #-} -- See NOTE [Inlining writeMetaTyVar]

-- | Write into the 'MetaDetails' mutable references of a 'MetaTv'.
writeMetaTyVarRef :: HasDebugCallStack
                  => TcTyVar -- ^ for debug assertions only;
                  -> TcRef MetaDetails -- ^ ref cell must be for the same tyvar
                  -> TcType -- ^ the type to write to the mutable reference
                  -> ZonkM ()
writeMetaTyVarRef tyvar ref ty
  | not debugIsOn
  = do { traceZonk "writeMetaTyVar" (ppr tyvar <+> dcolon <+> ppr (tyVarKind tyvar)
                                     <+> text ":=" <+> ppr ty)
       ; writeTcRef ref (Indirect ty) }

  -- Everything from here on only happens if DEBUG is on
  -- Need to zonk 'ty' because we may only recently have promoted
  -- its free meta-tyvars (see GHC.Tc.Utils.Unify.checkPromoteFreeVars)
  | otherwise
  = do { meta_details <- readTcRef ref;
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
       ; writeTcRef ref (Indirect ty) }
  where
    tv_kind = tyVarKind tyvar

    tv_lvl = tcTyVarLevel tyvar


    double_upd_msg details = hang (text "Double update of meta tyvar")
                                2 (ppr tyvar $$ ppr details)
{-# INLINE writeMetaTyVarRef #-} -- See NOTE [Inlining writeMetaTyVar]

{- NOTE [Inlining writeMetaTyVar]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
writeMetaTyVar is defined in the ZonkM monad, but it is often used within
TcM with the following idiom:

  liftZonkM $ writeMetaTyVar tv ty

Using liftZonkM within TcM generally means extracting out a ZonkGblEnv
from the TcM monad to pass to the inner ZonkM computation (see the definition
of liftZonkM). This can cause writeMetaTyVar to allocate a ZonkGblEnv, which we
would much rather avoid!
Instead, we should directly pass the bits of the ZonkGblEnv that writeMetaTyVar
needs (the Logger and NamePprCtxt, which are needed for the traceZonk call
in writeMetaTyVar). This is achieved by inlining writeMetaTyVar and writeMetaTyVarRef.
These functions just wrap writeTcRef, with some extra tracing
(and some assertions if running in debug mode), so it's fine to inline them.

See for example test T5631, which regresses without this change.
-}

{-
************************************************************************
*                                                                      *
     Zonking -- the main work-horses: zonkTcType, zonkTcTyVar
*                                                                      *
************************************************************************
-}

zonkScaledTcType :: Scaled TcType -> ZonkM (Scaled TcType)
zonkScaledTcType (Scaled m ty)
  = Scaled <$> zonkTcType m <*> zonkTcType ty

-- For unbound, mutable tyvars, zonkType uses the function given to it
-- For tyvars bound at a for-all, zonkType zonks them to an immutable
--      type variable and zonks the kind too
zonkTcType  :: TcType   -> ZonkM TcType
zonkTcTypes :: [TcType] -> ZonkM [TcType]
zonkCo      :: Coercion -> ZonkM Coercion
(zonkTcType, zonkTcTypes, zonkCo, _)
  = mapTyCo zonkTcTypeMapper
  where
    -- A suitable TyCoMapper for zonking a type during type-checking,
    -- before all metavars are filled in.
    zonkTcTypeMapper :: TyCoMapper () ZonkM
    zonkTcTypeMapper = TyCoMapper
      { tcm_tyvar = const zonkTcTyVar
      , tcm_covar = const (\cv -> mkCoVarCo <$> zonkTyCoVarKind cv)
      , tcm_hole  = hole
      , tcm_tycobinder = \ _env tcv _vis k -> zonkTyCoVarKind tcv >>= k ()
      , tcm_tycon      = zonkTcTyCon }
      where
        hole :: () -> CoercionHole -> ZonkM Coercion
        hole _ hole@(CoercionHole { ch_ref = ref, ch_co_var = cv })
          = do { contents <- readTcRef ref
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
         -> do { cts <- readTcRef ref
               ; case cts of
                    Flexi       -> zonk_kind_and_return
                    Indirect ty -> do { zty <- zonkTcType ty
                                      ; writeTcRef ref (Indirect zty)
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

zonkTyCoVarBndrKind :: VarBndr TyCoVar flag -> ZonkM (VarBndr TyCoVar flag)
zonkTyCoVarBndrKind (Bndr tv flag) =
  do { tv' <- zonkTyCoVarKind tv
     ; return (Bndr tv' flag) }

-- | zonkId is used *during* typechecking just to zonk the 'Id''s type
zonkId :: TcId -> ZonkM TcId
zonkId id = updateIdTypeAndMultM zonkTcType id

zonkCoVar :: CoVar -> ZonkM CoVar
zonkCoVar = zonkId

{-
************************************************************************
*                                                                      *
        Coercion holes
*                                                                      *
************************************************************************
-}

-- | Debugging-only!  Check that a coercion is appropriate for filling a
--   hole. (The hole itself is needed only for printing.)
--
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
                    ppr cv Outputable.<> colon
                    <+> vcat [ ppr t1, ppr t2, ppr role, ppr cv_ty ])
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
zonkDelayedError (DE_Multiplicity mult_co loc)
  = DE_Multiplicity <$> zonkCo mult_co <*> pure loc

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
GHC.Tc.Solver.Solve.solveCt even if they come in as CDictCan. Only canonical
constraints that are actually in the inert set carry all the guarantees. So it
is okay if zonkCt creates e.g. a CDictCan where the cc_tyars are /not/ fully
reduced.
-}

zonkCt :: Ct -> ZonkM Ct
-- See Note [zonkCt behaviour]
zonkCt (CDictCan dict@(DictCt { di_ev = ev, di_tys = args }))
  = do { ev'   <- zonkCtEvidence ev
       ; args' <- mapM zonkTcType args
       ; return (CDictCan (dict { di_ev = ev', di_tys = args' })) }

zonkCt (CEqCan (EqCt { eq_ev = ev }))
  = mkNonCanonical <$> zonkCtEvidence ev

zonkCt (CIrredCan ir@(IrredCt { ir_ev = ev })) -- Preserve the ir_reason flag
  = do { ev' <- zonkCtEvidence ev
       ; return (CIrredCan (ir { ir_ev = ev' })) }

zonkCt ct
  = do { fl' <- zonkCtEvidence (ctEvidence ct)
       ; return (mkNonCanonical fl') }

zonkCtEvidence :: CtEvidence -> ZonkM CtEvidence
-- Zonks the ctev_pred and the ctev_rewriters; but not ctev_evar
-- For ctev_rewriters, see (WRW2) in Note [Wanteds rewrite Wanteds]
zonkCtEvidence (CtGiven (GivenCt { ctev_pred = pred, ctev_evar = var, ctev_loc = loc }))
  = do { pred' <- zonkTcType pred
       ; return (CtGiven (GivenCt { ctev_pred = pred', ctev_evar = var, ctev_loc = loc })) }
zonkCtEvidence (CtWanted wanted@(WantedCt { ctev_pred = pred, ctev_rewriters = rws }))
  = do { pred' <- zonkTcType pred
       ; rws'  <- zonkRewriterSet rws
       ; return (CtWanted (wanted { ctev_pred = pred', ctev_rewriters = rws' })) }

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

But c.f Note [Sharing when zonking to Type] in GHC.Tc.Zonk.Type.

%************************************************************************
%*                                                                      *
                 Zonking rewriter sets
*                                                                      *
************************************************************************
-}

zonkCtRewriterSet :: Ct -> ZonkM Ct
zonkCtRewriterSet ct
  | isGivenCt ct
  = return ct
  | otherwise
  = case ct of
      CEqCan eq@(EqCt { eq_ev = ev })       -> do { ev' <- zonkCtEvRewriterSet ev
                                                  ; return (CEqCan (eq { eq_ev = ev' })) }
      CIrredCan ir@(IrredCt { ir_ev = ev }) -> do { ev' <- zonkCtEvRewriterSet ev
                                                  ; return (CIrredCan (ir { ir_ev = ev' })) }
      CDictCan di@(DictCt { di_ev = ev })   -> do { ev' <- zonkCtEvRewriterSet ev
                                                  ; return (CDictCan (di { di_ev = ev' })) }
      CQuantCan {}     -> return ct
      CNonCanonical ev -> do { ev' <- zonkCtEvRewriterSet ev
                             ; return (CNonCanonical ev') }

zonkCtEvRewriterSet :: CtEvidence -> ZonkM CtEvidence
zonkCtEvRewriterSet ev@(CtGiven {})
  = return ev
zonkCtEvRewriterSet ev@(CtWanted wtd)
  = do { rewriters' <- zonkRewriterSet (ctEvRewriters ev)
       ; return (CtWanted $ setWantedCtEvRewriters wtd rewriters') }

-- | Zonk a rewriter set; if a coercion hole in the set has been filled,
-- find all the free un-filled coercion holes in the coercion that fills it
zonkRewriterSet :: RewriterSet -> ZonkM RewriterSet
zonkRewriterSet (RewriterSet set)
  = nonDetStrictFoldUniqSet go (return emptyRewriterSet) set
     -- This does not introduce non-determinism, because the only
     -- monadic action is to read, and the combining function is
     -- commutative
  where
    go :: CoercionHole -> ZonkM RewriterSet -> ZonkM RewriterSet
    go hole m_acc = unionRewriterSet <$> check_hole hole <*> m_acc

    check_hole :: CoercionHole -> ZonkM RewriterSet
    check_hole hole
      = do { m_co <- unpackCoercionHole_maybe hole
           ; case m_co of
               Nothing -> return (unitRewriterSet hole)  -- Not filled
               Just co -> unUCHM (check_co co) }         -- Filled: look inside

    check_ty :: Type -> UnfilledCoercionHoleMonoid
    check_co :: Coercion -> UnfilledCoercionHoleMonoid
    (check_ty, _, check_co, _) = foldTyCo folder ()

    folder :: TyCoFolder () UnfilledCoercionHoleMonoid
    folder = TyCoFolder { tcf_view  = noView
                        , tcf_tyvar = \ _ tv -> check_ty (tyVarKind tv)
                        , tcf_covar = \ _ cv -> check_ty (varType cv)
                        , tcf_hole  = \ _ -> UCHM . check_hole
                        , tcf_tycobinder = \ _ _ _ -> () }

newtype UnfilledCoercionHoleMonoid = UCHM { unUCHM :: ZonkM RewriterSet }

instance Semigroup UnfilledCoercionHoleMonoid where
  UCHM l <> UCHM r = UCHM (unionRewriterSet <$> l <*> r)

instance Monoid UnfilledCoercionHoleMonoid where
  mempty = UCHM (return emptyRewriterSet)


{-
************************************************************************
*                                                                      *
             Checking for coercion holes
*                                                                      *
************************************************************************
-}

-- | Is a coercion hole filled in?
isFilledCoercionHole :: CoercionHole -> ZonkM Bool
isFilledCoercionHole (CoercionHole { ch_ref = ref })
  = isJust <$> readTcRef ref

-- | Retrieve the contents of a coercion hole. Panics if the hole
-- is unfilled
unpackCoercionHole :: CoercionHole -> ZonkM Coercion
unpackCoercionHole hole
  = do { contents <- unpackCoercionHole_maybe hole
       ; case contents of
           Just co -> return co
           Nothing -> pprPanic "Unfilled coercion hole" (ppr hole) }

-- | Retrieve the contents of a coercion hole, if it is filled
unpackCoercionHole_maybe :: CoercionHole -> ZonkM (Maybe Coercion)
unpackCoercionHole_maybe (CoercionHole { ch_ref = ref }) = readTcRef ref


{-
%************************************************************************
%*                                                                      *
                 Tidying
*                                                                      *
************************************************************************
-}

tcInitTidyEnv :: ZonkM TidyEnv
-- We initialise the "tidy-env", used for tidying types before printing,
-- by building a reverse map from the in-scope type variables to the
-- OccName that the programmer originally used for them
tcInitTidyEnv
  = do  { ZonkGblEnv { zge_binder_stack = bndrs } <- getZonkGblEnv
        ; go emptyTidyEnv bndrs }
  where
    go (env, subst) []
      = return (env, subst)
    go (env, subst) (b : bs)
      | TcTvBndr name tyvar <- b
       = do { let (env', occ') = tidyOccName env (nameOccName name)
                  name'  = tidyNameOcc name occ'
                  tyvar1 = setTyVarName tyvar name'
            ; tyvar2 <- zonkTcTyVarToTcTyVar tyvar1
              -- Be sure to zonk here!  Tidying applies to zonked
              -- types, so if we don't zonk we may create an
              -- ill-kinded type (#14175)
            ; go (env', extendVarEnv subst tyvar tyvar2) bs }
      | otherwise
      = go (env, subst) bs

-- | Get a 'TidyEnv' that includes mappings for all vars free in the given
-- type. Useful when tidying open types.
tcInitOpenTidyEnv :: [TyCoVar] -> ZonkM TidyEnv
tcInitOpenTidyEnv tvs
  = do { env1 <- tcInitTidyEnv
       ; return (tidyFreeTyCoVars env1 tvs) }

zonkTidyTcType :: TidyEnv -> TcType -> ZonkM (TidyEnv, TcType)
zonkTidyTcType env ty = do { ty' <- zonkTcType ty
                           ; return (tidyOpenTypeX env ty') }

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
                        , frr_info_not_concrete = mb_not_conc
                        , frr_info_other_origin = mb_other_orig
                        } : tys)
      = do { (env, ty) <- zonkTidyTcType env ty
           ; (env, mb_not_conc) <- go_mb_not_conc env mb_not_conc
           ; (env, mb_other_orig) <-
               case mb_other_orig of
                 Nothing -> return (env, Nothing)
                 Just o  -> do { (env', o') <- zonkTidyOrigin env o; return (env', Just o') }
           ; let info = FRR_Info { frr_info_origin = FixedRuntimeRepOrigin ty orig
                                 , frr_info_not_concrete = mb_not_conc
                                 , frr_info_other_origin = mb_other_orig }
           ; go (info:zs) env tys }

    go_mb_not_conc env Nothing = return (env, Nothing)
    go_mb_not_conc env (Just (tv, ty))
      = do { (env, tv) <- return $ tidyFreeTyCoVarX env tv
           ; (env, ty) <- zonkTidyTcType env ty
           ; return (env, Just (tv, ty)) }

----------------
tidyCt :: TidyEnv -> Ct -> Ct
-- Used only in error reporting
tidyCt env = updCtEvidence (tidyCtEvidence env)

tidyCtEvidence :: TidyEnv -> CtEvidence -> CtEvidence
     -- NB: we do not tidy the ctev_evar field because we don't
     --     show it in error messages
tidyCtEvidence env ctev
  = setCtEvPredType ctev (tidyOpenType env (ctEvPred ctev))
  -- tidyOpenType: for (beta ~ (forall a. a->a), don't gratuitously
  -- rename the 'forall a' just because of an 'a' in scope somewhere
  -- else entirely.

tidyHole :: TidyEnv -> Hole -> Hole
tidyHole env h@(Hole { hole_ty = ty })
  = h { hole_ty = tidyOpenType env ty }
  -- tidyOpenType: for, say, (b -> (forall a. a->a)), don't gratuitously
  -- rename the 'forall a' just because of an 'a' in scope somewhere
  -- else entirely.

tidyDelayedError :: TidyEnv -> DelayedError -> DelayedError
tidyDelayedError env (DE_Hole hole)       = DE_Hole        $ tidyHole env hole
tidyDelayedError env (DE_NotConcrete err) = DE_NotConcrete $ tidyConcreteError env err
tidyDelayedError env (DE_Multiplicity mult_co loc)
  = DE_Multiplicity (tidyCo env mult_co) loc

tidyConcreteError :: TidyEnv -> NotConcreteError -> NotConcreteError
tidyConcreteError env err@(NCE_FRR { nce_frr_origin = frr_orig })
  = err { nce_frr_origin = tidyFRROrigin env frr_orig }

tidyFRROrigin :: TidyEnv -> FixedRuntimeRepOrigin -> FixedRuntimeRepOrigin
tidyFRROrigin env (FixedRuntimeRepOrigin ty orig)
  = FixedRuntimeRepOrigin (tidyType env ty) orig
  -- No need for tidyOpenType because all the free tyvars are already tidied

----------------
tidyEvVar :: TidyEnv -> EvVar -> EvVar
tidyEvVar env var = updateIdTypeAndMult (tidyType env) var
  -- No need for tidyOpenType because all the free tyvars are already tidied
