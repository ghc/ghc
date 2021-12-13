{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-2002

-}


{-# LANGUAGE TypeFamilies #-}

module GHC.Tc.Gen.Sig(
       TcSigInfo(..),
       TcIdSigInfo(..), TcIdSigInst,
       TcPatSynInfo(..),
       TcSigFun,

       isPartialSig, hasCompleteSig, tcIdSigName, tcSigInfoName,
       completeSigPolyId_maybe, isCompleteHsSig,
       lhsSigWcTypeContextSpan, lhsSigTypeContextSpan,

       tcTySigs, tcUserTypeSig, completeSigFromId,
       tcInstSig,

       TcPragEnv, emptyPragEnv, lookupPragEnv, extendPragEnv,
       mkPragEnv, tcSpecPrags, tcSpecWrapper, tcImpPrags, addInlinePrags
   ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Backend

import GHC.Hs


import GHC.Tc.Errors.Types ( FixedRuntimeRepProvenance(..), TcRnMessage(..) )
import GHC.Tc.Gen.HsType
import GHC.Tc.Types
import GHC.Tc.Solver( pushLevelAndSolveEqualitiesX, reportUnsolvedEqualities )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcMType ( checkTypeHasFixedRuntimeRep )
import GHC.Tc.Utils.Zonk
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType
import GHC.Tc.Validity ( checkValidType )
import GHC.Tc.Utils.Unify( tcSkolemise, unifyType )
import GHC.Tc.Utils.Instantiate( topInstantiate, tcInstTypeBndrs )
import GHC.Tc.Utils.Env( tcLookupId )
import GHC.Tc.Types.Evidence( HsWrapper, (<.>) )

import GHC.Core( hasSomeUnfolding )
import GHC.Core.Type ( mkTyVarBinders )
import GHC.Core.Multiplicity

import GHC.Types.Error
import GHC.Types.Var ( TyVar, Specificity(..), tyVarKind, binderVars )
import GHC.Types.Id  ( Id, idName, idType, setInlinePragma
                     , mkLocalId, realIdUnfolding )
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.SrcLoc

import GHC.Builtin.Names( mkUnboundName )
import GHC.Unit.Module( getModule )

import GHC.Utils.Misc as Utils ( singleton )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Trace

import GHC.Data.Maybe( orElse )

import Data.Maybe( mapMaybe )
import Control.Monad( unless )


{- -------------------------------------------------------------
          Note [Overview of type signatures]
----------------------------------------------------------------
Type signatures, including partial signatures, are jolly tricky,
especially on value bindings.  Here's an overview.

    f :: forall a. [a] -> [a]
    g :: forall b. _ -> b

    f = ...g...
    g = ...f...

* HsSyn: a signature in a binding starts off as a TypeSig, in
  type HsBinds.Sig

* When starting a mutually recursive group, like f/g above, we
  call tcTySig on each signature in the group.

* tcTySig: Sig -> TcIdSigInfo
  - For a /complete/ signature, like 'f' above, tcTySig kind-checks
    the HsType, producing a Type, and wraps it in a CompleteSig, and
    extend the type environment with this polymorphic 'f'.

  - For a /partial/signature, like 'g' above, tcTySig does nothing
    Instead it just wraps the pieces in a PartialSig, to be handled
    later.

* tcInstSig: TcIdSigInfo -> TcIdSigInst
  In tcMonoBinds, when looking at an individual binding, we use
  tcInstSig to instantiate the signature forall's in the signature,
  and attribute that instantiated (monomorphic) type to the
  binder.  You can see this in GHC.Tc.Gen.Bind.tcLhsId.

  The instantiation does the obvious thing for complete signatures,
  but for /partial/ signatures it starts from the HsSyn, so it
  has to kind-check it etc: tcHsPartialSigType.  It's convenient
  to do this at the same time as instantiation, because we can
  make the wildcards into unification variables right away, raather
  than somehow quantifying over them.  And the "TcLevel" of those
  unification variables is correct because we are in tcMonoBinds.


Note [Binding scoped type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type variables *brought into lexical scope* by a type signature
may be a subset of the *quantified type variables* of the signatures,
for two reasons:

* With kind polymorphism a signature like
    f :: forall f a. f a -> f a
  may actually give rise to
    f :: forall k. forall (f::k -> *) (a:k). f a -> f a
  So the sig_tvs will be [k,f,a], but only f,a are scoped.
  NB: the scoped ones are not necessarily the *initial* ones!

* Even aside from kind polymorphism, there may be more instantiated
  type variables than lexically-scoped ones.  For example:
        type T a = forall b. b -> (a,b)
        f :: forall c. T c
  Here, the signature for f will have one scoped type variable, c,
  but two instantiated type variables, c' and b'.

However, all of this only applies to the renamer.  The typechecker
just puts all of them into the type environment; any lexical-scope
errors were dealt with by the renamer.

-}


{- *********************************************************************
*                                                                      *
             Utility functions for TcSigInfo
*                                                                      *
********************************************************************* -}

tcIdSigName :: TcIdSigInfo -> Name
tcIdSigName (CompleteSig { sig_bndr = id }) = idName id
tcIdSigName (PartialSig { psig_name = n })  = n

tcSigInfoName :: TcSigInfo -> Name
tcSigInfoName (TcIdSig     idsi) = tcIdSigName idsi
tcSigInfoName (TcPatSynSig tpsi) = patsig_name tpsi

completeSigPolyId_maybe :: TcSigInfo -> Maybe TcId
completeSigPolyId_maybe sig
  | TcIdSig sig_info <- sig
  , CompleteSig { sig_bndr = id } <- sig_info = Just id
  | otherwise                                 = Nothing


{- *********************************************************************
*                                                                      *
               Typechecking user signatures
*                                                                      *
********************************************************************* -}

tcTySigs :: [LSig GhcRn] -> TcM ([TcId], TcSigFun)
tcTySigs hs_sigs
  = checkNoErrs $
    do { -- Fail if any of the signatures is duff
         -- Hence mapAndReportM
         -- See Note [Fail eagerly on bad signatures]
         ty_sigs_s <- mapAndReportM tcTySig hs_sigs

       ; let ty_sigs = concat ty_sigs_s
             poly_ids = mapMaybe completeSigPolyId_maybe ty_sigs
                        -- The returned [TcId] are the ones for which we have
                        -- a complete type signature.
                        -- See Note [Complete and partial type signatures]
             env = mkNameEnv [(tcSigInfoName sig, sig) | sig <- ty_sigs]

       ; return (poly_ids, lookupNameEnv env) }

tcTySig :: LSig GhcRn -> TcM [TcSigInfo]
tcTySig (L _ (IdSig _ id))
  = do { let ctxt = FunSigCtxt (idName id) NoRRC
                    -- NoRRC: do not report redundant constraints
                    -- The user has no control over the signature!
             sig = completeSigFromId ctxt id
       ; return [TcIdSig sig] }

tcTySig (L loc (TypeSig _ names sig_ty))
  = setSrcSpanA loc $
    do { sigs <- sequence [ tcUserTypeSig (locA loc) sig_ty (Just name)
                          | L _ name <- names ]
       ; return (map TcIdSig sigs) }

tcTySig (L loc (PatSynSig _ names sig_ty))
  = setSrcSpanA loc $
    do { tpsigs <- sequence [ tcPatSynSig name sig_ty
                            | L _ name <- names ]
       ; return (map TcPatSynSig tpsigs) }

tcTySig _ = return []


tcUserTypeSig :: SrcSpan -> LHsSigWcType GhcRn -> Maybe Name
              -> TcM TcIdSigInfo
-- A function or expression type signature
-- Returns a fully quantified type signature; even the wildcards
-- are quantified with ordinary skolems that should be instantiated
--
-- The SrcSpan is what to declare as the binding site of the
-- any skolems in the signature. For function signatures we
-- use the whole `f :: ty' signature; for expression signatures
-- just the type part.
--
-- Just n  => Function type signature       name :: type
-- Nothing => Expression type signature   <expr> :: type
tcUserTypeSig loc hs_sig_ty mb_name
  | isCompleteHsSig hs_sig_ty
  = do { sigma_ty <- tcHsSigWcType ctxt_no_rrc hs_sig_ty
       ; traceTc "tcuser" (ppr sigma_ty)
       ; return $
         CompleteSig { sig_bndr  = mkLocalId name Many sigma_ty
                                   -- We use `Many' as the multiplicity here,
                                   -- as if this identifier corresponds to
                                   -- anything, it is a top-level
                                   -- definition. Which are all unrestricted in
                                   -- the current implementation.
                     , sig_ctxt  = ctxt_rrc  -- Report redundant constraints
                     , sig_loc   = loc } }
                       -- Location of the <type> in   f :: <type>

  -- Partial sig with wildcards
  | otherwise
  = return (PartialSig { psig_name = name, psig_hs_ty = hs_sig_ty
                       , sig_ctxt = ctxt_no_rrc, sig_loc = loc })
  where
    name   = case mb_name of
               Just n  -> n
               Nothing -> mkUnboundName (mkVarOcc "<expression>")

    ctxt_rrc    = ctxt_fn (lhsSigWcTypeContextSpan hs_sig_ty)
    ctxt_no_rrc = ctxt_fn NoRRC

    ctxt_fn :: ReportRedundantConstraints -> UserTypeCtxt
    ctxt_fn rcc = case mb_name of
               Just n  -> FunSigCtxt n rcc
               Nothing -> ExprSigCtxt rcc

lhsSigWcTypeContextSpan :: LHsSigWcType GhcRn -> ReportRedundantConstraints
-- | Find the location of the top-level context of a HsType.  For example:
--
-- @
--   forall a b. (Eq a, Ord b) => blah
--               ^^^^^^^^^^^^^
-- @
-- If there is none, return Nothing
lhsSigWcTypeContextSpan (HsWC { hswc_body = sigType }) = lhsSigTypeContextSpan sigType

lhsSigTypeContextSpan :: LHsSigType GhcRn -> ReportRedundantConstraints
lhsSigTypeContextSpan (L _ HsSig { sig_body = sig_ty }) = go sig_ty
  where
    go (L _ (HsQualTy { hst_ctxt = L span _ })) = WantRRC $ locA span -- Found it!
    go (L _ (HsForAllTy { hst_body = hs_ty })) = go hs_ty  -- Look under foralls
    go (L _ (HsParTy _ hs_ty)) = go hs_ty  -- Look under parens
    go _ = NoRRC  -- Did not find it

completeSigFromId :: UserTypeCtxt -> Id -> TcIdSigInfo
-- Used for instance methods and record selectors
completeSigFromId ctxt id
  = CompleteSig { sig_bndr = id
                , sig_ctxt = ctxt
                , sig_loc  = getSrcSpan id }

isCompleteHsSig :: LHsSigWcType GhcRn -> Bool
-- ^ If there are no wildcards, return a LHsSigWcType
isCompleteHsSig (HsWC { hswc_ext = wcs, hswc_body = hs_sig_ty })
   = null wcs && no_anon_wc_sig_ty hs_sig_ty

no_anon_wc_sig_ty :: LHsSigType GhcRn -> Bool
no_anon_wc_sig_ty (L _ (HsSig{sig_bndrs = outer_bndrs, sig_body = body}))
  =  all no_anon_wc_tvb (hsOuterExplicitBndrs outer_bndrs)
  && no_anon_wc_ty body

no_anon_wc_ty :: LHsType GhcRn -> Bool
no_anon_wc_ty lty = go lty
  where
    go (L _ ty) = case ty of
      HsWildCardTy _                 -> False
      HsAppTy _ ty1 ty2              -> go ty1 && go ty2
      HsAppKindTy _ ty ki            -> go ty && go ki
      HsFunTy _ w ty1 ty2            -> go ty1 && go ty2 && go (arrowToHsType w)
      HsListTy _ ty                  -> go ty
      HsTupleTy _ _ tys              -> gos tys
      HsSumTy _ tys                  -> gos tys
      HsOpTy _ ty1 _ ty2             -> go ty1 && go ty2
      HsParTy _ ty                   -> go ty
      HsIParamTy _ _ ty              -> go ty
      HsKindSig _ ty kind            -> go ty && go kind
      HsDocTy _ ty _                 -> go ty
      HsBangTy _ _ ty                -> go ty
      HsRecTy _ flds                 -> gos $ map (cd_fld_type . unLoc) flds
      HsExplicitListTy _ _ tys       -> gos tys
      HsExplicitTupleTy _ tys        -> gos tys
      HsForAllTy { hst_tele = tele
                 , hst_body = ty } -> no_anon_wc_tele tele
                                        && go ty
      HsQualTy { hst_ctxt = ctxt
               , hst_body = ty }  -> gos (unLoc ctxt) && go ty
      HsSpliceTy _ (HsSpliced _ _ (HsSplicedTy ty)) -> go $ L noSrcSpanA ty
      HsSpliceTy{} -> True
      HsTyLit{} -> True
      HsTyVar{} -> True
      HsStarTy{} -> True
      XHsType{} -> True       -- HsCoreTy, which does not have any wildcard

    gos = all go

no_anon_wc_tele :: HsForAllTelescope GhcRn -> Bool
no_anon_wc_tele tele = case tele of
  HsForAllVis   { hsf_vis_bndrs   = ltvs } -> all no_anon_wc_tvb ltvs
  HsForAllInvis { hsf_invis_bndrs = ltvs } -> all no_anon_wc_tvb ltvs

no_anon_wc_tvb :: LHsTyVarBndr flag GhcRn -> Bool
no_anon_wc_tvb (L _ tvb) = case tvb of
  UserTyVar _ _ _      -> True
  KindedTyVar _ _ _ ki -> no_anon_wc_ty ki

{- Note [Fail eagerly on bad signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If a type signature is wrong, fail immediately:

 * the type sigs may bind type variables, so proceeding without them
   can lead to a cascade of errors

 * the type signature might be ambiguous, in which case checking
   the code against the signature will give a very similar error
   to the ambiguity error.

ToDo: this means we fall over if any top-level type signature in the
module is wrong, because we typecheck all the signatures together
(see GHC.Tc.Gen.Bind.tcValBinds).  Moreover, because of top-level
captureTopConstraints, only insoluble constraints will be reported.
We typecheck all signatures at the same time because a signature
like   f,g :: blah   might have f and g from different SCCs.

So it's a bit awkward to get better error recovery, and no one
has complained!
-}

{- *********************************************************************
*                                                                      *
        Type checking a pattern synonym signature
*                                                                      *
************************************************************************

Note [Pattern synonym signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Pattern synonym signatures are surprisingly tricky (see #11224 for example).
In general they look like this:

   pattern P :: forall univ_tvs. req_theta
             => forall ex_tvs. prov_theta
             => arg1 -> .. -> argn -> res_ty

For parsing and renaming we treat the signature as an ordinary LHsSigType.

Once we get to type checking, we decompose it into its parts, in tcPatSynSig.

* Note that 'forall univ_tvs' and 'req_theta =>'
        and 'forall ex_tvs'   and 'prov_theta =>'
  are all optional.  We gather the pieces at the top of tcPatSynSig

* Initially the implicitly-bound tyvars (added by the renamer) include both
  universal and existential vars.

* After we kind-check the pieces and convert to Types, we do kind generalisation.

Note [Report unsolved equalities in tcPatSynSig]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's important that we solve /all/ the equalities in a pattern
synonym signature, because we are going to zonk the signature to
a Type (not a TcType), in GHC.Tc.TyCl.PatSyn.tc_patsyn_finish, and that
fails if there are un-filled-in coercion variables mentioned
in the type (#15694).

So we solve all the equalities we can, and report any unsolved ones,
rather than leaving them in the ambient constraints to be solved
later.  Pattern synonyms are top-level, so there's no problem with
completely solving them.
-}

tcPatSynSig :: Name -> LHsSigType GhcRn -> TcM TcPatSynInfo
-- See Note [Pattern synonym signatures]
-- See Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
tcPatSynSig name sig_ty@(L _ (HsSig{sig_bndrs = hs_outer_bndrs, sig_body = hs_ty}))
  | (hs_req, hs_ty1) <- splitLHsQualTy hs_ty
  , (ex_hs_tvbndrs, hs_prov, hs_body_ty) <- splitLHsSigmaTyInvis hs_ty1
  = do { traceTc "tcPatSynSig 1" (ppr sig_ty)

       ; skol_info <- mkSkolemInfo (DataConSkol name)
       ; (tclvl, wanted, (outer_bndrs, (ex_bndrs, (req, prov, body_ty))))
           <- pushLevelAndSolveEqualitiesX "tcPatSynSig"           $
                     -- See Note [solveEqualities in tcPatSynSig]
              tcOuterTKBndrs skol_info hs_outer_bndrs $
              tcExplicitTKBndrs ex_hs_tvbndrs         $
              do { req     <- tcHsContext hs_req
                 ; prov    <- tcHsContext hs_prov
                 ; body_ty <- tcHsOpenType hs_body_ty
                     -- A (literal) pattern can be unlifted;
                     -- e.g. pattern Zero <- 0#   (#12094)
                 ; return (req, prov, body_ty) }

       ; let implicit_tvs :: [TcTyVar]
             univ_bndrs   :: [TcInvisTVBinder]
             (implicit_tvs, univ_bndrs) = case outer_bndrs of
               HsOuterImplicit{hso_ximplicit = implicit_tvs} -> (implicit_tvs, [])
               HsOuterExplicit{hso_xexplicit = univ_bndrs}   -> ([], univ_bndrs)

       ; implicit_tvs <- zonkAndScopedSort implicit_tvs
       ; let implicit_bndrs = mkTyVarBinders SpecifiedSpec implicit_tvs

       -- Kind generalisation
       ; let ungen_patsyn_ty = build_patsyn_type implicit_bndrs univ_bndrs
                                                 req ex_bndrs prov body_ty
       ; traceTc "tcPatSynSig" (ppr ungen_patsyn_ty)
       ; kvs <- kindGeneralizeAll skol_info ungen_patsyn_ty
       ; reportUnsolvedEqualities skol_info kvs tclvl wanted
               -- See Note [Report unsolved equalities in tcPatSynSig]

       -- These are /signatures/ so we zonk to squeeze out any kind
       -- unification variables.  Do this after kindGeneralizeAll which may
       -- default kind variables to *.
       ; ze                   <- mkEmptyZonkEnv NoFlexi
       ; (ze, kv_bndrs)       <- zonkTyVarBindersX   ze (mkTyVarBinders InferredSpec kvs)
       ; (ze, implicit_bndrs) <- zonkTyVarBindersX   ze implicit_bndrs
       ; (ze, univ_bndrs)     <- zonkTyVarBindersX   ze univ_bndrs
       ; (ze, ex_bndrs)       <- zonkTyVarBindersX   ze ex_bndrs
       ; req                  <- zonkTcTypesToTypesX ze req
       ; prov                 <- zonkTcTypesToTypesX ze prov
       ; body_ty              <- zonkTcTypeToTypeX   ze body_ty

       -- Now do validity checking
       ; checkValidType ctxt $
         build_patsyn_type implicit_bndrs univ_bndrs req ex_bndrs prov body_ty

       -- Neither argument types nor the return type may be representation polymorphic.
       -- This is because, when creating a matcher:
       --   - the argument types become the the binder types (see test RepPolyPatySynArg),
       --   - the return type becomes the scrutinee type (see test RepPolyPatSynRes).
       ; let (arg_tys, res_ty) = tcSplitFunTys body_ty
       ; mapM_
           (\(Scaled _ arg_ty) -> checkTypeHasFixedRuntimeRep FixedRuntimeRepPatSynSigArg arg_ty)
           arg_tys
       ; checkTypeHasFixedRuntimeRep FixedRuntimeRepPatSynSigRes res_ty

       ; traceTc "tcTySig }" $
         vcat [ text "kvs"          <+> ppr_tvs (binderVars kv_bndrs)
              , text "implicit_tvs" <+> ppr_tvs (binderVars implicit_bndrs)
              , text "univ_tvs"     <+> ppr_tvs (binderVars univ_bndrs)
              , text "req" <+> ppr req
              , text "ex_tvs" <+> ppr_tvs (binderVars ex_bndrs)
              , text "prov" <+> ppr prov
              , text "body_ty" <+> ppr body_ty ]
       ; return (TPSI { patsig_name = name
                      , patsig_implicit_bndrs = kv_bndrs ++ implicit_bndrs
                      , patsig_univ_bndrs     = univ_bndrs
                      , patsig_req            = req
                      , patsig_ex_bndrs       = ex_bndrs
                      , patsig_prov           = prov
                      , patsig_body_ty        = body_ty }) }
  where
    ctxt = PatSynCtxt name

    build_patsyn_type implicit_bndrs univ_bndrs req ex_bndrs prov body
      = mkInvisForAllTys implicit_bndrs $
        mkInvisForAllTys univ_bndrs $
        mkPhiTy req $
        mkInvisForAllTys ex_bndrs $
        mkPhiTy prov $
        body

ppr_tvs :: [TyVar] -> SDoc
ppr_tvs tvs = braces (vcat [ ppr tv <+> dcolon <+> ppr (tyVarKind tv)
                           | tv <- tvs])


{- *********************************************************************
*                                                                      *
               Instantiating user signatures
*                                                                      *
********************************************************************* -}


tcInstSig :: TcIdSigInfo -> TcM TcIdSigInst
-- Instantiate a type signature; only used with plan InferGen
tcInstSig sig@(CompleteSig { sig_bndr = poly_id, sig_loc = loc })
  = setSrcSpan loc $  -- Set the binding site of the tyvars
    do { (tv_prs, theta, tau) <- tcInstTypeBndrs poly_id
              -- See Note [Pattern bindings and complete signatures]

       ; return (TISI { sig_inst_sig   = sig
                      , sig_inst_skols = tv_prs
                      , sig_inst_wcs   = []
                      , sig_inst_wcx   = Nothing
                      , sig_inst_theta = theta
                      , sig_inst_tau   = tau }) }

tcInstSig hs_sig@(PartialSig { psig_hs_ty = hs_ty
                             , sig_ctxt = ctxt
                             , sig_loc = loc })
  = setSrcSpan loc $  -- Set the binding site of the tyvars
    do { traceTc "Staring partial sig {" (ppr hs_sig)
       ; (wcs, wcx, tv_prs, theta, tau) <- tcHsPartialSigType ctxt hs_ty
         -- See Note [Checking partial type signatures] in GHC.Tc.Gen.HsType
       ; let inst_sig = TISI { sig_inst_sig   = hs_sig
                             , sig_inst_skols = tv_prs
                             , sig_inst_wcs   = wcs
                             , sig_inst_wcx   = wcx
                             , sig_inst_theta = theta
                             , sig_inst_tau   = tau }
       ; traceTc "End partial sig }" (ppr inst_sig)
       ; return inst_sig }


{- Note [Pattern bindings and complete signatures]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
      data T a = MkT a a
      f :: forall a. a->a
      g :: forall b. b->b
      MkT f g = MkT (\x->x) (\y->y)
Here we'll infer a type from the pattern of 'T a', but if we feed in
the signature types for f and g, we'll end up unifying 'a' and 'b'

So we instantiate f and g's signature with TyVarTv skolems
(newMetaTyVarTyVars) that can unify with each other.  If too much
unification takes place, we'll find out when we do the final
impedance-matching check in GHC.Tc.Gen.Bind.mkExport

See Note [TyVarTv] in GHC.Tc.Utils.TcMType

None of this applies to a function binding with a complete
signature, which doesn't use tcInstSig.  See GHC.Tc.Gen.Bind.tcPolyCheck.
-}

{- *********************************************************************
*                                                                      *
                   Pragmas and PragEnv
*                                                                      *
********************************************************************* -}

type TcPragEnv = NameEnv [LSig GhcRn]

emptyPragEnv :: TcPragEnv
emptyPragEnv = emptyNameEnv

lookupPragEnv :: TcPragEnv -> Name -> [LSig GhcRn]
lookupPragEnv prag_fn n = lookupNameEnv prag_fn n `orElse` []

extendPragEnv :: TcPragEnv -> (Name, LSig GhcRn) -> TcPragEnv
extendPragEnv prag_fn (n, sig) = extendNameEnv_Acc (:) Utils.singleton prag_fn n sig

---------------
mkPragEnv :: [LSig GhcRn] -> LHsBinds GhcRn -> TcPragEnv
mkPragEnv sigs binds
  = foldl' extendPragEnv emptyNameEnv prs
  where
    prs = mapMaybe get_sig sigs

    get_sig :: LSig GhcRn -> Maybe (Name, LSig GhcRn)
    get_sig (L l (SpecSig x lnm@(L _ nm) ty inl))
      = Just (nm, L l $ SpecSig   x lnm ty (add_arity nm inl))
    get_sig (L l (InlineSig x lnm@(L _ nm) inl))
      = Just (nm, L l $ InlineSig x lnm    (add_arity nm inl))
    get_sig (L l (SCCFunSig x st lnm@(L _ nm) str))
      = Just (nm, L l $ SCCFunSig x st lnm str)
    get_sig _ = Nothing

    add_arity n inl_prag   -- Adjust inl_sat field to match visible arity of function
      | isInlinePragma inl_prag
        -- add arity only for real INLINE pragmas, not INLINABLE
      = case lookupNameEnv ar_env n of
          Just ar -> inl_prag { inl_sat = Just ar }
          Nothing -> warnPprTrace True (text "mkPragEnv no arity" <+> ppr n) $
                     -- There really should be a binding for every INLINE pragma
                     inl_prag
      | otherwise
      = inl_prag

    -- ar_env maps a local to the arity of its definition
    ar_env :: NameEnv Arity
    ar_env = foldr lhsBindArity emptyNameEnv binds

lhsBindArity :: LHsBind GhcRn -> NameEnv Arity -> NameEnv Arity
lhsBindArity (L _ (FunBind { fun_id = id, fun_matches = ms })) env
  = extendNameEnv env (unLoc id) (matchGroupArity ms)
lhsBindArity _ env = env        -- PatBind/VarBind


-----------------
addInlinePrags :: TcId -> [LSig GhcRn] -> TcM TcId
addInlinePrags poly_id prags_for_me
  | inl@(L _ prag) : inls <- inl_prags
  = do { traceTc "addInlinePrag" (ppr poly_id $$ ppr prag)
       ; unless (null inls) (warn_multiple_inlines inl inls)
       ; return (poly_id `setInlinePragma` prag) }
  | otherwise
  = return poly_id
  where
    inl_prags = [L loc prag | L loc (InlineSig _ _ prag) <- prags_for_me]

    warn_multiple_inlines _ [] = return ()

    warn_multiple_inlines inl1@(L loc prag1) (inl2@(L _ prag2) : inls)
       | inlinePragmaActivation prag1 == inlinePragmaActivation prag2
       , noUserInlineSpec (inlinePragmaSpec prag1)
       =    -- Tiresome: inl1 is put there by virtue of being in a hs-boot loop
            -- and inl2 is a user NOINLINE pragma; we don't want to complain
         warn_multiple_inlines inl2 inls
       | otherwise
       = setSrcSpanA loc $
         let dia = TcRnUnknownMessage $
               mkPlainDiagnostic WarningWithoutFlag noHints $
                 (hang (text "Multiple INLINE pragmas for" <+> ppr poly_id)
                   2 (vcat (text "Ignoring all but the first"
                            : map pp_inl (inl1:inl2:inls))))
         in addDiagnosticTc dia

    pp_inl (L loc prag) = ppr prag <+> parens (ppr loc)


{- *********************************************************************
*                                                                      *
                   SPECIALISE pragmas
*                                                                      *
************************************************************************

Note [Handling SPECIALISE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea is this:

   foo :: Num a => a -> b -> a
   {-# SPECIALISE foo :: Int -> b -> Int #-}

We check that
   (forall a b. Num a => a -> b -> a)
      is more polymorphic than
   forall b. Int -> b -> Int
(for which we could use tcSubType, but see below), generating a HsWrapper
to connect the two, something like
      wrap = /\b. <hole> Int b dNumInt
This wrapper is put in the TcSpecPrag, in the ABExport record of
the AbsBinds.


        f :: (Eq a, Ix b) => a -> b -> Bool
        {-# SPECIALISE f :: (Ix p, Ix q) => Int -> (p,q) -> Bool #-}
        f = <poly_rhs>

From this the typechecker generates

    AbsBinds [ab] [d1,d2] [([ab], f, f_mono, prags)] binds

    SpecPrag (wrap_fn :: forall a b. (Eq a, Ix b) => XXX
                      -> forall p q. (Ix p, Ix q) => XXX[ Int/a, (p,q)/b ])

From these we generate:

    Rule:       forall p, q, (dp:Ix p), (dq:Ix q).
                    f Int (p,q) dInt ($dfInPair dp dq) = f_spec p q dp dq

    Spec bind:  f_spec = wrap_fn <poly_rhs>

Note that

  * The LHS of the rule may mention dictionary *expressions* (eg
    $dfIxPair dp dq), and that is essential because the dp, dq are
    needed on the RHS.

  * The RHS of f_spec, <poly_rhs> has a *copy* of 'binds', so that it
    can fully specialise it.

From the TcSpecPrag, in GHC.HsToCore.Binds we generate a binding for f_spec and a RULE:

   f_spec :: Int -> b -> Int
   f_spec = wrap<f rhs>

   RULE: forall b (d:Num b). f b d = f_spec b

The RULE is generated by taking apart the HsWrapper, which is a little
delicate, but works.

Some wrinkles

1. In tcSpecWrapper, rather than calling tcSubType, we directly call
   skolemise/instantiate.  That is mainly because of wrinkle (2).

   Historical note: in the past, tcSubType did co/contra stuff, which
   could generate too complex a LHS for the RULE, which was another
   reason for not using tcSubType.  But that reason has gone away
   with simple subsumption (#17775).

2. We need to take care with type families (#5821).  Consider
      type instance F Int = Bool
      f :: Num a => a -> F a
      {-# SPECIALISE foo :: Int -> Bool #-}

  We *could* try to generate an f_spec with precisely the declared type:
      f_spec :: Int -> Bool
      f_spec = <f rhs> Int dNumInt |> co

      RULE: forall d. f Int d = f_spec |> sym co

  but the 'co' and 'sym co' are (a) playing no useful role, and (b) are
  hard to generate.  At all costs we must avoid this:
      RULE: forall d. f Int d |> co = f_spec
  because the LHS will never match (indeed it's rejected in
  decomposeRuleLhs).

  So we simply do this:
    - Generate a constraint to check that the specialised type (after
      skolemisation) is equal to the instantiated function type.
    - But *discard* the evidence (coercion) for that constraint,
      so that we ultimately generate the simpler code
          f_spec :: Int -> F Int
          f_spec = <f rhs> Int dNumInt

          RULE: forall d. f Int d = f_spec
      You can see this discarding happening in tcSpecPrag

3. Note that the HsWrapper can transform *any* function with the right
   type prefix
       forall ab. (Eq a, Ix b) => XXX
   regardless of XXX.  It's sort of polymorphic in XXX.  This is
   useful: we use the same wrapper to transform each of the class ops, as
   well as the dict.  That's what goes on in GHC.Tc.TyCl.Instance.mk_meth_spec_prags
-}

tcSpecPrags :: Id -> [LSig GhcRn]
            -> TcM [LTcSpecPrag]
-- Add INLINE and SPECIALSE pragmas
--    INLINE prags are added to the (polymorphic) Id directly
--    SPECIALISE prags are passed to the desugarer via TcSpecPrags
-- Pre-condition: the poly_id is zonked
-- Reason: required by tcSubExp
tcSpecPrags poly_id prag_sigs
  = do { traceTc "tcSpecPrags" (ppr poly_id <+> ppr spec_sigs)
       ; unless (null bad_sigs) warn_discarded_sigs
       ; pss <- mapAndRecoverM (wrapLocMA (tcSpecPrag poly_id)) spec_sigs
       ; return $ concatMap (\(L l ps) -> map (L (locA l)) ps) pss }
  where
    spec_sigs = filter isSpecLSig prag_sigs
    bad_sigs  = filter is_bad_sig prag_sigs
    is_bad_sig s = not (isSpecLSig s || isInlineLSig s || isSCCFunSig s)

    warn_discarded_sigs
      = let dia = TcRnUnknownMessage $
              mkPlainDiagnostic WarningWithoutFlag noHints $
                (hang (text "Discarding unexpected pragmas for" <+> ppr poly_id)
                    2 (vcat (map (ppr . getLoc) bad_sigs)))
        in addDiagnosticTc dia

--------------
tcSpecPrag :: TcId -> Sig GhcRn -> TcM [TcSpecPrag]
tcSpecPrag poly_id prag@(SpecSig _ fun_name hs_tys inl)
-- See Note [Handling SPECIALISE pragmas]
--
-- The Name fun_name in the SpecSig may not be the same as that of the poly_id
-- Example: SPECIALISE for a class method: the Name in the SpecSig is
--          for the selector Id, but the poly_id is something like $cop
-- However we want to use fun_name in the error message, since that is
-- what the user wrote (#8537)
  = addErrCtxt (spec_ctxt prag) $
    do  { warnIf (not (isOverloadedTy poly_ty || isInlinePragma inl)) $
                 TcRnUnknownMessage $ mkPlainDiagnostic WarningWithoutFlag noHints
                   (text "SPECIALISE pragma for non-overloaded function"
                    <+> quotes (ppr fun_name))
                    -- Note [SPECIALISE pragmas]
        ; spec_prags <- mapM tc_one hs_tys
        ; traceTc "tcSpecPrag" (ppr poly_id $$ nest 2 (vcat (map ppr spec_prags)))
        ; return spec_prags }
  where
    name      = idName poly_id
    poly_ty   = idType poly_id
    spec_ctxt prag = hang (text "In the pragma:") 2 (ppr prag)

    tc_one hs_ty
      = do { spec_ty <- tcHsSigType   (FunSigCtxt name NoRRC) hs_ty
           ; wrap    <- tcSpecWrapper (FunSigCtxt name (lhsSigTypeContextSpan hs_ty)) poly_ty spec_ty
           ; return (SpecPrag poly_id wrap inl) }

tcSpecPrag _ prag = pprPanic "tcSpecPrag" (ppr prag)

--------------
tcSpecWrapper :: UserTypeCtxt -> TcType -> TcType -> TcM HsWrapper
-- A simpler variant of tcSubType, used for SPECIALISE pragmas
-- See Note [Handling SPECIALISE pragmas], wrinkle 1
tcSpecWrapper ctxt poly_ty spec_ty
  = do { (sk_wrap, inst_wrap)
               <- tcSkolemise ctxt spec_ty $ \ spec_tau ->
                  do { (inst_wrap, tau) <- topInstantiate orig poly_ty
                     ; _ <- unifyType Nothing spec_tau tau
                            -- Deliberately ignore the evidence
                            -- See Note [Handling SPECIALISE pragmas],
                            --   wrinkle (2)
                     ; return inst_wrap }
       ; return (sk_wrap <.> inst_wrap) }
  where
    orig = SpecPragOrigin ctxt

--------------
tcImpPrags :: [LSig GhcRn] -> TcM [LTcSpecPrag]
-- SPECIALISE pragmas for imported things
tcImpPrags prags
  = do { this_mod <- getModule
       ; dflags <- getDynFlags
       ; if (not_specialising dflags) then
            return []
         else do
            { pss <- mapAndRecoverM (wrapLocMA tcImpSpec)
                     [L loc (name,prag)
                             | (L loc prag@(SpecSig _ (L _ name) _ _)) <- prags
                             , not (nameIsLocalOrFrom this_mod name) ]
            ; return $ concatMap (\(L l ps) -> map (L (locA l)) ps) pss } }
  where
    -- Ignore SPECIALISE pragmas for imported things
    -- when we aren't specialising, or when we aren't generating
    -- code.  The latter happens when Haddocking the base library;
    -- we don't want complaints about lack of INLINABLE pragmas
    not_specialising dflags
      | not (gopt Opt_Specialise dflags) = True
      | otherwise = case backend dflags of
                      NoBackend   -> True
                      Interpreter -> True
                      _other      -> False

tcImpSpec :: (Name, Sig GhcRn) -> TcM [TcSpecPrag]
tcImpSpec (name, prag)
 = do { id <- tcLookupId name
      ; if hasSomeUnfolding (realIdUnfolding id)
           -- See Note [SPECIALISE pragmas for imported Ids]
        then tcSpecPrag id prag
        else do { let dia = TcRnUnknownMessage $
                        mkPlainDiagnostic WarningWithoutFlag noHints (impSpecErr name)
                ; addDiagnosticTc dia
                ; return [] } }

impSpecErr :: Name -> SDoc
impSpecErr name
  = hang (text "You cannot SPECIALISE" <+> quotes (ppr name))
       2 (vcat [ text "because its definition is not visible in this module"
               , text "Hint: make sure" <+> ppr mod <+> text "is compiled with -O"
               , text "      and that" <+> quotes (ppr name)
                 <+> text "has an INLINABLE pragma" ])
  where
    mod = nameModule name

{- Note [SPECIALISE pragmas for imported Ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An imported Id may or may not have an unfolding.  If not, we obviously
can't specialise it here; indeed the desugar falls over (#18118).

We used to test whether it had a user-specified INLINABLE pragma but,
because of Note [Worker/wrapper for INLINABLE functions] in
GHC.Core.Opt.WorkWrap, even an INLINABLE function may end up with
a wrapper that has no pragma, just an unfolding (#19246).  So now
we just test whether the function has an unfolding.

There's a risk that a pragma-free function may have an unfolding now
(because it is fairly small), and then gets a bit bigger, and no
longer has an unfolding in the future.  But then you'll get a helpful
error message suggesting an INLINABLE pragma, which you can follow.
That seems enough for now.
-}
