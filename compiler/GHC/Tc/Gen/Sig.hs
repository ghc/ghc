{-
(c) The University of Glasgow 2006-2012
(c) The GRASP Project, Glasgow University, 1992-2002

-}


{-# LANGUAGE TypeFamilies #-}

module GHC.Tc.Gen.Sig(
       TcSigInfo(..), TcIdSig(..), TcSigFun,

       isPartialSig, hasCompleteSig, tcSigInfoName, tcIdSigLoc,
       completeSigPolyId_maybe, isCompleteHsSig,
       lhsSigWcTypeContextSpan, lhsSigTypeContextSpan,

       tcTySigs, tcUserTypeSig, completeSigFromId,
       tcInstSig,

       TcPragEnv, emptyPragEnv, lookupPragEnv, extendPragEnv,
       mkPragEnv, tcSpecPrags, tcSpecWrapper, tcImpPrags,
       addInlinePrags, addInlinePragArity,

       tcRules
   ) where

import GHC.Prelude
import GHC.Data.FastString

import GHC.Driver.DynFlags
import GHC.Driver.Backend

import GHC.Hs

import {-# SOURCE #-} GHC.Tc.Gen.Expr  ( tcInferRho, tcCheckMonoExpr )

import GHC.Tc.Errors.Types
import GHC.Tc.Gen.HsType
import GHC.Tc.Solver( reportUnsolvedEqualities, pushLevelAndSolveEqualitiesX
                    , emitResidualConstraints )
import GHC.Tc.Solver.Solve( solveWanteds )
import GHC.Tc.Solver.Monad( runTcS, runTcSSpecPrag )
import GHC.Tc.Validity ( checkValidType )

import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Utils.Unify( DeepSubsumptionFlag(..), tcSkolemise, unifyType, buildImplicationFor )
import GHC.Tc.Utils.Instantiate( topInstantiate, tcInstTypeBndrs )
import GHC.Tc.Utils.Env

import GHC.Tc.Types.Origin
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.Constraint

import GHC.Tc.Zonk.TcType
import GHC.Tc.Zonk.Type

import GHC.Core( hasSomeUnfolding )
import GHC.Core.Type
import GHC.Core.Multiplicity
import GHC.Core.Predicate
import GHC.Core.TyCo.Rep( mkNakedFunTy )
import GHC.Core.TyCon( isTypeFamilyTyCon )

import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Types.Id  ( idName, idType, setInlinePragma
                     , mkLocalId, realIdUnfolding )
import GHC.Types.Basic
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.SrcLoc

import GHC.Builtin.Names( mkUnboundName )
import GHC.Unit.Module( Module, getModule )

import GHC.Utils.Misc as Utils ( singleton )
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.Bag
import GHC.Data.Maybe( orElse, whenIsJust )

import Control.Monad( unless )
import Data.Foldable ( toList )
import qualified Data.List.NonEmpty as NE
import Data.Maybe( mapMaybe )

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

* tcTySig: Sig -> TcIdSig
  - For a /complete/ signature, like 'f' above, tcTySig kind-checks
    the HsType, producing a Type, and wraps it in a TcCompleteSig, and
    extend the type environment with this polymorphic 'f'.

  - For a /partial/signature, like 'g' above, tcTySig does nothing
    Instead it just wraps the pieces in a PartialSig, to be handled
    later.

* tcInstSig: TcIdSig -> TcIdSigInst
  In tcMonoBinds, when looking at an individual binding, we use
  tcInstSig to instantiate the signature forall's in the signature,
  and attribute that instantiated (monomorphic) type to the
  binder.  You can see this in GHC.Tc.Gen.Bind.tcLhsId.

  The instantiation does the obvious thing for complete signatures,
  but for /partial/ signatures it starts from the HsSyn, so it
  has to kind-check it etc: tcHsPartialSigType.  It's convenient
  to do this at the same time as instantiation, because we can
  make the wildcards into unification variables right away, rather
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
tcTySig (L _ (XSig (IdSig id)))
  = do { let ctxt = FunSigCtxt (idName id) NoRRC
                    -- NoRRC: do not report redundant constraints
                    -- The user has no control over the signature!
             sig = completeSigFromId ctxt id
       ; return [TcIdSig (TcCompleteSig sig)] }

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


tcUserTypeSig :: SrcSpan -> LHsSigWcType GhcRn -> Maybe Name -> TcM TcIdSig
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
       ; return $ TcCompleteSig $
         CSig { sig_bndr  = mkLocalId name ManyTy sigma_ty
                                   -- We use `Many' as the multiplicity here,
                                   -- as if this identifier corresponds to
                                   -- anything, it is a top-level
                                   -- definition. Which are all unrestricted in
                                   -- the current implementation.
              , sig_ctxt  = ctxt_rrc  -- Report redundant constraints
              , sig_loc   = loc } }   -- Location of the <type> in   f :: <type>

  -- Partial sig with wildcards
  | otherwise
  = return $ TcPartialSig $
    PSig { psig_name = name, psig_hs_ty = hs_sig_ty
         , psig_ctxt = ctxt_no_rrc, psig_loc = loc }
  where
    name = case mb_name of
               Just n  -> n
               Nothing -> mkUnboundName (mkVarOccFS (fsLit "<expression>"))

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

completeSigFromId :: UserTypeCtxt -> Id -> TcCompleteSig
-- Used for instance methods and record selectors
completeSigFromId ctxt id
  = CSig { sig_bndr = id
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
      HsFunTy _ w ty1 ty2            -> go ty1 && go ty2 && all go (multAnnToHsType w)
      HsListTy _ ty                  -> go ty
      HsTupleTy _ _ tys              -> gos tys
      HsSumTy _ tys                  -> gos tys
      HsOpTy _ _ ty1 _ ty2           -> go ty1 && go ty2
      HsParTy _ ty                   -> go ty
      HsIParamTy _ _ ty              -> go ty
      HsKindSig _ ty kind            -> go ty && go kind
      HsDocTy _ ty _                 -> go ty
      HsExplicitListTy _ _ tys       -> gos tys
      HsExplicitTupleTy _ _ tys      -> gos tys
      HsForAllTy { hst_tele = tele
                 , hst_body = ty } -> no_anon_wc_tele tele
                                        && go ty
      HsQualTy { hst_ctxt = ctxt
               , hst_body = ty }  -> gos (unLoc ctxt) && go ty
      HsSpliceTy (HsUntypedSpliceTop _ ty) _ -> go ty
      HsSpliceTy (HsUntypedSpliceNested _) _ -> True
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
no_anon_wc_tvb (L _ tvb) = case hsBndrKind tvb of
  HsBndrNoKind _  -> True
  HsBndrKind _ ki -> no_anon_wc_ty ki

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

tcPatSynSig :: Name -> LHsSigType GhcRn -> TcM TcPatSynSig
-- See Note [Pattern synonym signatures]
-- See Note [Recipe for checking a signature] in GHC.Tc.Gen.HsType
tcPatSynSig name sig_ty@(L _ (HsSig{sig_bndrs = hs_outer_bndrs, sig_body = hs_ty}))
  | (hs_req, hs_ty1) <- splitLHsQualTy hs_ty
  , (ex_hs_tvbndrs, hs_prov, hs_body_ty) <- splitLHsSigmaTyInvis hs_ty1
  = do { traceTc "tcPatSynSig 1" (ppr sig_ty)

       ; skol_info <- mkSkolemInfo (DataConSkol name)
       ; (tclvl, wanted, (outer_bndrs, (ex_bndrs, (req, prov, body_ty))))
           <- pushLevelAndSolveEqualitiesX "tcPatSynSig"           $
                     -- See Note [Report unsolved equalities in tcPatSynSig]
              do { res_kind  <- newOpenTypeKind
                             -- "open" because a (literal) pattern can be unlifted;
                             -- e.g. pattern Zero <- 0#   (#12094)
                   -- See Note [Escaping kind in type signatures] in GHC.Tc.Gen.HsType
                 ; tcOuterTKBndrs skol_info hs_outer_bndrs   $
                   tcExplicitTKBndrs skol_info ex_hs_tvbndrs $
                   do { req     <- tcHsContext hs_req
                      ; prov    <- tcHsContext hs_prov
                      ; body_ty <- tcCheckLHsType hs_body_ty res_kind
                      ; return (req, prov, body_ty) } }

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
       ; (kv_bndrs, implicit_bndrs, univ_bndrs, ex_bndrs, req, prov, body_ty) <-
         initZonkEnv NoFlexi $
         runZonkBndrT (zonkTyVarBindersX (mkTyVarBinders InferredSpec kvs)) $ \ kv_bndrs ->
         runZonkBndrT (zonkTyVarBindersX implicit_bndrs) $ \ implicit_bndrs  ->
         runZonkBndrT (zonkTyVarBindersX univ_bndrs) $ \ univ_bndrs ->
           do { req            <- zonkTcTypesToTypesX req
              ; runZonkBndrT (zonkTyVarBindersX ex_bndrs) $ \ ex_bndrs ->
           do { prov           <- zonkTcTypesToTypesX prov
              ; body_ty        <- zonkTcTypeToTypeX   body_ty
              ; return (kv_bndrs, implicit_bndrs, univ_bndrs, ex_bndrs,
                         req, prov, body_ty) } }

       -- Now do validity checking
       ; checkValidType ctxt $
         build_patsyn_type implicit_bndrs univ_bndrs req ex_bndrs prov body_ty

       -- Neither argument types nor the return type may be representation polymorphic.
       -- This is because, when creating a matcher:
       --   - the argument types become the binder types (see test RepPolyPatySynArg),
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
       ; return $
         PatSig { patsig_name = name
                , patsig_implicit_bndrs = kv_bndrs ++ implicit_bndrs
                , patsig_univ_bndrs     = univ_bndrs
                , patsig_req            = req
                , patsig_ex_bndrs       = ex_bndrs
                , patsig_prov           = prov
                , patsig_body_ty        = body_ty } }
  where
    ctxt = PatSynCtxt name

    build_patsyn_type implicit_bndrs univ_bndrs req ex_bndrs prov body
      = mkInvisForAllTys implicit_bndrs $
        mkInvisForAllTys univ_bndrs $
        mk_naked_phi_ty req $
        mkInvisForAllTys ex_bndrs $
        mk_naked_phi_ty prov $
        body

    -- Use mk_naked_phi_ty because we call build_patsyn_type /before zonking/
    -- just before kindGeneraliseAll, and the invariants that mkPhiTy checks
    -- don't hold of the un-zonked types.  #22521 was a case in point.
    -- (We also called build_patsyn_type on the fully zonked type, so mkPhiTy
    --  would work; but it doesn't seem worth duplicating the code.)
    mk_naked_phi_ty :: [TcPredType] -> TcType -> TcType
    mk_naked_phi_ty theta body = foldr (mkNakedFunTy invisArgTypeLike) body theta

ppr_tvs :: [TyVar] -> SDoc
ppr_tvs tvs = braces (vcat [ ppr tv <+> dcolon <+> ppr (tyVarKind tv)
                           | tv <- tvs])


{- *********************************************************************
*                                                                      *
               Instantiating user signatures
*                                                                      *
********************************************************************* -}


tcInstSig :: TcIdSig -> TcM TcIdSigInst
-- Instantiate a type signature; only used with plan InferGen
tcInstSig hs_sig@(TcCompleteSig (CSig { sig_bndr = poly_id, sig_loc = loc }))
  = setSrcSpan loc $  -- Set the binding site of the tyvars
    do { (tv_prs, theta, tau) <- tcInstTypeBndrs (idType poly_id)
              -- See Note [Pattern bindings and complete signatures]

       ; return (TISI { sig_inst_sig   = hs_sig
                      , sig_inst_skols = tv_prs
                      , sig_inst_wcs   = []
                      , sig_inst_wcx   = Nothing
                      , sig_inst_theta = theta
                      , sig_inst_tau   = tau }) }

tcInstSig hs_sig@(TcPartialSig (PSig { psig_hs_ty = hs_ty
                                     , psig_ctxt = ctxt
                                     , psig_loc = loc }))
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
    get_sig sig@(L _ (SpecSig _ (L _ nm) _ _)) = Just (nm, add_arity nm sig)
    get_sig sig@(L _ (SpecSigE nm _ _ _))      = Just (nm, add_arity nm sig)
    get_sig sig@(L _ (InlineSig _ (L _ nm) _)) = Just (nm, add_arity nm sig)
    get_sig sig@(L _ (SCCFunSig _ (L _ nm) _)) = Just (nm, sig)
    get_sig _ = Nothing

    add_arity n sig  -- Adjust inl_sat field to match visible arity of function
      = case lookupNameEnv ar_env n of
          Just ar -> addInlinePragArity ar sig
          Nothing -> sig -- See Note [Pattern synonym inline arity]

    -- ar_env maps a local to the arity of its definition
    ar_env :: NameEnv Arity
    ar_env = foldr lhsBindArity emptyNameEnv binds

addInlinePragArity :: Arity -> LSig GhcRn -> LSig GhcRn
addInlinePragArity ar (L l (InlineSig x nm inl))  = L l (InlineSig x nm (add_inl_arity ar inl))
addInlinePragArity ar (L l (SpecSig x nm ty inl)) = L l (SpecSig x nm ty (add_inl_arity ar inl))
addInlinePragArity ar (L l (SpecSigE n x e inl))  = L l (SpecSigE n x e (add_inl_arity ar inl))
addInlinePragArity _ sig = sig

add_inl_arity :: Arity -> InlinePragma -> InlinePragma
add_inl_arity ar prag@(InlinePragma { inl_inline = inl_spec })
  | Inline {} <- inl_spec  -- Add arity only for real INLINE pragmas, not INLINABLE
  = prag { inl_sat = Just ar }
  | otherwise
  = prag

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
         let dia = TcRnMultipleInlinePragmas poly_id inl1 (inl2 NE.:| inls)
         in addDiagnosticTc dia


{- Note [Pattern synonym inline arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
    {-# INLINE P #-}
    pattern P x = (x, True)

The INLINE pragma attaches to both the /matcher/ and the /builder/ for
the pattern synonym; see Note [Pragmas for pattern synonyms] in
GHC.Tc.TyCl.PatSyn.  But they have different inline arities (i.e. number
of binders to which we apply the function before inlining), and we don't
know what those arities are yet.  So for pattern synonyms we don't set
the inl_sat field yet; instead we do so (via addInlinePragArity) in
GHC.Tc.TyCl.PatSyn.tcPatSynMatcher and tcPatSynBuilderBind.

It's a bit messy that we set the arities in different ways.  Perhaps we
should add the arity later for all binders.  But it works fine like this.
-}


{- *********************************************************************
*                                                                      *
                   SPECIALISE pragmas
*                                                                      *
************************************************************************

Note [Overview of SPECIALISE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea is this:

   foo :: Num a => a -> b -> a
   foo = rhs
   {-# SPECIALISE foo :: Int -> b -> Int #-}   -- Old form
   {-# SPECIALISE foo @Float #-}               -- New form

Generally:
* Rename as usual
* Typecheck, attaching info to the ABExport record of the AbsBinds for foo
* Desugar by generating
   - a specialised binding $sfoo = rhs @Float
   - a rewrite rule like   RULE "USPEC foo" foo @Float = $sfoo

There are two major routes:

* Old form
  - Handled by `SpecSig` and `SpecPrag`
  - Deals with SPECIALISE pragmas have multiple signatures
       {-# SPECIALISE f :: Int -> Int, Float -> Float #-}
  - See Note [Handling old-form SPECIALISE pragmas]
  - Deprecated, to be removed in GHC 9.18 as per #25540.

* New form, described in GHC Proposal #493
  - Handled by `SpecSigE` and `SpecPragE`
  - Deals with SPECIALISE pragmas which may have value arguments
       {-# SPECIALISE f @Int 3 #-}
  - See Note [Handling new-form SPECIALISE pragmas]

Note [Handling new-form SPECIALISE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
New-form SPECIALISE pragmas are described by GHC Proposal #493.

The pragma takes the form of a function application, possibly with intervening
parens and type signatures, with a variable at the head:

    {-# SPECIALISE f1 @Int 3 #-}
    {-# SPECIALISE f2 :: Int -> Int #-}
    {-# SPECIALISE (f3 :: Int -> Int) 5 #-}

It may also have rule for-alls at the top, e.g.

    {-# SPECIALISE forall x xs. f4 (x:xs) #-}
    {-# SPECIALISE forall a. forall x xs. f5 @a @a (x:xs) #-}

See `GHC.Rename.Bind.checkSpecESigShape` for the shape-check.

Example:
  f :: forall a b. (Eq a, Eq b, Eq c) => a -> b -> c -> Bool -> blah
  {-# SPECIALISE forall x y. f (x::Int) y y True #-}
                 -- y::p

We want to generate:

  RULE forall @p (d1::Eq Int) (d2::Eq p) (d3::Eq p) (x::Int) (y::p).
     f @Int @p @p d1 d2 d3 x y y True
        = $sf @p d2 x y
  $sf @p (d2::Eq p) (x::Int) (y::p)
     = let d1 = $fEqInt
           d3 = d2
       in <f-rhs> @p @p @Int (d1::Eq p) (d2::Eq p) (d3::Eq p) x y y True

Note that

* The `rule_bndrs`, over which the RULE is quantified, are all the variables
  free in the call to `f`, /ignoring/ all dictionary simplification.  Why?
  Because we want to make the rule maximally applicable; provided the types
  match, the dictionaries should match.

    rule_bndrs = @p (d1::Eq Int) (d2::Eq p) (d3::Eq p) (x::Int) (y::p).

  Note that we have separate binders for `d1` and `d2` even though they are
  the same (Eq p) dictionary. Reason: we don't want to force them to be visibly
  equal at the call site.

* The `spec_bnrs`, which are lambda-bound in the specialised function `$sf`,
  are a subset of `rule_bndrs`.

    spec_bndrs = @p (d2::Eq p) (x::Int) (y::p)

* The `spec_const_binds` make up the difference between `rule_bndrs` and
  `spec_bndrs`.  They communicate the specialisation!
   If `spec_bndrs` = `rule_bndrs`, no specialisation has happened.

    spec_const_binds =  let d1 = $fEqInt
                            d3 = d2

This is done in three parts.

  A. Typechecker: `GHC.Tc.Gen.Sig.tcSpecPrag`

    (1) Typecheck the expression, capturing its constraints

    (2) Solve these constraints, but in special TcSSpecPrag mode which ensures
        each original Wanted is either fully solved or left untouched.
        See Note [Fully solving constraints for specialisation].

    (3) Compute the constraints to quantify over, using `getRuleQuantCts` on
        the unsolved constraints returned by (2).

    (4) Emit the residual (non-solved, non-quantified) constraints, and wrap the
        expression in a let binding for those constraints.

    (5) Wrap the call in the combined evidence bindings from steps (2) and (4)

    (6) Store all the information in a 'SpecPragE' record, to be consumed
        by the desugarer.

  B. Zonker: `GHC.Tc.Zonk.Type.zonkLTcSpecPrags`

    The zonker does a little extra work to collect any free type variables
    of the LHS. See Note [Free tyvars on rule LHS] in GHC.Tc.Zonk.Type.
    These weren't conveniently available earlier.

  C. Desugarer: `GHC.HsToCore.Binds.dsSpec`.

    See Note [Desugaring new-form SPECIALISE pragmas] in GHC.HsToCore.Binds for details,
    but in brief:

    (1) Simplify the expression. This is important because a type signature in
        the expression will have led to type/dictionary abstractions/applications.
        After simplification it should look like
            let <dict-binds> in f d1 d2 d3

    (2) `prepareSpecLHS` identifies the `spec_const_binds`, discards the other
        dictionary bindings, and decomposes the call.

    (3) Then we build the specialised function $sf, and concoct a RULE
        of the form:
           forall @a @b d1 d2 d3. f d1 d2 d3 = $sf d1 d2 d3

Note [Fully solving constraints for specialisation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As far as specialisation is concerned, it is actively harmful to simplify
constraints without /fully/ solving them.

Example:

  f :: ∀ a t. (Eq a, ∀x. Eq x => Eq (t x)). t a -> Char
  {-# SPECIALISE f @Int #-}

  Typechecking 'f' will result in [W] Eq Int, [W] ∀x. Eq x => Eq (t x).
  We absolutely MUST leave the quantified constraint alone, because we want to
  quantify over it. If we were to try to simplify it, we would emit an
  implication and would thereafter never be able to quantify over the original
  quantified constraint.

  However, we still need to simplify quantified constraints that can be
  /fully solved/ from instances, otherwise we would never be able to
  specialise them away. Example: {-# SPECIALISE f @a @[] #-}.

The conclusion is this:

  when solving the constraints that arise from a specialise pragma, following
  the recipe described in Note [Handling new-form SPECIALISE pragmas], all
  Wanted quantified constraints should either be:
    - fully solved (no free evidence variables), or
    - left untouched.

To achieve this, we run the solver in a special "all-or-nothing" solving mode,
described in Note [TcSSpecPrag] in GHC.Tc.Solver.Monad.

Note that a similar problem arises in other situations in which the solver takes
an irreversible step, such as using a top-level class instance. This is currently
less important, as the desugarer can handle these cases. To explain, consider:

    g :: ∀ a. Eq a => a -> Bool
    {-# SPECIALISE g @[e] #-}

  Typechecking 'g' will result in [W] Eq [e]. Were we to simplify this to
  [W] Eq e, we would have difficulty generating a RULE for the specialisation:

    $sg :: Eq e => [e] -> Bool

    RULE ∀ e (d :: Eq [e]). g @[e] d = $sg @e (??? :: Eq e)
      -- Can't fill in ??? because we can't run instances in reverse.

    RULE ∀ e (d :: Eq e). g @[e] ($fEqList @e d) = $sg @e d
      -- Bad RULE matching template: matches on the structure of a dictionary

  Moreover, there is no real benefit to any of this, because the specialiser
  can't do anything useful from the knowledge that a dictionary for 'Eq [e]' is
  constructed from a dictionary for 'Eq e' using the 'Eq' instance for lists.

Here, it would make sense to also use the "solve completely" mechanism in the
typechecker to avoid producing evidence terms that we can't "run in reverse".
However, the current implementation tackles this issue in the desugarer, as is
explained in Note [prepareSpecLHS] in GHC.HsToCore.Binds.
So, for the time being at least, in TcSSpecPrag mode, we don't attempt to "fully solve"
constraints when we use a top-level instance. This might change in the future,
were we to decide to attempt to address [Shortcoming] in Note [prepareSpecLHS]
in GHC.HsToCore.Binds.

Note [Handling old-form SPECIALISE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
NB: this code path is deprecated, and is scheduled to be removed in GHC 9.18, as per #25440.
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

Note [SPECIALISE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~
There is no point in a SPECIALISE pragma for a non-overloaded function:
   reverse :: [a] -> [a]
   {-# SPECIALISE reverse :: [Int] -> [Int] #-}

But SPECIALISE INLINE *can* make sense for GADTS:
   data Arr e where
     ArrInt :: !Int -> ByteArray# -> Arr Int
     ArrPair :: !Int -> Arr e1 -> Arr e2 -> Arr (e1, e2)

   (!:) :: Arr e -> Int -> e
   {-# SPECIALISE INLINE (!:) :: Arr Int -> Int -> Int #-}
   {-# SPECIALISE INLINE (!:) :: Arr (a, b) -> Int -> (a, b) #-}
   (ArrInt _ ba)     !: (I# i) = I# (indexIntArray# ba i)
   (ArrPair _ a1 a2) !: i      = (a1 !: i, a2 !: i)

When (!:) is specialised it becomes non-recursive, and can usefully
be inlined.  Scary!  So we only warn for SPECIALISE *without* INLINE
for a non-overloaded function.
-}

tcSpecPrags :: Id -> [LSig GhcRn]
            -> TcM [LTcSpecPrag]
-- Add INLINE and SPECIALISE pragmas
--    INLINE prags are added to the (polymorphic) Id directly
--    SPECIALISE prags are passed to the desugarer via TcSpecPrags
-- Pre-condition: the poly_id is zonked
-- Reason: required by tcSubExp
tcSpecPrags poly_id prag_sigs
  = do { traceTc "tcSpecPrags" (ppr poly_id <+> ppr spec_sigs)
       ; whenIsJust (NE.nonEmpty bad_sigs) warn_discarded_sigs
       ; pss <- mapAndRecoverM (wrapLocMA (tcSpecPrag poly_id)) spec_sigs
       ; return $ concatMap (\(L l ps) -> map (L (locA l)) ps) pss }
  where
    spec_sigs = filter isSpecLSig prag_sigs
    bad_sigs  = filter is_bad_sig prag_sigs
    is_bad_sig s = not (isSpecLSig s || isInlineLSig s || isSCCFunSig s)

    warn_discarded_sigs bad_sigs_ne
      = let dia = TcRnUnexpectedPragmas poly_id bad_sigs_ne
        in addDiagnosticTc dia

--------------
tcSpecPrag :: TcId -> Sig GhcRn -> TcM [TcSpecPrag]
tcSpecPrag poly_id prag@(SpecSig _ fun_name hs_tys inl)
-- See Note [Handling old-form SPECIALISE pragmas]
--
-- The Name fun_name in the SpecSig may not be the same as that of the poly_id
-- Example: SPECIALISE for a class method: the Name in the SpecSig is
--          for the selector Id, but the poly_id is something like $cop
-- However we want to use fun_name in the error message, since that is
-- what the user wrote (#8537)
  = addErrCtxt (SpecPragmaCtxt prag) $
    do  { warnIf (not (isOverloadedTy poly_ty || isInlinePragma inl)) $
                 TcRnNonOverloadedSpecialisePragma fun_name
                    -- Note [SPECIALISE pragmas]
        ; spec_prags <- mapM tc_one hs_tys
        ; traceTc "tcSpecPrag" (ppr poly_id $$ nest 2 (vcat (map ppr spec_prags)))
        ; return spec_prags }
  where
    name      = idName poly_id
    poly_ty   = idType poly_id

    tc_one hs_ty
      = do { spec_ty <- tcHsSigType   (FunSigCtxt name NoRRC) hs_ty
           ; wrap    <- tcSpecWrapper (FunSigCtxt name (lhsSigTypeContextSpan hs_ty)) poly_ty spec_ty
           ; return (SpecPrag poly_id wrap inl) }

tcSpecPrag poly_id (SpecSigE nm rule_bndrs spec_e inl)
  -- For running commentary, see Note [Handling new-form SPECIALISE pragmas]
  = do { -- (1) Typecheck the expression, spec_e, capturing its constraints
         let skol_info_anon = SpecESkol nm
       ; traceTc "tcSpecPrag SpecSigE {" (ppr nm $$ ppr spec_e)
       ; skol_info <- mkSkolemInfo skol_info_anon
       ; (rhs_tclvl, spec_e_wanted, (rule_bndrs', (tc_spec_e, _rho)))
            <- tcRuleBndrs skol_info rule_bndrs $
               tcInferRho spec_e

         -- (2) Solve the resulting wanteds in TcSSpecPrag mode.
       ; ev_binds_var <- newTcEvBinds
       ; spec_e_wanted <- setTcLevel rhs_tclvl $
                          runTcSSpecPrag ev_binds_var $
                          solveWanteds spec_e_wanted
       ; spec_e_wanted <- liftZonkM $ zonkWC spec_e_wanted

         -- (3) Compute which constraints to quantify over, by looking
         --     at the unsolved constraints from (2)
       ; (quant_cands, residual_wc) <- getRuleQuantCts spec_e_wanted

         -- (4) Emit the residual constraints (i.e. ones that we have
         --     not solved in (2) nor quantified in (3)
         -- NB: use the same `ev_binds_var` as (2), so the bindings
         --     for (2) and (4) are combined
       ; let tv_bndrs = filter isTyVar rule_bndrs'
             qevs = map ctEvId (bagToList quant_cands)
       ; emitResidualConstraints rhs_tclvl skol_info_anon ev_binds_var
                                 emptyVarSet tv_bndrs qevs
                                 residual_wc

         -- (5) Wrap the call in the combined evidence bindings
         --     from steps (2) and (4)
       ; let lhs_call = mkLHsWrap (WpLet (TcEvBinds ev_binds_var)) tc_spec_e

       ; ev_binds <- getTcEvBindsMap ev_binds_var

       ; traceTc "tcSpecPrag SpecSigE }" $
         vcat [ text "nm:" <+> ppr nm
              , text "rule_bndrs':" <+> ppr rule_bndrs'
              , text "qevs:" <+> ppr qevs
              , text "spec_e:" <+> ppr tc_spec_e
              , text "inl:" <+> ppr inl
              , text "spec_e_wanted:" <+> ppr spec_e_wanted
              , text "quant_cands:" <+> ppr quant_cands
              , text "residual_wc:" <+> ppr residual_wc
              , text (replicate 80 '-')
              , text "ev_binds_var:" <+> ppr ev_binds_var
              , text "ev_binds:" <+> ppr ev_binds
              ]

         -- (6) Store the results in a SpecPragE record, which will be
         -- zonked and then consumed by the desugarer.

       ; return [SpecPragE { spe_fn_nm = nm
                           , spe_fn_id = poly_id
                           , spe_bndrs = qevs ++ rule_bndrs' -- Dependency order
                                                             -- does not matter
                           , spe_call  = lhs_call
                           , spe_inl   = inl }] }

tcSpecPrag _ prag = pprPanic "tcSpecPrag" (ppr prag)

--------------
tcSpecWrapper :: UserTypeCtxt -> TcType -> TcType -> TcM HsWrapper
-- A simpler variant of tcSubType, used for SPECIALISE pragmas
-- See Note [Handling old-form SPECIALISE pragmas], wrinkle 1
tcSpecWrapper ctxt poly_ty spec_ty
  = do { (sk_wrap, inst_wrap)
               <- tcSkolemise Shallow ctxt spec_ty $ \spec_tau ->
                  do { (inst_wrap, tau) <- topInstantiate orig poly_ty
                     ; _ <- unifyType Nothing spec_tau tau
                            -- Deliberately ignore the evidence
                            -- See Note [Handling old-form SPECIALISE pragmas],
                            --   wrinkle (2)
                     ; return inst_wrap }
       ; return (sk_wrap <.> inst_wrap) }
  where
    orig = SpecPragOrigin ctxt

--------------
tcImpPrags :: [LSig GhcRn] -> TcM [LTcSpecPrag]
-- SPECIALISE pragmas for imported things
tcImpPrags prags
  = do { dflags <- getDynFlags
       ; traceTc "tcImpPrags1" (ppr prags)
       ; if (not_specialising dflags) then
            return []
         else do
            { this_mod <- getModule
            ; pss <- mapAndRecoverM (wrapLocMA (tcImpSpec this_mod)) prags
            ; return $ concatMap (\(L l ps) -> map (L (locA l)) ps) pss } }
  where
    -- Ignore SPECIALISE pragmas for imported things
    -- when we aren't specialising, or when we aren't generating
    -- code.  The latter happens when Haddocking the base library;
    -- we don't want complaints about lack of INLINABLE pragmas
    not_specialising dflags =
      not (gopt Opt_Specialise dflags) || not (backendRespectsSpecialise (backend dflags))

tcImpSpec :: Module -> Sig GhcRn -> TcM [TcSpecPrag]
tcImpSpec this_mod prag
 | Just name <- is_spec_prag prag         -- It's a specialisation pragma
 , not (nameIsLocalOrFrom this_mod name)  -- The Id is imported
 = do { id <- tcLookupId name
      ; if hasSomeUnfolding (realIdUnfolding id)
           -- See Note [SPECIALISE pragmas for imported Ids]
        then tcSpecPrag id prag
        else do { let dia = TcRnSpecialiseNotVisible name
                ; addDiagnosticTc dia
                ; return [] } }
  | otherwise
  = return []
  where
    is_spec_prag (SpecSig _ (L _ nm) _ _) = Just nm
    is_spec_prag (SpecSigE nm _ _ _)      = Just nm
    is_spec_prag _                        = Nothing

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


{- *********************************************************************
*                                                                      *
                   Rules
*                                                                      *
************************************************************************

Note [Typechecking rules]
~~~~~~~~~~~~~~~~~~~~~~~~~
We *infer* the type of the LHS, and use that type to *check* the type of
the RHS.  That means that higher-rank rules work reasonably well. Here's
an example (test simplCore/should_compile/rule2.hs) produced by Roman:

   foo :: (forall m. m a -> m b) -> m a -> m b
   foo f = ...

   bar :: (forall m. m a -> m a) -> m a -> m a
   bar f = ...

   {-# RULES "foo/bar" foo = bar #-}

He wanted the rule to typecheck.

Note [TcLevel in type checking rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Bringing type variables into scope naturally bumps the TcLevel. Thus, we type
check the term-level binders in a bumped level, and we must accordingly bump
the level whenever these binders are in scope.

Note [Re-quantify type variables in rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example from #17710:

  foo :: forall k (a :: k) (b :: k). Proxy a -> Proxy b
  foo x = Proxy
  {-# RULES "foo" forall (x :: Proxy (a :: k)). foo x = Proxy #-}

Written out in more detail, the "foo" rewrite rule looks like this:

  forall k (a :: k). forall (x :: Proxy (a :: k)). foo @k @a @b0 x = Proxy @k @b0

Where b0 is a unification variable. Where should b0 be quantified? We have to
quantify it after k, since (b0 :: k). But generalization usually puts inferred
type variables (such as b0) at the /front/ of the telescope! This creates a
conflict.

One option is to simply throw an error, per the principles of
Note [Naughty quantification candidates] in GHC.Tc.Utils.TcMType. This is what would happen
if we were generalising over a normal type signature. On the other hand, the
types in a rewrite rule aren't quite "normal", since the notions of specified
and inferred type variables aren't applicable.

A more permissive design (and the design that GHC uses) is to simply requantify
all of the type variables. That is, we would end up with this:

  forall k (a :: k) (b :: k). forall (x :: Proxy (a :: k)). foo @k @a @b x = Proxy @k @b

It's a bit strange putting the generalized variable `b` after the user-written
variables `k` and `a`. But again, the notion of specificity is not relevant to
rewrite rules, since one cannot "visibly apply" a rewrite rule. This design not
only makes "foo" typecheck, but it also makes the implementation simpler.

See also Note [Generalising in tcTyFamInstEqnGuts] in GHC.Tc.TyCl, which
explains a very similar design when generalising over a type family instance
equation.
-}

tcRules :: [LRuleDecls GhcRn] -> TcM [LRuleDecls GhcTc]
tcRules decls = mapM (wrapLocMA tcRuleDecls) decls

tcRuleDecls :: RuleDecls GhcRn -> TcM (RuleDecls GhcTc)
tcRuleDecls (HsRules { rds_ext = src
                     , rds_rules = decls })
   = do { maybe_tc_decls <- mapM (wrapLocMA tcRule) decls
        ; let tc_decls = [L loc rule | (L loc (Just rule)) <- maybe_tc_decls]
        ; return $ HsRules { rds_ext   = src
                           , rds_rules = tc_decls } }

tcRule :: RuleDecl GhcRn -> TcM (Maybe (RuleDecl GhcTc))
tcRule (HsRule { rd_ext  = ext
               , rd_name = rname@(L _ name)
               , rd_act  = act
               , rd_bndrs = bndrs
               , rd_lhs  = lhs
               , rd_rhs  = rhs })
  = addErrCtxt (RuleCtxt name)  $
    do { traceTc "---- Rule ------" (pprFullRuleName (snd ext) rname)
       ; skol_info <- mkSkolemInfo (RuleSkol name)
        -- Note [Typechecking rules]
       ; (tc_lvl, lhs_wanted, stuff)
              <- tcRuleBndrs skol_info bndrs $
                 do { (lhs', rule_ty)    <- tcInferRho lhs
                    ; (rhs', rhs_wanted) <- captureConstraints $
                                            tcCheckMonoExpr rhs rule_ty
                    ; return (lhs', rule_ty, rhs', rhs_wanted) }

       ; let (bndrs', (lhs', rule_ty, rhs', rhs_wanted)) = stuff

       ; traceTc "tcRule 1" (vcat [ pprFullRuleName (snd ext) rname
                                  , ppr lhs_wanted
                                  , ppr rhs_wanted ])

       ; (lhs_evs, residual_lhs_wanted, dont_default)
            <- simplifyRule name tc_lvl lhs_wanted rhs_wanted

       -- SimplifyRule Plan, step 4
       -- Now figure out what to quantify over
       -- c.f. GHC.Tc.Solver.simplifyInfer
       -- We quantify over any tyvars free in *either* the rule
       --  *or* the bound variables.  The latter is important.  Consider
       --      ss (x,(y,z)) = (x,z)
       --      RULE:  forall v. fst (ss v) = fst v
       -- The type of the rhs of the rule is just a, but v::(a,(b,c))
       --
       -- We also need to get the completely-unconstrained tyvars of
       -- the LHS, lest they otherwise get defaulted to Any; but we do that
       -- during zonking (see GHC.Tc.Zonk.Type.zonkRule)

       ; let tpl_ids = lhs_evs ++ filter isId bndrs'

       -- See Note [Re-quantify type variables in rules]
       ; dvs <- candidateQTyVarsOfTypes (rule_ty : map idType tpl_ids)
       ; let weed_out = (`dVarSetMinusVarSet` dont_default)
             weeded_dvs = weedOutCandidates weed_out dvs
       ; qtkvs <- quantifyTyVars skol_info weeded_dvs
       ; traceTc "tcRule" (vcat [ pprFullRuleName (snd ext) rname
                                , text "dvs:" <+> ppr dvs
                                , text "weeded_dvs:" <+> ppr weeded_dvs
                                , text "dont_default:" <+> ppr dont_default
                                , text "residual_lhs_wanted:" <+> ppr residual_lhs_wanted
                                , text "qtkvs:" <+> ppr qtkvs
                                , text "rule_ty:" <+> ppr rule_ty
                                , text "bndrs:" <+> ppr bndrs
                                , text "qtkvs ++ tpl_ids:" <+> ppr (qtkvs ++ tpl_ids)
                                , text "tpl_id info:" <+>
                                  vcat [ ppr id <+> dcolon <+> ppr (idType id) | id <- tpl_ids ]
                  ])

       -- /Temporarily/ deal with the fact that we previously accepted
       -- rules that quantify over certain equality constraints.
       --
       -- See Note [Quantifying over equalities in RULES].
       ; case allPreviouslyQuantifiableEqualities residual_lhs_wanted of {
           Just cts | not (insolubleWC rhs_wanted)
                    -> do { addDiagnostic $ TcRnRuleLhsEqualities name lhs cts
                          ; return Nothing } ;
           _  ->

   do  { -- SimplifyRule Plan, step 5
       -- Simplify the LHS and RHS constraints:
       -- For the LHS constraints we must solve the remaining constraints
       -- (a) so that we report insoluble ones
       -- (b) so that we bind any soluble ones
         (lhs_implic, lhs_binds) <- buildImplicationFor tc_lvl (getSkolemInfo skol_info) qtkvs
                                         lhs_evs residual_lhs_wanted
       ; (rhs_implic, rhs_binds) <- buildImplicationFor tc_lvl (getSkolemInfo skol_info) qtkvs
                                         lhs_evs rhs_wanted

       ; emitImplications (lhs_implic `unionBags` rhs_implic)

       ; return $ Just $
         HsRule { rd_ext   = ext
                , rd_name  = rname
                , rd_act   = act
                , rd_bndrs = bndrs { rb_ext = qtkvs ++ tpl_ids }
                , rd_lhs   = mkHsDictLet lhs_binds lhs'
                , rd_rhs   = mkHsDictLet rhs_binds rhs' } } } }

{- ********************************************************************************
*                                                                                 *
                      tcRuleBndrs
*                                                                                 *
******************************************************************************** -}

tcRuleBndrs :: SkolemInfo -> RuleBndrs GhcRn
            -> TcM a      -- Typecheck this with the rule binders in scope
            -> TcM (TcLevel, WantedConstraints, ([Var], a))
                        -- The [Var] are the explicitly-quantified variables,
                        -- both type variables and term variables
tcRuleBndrs skol_info (RuleBndrs { rb_tyvs = mb_tv_bndrs, rb_tmvs = tm_bndrs })
            thing_inside
  = pushLevelAndCaptureConstraints $
    case mb_tv_bndrs of
      Nothing       ->  go_tms tm_bndrs thing_inside
      Just tv_bndrs -> do { (bndrs1, (bndrs2, res)) <- go_tvs tv_bndrs $
                                                       go_tms tm_bndrs $
                                                       thing_inside
                          ; return (binderVars bndrs1 ++ bndrs2, res) }
  where
    --------------
    go_tvs tvs thing_inside = bindExplicitTKBndrs_Skol skol_info tvs thing_inside

    --------------
    go_tms [] thing_inside
      = do { res <- thing_inside; return ([], res) }
    go_tms (L _ (RuleBndr _ (L _ name)) : rule_bndrs) thing_inside
      = do  { ty <- newOpenFlexiTyVarTy
            ; let bndr_id = mkLocalId name ManyTy ty
            ; (bndrs, res) <- tcExtendIdEnv [bndr_id] $
                              go_tms rule_bndrs thing_inside
            ; return (bndr_id : bndrs, res) }

    go_tms (L _ (RuleBndrSig _ (L _ name) rn_ty) : rule_bndrs) thing_inside
      --  e.g         x :: a->a
      --  The tyvar 'a' is brought into scope first, just as if you'd written
      --              a::*, x :: a->a
      --  If there's an explicit forall, the renamer would have already reported an
      --   error for each out-of-scope type variable used
      = do  { (_ , tv_prs, id_ty) <- tcRuleBndrSig name skol_info rn_ty
            ; let bndr_id  = mkLocalId name ManyTy id_ty
                     -- See Note [Typechecking pattern signature binders] in GHC.Tc.Gen.HsType

                     -- The type variables scope over subsequent bindings; yuk
            ; (bndrs, res) <- tcExtendNameTyVarEnv tv_prs $
                              tcExtendIdEnv [bndr_id]     $
                              go_tms rule_bndrs thing_inside
            ; return (map snd tv_prs ++ bndr_id : bndrs, res) }


{-
*********************************************************************************
*                                                                                 *
              Constraint simplification for rules
*                                                                                 *
***********************************************************************************

Note [The SimplifyRule Plan]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Example.  Consider the following left-hand side of a rule
        f (x == y) (y > z) = ...
If we typecheck this expression we get constraints
        d1 :: Ord a, d2 :: Eq a
We do NOT want to "simplify" to the LHS
        forall x::a, y::a, z::a, d1::Ord a.
          f ((==) (eqFromOrd d1) x y) ((>) d1 y z) = ...
Instead we want
        forall x::a, y::a, z::a, d1::Ord a, d2::Eq a.
          f ((==) d2 x y) ((>) d1 y z) = ...

Here is another example:
        fromIntegral :: (Integral a, Num b) => a -> b
        {-# RULES "foo"  fromIntegral = id :: Int -> Int #-}
In the rule, a=b=Int, and Num Int is a superclass of Integral Int. But
we *dont* want to get
        forall dIntegralInt.
           fromIntegral Int Int dIntegralInt (scsel dIntegralInt) = id Int
because the scsel will mess up RULE matching.  Instead we want
        forall dIntegralInt, dNumInt.
          fromIntegral Int Int dIntegralInt dNumInt = id Int

Even if we have
        g (x == y) (y == z) = ..
where the two dictionaries are *identical*, we do NOT WANT
        forall x::a, y::a, z::a, d1::Eq a
          f ((==) d1 x y) ((>) d1 y z) = ...
because that will only match if the dict args are (visibly) equal.
Instead we want to quantify over the dictionaries separately.

In short, simplifyRuleLhs must *only* squash equalities, leaving
all dicts unchanged, with absolutely no sharing.

Also note that we can't solve the LHS constraints in isolation:
Example   foo :: Ord a => a -> a
          foo_spec :: Int -> Int
          {-# RULE "foo"  foo = foo_spec #-}
Here, it's the RHS that fixes the type variable

HOWEVER, under a nested implication things are different
Consider
  f :: (forall a. Eq a => a->a) -> Bool -> ...
  {-# RULES "foo" forall (v::forall b. Eq b => b->b).
       f b True = ...
    #-}
Here we *must* solve the wanted (Eq a) from the given (Eq a)
resulting from skolemising the argument type of g.  So we
revert to SimplCheck when going under an implication.


--------- So the SimplifyRule Plan is this -----------------------

* Step 0: typecheck the LHS and RHS to get constraints from each

* Step 1: Simplify the LHS and RHS constraints all together in one bag,
          but /discarding/ the simplified constraints. We do this only
          to discover all unification equalities.

* Step 2: Zonk the ORIGINAL (unsimplified) LHS constraints, to take
          advantage of those unifications

* Step 3: Partition the LHS constraints into the ones we will
          quantify over, and the others.
          See Note [RULE quantification over equalities]

* Step 4: Decide on the type variables to quantify over

* Step 5: Simplify the LHS and RHS constraints separately, using the
          quantified constraints as givens

Note [Solve order for RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In step 1 above, we need to be a bit careful about solve order.
Consider
   f :: Int -> T Int
   type instance T Int = Bool

   RULE f 3 = True

From the RULE we get
   lhs-constraints:  T Int ~ alpha
   rhs-constraints:  Bool ~ alpha
where 'alpha' is the type that connects the two.  If we glom them
all together, and solve the RHS constraint first, we might solve
with alpha := Bool.  But then we'd end up with a RULE like

    RULE: f 3 |> (co :: T Int ~ Bool) = True

which is terrible.  We want

    RULE: f 3 = True |> (sym co :: Bool ~ T Int)

So we are careful to solve the LHS constraints first, and *then* the
RHS constraints.  Actually much of this is done by the on-the-fly
constraint solving, so the same order must be observed in
tcRule.

Note [RULE quantification over equalities]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At the moment a RULE never quantifies over an equality; see `rule_quant_ct`
in `getRuleQuantCts`.  Why not?

 * It's not clear why we would want to do so (see Historical Note
   below)

 * We do not want to quantify over insoluble equalities (Int ~ Bool)
    (a) because we prefer to report a LHS type error
    (b) because if such things end up in 'givens' we get a bogus
        "inaccessible code" error

 * Matching on coercions is Deeply Suspicious.  We don't want to generate a
   RULE like
         forall a (co :: F a ~ Int).
                foo (x |> Sym co) = ...co...
   because matching on that template, to bind `co`, would require us to
   match on the /structure/ of a coercion, which we must never do.
   See GHC.Core.Rules Note [Casts in the template]

 * Equality constraints are unboxed, and that leads to complications
   For example equality constraints from the LHS will emit coercion hole
   Wanteds.  These don't have a name, so we can't quantify over them directly.
   Instead, in `getRuleQuantCts`, we'd have to invent a new EvVar for the
   coercion, fill the hole with the invented EvVar, and then quantify over the
   EvVar. Here is old code from `mk_one`
         do { ev_id <- newEvVar pred
            ; fillCoercionHole hole (mkCoVarCo ev_id)
            ; return ev_id }
    But that led to new complications becuase of the side effect on the coercion
    hole. Much easier just to side-step the issue entirely by not quantifying over
    equalities.

Historical Note:
  Back in 2012 (5aa1ae24567) we started quantifying over some equality
  constraints, saying
   * But we do want to quantify over things like (a ~ F b),
     where F is a type function.
  It is not clear /why/ we did so, and we don't do so any longer.
End of historical note.

Note [Simplify cloned constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At this stage, we're simplifying constraints only for insolubility
and for unification. Note that all the evidence is quickly discarded.
We use a clone of the real constraint. If we don't do this,
then RHS coercion-hole constraints get filled in, only to get filled
in *again* when solving the implications emitted from tcRule. That's
terrible, so we avoid the problem by cloning the constraints.

-}

simplifyRule :: RuleName
             -> TcLevel                 -- Level at which to solve the constraints
             -> WantedConstraints       -- Constraints from LHS
             -> WantedConstraints       -- Constraints from RHS
             -> TcM ( [EvVar]               -- Quantify over these LHS vars
                    , WantedConstraints     -- Residual un-quantified LHS constraints
                    , TcTyVarSet )          -- Don't default these
-- See Note [The SimplifyRule Plan]
-- NB: This consumes all simple constraints on the LHS, but not
-- any LHS implication constraints.
simplifyRule name tc_lvl lhs_wanted rhs_wanted
  = do {
       -- Note [The SimplifyRule Plan] step 1
       -- First solve the LHS and *then* solve the RHS
       -- Crucially, this performs unifications
       -- Why clone?  See Note [Simplify cloned constraints]
       ; lhs_clone <- cloneWC lhs_wanted
       ; rhs_clone <- cloneWC rhs_wanted
       ; (dont_default, _)
            <- setTcLevel tc_lvl $
               runTcS            $
               do { lhs_wc  <- solveWanteds lhs_clone
                  ; _rhs_wc <- solveWanteds rhs_clone
                        -- Why do them separately?
                        -- See Note [Solve order for RULES]

                  ; let dont_default = nonDefaultableTyVarsOfWC lhs_wc
                        -- If lhs_wanteds has
                        --   (a[sk] :: TYPE rr[sk]) ~ (b0[tau] :: TYPE r0[conc])
                        -- we want r0 to be non-defaultable;
                        -- see nonDefaultableTyVarsOfWC.  Simplest way to get
                        -- this is to look at the post-simplified lhs_wc, which
                        -- will contain (rr[sk] ~ r0[conc)].  An example is in
                        -- test rep-poly/RepPolyRule1
                  ; return dont_default }

       -- Note [The SimplifyRule Plan] step 2
       ; lhs_wanted <- liftZonkM $ zonkWC lhs_wanted

       -- Note [The SimplifyRule Plan] step 3
       ; (quant_cts, residual_lhs_wanted) <- getRuleQuantCts lhs_wanted
       ; let quant_evs = map ctEvId (bagToList quant_cts)

       ; traceTc "simplifyRule" $
         vcat [ text "LHS of rule" <+> doubleQuotes (ftext name)
              , text "lhs_wanted" <+> ppr lhs_wanted
              , text "rhs_wanted" <+> ppr rhs_wanted
              , text "quant_cts" <+> ppr quant_evs
              , text "residual_lhs_wanted" <+> ppr residual_lhs_wanted
              , text "dont_default" <+> ppr dont_default
              ]

       ; return (quant_evs, residual_lhs_wanted, dont_default) }

getRuleQuantCts :: WantedConstraints -> TcM (Cts, WantedConstraints)
-- Extract all the constraints that we can quantify over,
--   also returning the depleted WantedConstraints
--
-- Unlike simplifyInfer, we don't leave the WantedConstraints unchanged,
--   and attempt to solve them from the quantified constraints.  Instead
--   we /partition/ the WantedConstraints into ones to quantify and ones
--   we can't quantify.  We could use approximateWC instead, and leave
--   `wanted` unchanged; but then we'd have to clone fresh binders and
--   generate silly identity bindings.  Seems more direct to do this.
--   Probably not a big deal wither way.
--
-- NB: we must look inside implications, because with
--     -fdefer-type-errors we generate implications rather eagerly;
--     see GHC.Tc.Utils.Unify.implicationNeeded. Not doing so caused #14732.

getRuleQuantCts wc
  = return $ float_wc emptyVarSet wc
  where
    float_wc :: TcTyCoVarSet -> WantedConstraints -> (Cts, WantedConstraints)
    float_wc skol_tvs (WC { wc_simple = simples, wc_impl = implics, wc_errors = errs })
      = ( simple_yes `andCts` implic_yes
        , emptyWC { wc_simple = simple_no, wc_impl = implics_no, wc_errors = errs })
     where
        (simple_yes, simple_no)  = partitionBag (rule_quant_ct skol_tvs) simples
        (implic_yes, implics_no) = mapAccumBagL (float_implic skol_tvs)  emptyBag implics

    float_implic :: TcTyCoVarSet -> Cts -> Implication -> (Cts, Implication)
    float_implic skol_tvs yes1 imp
      = (yes1 `andCts` yes2, imp { ic_wanted = no })
      where
        (yes2, no) = float_wc new_skol_tvs (ic_wanted imp)
        new_skol_tvs = skol_tvs `extendVarSetList` ic_skols imp

    rule_quant_ct :: TcTyCoVarSet -> Ct -> Bool
    rule_quant_ct skol_tvs ct
      | insolubleWantedCt ct
      = False
      | otherwise
      = case classifyPredType (ctPred ct) of
           EqPred {} -> False  -- Note [RULE quantification over equalities]
           _         -> tyCoVarsOfCt ct `disjointVarSet` skol_tvs

{- Note [Quantifying over equalities in RULES]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Up until version 9.12 (inclusive), GHC would happily quantify over certain Wanted
equalities in the LHS of a RULE. This was incorrect behaviour that led to a RULE
that would never fire, so GHC 9.14 and above no longer allow such RULES.
However, instead of throwing an error, GHC will /temporarily/ emit a warning
and drop the rule instead, in order to ease migration for library maintainers
(NB: this warning is not emitted when the RHS constraints are insoluble; in that
case we simply report those constraints as errors instead).
This warning is scheduled to be turned into an error, and the warning flag
removed (becoming a normal typechecker error), starting from version 9.18.

The function 'allPreviouslyQuantifiableEqualities' computes the equality
constraints that previous (<= 9.12) versions of GHC accepted quantifying over.


  Example (test case 'RuleEqs', extracted from the 'mono-traversable' library):

    type family Element mono
    type instance Element [a] = a

    class MonoFoldable mono where
        otoList :: mono -> [Element mono]
    instance MonoFoldable [a] where
        otoList = id

    ointercalate :: (MonoFoldable mono, Monoid (Element mono))
                 => Element mono -> mono -> Element mono
    {-# RULES "ointercalate list" forall x. ointercalate x = Data.List.intercalate x . otoList #-}

  Now, because Data.List.intercalate has the type signature

    forall a. [a] -> [[a]] -> [a]

  typechecking the LHS of this rule would give rise to the Wanted equality

    [W] Element mono ~ [a]

  Due to the type family, GHC 9.12 and below accepted to quantify over this
  equality, which would lead to a rule LHS template of the form:

    forall (@mono) (@a)
           ($dMonoFoldable :: MonoFoldable mono)
           ($dMonoid :: Monoid (Element mono))
           (co :: [a] ~ Element mono)
           (x :: [a]).
      ointercalate @mono $dMonoFoldable $dMonoid
        (x `cast` (Sub co))

  Matching against this template would match on the structure of a coercion,
  which goes against Note [Casts in the template] in GHC.Core.Rules.
  In practice, this meant that this RULE would never fire.
-}

-- | Computes all equality constraints that GHC doesn't accept, but previously
-- did accept (until GHC 9.12 (included)), when deciding what to quantify over
-- in the LHS of a RULE.
--
-- See Note [Quantifying over equalities in RULES].
allPreviouslyQuantifiableEqualities :: WantedConstraints -> Maybe (NE.NonEmpty Ct)
allPreviouslyQuantifiableEqualities wc = go emptyVarSet wc
  where
    go :: TyVarSet -> WantedConstraints -> Maybe (NE.NonEmpty Ct)
    go skol_tvs (WC { wc_simple = simples, wc_impl = implics })
      = do { cts1 <-       mapM (go_simple skol_tvs) simples
           ; cts2 <- concatMapM (go_implic skol_tvs) implics
           ; NE.nonEmpty $ toList cts1 ++ toList cts2 }

    go_simple :: TyVarSet -> Ct -> Maybe Ct
    go_simple skol_tvs ct
      | not (tyCoVarsOfCt ct `disjointVarSet` skol_tvs)
      = Nothing
      | EqPred _ t1 t2 <- classifyPredType (ctPred ct), ok_eq t1 t2
      = Just ct
      | otherwise
      = Nothing

    go_implic :: TyVarSet -> Implication -> Maybe [Ct]
    go_implic skol_tvs (Implic { ic_skols = skols, ic_wanted = wc })
      = fmap toList $ go (skol_tvs `extendVarSetList` skols) wc

    ok_eq t1 t2
       | t1 `tcEqType` t2 = False
       | otherwise        = is_fun_app t1 || is_fun_app t2

    is_fun_app ty   -- ty is of form (F tys) where F is a type function
      = case tyConAppTyCon_maybe ty of
          Just tc -> isTypeFamilyTyCon tc
          Nothing -> False
