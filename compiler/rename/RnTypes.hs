{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[RnSource]{Main pass of renamer}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RnTypes (
        -- Type related stuff
        rnHsType, rnLHsType, rnLHsTypes, rnContext,
        rnHsKind, rnLHsKind, rnLHsMaybeKind,
        rnHsSigType, rnHsWcType,
        rnHsSigWcType, rnHsSigWcTypeScoped,
        rnLHsInstType,
        newTyVarNameRn, collectAnonWildCards,
        rnConDeclFields,
        rnLTyVar, rnLHsTyVarBndr,

        -- Precence related stuff
        mkOpAppRn, mkNegAppRn, mkOpFormRn, mkConOpPatRn,
        checkPrecMatch, checkSectionPrec,

        -- Binding related stuff
        warnUnusedForAlls,
        bindSigTyVarsFV, bindHsQTyVars,
        extractHsTyRdrTyVars, extractHsTysRdrTyVars,
        extractRdrKindSigVars, extractDataDefnKindVars
  ) where

import {-# SOURCE #-} RnSplice( rnSpliceType )

import DynFlags
import HsSyn
import RnHsDoc          ( rnLHsDoc, rnMbLHsDoc )
import RnEnv
import TcRnMonad
import RdrName
import PrelNames        ( negateName, dot_tv_RDR, forall_tv_RDR )
import TysPrim          ( funTyConName )
import Name
import SrcLoc
import NameSet
import FieldLabel

import Util
import BasicTypes       ( compareFixity, funTyFixity, negateFixity,
                          Fixity(..), FixityDirection(..) )
import Outputable
import FastString
import Maybes
import Data.List        ( nub, nubBy )
import Control.Monad    ( unless, when )

#if __GLASGOW_HASKELL__ < 709
import Data.Monoid      ( mappend, mempty, mconcat )
#endif

#include "HsVersions.h"

{-
These type renamers are in a separate module, rather than in (say) RnSource,
to break several loop.

*********************************************************
*                                                       *
           HsSigWcType (i.e with wildcards)
*                                                       *
*********************************************************
-}

rnHsSigWcType :: HsDocContext -> LHsSigWcType RdrName
            -> RnM (LHsSigWcType Name, FreeVars)
rnHsSigWcType doc sig_ty
  = rn_hs_sig_wc_type True doc sig_ty $ \sig_ty' ->
    return (sig_ty', emptyFVs)

rnHsSigWcTypeScoped :: HsDocContext -> LHsSigWcType RdrName
                    -> (LHsSigWcType Name -> RnM (a, FreeVars))
                    -> RnM (a, FreeVars)
-- Used for
--   - Signatures on binders in a RULE
--   - Pattern type signatures
-- Wildcards are allowed
rnHsSigWcTypeScoped ctx sig_ty thing_inside
  = rn_hs_sig_wc_type False ctx sig_ty thing_inside
    -- False: for pattern type sigs and rules we /do/ want
    --        to bring those type varibles into scope
    -- e.g  \ (x :: forall a. a-> b) -> e
    -- Here we do bring 'b' into scope

rn_hs_sig_wc_type :: Bool   -- see rnImplicitBndrs
                  -> HsDocContext
                  -> LHsSigWcType RdrName
                  -> (LHsSigWcType Name -> RnM (a, FreeVars))
                  -> RnM (a, FreeVars)
-- rn_hs_sig_wc_type is used for source-language type signatures
rn_hs_sig_wc_type no_implicit_if_forall ctxt
                  (HsIB { hsib_body = wc_ty }) thing_inside
  = rnImplicitBndrs no_implicit_if_forall (hswc_body wc_ty) $ \ kvs tvs ->
    rn_hs_wc_type ctxt wc_ty $ \ wc_ty' ->
    thing_inside (HsIB { hsib_kvs  = kvs
                       , hsib_tvs  = tvs
                       , hsib_body = wc_ty' })

rnHsWcType :: HsDocContext -> LHsWcType RdrName -> RnM (LHsWcType Name, FreeVars)
rnHsWcType ctxt wc_ty
  = rn_hs_wc_type ctxt wc_ty $ \ wc_ty' ->
    return (wc_ty', emptyFVs)

rn_hs_wc_type :: HsDocContext -> LHsWcType RdrName
              -> (LHsWcType Name -> RnM (a, FreeVars))
              -> RnM (a, FreeVars)
rn_hs_wc_type ctxt (HsWC { hswc_body = hs_ty }) thing_inside
  = do { let nwc_rdrs = collectNamedWildCards hs_ty
       ; rdr_env <- getLocalRdrEnv
       ; nwcs <- sequence [ newLocalBndrRn lrdr
                          | lrdr@(L _ rdr) <- nwc_rdrs
                          , not (inScope rdr_env rdr) ]
                 -- nwcs :: [Name]   Named wildcards
       ; bindLocalNamesFV nwcs $
    do { (wc_ty, fvs1) <- rnWcSigTy ctxt hs_ty
       ; let wc_ty' = wc_ty { hswc_wcs = nwcs ++ hswc_wcs wc_ty }
       ; (res, fvs2) <- thing_inside wc_ty'
       ; return (res, fvs1 `plusFV` fvs2) } }

rnWcSigTy :: HsDocContext -> LHsType RdrName
          -> RnM (LHsWcType Name, FreeVars)
-- Renames just the top level of a type signature
-- It's exactly like rnHsTyKi, except that it uses rnWcSigContext
-- on a qualified type, and return info on any extra-constraints
-- wildcard.  Some code duplication, but no big deal.
rnWcSigTy ctxt (L loc hs_ty@(HsForAllTy { hst_bndrs = tvs, hst_body = hs_tau }))
  = bindLHsTyVarBndrs ctxt Nothing tvs $ \ tvs' ->
    do { (hs_tau', fvs) <- rnWcSigTy ctxt hs_tau
       ; warnUnusedForAlls (inTypeDoc hs_ty) tvs' fvs
       ; let hs_ty' = HsForAllTy { hst_bndrs = tvs', hst_body = hswc_body hs_tau' }
       ; return ( hs_tau' { hswc_body = L loc hs_ty' }, fvs) }

rnWcSigTy ctxt (L loc (HsQualTy { hst_ctxt = hs_ctxt, hst_body = tau }))
  = do { (hs_ctxt', fvs1) <- rnWcSigContext ctxt hs_ctxt
       ; (tau',     fvs2) <- rnLHsType ctxt tau
       ; let awcs_tau = collectAnonWildCards tau'
             hs_ty'   = HsQualTy { hst_ctxt = hswc_body hs_ctxt'
                                 , hst_body = tau' }
       ; return ( HsWC { hswc_wcs = hswc_wcs hs_ctxt' ++ awcs_tau
                       , hswc_ctx = hswc_ctx hs_ctxt'
                       , hswc_body = L loc hs_ty' }
                , fvs1 `plusFV` fvs2) }

rnWcSigTy ctxt hs_ty
  = do { (hs_ty', fvs) <- rnLHsType ctxt hs_ty
       ; return (HsWC { hswc_wcs = collectAnonWildCards hs_ty'
                      , hswc_ctx = Nothing
                      , hswc_body = hs_ty' }
                , fvs) }

rnWcSigContext :: HsDocContext -> LHsContext RdrName
               -> RnM (HsWildCardBndrs Name (LHsContext Name), FreeVars)
rnWcSigContext ctxt (L loc hs_ctxt)
  | Just (hs_ctxt1, hs_ctxt_last) <- snocView hs_ctxt
  , L lx (HsWildCardTy wc) <- ignoreParens hs_ctxt_last
  = do { (hs_ctxt1', fvs) <- mapFvRn (rnLHsTyKi RnTopConstraint ctxt) hs_ctxt1
       ; wc'              <- setSrcSpan lx $
                             rnExtraConstraintWildCard ctxt wc
       ; let hs_ctxt' = hs_ctxt1' ++ [L lx (HsWildCardTy wc')]
             awcs     = concatMap collectAnonWildCards hs_ctxt1'
             -- NB: *not* including the extra-constraint wildcard
       ; return ( HsWC { hswc_wcs = awcs
                       , hswc_ctx = Just lx
                       , hswc_body = L loc hs_ctxt' }
                , fvs ) }
  | otherwise
  = do { (hs_ctxt', fvs) <- mapFvRn (rnLHsTyKi RnTopConstraint ctxt) hs_ctxt
       ; return (HsWC { hswc_wcs = concatMap collectAnonWildCards hs_ctxt'
                      , hswc_ctx = Nothing
                      , hswc_body = L loc hs_ctxt' }, fvs) }

{- Note [Error checkingp for wildcards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is how various wildcard-related errors are reported

* Named extra-constraints wild cards aren't allowed,
  e.g. invalid: @(Show a, _x) => a -> String@.

* There is only one extra-constraints wild card in the context and it must
  come last, e.g. invalid: @(_, Show a) => a -> String@
  or @(_, Show a, _) => a -> String@.

* There should be no unnamed wild cards in the context.

* An extra-constraints wild card can only occur in the top-level context.
  This would be invalid: @(Eq a, _) => a -> (Num a, _) => a -> Bool@.

* Named wild cards occurring in the context must also occur in the monotype.

When an invalid wild card is found, we fail with an error.

????? What about f :: (forall a. (Eq _) => a -> a -> Bool) -> Int
-}

{- ******************************************************
*                                                       *
           HsSigtype (i.e. no wildcards)
*                                                       *
****************************************************** -}

rnHsSigType :: HsDocContext -> LHsSigType RdrName
            -> RnM (LHsSigType Name, FreeVars)
-- Used for source-language type signatures
-- that cannot have wildcards
rnHsSigType ctx (HsIB { hsib_body = hs_ty })
  = rnImplicitBndrs True hs_ty $ \ kvs tvs ->
    do { (body', fvs) <- rnLHsType ctx hs_ty
       ; return (HsIB { hsib_kvs  = kvs
                      , hsib_tvs  = tvs
                      , hsib_body = body' }, fvs) }

rnImplicitBndrs :: Bool    -- True <=> no implicit quantification
                           --          if type is headed by a forall
                           -- E.g.  f :: forall a. a->b
                           -- Do not quantify over 'b' too.
                -> LHsType RdrName
                -> ([Name] -> [Name] -> RnM (a, FreeVars))
                -> RnM (a, FreeVars)
rnImplicitBndrs no_implicit_if_forall hs_ty@(L loc _) thing_inside
  = do { rdr_env <- getLocalRdrEnv
       ; let (kv_rdrs, tv_rdrs) = filterInScope rdr_env $
                                  extractHsTyRdrTyVars hs_ty
             real_tv_rdrs  -- Implicit quantification only if
                           -- there is no explicit forall
               | no_implicit_if_forall
               , L _ (HsForAllTy {}) <- hs_ty = []
               | otherwise                    = tv_rdrs
       ; traceRn (text "rnSigType" <+> (ppr hs_ty $$ ppr kv_rdrs $$ ppr tv_rdrs))
       ; kvs <- mapM (newLocalBndrRn . L loc) kv_rdrs
       ; tvs <- mapM (newLocalBndrRn . L loc) real_tv_rdrs
       ; bindLocalNamesFV (kvs ++ tvs) $
         thing_inside kvs tvs }

rnLHsInstType :: SDoc -> LHsSigType RdrName -> RnM (LHsSigType Name, FreeVars)
-- Rename the type in an instance or standalone deriving decl
-- The 'doc_str' is "an instance declaration" or "a VECTORISE pragma"
rnLHsInstType doc_str inst_ty
  | Just cls <- getLHsInstDeclClass_maybe inst_ty
  , isTcOcc (rdrNameOcc (unLoc cls))
         -- The guards check that the instance type looks like
         --   blah => C ty1 .. tyn
  = do { let full_doc = doc_str <+> ptext (sLit "for") <+> quotes (ppr cls)
       ; rnHsSigType (GenericCtx full_doc) inst_ty }

  | otherwise  -- The instance is malformed, but we'd still like
               -- to make progress rather than failing outright, so
               -- we report more errors.  So we rename it anyway.
  = do { addErrAt (getLoc (hsSigType inst_ty)) $
         ptext (sLit "Malformed instance:") <+> ppr inst_ty
       ; rnHsSigType (GenericCtx doc_str) inst_ty }


{- ******************************************************
*                                                       *
           LHsType and HsType
*                                                       *
****************************************************** -}

{-
rnHsType is here because we call it from loadInstDecl, and I didn't
want a gratuitous knot.

Note [Context quantification]
-----------------------------
Variables in type signatures are implicitly quantified
when (1) they are in a type signature not beginning
with "forall" or (2) in any qualified type T => R.
We are phasing out (2) since it leads to inconsistencies
(Trac #4426):

data A = A (a -> a)           is an error
data A = A (Eq a => a -> a)   binds "a"
data A = A (Eq a => a -> b)   binds "a" and "b"
data A = A (() => a -> b)     binds "a" and "b"
f :: forall a. a -> b         is an error
f :: forall a. () => a -> b   is an error
f :: forall a. a -> (() => b) binds "a" and "b"

The -fwarn-context-quantification flag warns about
this situation. See rnHsTyKi for case HsForAllTy Qualified.
-}

rnLHsTyKi  :: RnTyKiWhat
           -> HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnLHsTyKi what doc (L loc ty)
  = setSrcSpan loc $
    do { (ty', fvs) <- rnHsTyKi what doc ty
       ; return (L loc ty', fvs) }

rnLHsType  :: HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnLHsType = rnLHsTyKi RnType

rnLHsPred  :: HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnLHsPred = rnLHsTyKi RnConstraint

rnLHsKind  :: HsDocContext -> LHsKind RdrName -> RnM (LHsKind Name, FreeVars)
rnLHsKind = rnLHsTyKi RnKind

rnLHsMaybeKind  :: HsDocContext -> Maybe (LHsKind RdrName)
                -> RnM (Maybe (LHsKind Name), FreeVars)
rnLHsMaybeKind _ Nothing
  = return (Nothing, emptyFVs)
rnLHsMaybeKind doc (Just kind)
  = do { (kind', fvs) <- rnLHsKind doc kind
       ; return (Just kind', fvs) }

rnHsType  :: HsDocContext -> HsType RdrName -> RnM (HsType Name, FreeVars)
rnHsType = rnHsTyKi RnType

rnHsKind  :: HsDocContext -> HsKind RdrName -> RnM (HsKind Name, FreeVars)
rnHsKind = rnHsTyKi RnKind

data RnTyKiWhat = RnType
                | RnKind
                | RnTopConstraint  -- Top-level context of HsSigWcTypes
                | RnConstraint     -- All other constraints

instance Outputable RnTyKiWhat where
  ppr RnType          = ptext (sLit "RnType")
  ppr RnKind          = ptext (sLit "RnKind")
  ppr RnTopConstraint = ptext (sLit "RnTopConstraint")
  ppr RnConstraint    = ptext (sLit "RnConstraint")

isRnType :: RnTyKiWhat -> Bool
isRnType RnType = True
isRnType _      = False

isRnKind :: RnTyKiWhat -> Bool
isRnKind RnKind = True
isRnKind _      = False

rnHsTyKi :: RnTyKiWhat -> HsDocContext -> HsType RdrName -> RnM (HsType Name, FreeVars)

rnHsTyKi _ doc ty@(HsForAllTy { hst_bndrs = tyvars, hst_body  = tau })
  = bindLHsTyVarBndrs doc Nothing tyvars $ \ tyvars' ->
    do { (tau',  fvs) <- rnLHsType doc tau
       ; warnUnusedForAlls (inTypeDoc ty) tyvars' fvs
       ; return ( HsForAllTy { hst_bndrs = tyvars', hst_body =  tau' }
                , fvs) }

rnHsTyKi _ doc (HsQualTy { hst_ctxt = lctxt
                              , hst_body = tau })
  = do { (ctxt', fvs1) <- rnContext doc lctxt
       ; (tau',  fvs2) <- rnLHsType doc tau
       ; return (HsQualTy { hst_ctxt = ctxt', hst_body =  tau' }
                , fvs1 `plusFV` fvs2) }

rnHsTyKi what _ (HsTyVar rdr_name)
  = do { name <- rnTyVar what rdr_name
       ; return (HsTyVar name, unitFV name) }

-- If we see (forall a . ty), without foralls on, the forall will give
-- a sensible error message, but we don't want to complain about the dot too
-- Hence the jiggery pokery with ty1
rnHsTyKi what doc ty@(HsOpTy ty1 (wrapper, L loc op) ty2)
  = setSrcSpan loc $
    do  { ops_ok <- xoptM Opt_TypeOperators
        ; op' <- if ops_ok
                 then rnTyVar what op
                 else do { addErr (opTyErr op ty)
                         ; return (mkUnboundNameRdr op) }  -- Avoid double complaint
        ; let l_op' = L loc op'
        ; fix <- lookupTyFixityRn l_op'
        ; (ty1', fvs1) <- rnLHsTyKi what doc ty1
        ; (ty2', fvs2) <- rnLHsTyKi what doc ty2
        ; res_ty <- mkHsOpTyRn (\t1 t2 -> HsOpTy t1 (wrapper, l_op') t2)
                               op' fix ty1' ty2'
        ; return (res_ty, (fvs1 `plusFV` fvs2) `addOneFV` op') }

rnHsTyKi what doc (HsParTy ty)
  = do { (ty', fvs) <- rnLHsTyKi what doc ty
       ; return (HsParTy ty', fvs) }

rnHsTyKi _ doc (HsBangTy b ty)
  = do { (ty', fvs) <- rnLHsType doc ty
       ; return (HsBangTy b ty', fvs) }

rnHsTyKi _ doc ty@(HsRecTy flds)
  = do { addErr (hang (ptext (sLit "Record syntax is illegal here:"))
                    2 (ppr ty))
       ; (flds', fvs) <- rnConDeclFields [] doc flds
       ; return (HsRecTy flds', fvs) }

rnHsTyKi what doc (HsFunTy ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi what doc ty1
        -- Might find a for-all as the arg of a function type
       ; (ty2', fvs2) <- rnLHsTyKi what doc ty2
        -- Or as the result.  This happens when reading Prelude.hi
        -- when we find return :: forall m. Monad m -> forall a. a -> m a

        -- Check for fixity rearrangements
       ; res_ty <- if isRnType what
                   then mkHsOpTyRn HsFunTy funTyConName funTyFixity ty1' ty2'
                   else return (HsFunTy ty1' ty2')

       ; return (res_ty, fvs1 `plusFV` fvs2) }

rnHsTyKi what doc listTy@(HsListTy ty)
  = do { data_kinds <- xoptM Opt_DataKinds
       ; when (not data_kinds && isRnKind what)
              (addErr (dataKindsErr what listTy))
       ; (ty', fvs) <- rnLHsTyKi what doc ty
       ; return (HsListTy ty', fvs) }

rnHsTyKi _ doc (HsKindSig ty k)
  = do { kind_sigs_ok <- xoptM Opt_KindSignatures
       ; unless kind_sigs_ok (badKindSigErr doc ty)
       ; (ty', fvs1) <- rnLHsType doc ty
       ; (k', fvs2) <- rnLHsKind doc k
       ; return (HsKindSig ty' k', fvs1 `plusFV` fvs2) }

rnHsTyKi _ doc (HsPArrTy ty)
  = do { (ty', fvs) <- rnLHsType doc ty
       ; return (HsPArrTy ty', fvs) }

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsTyKi what doc tupleTy@(HsTupleTy tup_con tys)
  = do { data_kinds <- xoptM Opt_DataKinds
       ; when (not data_kinds && isRnKind what)
              (addErr (dataKindsErr what tupleTy))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi what doc) tys
       ; return (HsTupleTy tup_con tys', fvs) }

-- Ensure that a type-level integer is nonnegative (#8306, #8412)
rnHsTyKi what _ tyLit@(HsTyLit t)
  = do { data_kinds <- xoptM Opt_DataKinds
       ; unless data_kinds (addErr (dataKindsErr what tyLit))
       ; when (negLit t) (addErr negLitErr)
       ; return (HsTyLit t, emptyFVs) }
  where
    negLit (HsStrTy _ _) = False
    negLit (HsNumTy _ i) = i < 0
    negLitErr = ptext (sLit "Illegal literal in type (type literals must not be negative):") <+> ppr tyLit

rnHsTyKi what doc (HsAppTy ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi what doc ty1
       ; (ty2', fvs2) <- rnLHsTyKi what doc ty2
       ; return (HsAppTy ty1' ty2', fvs1 `plusFV` fvs2) }

rnHsTyKi _ doc (HsIParamTy n ty)
  = do { (ty', fvs) <- rnLHsType doc ty
       ; return (HsIParamTy n ty', fvs) }

rnHsTyKi _ doc (HsEqTy ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsType doc ty1
       ; (ty2', fvs2) <- rnLHsType doc ty2
       ; return (HsEqTy ty1' ty2', fvs1 `plusFV` fvs2) }

rnHsTyKi _ _ (HsSpliceTy sp k)
  = rnSpliceType sp k

rnHsTyKi _ doc (HsDocTy ty haddock_doc)
  = do { (ty', fvs) <- rnLHsType doc ty
       ; haddock_doc' <- rnLHsDoc haddock_doc
       ; return (HsDocTy ty' haddock_doc', fvs) }

rnHsTyKi _ _ (HsCoreTy ty)
  = return (HsCoreTy ty, emptyFVs)
    -- The emptyFVs probably isn't quite right
    -- but I don't think it matters

rnHsTyKi _ _ (HsWrapTy {})
  = panic "rnHsTyKi"

rnHsTyKi what doc ty@(HsExplicitListTy k tys)
  = do { data_kinds <- xoptM Opt_DataKinds
       ; unless data_kinds (addErr (dataKindsErr what ty))
       ; (tys', fvs) <- rnLHsTypes doc tys
       ; return (HsExplicitListTy k tys', fvs) }

rnHsTyKi what doc ty@(HsExplicitTupleTy kis tys)
  = do { data_kinds <- xoptM Opt_DataKinds
       ; unless data_kinds (addErr (dataKindsErr what ty))
       ; (tys', fvs) <- rnLHsTypes doc tys
       ; return (HsExplicitTupleTy kis tys', fvs) }

rnHsTyKi what ctxt (HsWildCardTy wc)
  = do { wc' <- case mb_bad of
           Just msg -> do { addErr (wildCardMsg ctxt msg)
                          ; discardErrs (rnWildCard ctxt wc) }
                          -- discardErrs: avoid reporting
                          -- a second error
           Nothing  -> rnWildCard ctxt wc

       ; traceRn (text "rnHsTyKi wild" <+> ppr wc <+> ppr (isJust mb_bad))
       ; return (HsWildCardTy wc', emptyFVs) }
         -- emptyFVs: this occurrence does not refer to a
         --           user-written binding site, so don't treat
         --           it as a free variable
  where
    mb_bad :: Maybe SDoc
    mb_bad | not (wildCardsAllowed ctxt)
           = Just (notAllowed wc)
           | otherwise
           = case what of
               RnType          -> Nothing
               RnKind          -> Just (notAllowed wc <+> ptext (sLit "in a kind"))
               RnConstraint    -> Just constraint_msg
               RnTopConstraint -> case wc of
                     AnonWildCard {}  -> Just constraint_msg
                     NamedWildCard {} -> Nothing

    constraint_msg = hang (notAllowed wc <+> ptext (sLit "in a constraint"))
                        2 hint_msg

    hint_msg = case wc of
       NamedWildCard {} -> empty
       AnonWildCard {}  -> vcat [ ptext (sLit "except as the last top-level constraint of a type signature")
                                , nest 2 (ptext (sLit "e.g  f :: (Eq a, _) => blah")) ]

notAllowed :: HsWildCardInfo RdrName -> SDoc
notAllowed wc =  ptext (sLit "Wildcard") <+> quotes (ppr wc)
                 <+> ptext (sLit "not allowed")

wildCardMsg :: HsDocContext -> SDoc -> SDoc
wildCardMsg ctxt doc
  = vcat [doc, nest 2 (ptext (sLit "in") <+> pprHsDocContext ctxt)]

--------------
rnTyVar :: RnTyKiWhat -> RdrName -> RnM Name
rnTyVar what rdr_name
  | isRnKind what = lookupKindOccRn rdr_name
  | otherwise     = lookupTypeOccRn rdr_name

rnLTyVar :: Located RdrName -> RnM (Located Name)
rnLTyVar (L loc rdr_name)
  = do { tyvar <- lookupTypeOccRn rdr_name
       ; return (L loc tyvar) }

--------------
rnLHsTypes :: HsDocContext -> [LHsType RdrName]
           -> RnM ([LHsType Name], FreeVars)
rnLHsTypes doc tys = mapFvRn (rnLHsType doc) tys

--------------
extraConstraintWildCardsAllowed :: HsDocContext -> Bool
extraConstraintWildCardsAllowed ctxt
  = case ctxt of
      TypeSigCtx {}       -> True
      _                   -> False

wildCardsAllowed :: HsDocContext -> Bool
-- ^ In what contexts are wildcards permitted
wildCardsAllowed ctxt
   = case ctxt of
       TypeSigCtx {}       -> True
       TypBrCtx {}         -> True   -- Template Haskell quoted type
       SpliceTypeCtx {}    -> True   -- Result of a Template Haskell splice
       ExprWithTySigCtx {} -> True
       PatCtx {}           -> True
       RuleCtx {}          -> True
       FamPatCtx {}        -> True   -- Not named wildcards though
       GHCiCtx {}          -> True
       _                   -> False

rnExtraConstraintWildCard :: HsDocContext -> HsWildCardInfo RdrName
                          -> RnM (HsWildCardInfo Name)
-- Rename the extra-constraint spot in a type signature
--    (blah, _) => type
-- Check that extra-constraints are allowed at all, and
-- if so that it's an anonymous wildcard
rnExtraConstraintWildCard ctxt wc
  = case mb_bad of
      Nothing  -> rnWildCard ctxt wc
      Just msg -> do { addErr (wildCardMsg ctxt msg)
                     ; discardErrs (rnWildCard ctxt wc) }
  where
    mb_bad | not (extraConstraintWildCardsAllowed ctxt)
           = Just (ptext (sLit "Extra-contraint wildcard") <+> quotes (ppr wc)
                   <+> ptext (sLit "not allowed"))
           | isNamedWildCard wc
           = Just (hang (ptext (sLit "Named wildcard") <+> quotes (ppr wc)
                         <+> ptext (sLit "not allowed as an extra-contraint"))
                      2 (ptext (sLit "Use an anonymous wildcard instead")))
           | otherwise
           = Nothing

rnWildCard :: HsDocContext -> HsWildCardInfo RdrName -> RnM (HsWildCardInfo Name)
rnWildCard _ (AnonWildCard _)
  = do { loc <- getSrcSpanM
       ; uniq <- newUnique
       ; let name = mkInternalName uniq (mkTyVarOcc "_") loc
       ; return (AnonWildCard name) }

rnWildCard ctxt wc@(NamedWildCard rdr_name)
  -- NB: The parser only generates NamedWildCard if -XNamedWildCards
  --     is on, so we don't need to check for that here
  = do { mb_name <- lookupOccRn_maybe rdr_name
       ; traceRn (text "rnWildCard named" <+> (ppr rdr_name $$ ppr mb_name))
       ; case mb_name of
           Just n  -> return (NamedWildCard n)
           Nothing -> do { addErr msg  -- I'm not sure how this can happen
                         ; return (NamedWildCard (mkUnboundNameRdr rdr_name)) } }
  where
    msg = wildCardMsg ctxt (notAllowed wc)


{- *****************************************************
*                                                      *
          Binding type variables
*                                                      *
***************************************************** -}

bindSigTyVarsFV :: [Name]
                -> RnM (a, FreeVars)
                -> RnM (a, FreeVars)
-- Used just before renaming the defn of a function
-- with a separate type signature, to bring its tyvars into scope
-- With no -XScopedTypeVariables, this is a no-op
bindSigTyVarsFV tvs thing_inside
  = do  { scoped_tyvars <- xoptM Opt_ScopedTypeVariables
        ; if not scoped_tyvars then
                thing_inside
          else
                bindLocalNamesFV tvs thing_inside }

---------------
bindHsQTyVars :: HsDocContext
              -> Maybe a              -- Just _  => an associated type decl
              -> [RdrName]            -- Kind variables from scope
              -> LHsQTyVars RdrName   -- Type variables
              -> (LHsQTyVars Name -> RnM (b, FreeVars))
              -> RnM (b, FreeVars)
-- (a) Bring kind variables into scope
--     both (i)  passed in (kv_bndrs)
--     and  (ii) mentioned in the kinds of tv_bndrs
-- (b) Bring type variables into scope
bindHsQTyVars doc mb_assoc kv_bndrs tv_bndrs thing_inside
  = do { rdr_env <- getLocalRdrEnv
       ; let tvs = hsQTvBndrs tv_bndrs
             kvs_from_tv_bndrs = [ kv | L _ (KindedTyVar _ kind) <- tvs
                                 , let (_, kvs) = extractHsTyRdrTyVars kind
                                 , kv <- kvs ]
             all_kvs' = nub (kv_bndrs ++ kvs_from_tv_bndrs)
             all_kvs  = filterOut (inScope rdr_env) all_kvs'

             overlap_kvs = [ kv | kv <- all_kvs, any ((==) kv . hsLTyVarName) tvs ]
                -- These variables appear both as kind and type variables
                -- in the same declaration; eg  type family  T (x :: *) (y :: x)
                -- We disallow this: too confusing!

       ; poly_kind <- xoptM Opt_PolyKinds
       ; unless (poly_kind || null all_kvs)
                (addErr (badKindBndrs doc all_kvs))
       ; unless (null overlap_kvs)
                (addErr (overlappingKindVars doc overlap_kvs))

       ; loc <- getSrcSpanM
       ; kv_names <- mapM (newLocalBndrRn . L loc) all_kvs
       ; bindLocalNamesFV kv_names $
         bindLHsTyVarBndrs doc mb_assoc tvs $ \ tv_bndrs' ->
         thing_inside (HsQTvs { hsq_tvs = tv_bndrs', hsq_kvs = kv_names }) }

bindLHsTyVarBndrs :: HsDocContext
                  -> Maybe a                 -- Just _  => an associated type decl
                  -> [LHsTyVarBndr RdrName]
                  -> ([LHsTyVarBndr Name] -> RnM (b, FreeVars))
                  -> RnM (b, FreeVars)
bindLHsTyVarBndrs doc mb_assoc tv_bndrs thing_inside
  = do { let tv_names_w_loc = map hsLTyVarLocName tv_bndrs

       -- Check for duplicate or shadowed tyvar bindrs
       ; checkDupRdrNames tv_names_w_loc
       ; when (isNothing mb_assoc) (checkShadowedRdrNames tv_names_w_loc)

       ; rdr_env <- getLocalRdrEnv
       ; (tv_bndrs', fvs1) <- mapFvRn (rnLHsTyVarBndr doc mb_assoc rdr_env) tv_bndrs
       ; (res, fvs2) <- bindLocalNamesFV (map hsLTyVarName tv_bndrs') $
                        thing_inside tv_bndrs'
       ; return (res, fvs1 `plusFV` fvs2) }

rnLHsTyVarBndr :: HsDocContext -> Maybe a -> LocalRdrEnv
               -> LHsTyVarBndr RdrName -> RnM (LHsTyVarBndr Name, FreeVars)
rnLHsTyVarBndr _ mb_assoc rdr_env (L loc (UserTyVar rdr))
  = do { nm <- newTyVarNameRn mb_assoc rdr_env loc rdr
       ; return (L loc (UserTyVar nm), emptyFVs) }
rnLHsTyVarBndr doc mb_assoc rdr_env (L loc (KindedTyVar (L lv rdr) kind))
  = do { sig_ok <- xoptM Opt_KindSignatures
       ; unless sig_ok (badKindSigErr doc kind)
       ; nm <- newTyVarNameRn mb_assoc rdr_env loc rdr
       ; (kind', fvs) <- rnLHsKind doc kind
       ; return (L loc (KindedTyVar (L lv nm) kind'), fvs) }

newTyVarNameRn :: Maybe a -> LocalRdrEnv -> SrcSpan -> RdrName -> RnM Name
newTyVarNameRn mb_assoc rdr_env loc rdr
  | Just _ <- mb_assoc    -- Use the same Name as the parent class decl
  , Just n <- lookupLocalRdrEnv rdr_env rdr
  = return n
  | otherwise
  = newLocalBndrRn (L loc rdr)

---------------------
collectNamedWildCards :: LHsType RdrName -> [Located RdrName]
collectNamedWildCards hs_ty
  = nubBy eqLocated $
    [L l n | L l (NamedWildCard n) <- collectWildCards hs_ty ]

collectAnonWildCards :: LHsType Name -> [Name]
collectAnonWildCards hs_ty
  = [n | L _ (AnonWildCard n) <- collectWildCards hs_ty ]

collectWildCards :: LHsType name -> [Located (HsWildCardInfo name)]
-- | Extract all wild cards from a type.
collectWildCards lty = go lty
  where
    go (L loc ty) = case ty of
      HsAppTy ty1 ty2         -> go ty1 `mappend` go ty2
      HsFunTy ty1 ty2         -> go ty1 `mappend` go ty2
      HsListTy ty             -> go ty
      HsPArrTy ty             -> go ty
      HsTupleTy _ tys         -> gos tys
      HsOpTy ty1 _ ty2        -> go ty1 `mappend` go ty2
      HsParTy ty              -> go ty
      HsIParamTy _ ty         -> go ty
      HsEqTy ty1 ty2          -> go ty1 `mappend` go ty2
      HsKindSig ty kind       -> go ty `mappend` go kind
      HsDocTy ty _            -> go ty
      HsBangTy _ ty           -> go ty
      HsRecTy flds            -> gos $ map (cd_fld_type . unLoc) flds
      HsExplicitListTy _ tys  -> gos tys
      HsExplicitTupleTy _ tys -> gos tys
      HsWrapTy _ ty           -> go (L loc ty)
      -- Interesting cases
      HsWildCardTy wc         -> [L loc wc]
      HsForAllTy { hst_body = ty } -> go ty
      HsQualTy { hst_ctxt = L _ ctxt
               , hst_body = ty }  -> gos ctxt `mappend` go ty
      -- HsQuasiQuoteTy, HsSpliceTy, HsCoreTy, HsTyLit
      _ -> mempty

    gos = mconcat . map go


{-
*********************************************************
*                                                       *
        ConDeclField
*                                                       *
*********************************************************

When renaming a ConDeclField, we have to find the FieldLabel
associated with each field.  But we already have all the FieldLabels
available (since they were brought into scope by
RnNames.getLocalNonValBinders), so we just take the list as an
argument, build a map and look them up.
-}

rnConDeclFields :: [FieldLabel] -> HsDocContext -> [LConDeclField RdrName]
                -> RnM ([LConDeclField Name], FreeVars)
rnConDeclFields fls doc fields = mapFvRn (rnField fl_env doc) fields
  where
    fl_env = mkFsEnv [ (flLabel fl, fl) | fl <- fls ]

rnField :: FastStringEnv FieldLabel -> HsDocContext -> LConDeclField RdrName
        -> RnM (LConDeclField Name, FreeVars)
rnField fl_env doc (L l (ConDeclField names ty haddock_doc))
  = do { let new_names = map (fmap lookupField) names
       ; (new_ty, fvs) <- rnLHsType doc ty
       ; new_haddock_doc <- rnMbLHsDoc haddock_doc
       ; return (L l (ConDeclField new_names new_ty new_haddock_doc), fvs) }
  where
    lookupField :: FieldOcc RdrName -> FieldOcc Name
    lookupField (FieldOcc rdr _) = FieldOcc rdr (flSelector fl)
      where
        lbl = occNameFS $ rdrNameOcc rdr
        fl  = expectJust "rnField" $ lookupFsEnv fl_env lbl


{-
*********************************************************
*                                                       *
        Contexts
*                                                       *
*********************************************************
-}

rnContext :: HsDocContext -> LHsContext RdrName -> RnM (LHsContext Name, FreeVars)
rnContext doc (L loc cxt)
  = do { traceRn (text "rncontext" <+> ppr cxt)
       ; (cxt', fvs) <- mapFvRn (rnLHsPred doc) cxt
       ; return (L loc cxt', fvs) }

{-
************************************************************************
*                                                                      *
        Fixities and precedence parsing
*                                                                      *
************************************************************************

@mkOpAppRn@ deals with operator fixities.  The argument expressions
are assumed to be already correctly arranged.  It needs the fixities
recorded in the OpApp nodes, because fixity info applies to the things
the programmer actually wrote, so you can't find it out from the Name.

Furthermore, the second argument is guaranteed not to be another
operator application.  Why? Because the parser parses all
operator appications left-associatively, EXCEPT negation, which
we need to handle specially.
Infix types are read in a *right-associative* way, so that
        a `op` b `op` c
is always read in as
        a `op` (b `op` c)

mkHsOpTyRn rearranges where necessary.  The two arguments
have already been renamed and rearranged.  It's made rather tiresome
by the presence of ->, which is a separate syntactic construct.
-}

---------------
-- Building (ty1 `op1` (ty21 `op2` ty22))
mkHsOpTyRn :: (LHsType Name -> LHsType Name -> HsType Name)
           -> Name -> Fixity -> LHsType Name -> LHsType Name
           -> RnM (HsType Name)

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsOpTy ty21 (w2, op2) ty22))
  = do  { fix2 <- lookupTyFixityRn op2
        ; mk_hs_op_ty mk1 pp_op1 fix1 ty1
                      (\t1 t2 -> HsOpTy t1 (w2, op2) t2)
                      (unLoc op2) fix2 ty21 ty22 loc2 }

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsFunTy ty21 ty22))
  = mk_hs_op_ty mk1 pp_op1 fix1 ty1
                HsFunTy funTyConName funTyFixity ty21 ty22 loc2

mkHsOpTyRn mk1 _ _ ty1 ty2              -- Default case, no rearrangment
  = return (mk1 ty1 ty2)

---------------
mk_hs_op_ty :: (LHsType Name -> LHsType Name -> HsType Name)
            -> Name -> Fixity -> LHsType Name
            -> (LHsType Name -> LHsType Name -> HsType Name)
            -> Name -> Fixity -> LHsType Name -> LHsType Name -> SrcSpan
            -> RnM (HsType Name)
mk_hs_op_ty mk1 op1 fix1 ty1
            mk2 op2 fix2 ty21 ty22 loc2
  | nofix_error     = do { precParseErr (op1,fix1) (op2,fix2)
                         ; return (mk1 ty1 (L loc2 (mk2 ty21 ty22))) }
  | associate_right = return (mk1 ty1 (L loc2 (mk2 ty21 ty22)))
  | otherwise       = do { -- Rearrange to ((ty1 `op1` ty21) `op2` ty22)
                           new_ty <- mkHsOpTyRn mk1 op1 fix1 ty1 ty21
                         ; return (mk2 (noLoc new_ty) ty22) }
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2


---------------------------
mkOpAppRn :: LHsExpr Name                       -- Left operand; already rearranged
          -> LHsExpr Name -> Fixity             -- Operator and fixity
          -> LHsExpr Name                       -- Right operand (not an OpApp, but might
                                                -- be a NegApp)
          -> RnM (HsExpr Name)

-- (e11 `op1` e12) `op2` e2
mkOpAppRn e1@(L _ (OpApp e11 op1 fix1 e12)) op2 fix2 e2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (OpApp e1 op2 fix2 e2)

  | associate_right = do
    new_e <- mkOpAppRn e12 op2 fix2 e2
    return (OpApp e11 op1 fix1 (L loc' new_e))
  where
    loc'= combineLocs e12 e2
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--      (- neg_arg) `op` e2
mkOpAppRn e1@(L _ (NegApp neg_arg neg_name)) op2 fix2 e2
  | nofix_error
  = do precParseErr (negateName,negateFixity) (get_op op2,fix2)
       return (OpApp e1 op2 fix2 e2)

  | associate_right
  = do new_e <- mkOpAppRn neg_arg op2 fix2 e2
       return (NegApp (L loc' new_e) neg_name)
  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--      e1 `op` - neg_arg
mkOpAppRn e1 op1 fix1 e2@(L _ (NegApp _ _))     -- NegApp can occur on the right
  | not associate_right                 -- We *want* right association
  = do precParseErr (get_op op1, fix1) (negateName, negateFixity)
       return (OpApp e1 op1 fix1 e2)
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--      Default case
mkOpAppRn e1 op fix e2                  -- Default case, no rearrangment
  = ASSERT2( right_op_ok fix (unLoc e2),
             ppr e1 $$ text "---" $$ ppr op $$ text "---" $$ ppr fix $$ text "---" $$ ppr e2
    )
    return (OpApp e1 op fix e2)

----------------------------
get_op :: LHsExpr Name -> Name
-- An unbound name could be either HsVar or HsUnboundVar
-- See RnExpr.rnUnboundVar
get_op (L _ (HsVar n))          = n
get_op (L _ (HsUnboundVar occ)) = mkUnboundName occ
get_op other                    = pprPanic "get_op" (ppr other)

-- Parser left-associates everything, but
-- derived instances may have correctly-associated things to
-- in the right operarand.  So we just check that the right operand is OK
right_op_ok :: Fixity -> HsExpr Name -> Bool
right_op_ok fix1 (OpApp _ _ fix2 _)
  = not error_please && associate_right
  where
    (error_please, associate_right) = compareFixity fix1 fix2
right_op_ok _ _
  = True

-- Parser initially makes negation bind more tightly than any other operator
-- And "deriving" code should respect this (use HsPar if not)
mkNegAppRn :: LHsExpr id -> SyntaxExpr id -> RnM (HsExpr id)
mkNegAppRn neg_arg neg_name
  = ASSERT( not_op_app (unLoc neg_arg) )
    return (NegApp neg_arg neg_name)

not_op_app :: HsExpr id -> Bool
not_op_app (OpApp _ _ _ _) = False
not_op_app _               = True

---------------------------
mkOpFormRn :: LHsCmdTop Name            -- Left operand; already rearranged
          -> LHsExpr Name -> Fixity     -- Operator and fixity
          -> LHsCmdTop Name             -- Right operand (not an infix)
          -> RnM (HsCmd Name)

-- (e11 `op1` e12) `op2` e2
mkOpFormRn a1@(L loc (HsCmdTop (L _ (HsCmdArrForm op1 (Just fix1) [a11,a12])) _ _ _))
        op2 fix2 a2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (HsCmdArrForm op2 (Just fix2) [a1, a2])

  | associate_right
  = do new_c <- mkOpFormRn a12 op2 fix2 a2
       return (HsCmdArrForm op1 (Just fix1)
               [a11, L loc (HsCmdTop (L loc new_c)
               placeHolderType placeHolderType [])])
        -- TODO: locs are wrong
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2

--      Default case
mkOpFormRn arg1 op fix arg2                     -- Default case, no rearrangment
  = return (HsCmdArrForm op (Just fix) [arg1, arg2])


--------------------------------------
mkConOpPatRn :: Located Name -> Fixity -> LPat Name -> LPat Name
             -> RnM (Pat Name)

mkConOpPatRn op2 fix2 p1@(L loc (ConPatIn op1 (InfixCon p11 p12))) p2
  = do  { fix1 <- lookupFixityRn (unLoc op1)
        ; let (nofix_error, associate_right) = compareFixity fix1 fix2

        ; if nofix_error then do
                { precParseErr (unLoc op1,fix1) (unLoc op2,fix2)
                ; return (ConPatIn op2 (InfixCon p1 p2)) }

          else if associate_right then do
                { new_p <- mkConOpPatRn op2 fix2 p12 p2
                ; return (ConPatIn op1 (InfixCon p11 (L loc new_p))) } -- XXX loc right?
          else return (ConPatIn op2 (InfixCon p1 p2)) }

mkConOpPatRn op _ p1 p2                         -- Default case, no rearrangment
  = ASSERT( not_op_pat (unLoc p2) )
    return (ConPatIn op (InfixCon p1 p2))

not_op_pat :: Pat Name -> Bool
not_op_pat (ConPatIn _ (InfixCon _ _)) = False
not_op_pat _                           = True

--------------------------------------
checkPrecMatch :: Name -> MatchGroup Name body -> RnM ()
  -- Check precedence of a function binding written infix
  --   eg  a `op` b `C` c = ...
  -- See comments with rnExpr (OpApp ...) about "deriving"

checkPrecMatch op (MG { mg_alts = ms })
  = mapM_ check ms
  where
    check (L _ (Match _ (L l1 p1 : L l2 p2 :_) _ _))
      = setSrcSpan (combineSrcSpans l1 l2) $
        do checkPrec op p1 False
           checkPrec op p2 True

    check _ = return ()
        -- This can happen.  Consider
        --      a `op` True = ...
        --      op          = ...
        -- The infix flag comes from the first binding of the group
        -- but the second eqn has no args (an error, but not discovered
        -- until the type checker).  So we don't want to crash on the
        -- second eqn.

checkPrec :: Name -> Pat Name -> Bool -> IOEnv (Env TcGblEnv TcLclEnv) ()
checkPrec op (ConPatIn op1 (InfixCon _ _)) right = do
    op_fix@(Fixity op_prec  op_dir) <- lookupFixityRn op
    op1_fix@(Fixity op1_prec op1_dir) <- lookupFixityRn (unLoc op1)
    let
        inf_ok = op1_prec > op_prec ||
                 (op1_prec == op_prec &&
                  (op1_dir == InfixR && op_dir == InfixR && right ||
                   op1_dir == InfixL && op_dir == InfixL && not right))

        info  = (op,        op_fix)
        info1 = (unLoc op1, op1_fix)
        (infol, infor) = if right then (info, info1) else (info1, info)
    unless inf_ok (precParseErr infol infor)

checkPrec _ _ _
  = return ()

-- Check precedence of (arg op) or (op arg) respectively
-- If arg is itself an operator application, then either
--   (a) its precedence must be higher than that of op
--   (b) its precedency & associativity must be the same as that of op
checkSectionPrec :: FixityDirection -> HsExpr RdrName
        -> LHsExpr Name -> LHsExpr Name -> RnM ()
checkSectionPrec direction section op arg
  = case unLoc arg of
        OpApp _ op fix _ -> go_for_it (get_op op) fix
        NegApp _ _       -> go_for_it negateName  negateFixity
        _                -> return ()
  where
    op_name = get_op op
    go_for_it arg_op arg_fix@(Fixity arg_prec assoc) = do
          op_fix@(Fixity op_prec _) <- lookupFixityRn op_name
          unless (op_prec < arg_prec
                  || (op_prec == arg_prec && direction == assoc))
                 (sectionPrecErr (op_name, op_fix)
                                 (arg_op, arg_fix) section)

-- Precedence-related error messages

precParseErr :: (Name, Fixity) -> (Name, Fixity) -> RnM ()
precParseErr op1@(n1,_) op2@(n2,_)
  | isUnboundName n1 || isUnboundName n2
  = return ()     -- Avoid error cascade
  | otherwise
  = addErr $ hang (ptext (sLit "Precedence parsing error"))
      4 (hsep [ptext (sLit "cannot mix"), ppr_opfix op1, ptext (sLit "and"),
               ppr_opfix op2,
               ptext (sLit "in the same infix expression")])

sectionPrecErr :: (Name, Fixity) -> (Name, Fixity) -> HsExpr RdrName -> RnM ()
sectionPrecErr op@(n1,_) arg_op@(n2,_) section
  | isUnboundName n1 || isUnboundName n2
  = return ()     -- Avoid error cascade
  | otherwise
  = addErr $ vcat [ptext (sLit "The operator") <+> ppr_opfix op <+> ptext (sLit "of a section"),
         nest 4 (sep [ptext (sLit "must have lower precedence than that of the operand,"),
                      nest 2 (ptext (sLit "namely") <+> ppr_opfix arg_op)]),
         nest 4 (ptext (sLit "in the section:") <+> quotes (ppr section))]

ppr_opfix :: (Name, Fixity) -> SDoc
ppr_opfix (op, fixity) = pp_op <+> brackets (ppr fixity)
   where
     pp_op | op == negateName = ptext (sLit "prefix `-'")
           | otherwise        = quotes (ppr op)

{- *****************************************************
*                                                      *
                 Errors
*                                                      *
***************************************************** -}

overlappingKindVars :: HsDocContext -> [RdrName] -> SDoc
overlappingKindVars doc kvs
  = withHsDocContext doc $
    ptext (sLit "Kind variable") <> plural kvs
    <+> ptext (sLit "also used as type variable") <> plural kvs
    <> colon <+> pprQuotedList kvs

badKindBndrs :: HsDocContext -> [RdrName] -> SDoc
badKindBndrs doc kvs
  = withHsDocContext doc $
    hang (ptext (sLit "Unexpected kind variable") <> plural kvs
                 <+> pprQuotedList kvs)
       2 (ptext (sLit "Perhaps you intended to use PolyKinds"))

badKindSigErr :: HsDocContext -> LHsType RdrName -> TcM ()
badKindSigErr doc (L loc ty)
  = setSrcSpan loc $ addErr $
    withHsDocContext doc $
    hang (ptext (sLit "Illegal kind signature:") <+> quotes (ppr ty))
       2 (ptext (sLit "Perhaps you intended to use KindSignatures"))

dataKindsErr :: RnTyKiWhat -> HsType RdrName -> SDoc
dataKindsErr what thing
  = hang (ptext (sLit "Illegal") <+> pp_what <> colon <+> quotes (ppr thing))
       2 (ptext (sLit "Perhaps you intended to use DataKinds"))
  where
    pp_what | isRnKind what = ptext (sLit "kind")
            | otherwise     = ptext (sLit "type")

inTypeDoc :: HsType RdrName -> SDoc
inTypeDoc ty = ptext (sLit "In the type") <+> quotes (ppr ty)

warnUnusedForAlls :: SDoc -> [LHsTyVarBndr Name] -> FreeVars -> TcM ()
warnUnusedForAlls in_doc bound_names used_names
  = whenWOptM Opt_WarnUnusedMatches $
    mapM_ add_warn bound_names
  where
    add_warn (L loc tv)
      = unless (hsTyVarName tv `elemNameSet` used_names) $
        addWarnAt loc $
        vcat [ ptext (sLit "Unused quantified type variable") <+> quotes (ppr tv)
             , in_doc ]

opTyErr :: RdrName -> HsType RdrName -> SDoc
opTyErr op ty@(HsOpTy ty1 _ _)
  = hang (ptext (sLit "Illegal operator") <+> quotes (ppr op) <+> ptext (sLit "in type") <+> quotes (ppr ty))
         2 extra
  where
    extra | op == dot_tv_RDR && forall_head ty1
          = perhapsForallMsg
          | otherwise
          = ptext (sLit "Use TypeOperators to allow operators in types")

    forall_head (L _ (HsTyVar tv))   = tv == forall_tv_RDR
    forall_head (L _ (HsAppTy ty _)) = forall_head ty
    forall_head _other               = False
opTyErr _ ty = pprPanic "opTyErr: Not an op" (ppr ty)

{-
************************************************************************
*                                                                      *
      Finding the free type variables of a (HsType RdrName)
*                                                                      *
************************************************************************


Note [Kind and type-variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type signature we may implicitly bind type variable and, more
recently, kind variables.  For example:
  *   f :: a -> a
      f = ...
    Here we need to find the free type variables of (a -> a),
    so that we know what to quantify

  *   class C (a :: k) where ...
    This binds 'k' in ..., as well as 'a'

  *   f (x :: a -> [a]) = ....
    Here we bind 'a' in ....

  *   f (x :: T a -> T (b :: k)) = ...
    Here we bind both 'a' and the kind variable 'k'

  *   type instance F (T (a :: Maybe k)) = ...a...k...
    Here we want to constrain the kind of 'a', and bind 'k'.

In general we want to walk over a type, and find
  * Its free type variables
  * The free kind variables of any kind signatures in the type

Hence we returns a pair (kind-vars, type vars)
See also Note [HsBSig binder lists] in HsTypes
-}

type FreeKiTyVars = ([RdrName], [RdrName]) -- (Kind vars, type vars)

filterInScope :: LocalRdrEnv -> FreeKiTyVars -> FreeKiTyVars
filterInScope rdr_env (kvs, tvs)
  = (filterOut (inScope rdr_env) kvs, filterOut (inScope rdr_env) tvs)

inScope :: LocalRdrEnv -> RdrName -> Bool
inScope rdr_env rdr = rdr `elemLocalRdrEnv` rdr_env

extractHsTyRdrTyVars :: LHsType RdrName -> FreeKiTyVars
-- extractHsTyRdrNames finds the free (kind, type) variables of a HsType
--                        or the free (sort, kind) variables of a HsKind
-- It's used when making the for-alls explicit.
-- Does not return any wildcards
-- See Note [Kind and type-variable binders]
extractHsTyRdrTyVars ty
  = case extract_lty ty ([],[]) of
     (kvs, tvs) -> (nub kvs, nub tvs)

extractHsTysRdrTyVars :: [LHsType RdrName] -> FreeKiTyVars
-- See Note [Kind and type-variable binders]
extractHsTysRdrTyVars ty
  = case extract_ltys ty ([],[]) of
     (kvs, tvs) -> (nub kvs, nub tvs)

extractRdrKindSigVars :: LFamilyResultSig RdrName -> [RdrName]
extractRdrKindSigVars (L _ resultSig)
    | KindSig k                        <- resultSig = kindRdrNameFromSig k
    | TyVarSig (L _ (KindedTyVar _ k)) <- resultSig = kindRdrNameFromSig k
    | TyVarSig (L _ (UserTyVar _))     <- resultSig = []
    | otherwise = [] -- this can only be NoSig but pattern exhasutiveness
                     -- checker complains about "NoSig <- resultSig"
    where kindRdrNameFromSig k = nub (fst (extract_lkind k ([],[])))

extractDataDefnKindVars :: HsDataDefn RdrName -> [RdrName]
-- Get the scoped kind variables mentioned free in the constructor decls
-- Eg    data T a = T1 (S (a :: k) | forall (b::k). T2 (S b)
-- Here k should scope over the whole definition
extractDataDefnKindVars (HsDataDefn { dd_ctxt = ctxt, dd_kindSig = ksig
                                    , dd_cons = cons, dd_derivs = derivs })
  = fst $ extract_lctxt ctxt $
          extract_mb extract_lkind ksig $
          extract_mb (extract_sig_tys . unLoc) derivs $
          foldr (extract_con . unLoc) ([],[]) cons
  where
    extract_con (ConDecl { con_res = ResTyGADT {} }) acc = acc
    extract_con (ConDecl { con_res = ResTyH98, con_qvars = qvs
                         , con_cxt = ctxt, con_details = details }) acc
      = extract_hs_tv_bndrs (hsQTvBndrs qvs) acc $
        extract_lctxt ctxt $
        extract_ltys (hsConDeclArgTys details) ([],[])


extract_lctxt :: LHsContext RdrName -> FreeKiTyVars -> FreeKiTyVars
extract_lctxt ctxt = extract_ltys (unLoc ctxt)

extract_sig_tys :: [LHsSigType RdrName] -> FreeKiTyVars -> FreeKiTyVars
extract_sig_tys sig_tys acc
  = foldr (\sig_ty acc -> extract_lty (hsSigType sig_ty) acc)
          acc sig_tys

extract_ltys :: [LHsType RdrName] -> FreeKiTyVars -> FreeKiTyVars
extract_ltys tys acc = foldr extract_lty acc tys

extract_mb :: (a -> FreeKiTyVars -> FreeKiTyVars) -> Maybe a -> FreeKiTyVars -> FreeKiTyVars
extract_mb _ Nothing  acc = acc
extract_mb f (Just x) acc = f x acc

extract_lkind :: LHsType RdrName -> FreeKiTyVars -> FreeKiTyVars
extract_lkind kind (acc_kvs, acc_tvs) = case extract_lty kind ([], acc_kvs) of
                                          (_, res_kvs) -> (res_kvs, acc_tvs)
                                        -- Kinds shouldn't have sort signatures!

extract_lty :: LHsType RdrName -> FreeKiTyVars -> FreeKiTyVars
extract_lty (L _ ty) acc
  = case ty of
      HsTyVar tv                -> extract_tv tv acc
      HsBangTy _ ty             -> extract_lty ty acc
      HsRecTy flds              -> foldr (extract_lty . cd_fld_type . unLoc) acc
                                         flds
      HsAppTy ty1 ty2           -> extract_lty ty1 (extract_lty ty2 acc)
      HsListTy ty               -> extract_lty ty acc
      HsPArrTy ty               -> extract_lty ty acc
      HsTupleTy _ tys           -> extract_ltys tys acc
      HsFunTy ty1 ty2           -> extract_lty ty1 (extract_lty ty2 acc)
      HsIParamTy _ ty           -> extract_lty ty acc
      HsEqTy ty1 ty2            -> extract_lty ty1 (extract_lty ty2 acc)
      HsOpTy ty1 (_, (L _ tv)) ty2 -> extract_tv tv (extract_lty ty1 (extract_lty ty2 acc))
      HsParTy ty                -> extract_lty ty acc
      HsCoreTy {}               -> acc  -- The type is closed
      HsSpliceTy {}             -> acc  -- Type splices mention no type variables
      HsDocTy ty _              -> extract_lty ty acc
      HsExplicitListTy _ tys    -> extract_ltys tys acc
      HsExplicitTupleTy _ tys   -> extract_ltys tys acc
      HsTyLit _                 -> acc
      HsWrapTy _ _              -> panic "extract_lty"
      HsKindSig ty ki           -> extract_lty ty (extract_lkind ki acc)
      HsForAllTy { hst_bndrs = tvs, hst_body = ty }
                                -> extract_hs_tv_bndrs tvs acc $
                                   extract_lty ty ([],[])
      HsQualTy { hst_ctxt = cx, hst_body = ty }
                                -> extract_lctxt cx (extract_lty ty acc)
      HsWildCardTy {}           -> acc

extract_hs_tv_bndrs :: [LHsTyVarBndr RdrName] -> FreeKiTyVars
                    -> FreeKiTyVars -> FreeKiTyVars
-- In (forall (a :: Maybe e). a -> b) we have
--     'a' is bound by the forall
--     'b' is a free type variable
--     'e' is a free kind variable
extract_hs_tv_bndrs tvs
                    (acc_kvs, acc_tvs)   -- Note accumulator comes first
                    (body_kvs, body_tvs)
  | null tvs
  = (body_kvs ++ acc_kvs, body_tvs ++ acc_tvs)
  | otherwise
  = (acc_kvs ++ bndr_kvs ++ body_kvs,
     acc_tvs ++ filterOut (`elem` local_tvs) body_tvs)
  where
    local_tvs = map hsLTyVarName tvs
    (_, bndr_kvs) = foldr extract_lty ([], []) [k | L _ (KindedTyVar _ k) <- tvs]

extract_tv :: RdrName -> FreeKiTyVars -> FreeKiTyVars
extract_tv tv acc
  | isRdrTyVar tv = add_tv tv acc
  | otherwise     = acc

add_tv :: RdrName -> FreeKiTyVars -> FreeKiTyVars
add_tv tv (kvs,tvs) = (kvs, tv : tvs)
