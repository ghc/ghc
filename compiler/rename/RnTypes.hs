{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[RnSource]{Main pass of renamer}
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module RnTypes (
        -- Type related stuff
        rnHsType, rnLHsType, rnLHsTypes, rnContext,
        rnHsKind, rnLHsKind, rnLHsMaybeKind,
        rnHsSigType, rnHsWcType,
        rnHsSigWcType, rnHsSigWcTypeScoped,
        rnLHsInstType,
        newTyVarNameRn, collectAnonWildCards,
        rnConDeclFields,
        rnLTyVar,

        -- Precence related stuff
        mkOpAppRn, mkNegAppRn, mkOpFormRn, mkConOpPatRn,
        checkPrecMatch, checkSectionPrec,

        -- Binding related stuff
        warnUnusedForAlls, bindLHsTyVarBndr,
        bindSigTyVarsFV, bindHsQTyVars, bindLRdrNames,
        extractHsTyRdrTyVars, extractHsTysRdrTyVars,
        extractHsTysRdrTyVarsDups, rmDupsInRdrTyVars,
        extractRdrKindSigVars, extractDataDefnKindVars,
        freeKiTyVarsAllVars, freeKiTyVarsKindVars, freeKiTyVarsTypeVars
  ) where

import {-# SOURCE #-} RnSplice( rnSpliceType )

import DynFlags
import HsSyn
import RnHsDoc          ( rnLHsDoc, rnMbLHsDoc )
import RnEnv
import TcRnMonad
import RdrName
import PrelNames
import TysPrim          ( funTyConName )
import TysWiredIn       ( starKindTyConName, unicodeStarKindTyConName )
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
import qualified GHC.LanguageExtensions as LangExt

import Data.List        ( (\\), nubBy, partition )
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
  = do { let hs_ty = hswc_body wc_ty
       ; free_vars <- extract_filtered_rdr_ty_vars hs_ty
       ; (free_vars', nwc_rdrs) <- partition_nwcs free_vars
       ; rnImplicitBndrs no_implicit_if_forall free_vars' hs_ty $ \ vars ->
    do { rn_hs_wc_type ctxt wc_ty nwc_rdrs $ \ wc_ty' ->
         thing_inside (HsIB { hsib_vars = vars
                            , hsib_body = wc_ty' }) } }

rnHsWcType :: HsDocContext -> LHsWcType RdrName -> RnM (LHsWcType Name, FreeVars)
rnHsWcType ctxt wc_ty@(HsWC { hswc_body = hs_ty })
  = do { free_vars <- extract_filtered_rdr_ty_vars hs_ty
       ; (_, nwc_rdrs) <- partition_nwcs free_vars
       ; rn_hs_wc_type ctxt wc_ty nwc_rdrs $ \ wc_ty' ->
         return (wc_ty', emptyFVs) }

-- | Finds free type and kind variables in a type, without duplicates and
-- variables that are already in LocalRdrEnv.
extract_filtered_rdr_ty_vars :: LHsType RdrName -> RnM FreeKiTyVars
extract_filtered_rdr_ty_vars hs_ty
  = do { rdr_env <- getLocalRdrEnv
       ; filterInScope rdr_env <$> extractHsTyRdrTyVars hs_ty }

-- | When the NamedWildCards extension is enabled, removes type variables
-- that start with an underscore from the FreeKiTyVars in the argument
-- and returns them in a separate list.
-- When the extension is disabled, the function returns the argument and
-- empty list.
-- See Note [Renaming named wild cards]
partition_nwcs :: FreeKiTyVars -> RnM (FreeKiTyVars, [Located RdrName])
partition_nwcs free_vars@(FKTV { fktv_tys = tys, fktv_all = all })
  = do { wildcards_enabled <- fmap (xopt LangExt.NamedWildCards) getDynFlags
       ; let (nwcs, no_nwcs) =
                if wildcards_enabled
                then partition (startsWithUnderscore . rdrNameOcc . unLoc) tys
                else ([], tys)
             free_vars' = free_vars { fktv_tys = no_nwcs
                                    , fktv_all = all \\ nwcs }
       ; return (free_vars', nwcs) }

-- | Renames a type with wild card binders.
-- Expects a list of names of type variables that should be replaced with
-- named wild cards. (See Note [Renaming named wild cards])
-- Although the parser does not create named wild cards, it is possible to find
-- them in declaration splices, so the function tries to collect them.
rn_hs_wc_type :: HsDocContext -> LHsWcType RdrName -> [Located RdrName]
              -> (LHsWcType Name -> RnM (a, FreeVars))
              -> RnM (a, FreeVars)
rn_hs_wc_type ctxt (HsWC { hswc_body = hs_ty }) nwc_rdrs thing_inside
  = do { let nwc_collected = collectNamedWildCards hs_ty
       -- the parser doesn't generate named wcs, but they may be in splices
       ; rdr_env <- getLocalRdrEnv
       ; nwcs <- sequence [ newLocalBndrRn lrdr
                          | lrdr@(L _ rdr) <- nwc_collected ++ nwc_rdrs
                          , not (inScope rdr_env rdr) ]
       ; setLocalRdrEnv (extendLocalRdrEnvNwcs rdr_env nwcs) $
         bindLocalNamesFV nwcs $
    do { (wc_ty, fvs1) <- rnWcSigTy ctxt hs_ty
       ; let wc_ty' :: HsWildCardBndrs Name (LHsType Name)
             wc_ty' = wc_ty { hswc_wcs = nwcs ++ hswc_wcs wc_ty }
       ; (res, fvs2) <- thing_inside wc_ty'
       ; return (res, fvs1 `plusFV` fvs2) } }

rnWcSigTy :: HsDocContext -> LHsType RdrName
          -> RnM (LHsWcType Name, FreeVars)
-- ^ Renames just the top level of a type signature
-- It's exactly like rnHsTyKi, except that it uses rnWcSigContext
-- on a qualified type, and return info on any extra-constraints
-- wildcard.  Some code duplication, but no big deal.
rnWcSigTy ctxt (L loc hs_ty@(HsForAllTy { hst_bndrs = tvs, hst_body = hs_tau }))
  = bindLHsTyVarBndrs ctxt Nothing [] tvs $ \ _ tvs' ->
    do { lcl_env <- getLocalRdrEnv
       ; let explicitly_bound = fmap hsLTyVarName tvs'
       ; setLocalRdrEnv (delLocalRdrEnvNwcs lcl_env explicitly_bound) $
           -- See Note [Renaming named wild cards]
    do { (hs_tau', fvs) <- rnWcSigTy ctxt hs_tau
       ; warnUnusedForAlls (inTypeDoc hs_ty) tvs' fvs
       ; let hs_ty' = HsForAllTy { hst_bndrs = tvs', hst_body = hswc_body hs_tau' }
       ; return ( hs_tau' { hswc_body = L loc hs_ty' }, fvs) } }

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
  = getLocalRdrEnv >>= rn_wc_sig_context
  where
    rn_wc_sig_context :: LocalRdrEnv
                      -> RnM (HsWildCardBndrs Name (LHsContext Name), FreeVars)
    rn_wc_sig_context lcl_env
      | Just (hs_ctxt1, hs_ctxt_last) <- snocView hs_ctxt
      , L lx (HsWildCardTy wc) <- (to_nwc lcl_env . ignoreParens) hs_ctxt_last
      = do { (hs_ctxt1', fvs) <- mapFvRn rn_top_constraint hs_ctxt1
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
      = do { (hs_ctxt', fvs) <- mapFvRn rn_top_constraint hs_ctxt
           ; return (HsWC { hswc_wcs = concatMap collectAnonWildCards hs_ctxt'
                          , hswc_ctx = Nothing
                          , hswc_body = L loc hs_ctxt' }, fvs) }

    to_nwc :: LocalRdrEnv -> LHsType RdrName -> LHsType RdrName
    to_nwc _  lnwc@(L _ (HsWildCardTy {})) = lnwc
    to_nwc lcl_env (L loc (HsTyVar lname@(L _ rdr_name)))
      | rdr_name `inLocalRdrEnvNwcsRdrName` lcl_env
      = L loc (HsWildCardTy (NamedWildCard lname))
    to_nwc _ lt = lt

    rn_top_constraint = rnLHsTyKi RnTopConstraint ctxt


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
  = do { vars <- extract_filtered_rdr_ty_vars hs_ty
       ; rnImplicitBndrs True vars hs_ty $ \ vars ->
    do { (body', fvs) <- rnLHsType ctx hs_ty
       ; return (HsIB { hsib_vars = vars
                      , hsib_body = body' }, fvs) } }

rnImplicitBndrs :: Bool    -- True <=> no implicit quantification
                           --          if type is headed by a forall
                           -- E.g.  f :: forall a. a->b
                           -- Do not quantify over 'b' too.
                -> FreeKiTyVars
                -> LHsType RdrName
                -> ([Name] -> RnM (a, FreeVars))
                -> RnM (a, FreeVars)
rnImplicitBndrs no_implicit_if_forall free_vars hs_ty@(L loc _) thing_inside
  = do { let real_tv_rdrs  -- Implicit quantification only if
                           -- there is no explicit forall
               | no_implicit_if_forall
               , L _ (HsForAllTy {}) <- hs_ty = []
               | otherwise                    = freeKiTyVarsTypeVars free_vars
             real_rdrs = freeKiTyVarsKindVars free_vars ++ real_tv_rdrs
       ; traceRn (text "rnSigType" <+> (ppr hs_ty $$ ppr free_vars $$
                                        ppr real_rdrs))
       ; vars <- mapM (newLocalBndrRn . L loc . unLoc) real_rdrs
       ; bindLocalNamesFV vars $
         thing_inside vars }

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

This situation is now considered to be an error. See rnHsTyKi for case
HsForAllTy Qualified.

Note [Dealing with *]
~~~~~~~~~~~~~~~~~~~~~
As a legacy from the days when types and kinds were different, we use
the type * to mean what we now call GHC.Types.Type. The problem is that
* should associate just like an identifier, *not* a symbol.
Running example: the user has written

  T (Int, Bool) b + c * d

At this point, we have a bunch of stretches of types

  [[T, (Int, Bool), b], [c], [d]]

these are the [[LHsType Name]] and a bunch of operators

  [GHC.TypeLits.+, GHC.Types.*]

Note that the * is GHC.Types.*. So, we want to rearrange to have

  [[T, (Int, Bool), b], [c, *, d]]

and

  [GHC.TypeLits.+]

as our lists. We can then do normal fixity resolution on these. The fixities
must come along for the ride just so that the list stays in sync with the
operators.

Note [Renaming named wild cards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Identifiers starting with an underscore are always parsed as type variables.
(Parser.y) When the NamedWildCards extension is enabled, the renamer replaces
those variables with named wild cards.

The NameSet lre_nwcs in LocalRdrEnv is used to keep the names of the type
variables that should be replaced with named wild cards. The set is filled only
in functions that return a LHsWcType and thus expect to find wild cards.
In other functions, the set remains empty and the wild cards are not created.
Because of this, the replacement does not occur in contexts where the wild
cards are not expected, like data type declarations or type synonyms.
(See the comments in Trac #10982)

While renaming HsForAllTy (rnWcSigTy, rnHsTyKi), the explicitly bound names are
removed from the lre_nwcs NameSet. As a result, they are not replaced in the
quantifier body even if they start with an underscore. (Trac #11098) Eg

    qux :: _a -> (forall _a . _a -> _a) -> _a

The _a bound by forall is a tyvar, the _a outside the parens are wild cards.
-}

rnLHsTyKi  :: RnTyKiWhat
           -> HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnLHsTyKi what doc (L loc ty)
  = setSrcSpan loc $
    do { (ty', fvs) <- rnHsTyKi what doc ty
       ; return (L loc ty', fvs) }

rnLHsType  :: HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnLHsType cxt ty = -- pprTrace "rnHsType" (pprHsDocContext cxt $$ ppr ty) $
                   rnLHsTyKi (RnTypeBody TypeLevel) cxt ty

rnLHsPred  :: RnTyKiWhat -> HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnLHsPred (RnTypeBody level) = rnLHsTyKi (RnConstraint level)
rnLHsPred what               = rnLHsTyKi what

rnLHsKind  :: HsDocContext -> LHsKind RdrName -> RnM (LHsKind Name, FreeVars)
rnLHsKind = rnLHsTyKi (RnTypeBody KindLevel)

rnLHsMaybeKind  :: HsDocContext -> Maybe (LHsKind RdrName)
                -> RnM (Maybe (LHsKind Name), FreeVars)
rnLHsMaybeKind _ Nothing
  = return (Nothing, emptyFVs)
rnLHsMaybeKind doc (Just kind)
  = do { (kind', fvs) <- rnLHsKind doc kind
       ; return (Just kind', fvs) }

rnHsType  :: HsDocContext -> HsType RdrName -> RnM (HsType Name, FreeVars)
rnHsType cxt ty = rnHsTyKi (RnTypeBody TypeLevel) cxt ty

rnHsKind  :: HsDocContext -> HsKind RdrName -> RnM (HsKind Name, FreeVars)
rnHsKind = rnHsTyKi (RnTypeBody KindLevel)

data RnTyKiWhat = RnTypeBody TypeOrKind
                | RnTopConstraint           -- Top-level context of HsSigWcTypes
                | RnConstraint TypeOrKind   -- All other constraints

instance Outputable RnTyKiWhat where
  ppr (RnTypeBody lev)   = text "RnTypeBody" <+> ppr lev
  ppr RnTopConstraint    = text "RnTopConstraint"
  ppr (RnConstraint lev) = text "RnConstraint" <+> ppr lev

isRnKindLevel :: RnTyKiWhat -> Bool
isRnKindLevel (RnTypeBody KindLevel)   = True
isRnKindLevel (RnConstraint KindLevel) = True
isRnKindLevel _                        = False

rnHsTyKi :: RnTyKiWhat -> HsDocContext -> HsType RdrName -> RnM (HsType Name, FreeVars)

rnHsTyKi what doc ty@(HsForAllTy { hst_bndrs = tyvars, hst_body  = tau })
  = do { checkTypeInType what ty
       ; bindLHsTyVarBndrs doc Nothing [] tyvars $ \ _ tyvars' ->
    do { lcl_env <- getLocalRdrEnv
       ; let explicitly_bound = fmap hsLTyVarName tyvars'
       ; setLocalRdrEnv (delLocalRdrEnvNwcs lcl_env explicitly_bound) $
           -- See Note [Renaming named wild cards]
    do { (tau',  fvs) <- rnLHsTyKi what doc tau
       ; warnUnusedForAlls (inTypeDoc ty) tyvars' fvs
       ; return ( HsForAllTy { hst_bndrs = tyvars', hst_body =  tau' }
                , fvs) } } }

rnHsTyKi what doc ty@(HsQualTy { hst_ctxt = lctxt
                               , hst_body = tau })
  = do { checkTypeInType what ty
       ; (ctxt', fvs1) <- rnTyKiContext what doc lctxt
       ; (tau',  fvs2) <- rnLHsTyKi what doc tau
       ; return (HsQualTy { hst_ctxt = ctxt', hst_body =  tau' }
                , fvs1 `plusFV` fvs2) }

rnHsTyKi what doc (HsTyVar lname@(L loc rdr_name))
  = do { lcl_env <- getLocalRdrEnv
           -- See Note [Renaming named wild cards]
       ; if rdr_name `inLocalRdrEnvNwcsRdrName` lcl_env
         then rnHsTyKi what doc (HsWildCardTy (NamedWildCard lname))
         else do { name <- rnTyVar what rdr_name
                 ; return (HsTyVar (L loc name), unitFV name) } }

rnHsTyKi what doc ty@(HsOpTy ty1 l_op ty2)
  = setSrcSpan (getLoc l_op) $
    do  { (l_op', fvs1) <- rnHsTyOp what ty l_op
        ; fix   <- lookupTyFixityRn l_op'
        ; (ty1', fvs2) <- rnLHsTyKi what doc ty1
        ; (ty2', fvs3) <- rnLHsTyKi what doc ty2
        ; res_ty <- mkHsOpTyRn (\t1 t2 -> HsOpTy t1 l_op' t2)
                               (unLoc l_op') fix ty1' ty2'
        ; return (res_ty, plusFVs [fvs1, fvs2, fvs3]) }

rnHsTyKi what doc (HsParTy ty)
  = do { (ty', fvs) <- rnLHsTyKi what doc ty
       ; return (HsParTy ty', fvs) }

rnHsTyKi _ doc (HsBangTy b ty)
  = do { (ty', fvs) <- rnLHsType doc ty
       ; return (HsBangTy b ty', fvs) }

rnHsTyKi _ doc@(ConDeclCtx names) (HsRecTy flds)
  = do { fls <- concatMapM (lookupConstructorFields . unLoc) names
       ; (flds', fvs) <- rnConDeclFields fls doc flds
       ; return (HsRecTy flds', fvs) }

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
       ; res_ty <- mkHsOpTyRn HsFunTy funTyConName funTyFixity ty1' ty2'
       ; return (res_ty, fvs1 `plusFV` fvs2) }

rnHsTyKi what doc listTy@(HsListTy ty)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; when (not data_kinds && isRnKindLevel what)
              (addErr (dataKindsErr what listTy))
       ; (ty', fvs) <- rnLHsTyKi what doc ty
       ; return (HsListTy ty', fvs) }

rnHsTyKi what doc t@(HsKindSig ty k)
  = do { checkTypeInType what t
       ; kind_sigs_ok <- xoptM LangExt.KindSignatures
       ; unless kind_sigs_ok (badKindSigErr doc ty)
       ; (ty', fvs1) <- rnLHsTyKi what doc ty
       ; (k', fvs2) <- rnLHsKind doc k
       ; return (HsKindSig ty' k', fvs1 `plusFV` fvs2) }

rnHsTyKi what doc t@(HsPArrTy ty)
  = do { notInKinds what t
       ; (ty', fvs) <- rnLHsType doc ty
       ; return (HsPArrTy ty', fvs) }

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsTyKi what doc tupleTy@(HsTupleTy tup_con tys)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; when (not data_kinds && isRnKindLevel what)
              (addErr (dataKindsErr what tupleTy))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi what doc) tys
       ; return (HsTupleTy tup_con tys', fvs) }

-- Ensure that a type-level integer is nonnegative (#8306, #8412)
rnHsTyKi what _ tyLit@(HsTyLit t)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; unless data_kinds (addErr (dataKindsErr what tyLit))
       ; when (negLit t) (addErr negLitErr)
       ; checkTypeInType what tyLit
       ; return (HsTyLit t, emptyFVs) }
  where
    negLit (HsStrTy _ _) = False
    negLit (HsNumTy _ i) = i < 0
    negLitErr = ptext (sLit "Illegal literal in type (type literals must not be negative):") <+> ppr tyLit

rnHsTyKi isType doc overall_ty@(HsAppsTy tys)
  = do { -- Step 1: Break up the HsAppsTy into symbols and non-symbol regions
         let (non_syms, syms) = splitHsAppsTy tys

             -- Step 2: rename the pieces
       ; (syms1, fvs1)      <- mapFvRn (rnHsTyOp isType overall_ty) syms
       ; (non_syms1, fvs2)  <- (mapFvRn . mapFvRn) (rnLHsTyKi isType doc) non_syms

             -- Step 3: deal with *. See Note [Dealing with *]
       ; let (non_syms2, syms2) = deal_with_star [] [] non_syms1 syms1

             -- Step 4: collapse the non-symbol regions with HsAppTy
       ; non_syms3 <- mapM deal_with_non_syms non_syms2

             -- Step 5: assemble the pieces, using mkHsOpTyRn
       ; L _ res_ty <- build_res_ty non_syms3 syms2

        -- all done. Phew.
       ; return (res_ty, fvs1 `plusFV` fvs2) }
  where
    -- See Note [Dealing with *]
    deal_with_star :: [[LHsType Name]] -> [Located Name]
                   -> [[LHsType Name]] -> [Located Name]
                   -> ([[LHsType Name]], [Located Name])
    deal_with_star acc1 acc2
                   (non_syms1 : non_syms2 : non_syms) (L loc star : ops)
      | star `hasKey` starKindTyConKey || star `hasKey` unicodeStarKindTyConKey
      = deal_with_star acc1 acc2
                       ((non_syms1 ++ L loc (HsTyVar (L loc star)) : non_syms2) : non_syms)
                       ops
    deal_with_star acc1 acc2 (non_syms1 : non_syms) (op1 : ops)
      = deal_with_star (non_syms1 : acc1) (op1 : acc2) non_syms ops
    deal_with_star acc1 acc2 [non_syms] []
      = (reverse (non_syms : acc1), reverse acc2)
    deal_with_star _ _ _ _
      = pprPanic "deal_with_star" (ppr overall_ty)

    -- collapse [LHsType Name] to LHsType Name by making applications
    -- monadic only for failure
    deal_with_non_syms :: [LHsType Name] -> RnM (LHsType Name)
    deal_with_non_syms (non_sym : non_syms) = return $ mkHsAppTys non_sym non_syms
    deal_with_non_syms []                   = failWith (emptyNonSymsErr overall_ty)

    -- assemble a right-biased OpTy for use in mkHsOpTyRn
    build_res_ty :: [LHsType Name] -> [Located Name] -> RnM (LHsType Name)
    build_res_ty (arg1 : args) (op1 : ops)
      = do { rhs <- build_res_ty args ops
           ; fix <- lookupTyFixityRn op1
           ; res <-
               mkHsOpTyRn (\t1 t2 -> HsOpTy t1 op1 t2) (unLoc op1) fix arg1 rhs
           ; let loc = combineSrcSpans (getLoc arg1) (getLoc rhs)
           ; return (L loc res)
           }
    build_res_ty [arg] [] = return arg
    build_res_ty _ _ = pprPanic "build_op_ty" (ppr overall_ty)

rnHsTyKi what doc (HsAppTy ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi what doc ty1
       ; (ty2', fvs2) <- rnLHsTyKi what doc ty2
       ; return (HsAppTy ty1' ty2', fvs1 `plusFV` fvs2) }

rnHsTyKi what doc t@(HsIParamTy n ty)
  = do { notInKinds what t
       ; (ty', fvs) <- rnLHsType doc ty
       ; return (HsIParamTy n ty', fvs) }

rnHsTyKi what doc t@(HsEqTy ty1 ty2)
  = do { checkTypeInType what t
       ; (ty1', fvs1) <- rnLHsTyKi what doc ty1
       ; (ty2', fvs2) <- rnLHsTyKi what doc ty2
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

rnHsTyKi what doc ty@(HsExplicitListTy k tys)
  = do { checkTypeInType what ty
       ; data_kinds <- xoptM LangExt.DataKinds
       ; unless data_kinds (addErr (dataKindsErr what ty))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi what doc) tys
       ; return (HsExplicitListTy k tys', fvs) }

rnHsTyKi what doc ty@(HsExplicitTupleTy kis tys)
  = do { checkTypeInType what ty
       ; data_kinds <- xoptM LangExt.DataKinds
       ; unless data_kinds (addErr (dataKindsErr what ty))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi what doc) tys
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
               RnTypeBody _    -> Nothing
               RnConstraint _  -> Just constraint_msg
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
  | isRnKindLevel what = lookupKindOccRn rdr_name
  | otherwise          = lookupTypeOccRn rdr_name

rnLTyVar :: Located RdrName -> RnM (Located Name)
rnLTyVar (L loc rdr_name)
  = do { tyvar <- lookupTypeOccRn rdr_name
       ; return (L loc tyvar) }

--------------
rnHsTyOp :: Outputable a
         => RnTyKiWhat -> a -> Located RdrName -> RnM (Located Name, FreeVars)
rnHsTyOp what overall_ty (L loc op)
  = do { ops_ok <- xoptM LangExt.TypeOperators
       ; op' <- rnTyVar what op
       ; unless (ops_ok
                 || op' == starKindTyConName
                 || op' == unicodeStarKindTyConName
                 || op' `hasKey` eqTyConKey) $
           addErr (opTyErr op overall_ty)
       ; let l_op' = L loc op'
       ; return (l_op', unitFV op') }

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
       ; return (AnonWildCard (L loc name)) }

rnWildCard ctxt wc@(NamedWildCard (L loc rdr_name))
  -- NB: The parser only generates NamedWildCard if -XNamedWildCards
  --     is on, so we don't need to check for that here
  = do { mb_name <- lookupOccRn_maybe rdr_name
       ; traceRn (text "rnWildCard named" <+> (ppr rdr_name $$ ppr mb_name))
       ; case mb_name of
           Just n  -> return (NamedWildCard (L loc n))
           Nothing -> do { addErr msg  -- I'm not sure how this can happen
                         ; return (NamedWildCard (L loc (mkUnboundNameRdr rdr_name))) } }
  where
    msg = wildCardMsg ctxt (notAllowed wc)


---------------
-- | Ensures either that we're in a type or that -XTypeInType is set
checkTypeInType :: Outputable ty
                => RnTyKiWhat
                -> ty      -- ^ type
                -> RnM ()
checkTypeInType what ty
  | isRnKindLevel what
  = do { type_in_type <- xoptM LangExt.TypeInType
       ; unless type_in_type $
         addErr (text "Illegal kind:" <+> ppr ty $$
                 text "Did you mean to enable TypeInType?") }
checkTypeInType _ _ = return ()

notInKinds :: Outputable ty
           => RnTyKiWhat
           -> ty
           -> RnM ()
notInKinds what ty
  | isRnKindLevel what
  = addErr (text "Illegal kind (even with TypeInType enabled):" <+> ppr ty)
notInKinds _ _ = return ()

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
  = do  { scoped_tyvars <- xoptM LangExt.ScopedTypeVariables
        ; if not scoped_tyvars then
                thing_inside
          else
                bindLocalNamesFV tvs thing_inside }

-- | Simply bring a bunch of RdrNames into scope. No checking for
-- validity, at all. The binding location is taken from the location
-- on each name.
bindLRdrNames :: [Located RdrName]
              -> ([Name] -> RnM (a, FreeVars))
              -> RnM (a, FreeVars)
bindLRdrNames rdrs thing_inside
  = do { var_names <- mapM (newTyVarNameRn Nothing) rdrs
       ; bindLocalNamesFV var_names $
         thing_inside var_names }

---------------
bindHsQTyVars :: forall a b.
                 HsDocContext
              -> Maybe a                 -- Just _  => an associated type decl
              -> [Located RdrName]       -- Kind variables from scope, in l-to-r
                                         -- order, but not from ...
              -> (LHsQTyVars RdrName)     -- ... these user-written tyvars
              -> (LHsQTyVars Name -> RnM (b, FreeVars))
              -> RnM (b, FreeVars)
-- (a) Bring kind variables into scope
--     both (i)  passed in (kv_bndrs)
--     and  (ii) mentioned in the kinds of tv_bndrs
-- (b) Bring type variables into scope
bindHsQTyVars doc mb_assoc kv_bndrs tv_bndrs thing_inside
  = do { bindLHsTyVarBndrs doc mb_assoc kv_bndrs (hsQTvExplicit tv_bndrs) $
         \ rn_kvs rn_bndrs ->
         thing_inside (HsQTvs { hsq_implicit = rn_kvs
                              , hsq_explicit = rn_bndrs }) }

bindLHsTyVarBndrs :: forall a b.
                     HsDocContext
                  -> Maybe a            -- Just _  => an associated type decl
                  -> [Located RdrName]  -- Unbound kind variables from scope,
                                        -- in l-to-r order, but not from ...
                  -> [LHsTyVarBndr RdrName]  -- ... these user-written tyvars
                  -> (   [Name]  -- all kv names
                      -> [LHsTyVarBndr Name]
                      -> RnM (b, FreeVars))
                  -> RnM (b, FreeVars)
bindLHsTyVarBndrs doc mb_assoc kv_bndrs tv_bndrs thing_inside
  = do { when (isNothing mb_assoc) (checkShadowedRdrNames tv_names_w_loc)
       ; go [] [] emptyNameSet emptyNameSet tv_bndrs }
  where
    tv_names_w_loc = map hsLTyVarLocName tv_bndrs

    go :: [Name]                 -- kind-vars found (in reverse order)
       -> [LHsTyVarBndr Name]    -- already renamed (in reverse order)
       -> NameSet                -- kind vars already in scope (for dup checking)
       -> NameSet                -- type vars already in scope (for dup checking)
       -> [LHsTyVarBndr RdrName] -- still to be renamed, scoped
       -> RnM (b, FreeVars)
    go rn_kvs rn_tvs kv_names tv_names (tv_bndr : tv_bndrs)
      = bindLHsTyVarBndr doc mb_assoc kv_names tv_names tv_bndr $
        \ kv_nms tv_bndr' -> go (reverse kv_nms ++ rn_kvs)
                                (tv_bndr' : rn_tvs)
                                (kv_names `extendNameSetList` kv_nms)
                                (tv_names `extendNameSet` hsLTyVarName tv_bndr')
                                tv_bndrs

    go rn_kvs rn_tvs _kv_names tv_names []
      = -- still need to deal with the kv_bndrs passed in originally
        bindImplicitKvs doc mb_assoc kv_bndrs tv_names $ \ kv_nms ->
        do { let all_rn_kvs = reverse (reverse kv_nms ++ rn_kvs)
                 all_rn_tvs = reverse rn_tvs
           ; env <- getLocalRdrEnv
           ; traceRn (text "bindHsTyVars" <+> (ppr env $$
                                               ppr all_rn_kvs $$
                                               ppr all_rn_tvs))
           ; thing_inside all_rn_kvs all_rn_tvs }

bindLHsTyVarBndr :: HsDocContext
                 -> Maybe a   -- associated class
                 -> NameSet   -- kind vars already in scope
                 -> NameSet   -- type vars already in scope
                 -> LHsTyVarBndr RdrName
                 -> ([Name] -> LHsTyVarBndr Name -> RnM (b, FreeVars))
                   -- passed the newly-bound implicitly-declared kind vars,
                   -- and the renamed LHsTyVarBndr
                 -> RnM (b, FreeVars)
bindLHsTyVarBndr doc mb_assoc kv_names tv_names hs_tv_bndr thing_inside
  = case hs_tv_bndr of
      L loc (UserTyVar lrdr@(L lv rdr)) ->
        do { check_dup loc rdr
           ; nm <- newTyVarNameRn mb_assoc lrdr
           ; bindLocalNamesFV [nm] $
             thing_inside [] (L loc (UserTyVar (L lv nm))) }
      L loc (KindedTyVar lrdr@(L lv rdr) kind) ->
        do { check_dup lv rdr

             -- check for -XKindSignatures
           ; sig_ok <- xoptM LangExt.KindSignatures
           ; unless sig_ok (badKindSigErr doc kind)

             -- deal with kind vars in the user-written kind
           ; free_kvs <- freeKiTyVarsAllVars <$> extractHsTyRdrTyVars kind
           ; bindImplicitKvs doc mb_assoc free_kvs tv_names $ \ kv_nms ->
             do { (kind', fvs1) <- rnLHsKind doc kind
                ; tv_nm  <- newTyVarNameRn mb_assoc lrdr
                ; (b, fvs2) <- bindLocalNamesFV [tv_nm] $
                               thing_inside kv_nms
                                 (L loc (KindedTyVar (L lv tv_nm) kind'))
                ; return (b, fvs1 `plusFV` fvs2) }}
  where
      -- make sure that the RdrName isn't in the sets of
      -- names. We can't just check that it's not in scope at all
      -- because we might be inside an associated class.
    check_dup :: SrcSpan -> RdrName -> RnM ()
    check_dup loc rdr
      = do { m_name <- lookupLocalOccRn_maybe rdr
           ; whenIsJust m_name $ \name ->
        do { when (name `elemNameSet` kv_names) $
             addErrAt loc (vcat [ ki_ty_err_msg name
                                , pprHsDocContext doc ])
           ; when (name `elemNameSet` tv_names) $
             dupNamesErr getLoc [L loc name, L (nameSrcSpan name) name] }}

    ki_ty_err_msg n = text "Variable" <+> quotes (ppr n) <+>
                      text "used as a kind variable before being bound" $$
                      text "as a type variable. Perhaps reorder your variables?"


bindImplicitKvs :: HsDocContext
                -> Maybe a
                -> [Located RdrName]  -- ^ kind var *occurrences*, from which
                                      -- intent to bind is inferred
                -> NameSet            -- ^ *type* variables, for type/kind
                                      -- misuse check for -XNoTypeInType
                -> ([Name] -> RnM (b, FreeVars)) -- ^ passed new kv_names
                -> RnM (b, FreeVars)
bindImplicitKvs _   _        []       _        thing_inside = thing_inside []
bindImplicitKvs doc mb_assoc free_kvs tv_names thing_inside
  = do { rdr_env <- getLocalRdrEnv
       ; let part_kvs lrdr@(L loc kv_rdr)
               = case lookupLocalRdrEnv rdr_env kv_rdr of
                   Just kv_name -> Left (L loc kv_name)
                   _            -> Right lrdr
             (bound_kvs, new_kvs) = partitionWith part_kvs free_kvs

          -- check whether we're mixing types & kinds illegally
       ; type_in_type <- xoptM LangExt.TypeInType
       ; unless type_in_type $
         mapM_ (check_tv_used_in_kind tv_names) bound_kvs

       ; poly_kinds <- xoptM LangExt.PolyKinds
       ; unless poly_kinds $
         addErr (badKindBndrs doc new_kvs)

          -- bind the vars and move on
       ; kv_nms <- mapM (newTyVarNameRn mb_assoc) new_kvs
       ; bindLocalNamesFV kv_nms $
         thing_inside kv_nms }
  where
      -- check to see if the variables free in a kind are bound as type
      -- variables. Assume -XNoTypeInType.
    check_tv_used_in_kind :: NameSet       -- ^ *type* variables
                          -> Located Name  -- ^ renamed var used in kind
                          -> RnM ()
    check_tv_used_in_kind tv_names (L loc kv_name)
      = when (kv_name `elemNameSet` tv_names) $
        addErrAt loc (vcat [ text "Type variable" <+> quotes (ppr kv_name) <+>
                             text "used in a kind." $$
                             text "Did you mean to use TypeInType?"
                           , pprHsDocContext doc ])


newTyVarNameRn :: Maybe a -> Located RdrName -> RnM Name
newTyVarNameRn mb_assoc (L loc rdr)
  = do { rdr_env <- getLocalRdrEnv
       ; case (mb_assoc, lookupLocalRdrEnv rdr_env rdr) of
           (Just _, Just n) -> return n
              -- Use the same Name as the parent class decl

           _                -> newLocalBndrRn (L loc rdr) }

---------------------
collectNamedWildCards :: LHsType RdrName -> [Located RdrName]
collectNamedWildCards hs_ty
  = nubBy eqLocated $
    [n | L _ (NamedWildCard n) <- collectWildCards hs_ty ]

collectAnonWildCards :: LHsType Name -> [Name]
collectAnonWildCards hs_ty
  = [n | L _ (AnonWildCard (L _ n)) <- collectWildCards hs_ty ]

collectWildCards :: LHsType name -> [Located (HsWildCardInfo name)]
-- | Extract all wild cards from a type.
collectWildCards lty = go lty
  where
    go (L loc ty) = case ty of
      HsAppsTy tys            -> gos (mapMaybe prefix_types_only tys)
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
      -- Interesting cases
      HsWildCardTy wc         -> [L loc wc]
      HsForAllTy { hst_body = ty } -> go ty
      HsQualTy { hst_ctxt = L _ ctxt
               , hst_body = ty }  -> gos ctxt `mappend` go ty
      -- HsQuasiQuoteTy, HsSpliceTy, HsCoreTy, HsTyLit
      _ -> mempty

    gos = mconcat . map go

    prefix_types_only (HsAppPrefix ty) = Just ty
    prefix_types_only (HsAppInfix _)   = Nothing


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
    lookupField (FieldOcc (L lr rdr) _) = FieldOcc (L lr rdr) (flSelector fl)
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

rnTyKiContext :: RnTyKiWhat
              -> HsDocContext -> LHsContext RdrName -> RnM (LHsContext Name, FreeVars)
rnTyKiContext what doc (L loc cxt)
  = do { traceRn (text "rncontext" <+> ppr cxt)
       ; (cxt', fvs) <- mapFvRn (rnLHsPred what doc) cxt
       ; return (L loc cxt', fvs) }

rnContext :: HsDocContext -> LHsContext RdrName -> RnM (LHsContext Name, FreeVars)
rnContext = rnTyKiContext (RnConstraint TypeLevel)

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

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsOpTy ty21 op2 ty22))
  = do  { fix2 <- lookupTyFixityRn op2
        ; mk_hs_op_ty mk1 pp_op1 fix1 ty1
                      (\t1 t2 -> HsOpTy t1 op2 t2)
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
get_op (L _ (HsVar (L _ n)))    = n
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

checkPrecMatch op (MG { mg_alts = L _ ms })
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

badKindBndrs :: HsDocContext -> [Located RdrName] -> SDoc
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
    pp_what | isRnKindLevel what = ptext (sLit "kind")
            | otherwise          = ptext (sLit "type")

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

opTyErr :: Outputable a => RdrName -> a -> SDoc
opTyErr op overall_ty
  = hang (ptext (sLit "Illegal operator") <+> quotes (ppr op) <+> ptext (sLit "in type") <+> quotes (ppr overall_ty))
         2 extra
  where
    extra | op == dot_tv_RDR
          = perhapsForallMsg
          | otherwise
          = ptext (sLit "Use TypeOperators to allow operators in types")

emptyNonSymsErr :: HsType RdrName -> SDoc
emptyNonSymsErr overall_ty
  = text "Operator applied to too few arguments:" <+> ppr overall_ty

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

data FreeKiTyVars = FKTV { fktv_kis    :: [Located RdrName]
                         , _fktv_k_set :: OccSet  -- for efficiency,
                                                  -- only used internally
                         , fktv_tys    :: [Located RdrName]
                         , _fktv_t_set :: OccSet
                         , fktv_all    :: [Located RdrName] }

instance Outputable FreeKiTyVars where
  ppr (FKTV kis _ tys _ _) = ppr (kis, tys)

emptyFKTV :: FreeKiTyVars
emptyFKTV = FKTV [] emptyOccSet [] emptyOccSet []

freeKiTyVarsAllVars :: FreeKiTyVars -> [Located RdrName]
freeKiTyVarsAllVars = fktv_all

freeKiTyVarsKindVars :: FreeKiTyVars -> [Located RdrName]
freeKiTyVarsKindVars = fktv_kis

freeKiTyVarsTypeVars :: FreeKiTyVars -> [Located RdrName]
freeKiTyVarsTypeVars = fktv_tys

filterInScope :: LocalRdrEnv -> FreeKiTyVars -> FreeKiTyVars
filterInScope rdr_env (FKTV kis k_set tys t_set all)
  = FKTV (filterOut in_scope kis)
         (filterOccSet (not . in_scope_occ) k_set)
         (filterOut in_scope tys)
         (filterOccSet (not . in_scope_occ) t_set)
         (filterOut in_scope all)
  where
    in_scope         = inScope rdr_env . unLoc
    in_scope_occ occ = isJust $ lookupLocalRdrOcc rdr_env occ

inScope :: LocalRdrEnv -> RdrName -> Bool
inScope rdr_env rdr = rdr `elemLocalRdrEnv` rdr_env

extractHsTyRdrTyVars :: LHsType RdrName -> RnM FreeKiTyVars
-- extractHsTyRdrNames finds the free (kind, type) variables of a HsType
--                        or the free (sort, kind) variables of a HsKind
-- It's used when making the for-alls explicit.
-- Does not return any wildcards
-- When the same name occurs multiple times in the types, only the first
-- occurence is returned.
-- See Note [Kind and type-variable binders]
extractHsTyRdrTyVars ty
  = do { FKTV kis k_set tys t_set all <- extract_lty TypeLevel ty emptyFKTV
       ; return (FKTV (nubL kis) k_set
                      (nubL tys) t_set
                      (nubL all)) }

-- | Extracts free type and kind variables from types in a list.
-- When the same name occurs multiple times in the types, only the first
-- occurence is returned and the rest is filtered out.
-- See Note [Kind and type-variable binders]
extractHsTysRdrTyVars :: [LHsType RdrName] -> RnM FreeKiTyVars
extractHsTysRdrTyVars tys
  = rmDupsInRdrTyVars <$> extractHsTysRdrTyVarsDups tys

-- | Extracts free type and kind variables from types in a list.
-- When the same name occurs multiple times in the types, all occurences
-- are returned.
extractHsTysRdrTyVarsDups :: [LHsType RdrName] -> RnM FreeKiTyVars
extractHsTysRdrTyVarsDups tys
  = extract_ltys TypeLevel tys emptyFKTV

-- | Removes multiple occurences of the same name from FreeKiTyVars.
rmDupsInRdrTyVars :: FreeKiTyVars -> FreeKiTyVars
rmDupsInRdrTyVars (FKTV kis k_set tys t_set all)
  = FKTV (nubL kis) k_set (nubL tys) t_set (nubL all)

extractRdrKindSigVars :: LFamilyResultSig RdrName -> RnM [Located RdrName]
extractRdrKindSigVars (L _ resultSig)
    | KindSig k                        <- resultSig = kindRdrNameFromSig k
    | TyVarSig (L _ (KindedTyVar _ k)) <- resultSig = kindRdrNameFromSig k
    | otherwise = return []
    where kindRdrNameFromSig k = freeKiTyVarsAllVars <$> extractHsTyRdrTyVars k

extractDataDefnKindVars :: HsDataDefn RdrName -> RnM [Located RdrName]
-- Get the scoped kind variables mentioned free in the constructor decls
-- Eg    data T a = T1 (S (a :: k) | forall (b::k). T2 (S b)
-- Here k should scope over the whole definition
extractDataDefnKindVars (HsDataDefn { dd_ctxt = ctxt, dd_kindSig = ksig
                                    , dd_cons = cons, dd_derivs = derivs })
  = (nubL . freeKiTyVarsKindVars) <$>
    (extract_lctxt TypeLevel ctxt =<<
     extract_mb extract_lkind ksig =<<
     extract_mb (extract_sig_tys . unLoc) derivs =<<
     foldrM (extract_con . unLoc) emptyFKTV cons)
  where
    extract_con (ConDeclGADT { }) acc = return acc
    extract_con (ConDeclH98 { con_qvars = qvs
                            , con_cxt = ctxt, con_details = details }) acc
      = extract_hs_tv_bndrs (maybe [] hsQTvExplicit qvs) acc =<<
        extract_mlctxt ctxt =<<
        extract_ltys TypeLevel (hsConDeclArgTys details) emptyFKTV

extract_mlctxt :: Maybe (LHsContext RdrName) -> FreeKiTyVars -> RnM FreeKiTyVars
extract_mlctxt Nothing     acc = return acc
extract_mlctxt (Just ctxt) acc = extract_lctxt TypeLevel ctxt acc

extract_lctxt :: TypeOrKind
              -> LHsContext RdrName -> FreeKiTyVars -> RnM FreeKiTyVars
extract_lctxt t_or_k ctxt = extract_ltys t_or_k (unLoc ctxt)

extract_sig_tys :: [LHsSigType RdrName] -> FreeKiTyVars -> RnM FreeKiTyVars
extract_sig_tys sig_tys acc
  = foldrM (\sig_ty acc -> extract_lty TypeLevel (hsSigType sig_ty) acc)
           acc sig_tys

extract_ltys :: TypeOrKind
             -> [LHsType RdrName] -> FreeKiTyVars -> RnM FreeKiTyVars
extract_ltys t_or_k tys acc = foldrM (extract_lty t_or_k) acc tys

extract_mb :: (a -> FreeKiTyVars -> RnM FreeKiTyVars)
           -> Maybe a -> FreeKiTyVars -> RnM FreeKiTyVars
extract_mb _ Nothing  acc = return acc
extract_mb f (Just x) acc = f x acc

extract_lkind :: LHsType RdrName -> FreeKiTyVars -> RnM FreeKiTyVars
extract_lkind = extract_lty KindLevel

extract_lty :: TypeOrKind -> LHsType RdrName -> FreeKiTyVars -> RnM FreeKiTyVars
extract_lty t_or_k (L _ ty) acc
  = case ty of
      HsTyVar ltv               -> extract_tv t_or_k ltv acc
      HsBangTy _ ty             -> extract_lty t_or_k ty acc
      HsRecTy flds              -> foldrM (extract_lty t_or_k
                                           . cd_fld_type . unLoc) acc
                                         flds
      HsAppsTy tys              -> extract_apps t_or_k tys acc
      HsAppTy ty1 ty2           -> extract_lty t_or_k ty1 =<<
                                   extract_lty t_or_k ty2 acc
      HsListTy ty               -> extract_lty t_or_k ty acc
      HsPArrTy ty               -> extract_lty t_or_k ty acc
      HsTupleTy _ tys           -> extract_ltys t_or_k tys acc
      HsFunTy ty1 ty2           -> extract_lty t_or_k ty1 =<<
                                   extract_lty t_or_k ty2 acc
      HsIParamTy _ ty           -> extract_lty t_or_k ty acc
      HsEqTy ty1 ty2            -> extract_lty t_or_k ty1 =<<
                                   extract_lty t_or_k ty2 acc
      HsOpTy ty1 tv ty2         -> extract_tv t_or_k tv =<<
                                   extract_lty t_or_k ty1 =<<
                                   extract_lty t_or_k ty2 acc
      HsParTy ty                -> extract_lty t_or_k ty acc
      HsCoreTy {}               -> return acc  -- The type is closed
      HsSpliceTy {}             -> return acc  -- Type splices mention no tvs
      HsDocTy ty _              -> extract_lty t_or_k ty acc
      HsExplicitListTy _ tys    -> extract_ltys t_or_k tys acc
      HsExplicitTupleTy _ tys   -> extract_ltys t_or_k tys acc
      HsTyLit _                 -> return acc
      HsKindSig ty ki           -> extract_lty t_or_k ty =<<
                                   extract_lkind ki acc
      HsForAllTy { hst_bndrs = tvs, hst_body = ty }
                                -> extract_hs_tv_bndrs tvs acc =<<
                                   extract_lty t_or_k ty emptyFKTV
      HsQualTy { hst_ctxt = ctxt, hst_body = ty }
                                -> extract_lctxt t_or_k ctxt   =<<
                                   extract_lty t_or_k ty acc
      -- We deal with these separately in rnLHsTypeWithWildCards
      HsWildCardTy {}           -> return acc

extract_apps :: TypeOrKind
             -> [HsAppType RdrName] -> FreeKiTyVars -> RnM FreeKiTyVars
extract_apps t_or_k tys acc = foldrM (extract_app t_or_k) acc tys

extract_app :: TypeOrKind -> HsAppType RdrName -> FreeKiTyVars -> RnM FreeKiTyVars
extract_app t_or_k (HsAppInfix tv)  acc = extract_tv t_or_k tv acc
extract_app t_or_k (HsAppPrefix ty) acc = extract_lty t_or_k ty acc

extract_hs_tv_bndrs :: [LHsTyVarBndr RdrName] -> FreeKiTyVars
                    -> FreeKiTyVars -> RnM FreeKiTyVars
-- In (forall (a :: Maybe e). a -> b) we have
--     'a' is bound by the forall
--     'b' is a free type variable
--     'e' is a free kind variable
extract_hs_tv_bndrs tvs
                    (FKTV acc_kvs acc_k_set acc_tvs acc_t_set acc_all)
                           -- Note accumulator comes first
                    (FKTV body_kvs body_k_set body_tvs body_t_set body_all)
  | null tvs
  = return $
    FKTV (body_kvs ++ acc_kvs) (body_k_set `unionOccSets` acc_k_set)
         (body_tvs ++ acc_tvs) (body_t_set `unionOccSets` acc_t_set)
         (body_all ++ acc_all)
  | otherwise
  = do { FKTV bndr_kvs bndr_k_set _ _ _
           <- foldrM extract_lkind emptyFKTV [k | L _ (KindedTyVar _ k) <- tvs]

       ; let locals = mkOccSet $ map (rdrNameOcc . hsLTyVarName) tvs
       ; return $
         FKTV (filterOut ((`elemOccSet` locals) . rdrNameOcc . unLoc) (bndr_kvs ++ body_kvs) ++ acc_kvs)
              ((body_k_set `minusOccSet` locals) `unionOccSets` acc_k_set `unionOccSets` bndr_k_set)
              (filterOut ((`elemOccSet` locals) . rdrNameOcc . unLoc) body_tvs ++ acc_tvs)
              ((body_t_set `minusOccSet` locals) `unionOccSets` acc_t_set)
              (filterOut ((`elemOccSet` locals) . rdrNameOcc . unLoc) (bndr_kvs ++ body_all) ++ acc_all) }

extract_tv :: TypeOrKind -> Located RdrName -> FreeKiTyVars -> RnM FreeKiTyVars
extract_tv t_or_k ltv@(L _ tv) acc
  | isRdrTyVar tv = case acc of
      FKTV kvs k_set tvs t_set all
        |  isTypeLevel t_or_k
        -> do { when (occ `elemOccSet` k_set) $
                mixedVarsErr ltv
              ; return (FKTV kvs k_set (ltv : tvs) (t_set `extendOccSet` occ)
                             (ltv : all)) }
        |  otherwise
        -> do { when (occ `elemOccSet` t_set) $
                mixedVarsErr ltv
              ; return (FKTV (ltv : kvs) (k_set `extendOccSet` occ) tvs t_set
                             (ltv : all)) }
  | otherwise     = return acc
  where
    occ = rdrNameOcc tv

mixedVarsErr :: Located RdrName -> RnM ()
mixedVarsErr (L loc tv)
  = do { typeintype <- xoptM LangExt.TypeInType
       ; unless typeintype $
         addErrAt loc $ text "Variable" <+> quotes (ppr tv) <+>
                        text "used as both a kind and a type" $$
                        text "Did you intend to use TypeInType?" }

-- just used in this module; seemed convenient here
nubL :: Eq a => [Located a] -> [Located a]
nubL = nubBy eqLocated
