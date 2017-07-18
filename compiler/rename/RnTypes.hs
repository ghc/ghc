{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[RnSource]{Main pass of renamer}
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module RnTypes (
        -- Type related stuff
        rnHsType, rnLHsType, rnLHsTypes, rnContext,
        rnHsKind, rnLHsKind,
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
        bindLHsTyVarBndr,
        bindSigTyVarsFV, bindHsQTyVars, bindLRdrNames,
        extractFilteredRdrTyVars,
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
import RnUnbound        ( perhapsForallMsg )
import RnUtils          ( HsDocContext(..), withHsDocContext, mapFvRn
                        , pprHsDocContext, bindLocalNamesFV, dupNamesErr
                        , newLocalBndrRn, checkShadowedRdrNames )
import RnFixity         ( lookupFieldFixityRn, lookupFixityRn
                        , lookupTyFixityRn )
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
                          Fixity(..), FixityDirection(..), LexicalFixity(..) )
import Outputable
import FastString
import Maybes
import qualified GHC.LanguageExtensions as LangExt

import Data.List        ( nubBy, partition )
import Control.Monad    ( unless, when )

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

rnHsSigWcType :: HsDocContext -> LHsSigWcType GhcPs
            -> RnM (LHsSigWcType GhcRn, FreeVars)
rnHsSigWcType doc sig_ty
  = rn_hs_sig_wc_type True doc sig_ty $ \sig_ty' ->
    return (sig_ty', emptyFVs)

rnHsSigWcTypeScoped :: HsDocContext -> LHsSigWcType GhcPs
                    -> (LHsSigWcType GhcRn -> RnM (a, FreeVars))
                    -> RnM (a, FreeVars)
-- Used for
--   - Signatures on binders in a RULE
--   - Pattern type signatures
-- Wildcards are allowed
-- type signatures on binders only allowed with ScopedTypeVariables
rnHsSigWcTypeScoped ctx sig_ty thing_inside
  = do { ty_sig_okay <- xoptM LangExt.ScopedTypeVariables
       ; checkErr ty_sig_okay (unexpectedTypeSigErr sig_ty)
       ; rn_hs_sig_wc_type False ctx sig_ty thing_inside
       }
    -- False: for pattern type sigs and rules we /do/ want
    --        to bring those type variables into scope
    -- e.g  \ (x :: forall a. a-> b) -> e
    -- Here we do bring 'b' into scope

rn_hs_sig_wc_type :: Bool   -- see rnImplicitBndrs
                  -> HsDocContext
                  -> LHsSigWcType GhcPs
                  -> (LHsSigWcType GhcRn -> RnM (a, FreeVars))
                  -> RnM (a, FreeVars)
-- rn_hs_sig_wc_type is used for source-language type signatures
rn_hs_sig_wc_type no_implicit_if_forall ctxt
                  (HsWC { hswc_body = HsIB { hsib_body = hs_ty }})
                  thing_inside
  = do { free_vars <- extractFilteredRdrTyVars hs_ty
       ; (tv_rdrs, nwc_rdrs) <- partition_nwcs free_vars
       ; rnImplicitBndrs no_implicit_if_forall tv_rdrs hs_ty $ \ vars ->
    do { (wcs, hs_ty', fvs1) <- rnWcBody ctxt nwc_rdrs hs_ty
       ; let sig_ty' = HsWC { hswc_wcs = wcs, hswc_body = ib_ty' }
             ib_ty'  = mk_implicit_bndrs vars hs_ty' fvs1
       ; (res, fvs2) <- thing_inside sig_ty'
       ; return (res, fvs1 `plusFV` fvs2) } }

rnHsWcType :: HsDocContext -> LHsWcType GhcPs -> RnM (LHsWcType GhcRn, FreeVars)
rnHsWcType ctxt (HsWC { hswc_body = hs_ty })
  = do { free_vars <- extractFilteredRdrTyVars hs_ty
       ; (_, nwc_rdrs) <- partition_nwcs free_vars
       ; (wcs, hs_ty', fvs) <- rnWcBody ctxt nwc_rdrs hs_ty
       ; let sig_ty' = HsWC { hswc_wcs = wcs, hswc_body = hs_ty' }
       ; return (sig_ty', fvs) }

rnWcBody :: HsDocContext -> [Located RdrName] -> LHsType GhcPs
         -> RnM ([Name], LHsType GhcRn, FreeVars)
rnWcBody ctxt nwc_rdrs hs_ty
  = do { nwcs <- mapM newLocalBndrRn nwc_rdrs
       ; let env = RTKE { rtke_level = TypeLevel
                        , rtke_what  = RnTypeBody
                        , rtke_nwcs  = mkNameSet nwcs
                        , rtke_ctxt  = ctxt }
       ; (hs_ty', fvs) <- bindLocalNamesFV nwcs $
                          rn_lty env hs_ty
       ; let awcs = collectAnonWildCards hs_ty'
       ; return (nwcs ++ awcs, hs_ty', fvs) }
  where
    rn_lty env (L loc hs_ty)
      = setSrcSpan loc $
        do { (hs_ty', fvs) <- rn_ty env hs_ty
           ; return (L loc hs_ty', fvs) }

    rn_ty :: RnTyKiEnv -> HsType GhcPs -> RnM (HsType GhcRn, FreeVars)
    -- A lot of faff just to allow the extra-constraints wildcard to appear
    rn_ty env hs_ty@(HsForAllTy { hst_bndrs = tvs, hst_body = hs_body })
      = bindLHsTyVarBndrs (rtke_ctxt env) (Just $ inTypeDoc hs_ty)
                           Nothing [] tvs $ \ _ tvs' _ _ ->
        do { (hs_body', fvs) <- rn_lty env hs_body
           ; return (HsForAllTy { hst_bndrs = tvs', hst_body = hs_body' }, fvs) }

    rn_ty env (HsQualTy { hst_ctxt = L cx hs_ctxt, hst_body = hs_ty })
      | Just (hs_ctxt1, hs_ctxt_last) <- snocView hs_ctxt
      , L lx (HsWildCardTy wc) <- ignoreParens hs_ctxt_last
      = do { (hs_ctxt1', fvs1) <- mapFvRn (rn_top_constraint env) hs_ctxt1
           ; wc' <- setSrcSpan lx $
                    do { checkExtraConstraintWildCard env wc
                       ; rnAnonWildCard wc }
           ; let hs_ctxt' = hs_ctxt1' ++ [L lx (HsWildCardTy wc')]
           ; (hs_ty', fvs2) <- rnLHsTyKi env hs_ty
           ; return (HsQualTy { hst_ctxt = L cx hs_ctxt', hst_body = hs_ty' }
                    , fvs1 `plusFV` fvs2) }

      | otherwise
      = do { (hs_ctxt', fvs1) <- mapFvRn (rn_top_constraint env) hs_ctxt
           ; (hs_ty', fvs2)   <- rnLHsTyKi env hs_ty
           ; return (HsQualTy { hst_ctxt = L cx hs_ctxt', hst_body = hs_ty' }
                    , fvs1 `plusFV` fvs2) }

    rn_ty env hs_ty = rnHsTyKi env hs_ty

    rn_top_constraint env = rnLHsTyKi (env { rtke_what = RnTopConstraint })


checkExtraConstraintWildCard :: RnTyKiEnv -> HsWildCardInfo GhcPs
                             -> RnM ()
-- Rename the extra-constraint spot in a type signature
--    (blah, _) => type
-- Check that extra-constraints are allowed at all, and
-- if so that it's an anonymous wildcard
checkExtraConstraintWildCard env wc
  = checkWildCard env mb_bad
  where
    mb_bad | not (extraConstraintWildCardsAllowed env)
           = Just (text "Extra-constraint wildcard" <+> quotes (ppr wc)
                   <+> text "not allowed")
           | otherwise
           = Nothing

extraConstraintWildCardsAllowed :: RnTyKiEnv -> Bool
extraConstraintWildCardsAllowed env
  = case rtke_ctxt env of
      TypeSigCtx {}       -> True
      ExprWithTySigCtx {} -> True
      _                   -> False

-- | Finds free type and kind variables in a type,
--     without duplicates, and
--     without variables that are already in scope in LocalRdrEnv
--   NB: this includes named wildcards, which look like perfectly
--       ordinary type variables at this point
extractFilteredRdrTyVars :: LHsType GhcPs -> RnM FreeKiTyVars
extractFilteredRdrTyVars hs_ty
  = do { rdr_env <- getLocalRdrEnv
       ; filterInScope rdr_env <$> extractHsTyRdrTyVars hs_ty }

-- | When the NamedWildCards extension is enabled, partition_nwcs
-- removes type variables that start with an underscore from the
-- FreeKiTyVars in the argument and returns them in a separate list.
-- When the extension is disabled, the function returns the argument
-- and empty list.  See Note [Renaming named wild cards]
partition_nwcs :: FreeKiTyVars -> RnM (FreeKiTyVars, [Located RdrName])
partition_nwcs free_vars@(FKTV { fktv_tys = tys })
  = do { wildcards_enabled <- fmap (xopt LangExt.NamedWildCards) getDynFlags
       ; let (nwcs, no_nwcs) | wildcards_enabled = partition is_wildcard tys
                             | otherwise         = ([], tys)
             free_vars' = free_vars { fktv_tys = no_nwcs }
       ; return (free_vars', nwcs) }
  where
     is_wildcard :: Located RdrName -> Bool
     is_wildcard rdr = startsWithUnderscore (rdrNameOcc (unLoc rdr))

{- Note [Renaming named wild cards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Identifiers starting with an underscore are always parsed as type variables.
It is only here in the renamer that we give the special treatment.
See Note [The wildcard story for types] in HsTypes.

It's easy!  When we collect the implicitly bound type variables, ready
to bring them into scope, and NamedWildCards is on, we partition the
variables into the ones that start with an underscore (the named
wildcards) and the rest. Then we just add them to the hswc_wcs field
of the HsWildCardBndrs structure, and we are done.


*********************************************************
*                                                       *
           HsSigtype (i.e. no wildcards)
*                                                       *
****************************************************** -}

rnHsSigType :: HsDocContext -> LHsSigType GhcPs
            -> RnM (LHsSigType GhcRn, FreeVars)
-- Used for source-language type signatures
-- that cannot have wildcards
rnHsSigType ctx (HsIB { hsib_body = hs_ty })
  = do { vars <- extractFilteredRdrTyVars hs_ty
       ; rnImplicitBndrs True vars hs_ty $ \ vars ->
    do { (body', fvs) <- rnLHsType ctx hs_ty
       ; return ( mk_implicit_bndrs vars body' fvs, fvs ) } }

rnImplicitBndrs :: Bool    -- True <=> no implicit quantification
                           --          if type is headed by a forall
                           -- E.g.  f :: forall a. a->b
                           -- Do not quantify over 'b' too.
                -> FreeKiTyVars
                -> LHsType GhcPs
                -> ([Name] -> RnM (a, FreeVars))
                -> RnM (a, FreeVars)
rnImplicitBndrs no_implicit_if_forall free_vars hs_ty@(L loc _) thing_inside
  = do { let real_tv_rdrs  -- Implicit quantification only if
                           -- there is no explicit forall
               | no_implicit_if_forall
               , L _ (HsForAllTy {}) <- hs_ty = []
               | otherwise                    = freeKiTyVarsTypeVars free_vars
             real_rdrs = freeKiTyVarsKindVars free_vars ++ real_tv_rdrs
       ; traceRn "rnSigType" (ppr hs_ty $$ ppr free_vars $$
                                        ppr real_rdrs)

       ; traceRn "" (text "rnSigType2" <+> ppr hs_ty $$ ppr free_vars $$
                                        ppr real_rdrs)
       ; vars <- mapM (newLocalBndrRn . L loc . unLoc) real_rdrs
       ; bindLocalNamesFV vars $
         thing_inside vars }

rnLHsInstType :: SDoc -> LHsSigType GhcPs -> RnM (LHsSigType GhcRn, FreeVars)
-- Rename the type in an instance or standalone deriving decl
-- The 'doc_str' is "an instance declaration" or "a VECTORISE pragma"
rnLHsInstType doc_str inst_ty
  | Just cls <- getLHsInstDeclClass_maybe inst_ty
  , isTcOcc (rdrNameOcc (unLoc cls))
         -- The guards check that the instance type looks like
         --   blah => C ty1 .. tyn
  = do { let full_doc = doc_str <+> text "for" <+> quotes (ppr cls)
       ; rnHsSigType (GenericCtx full_doc) inst_ty }

  | otherwise  -- The instance is malformed, but we'd still like
               -- to make progress rather than failing outright, so
               -- we report more errors.  So we rename it anyway.
  = do { addErrAt (getLoc (hsSigType inst_ty)) $
         text "Malformed instance:" <+> ppr inst_ty
       ; rnHsSigType (GenericCtx doc_str) inst_ty }

mk_implicit_bndrs :: [Name]  -- implicitly bound
                  -> a           -- payload
                  -> FreeVars    -- FreeVars of payload
                  -> HsImplicitBndrs GhcRn a
mk_implicit_bndrs vars body fvs
  = HsIB { hsib_vars = vars
         , hsib_body = body
         , hsib_closed = nameSetAll (not . isTyVarName) (vars `delFVs` fvs) }


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

Note [QualTy in kinds]
~~~~~~~~~~~~~~~~~~~~~~
I was wondering whether QualTy could occur only at TypeLevel.  But no,
we can have a qualified type in a kind too. Here is an example:

  type family F a where
    F Bool = Nat
    F Nat  = Type

  type family G a where
    G Type = Type -> Type
    G ()   = Nat

  data X :: forall k1 k2. (F k1 ~ G k2) => k1 -> k2 -> Type where
    MkX :: X 'True '()

See that k1 becomes Bool and k2 becomes (), so the equality is
satisfied. If I write MkX :: X 'True 'False, compilation fails with a
suitable message:

  MkX :: X 'True '()
    • Couldn't match kind ‘G Bool’ with ‘Nat’
      Expected kind: G Bool
        Actual kind: F Bool

However: in a kind, the constraints in the QualTy must all be
equalities; or at least, any kinds with a class constraint are
uninhabited.
-}

data RnTyKiEnv
  = RTKE { rtke_ctxt  :: HsDocContext
         , rtke_level :: TypeOrKind  -- Am I renaming a type or a kind?
         , rtke_what  :: RnTyKiWhat  -- And within that what am I renaming?
         , rtke_nwcs  :: NameSet     -- These are the in-scope named wildcards
    }

data RnTyKiWhat = RnTypeBody
                | RnTopConstraint   -- Top-level context of HsSigWcTypes
                | RnConstraint      -- All other constraints

instance Outputable RnTyKiEnv where
  ppr (RTKE { rtke_level = lev, rtke_what = what
            , rtke_nwcs = wcs, rtke_ctxt = ctxt })
    = text "RTKE"
      <+> braces (sep [ ppr lev, ppr what, ppr wcs
                      , pprHsDocContext ctxt ])

instance Outputable RnTyKiWhat where
  ppr RnTypeBody      = text "RnTypeBody"
  ppr RnTopConstraint = text "RnTopConstraint"
  ppr RnConstraint    = text "RnConstraint"

mkTyKiEnv :: HsDocContext -> TypeOrKind -> RnTyKiWhat -> RnTyKiEnv
mkTyKiEnv cxt level what
 = RTKE { rtke_level = level, rtke_nwcs = emptyNameSet
        , rtke_what = what, rtke_ctxt = cxt }

isRnKindLevel :: RnTyKiEnv -> Bool
isRnKindLevel (RTKE { rtke_level = KindLevel }) = True
isRnKindLevel _                                 = False

--------------
rnLHsType  :: HsDocContext -> LHsType GhcPs -> RnM (LHsType GhcRn, FreeVars)
rnLHsType ctxt ty = rnLHsTyKi (mkTyKiEnv ctxt TypeLevel RnTypeBody) ty

rnLHsTypes :: HsDocContext -> [LHsType GhcPs] -> RnM ([LHsType GhcRn], FreeVars)
rnLHsTypes doc tys = mapFvRn (rnLHsType doc) tys

rnHsType  :: HsDocContext -> HsType GhcPs -> RnM (HsType GhcRn, FreeVars)
rnHsType ctxt ty = rnHsTyKi (mkTyKiEnv ctxt TypeLevel RnTypeBody) ty

rnLHsKind  :: HsDocContext -> LHsKind GhcPs -> RnM (LHsKind GhcRn, FreeVars)
rnLHsKind ctxt kind = rnLHsTyKi (mkTyKiEnv ctxt KindLevel RnTypeBody) kind

rnHsKind  :: HsDocContext -> HsKind GhcPs -> RnM (HsKind GhcRn, FreeVars)
rnHsKind ctxt kind = rnHsTyKi  (mkTyKiEnv ctxt KindLevel RnTypeBody) kind

--------------
rnTyKiContext :: RnTyKiEnv -> LHsContext GhcPs
              -> RnM (LHsContext GhcRn, FreeVars)
rnTyKiContext env (L loc cxt)
  = do { traceRn "rncontext" (ppr cxt)
       ; let env' = env { rtke_what = RnConstraint }
       ; (cxt', fvs) <- mapFvRn (rnLHsTyKi env') cxt
       ; return (L loc cxt', fvs) }

rnContext :: HsDocContext -> LHsContext GhcPs
          -> RnM (LHsContext GhcRn, FreeVars)
rnContext doc theta = rnTyKiContext (mkTyKiEnv doc TypeLevel RnConstraint) theta

--------------
rnLHsTyKi  :: RnTyKiEnv -> LHsType GhcPs -> RnM (LHsType GhcRn, FreeVars)
rnLHsTyKi env (L loc ty)
  = setSrcSpan loc $
    do { (ty', fvs) <- rnHsTyKi env ty
       ; return (L loc ty', fvs) }

rnHsTyKi :: RnTyKiEnv -> HsType GhcPs -> RnM (HsType GhcRn, FreeVars)

rnHsTyKi env ty@(HsForAllTy { hst_bndrs = tyvars, hst_body  = tau })
  = do { checkTypeInType env ty
       ; bindLHsTyVarBndrs (rtke_ctxt env) (Just $ inTypeDoc ty)
                           Nothing [] tyvars $ \ _ tyvars' _ _ ->
    do { (tau',  fvs) <- rnLHsTyKi env tau
       ; return ( HsForAllTy { hst_bndrs = tyvars', hst_body =  tau' }
                , fvs) } }

rnHsTyKi env ty@(HsQualTy { hst_ctxt = lctxt, hst_body = tau })
  = do { checkTypeInType env ty  -- See Note [QualTy in kinds]
       ; (ctxt', fvs1) <- rnTyKiContext env lctxt
       ; (tau',  fvs2) <- rnLHsTyKi env tau
       ; return (HsQualTy { hst_ctxt = ctxt', hst_body =  tau' }
                , fvs1 `plusFV` fvs2) }

rnHsTyKi env (HsTyVar ip (L loc rdr_name))
  = do { name <- rnTyVar env rdr_name
       ; return (HsTyVar ip (L loc name), unitFV name) }

rnHsTyKi env ty@(HsOpTy ty1 l_op ty2)
  = setSrcSpan (getLoc l_op) $
    do  { (l_op', fvs1) <- rnHsTyOp env ty l_op
        ; fix   <- lookupTyFixityRn l_op'
        ; (ty1', fvs2) <- rnLHsTyKi env ty1
        ; (ty2', fvs3) <- rnLHsTyKi env ty2
        ; res_ty <- mkHsOpTyRn (\t1 t2 -> HsOpTy t1 l_op' t2)
                               (unLoc l_op') fix ty1' ty2'
        ; return (res_ty, plusFVs [fvs1, fvs2, fvs3]) }

rnHsTyKi env (HsParTy ty)
  = do { (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsParTy ty', fvs) }

rnHsTyKi env (HsBangTy b ty)
  = do { (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsBangTy b ty', fvs) }

rnHsTyKi env ty@(HsRecTy flds)
  = do { let ctxt = rtke_ctxt env
       ; fls          <- get_fields ctxt
       ; (flds', fvs) <- rnConDeclFields ctxt fls flds
       ; return (HsRecTy flds', fvs) }
  where
    get_fields (ConDeclCtx names)
      = concatMapM (lookupConstructorFields . unLoc) names
    get_fields _
      = do { addErr (hang (text "Record syntax is illegal here:")
                                   2 (ppr ty))
           ; return [] }

rnHsTyKi env (HsFunTy ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi env ty1
        -- Might find a for-all as the arg of a function type
       ; (ty2', fvs2) <- rnLHsTyKi env ty2
        -- Or as the result.  This happens when reading Prelude.hi
        -- when we find return :: forall m. Monad m -> forall a. a -> m a

        -- Check for fixity rearrangements
       ; res_ty <- mkHsOpTyRn HsFunTy funTyConName funTyFixity ty1' ty2'
       ; return (res_ty, fvs1 `plusFV` fvs2) }

rnHsTyKi env listTy@(HsListTy ty)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; when (not data_kinds && isRnKindLevel env)
              (addErr (dataKindsErr env listTy))
       ; (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsListTy ty', fvs) }

rnHsTyKi env t@(HsKindSig ty k)
  = do { checkTypeInType env t
       ; kind_sigs_ok <- xoptM LangExt.KindSignatures
       ; unless kind_sigs_ok (badKindSigErr (rtke_ctxt env) ty)
       ; (ty', fvs1) <- rnLHsTyKi env ty
       ; (k', fvs2)  <- rnLHsTyKi (env { rtke_level = KindLevel }) k
       ; return (HsKindSig ty' k', fvs1 `plusFV` fvs2) }

rnHsTyKi env t@(HsPArrTy ty)
  = do { notInKinds env t
       ; (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsPArrTy ty', fvs) }

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsTyKi env tupleTy@(HsTupleTy tup_con tys)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; when (not data_kinds && isRnKindLevel env)
              (addErr (dataKindsErr env tupleTy))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsTupleTy tup_con tys', fvs) }

rnHsTyKi env sumTy@(HsSumTy tys)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; when (not data_kinds && isRnKindLevel env)
              (addErr (dataKindsErr env sumTy))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsSumTy tys', fvs) }

-- Ensure that a type-level integer is nonnegative (#8306, #8412)
rnHsTyKi env tyLit@(HsTyLit t)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; unless data_kinds (addErr (dataKindsErr env tyLit))
       ; when (negLit t) (addErr negLitErr)
       ; checkTypeInType env tyLit
       ; return (HsTyLit t, emptyFVs) }
  where
    negLit (HsStrTy _ _) = False
    negLit (HsNumTy _ i) = i < 0
    negLitErr = text "Illegal literal in type (type literals must not be negative):" <+> ppr tyLit

rnHsTyKi env overall_ty@(HsAppsTy tys)
  = do { -- Step 1: Break up the HsAppsTy into symbols and non-symbol regions
         let (non_syms, syms) = splitHsAppsTy tys

             -- Step 2: rename the pieces
       ; (syms1, fvs1)      <- mapFvRn (rnHsTyOp env overall_ty) syms
       ; (non_syms1, fvs2)  <- (mapFvRn . mapFvRn) (rnLHsTyKi env) non_syms

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
    deal_with_star :: [[LHsType GhcRn]] -> [Located Name]
                   -> [[LHsType GhcRn]] -> [Located Name]
                   -> ([[LHsType GhcRn]], [Located Name])
    deal_with_star acc1 acc2
                   (non_syms1 : non_syms2 : non_syms) (L loc star : ops)
      | star `hasKey` starKindTyConKey || star `hasKey` unicodeStarKindTyConKey
      = deal_with_star acc1 acc2
                       ((non_syms1 ++ L loc (HsTyVar NotPromoted (L loc star))
                            : non_syms2) : non_syms)
                       ops
    deal_with_star acc1 acc2 (non_syms1 : non_syms) (op1 : ops)
      = deal_with_star (non_syms1 : acc1) (op1 : acc2) non_syms ops
    deal_with_star acc1 acc2 [non_syms] []
      = (reverse (non_syms : acc1), reverse acc2)
    deal_with_star _ _ _ _
      = pprPanic "deal_with_star" (ppr overall_ty)

    -- collapse [LHsType GhcRn] to LHsType GhcRn by making applications
    -- monadic only for failure
    deal_with_non_syms :: [LHsType GhcRn] -> RnM (LHsType GhcRn)
    deal_with_non_syms (non_sym : non_syms) = return $ mkHsAppTys non_sym non_syms
    deal_with_non_syms []                   = failWith (emptyNonSymsErr overall_ty)

    -- assemble a right-biased OpTy for use in mkHsOpTyRn
    build_res_ty :: [LHsType GhcRn] -> [Located Name] -> RnM (LHsType GhcRn)
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

rnHsTyKi env (HsAppTy ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi env ty1
       ; (ty2', fvs2) <- rnLHsTyKi env ty2
       ; return (HsAppTy ty1' ty2', fvs1 `plusFV` fvs2) }

rnHsTyKi env t@(HsIParamTy n ty)
  = do { notInKinds env t
       ; (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsIParamTy n ty', fvs) }

rnHsTyKi env t@(HsEqTy ty1 ty2)
  = do { checkTypeInType env t
       ; (ty1', fvs1) <- rnLHsTyKi env ty1
       ; (ty2', fvs2) <- rnLHsTyKi env ty2
       ; return (HsEqTy ty1' ty2', fvs1 `plusFV` fvs2) }

rnHsTyKi _ (HsSpliceTy sp k)
  = rnSpliceType sp k

rnHsTyKi env (HsDocTy ty haddock_doc)
  = do { (ty', fvs) <- rnLHsTyKi env ty
       ; haddock_doc' <- rnLHsDoc haddock_doc
       ; return (HsDocTy ty' haddock_doc', fvs) }

rnHsTyKi _ (HsCoreTy ty)
  = return (HsCoreTy ty, emptyFVs)
    -- The emptyFVs probably isn't quite right
    -- but I don't think it matters

rnHsTyKi env ty@(HsExplicitListTy ip k tys)
  = do { checkTypeInType env ty
       ; data_kinds <- xoptM LangExt.DataKinds
       ; unless data_kinds (addErr (dataKindsErr env ty))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsExplicitListTy ip k tys', fvs) }

rnHsTyKi env ty@(HsExplicitTupleTy kis tys)
  = do { checkTypeInType env ty
       ; data_kinds <- xoptM LangExt.DataKinds
       ; unless data_kinds (addErr (dataKindsErr env ty))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsExplicitTupleTy kis tys', fvs) }

rnHsTyKi env (HsWildCardTy wc)
  = do { checkAnonWildCard env wc
       ; wc' <- rnAnonWildCard wc
       ; return (HsWildCardTy wc', emptyFVs) }
         -- emptyFVs: this occurrence does not refer to a
         --           user-written binding site, so don't treat
         --           it as a free variable

--------------
rnTyVar :: RnTyKiEnv -> RdrName -> RnM Name
rnTyVar env rdr_name
  = do { name <- if   isRnKindLevel env
                 then lookupKindOccRn rdr_name
                 else lookupTypeOccRn rdr_name
       ; checkNamedWildCard env name
       ; return name }

rnLTyVar :: Located RdrName -> RnM (Located Name)
-- Called externally; does not deal with wildards
rnLTyVar (L loc rdr_name)
  = do { tyvar <- lookupTypeOccRn rdr_name
       ; return (L loc tyvar) }

--------------
rnHsTyOp :: Outputable a
         => RnTyKiEnv -> a -> Located RdrName
         -> RnM (Located Name, FreeVars)
rnHsTyOp env overall_ty (L loc op)
  = do { ops_ok <- xoptM LangExt.TypeOperators
       ; op' <- rnTyVar env op
       ; unless (ops_ok
                 || op' == starKindTyConName
                 || op' == unicodeStarKindTyConName
                 || op' `hasKey` eqTyConKey) $
           addErr (opTyErr op overall_ty)
       ; let l_op' = L loc op'
       ; return (l_op', unitFV op') }

--------------
notAllowed :: SDoc -> SDoc
notAllowed doc
  = text "Wildcard" <+> quotes doc <+> ptext (sLit "not allowed")

checkWildCard :: RnTyKiEnv -> Maybe SDoc -> RnM ()
checkWildCard env (Just doc)
  = addErr $ vcat [doc, nest 2 (text "in" <+> pprHsDocContext (rtke_ctxt env))]
checkWildCard _ Nothing
  = return ()

checkAnonWildCard :: RnTyKiEnv -> HsWildCardInfo GhcPs -> RnM ()
-- Report an error if an anonymous wildcard is illegal here
checkAnonWildCard env wc
  = checkWildCard env mb_bad
  where
    mb_bad :: Maybe SDoc
    mb_bad | not (wildCardsAllowed env)
           = Just (notAllowed (ppr wc))
           | otherwise
           = case rtke_what env of
               RnTypeBody      -> Nothing
               RnConstraint    -> Just constraint_msg
               RnTopConstraint -> Just constraint_msg

    constraint_msg = hang (notAllowed (ppr wc) <+> text "in a constraint")
                        2 hint_msg
    hint_msg = vcat [ text "except as the last top-level constraint of a type signature"
                    , nest 2 (text "e.g  f :: (Eq a, _) => blah") ]

checkNamedWildCard :: RnTyKiEnv -> Name -> RnM ()
-- Report an error if a named wildcard is illegal here
checkNamedWildCard env name
  = checkWildCard env mb_bad
  where
    mb_bad | not (name `elemNameSet` rtke_nwcs env)
           = Nothing  -- Not a wildcard
           | not (wildCardsAllowed env)
           = Just (notAllowed (ppr name))
           | otherwise
           = case rtke_what env of
               RnTypeBody      -> Nothing   -- Allowed
               RnTopConstraint -> Nothing   -- Allowed
               RnConstraint    -> Just constraint_msg
    constraint_msg = notAllowed (ppr name) <+> text "in a constraint"

wildCardsAllowed :: RnTyKiEnv -> Bool
-- ^ In what contexts are wildcards permitted
wildCardsAllowed env
   = case rtke_ctxt env of
       TypeSigCtx {}       -> True
       TypBrCtx {}         -> True   -- Template Haskell quoted type
       SpliceTypeCtx {}    -> True   -- Result of a Template Haskell splice
       ExprWithTySigCtx {} -> True
       PatCtx {}           -> True
       RuleCtx {}          -> True
       FamPatCtx {}        -> True   -- Not named wildcards though
       GHCiCtx {}          -> True
       HsTypeCtx {}        -> True
       _                   -> False

rnAnonWildCard :: HsWildCardInfo GhcPs -> RnM (HsWildCardInfo GhcRn)
rnAnonWildCard (AnonWildCard _)
  = do { loc <- getSrcSpanM
       ; uniq <- newUnique
       ; let name = mkInternalName uniq (mkTyVarOcc "_") loc
       ; return (AnonWildCard (L loc name)) }

---------------
-- | Ensures either that we're in a type or that -XTypeInType is set
checkTypeInType :: Outputable ty
                => RnTyKiEnv
                -> ty      -- ^ type
                -> RnM ()
checkTypeInType env ty
  | isRnKindLevel env
  = do { type_in_type <- xoptM LangExt.TypeInType
       ; unless type_in_type $
         addErr (text "Illegal kind:" <+> ppr ty $$
                 text "Did you mean to enable TypeInType?") }
checkTypeInType _ _ = return ()

notInKinds :: Outputable ty
           => RnTyKiEnv
           -> ty
           -> RnM ()
notInKinds env ty
  | isRnKindLevel env
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
              -> Maybe SDoc         -- if we are to check for unused tvs,
                                    -- a phrase like "in the type ..."
              -> Maybe a                 -- Just _  => an associated type decl
              -> [Located RdrName]    -- Kind variables from scope, in l-to-r
                                         -- order, but not from ...
              -> (LHsQTyVars GhcPs)       -- ... these user-written tyvars
              -> (LHsQTyVars GhcRn -> NameSet -> RnM (b, FreeVars))
                  -- also returns all names used in kind signatures, for the
                  -- TypeInType clause of Note [Complete user-supplied kind
                  -- signatures] in HsDecls
              -> RnM (b, FreeVars)
-- (a) Bring kind variables into scope
--     both (i)  passed in (kv_bndrs)
--     and  (ii) mentioned in the kinds of tv_bndrs
-- (b) Bring type variables into scope
bindHsQTyVars doc mb_in_doc mb_assoc kv_bndrs tv_bndrs thing_inside
  = do { bindLHsTyVarBndrs doc mb_in_doc
                           mb_assoc kv_bndrs (hsQTvExplicit tv_bndrs) $
         \ rn_kvs rn_bndrs dep_var_set all_dep_vars ->
         thing_inside (HsQTvs { hsq_implicit = rn_kvs
                              , hsq_explicit = rn_bndrs
                              , hsq_dependent = dep_var_set }) all_dep_vars }

bindLHsTyVarBndrs :: forall a b.
                     HsDocContext
                  -> Maybe SDoc         -- if we are to check for unused tvs,
                                        -- a phrase like "in the type ..."
                  -> Maybe a            -- Just _  => an associated type decl
                  -> [Located RdrName] -- Unbound kind variables from scope,
                                          -- in l-to-r order, but not from ...
                  -> [LHsTyVarBndr GhcPs]  -- ... these user-written tyvars
                  -> (   [Name]  -- all kv names
                      -> [LHsTyVarBndr GhcRn]
                      -> NameSet -- which names, from the preceding list,
                                 -- are used dependently within that list
                                 -- See Note [Dependent LHsQTyVars] in TcHsType
                      -> NameSet -- all names used in kind signatures
                      -> RnM (b, FreeVars))
                  -> RnM (b, FreeVars)
bindLHsTyVarBndrs doc mb_in_doc mb_assoc kv_bndrs tv_bndrs thing_inside
  = do { when (isNothing mb_assoc) (checkShadowedRdrNames tv_names_w_loc)
       ; go [] [] emptyNameSet emptyNameSet emptyNameSet tv_bndrs }
  where
    tv_names_w_loc = map hsLTyVarLocName tv_bndrs

    go :: [Name]                 -- kind-vars found (in reverse order)
       -> [LHsTyVarBndr GhcRn]   -- already renamed (in reverse order)
       -> NameSet                -- kind vars already in scope (for dup checking)
       -> NameSet                -- type vars already in scope (for dup checking)
       -> NameSet                -- (all) variables used dependently
       -> [LHsTyVarBndr GhcPs]   -- still to be renamed, scoped
       -> RnM (b, FreeVars)
    go rn_kvs rn_tvs kv_names tv_names dep_vars (tv_bndr : tv_bndrs)
      = bindLHsTyVarBndr doc mb_assoc kv_names tv_names tv_bndr $
        \ kv_nms used_dependently tv_bndr' ->
        do { (b, fvs) <- go (reverse kv_nms ++ rn_kvs)
                            (tv_bndr' : rn_tvs)
                            (kv_names `extendNameSetList` kv_nms)
                            (tv_names `extendNameSet` hsLTyVarName tv_bndr')
                            (dep_vars `unionNameSet` used_dependently)
                            tv_bndrs
           ; warn_unused tv_bndr' fvs
           ; return (b, fvs) }

    go rn_kvs rn_tvs _kv_names tv_names dep_vars []
      = -- still need to deal with the kv_bndrs passed in originally
        bindImplicitKvs doc mb_assoc kv_bndrs tv_names $ \ kv_nms others ->
        do { let all_rn_kvs = reverse (reverse kv_nms ++ rn_kvs)
                 all_rn_tvs = reverse rn_tvs
           ; env <- getLocalRdrEnv
           ; let all_dep_vars = dep_vars `unionNameSet` others
                 exp_dep_vars -- variables in all_rn_tvs that are in dep_vars
                   = mkNameSet [ name
                               | v <- all_rn_tvs
                               , let name = hsLTyVarName v
                               , name `elemNameSet` all_dep_vars ]
           ; traceRn "bindHsTyVars" (ppr env $$
                                     ppr all_rn_kvs $$
                                     ppr all_rn_tvs $$
                                     ppr exp_dep_vars)
           ; thing_inside all_rn_kvs all_rn_tvs exp_dep_vars all_dep_vars }

    warn_unused tv_bndr fvs = case mb_in_doc of
      Just in_doc -> warnUnusedForAll in_doc tv_bndr fvs
      Nothing     -> return ()

bindLHsTyVarBndr :: HsDocContext
                 -> Maybe a   -- associated class
                 -> NameSet   -- kind vars already in scope
                 -> NameSet   -- type vars already in scope
                 -> LHsTyVarBndr GhcPs
                 -> ([Name] -> NameSet -> LHsTyVarBndr GhcRn
                        -> RnM (b, FreeVars))
                   -- passed the newly-bound implicitly-declared kind vars,
                   -- any other names used in a kind
                   -- and the renamed LHsTyVarBndr
                 -> RnM (b, FreeVars)
bindLHsTyVarBndr doc mb_assoc kv_names tv_names hs_tv_bndr thing_inside
  = case hs_tv_bndr of
      L loc (UserTyVar lrdr@(L lv rdr)) ->
        do { check_dup loc rdr []
           ; nm <- newTyVarNameRn mb_assoc lrdr
           ; bindLocalNamesFV [nm] $
             thing_inside [] emptyNameSet (L loc (UserTyVar (L lv nm))) }
      L loc (KindedTyVar lrdr@(L lv rdr) kind) ->
        do { free_kvs <- freeKiTyVarsAllVars <$> extractHsTyRdrTyVars kind
           ; check_dup lv rdr (map unLoc free_kvs)

             -- check for -XKindSignatures
           ; sig_ok <- xoptM LangExt.KindSignatures
           ; unless sig_ok (badKindSigErr doc kind)

             -- deal with kind vars in the user-written kind
           ; bindImplicitKvs doc mb_assoc free_kvs tv_names $
             \ new_kv_nms other_kv_nms ->
             do { (kind', fvs1) <- rnLHsKind doc kind
                ; tv_nm  <- newTyVarNameRn mb_assoc lrdr
                ; (b, fvs2) <- bindLocalNamesFV [tv_nm] $
                               thing_inside new_kv_nms other_kv_nms
                                 (L loc (KindedTyVar (L lv tv_nm) kind'))
                ; return (b, fvs1 `plusFV` fvs2) }}
  where
      -- make sure that the RdrName isn't in the sets of
      -- names. We can't just check that it's not in scope at all
      -- because we might be inside an associated class.
    check_dup :: SrcSpan -> RdrName -> [RdrName] -> RnM ()
    check_dup loc rdr kindFreeVars
      = do { -- Disallow use of a type variable name in its
             -- kind signature (#11592).
             when (rdr `elem` kindFreeVars) $
             addErrAt loc (vcat [ ki_ty_self_err rdr
                                , pprHsDocContext doc ])

           ; m_name <- lookupLocalOccRn_maybe rdr
           ; whenIsJust m_name $ \name ->
        do { when (name `elemNameSet` kv_names) $
             addErrAt loc (vcat [ ki_ty_err_msg name
                                , pprHsDocContext doc ])
           ; when (name `elemNameSet` tv_names) $
             dupNamesErr getLoc [L loc name, L (nameSrcSpan name) name] }}

    ki_ty_err_msg n = text "Variable" <+> quotes (ppr n) <+>
                      text "used as a kind variable before being bound" $$
                      text "as a type variable. Perhaps reorder your variables?"

    ki_ty_self_err n = text "Variable" <+> quotes (ppr n) <+>
                       text "is used in the kind signature of its" $$
                       text "declaration as a type variable."


bindImplicitKvs :: HsDocContext
                -> Maybe a
                -> [Located RdrName]  -- ^ kind var *occurrences*, from which
                                      -- intent to bind is inferred
                -> NameSet            -- ^ *type* variables, for type/kind
                                      -- misuse check for -XNoTypeInType
                -> ([Name] -> NameSet -> RnM (b, FreeVars))
                   -- ^ passed new kv_names, and any other names used in a kind
                -> RnM (b, FreeVars)
bindImplicitKvs _   _        []       _        thing_inside
  = thing_inside [] emptyNameSet
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
         thing_inside kv_nms (mkNameSet (map unLoc bound_kvs)) }
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
collectAnonWildCards :: LHsType GhcRn -> [Name]
-- | Extract all wild cards from a type.
collectAnonWildCards lty = go lty
  where
    go (L _ ty) = case ty of
      HsWildCardTy (AnonWildCard (L _ wc)) -> [wc]
      HsAppsTy tys                 -> gos (mapMaybe (prefix_types_only . unLoc) tys)
      HsAppTy ty1 ty2              -> go ty1 `mappend` go ty2
      HsFunTy ty1 ty2              -> go ty1 `mappend` go ty2
      HsListTy ty                  -> go ty
      HsPArrTy ty                  -> go ty
      HsTupleTy _ tys              -> gos tys
      HsSumTy tys                  -> gos tys
      HsOpTy ty1 _ ty2             -> go ty1 `mappend` go ty2
      HsParTy ty                   -> go ty
      HsIParamTy _ ty              -> go ty
      HsEqTy ty1 ty2               -> go ty1 `mappend` go ty2
      HsKindSig ty kind            -> go ty `mappend` go kind
      HsDocTy ty _                 -> go ty
      HsBangTy _ ty                -> go ty
      HsRecTy flds                 -> gos $ map (cd_fld_type . unLoc) flds
      HsExplicitListTy _ _ tys     -> gos tys
      HsExplicitTupleTy _ tys      -> gos tys
      HsForAllTy { hst_bndrs = bndrs
                 , hst_body = ty } -> collectAnonWildCardsBndrs bndrs
                                      `mappend` go ty
      HsQualTy { hst_ctxt = L _ ctxt
               , hst_body = ty }  -> gos ctxt `mappend` go ty
      HsSpliceTy (HsSpliced _ (HsSplicedTy ty)) _ -> go $ L noSrcSpan ty
      HsSpliceTy{} -> mempty
      HsCoreTy{} -> mempty
      HsTyLit{} -> mempty
      HsTyVar{} -> mempty

    gos = mconcat . map go

    prefix_types_only (HsAppPrefix ty) = Just ty
    prefix_types_only (HsAppInfix _)   = Nothing

collectAnonWildCardsBndrs :: [LHsTyVarBndr GhcRn] -> [Name]
collectAnonWildCardsBndrs ltvs = concatMap (go . unLoc) ltvs
  where
    go (UserTyVar _)      = []
    go (KindedTyVar _ ki) = collectAnonWildCards ki

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

rnConDeclFields :: HsDocContext -> [FieldLabel] -> [LConDeclField GhcPs]
                -> RnM ([LConDeclField GhcRn], FreeVars)
-- Also called from RnSource
-- No wildcards can appear in record fields
rnConDeclFields ctxt fls fields
   = mapFvRn (rnField fl_env env) fields
  where
    env    = mkTyKiEnv ctxt TypeLevel RnTypeBody
    fl_env = mkFsEnv [ (flLabel fl, fl) | fl <- fls ]

rnField :: FastStringEnv FieldLabel -> RnTyKiEnv -> LConDeclField GhcPs
        -> RnM (LConDeclField GhcRn, FreeVars)
rnField fl_env env (L l (ConDeclField names ty haddock_doc))
  = do { let new_names = map (fmap lookupField) names
       ; (new_ty, fvs) <- rnLHsTyKi env ty
       ; new_haddock_doc <- rnMbLHsDoc haddock_doc
       ; return (L l (ConDeclField new_names new_ty new_haddock_doc), fvs) }
  where
    lookupField :: FieldOcc GhcPs -> FieldOcc GhcRn
    lookupField (FieldOcc (L lr rdr) _) = FieldOcc (L lr rdr) (flSelector fl)
      where
        lbl = occNameFS $ rdrNameOcc rdr
        fl  = expectJust "rnField" $ lookupFsEnv fl_env lbl

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
operator applications left-associatively, EXCEPT negation, which
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
mkHsOpTyRn :: (LHsType GhcRn -> LHsType GhcRn -> HsType GhcRn)
           -> Name -> Fixity -> LHsType GhcRn -> LHsType GhcRn
           -> RnM (HsType GhcRn)

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
mk_hs_op_ty :: (LHsType GhcRn -> LHsType GhcRn -> HsType GhcRn)
            -> Name -> Fixity -> LHsType GhcRn
            -> (LHsType GhcRn -> LHsType GhcRn -> HsType GhcRn)
            -> Name -> Fixity -> LHsType GhcRn -> LHsType GhcRn -> SrcSpan
            -> RnM (HsType GhcRn)
mk_hs_op_ty mk1 op1 fix1 ty1
            mk2 op2 fix2 ty21 ty22 loc2
  | nofix_error     = do { precParseErr (NormalOp op1,fix1) (NormalOp op2,fix2)
                         ; return (mk1 ty1 (L loc2 (mk2 ty21 ty22))) }
  | associate_right = return (mk1 ty1 (L loc2 (mk2 ty21 ty22)))
  | otherwise       = do { -- Rearrange to ((ty1 `op1` ty21) `op2` ty22)
                           new_ty <- mkHsOpTyRn mk1 op1 fix1 ty1 ty21
                         ; return (mk2 (noLoc new_ty) ty22) }
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2


---------------------------
mkOpAppRn :: LHsExpr GhcRn             -- Left operand; already rearranged
          -> LHsExpr GhcRn -> Fixity   -- Operator and fixity
          -> LHsExpr GhcRn             -- Right operand (not an OpApp, but might
                                       -- be a NegApp)
          -> RnM (HsExpr GhcRn)

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
  = do precParseErr (NegateOp,negateFixity) (get_op op2,fix2)
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
  = do precParseErr (get_op op1, fix1) (NegateOp, negateFixity)
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

-- | Name of an operator in an operator application or section
data OpName = NormalOp Name         -- ^ A normal identifier
            | NegateOp              -- ^ Prefix negation
            | UnboundOp UnboundVar  -- ^ An unbound indentifier
            | RecFldOp (AmbiguousFieldOcc GhcRn)
              -- ^ A (possibly ambiguous) record field occurrence

instance Outputable OpName where
  ppr (NormalOp n)   = ppr n
  ppr NegateOp       = ppr negateName
  ppr (UnboundOp uv) = ppr uv
  ppr (RecFldOp fld) = ppr fld

get_op :: LHsExpr GhcRn -> OpName
-- An unbound name could be either HsVar or HsUnboundVar
-- See RnExpr.rnUnboundVar
get_op (L _ (HsVar (L _ n)))   = NormalOp n
get_op (L _ (HsUnboundVar uv)) = UnboundOp uv
get_op (L _ (HsRecFld fld))    = RecFldOp fld
get_op other                   = pprPanic "get_op" (ppr other)

-- Parser left-associates everything, but
-- derived instances may have correctly-associated things to
-- in the right operand.  So we just check that the right operand is OK
right_op_ok :: Fixity -> HsExpr GhcRn -> Bool
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
mkOpFormRn :: LHsCmdTop GhcRn            -- Left operand; already rearranged
          -> LHsExpr GhcRn -> Fixity     -- Operator and fixity
          -> LHsCmdTop GhcRn             -- Right operand (not an infix)
          -> RnM (HsCmd GhcRn)

-- (e11 `op1` e12) `op2` e2
mkOpFormRn a1@(L loc (HsCmdTop (L _ (HsCmdArrForm op1 f (Just fix1)
                                     [a11,a12])) _ _ _))
        op2 fix2 a2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (HsCmdArrForm op2 f (Just fix2) [a1, a2])

  | associate_right
  = do new_c <- mkOpFormRn a12 op2 fix2 a2
       return (HsCmdArrForm op1 f (Just fix1)
               [a11, L loc (HsCmdTop (L loc new_c)
               placeHolderType placeHolderType [])])
        -- TODO: locs are wrong
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2

--      Default case
mkOpFormRn arg1 op fix arg2                     -- Default case, no rearrangment
  = return (HsCmdArrForm op Infix (Just fix) [arg1, arg2])


--------------------------------------
mkConOpPatRn :: Located Name -> Fixity -> LPat GhcRn -> LPat GhcRn
             -> RnM (Pat GhcRn)

mkConOpPatRn op2 fix2 p1@(L loc (ConPatIn op1 (InfixCon p11 p12))) p2
  = do  { fix1 <- lookupFixityRn (unLoc op1)
        ; let (nofix_error, associate_right) = compareFixity fix1 fix2

        ; if nofix_error then do
                { precParseErr (NormalOp (unLoc op1),fix1)
                               (NormalOp (unLoc op2),fix2)
                ; return (ConPatIn op2 (InfixCon p1 p2)) }

          else if associate_right then do
                { new_p <- mkConOpPatRn op2 fix2 p12 p2
                ; return (ConPatIn op1 (InfixCon p11 (L loc new_p))) } -- XXX loc right?
          else return (ConPatIn op2 (InfixCon p1 p2)) }

mkConOpPatRn op _ p1 p2                         -- Default case, no rearrangment
  = ASSERT( not_op_pat (unLoc p2) )
    return (ConPatIn op (InfixCon p1 p2))

not_op_pat :: Pat GhcRn -> Bool
not_op_pat (ConPatIn _ (InfixCon _ _)) = False
not_op_pat _                           = True

--------------------------------------
checkPrecMatch :: Name -> MatchGroup GhcRn body -> RnM ()
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

checkPrec :: Name -> Pat GhcRn -> Bool -> IOEnv (Env TcGblEnv TcLclEnv) ()
checkPrec op (ConPatIn op1 (InfixCon _ _)) right = do
    op_fix@(Fixity _ op_prec  op_dir) <- lookupFixityRn op
    op1_fix@(Fixity _ op1_prec op1_dir) <- lookupFixityRn (unLoc op1)
    let
        inf_ok = op1_prec > op_prec ||
                 (op1_prec == op_prec &&
                  (op1_dir == InfixR && op_dir == InfixR && right ||
                   op1_dir == InfixL && op_dir == InfixL && not right))

        info  = (NormalOp op,          op_fix)
        info1 = (NormalOp (unLoc op1), op1_fix)
        (infol, infor) = if right then (info, info1) else (info1, info)
    unless inf_ok (precParseErr infol infor)

checkPrec _ _ _
  = return ()

-- Check precedence of (arg op) or (op arg) respectively
-- If arg is itself an operator application, then either
--   (a) its precedence must be higher than that of op
--   (b) its precedency & associativity must be the same as that of op
checkSectionPrec :: FixityDirection -> HsExpr GhcPs
        -> LHsExpr GhcRn -> LHsExpr GhcRn -> RnM ()
checkSectionPrec direction section op arg
  = case unLoc arg of
        OpApp _ op' fix _ -> go_for_it (get_op op') fix
        NegApp _ _        -> go_for_it NegateOp     negateFixity
        _                 -> return ()
  where
    op_name = get_op op
    go_for_it arg_op arg_fix@(Fixity _ arg_prec assoc) = do
          op_fix@(Fixity _ op_prec _) <- lookupFixityOp op_name
          unless (op_prec < arg_prec
                  || (op_prec == arg_prec && direction == assoc))
                 (sectionPrecErr (get_op op, op_fix)
                                 (arg_op, arg_fix) section)

-- | Look up the fixity for an operator name.  Be careful to use
-- 'lookupFieldFixityRn' for (possibly ambiguous) record fields
-- (see Trac #13132).
lookupFixityOp :: OpName -> RnM Fixity
lookupFixityOp (NormalOp n)  = lookupFixityRn n
lookupFixityOp NegateOp      = lookupFixityRn negateName
lookupFixityOp (UnboundOp u) = lookupFixityRn (mkUnboundName (unboundVarOcc u))
lookupFixityOp (RecFldOp f)  = lookupFieldFixityRn f


-- Precedence-related error messages

precParseErr :: (OpName,Fixity) -> (OpName,Fixity) -> RnM ()
precParseErr op1@(n1,_) op2@(n2,_)
  | is_unbound n1 || is_unbound n2
  = return ()     -- Avoid error cascade
  | otherwise
  = addErr $ hang (text "Precedence parsing error")
      4 (hsep [text "cannot mix", ppr_opfix op1, ptext (sLit "and"),
               ppr_opfix op2,
               text "in the same infix expression"])

sectionPrecErr :: (OpName,Fixity) -> (OpName,Fixity) -> HsExpr GhcPs -> RnM ()
sectionPrecErr op@(n1,_) arg_op@(n2,_) section
  | is_unbound n1 || is_unbound n2
  = return ()     -- Avoid error cascade
  | otherwise
  = addErr $ vcat [text "The operator" <+> ppr_opfix op <+> ptext (sLit "of a section"),
         nest 4 (sep [text "must have lower precedence than that of the operand,",
                      nest 2 (text "namely" <+> ppr_opfix arg_op)]),
         nest 4 (text "in the section:" <+> quotes (ppr section))]

is_unbound :: OpName -> Bool
is_unbound (NormalOp n) = isUnboundName n
is_unbound UnboundOp{}  = True
is_unbound _            = False

ppr_opfix :: (OpName, Fixity) -> SDoc
ppr_opfix (op, fixity) = pp_op <+> brackets (ppr fixity)
   where
     pp_op | NegateOp <- op = text "prefix `-'"
           | otherwise      = quotes (ppr op)


{- *****************************************************
*                                                      *
                 Errors
*                                                      *
***************************************************** -}

unexpectedTypeSigErr :: LHsSigWcType GhcPs -> SDoc
unexpectedTypeSigErr ty
  = hang (text "Illegal type signature:" <+> quotes (ppr ty))
       2 (text "Type signatures are only allowed in patterns with ScopedTypeVariables")

badKindBndrs :: HsDocContext -> [Located RdrName] -> SDoc
badKindBndrs doc kvs
  = withHsDocContext doc $
    hang (text "Unexpected kind variable" <> plural kvs
                 <+> pprQuotedList kvs)
       2 (text "Perhaps you intended to use PolyKinds")

badKindSigErr :: HsDocContext -> LHsType GhcPs -> TcM ()
badKindSigErr doc (L loc ty)
  = setSrcSpan loc $ addErr $
    withHsDocContext doc $
    hang (text "Illegal kind signature:" <+> quotes (ppr ty))
       2 (text "Perhaps you intended to use KindSignatures")

dataKindsErr :: RnTyKiEnv -> HsType GhcPs -> SDoc
dataKindsErr env thing
  = hang (text "Illegal" <+> pp_what <> colon <+> quotes (ppr thing))
       2 (text "Perhaps you intended to use DataKinds")
  where
    pp_what | isRnKindLevel env = text "kind"
            | otherwise          = text "type"

inTypeDoc :: HsType GhcPs -> SDoc
inTypeDoc ty = text "In the type" <+> quotes (ppr ty)

warnUnusedForAll :: SDoc -> LHsTyVarBndr GhcRn -> FreeVars -> TcM ()
warnUnusedForAll in_doc (L loc tv) used_names
  = whenWOptM Opt_WarnUnusedForalls $
    unless (hsTyVarName tv `elemNameSet` used_names) $
    addWarnAt (Reason Opt_WarnUnusedForalls) loc $
    vcat [ text "Unused quantified type variable" <+> quotes (ppr tv)
         , in_doc ]

opTyErr :: Outputable a => RdrName -> a -> SDoc
opTyErr op overall_ty
  = hang (text "Illegal operator" <+> quotes (ppr op) <+> ptext (sLit "in type") <+> quotes (ppr overall_ty))
         2 extra
  where
    extra | op == dot_tv_RDR
          = perhapsForallMsg
          | otherwise
          = text "Use TypeOperators to allow operators in types"

emptyNonSymsErr :: HsType GhcPs -> SDoc
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
                         , fktv_tys    :: [Located RdrName] }

instance Outputable FreeKiTyVars where
  ppr (FKTV kis tys) = ppr (kis, tys)

emptyFKTV :: FreeKiTyVars
emptyFKTV = FKTV [] []

freeKiTyVarsAllVars :: FreeKiTyVars -> [Located RdrName]
freeKiTyVarsAllVars (FKTV tys kvs) = tys ++ kvs

freeKiTyVarsKindVars :: FreeKiTyVars -> [Located RdrName]
freeKiTyVarsKindVars = fktv_kis

freeKiTyVarsTypeVars :: FreeKiTyVars -> [Located RdrName]
freeKiTyVarsTypeVars = fktv_tys

filterInScope :: LocalRdrEnv -> FreeKiTyVars -> FreeKiTyVars
filterInScope rdr_env (FKTV kis tys)
  = FKTV (filterOut in_scope kis)
         (filterOut in_scope tys)
  where
    in_scope         = inScope rdr_env . unLoc

inScope :: LocalRdrEnv -> RdrName -> Bool
inScope rdr_env rdr = rdr `elemLocalRdrEnv` rdr_env

extractHsTyRdrTyVars :: LHsType GhcPs -> RnM FreeKiTyVars
-- extractHsTyRdrNames finds the free (kind, type) variables of a HsType
--                        or the free (sort, kind) variables of a HsKind
-- It's used when making the for-alls explicit.
-- Does not return any wildcards
-- When the same name occurs multiple times in the types, only the first
-- occurrence is returned.
-- See Note [Kind and type-variable binders]
extractHsTyRdrTyVars ty
  = do { FKTV kis tys <- extract_lty TypeLevel ty emptyFKTV
       ; return (FKTV (nubL kis)
                      (nubL tys)) }


-- | Extracts free type and kind variables from types in a list.
-- When the same name occurs multiple times in the types, only the first
-- occurrence is returned and the rest is filtered out.
-- See Note [Kind and type-variable binders]
extractHsTysRdrTyVars :: [LHsType GhcPs] -> RnM FreeKiTyVars
extractHsTysRdrTyVars tys
  = rmDupsInRdrTyVars <$> extractHsTysRdrTyVarsDups tys

-- | Extracts free type and kind variables from types in a list.
-- When the same name occurs multiple times in the types, all occurrences
-- are returned.
extractHsTysRdrTyVarsDups :: [LHsType GhcPs] -> RnM FreeKiTyVars
extractHsTysRdrTyVarsDups tys
  = extract_ltys TypeLevel tys emptyFKTV

-- | Removes multiple occurrences of the same name from FreeKiTyVars.
rmDupsInRdrTyVars :: FreeKiTyVars -> FreeKiTyVars
rmDupsInRdrTyVars (FKTV kis tys)
  = FKTV (nubL kis) (nubL tys)

extractRdrKindSigVars :: LFamilyResultSig GhcPs -> RnM [Located RdrName]
extractRdrKindSigVars (L _ resultSig)
    | KindSig k                        <- resultSig = kindRdrNameFromSig k
    | TyVarSig (L _ (KindedTyVar _ k)) <- resultSig = kindRdrNameFromSig k
    | otherwise = return []
    where kindRdrNameFromSig k = freeKiTyVarsAllVars <$> extractHsTyRdrTyVars k

extractDataDefnKindVars :: HsDataDefn GhcPs -> RnM [Located RdrName]
-- Get the scoped kind variables mentioned free in the constructor decls
-- Eg    data T a = T1 (S (a :: k) | forall (b::k). T2 (S b)
-- Here k should scope over the whole definition
extractDataDefnKindVars (HsDataDefn { dd_ctxt = ctxt, dd_kindSig = ksig
                                    , dd_cons = cons, dd_derivs = L _ derivs })
  = (nubL . freeKiTyVarsKindVars) <$>
    (extract_lctxt TypeLevel ctxt =<<
     extract_mb extract_lkind ksig =<<
     extract_sig_tys (concatMap (unLoc . deriv_clause_tys . unLoc) derivs) =<<
     foldrM (extract_con . unLoc) emptyFKTV cons)
  where
    extract_con (ConDeclGADT { }) acc = return acc
    extract_con (ConDeclH98 { con_qvars = qvs
                            , con_cxt = ctxt, con_details = details }) acc
      = extract_hs_tv_bndrs (maybe [] hsQTvExplicit qvs) acc =<<
        extract_mlctxt ctxt =<<
        extract_ltys TypeLevel (hsConDeclArgTys details) emptyFKTV

extract_mlctxt :: Maybe (LHsContext GhcPs) -> FreeKiTyVars -> RnM FreeKiTyVars
extract_mlctxt Nothing     acc = return acc
extract_mlctxt (Just ctxt) acc = extract_lctxt TypeLevel ctxt acc

extract_lctxt :: TypeOrKind
              -> LHsContext GhcPs -> FreeKiTyVars -> RnM FreeKiTyVars
extract_lctxt t_or_k ctxt = extract_ltys t_or_k (unLoc ctxt)

extract_sig_tys :: [LHsSigType GhcPs] -> FreeKiTyVars -> RnM FreeKiTyVars
extract_sig_tys sig_tys acc
  = foldrM (\sig_ty acc -> extract_lty TypeLevel (hsSigType sig_ty) acc)
           acc sig_tys

extract_ltys :: TypeOrKind
             -> [LHsType GhcPs] -> FreeKiTyVars -> RnM FreeKiTyVars
extract_ltys t_or_k tys acc = foldrM (extract_lty t_or_k) acc tys

extract_mb :: (a -> FreeKiTyVars -> RnM FreeKiTyVars)
           -> Maybe a -> FreeKiTyVars -> RnM FreeKiTyVars
extract_mb _ Nothing  acc = return acc
extract_mb f (Just x) acc = f x acc

extract_lkind :: LHsType GhcPs -> FreeKiTyVars -> RnM FreeKiTyVars
extract_lkind = extract_lty KindLevel

extract_lty :: TypeOrKind -> LHsType GhcPs -> FreeKiTyVars -> RnM FreeKiTyVars
extract_lty t_or_k (L _ ty) acc
  = case ty of
      HsTyVar _  ltv            -> extract_tv t_or_k ltv acc
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
      HsSumTy tys               -> extract_ltys t_or_k tys acc
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
      HsExplicitListTy _ _ tys  -> extract_ltys t_or_k tys acc
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
             -> [LHsAppType GhcPs] -> FreeKiTyVars -> RnM FreeKiTyVars
extract_apps t_or_k tys acc = foldrM (extract_app t_or_k) acc tys

extract_app :: TypeOrKind -> LHsAppType GhcPs -> FreeKiTyVars
            -> RnM FreeKiTyVars
extract_app t_or_k (L _ (HsAppInfix tv))  acc = extract_tv t_or_k tv acc
extract_app t_or_k (L _ (HsAppPrefix ty)) acc = extract_lty t_or_k ty acc

extract_hs_tv_bndrs :: [LHsTyVarBndr GhcPs] -> FreeKiTyVars
                    -> FreeKiTyVars -> RnM FreeKiTyVars
-- In (forall (a :: Maybe e). a -> b) we have
--     'a' is bound by the forall
--     'b' is a free type variable
--     'e' is a free kind variable
extract_hs_tv_bndrs tvs
                    (FKTV acc_kvs acc_tvs)
                           -- Note accumulator comes first
                    (FKTV body_kvs body_tvs)
  | null tvs
  = return $
    FKTV (body_kvs ++ acc_kvs) (body_tvs ++ acc_tvs)
  | otherwise
  = do { FKTV bndr_kvs _
           <- foldrM extract_lkind emptyFKTV [k | L _ (KindedTyVar _ k) <- tvs]

       ; let locals = map hsLTyVarLocName tvs

         -- These checks are all tested in typecheck/should_fail/T11963
       ; check_for_mixed_vars bndr_kvs acc_tvs
       ; check_for_mixed_vars bndr_kvs body_tvs
       ; check_for_mixed_vars body_tvs acc_kvs
       ; check_for_mixed_vars body_kvs acc_tvs
       ; check_for_mixed_vars locals body_kvs

       ; return $
         FKTV (filterOut (`elemRdr` locals) (bndr_kvs ++ body_kvs)
                ++ acc_kvs)
              (filterOut (`elemRdr` locals) body_tvs ++ acc_tvs) }
  where
    check_for_mixed_vars :: [Located RdrName] -> [Located RdrName] -> RnM ()
    check_for_mixed_vars tvs1 tvs2 = mapM_ check tvs1
      where
        check tv1 = when (isRdrTyVar (unLoc tv1) && (tv1 `elemRdr` tvs2)) $
                    mixedVarsErr tv1

extract_tv :: TypeOrKind -> Located RdrName -> FreeKiTyVars
           -> RnM FreeKiTyVars
extract_tv t_or_k ltv@(L _ tv) acc
  | isRdrTyVar tv = case acc of
      FKTV kvs tvs
        |  isTypeLevel t_or_k
        -> do { when (ltv `elemRdr` kvs) $
                mixedVarsErr ltv
              ; return (FKTV kvs (ltv : tvs)) }
        |  otherwise
        -> do { when (ltv `elemRdr` tvs) $
                mixedVarsErr ltv
              ; return (FKTV (ltv : kvs) tvs) }
  | otherwise     = return acc

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

elemRdr :: Located RdrName -> [Located RdrName] -> Bool
elemRdr x = any (eqLocated x)
