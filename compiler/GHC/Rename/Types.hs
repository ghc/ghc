{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.Rename.Types (
        -- Type related stuff
        rnHsType, rnLHsType, rnLHsTypes, rnContext,
        rnHsKind, rnLHsKind, rnLHsTypeArgs,
        rnHsSigType, rnHsWcType,
        HsSigWcTypeScoping(..), rnHsSigWcType, rnHsSigWcTypeScoped,
        newTyVarNameRn,
        rnConDeclFields,
        rnLTyVar,

        -- Precence related stuff
        mkOpAppRn, mkNegAppRn, mkOpFormRn, mkConOpPatRn,
        checkPrecMatch, checkSectionPrec,

        -- Binding related stuff
        bindLHsTyVarBndr, bindLHsTyVarBndrs, rnImplicitBndrs,
        bindSigTyVarsFV, bindHsQTyVars, bindLRdrNames,
        extractHsTyRdrTyVars, extractHsTyRdrTyVarsKindVars,
        extractHsTysRdrTyVarsDups,
        extractRdrKindSigVars, extractDataDefnKindVars,
        extractHsTvBndrs, extractHsTyArgRdrKiTyVarsDup,
        nubL, elemRdr
  ) where

import GhcPrelude

import {-# SOURCE #-} GHC.Rename.Splice( rnSpliceType )

import DynFlags
import GHC.Hs
import GHC.Rename.Doc    ( rnLHsDoc, rnMbLHsDoc )
import GHC.Rename.Env
import GHC.Rename.Utils  ( HsDocContext(..), withHsDocContext, mapFvRn
                         , pprHsDocContext, bindLocalNamesFV, typeAppErr
                         , newLocalBndrRn, checkDupRdrNames, checkShadowedRdrNames )
import GHC.Rename.Fixity ( lookupFieldFixityRn, lookupFixityRn
                         , lookupTyFixityRn )
import TcRnMonad
import RdrName
import PrelNames
import TysPrim          ( funTyConName )
import Name
import SrcLoc
import NameSet
import FieldLabel

import Util
import ListSetOps       ( deleteBys )
import BasicTypes       ( compareFixity, funTyFixity, negateFixity
                        , Fixity(..), FixityDirection(..), LexicalFixity(..)
                        , TypeOrKind(..) )
import Outputable
import FastString
import Maybes
import qualified GHC.LanguageExtensions as LangExt

import Data.List          ( nubBy, partition, (\\) )
import Control.Monad      ( unless, when )

#include "HsVersions.h"

{-
These type renamers are in a separate module, rather than in (say) GHC.Rename.Source,
to break several loop.

*********************************************************
*                                                       *
           HsSigWcType (i.e with wildcards)
*                                                       *
*********************************************************
-}

data HsSigWcTypeScoping = AlwaysBind
                          -- ^ Always bind any free tyvars of the given type,
                          --   regardless of whether we have a forall at the top
                        | BindUnlessForall
                          -- ^ Unless there's forall at the top, do the same
                          --   thing as 'AlwaysBind'
                        | NeverBind
                          -- ^ Never bind any free tyvars

rnHsSigWcType :: HsSigWcTypeScoping -> HsDocContext -> LHsSigWcType GhcPs
              -> RnM (LHsSigWcType GhcRn, FreeVars)
rnHsSigWcType scoping doc sig_ty
  = rn_hs_sig_wc_type scoping doc sig_ty $ \sig_ty' ->
    return (sig_ty', emptyFVs)

rnHsSigWcTypeScoped :: HsSigWcTypeScoping
                       -- AlwaysBind: for pattern type sigs and rules we /do/ want
                       --             to bring those type variables into scope, even
                       --             if there's a forall at the top which usually
                       --             stops that happening
                       -- e.g  \ (x :: forall a. a-> b) -> e
                       -- Here we do bring 'b' into scope
                    -> HsDocContext -> LHsSigWcType GhcPs
                    -> (LHsSigWcType GhcRn -> RnM (a, FreeVars))
                    -> RnM (a, FreeVars)
-- Used for
--   - Signatures on binders in a RULE
--   - Pattern type signatures
-- Wildcards are allowed
-- type signatures on binders only allowed with ScopedTypeVariables
rnHsSigWcTypeScoped scoping ctx sig_ty thing_inside
  = do { ty_sig_okay <- xoptM LangExt.ScopedTypeVariables
       ; checkErr ty_sig_okay (unexpectedTypeSigErr sig_ty)
       ; rn_hs_sig_wc_type scoping ctx sig_ty thing_inside
       }

rn_hs_sig_wc_type :: HsSigWcTypeScoping -> HsDocContext -> LHsSigWcType GhcPs
                  -> (LHsSigWcType GhcRn -> RnM (a, FreeVars))
                  -> RnM (a, FreeVars)
-- rn_hs_sig_wc_type is used for source-language type signatures
rn_hs_sig_wc_type scoping ctxt
                  (HsWC { hswc_body = HsIB { hsib_body = hs_ty }})
                  thing_inside
  = do { free_vars <- extractFilteredRdrTyVarsDups hs_ty
       ; (nwc_rdrs', tv_rdrs) <- partition_nwcs free_vars
       ; let nwc_rdrs = nubL nwc_rdrs'
             bind_free_tvs = case scoping of
                               AlwaysBind       -> True
                               BindUnlessForall -> not (isLHsForAllTy hs_ty)
                               NeverBind        -> False
       ; rnImplicitBndrs bind_free_tvs tv_rdrs $ \ vars ->
    do { (wcs, hs_ty', fvs1) <- rnWcBody ctxt nwc_rdrs hs_ty
       ; let sig_ty' = HsWC { hswc_ext = wcs, hswc_body = ib_ty' }
             ib_ty'  = HsIB { hsib_ext = vars
                            , hsib_body = hs_ty' }
       ; (res, fvs2) <- thing_inside sig_ty'
       ; return (res, fvs1 `plusFV` fvs2) } }
rn_hs_sig_wc_type _ _ (HsWC _ (XHsImplicitBndrs nec)) _
  = noExtCon nec
rn_hs_sig_wc_type _ _ (XHsWildCardBndrs nec) _
  = noExtCon nec

rnHsWcType :: HsDocContext -> LHsWcType GhcPs -> RnM (LHsWcType GhcRn, FreeVars)
rnHsWcType ctxt (HsWC { hswc_body = hs_ty })
  = do { free_vars <- extractFilteredRdrTyVars hs_ty
       ; (nwc_rdrs, _) <- partition_nwcs free_vars
       ; (wcs, hs_ty', fvs) <- rnWcBody ctxt nwc_rdrs hs_ty
       ; let sig_ty' = HsWC { hswc_ext = wcs, hswc_body = hs_ty' }
       ; return (sig_ty', fvs) }
rnHsWcType _ (XHsWildCardBndrs nec) = noExtCon nec

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
       ; return (nwcs, hs_ty', fvs) }
  where
    rn_lty env (L loc hs_ty)
      = setSrcSpan loc $
        do { (hs_ty', fvs) <- rn_ty env hs_ty
           ; return (L loc hs_ty', fvs) }

    rn_ty :: RnTyKiEnv -> HsType GhcPs -> RnM (HsType GhcRn, FreeVars)
    -- A lot of faff just to allow the extra-constraints wildcard to appear
    rn_ty env hs_ty@(HsForAllTy { hst_fvf = fvf, hst_bndrs = tvs
                                , hst_body = hs_body })
      = bindLHsTyVarBndrs (rtke_ctxt env) (Just $ inTypeDoc hs_ty) Nothing tvs $ \ tvs' ->
        do { (hs_body', fvs) <- rn_lty env hs_body
           ; return (HsForAllTy { hst_fvf = fvf, hst_xforall = noExtField
                                , hst_bndrs = tvs', hst_body = hs_body' }
                    , fvs) }

    rn_ty env (HsQualTy { hst_ctxt = L cx hs_ctxt
                        , hst_body = hs_ty })
      | Just (hs_ctxt1, hs_ctxt_last) <- snocView hs_ctxt
      , L lx (HsWildCardTy _)  <- ignoreParens hs_ctxt_last
      = do { (hs_ctxt1', fvs1) <- mapFvRn (rn_top_constraint env) hs_ctxt1
           ; setSrcSpan lx $ checkExtraConstraintWildCard env hs_ctxt1
           ; let hs_ctxt' = hs_ctxt1' ++ [L lx (HsWildCardTy noExtField)]
           ; (hs_ty', fvs2) <- rnLHsTyKi env hs_ty
           ; return (HsQualTy { hst_xqual = noExtField
                              , hst_ctxt = L cx hs_ctxt', hst_body = hs_ty' }
                    , fvs1 `plusFV` fvs2) }

      | otherwise
      = do { (hs_ctxt', fvs1) <- mapFvRn (rn_top_constraint env) hs_ctxt
           ; (hs_ty', fvs2)   <- rnLHsTyKi env hs_ty
           ; return (HsQualTy { hst_xqual = noExtField
                              , hst_ctxt = L cx hs_ctxt'
                              , hst_body = hs_ty' }
                    , fvs1 `plusFV` fvs2) }

    rn_ty env hs_ty = rnHsTyKi env hs_ty

    rn_top_constraint env = rnLHsTyKi (env { rtke_what = RnTopConstraint })


checkExtraConstraintWildCard :: RnTyKiEnv -> HsContext GhcPs -> RnM ()
-- Rename the extra-constraint spot in a type signature
--    (blah, _) => type
-- Check that extra-constraints are allowed at all, and
-- if so that it's an anonymous wildcard
checkExtraConstraintWildCard env hs_ctxt
  = checkWildCard env mb_bad
  where
    mb_bad | not (extraConstraintWildCardsAllowed env)
           = Just base_msg
             -- Currently, we do not allow wildcards in their full glory in
             -- standalone deriving declarations. We only allow a single
             -- extra-constraints wildcard à la:
             --
             --   deriving instance _ => Eq (Foo a)
             --
             -- i.e., we don't support things like
             --
             --   deriving instance (Eq a, _) => Eq (Foo a)
           | DerivDeclCtx {} <- rtke_ctxt env
           , not (null hs_ctxt)
           = Just deriv_decl_msg
           | otherwise
           = Nothing

    base_msg = text "Extra-constraint wildcard" <+> quotes pprAnonWildCard
                   <+> text "not allowed"

    deriv_decl_msg
      = hang base_msg
           2 (vcat [ text "except as the sole constraint"
                   , nest 2 (text "e.g., deriving instance _ => Eq (Foo a)") ])

extraConstraintWildCardsAllowed :: RnTyKiEnv -> Bool
extraConstraintWildCardsAllowed env
  = case rtke_ctxt env of
      TypeSigCtx {}       -> True
      ExprWithTySigCtx {} -> True
      DerivDeclCtx {}     -> True
      StandaloneKindSigCtx {} -> False  -- See Note [Wildcards in standalone kind signatures] in GHC/Hs/Decls
      _                   -> False

-- | Finds free type and kind variables in a type,
--     without duplicates, and
--     without variables that are already in scope in LocalRdrEnv
--   NB: this includes named wildcards, which look like perfectly
--       ordinary type variables at this point
extractFilteredRdrTyVars :: LHsType GhcPs -> RnM FreeKiTyVarsNoDups
extractFilteredRdrTyVars hs_ty = filterInScopeM (extractHsTyRdrTyVars hs_ty)

-- | Finds free type and kind variables in a type,
--     with duplicates, but
--     without variables that are already in scope in LocalRdrEnv
--   NB: this includes named wildcards, which look like perfectly
--       ordinary type variables at this point
extractFilteredRdrTyVarsDups :: LHsType GhcPs -> RnM FreeKiTyVarsWithDups
extractFilteredRdrTyVarsDups hs_ty = filterInScopeM (extractHsTyRdrTyVarsDups hs_ty)

-- | When the NamedWildCards extension is enabled, partition_nwcs
-- removes type variables that start with an underscore from the
-- FreeKiTyVars in the argument and returns them in a separate list.
-- When the extension is disabled, the function returns the argument
-- and empty list.  See Note [Renaming named wild cards]
partition_nwcs :: FreeKiTyVars -> RnM ([Located RdrName], FreeKiTyVars)
partition_nwcs free_vars
  = do { wildcards_enabled <- xoptM LangExt.NamedWildCards
       ; return $
           if wildcards_enabled
           then partition is_wildcard free_vars
           else ([], free_vars) }
  where
     is_wildcard :: Located RdrName -> Bool
     is_wildcard rdr = startsWithUnderscore (rdrNameOcc (unLoc rdr))

{- Note [Renaming named wild cards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Identifiers starting with an underscore are always parsed as type variables.
It is only here in the renamer that we give the special treatment.
See Note [The wildcard story for types] in GHC.Hs.Types.

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

rnHsSigType :: HsDocContext
            -> TypeOrKind
            -> LHsSigType GhcPs
            -> RnM (LHsSigType GhcRn, FreeVars)
-- Used for source-language type signatures
-- that cannot have wildcards
rnHsSigType ctx level (HsIB { hsib_body = hs_ty })
  = do { traceRn "rnHsSigType" (ppr hs_ty)
       ; vars <- extractFilteredRdrTyVarsDups hs_ty
       ; rnImplicitBndrs (not (isLHsForAllTy hs_ty)) vars $ \ vars ->
    do { (body', fvs) <- rnLHsTyKi (mkTyKiEnv ctx level RnTypeBody) hs_ty

       ; return ( HsIB { hsib_ext = vars
                       , hsib_body = body' }
                , fvs ) } }
rnHsSigType _ _ (XHsImplicitBndrs nec) = noExtCon nec

rnImplicitBndrs :: Bool    -- True <=> bring into scope any free type variables
                           -- E.g.  f :: forall a. a->b
                           --  we do not want to bring 'b' into scope, hence False
                           -- But   f :: a -> b
                           --  we want to bring both 'a' and 'b' into scope
                -> FreeKiTyVarsWithDups
                                   -- Free vars of hs_ty (excluding wildcards)
                                   -- May have duplicates, which is
                                   -- checked here
                -> ([Name] -> RnM (a, FreeVars))
                -> RnM (a, FreeVars)
rnImplicitBndrs bind_free_tvs
                fvs_with_dups
                thing_inside
  = do { let fvs = nubL fvs_with_dups
             real_fvs | bind_free_tvs = fvs
                      | otherwise     = []

       ; traceRn "rnImplicitBndrs" $
         vcat [ ppr fvs_with_dups, ppr fvs, ppr real_fvs ]

       ; loc <- getSrcSpanM
       ; vars <- mapM (newLocalBndrRn . L loc . unLoc) real_fvs

       ; bindLocalNamesFV vars $
         thing_inside vars }

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
(#4426):

data A = A (a -> a)           is an error
data A = A (Eq a => a -> a)   binds "a"
data A = A (Eq a => a -> b)   binds "a" and "b"
data A = A (() => a -> b)     binds "a" and "b"
f :: forall a. a -> b         is an error
f :: forall a. () => a -> b   is an error
f :: forall a. a -> (() => b) binds "a" and "b"

This situation is now considered to be an error. See rnHsTyKi for case
HsForAllTy Qualified.

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

-- renaming a type only, not a kind
rnLHsTypeArg :: HsDocContext -> LHsTypeArg GhcPs
                -> RnM (LHsTypeArg GhcRn, FreeVars)
rnLHsTypeArg ctxt (HsValArg ty)
   = do { (tys_rn, fvs) <- rnLHsType ctxt ty
        ; return (HsValArg tys_rn, fvs) }
rnLHsTypeArg ctxt (HsTypeArg l ki)
   = do { (kis_rn, fvs) <- rnLHsKind ctxt ki
        ; return (HsTypeArg l kis_rn, fvs) }
rnLHsTypeArg _ (HsArgPar sp)
   = return (HsArgPar sp, emptyFVs)

rnLHsTypeArgs :: HsDocContext -> [LHsTypeArg GhcPs]
                 -> RnM ([LHsTypeArg GhcRn], FreeVars)
rnLHsTypeArgs doc args = mapFvRn (rnLHsTypeArg doc) args

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

rnHsTyKi env ty@(HsForAllTy { hst_fvf = fvf, hst_bndrs = tyvars
                            , hst_body = tau })
  = do { checkPolyKinds env ty
       ; bindLHsTyVarBndrs (rtke_ctxt env) (Just $ inTypeDoc ty)
                           Nothing tyvars $ \ tyvars' ->
    do { (tau',  fvs) <- rnLHsTyKi env tau
       ; return ( HsForAllTy { hst_fvf = fvf, hst_xforall = noExtField
                             , hst_bndrs = tyvars' , hst_body =  tau' }
                , fvs) } }

rnHsTyKi env ty@(HsQualTy { hst_ctxt = lctxt, hst_body = tau })
  = do { checkPolyKinds env ty  -- See Note [QualTy in kinds]
       ; (ctxt', fvs1) <- rnTyKiContext env lctxt
       ; (tau',  fvs2) <- rnLHsTyKi env tau
       ; return (HsQualTy { hst_xqual = noExtField, hst_ctxt = ctxt'
                          , hst_body =  tau' }
                , fvs1 `plusFV` fvs2) }

rnHsTyKi env (HsTyVar _ ip (L loc rdr_name))
  = do { when (isRnKindLevel env && isRdrTyVar rdr_name) $
         unlessXOptM LangExt.PolyKinds $ addErr $
         withHsDocContext (rtke_ctxt env) $
         vcat [ text "Unexpected kind variable" <+> quotes (ppr rdr_name)
              , text "Perhaps you intended to use PolyKinds" ]
           -- Any type variable at the kind level is illegal without the use
           -- of PolyKinds (see #14710)
       ; name <- rnTyVar env rdr_name
       ; return (HsTyVar noExtField ip (L loc name), unitFV name) }

rnHsTyKi env ty@(HsOpTy _ ty1 l_op ty2)
  = setSrcSpan (getLoc l_op) $
    do  { (l_op', fvs1) <- rnHsTyOp env ty l_op
        ; fix   <- lookupTyFixityRn l_op'
        ; (ty1', fvs2) <- rnLHsTyKi env ty1
        ; (ty2', fvs3) <- rnLHsTyKi env ty2
        ; res_ty <- mkHsOpTyRn (\t1 t2 -> HsOpTy noExtField t1 l_op' t2)
                               (unLoc l_op') fix ty1' ty2'
        ; return (res_ty, plusFVs [fvs1, fvs2, fvs3]) }

rnHsTyKi env (HsParTy _ ty)
  = do { (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsParTy noExtField ty', fvs) }

rnHsTyKi env (HsBangTy _ b ty)
  = do { (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsBangTy noExtField b ty', fvs) }

rnHsTyKi env ty@(HsRecTy _ flds)
  = do { let ctxt = rtke_ctxt env
       ; fls          <- get_fields ctxt
       ; (flds', fvs) <- rnConDeclFields ctxt fls flds
       ; return (HsRecTy noExtField flds', fvs) }
  where
    get_fields (ConDeclCtx names)
      = concatMapM (lookupConstructorFields . unLoc) names
    get_fields _
      = do { addErr (hang (text "Record syntax is illegal here:")
                                   2 (ppr ty))
           ; return [] }

rnHsTyKi env (HsFunTy _ ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi env ty1
        -- Might find a for-all as the arg of a function type
       ; (ty2', fvs2) <- rnLHsTyKi env ty2
        -- Or as the result.  This happens when reading Prelude.hi
        -- when we find return :: forall m. Monad m -> forall a. a -> m a

        -- Check for fixity rearrangements
       ; res_ty <- mkHsOpTyRn (HsFunTy noExtField) funTyConName funTyFixity ty1' ty2'
       ; return (res_ty, fvs1 `plusFV` fvs2) }

rnHsTyKi env listTy@(HsListTy _ ty)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; when (not data_kinds && isRnKindLevel env)
              (addErr (dataKindsErr env listTy))
       ; (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsListTy noExtField ty', fvs) }

rnHsTyKi env t@(HsKindSig _ ty k)
  = do { checkPolyKinds env t
       ; kind_sigs_ok <- xoptM LangExt.KindSignatures
       ; unless kind_sigs_ok (badKindSigErr (rtke_ctxt env) ty)
       ; (ty', lhs_fvs) <- rnLHsTyKi env ty
       ; (k', sig_fvs)  <- rnLHsTyKi (env { rtke_level = KindLevel }) k
       ; return (HsKindSig noExtField ty' k', lhs_fvs `plusFV` sig_fvs) }

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsTyKi env tupleTy@(HsTupleTy _ tup_con tys)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; when (not data_kinds && isRnKindLevel env)
              (addErr (dataKindsErr env tupleTy))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsTupleTy noExtField tup_con tys', fvs) }

rnHsTyKi env sumTy@(HsSumTy _ tys)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; when (not data_kinds && isRnKindLevel env)
              (addErr (dataKindsErr env sumTy))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsSumTy noExtField tys', fvs) }

-- Ensure that a type-level integer is nonnegative (#8306, #8412)
rnHsTyKi env tyLit@(HsTyLit _ t)
  = do { data_kinds <- xoptM LangExt.DataKinds
       ; unless data_kinds (addErr (dataKindsErr env tyLit))
       ; when (negLit t) (addErr negLitErr)
       ; checkPolyKinds env tyLit
       ; return (HsTyLit noExtField t, emptyFVs) }
  where
    negLit (HsStrTy _ _) = False
    negLit (HsNumTy _ i) = i < 0
    negLitErr = text "Illegal literal in type (type literals must not be negative):" <+> ppr tyLit

rnHsTyKi env (HsAppTy _ ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi env ty1
       ; (ty2', fvs2) <- rnLHsTyKi env ty2
       ; return (HsAppTy noExtField ty1' ty2', fvs1 `plusFV` fvs2) }

rnHsTyKi env (HsAppKindTy l ty k)
  = do { kind_app <- xoptM LangExt.TypeApplications
       ; unless kind_app (addErr (typeAppErr "kind" k))
       ; (ty', fvs1) <- rnLHsTyKi env ty
       ; (k', fvs2) <- rnLHsTyKi (env {rtke_level = KindLevel }) k
       ; return (HsAppKindTy l ty' k', fvs1 `plusFV` fvs2) }

rnHsTyKi env t@(HsIParamTy _ n ty)
  = do { notInKinds env t
       ; (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsIParamTy noExtField n ty', fvs) }

rnHsTyKi _ (HsStarTy _ isUni)
  = return (HsStarTy noExtField isUni, emptyFVs)

rnHsTyKi _ (HsSpliceTy _ sp)
  = rnSpliceType sp

rnHsTyKi env (HsDocTy _ ty haddock_doc)
  = do { (ty', fvs) <- rnLHsTyKi env ty
       ; haddock_doc' <- rnLHsDoc haddock_doc
       ; return (HsDocTy noExtField ty' haddock_doc', fvs) }

rnHsTyKi _ (XHsType (NHsCoreTy ty))
  = return (XHsType (NHsCoreTy ty), emptyFVs)
    -- The emptyFVs probably isn't quite right
    -- but I don't think it matters

rnHsTyKi env ty@(HsExplicitListTy _ ip tys)
  = do { checkPolyKinds env ty
       ; data_kinds <- xoptM LangExt.DataKinds
       ; unless data_kinds (addErr (dataKindsErr env ty))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsExplicitListTy noExtField ip tys', fvs) }

rnHsTyKi env ty@(HsExplicitTupleTy _ tys)
  = do { checkPolyKinds env ty
       ; data_kinds <- xoptM LangExt.DataKinds
       ; unless data_kinds (addErr (dataKindsErr env ty))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsExplicitTupleTy noExtField tys', fvs) }

rnHsTyKi env (HsWildCardTy _)
  = do { checkAnonWildCard env
       ; return (HsWildCardTy noExtField, emptyFVs) }

--------------
rnTyVar :: RnTyKiEnv -> RdrName -> RnM Name
rnTyVar env rdr_name
  = do { name <- lookupTypeOccRn rdr_name
       ; checkNamedWildCard env name
       ; return name }

rnLTyVar :: Located RdrName -> RnM (Located Name)
-- Called externally; does not deal with wildcards
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
       ; unless (ops_ok || op' `hasKey` eqTyConKey) $
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

checkAnonWildCard :: RnTyKiEnv -> RnM ()
-- Report an error if an anonymous wildcard is illegal here
checkAnonWildCard env
  = checkWildCard env mb_bad
  where
    mb_bad :: Maybe SDoc
    mb_bad | not (wildCardsAllowed env)
           = Just (notAllowed pprAnonWildCard)
           | otherwise
           = case rtke_what env of
               RnTypeBody      -> Nothing
               RnTopConstraint -> Just constraint_msg
               RnConstraint    -> Just constraint_msg

    constraint_msg = hang
                         (notAllowed pprAnonWildCard <+> text "in a constraint")
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
               RnTopConstraint -> Nothing   -- Allowed; e.g.
                  -- f :: (Eq _a) => _a -> Int
                  -- g :: (_a, _b) => T _a _b -> Int
                  -- The named tyvars get filled in from elsewhere
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
       StandaloneKindSigCtx {} -> False  -- See Note [Wildcards in standalone kind signatures] in GHC/Hs/Decls
       _                   -> False



---------------
-- | Ensures either that we're in a type or that -XPolyKinds is set
checkPolyKinds :: Outputable ty
                => RnTyKiEnv
                -> ty      -- ^ type
                -> RnM ()
checkPolyKinds env ty
  | isRnKindLevel env
  = do { polykinds <- xoptM LangExt.PolyKinds
       ; unless polykinds $
         addErr (text "Illegal kind:" <+> ppr ty $$
                 text "Did you mean to enable PolyKinds?") }
checkPolyKinds _ _ = return ()

notInKinds :: Outputable ty
           => RnTyKiEnv
           -> ty
           -> RnM ()
notInKinds env ty
  | isRnKindLevel env
  = addErr (text "Illegal kind:" <+> ppr ty)
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
              -> Maybe SDoc         -- Just d => check for unused tvs
                                    --   d is a phrase like "in the type ..."
              -> Maybe a            -- Just _  => an associated type decl
              -> [Located RdrName]  -- Kind variables from scope, no dups
              -> (LHsQTyVars GhcPs)
              -> (LHsQTyVars GhcRn -> Bool -> RnM (b, FreeVars))
                  -- The Bool is True <=> all kind variables used in the
                  -- kind signature are bound on the left.  Reason:
                  -- the last clause of Note [CUSKs: Complete user-supplied
                  -- kind signatures] in GHC.Hs.Decls
              -> RnM (b, FreeVars)

-- See Note [bindHsQTyVars examples]
-- (a) Bring kind variables into scope
--     both (i)  passed in body_kv_occs
--     and  (ii) mentioned in the kinds of hsq_bndrs
-- (b) Bring type variables into scope
--
bindHsQTyVars doc mb_in_doc mb_assoc body_kv_occs hsq_bndrs thing_inside
  = do { let hs_tv_bndrs = hsQTvExplicit hsq_bndrs
             bndr_kv_occs = extractHsTyVarBndrsKVs hs_tv_bndrs

       ; let -- See Note [bindHsQTyVars examples] for what
             -- all these various things are doing
             bndrs, kv_occs, implicit_kvs :: [Located RdrName]
             bndrs        = map hsLTyVarLocName hs_tv_bndrs
             kv_occs      = nubL (bndr_kv_occs ++ body_kv_occs)
                                 -- Make sure to list the binder kvs before the
                                 -- body kvs, as mandated by
                                 -- Note [Ordering of implicit variables]
             implicit_kvs = filter_occs bndrs kv_occs
             del          = deleteBys eqLocated
             all_bound_on_lhs = null ((body_kv_occs `del` bndrs) `del` bndr_kv_occs)

       ; traceRn "checkMixedVars3" $
           vcat [ text "kv_occs" <+> ppr kv_occs
                , text "bndrs"   <+> ppr hs_tv_bndrs
                , text "bndr_kv_occs"   <+> ppr bndr_kv_occs
                , text "wubble" <+> ppr ((kv_occs \\ bndrs) \\ bndr_kv_occs)
                ]

       ; implicit_kv_nms <- mapM (newTyVarNameRn mb_assoc) implicit_kvs

       ; bindLocalNamesFV implicit_kv_nms                     $
         bindLHsTyVarBndrs doc mb_in_doc mb_assoc hs_tv_bndrs $ \ rn_bndrs ->
    do { traceRn "bindHsQTyVars" (ppr hsq_bndrs $$ ppr implicit_kv_nms $$ ppr rn_bndrs)
       ; thing_inside (HsQTvs { hsq_ext = implicit_kv_nms
                              , hsq_explicit  = rn_bndrs })
                      all_bound_on_lhs } }

  where
    filter_occs :: [Located RdrName]   -- Bound here
                -> [Located RdrName]   -- Potential implicit binders
                -> [Located RdrName]   -- Final implicit binders
    -- Filter out any potential implicit binders that are either
    -- already in scope, or are explicitly bound in the same HsQTyVars
    filter_occs bndrs occs
      = filterOut is_in_scope occs
      where
        is_in_scope locc = locc `elemRdr` bndrs

{- Note [bindHsQTyVars examples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we have
   data T k (a::k1) (b::k) :: k2 -> k1 -> *

Then:
  hs_tv_bndrs = [k, a::k1, b::k], the explicitly-bound variables
  bndrs       = [k,a,b]

  bndr_kv_occs = [k,k1], kind variables free in kind signatures
                         of hs_tv_bndrs

  body_kv_occs = [k2,k1], kind variables free in the
                          result kind signature

  implicit_kvs = [k1,k2], kind variables free in kind signatures
                          of hs_tv_bndrs, and not bound by bndrs

* We want to quantify add implicit bindings for implicit_kvs

* If implicit_body_kvs is non-empty, then there is a kind variable
  mentioned in the kind signature that is not bound "on the left".
  That's one of the rules for a CUSK, so we pass that info on
  as the second argument to thing_inside.

* Order is not important in these lists.  All we are doing is
  bring Names into scope.

Finally, you may wonder why filter_occs removes in-scope variables
from bndr/body_kv_occs.  How can anything be in scope?  Answer:
HsQTyVars is /also/ used (slightly oddly) for Haskell-98 syntax
ConDecls
   data T a = forall (b::k). MkT a b
The ConDecl has a LHsQTyVars in it; but 'a' scopes over the entire
ConDecl.  Hence the local RdrEnv may be non-empty and we must filter
out 'a' from the free vars.  (Mind you, in this situation all the
implicit kind variables are bound at the data type level, so there
are none to bind in the ConDecl, so there are no implicitly bound
variables at all.

Note [Kind variable scoping]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we have
  data T (a :: k) k = ...
we report "k is out of scope" for (a::k).  Reason: k is not brought
into scope until the explicit k-binding that follows.  It would be
terribly confusing to bring into scope an /implicit/ k for a's kind
and a distinct, shadowing explicit k that follows, something like
  data T {k1} (a :: k1) k = ...

So the rule is:

   the implicit binders never include any
   of the explicit binders in the group

Note that in the denerate case
  data T (a :: a) = blah
we get a complaint the second 'a' is not in scope.

That applies to foralls too: e.g.
   forall (a :: k) k . blah

But if the foralls are split, we treat the two groups separately:
   forall (a :: k). forall k. blah
Here we bring into scope an implicit k, which is later shadowed
by the explicit k.

In implementation terms

* In bindHsQTyVars 'k' is free in bndr_kv_occs; then we delete
  the binders {a,k}, and so end with no implicit binders.  Then we
  rename the binders left-to-right, and hence see that 'k' is out of
  scope in the kind of 'a'.

* Similarly in extract_hs_tv_bndrs

Note [Variables used as both types and kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We bind the type variables tvs, and kvs is the set of free variables of the
kinds in the scope of the binding. Here is one typical example:

   forall a b. a -> (b::k) -> (c::a)

Here, tvs will be {a,b}, and kvs {k,a}.

We must make sure that kvs includes all of variables in the kinds of type
variable bindings. For instance:

   forall k (a :: k). Proxy a

If we only look in the body of the `forall` type, we will mistakenly conclude
that kvs is {}. But in fact, the type variable `k` is also used as a kind
variable in (a :: k), later in the binding. (This mistake lead to #14710.)
So tvs is {k,a} and kvs is {k}.

NB: we do this only at the binding site of 'tvs'.
-}

bindLHsTyVarBndrs :: HsDocContext
                  -> Maybe SDoc            -- Just d => check for unused tvs
                                           --   d is a phrase like "in the type ..."
                  -> Maybe a               -- Just _  => an associated type decl
                  -> [LHsTyVarBndr GhcPs]  -- User-written tyvars
                  -> ([LHsTyVarBndr GhcRn] -> RnM (b, FreeVars))
                  -> RnM (b, FreeVars)
bindLHsTyVarBndrs doc mb_in_doc mb_assoc tv_bndrs thing_inside
  = do { when (isNothing mb_assoc) (checkShadowedRdrNames tv_names_w_loc)
       ; checkDupRdrNames tv_names_w_loc
       ; go tv_bndrs thing_inside }
  where
    tv_names_w_loc = map hsLTyVarLocName tv_bndrs

    go []     thing_inside = thing_inside []
    go (b:bs) thing_inside = bindLHsTyVarBndr doc mb_assoc b $ \ b' ->
                             do { (res, fvs) <- go bs $ \ bs' ->
                                                thing_inside (b' : bs')
                                ; warn_unused b' fvs
                                ; return (res, fvs) }

    warn_unused tv_bndr fvs = case mb_in_doc of
      Just in_doc -> warnUnusedForAll in_doc tv_bndr fvs
      Nothing     -> return ()

bindLHsTyVarBndr :: HsDocContext
                 -> Maybe a   -- associated class
                 -> LHsTyVarBndr GhcPs
                 -> (LHsTyVarBndr GhcRn -> RnM (b, FreeVars))
                 -> RnM (b, FreeVars)
bindLHsTyVarBndr _doc mb_assoc (L loc
                                 (UserTyVar x
                                    lrdr@(L lv _))) thing_inside
  = do { nm <- newTyVarNameRn mb_assoc lrdr
       ; bindLocalNamesFV [nm] $
         thing_inside (L loc (UserTyVar x (L lv nm))) }

bindLHsTyVarBndr doc mb_assoc (L loc (KindedTyVar x lrdr@(L lv _) kind))
                 thing_inside
  = do { sig_ok <- xoptM LangExt.KindSignatures
           ; unless sig_ok (badKindSigErr doc kind)
           ; (kind', fvs1) <- rnLHsKind doc kind
           ; tv_nm  <- newTyVarNameRn mb_assoc lrdr
           ; (b, fvs2) <- bindLocalNamesFV [tv_nm]
               $ thing_inside (L loc (KindedTyVar x (L lv tv_nm) kind'))
           ; return (b, fvs1 `plusFV` fvs2) }

bindLHsTyVarBndr _ _ (L _ (XTyVarBndr nec)) _ = noExtCon nec

newTyVarNameRn :: Maybe a -> Located RdrName -> RnM Name
newTyVarNameRn mb_assoc (L loc rdr)
  = do { rdr_env <- getLocalRdrEnv
       ; case (mb_assoc, lookupLocalRdrEnv rdr_env rdr) of
           (Just _, Just n) -> return n
              -- Use the same Name as the parent class decl

           _                -> newLocalBndrRn (L loc rdr) }
{-
*********************************************************
*                                                       *
        ConDeclField
*                                                       *
*********************************************************

When renaming a ConDeclField, we have to find the FieldLabel
associated with each field.  But we already have all the FieldLabels
available (since they were brought into scope by
GHC.Rename.Names.getLocalNonValBinders), so we just take the list as an
argument, build a map and look them up.
-}

rnConDeclFields :: HsDocContext -> [FieldLabel] -> [LConDeclField GhcPs]
                -> RnM ([LConDeclField GhcRn], FreeVars)
-- Also called from GHC.Rename.Source
-- No wildcards can appear in record fields
rnConDeclFields ctxt fls fields
   = mapFvRn (rnField fl_env env) fields
  where
    env    = mkTyKiEnv ctxt TypeLevel RnTypeBody
    fl_env = mkFsEnv [ (flLabel fl, fl) | fl <- fls ]

rnField :: FastStringEnv FieldLabel -> RnTyKiEnv -> LConDeclField GhcPs
        -> RnM (LConDeclField GhcRn, FreeVars)
rnField fl_env env (L l (ConDeclField _ names ty haddock_doc))
  = do { let new_names = map (fmap lookupField) names
       ; (new_ty, fvs) <- rnLHsTyKi env ty
       ; new_haddock_doc <- rnMbLHsDoc haddock_doc
       ; return (L l (ConDeclField noExtField new_names new_ty new_haddock_doc)
                , fvs) }
  where
    lookupField :: FieldOcc GhcPs -> FieldOcc GhcRn
    lookupField (FieldOcc _ (L lr rdr)) =
        FieldOcc (flSelector fl) (L lr rdr)
      where
        lbl = occNameFS $ rdrNameOcc rdr
        fl  = expectJust "rnField" $ lookupFsEnv fl_env lbl
    lookupField (XFieldOcc nec) = noExtCon nec
rnField _ _ (L _ (XConDeclField nec)) = noExtCon nec

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

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsOpTy noExtField ty21 op2 ty22))
  = do  { fix2 <- lookupTyFixityRn op2
        ; mk_hs_op_ty mk1 pp_op1 fix1 ty1
                      (\t1 t2 -> HsOpTy noExtField t1 op2 t2)
                      (unLoc op2) fix2 ty21 ty22 loc2 }

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsFunTy _ ty21 ty22))
  = mk_hs_op_ty mk1 pp_op1 fix1 ty1
                (HsFunTy noExtField) funTyConName funTyFixity ty21 ty22 loc2

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
mkOpAppRn e1@(L _ (OpApp fix1 e11 op1 e12)) op2 fix2 e2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (OpApp fix2 e1 op2 e2)

  | associate_right = do
    new_e <- mkOpAppRn e12 op2 fix2 e2
    return (OpApp fix1 e11 op1 (L loc' new_e))
  where
    loc'= combineLocs e12 e2
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--      (- neg_arg) `op` e2
mkOpAppRn e1@(L _ (NegApp _ neg_arg neg_name)) op2 fix2 e2
  | nofix_error
  = do precParseErr (NegateOp,negateFixity) (get_op op2,fix2)
       return (OpApp fix2 e1 op2 e2)

  | associate_right
  = do new_e <- mkOpAppRn neg_arg op2 fix2 e2
       return (NegApp noExtField (L loc' new_e) neg_name)
  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--      e1 `op` - neg_arg
mkOpAppRn e1 op1 fix1 e2@(L _ (NegApp {})) -- NegApp can occur on the right
  | not associate_right                        -- We *want* right association
  = do precParseErr (get_op op1, fix1) (NegateOp, negateFixity)
       return (OpApp fix1 e1 op1 e2)
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--      Default case
mkOpAppRn e1 op fix e2                  -- Default case, no rearrangment
  = ASSERT2( right_op_ok fix (unLoc e2),
             ppr e1 $$ text "---" $$ ppr op $$ text "---" $$ ppr fix $$ text "---" $$ ppr e2
    )
    return (OpApp fix e1 op e2)

----------------------------

-- | Name of an operator in an operator application or section
data OpName = NormalOp Name         -- ^ A normal identifier
            | NegateOp              -- ^ Prefix negation
            | UnboundOp OccName     -- ^ An unbound indentifier
            | RecFldOp (AmbiguousFieldOcc GhcRn)
              -- ^ A (possibly ambiguous) record field occurrence

instance Outputable OpName where
  ppr (NormalOp n)   = ppr n
  ppr NegateOp       = ppr negateName
  ppr (UnboundOp uv) = ppr uv
  ppr (RecFldOp fld) = ppr fld

get_op :: LHsExpr GhcRn -> OpName
-- An unbound name could be either HsVar or HsUnboundVar
-- See GHC.Rename.Expr.rnUnboundVar
get_op (L _ (HsVar _ n))         = NormalOp (unLoc n)
get_op (L _ (HsUnboundVar _ uv)) = UnboundOp uv
get_op (L _ (HsRecFld _ fld))    = RecFldOp fld
get_op other                     = pprPanic "get_op" (ppr other)

-- Parser left-associates everything, but
-- derived instances may have correctly-associated things to
-- in the right operand.  So we just check that the right operand is OK
right_op_ok :: Fixity -> HsExpr GhcRn -> Bool
right_op_ok fix1 (OpApp fix2 _ _ _)
  = not error_please && associate_right
  where
    (error_please, associate_right) = compareFixity fix1 fix2
right_op_ok _ _
  = True

-- Parser initially makes negation bind more tightly than any other operator
-- And "deriving" code should respect this (use HsPar if not)
mkNegAppRn :: LHsExpr (GhcPass id) -> SyntaxExpr (GhcPass id)
           -> RnM (HsExpr (GhcPass id))
mkNegAppRn neg_arg neg_name
  = ASSERT( not_op_app (unLoc neg_arg) )
    return (NegApp noExtField neg_arg neg_name)

not_op_app :: HsExpr id -> Bool
not_op_app (OpApp {}) = False
not_op_app _          = True

---------------------------
mkOpFormRn :: LHsCmdTop GhcRn            -- Left operand; already rearranged
          -> LHsExpr GhcRn -> Fixity     -- Operator and fixity
          -> LHsCmdTop GhcRn             -- Right operand (not an infix)
          -> RnM (HsCmd GhcRn)

-- (e11 `op1` e12) `op2` e2
mkOpFormRn a1@(L loc
                    (HsCmdTop _
                     (L _ (HsCmdArrForm x op1 f (Just fix1)
                        [a11,a12]))))
        op2 fix2 a2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (HsCmdArrForm x op2 f (Just fix2) [a1, a2])

  | associate_right
  = do new_c <- mkOpFormRn a12 op2 fix2 a2
       return (HsCmdArrForm noExtField op1 f (Just fix1)
               [a11, L loc (HsCmdTop [] (L loc new_c))])
        -- TODO: locs are wrong
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2

--      Default case
mkOpFormRn arg1 op fix arg2                     -- Default case, no rearrangment
  = return (HsCmdArrForm noExtField op Infix (Just fix) [arg1, arg2])


--------------------------------------
mkConOpPatRn :: Located Name -> Fixity -> LPat GhcRn -> LPat GhcRn
             -> RnM (Pat GhcRn)

mkConOpPatRn op2 fix2 p1@(L loc (ConPat op1 (InfixCon p11 p12) NoExtField)) p2
  = do  { fix1 <- lookupFixityRn (unLoc op1)
        ; let (nofix_error, associate_right) = compareFixity fix1 fix2

        ; if nofix_error then do
                { precParseErr (NormalOp (unLoc op1),fix1)
                               (NormalOp (unLoc op2),fix2)
                ; return (ConPat op2 (InfixCon p1 p2) NoExtField) }

          else if associate_right then do
                { new_p <- mkConOpPatRn op2 fix2 p12 p2
                ; return (ConPat op1 (InfixCon p11 (L loc new_p)) NoExtField) }
                -- XXX loc right?
          else return (ConPat op2 (InfixCon p1 p2) NoExtField) }

mkConOpPatRn op _ p1 p2                         -- Default case, no rearrangment
  = ASSERT( not_op_pat (unLoc p2) )
    return (ConPat op (InfixCon p1 p2) NoExtField)

not_op_pat :: Pat GhcRn -> Bool
not_op_pat (ConPat _ (InfixCon _ _) NoExtField) = False
not_op_pat _                                    = True

--------------------------------------
checkPrecMatch :: Name -> MatchGroup GhcRn body -> RnM ()
  -- Check precedence of a function binding written infix
  --   eg  a `op` b `C` c = ...
  -- See comments with rnExpr (OpApp ...) about "deriving"

checkPrecMatch op (MG { mg_alts = (L _ ms) })
  = mapM_ check ms
  where
    check (L _ (Match { m_pats = (L l1 p1)
                               : (L l2 p2)
                               : _ }))
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
checkPrecMatch _ (XMatchGroup nec) = noExtCon nec

checkPrec :: Name -> Pat GhcRn -> Bool -> IOEnv (Env TcGblEnv TcLclEnv) ()
checkPrec op (ConPat op1 (InfixCon _ _) NoExtField) right = do
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
        OpApp fix _ op' _ -> go_for_it (get_op op') fix
        NegApp _ _ _      -> go_for_it NegateOp     negateFixity
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
-- (see #13132).
lookupFixityOp :: OpName -> RnM Fixity
lookupFixityOp (NormalOp n)  = lookupFixityRn n
lookupFixityOp NegateOp      = lookupFixityRn negateName
lookupFixityOp (UnboundOp u) = lookupFixityRn (mkUnboundName u)
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
         2 (text "Use TypeOperators to allow operators in types")

{-
************************************************************************
*                                                                      *
      Finding the free type variables of a (HsType RdrName)
*                                                                      *
************************************************************************


Note [Kind and type-variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type signature we may implicitly bind type/kind variables. For example:
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

To do that, we need to walk over a type and find its free type/kind variables.
We preserve the left-to-right order of each variable occurrence.
See Note [Ordering of implicit variables].

Clients of this code can remove duplicates with nubL.

Note [Ordering of implicit variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since the advent of -XTypeApplications, GHC makes promises about the ordering
of implicit variable quantification. Specifically, we offer that implicitly
quantified variables (such as those in const :: a -> b -> a, without a `forall`)
will occur in left-to-right order of first occurrence. Here are a few examples:

  const :: a -> b -> a       -- forall a b. ...
  f :: Eq a => b -> a -> a   -- forall a b. ...  contexts are included

  type a <-< b = b -> a
  g :: a <-< b               -- forall a b. ...  type synonyms matter

  class Functor f where
    fmap :: (a -> b) -> f a -> f b   -- forall f a b. ...
    -- The f is quantified by the class, so only a and b are considered in fmap

This simple story is complicated by the possibility of dependency: all variables
must come after any variables mentioned in their kinds.

  typeRep :: Typeable a => TypeRep (a :: k)   -- forall k a. ...

The k comes first because a depends on k, even though the k appears later than
the a in the code. Thus, GHC does ScopedSort on the variables.
See Note [ScopedSort] in Type.

Implicitly bound variables are collected by any function which returns a
FreeKiTyVars, FreeKiTyVarsWithDups, or FreeKiTyVarsNoDups, which notably
includes the `extract-` family of functions (extractHsTysRdrTyVarsDups,
extractHsTyVarBndrsKVs, etc.).
These functions thus promise to keep left-to-right ordering.

Note [Implicit quantification in type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We typically bind type/kind variables implicitly when they are in a kind
annotation on the LHS, for example:

  data Proxy (a :: k) = Proxy
  type KindOf (a :: k) = k

Here 'k' is in the kind annotation of a type variable binding, KindedTyVar, and
we want to implicitly quantify over it.  This is easy: just extract all free
variables from the kind signature. That's what we do in extract_hs_tv_bndrs_kvs

By contrast, on the RHS we can't simply collect *all* free variables. Which of
the following are allowed?

  type TySyn1 = a :: Type
  type TySyn2 = 'Nothing :: Maybe a
  type TySyn3 = 'Just ('Nothing :: Maybe a)
  type TySyn4 = 'Left a :: Either Type a

After some design deliberations (see non-taken alternatives below), the answer
is to reject TySyn1 and TySyn3, but allow TySyn2 and TySyn4, at least for now.
We implicitly quantify over free variables of the outermost kind signature, if
one exists:

  * In TySyn1, the outermost kind signature is (:: Type), and it does not have
    any free variables.
  * In TySyn2, the outermost kind signature is (:: Maybe a), it contains a
    free variable 'a', which we implicitly quantify over.
  * In TySyn3, there is no outermost kind signature. The (:: Maybe a) signature
    is hidden inside 'Just.
  * In TySyn4, the outermost kind signature is (:: Either Type a), it contains
    a free variable 'a', which we implicitly quantify over. That is why we can
    also use it to the left of the double colon: 'Left a

The logic resides in extractHsTyRdrTyVarsKindVars. We use it both for type
synonyms and type family instances.

This is something of a stopgap solution until we can explicitly bind invisible
type/kind variables:

  type TySyn3 :: forall a. Maybe a
  type TySyn3 @a = 'Just ('Nothing :: Maybe a)

Note [Implicit quantification in type synonyms: non-taken alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Alternative I: No quantification
--------------------------------
We could offer no implicit quantification on the RHS, accepting none of the
TySyn<N> examples. The user would have to bind the variables explicitly:

  type TySyn1 a = a :: Type
  type TySyn2 a = 'Nothing :: Maybe a
  type TySyn3 a = 'Just ('Nothing :: Maybe a)
  type TySyn4 a = 'Left a :: Either Type a

However, this would mean that one would have to specify 'a' at call sites every
time, which could be undesired.

Alternative II: Indiscriminate quantification
---------------------------------------------
We could implicitly quantify over all free variables on the RHS just like we do
on the LHS. Then we would infer the following kinds:

  TySyn1 :: forall {a}. Type
  TySyn2 :: forall {a}. Maybe a
  TySyn3 :: forall {a}. Maybe (Maybe a)
  TySyn4 :: forall {a}. Either Type a

This would work fine for TySyn<2,3,4>, but TySyn1 is clearly bogus: the variable
is free-floating, not fixed by anything.

Alternative III: reportFloatingKvs
----------------------------------
We could augment Alternative II by hunting down free-floating variables during
type checking. While viable, this would mean we'd end up accepting this:

  data Prox k (a :: k)
  type T = Prox k

-}

-- See Note [Kind and type-variable binders]
-- These lists are guaranteed to preserve left-to-right ordering of
-- the types the variables were extracted from. See also
-- Note [Ordering of implicit variables].
type FreeKiTyVars = [Located RdrName]

-- | A 'FreeKiTyVars' list that is allowed to have duplicate variables.
type FreeKiTyVarsWithDups = FreeKiTyVars

-- | A 'FreeKiTyVars' list that contains no duplicate variables.
type FreeKiTyVarsNoDups   = FreeKiTyVars

filterInScope :: LocalRdrEnv -> FreeKiTyVars -> FreeKiTyVars
filterInScope rdr_env = filterOut (inScope rdr_env . unLoc)

filterInScopeM :: FreeKiTyVars -> RnM FreeKiTyVars
filterInScopeM vars
  = do { rdr_env <- getLocalRdrEnv
       ; return (filterInScope rdr_env vars) }

inScope :: LocalRdrEnv -> RdrName -> Bool
inScope rdr_env rdr = rdr `elemLocalRdrEnv` rdr_env

extract_tyarg :: LHsTypeArg GhcPs -> FreeKiTyVarsWithDups -> FreeKiTyVarsWithDups
extract_tyarg (HsValArg ty) acc = extract_lty ty acc
extract_tyarg (HsTypeArg _ ki) acc = extract_lty ki acc
extract_tyarg (HsArgPar _) acc = acc

extract_tyargs :: [LHsTypeArg GhcPs] -> FreeKiTyVarsWithDups -> FreeKiTyVarsWithDups
extract_tyargs args acc = foldr extract_tyarg acc args

extractHsTyArgRdrKiTyVarsDup :: [LHsTypeArg GhcPs] -> FreeKiTyVarsWithDups
extractHsTyArgRdrKiTyVarsDup args
  = extract_tyargs args []

-- | 'extractHsTyRdrTyVars' finds the type/kind variables
--                          of a HsType/HsKind.
-- It's used when making the @forall@s explicit.
-- When the same name occurs multiple times in the types, only the first
-- occurrence is returned.
-- See Note [Kind and type-variable binders]
extractHsTyRdrTyVars :: LHsType GhcPs -> FreeKiTyVarsNoDups
extractHsTyRdrTyVars ty
  = nubL (extractHsTyRdrTyVarsDups ty)

-- | 'extractHsTyRdrTyVarsDups' finds the type/kind variables
--                              of a HsType/HsKind.
-- It's used when making the @forall@s explicit.
-- When the same name occurs multiple times in the types, all occurrences
-- are returned.
extractHsTyRdrTyVarsDups :: LHsType GhcPs -> FreeKiTyVarsWithDups
extractHsTyRdrTyVarsDups ty
  = extract_lty ty []

-- | Extracts the free type/kind variables from the kind signature of a HsType.
--   This is used to implicitly quantify over @k@ in @type T = Nothing :: Maybe k@.
-- When the same name occurs multiple times in the type, only the first
-- occurrence is returned, and the left-to-right order of variables is
-- preserved.
-- See Note [Kind and type-variable binders] and
--     Note [Ordering of implicit variables] and
--     Note [Implicit quantification in type synonyms].
extractHsTyRdrTyVarsKindVars :: LHsType GhcPs -> FreeKiTyVarsNoDups
extractHsTyRdrTyVarsKindVars (unLoc -> ty) =
  case ty of
    HsParTy _ ty -> extractHsTyRdrTyVarsKindVars ty
    HsKindSig _ _ ki -> extractHsTyRdrTyVars ki
    _ -> []

-- | Extracts free type and kind variables from types in a list.
-- When the same name occurs multiple times in the types, all occurrences
-- are returned.
extractHsTysRdrTyVarsDups :: [LHsType GhcPs] -> FreeKiTyVarsWithDups
extractHsTysRdrTyVarsDups tys
  = extract_ltys tys []

-- Returns the free kind variables of any explicitly-kinded binders, returning
-- variable occurrences in left-to-right order.
-- See Note [Ordering of implicit variables].
-- NB: Does /not/ delete the binders themselves.
--     However duplicates are removed
--     E.g. given  [k1, a:k1, b:k2]
--          the function returns [k1,k2], even though k1 is bound here
extractHsTyVarBndrsKVs :: [LHsTyVarBndr GhcPs] -> FreeKiTyVarsNoDups
extractHsTyVarBndrsKVs tv_bndrs
  = nubL (extract_hs_tv_bndrs_kvs tv_bndrs)

-- Returns the free kind variables in a type family result signature, returning
-- variable occurrences in left-to-right order.
-- See Note [Ordering of implicit variables].
extractRdrKindSigVars :: LFamilyResultSig GhcPs -> [Located RdrName]
extractRdrKindSigVars (L _ resultSig)
  | KindSig _ k                          <- resultSig = extractHsTyRdrTyVars k
  | TyVarSig _ (L _ (KindedTyVar _ _ k)) <- resultSig = extractHsTyRdrTyVars k
  | otherwise =  []

-- Get type/kind variables mentioned in the kind signature, preserving
-- left-to-right order and without duplicates:
--
--  * data T a (b :: k1) :: k2 -> k1 -> k2 -> Type   -- result: [k2,k1]
--  * data T a (b :: k1)                             -- result: []
--
-- See Note [Ordering of implicit variables].
extractDataDefnKindVars :: HsDataDefn GhcPs ->  FreeKiTyVarsNoDups
extractDataDefnKindVars (HsDataDefn { dd_kindSig = ksig })
  = maybe [] extractHsTyRdrTyVars ksig
extractDataDefnKindVars (XHsDataDefn nec) = noExtCon nec

extract_lctxt :: LHsContext GhcPs
              -> FreeKiTyVarsWithDups -> FreeKiTyVarsWithDups
extract_lctxt ctxt = extract_ltys (unLoc ctxt)

extract_ltys :: [LHsType GhcPs]
             -> FreeKiTyVarsWithDups -> FreeKiTyVarsWithDups
extract_ltys tys acc = foldr extract_lty acc tys

extract_lty :: LHsType GhcPs
            -> FreeKiTyVarsWithDups -> FreeKiTyVarsWithDups
extract_lty (L _ ty) acc
  = case ty of
      HsTyVar _ _  ltv            -> extract_tv ltv acc
      HsBangTy _ _ ty             -> extract_lty ty acc
      HsRecTy _ flds              -> foldr (extract_lty
                                            . cd_fld_type . unLoc) acc
                                           flds
      HsAppTy _ ty1 ty2           -> extract_lty ty1 $
                                     extract_lty ty2 acc
      HsAppKindTy _ ty k          -> extract_lty ty $
                                     extract_lty k acc
      HsListTy _ ty               -> extract_lty ty acc
      HsTupleTy _ _ tys           -> extract_ltys tys acc
      HsSumTy _ tys               -> extract_ltys tys acc
      HsFunTy _ ty1 ty2           -> extract_lty ty1 $
                                     extract_lty ty2 acc
      HsIParamTy _ _ ty           -> extract_lty ty acc
      HsOpTy _ ty1 tv ty2         -> extract_tv tv $
                                     extract_lty ty1 $
                                     extract_lty ty2 acc
      HsParTy _ ty                -> extract_lty ty acc
      HsSpliceTy {}               -> acc  -- Type splices mention no tvs
      HsDocTy _ ty _              -> extract_lty ty acc
      HsExplicitListTy _ _ tys    -> extract_ltys tys acc
      HsExplicitTupleTy _ tys     -> extract_ltys tys acc
      HsTyLit _ _                 -> acc
      HsStarTy _ _                -> acc
      HsKindSig _ ty ki           -> extract_lty ty $
                                     extract_lty ki acc
      HsForAllTy { hst_bndrs = tvs, hst_body = ty }
                                  -> extract_hs_tv_bndrs tvs acc $
                                     extract_lty ty []
      HsQualTy { hst_ctxt = ctxt, hst_body = ty }
                                  -> extract_lctxt ctxt $
                                     extract_lty ty acc
      XHsType {}                  -> acc
      -- We deal with these separately in rnLHsTypeWithWildCards
      HsWildCardTy {}             -> acc

extractHsTvBndrs :: [LHsTyVarBndr GhcPs]
                 -> FreeKiTyVarsWithDups           -- Free in body
                 -> FreeKiTyVarsWithDups       -- Free in result
extractHsTvBndrs tv_bndrs body_fvs
  = extract_hs_tv_bndrs tv_bndrs [] body_fvs

extract_hs_tv_bndrs :: [LHsTyVarBndr GhcPs]
                    -> FreeKiTyVarsWithDups  -- Accumulator
                    -> FreeKiTyVarsWithDups  -- Free in body
                    -> FreeKiTyVarsWithDups
-- In (forall (a :: Maybe e). a -> b) we have
--     'a' is bound by the forall
--     'b' is a free type variable
--     'e' is a free kind variable
extract_hs_tv_bndrs tv_bndrs acc_vars body_vars
  | null tv_bndrs = body_vars ++ acc_vars
  | otherwise = filterOut (`elemRdr` tv_bndr_rdrs) (bndr_vars ++ body_vars) ++ acc_vars
    -- NB: delete all tv_bndr_rdrs from bndr_vars as well as body_vars.
    -- See Note [Kind variable scoping]
  where
    bndr_vars = extract_hs_tv_bndrs_kvs tv_bndrs
    tv_bndr_rdrs = map hsLTyVarLocName tv_bndrs

extract_hs_tv_bndrs_kvs :: [LHsTyVarBndr GhcPs] -> [Located RdrName]
-- Returns the free kind variables of any explicitly-kinded binders, returning
-- variable occurrences in left-to-right order.
-- See Note [Ordering of implicit variables].
-- NB: Does /not/ delete the binders themselves.
--     Duplicates are /not/ removed
--     E.g. given  [k1, a:k1, b:k2]
--          the function returns [k1,k2], even though k1 is bound here
extract_hs_tv_bndrs_kvs tv_bndrs =
    foldr extract_lty []
          [k | L _ (KindedTyVar _ _ k) <- tv_bndrs]

extract_tv :: Located RdrName
           -> [Located RdrName] -> [Located RdrName]
extract_tv tv acc =
  if isRdrTyVar (unLoc tv) then tv:acc else acc

-- Deletes duplicates in a list of Located things.
--
-- Importantly, this function is stable with respect to the original ordering
-- of things in the list. This is important, as it is a property that GHC
-- relies on to maintain the left-to-right ordering of implicitly quantified
-- type variables.
-- See Note [Ordering of implicit variables].
nubL :: Eq a => [Located a] -> [Located a]
nubL = nubBy eqLocated

elemRdr :: Located RdrName -> [Located RdrName] -> Bool
elemRdr x = any (eqLocated x)
