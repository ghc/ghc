
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

module GHC.Rename.HsType (
        -- Type related stuff
        rnHsType, rnLHsType, rnLHsTypes, rnContext, rnMaybeContext,
        rnLHsKind, rnLHsTypeArgs,
        rnHsSigType, rnHsWcType, rnHsTyLit, rnHsMultAnnWith,
        HsPatSigTypeScoping(..), rnHsSigWcType, rnHsPatSigType, rnHsPatSigKind,
        newTyVarNameRn,
        rnHsConDeclRecFields,
        lookupField, mkHsOpTyRn,
        rnLTyVar,

        rnHsConDeclField,

        -- Precence related stuff
        NegationHandling(..),
        mkOpAppRn, mkNegAppRn, mkConOpPatRn,
        checkPrecMatch, checkSectionPrec,

        -- Binding related stuff
        bindHsOuterTyVarBndrs, bindHsForAllTelescope,
        bindLHsTyVarBndr, bindLHsTyVarBndrs, WarnUnusedForalls(..),
        rnImplicitTvOccs, bindSigTyVarsFV, bindHsQTyVars,
        FreeKiTyVars, filterInScopeM,
        extractHsTyRdrTyVars, extractHsTyRdrTyVarsKindVars,
        extractHsTysRdrTyVars, extractRdrKindSigVars,
        extractConDeclGADTDetailsTyVars, extractDataDefnKindVars,
        extractHsOuterTvBndrs, extractHsTyArgRdrKiTyVars,
        nubL, nubN,

        -- Error helpers
        badKindSigErr
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Rename.Splice( rnSpliceType, checkThLocalTyName )

import GHC.Core.TyCo.FVs ( tyCoVarsOfTypeList )
import GHC.Core.TyCon    ( isKindName )
import GHC.Hs
import GHC.Rename.Env
import GHC.Rename.Doc
import GHC.Rename.Utils  ( mapFvRn, bindLocalNamesFV
                         , typeAppErr, newLocalBndrRn, checkDupRdrNames
                         , checkShadowedRdrNames )
import GHC.Rename.Fixity ( lookupFieldFixityRn, lookupFixityRn
                         , lookupTyFixityRn )
import GHC.Rename.Unbound ( notInScopeErr, WhereLooking(WL_LocalOnly) )
import GHC.Tc.Errors.Types
import GHC.Tc.Errors.Ppr ( pprHsDocContext )
import GHC.Tc.Utils.Monad
import GHC.Unit.Module ( getModule )
import GHC.Types.Name.Reader
import GHC.Builtin.Names
import GHC.Types.Hint ( UntickedPromotedThing(..) )
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Name.Set
import GHC.Types.FieldLabel
import GHC.Types.Error

import GHC.Utils.Misc
import GHC.Types.Fixity ( compareFixity, negateFixity
                        , Fixity(..), FixityDirection(..), LexicalFixity(..) )
import GHC.Types.Basic  ( TypeOrKind(..) )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Data.Maybe
import qualified GHC.LanguageExtensions as LangExt

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import Data.List (nubBy, partition)
import Control.Monad

{-
These type renamers are in a separate module, rather than in (say) GHC.Rename.Module,
to break several loops.

*********************************************************
*                                                       *
    HsSigWcType and HsPatSigType (i.e with wildcards)
*                                                       *
*********************************************************
-}

data HsPatSigTypeScoping
  = AlwaysBind
    -- ^ Always bind any free tyvars of the given type, regardless of whether we
    -- have a forall at the top.
    --
    -- For pattern type sigs, we /do/ want to bring those type
    -- variables into scope, even if there's a forall at the top which usually
    -- stops that happening, e.g:
    --
    -- > \ (x :: forall a. a -> b) -> e
    --
    -- Here we do bring 'b' into scope.
    --
    -- RULES can also use 'AlwaysBind', such as in the following example:
    --
    -- > {-# RULES \"f\" forall (x :: forall a. a -> b). f x = ... b ... #-}
    --
    -- This only applies to RULES that do not explicitly bind their type
    -- variables. If a RULE explicitly quantifies its type variables, then
    -- 'NeverBind' is used instead. See also
    -- @Note [Pattern signature binders and scoping]@ in "GHC.Hs.Type".
  | NeverBind
    -- ^ Never bind any free tyvars. This is used for RULES that have both
    -- explicit type and term variable binders, e.g.:
    --
    -- > {-# RULES \"const\" forall a. forall (x :: a) y. const x y = x #-}
    --
    -- The presence of the type variable binder @forall a.@ implies that the
    -- free variables in the types of the term variable binders @x@ and @y@
    -- are /not/ bound. In the example above, there are no such free variables,
    -- but if the user had written @(y :: b)@ instead of @y@ in the term
    -- variable binders, then @b@ would be rejected for being out of scope.
    -- See also @Note [Pattern signature binders and scoping]@ in
    -- "GHC.Hs.Type".

rnHsSigWcType :: HsDocContext
              -> LHsSigWcType GhcPs
              -> RnM (LHsSigWcType GhcRn, FreeVars)
rnHsSigWcType doc (HsWC { hswc_body =
    sig_ty@(L loc (HsSig{sig_bndrs = outer_bndrs, sig_body = body_ty })) })
  = do { free_vars <- filterInScopeM (extract_lhs_sig_ty sig_ty)
       ; (nwc_rdrs', imp_tv_nms) <- partition_nwcs free_vars
       ; let nwc_rdrs = nubL nwc_rdrs'
       ; bindHsOuterTyVarBndrs doc Nothing imp_tv_nms outer_bndrs $ \outer_bndrs' ->
    do { (wcs, body_ty', fvs) <- rnWcBodyType doc nwc_rdrs body_ty
       ; pure ( HsWC  { hswc_ext = wcs, hswc_body = L loc $
                HsSig { sig_ext = noExtField
                      , sig_bndrs = outer_bndrs', sig_body = body_ty' }}
              , fvs) } }

rnHsPatSigType :: HsPatSigTypeScoping
               -> HsDocContext
               -> HsPatSigType GhcPs
               -> (HsPatSigType GhcRn -> RnM (a, FreeVars))
               -> RnM (a, FreeVars)
rnHsPatSigType = rnHsPatSigTyKi TypeLevel

rnHsPatSigKind :: HsPatSigTypeScoping
               -> HsDocContext
               -> HsPatSigType GhcPs
               -> (HsPatSigType GhcRn -> RnM (a, FreeVars))
               -> RnM (a, FreeVars)
rnHsPatSigKind = rnHsPatSigTyKi KindLevel

rnHsPatSigTyKi :: TypeOrKind
               -> HsPatSigTypeScoping
               -> HsDocContext
               -> HsPatSigType GhcPs
               -> (HsPatSigType GhcRn -> RnM (a, FreeVars))
               -> RnM (a, FreeVars)
-- Used for
--   - Pattern type signatures, which are only allowed with ScopedTypeVariables
--   - Signatures on binders in a RULE, which are allowed even if
--     ScopedTypeVariables isn't enabled
-- Wildcards are allowed
--
-- See Note [Pattern signature binders and scoping] in GHC.Hs.Type
rnHsPatSigTyKi level scoping ctx sig_ty thing_inside
  = do { ty_sig_okay <- xoptM LangExt.ScopedTypeVariables
       ; checkErr ty_sig_okay (unexpectedPatSigTypeErr sig_ty)
       ; free_vars <- filterInScopeM (extractHsTyRdrTyVars pat_sig_ty)
       ; (nwc_rdrs', tv_rdrs) <- partition_nwcs free_vars
       ; let nwc_rdrs = nubN nwc_rdrs'
             implicit_bndrs = case scoping of
               AlwaysBind -> tv_rdrs
               NeverBind  -> []
       ; rnImplicitTvOccs Nothing implicit_bndrs $ \ imp_tvs ->
    do { (nwcs, pat_sig_ty', fvs1) <- rnWcBodyTyKi level ctx nwc_rdrs pat_sig_ty
       ; let sig_names = HsPSRn { hsps_nwcs = nwcs, hsps_imp_tvs = imp_tvs }
             sig_ty'   = HsPS { hsps_ext = sig_names, hsps_body = pat_sig_ty' }
       ; (res, fvs2) <- thing_inside sig_ty'
       ; return (res, fvs1 `plusFV` fvs2) } }
  where
    pat_sig_ty = hsPatSigType sig_ty

rnHsWcType :: HsDocContext -> LHsWcType GhcPs -> RnM (LHsWcType GhcRn, FreeVars)
rnHsWcType ctxt (HsWC { hswc_body = hs_ty })
  = do { free_vars <- filterInScopeM (extractHsTyRdrTyVars hs_ty)
       ; (nwc_rdrs', _) <- partition_nwcs free_vars
       ; let nwc_rdrs = nubL nwc_rdrs'
       ; (wcs, hs_ty', fvs) <- rnWcBodyType ctxt nwc_rdrs hs_ty
       ; let sig_ty' = HsWC { hswc_ext = wcs, hswc_body = hs_ty' }
       ; return (sig_ty', fvs) }


rnWcBodyType :: HsDocContext -> [LocatedN RdrName] -> LHsType GhcPs
  -> RnM ([Name], LHsType GhcRn, FreeVars)
rnWcBodyType = rnWcBodyTyKi TypeLevel

rnWcBodyTyKi :: TypeOrKind -> HsDocContext -> [LocatedN RdrName] -> LHsType GhcPs
         -> RnM ([Name], LHsType GhcRn, FreeVars)
rnWcBodyTyKi level ctxt nwc_rdrs hs_ty
  = do { nwcs <- mapM newLocalBndrRn nwc_rdrs
       ; let env = RTKE { rtke_level = level
                        , rtke_what  = RnTypeBody
                        , rtke_nwcs  = mkNameSet nwcs
                        , rtke_ctxt  = ctxt }
       ; (hs_ty', fvs) <- bindLocalNamesFV nwcs $
                          rn_lty env hs_ty
       ; return (nwcs, hs_ty', fvs) }
  where
    rn_lty env (L loc hs_ty)
      = setSrcSpanA loc $
        do { (hs_ty', fvs) <- rn_ty env hs_ty
           ; return (L loc hs_ty', fvs) }

    rn_ty :: RnTyKiEnv -> HsType GhcPs -> RnM (HsType GhcRn, FreeVars)
    -- A lot of faff just to allow the extra-constraints wildcard to appear
    rn_ty env (HsForAllTy { hst_tele = tele, hst_body = hs_body })
      = bindHsForAllTelescope (rtke_ctxt env) tele $ \ tele' ->
        do { (hs_body', fvs) <- rn_lty env hs_body
           ; return (HsForAllTy { hst_xforall = noExtField
                                , hst_tele = tele', hst_body = hs_body' }
                    , fvs) }

    rn_ty env (HsQualTy { hst_ctxt = L cx hs_ctxt
                        , hst_body = hs_ty })
      | Just (hs_ctxt1, hs_ctxt_last) <- snocView hs_ctxt
      , L lx (HsWildCardTy _)  <- ignoreParens hs_ctxt_last
      = do { (hs_ctxt1', fvs1) <- mapFvRn (rn_top_constraint env) hs_ctxt1
           ; setSrcSpanA lx $ checkExtraConstraintWildCard env hs_ctxt1
           ; let hs_ctxt' = hs_ctxt1' ++ [L lx (HsWildCardTy noExtField)]
           ; (hs_ty', fvs2) <- rnLHsTyKi env hs_ty
           ; return (HsQualTy { hst_xqual = noExtField
                              , hst_ctxt = L cx hs_ctxt'
                              , hst_body = hs_ty' }
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
  = checkWildCard env Nothing mb_bad
  where
    mb_bad | not (extraConstraintWildCardsAllowed env)
           = Just $ ExtraConstraintWildcardNotAllowed
                      SoleExtraConstraintWildcardNotAllowed
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
           = Just $ ExtraConstraintWildcardNotAllowed
                      SoleExtraConstraintWildcardAllowed
           | otherwise
           = Nothing

extraConstraintWildCardsAllowed :: RnTyKiEnv -> Bool
extraConstraintWildCardsAllowed env
  = case rtke_ctxt env of
      TypeSigCtx {}       -> True
      ExprWithTySigCtx {} -> True
      DerivDeclCtx {}     -> True
      StandaloneKindSigCtx {} -> False  -- See Note [Wildcards in standalone kind signatures] in "GHC.Hs.Decls"
      _                   -> False

-- | When the NamedWildCards extension is enabled, partition_nwcs
-- removes type variables that start with an underscore from the
-- FreeKiTyVars in the argument and returns them in a separate list.
-- When the extension is disabled, the function returns the argument
-- and empty list.  See Note [Renaming named wild cards]
partition_nwcs :: FreeKiTyVars -> RnM ([LocatedN RdrName], FreeKiTyVars)
partition_nwcs free_vars
  = do { wildcards_enabled <- xoptM LangExt.NamedWildCards
       ; return $
           if wildcards_enabled
           then partition is_wildcard free_vars
           else ([], free_vars) }
  where
     is_wildcard :: LocatedN RdrName -> Bool
     is_wildcard rdr = startsWithUnderscore (rdrNameOcc (unLoc rdr))

{- Note [Renaming named wild cards]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Identifiers starting with an underscore are always parsed as type variables.
It is only here in the renamer that we give the special treatment.
See Note [The wildcard story for types] in GHC.Hs.Type.

It's easy!  When we collect the implicitly bound type variables, ready
to bring them into scope, and NamedWildCards is on, we partition the
variables into the ones that start with an underscore (the named
wildcards) and the rest. Then we just add them to the hswc_wcs field
of the HsWildCardBndrs structure, and we are done.


*********************************************************
*                                                       *
           HsSigType (i.e. no wildcards)
*                                                       *
****************************************************** -}

rnHsSigType :: HsDocContext
            -> TypeOrKind
            -> LHsSigType GhcPs
            -> RnM (LHsSigType GhcRn, FreeVars)
-- Used for source-language type signatures
-- that cannot have wildcards
rnHsSigType ctx level
    (L loc sig_ty@(HsSig { sig_bndrs = outer_bndrs, sig_body = body }))
  = setSrcSpanA loc $
    do { traceRn "rnHsSigType" (ppr sig_ty)
       ; case outer_bndrs of
           HsOuterExplicit{} -> checkPolyKinds env (HsSigType sig_ty)
           HsOuterImplicit{} -> pure ()
       ; imp_vars <- filterInScopeM $ extractHsTyRdrTyVars body
       ; bindHsOuterTyVarBndrs ctx Nothing imp_vars outer_bndrs $ \outer_bndrs' ->
    do { (body', fvs) <- rnLHsTyKi env body

       ; return ( L loc $ HsSig { sig_ext = noExtField
                                , sig_bndrs = outer_bndrs', sig_body = body' }
                , fvs ) } }
  where
    env = mkTyKiEnv ctx level RnTypeBody

-- | Create new renamed type variables corresponding to source-level ones.
-- Duplicates are permitted, but will be removed. This is intended especially for
-- the case of handling the implicitly bound free variables of a type signature.
rnImplicitTvOccs :: Maybe assoc
                 -- ^ @'Just' _@ => an associated type decl
                 -> FreeKiTyVars
                 -- ^ Surface-syntax free vars that we will implicitly bind.
                 -- May have duplicates, which are removed here.
                 -> ([Name] -> RnM (a, FreeVars))
                 -> RnM (a, FreeVars)
rnImplicitTvOccs mb_assoc implicit_vs_with_dups thing_inside
  = do { let implicit_vs = nubN implicit_vs_with_dups

       ; traceRn "rnImplicitTvOccs" $
         vcat [ ppr implicit_vs_with_dups, ppr implicit_vs ]

         -- Use the currently set SrcSpan as the new source location for each Name.
         -- See Note [Source locations for implicitly bound type variables].
       ; loc <- getSrcSpanM
       ; let loc' = noAnnSrcSpan loc
       ; vars <- mapM (newTyVarNameRnImplicit mb_assoc . L loc' . unLoc) implicit_vs

       ; bindLocalNamesFV vars $
         thing_inside vars }

{-
Note [Source locations for implicitly bound type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When bringing implicitly bound type variables into scope (in rnImplicitTvOccs),
we do something peculiar: we drop the original SrcSpan attached to each
variable and replace it with the currently set SrcSpan. Moreover, this new
SrcSpan is usually /less/ precise than the original one, and that's OK. To see
why this is done, consider the following example:

  f :: a -> b -> a

Suppose that a warning or error message needs to point to the SrcSpans of the
binding sites for `a` and `b`. But where /are/ they bound, anyway? Technically,
they're bound by an unwritten `forall` at the front of the type signature, but
there is no SrcSpan for that. We could point to the first occurrence of `a` as
the binding site for `a`, but that would make the first occurrence of `a`
special. Moreover, we don't want IDEs to confuse binding sites and occurrences.

As a result, we make the `SrcSpan`s for `a` and `b` span the entirety of the
type signature, since the type signature implicitly carries their binding
sites. This is less precise, but more accurate.
-}

{- ******************************************************
*                                                       *
           LHsType and HsType
*                                                       *
****************************************************** -}

{-
rnHsType is here because we call it from loadInstDecl, and I didn't
want a gratuitous knot.
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

rnHsConDeclField :: HsDocContext -> HsConDeclField GhcPs
                 -> RnM (HsConDeclField GhcRn, FreeVars)
rnHsConDeclField doc = rnHsConDeclFieldTyKi (mkTyKiEnv doc TypeLevel RnTypeBody)

rnHsConDeclFieldTyKi :: RnTyKiEnv -> HsConDeclField GhcPs
                     -> RnM (HsConDeclField GhcRn, FreeVars)
rnHsConDeclFieldTyKi env cdf@(CDF { cdf_multiplicity, cdf_type, cdf_doc }) = do
  (w , fvs_w) <- rnHsMultAnnWith (rnLHsTyKi env) cdf_multiplicity
  (ty, fvs) <- rnLHsTyKi env cdf_type
  doc <- traverse rnLHsDoc cdf_doc
  return (cdf { cdf_multiplicity = w, cdf_type = ty, cdf_doc = doc }, fvs `plusFV` fvs_w)


rnHsType  :: HsDocContext -> HsType GhcPs -> RnM (HsType GhcRn, FreeVars)
rnHsType ctxt ty = rnHsTyKi (mkTyKiEnv ctxt TypeLevel RnTypeBody) ty

rnLHsKind  :: HsDocContext -> LHsKind GhcPs -> RnM (LHsKind GhcRn, FreeVars)
rnLHsKind ctxt kind = rnLHsTyKi (mkTyKiEnv ctxt KindLevel RnTypeBody) kind

-- renaming a type only, not a kind
rnLHsTypeArg :: HsDocContext -> LHsTypeArg GhcPs
                -> RnM (LHsTypeArg GhcRn, FreeVars)
rnLHsTypeArg ctxt (HsValArg _ ty)
   = do { (tys_rn, fvs) <- rnLHsType ctxt ty
        ; return (HsValArg noExtField tys_rn, fvs) }
rnLHsTypeArg ctxt (HsTypeArg _ ki)
   = do { (kis_rn, fvs) <- rnLHsKind ctxt ki
        ; return (HsTypeArg noExtField kis_rn, fvs) }
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

rnMaybeContext :: HsDocContext -> Maybe (LHsContext GhcPs)
          -> RnM (Maybe (LHsContext GhcRn), FreeVars)
rnMaybeContext _ Nothing = return (Nothing, emptyFVs)
rnMaybeContext doc (Just theta)
  = do { (theta', fvs) <- rnContext doc theta
       ; return (Just theta', fvs)
       }


--------------
rnLHsTyKi  :: RnTyKiEnv -> LHsType GhcPs -> RnM (LHsType GhcRn, FreeVars)
rnLHsTyKi env (L loc ty)
  = setSrcSpanA loc $
    do { (ty', fvs) <- rnHsTyKi env ty
       ; return (L loc ty', fvs) }

rnHsTyKi :: RnTyKiEnv -> HsType GhcPs -> RnM (HsType GhcRn, FreeVars)

rnHsTyKi env ty@(HsForAllTy { hst_tele = tele, hst_body = tau })
  = do { checkPolyKinds env (HsType ty)
       ; bindHsForAllTelescope (rtke_ctxt env) tele $ \ tele' ->
    do { (tau',  fvs) <- rnLHsTyKi env tau
       ; return ( HsForAllTy { hst_xforall = noExtField
                             , hst_tele = tele' , hst_body =  tau' }
                , fvs) } }

rnHsTyKi env (HsQualTy { hst_ctxt = lctxt, hst_body = tau })
  = do { -- no need to check type vs kind level here; this is
         -- checked in the type checker. See
         -- Note [No constraints in kinds] in GHC.Tc.Validity
         (ctxt', fvs1) <- rnTyKiContext env lctxt
       ; (tau',  fvs2) <- rnLHsTyKi env tau
       ; return (HsQualTy { hst_xqual = noExtField, hst_ctxt = ctxt'
                          , hst_body =  tau' }
                , fvs1 `plusFV` fvs2) }

rnHsTyKi env tv@(HsTyVar _ ip (L loc rdr_name))
  = do { when (isRnKindLevel env && isRdrTyVar rdr_name) $
         unlessXOptM LangExt.PolyKinds $ addErr $
         TcRnWithHsDocContext (rtke_ctxt env) $
         TcRnUnexpectedKindVar rdr_name
           -- Any type variable at the kind level is illegal without the use
           -- of PolyKinds (see #14710)
       ; name <- rnTyVar env rdr_name
       ; this_mod <- getModule
       ; when (nameIsLocalOrFrom this_mod name) $
         checkThLocalTyName name
       ; when (isDataConName name && not (isKindName name)) $
           -- Any use of a promoted data constructor name (that is not
           -- specifically exempted by isKindName) is illegal without the use
           -- of DataKinds. See Note [Checking for DataKinds] in
           -- GHC.Tc.Validity.
           checkDataKinds env tv
       ; when (isDataConName name && not (isPromoted ip)) $
         -- NB: a prefix symbolic operator such as (:) is represented as HsTyVar.
            addDiagnostic (TcRnUntickedPromotedThing $ UntickedConstructor Prefix name)
       ; return (HsTyVar noAnn ip (L loc $ WithUserRdr rdr_name name), unitFV name) }

rnHsTyKi env ty@(HsOpTy _ prom ty1 l_op ty2)
  = setSrcSpan (getLocA l_op) $
    do  { let op_rdr = unLoc l_op
        ; (l_op', fvs1) <- rnHsTyOp env (ppr ty) l_op
        ; let op_name = unLoc l_op'
        ; fix   <- lookupTyFixityRn l_op'
        ; (ty1', fvs2) <- rnLHsTyKi env ty1
        ; (ty2', fvs3) <- rnLHsTyKi env ty2
        ; res_ty <- mkHsOpTyRn prom (fmap (WithUserRdr op_rdr) l_op') fix ty1' ty2'
        ; when (isDataConName op_name && not (isPromoted prom)) $
            addDiagnostic (TcRnUntickedPromotedThing $ UntickedConstructor Infix op_name)
        ; return (res_ty, plusFVs [fvs1, fvs2, fvs3]) }

rnHsTyKi env (HsParTy _ ty)
  = do { (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsParTy noAnn ty', fvs) }

rnHsTyKi env (HsFunTy u mult ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi env ty1
       ; (ty2', fvs2) <- rnLHsTyKi env ty2
       ; (mult', w_fvs) <- rnHsMultAnnWith (rnLHsTyKi env) mult
       ; return (HsFunTy u mult' ty1' ty2'
                , plusFVs [fvs1, fvs2, w_fvs]) }

rnHsTyKi env listTy@(HsListTy x ty)
  = do { when (isRnKindLevel env) $
           checkDataKinds env listTy
       ; (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsListTy x ty', fvs) }

rnHsTyKi env (HsKindSig x ty k)
  = do { kind_sigs_ok <- xoptM LangExt.KindSignatures
       ; unless kind_sigs_ok (badKindSigErr (rtke_ctxt env) k)
       ; (k', sig_fvs)  <- rnLHsTyKi (env { rtke_level = KindLevel }) k
       ; (ty', lhs_fvs) <- bindSigTyVarsFV (hsScopedKvs k') $
                           rnLHsTyKi env ty
       ; return (HsKindSig x ty' k', lhs_fvs `plusFV` sig_fvs) }

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsTyKi env tupleTy@(HsTupleTy x tup_con tys)
  = do { when (isRnKindLevel env) $
           checkDataKinds env tupleTy
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsTupleTy x tup_con tys', fvs) }

rnHsTyKi env sumTy@(HsSumTy x tys)
  = do { when (isRnKindLevel env) $
           checkDataKinds env sumTy
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsSumTy x tys', fvs) }

-- Ensure that a type-level integer is nonnegative (#8306, #8412)
rnHsTyKi env tyLit@(HsTyLit src t)
  = do { checkDataKinds env tyLit
       ; t' <- rnHsTyLit t
       ; return (HsTyLit src t', emptyFVs) }

rnHsTyKi env (HsAppTy _ ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi env ty1
       ; (ty2', fvs2) <- rnLHsTyKi env ty2
       ; return (HsAppTy noExtField ty1' ty2', fvs1 `plusFV` fvs2) }

rnHsTyKi env (HsAppKindTy _ ty k)
  = do { kind_app <- xoptM LangExt.TypeApplications
       ; unless kind_app (addErr (typeAppErr KindLevel k))
       ; (ty', fvs1) <- rnLHsTyKi env ty
       ; (k', fvs2) <- rnLHsTyKi (env {rtke_level = KindLevel }) k
       ; return (HsAppKindTy noExtField ty' k', fvs1 `plusFV` fvs2) }

rnHsTyKi env t@(HsIParamTy x n ty)
  = do { notInKinds env t
       ; (ty', fvs) <- rnLHsTyKi env ty
       ; return (HsIParamTy x n ty', fvs) }

rnHsTyKi _ (HsStarTy _ isUni)
  = return (HsStarTy noExtField isUni, emptyFVs)

rnHsTyKi _ (HsSpliceTy _ sp)
  = rnSpliceType sp

rnHsTyKi env (HsDocTy x ty haddock_doc)
  = do { (ty', fvs) <- rnLHsTyKi env ty
       ; haddock_doc' <- rnLHsDoc haddock_doc
       ; return (HsDocTy x ty' haddock_doc', fvs) }

-- See Note [Renaming HsCoreTys]
rnHsTyKi env (XHsType (HsCoreTy ty))
  = do mapM_ (check_in_scope . nameRdrName) fvs_list
       return (XHsType ty, fvs)
  where
    fvs_list = map getName $ tyCoVarsOfTypeList ty
    fvs = mkFVs fvs_list

    check_in_scope :: RdrName -> RnM ()
    check_in_scope rdr_name = do
      mb_name <- lookupLocalOccRn_maybe rdr_name
      when (isNothing mb_name) $
        addErr $
          TcRnWithHsDocContext (rtke_ctxt env) $
            TcRnNotInScope (notInScopeErr WL_LocalOnly rdr_name) rdr_name

rnHsTyKi env ty@(XHsType (HsBangTy _ bang (L _ inner))) = do
  -- While top-level bangs at this point are eliminated (eg !(Maybe Int)),
  -- other kinds of bangs are not (eg ((!Maybe) Int)). These kinds of
  -- bangs are invalid, so fail. (#7210, #14761)
  addErr $
    TcRnWithHsDocContext (rtke_ctxt env) $
      TcRnUnexpectedAnnotation ty bang
  rnHsTyKi env inner

rnHsTyKi env ty@(XHsType (HsRecTy {})) = do
  -- Record types (which only show up temporarily in constructor
  -- signatures) should have been removed by now
  addErr $
    TcRnWithHsDocContext (rtke_ctxt env) $
      TcRnIllegalRecordSyntax ty
  return (HsWildCardTy noExtField, emptyFVs) -- trick to avoid `failWithTc`

rnHsTyKi env ty@(HsExplicitListTy _ ip tys)
  = do { checkDataKinds env ty
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; unless (isPromoted ip) $
           addDiagnostic (TcRnUntickedPromotedThing $ UntickedExplicitList)
       ; return (HsExplicitListTy noExtField ip tys', fvs) }

rnHsTyKi env ty@(HsExplicitTupleTy _ ip tys)
  = do { checkDataKinds env ty
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi env) tys
       ; return (HsExplicitTupleTy noExtField ip tys', fvs) }

rnHsTyKi env (HsWildCardTy _)
  = do { checkAnonWildCard env
       ; return (HsWildCardTy noExtField, emptyFVs) }


rnHsTyLit :: HsTyLit GhcPs -> RnM (HsTyLit GhcRn)
rnHsTyLit (HsStrTy x s) = pure (HsStrTy x s)
rnHsTyLit tyLit@(HsNumTy x i) = do
  when (i < 0) $
    addErr $ TcRnNegativeNumTypeLiteral tyLit
  pure (HsNumTy x i)
rnHsTyLit (HsCharTy x c) = pure (HsCharTy x c)


rnHsMultAnnWith :: (LocatedA (mult GhcPs) -> RnM (LocatedA (mult GhcRn), FreeVars))
                  -> HsMultAnnOf (LocatedA (mult GhcPs)) GhcPs
                  -> RnM (HsMultAnnOf (LocatedA (mult GhcRn)) GhcRn, FreeVars)
rnHsMultAnnWith _rn (HsUnannotated _) = pure (HsUnannotated noExtField, emptyFVs)
rnHsMultAnnWith _rn (HsLinearAnn _) = pure (HsLinearAnn noExtField, emptyFVs)
rnHsMultAnnWith rn (HsExplicitMult _ p)
  =  (\(mult, fvs) -> (HsExplicitMult noExtField mult, fvs)) <$> rn p

{-
Note [Renaming HsCoreTys]
~~~~~~~~~~~~~~~~~~~~~~~~~
HsCoreTy is an escape hatch that allows embedding Core Types in HsTypes.
As such, there's not much to be done in order to rename an HsCoreTy,
since it's already been renamed to some extent. However, in an attempt to
detect ill-formed HsCoreTys, the renamer checks to see if all free type
variables in an HsCoreTy are in scope. To see why this can matter, consider
this example from #18914:

  type T f = forall a. f a

  class C f where
    m :: T f

  newtype N f a = MkN (f a)
    deriving C

Because of #18914, a previous GHC would generate the following code:

  instance C f => C (N f) where
    m :: T (N f)
    m = coerce @(f a)   -- The type within @(...) is an HsCoreTy
               @(N f a) -- So is this
               (m @f)

There are two HsCoreTys in play—(f a) and (N f a)—both of which have
`f` and `a` as free type variables. The `f` is in scope from the instance head,
but `a` is completely unbound, which is what led to #18914. To avoid this sort
of mistake going forward, the renamer will now detect that `a` is unbound and
throw an error accordingly.
-}

--------------
rnTyVar :: RnTyKiEnv -> RdrName -> RnM Name
rnTyVar env rdr_name
  = do { name <- lookupTypeOccRn rdr_name
       ; checkNamedWildCard env name
       ; return name }

rnLTyVar :: LocatedN RdrName -> RnM (LocatedN Name)
-- Called externally; does not deal with wildcards
rnLTyVar (L loc rdr_name)
  = do { tyvar <- lookupTypeOccRn rdr_name
       ; return (L loc tyvar) }

--------------
rnHsTyOp :: RnTyKiEnv -> SDoc -> LocatedN RdrName
         -> RnM (LocatedN Name, FreeVars)
rnHsTyOp env overall_ty (L loc op)
  = do { op' <- rnTyVar env op
       ; unlessXOptM LangExt.TypeOperators $
           if (op' `hasKey` eqTyConKey) -- See [eqTyCon (~) compatibility fallback] in GHC.Rename.Env
           then addDiagnostic TcRnTypeEqualityRequiresOperators
           else addErr $ TcRnIllegalTypeOperator overall_ty op
       ; return (L loc op', unitFV op') }

--------------
checkWildCard :: RnTyKiEnv
              -> Maybe Name -- ^ name of the wildcard,
                            -- or 'Nothing' for an anonymous wildcard
              -> Maybe BadAnonWildcardContext
              -> RnM ()
checkWildCard env mb_name (Just bad)
  = addErr $ TcRnWithHsDocContext (rtke_ctxt env) $
             TcRnIllegalWildcardInType mb_name bad
checkWildCard _ _ Nothing
  = return ()

checkAnonWildCard :: RnTyKiEnv -> RnM ()
-- Report an error if an anonymous wildcard is illegal here
checkAnonWildCard env
  = checkWildCard env Nothing mb_bad
  where
    mb_bad :: Maybe BadAnonWildcardContext
    mb_bad | not (wildCardsAllowed env)
           = Just WildcardsNotAllowedAtAll
           | otherwise
           = case rtke_what env of
               RnTypeBody      -> Nothing
               RnTopConstraint -> Just WildcardNotLastInConstraint
               RnConstraint    -> Just WildcardNotLastInConstraint

checkNamedWildCard :: RnTyKiEnv -> Name -> RnM ()
-- Report an error if a named wildcard is illegal here
checkNamedWildCard env name
  = checkWildCard env (Just name) mb_bad
  where
    mb_bad | not (name `elemNameSet` rtke_nwcs env)
           = Nothing  -- Not a wildcard
           | not (wildCardsAllowed env)
           = Just WildcardsNotAllowedAtAll
           | otherwise
           = case rtke_what env of
               RnTypeBody      -> Nothing   -- Allowed
               RnTopConstraint -> Nothing   -- Allowed; e.g.
                  -- f :: (Eq _a) => _a -> Int
                  -- g :: (_a, _b) => T _a _b -> Int
                  -- The named tyvars get filled in from elsewhere
               RnConstraint    -> Just WildcardNotLastInConstraint

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
       SpecECtx {}         -> True
       FamPatCtx {}        -> True   -- Not named wildcards though
       GHCiCtx {}          -> True
       HsTypeCtx {}        -> True
       StandaloneKindSigCtx {} -> False  -- See Note [Wildcards in standalone kind signatures] in "GHC.Hs.Decls"
       _                   -> False



---------------
-- | Ensures either that we're in a type or that -XPolyKinds is set
checkPolyKinds :: RnTyKiEnv
               -> HsTypeOrSigType GhcPs
               -> RnM ()
checkPolyKinds env ty
  | isRnKindLevel env
  = do { polykinds <- xoptM LangExt.PolyKinds
       ; unless polykinds $
         addErr $ TcRnIllegalKind ty True }
checkPolyKinds _ _ = return ()

notInKinds :: RnTyKiEnv
           -> HsType GhcPs
           -> RnM ()
notInKinds env ty
  | isRnKindLevel env
  = addErr $ TcRnIllegalKind (HsType ty) False
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

---------------
bindHsQTyVars :: forall a b.
                 HsDocContext
              -> Maybe (a, [Name])  -- Just _  => an associated type decl
              -> FreeKiTyVars       -- Kind variables from scope
              -> LHsQTyVars GhcPs
              -> (LHsQTyVars GhcRn -> FreeKiTyVars -> RnM (b, FreeVars))
                  -- The FreeKiTyVars is null <=> all kind variables used in the
                  -- kind signature are bound on the left.  Reason:
                  -- the last clause of Note [CUSKs: complete user-supplied kind signatures]
                  -- in GHC.Hs.Decls
              -> RnM (b, FreeVars)

-- See Note [bindHsQTyVars examples]
-- (a) Bring kind variables into scope
--     both (i)  passed in body_kv_occs
--     and  (ii) mentioned in the kinds of hsq_bndrs
-- (b) Bring type variables into scope
--
bindHsQTyVars doc mb_assoc body_kv_occs hsq_bndrs thing_inside
  = do { let bndr_kv_occs = extractHsTyVarBndrsKVs hs_tv_bndrs

       ; let -- See Note [bindHsQTyVars examples] for what
             -- all these various things are doing
             bndrs, all_implicit_kvs :: [LocatedN RdrName]
             bndrs        = mapMaybe hsLTyVarLocName hs_tv_bndrs
             all_implicit_kvs = filterFreeVarsToBind bndrs $
               bndr_kv_occs ++ body_kv_occs
             body_remaining = filterFreeVarsToBind bndr_kv_occs $
              filterFreeVarsToBind bndrs body_kv_occs

       ; implicit_kvs <-
           case mb_assoc of
             Nothing -> filterInScopeM all_implicit_kvs
             Just (_, cls_tvs) -> filterInScopeNonClassM cls_tvs all_implicit_kvs
                 -- See Note [Class variables and filterInScope]

       ; traceRn "checkMixedVars3" $
           vcat [ text "bndrs"   <+> ppr hs_tv_bndrs
                , text "bndr_kv_occs"   <+> ppr bndr_kv_occs
                , text "body_kv_occs"   <+> ppr body_kv_occs
                , text "implicit_kvs"   <+> ppr implicit_kvs
                , text "body_remaining" <+> ppr body_remaining
                ]

       ; rnImplicitTvOccs mb_assoc implicit_kvs $ \ implicit_kv_nms' ->
         bindLHsTyVarBndrs doc NoWarnUnusedForalls mb_assoc hs_tv_bndrs $ \ rn_bndrs ->
           -- This is the only call site for bindLHsTyVarBndrs where we pass
           -- NoWarnUnusedForalls, which suppresses -Wunused-foralls warnings.
           -- See Note [Suppress -Wunused-foralls when binding LHsQTyVars].
    do { rn_bndrs <- traverse rnLHsTyVarBndrVisFlag rn_bndrs
       ; let -- The SrcSpan that rnImplicitTvOccs will attach to each Name will
             -- span the entire declaration to which the LHsQTyVars belongs,
             -- which will be reflected in warning and error messages. We can
             -- be a little more precise than that by pointing to the location
             -- of the LHsQTyVars instead, which is what bndrs_loc
             -- corresponds to.
             implicit_kv_nms = map (`setNameLoc` bndrs_loc) implicit_kv_nms'

       ; traceRn "bindHsQTyVars" (ppr hsq_bndrs $$ ppr implicit_kv_nms $$ ppr rn_bndrs)
       ; thing_inside (HsQTvs { hsq_ext = implicit_kv_nms
                              , hsq_explicit  = rn_bndrs })
                      body_remaining } }
  where
    hs_tv_bndrs = hsQTvExplicit hsq_bndrs

    -- The SrcSpan of the LHsQTyVars. For example, bndrs_loc would be the
    -- highlighted part in the class below:
    --
    --   class C (a :: j) (b :: k) where
    --            ^^^^^^^^^^^^^^^
    bndrs_loc = case map get_bndr_loc hs_tv_bndrs ++ map getLocA body_kv_occs of
      []         -> panic "bindHsQTyVars.bndrs_loc"
      [loc]      -> loc
      (loc:locs) -> loc `combineSrcSpans` last locs

    -- The in-tree API annotations extend the LHsTyVarBndr location to
    -- include surrounding parens. for error messages to be
    -- compatible, we recreate the location from the contents
    get_bndr_loc :: LHsTyVarBndr flag GhcPs -> SrcSpan
    get_bndr_loc (L l tvb) =
      combineSrcSpans
        (case hsBndrVar tvb of
          HsBndrWildCard tok ->
            case tok of
              NoEpTok   -> locA l
              EpTok loc -> locA loc
          HsBndrVar _ ln   -> getLocA ln)
        (case hsBndrKind tvb of
          HsBndrNoKind _ -> noSrcSpan
          HsBndrKind _ lk -> getLocA lk)

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

  implicit_kvs = [k1,k2,k1], kind variables free in kind signatures
                             of hs_tv_bndrs, and not bound by bndrs

* We want to quantify add implicit bindings for implicit_kvs

* If body_kv_occs is non-empty, then there is a kind variable
  mentioned in the kind signature that is not bound "on the left".
  That's one of the rules for a CUSK, so we pass that info on
  as the second argument to thing_inside.

* Order is important in these lists. Consider
    data T (a::k1) (b::k2)
  We want implicit_kvs to be [k1,k2], not [k2,k1], so that the inferred kind for
  T quantifies over kind variables in the user-specified order
    T :: forall k1 k2. k1 -> k2 -> Type  -- OK
    T :: forall k2 k1. k1 -> k2 -> Type  -- Bad
  This matters with TypeApplications

* bndr_kv_occs, body_kv_occs, and implicit_kvs can contain duplicates. All
  duplicate occurrences are removed when we bind them with rnImplicitTvOccs.

Finally, you may wonder why filterFreeVarsToBind removes in-scope variables
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

Note [Suppress -Wunused-foralls when binding LHsQTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The WarnUnusedForalls flag controls whether bindLHsTyVarBndrs should warn about
explicit type variable binders that go unused (e.g., the `a` in
`forall a. Int`). We almost always want to warn about these, since unused type
variables can usually be deleted without any repercussions. There is one
exception to this rule, however: binding LHsQTyVars. Consider this example:

  data Proxy a = Proxy

The `a` in `Proxy a` is bound by an LHsQTyVars, and the code which brings it
into scope, bindHsQTyVars, will invoke bindLHsTyVarBndrs in turn. As such, it
has a choice to make about whether to emit -Wunused-foralls warnings or not.
If it /did/ emit warnings, then the `a` would be flagged as unused. However,
this is not what we want! Removing the `a` in `Proxy a` would change its kind
entirely, which is a huge price to pay for fixing a warning.

Unlike other forms of type variable binders, dropping "unused" variables in
an LHsQTyVars can be semantically significant. As a result, we suppress
-Wunused-foralls warnings in exactly one place: in bindHsQTyVars.
-}

bindHsOuterTyVarBndrs :: OutputableBndrFlag flag 'Renamed
                      => HsDocContext
                      -> Maybe assoc
                         -- ^ @'Just' _@ => an associated type decl
                      -> FreeKiTyVars
                      -> HsOuterTyVarBndrs flag GhcPs
                      -> (HsOuterTyVarBndrs flag GhcRn -> RnM (a, FreeVars))
                      -> RnM (a, FreeVars)
bindHsOuterTyVarBndrs doc mb_cls implicit_vars outer_bndrs thing_inside =
  case outer_bndrs of
    HsOuterImplicit{} ->
      rnImplicitTvOccs mb_cls implicit_vars $ \implicit_vars' ->
        thing_inside $ HsOuterImplicit { hso_ximplicit = implicit_vars' }
    HsOuterExplicit{hso_bndrs = exp_bndrs} ->
      -- Note: If we pass mb_cls instead of Nothing below, bindLHsTyVarBndrs
      -- will use class variables for any names the user meant to bring in
      -- scope here. This is an explicit forall, so we want fresh names, not
      -- class variables. Thus: always pass Nothing.
      bindLHsTyVarBndrs doc WarnUnusedForalls Nothing exp_bndrs $ \exp_bndrs' -> do
        checkForAllTelescopeWildcardBndrs doc exp_bndrs'
        thing_inside $ HsOuterExplicit { hso_xexplicit = noExtField
                                       , hso_bndrs     = exp_bndrs' }

-- See Note [Term variable capture and implicit quantification]
warn_term_var_capture :: LocatedN RdrName -> RnM ()
warn_term_var_capture lVar = do
    gbl_env <- getGlobalRdrEnv
    local_env <- getLocalRdrEnv
    case demoteRdrNameTv $ unLoc lVar of
      Nothing           -> return ()
      Just demoted_name -> do
        let global_vars = lookupGRE gbl_env (LookupRdrName demoted_name SameNameSpace)
        let mlocal_var  = lookupLocalRdrEnv local_env demoted_name
        case mlocal_var of
          Just name -> warnCapturedTerm lVar (Right name)
          Nothing   -> unless (null global_vars) $
                         warnCapturedTerm lVar (Left global_vars)

bindHsForAllTelescope :: HsDocContext
                      -> HsForAllTelescope GhcPs
                      -> (HsForAllTelescope GhcRn -> RnM (a, FreeVars))
                      -> RnM (a, FreeVars)
bindHsForAllTelescope doc tele thing_inside =
  case tele of
    HsForAllVis { hsf_vis_bndrs = bndrs } ->
      bindLHsTyVarBndrs doc WarnUnusedForalls Nothing bndrs $ \bndrs' -> do
        checkForAllTelescopeWildcardBndrs doc bndrs'
        thing_inside $ mkHsForAllVisTele noAnn bndrs'
    HsForAllInvis { hsf_invis_bndrs = bndrs } ->
      bindLHsTyVarBndrs doc WarnUnusedForalls Nothing bndrs $ \bndrs' -> do
        checkForAllTelescopeWildcardBndrs doc bndrs'
        thing_inside $ mkHsForAllInvisTele noAnn bndrs'

-- See Note [Wildcard binders in disallowed contexts] in GHC.Hs.Type
checkForAllTelescopeWildcardBndrs :: HsDocContext
                                  -> [LHsTyVarBndr flag (GhcPass p)]
                                  -> RnM ()
checkForAllTelescopeWildcardBndrs doc tvbs = mapM_ report_err wc_bndr_locs
  where
    report_err :: SrcSpan -> RnM ()
    report_err loc =
      addErrAt loc $ TcRnWithHsDocContext doc $
        TcRnIllegalWildcardInType Nothing WildcardBndrInForallTelescope

    wc_bndr_locs :: [SrcSpan]
    wc_bndr_locs = [locA l | L l (HsTvb _ _ HsBndrWildCard{} _) <- tvbs ]

-- | Should GHC warn if a quantified type variable goes unused? Usually, the
-- answer is \"yes\", but in the particular case of binding 'LHsQTyVars', we
-- avoid emitting warnings.
-- See @Note [Suppress -Wunused-foralls when binding LHsQTyVars]@.
data WarnUnusedForalls
  = WarnUnusedForalls
  | NoWarnUnusedForalls

instance Outputable WarnUnusedForalls where
  ppr wuf = text $ case wuf of
    WarnUnusedForalls   -> "WarnUnusedForalls"
    NoWarnUnusedForalls -> "NoWarnUnusedForalls"

bindLHsTyVarBndrs :: (OutputableBndrFlag flag 'Renamed)
                  => HsDocContext
                  -> WarnUnusedForalls
                  -> Maybe a               -- Just _  => an associated type decl
                  -> [LHsTyVarBndr flag GhcPs]  -- User-written tyvars
                  -> ([LHsTyVarBndr flag GhcRn] -> RnM (b, FreeVars))
                  -> RnM (b, FreeVars)
bindLHsTyVarBndrs doc wuf mb_assoc tv_bndrs thing_inside
  = do { when (isNothing mb_assoc) (checkShadowedRdrNames tv_names_w_loc)
       ; checkDupRdrNames tv_names_w_loc
       ; go tv_bndrs thing_inside }
  where
    tv_names_w_loc = mapMaybe hsLTyVarLocName tv_bndrs

    go []     thing_inside = thing_inside []
    go (b:bs) thing_inside = bindLHsTyVarBndr doc mb_assoc b $ \ b' ->
                             do { (res, fvs) <- go bs $ \ bs' ->
                                                thing_inside (b' : bs')
                                ; warn_unused b' fvs
                                ; return (res, fvs) }

    warn_unused tv_bndr fvs = case wuf of
      WarnUnusedForalls   -> warnUnusedForAll doc tv_bndr fvs
      NoWarnUnusedForalls -> return ()

bindLHsTyVarBndr :: HsDocContext
                 -> Maybe a   -- associated class
                 -> LHsTyVarBndr flag GhcPs
                 -> (LHsTyVarBndr flag GhcRn -> RnM (b, FreeVars))
                 -> RnM (b, FreeVars)
bindLHsTyVarBndr doc mb_assoc (L loc (HsTvb x fl bvar kind)) thing_inside
  = do { (kind', fvs1) <- rnHsBndrKind doc kind
       ; (b, fvs2) <- bindHsBndrVar mb_assoc bvar $ \bvar' ->
            thing_inside (L loc (HsTvb x fl bvar' kind'))
       ; return (b, fvs1 `plusFV` fvs2) }

bindHsBndrVar :: Maybe a   -- associated class
              -> HsBndrVar GhcPs
              -> (HsBndrVar GhcRn -> RnM (b, FreeVars))
              -> RnM (b, FreeVars)
bindHsBndrVar mb_assoc (HsBndrVar _ lrdr@(L lv _)) thing_inside
  = do { tv_nm  <- newTyVarNameRn mb_assoc lrdr
       ; bindLocalNamesFV [tv_nm] $
         thing_inside (HsBndrVar noExtField (L lv tv_nm)) }
bindHsBndrVar _ (HsBndrWildCard _) thing_inside
  = thing_inside (HsBndrWildCard noExtField)

rnHsBndrKind :: HsDocContext -> HsBndrKind GhcPs -> RnM (HsBndrKind GhcRn, FreeVars)
rnHsBndrKind _ (HsBndrNoKind _) = return (HsBndrNoKind noExtField, emptyFVs)
rnHsBndrKind doc (HsBndrKind _ kind) =
  do { sig_ok <- xoptM LangExt.KindSignatures
     ; unless sig_ok (badKindSigErr doc kind)
     ; (kind', fvs) <- rnLHsKind doc kind
     ; return (HsBndrKind noExtField kind', fvs) }

-- Check for TypeAbstractions and update the type parameter of HsBndrVis.
-- The binder itself is already renamed and is returned unmodified.
rnLHsTyVarBndrVisFlag
  :: LHsTyVarBndr (HsBndrVis GhcPs) GhcRn
  -> RnM (LHsTyVarBndr (HsBndrVis GhcRn) GhcRn)
rnLHsTyVarBndrVisFlag (L loc bndr) = do
  let lbndr = L loc (updateHsTyVarBndrFlag rnHsBndrVis bndr)
  unlessXOptM LangExt.TypeAbstractions $ do
    when (isHsBndrInvisible (hsTyVarBndrFlag bndr)) $
      addErr (TcRnIllegalInvisTyVarBndr lbndr)
    when (isHsBndrWildCard (hsBndrVar bndr)) $
      addErr (TcRnIllegalWildcardTyVarBndr lbndr)
  return lbndr

-- rnHsBndrVis is almost a no-op, it simply discards the token for "@".
rnHsBndrVis :: HsBndrVis GhcPs -> HsBndrVis GhcRn
rnHsBndrVis (HsBndrRequired _)    = HsBndrRequired  noExtField
rnHsBndrVis (HsBndrInvisible _at) = HsBndrInvisible noExtField

newTyVarNameRn, newTyVarNameRnImplicit
  :: Maybe a -- associated class
  -> LocatedN RdrName -> RnM Name
newTyVarNameRn         mb_assoc = new_tv_name_rn mb_assoc newLocalBndrRn
newTyVarNameRnImplicit mb_assoc = new_tv_name_rn mb_assoc $ \lrdr ->
  do { warn_term_var_capture lrdr
     ; newLocalBndrRn lrdr }

new_tv_name_rn :: Maybe a -- associated class
               -> (LocatedN RdrName -> RnM Name) -- how to create a new name
               -> (LocatedN RdrName -> RnM Name)
new_tv_name_rn Nothing  cont lrdr = cont lrdr
new_tv_name_rn (Just _) cont lrdr@(L _ rdr)
  = do { rdr_env <- getLocalRdrEnv
       ; case lookupLocalRdrEnv rdr_env rdr of
           Just n -> return n -- Use the same Name as the parent class decl
           _      -> cont lrdr }

{- Note [Term variable capture and implicit quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-Wterm-variable-capture is a warning introduced in GHC Proposal #281 "Visible forall in types of terms",
Section 7.3: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst#73implicit-quantification

Its purpose is to notify users when implicit quantification occurs that would
stop working under RequiredTypeArguments. Example:

   a = 42
   id :: a -> a

As it stands, the `a` in the signature `id :: a -> a` is considered free and
leads to implicit quantification, as if the user wrote `id :: forall a. a -> a`.
Under RequiredTypeArguments it captures the term-level variable `a` (bound by `a = 42`),
leading to a type error.

`warn_term_var_capture` detects this by demoting the namespace of the
implicitly quantified type variable (`TvName` becomes `VarName`) and looking it up
in the environment. But when do we call `warn_term_var_capture`? It's tempting
to do so at the start of `rnImplicitTvOccs`, as soon as we know our implicit
variables:

  rnImplicitTvOccs mb_assoc implicit_vs_with_dups thing_inside
    = do { let implicit_vs = nubN implicit_vs_with_dups
         ; mapM_ warn_term_var_capture implicit_vs
         ... }

This approach generates false positives (#23434) because it misses a corner
case: class variables in associated types. Consider the following example:

  k = 12
  class C k a where
    type AT a :: k -> Type

If we look at the signature for `AT` in isolation, the `k` looks like a free
variable, so it's passed to `rnImplicitTvOccs`. And if we passed it to
`warn_term_var_capture`, we would find the `k` bound by `k = 12` and report a warning.
But we don't want that: `k` is actually bound in the declaration header of the
parent class.

The solution is to check if it's a class variable (this is done in `new_tv_name_rn`)
before we check for term variable capture.
-}

{-
*********************************************************
*                                                       *
        HsConDeclRecField
*                                                       *
*********************************************************

When renaming a HsConDeclRecField, we have to find the FieldLabel
associated with each field.  But we already have all the FieldLabels
available (since they were brought into scope by
GHC.Rename.Names.getLocalNonValBinders), so we just take the list as an
argument, build a map and look them up.
-}

rnHsConDeclRecFields :: HsDocContext -> [FieldLabel] -> [LHsConDeclRecField GhcPs]
                -> RnM ([LHsConDeclRecField GhcRn], FreeVars)
-- Also called from GHC.Rename.Module
-- No wildcards can appear in record fields
rnHsConDeclRecFields ctxt fls fields
   = mapFvRn (rnField fl_env env) fields
  where
    env    = mkTyKiEnv ctxt TypeLevel RnTypeBody
    fl_env = mkFsEnv [ (field_label $ flLabel fl, fl) | fl <- fls ]

rnField :: FastStringEnv FieldLabel -> RnTyKiEnv -> LHsConDeclRecField GhcPs
        -> RnM (LHsConDeclRecField GhcRn, FreeVars)
rnField fl_env env (L l (HsConDeclRecField _ names ty))
  = do { let new_names = map (fmap (lookupField fl_env)) names
       ; (new_ty, fvs) <- rnHsConDeclFieldTyKi env ty
       ; return (L l (HsConDeclRecField noExtField new_names new_ty)
                , fvs) }

lookupField :: FastStringEnv FieldLabel -> FieldOcc GhcPs -> FieldOcc GhcRn
lookupField fl_env (FieldOcc _ (L lr rdr)) =
    FieldOcc (mkRdrUnqual $ occName sel) (L lr sel)
  where
    lbl = occNameFS $ rdrNameOcc rdr
    sel = flSelector
        $ expectJust
        $ lookupFsEnv fl_env lbl

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
have already been renamed and rearranged.

In the past, mkHsOpTyRn used to handle (->), but this was unnecessary. In the
syntax tree produced by the parser, the arrow already has the least possible
precedence and does not require rearrangement.
-}

---------------
-- Building (ty1 `op1` (ty2a `op2` ty2b))
mkHsOpTyRn :: PromotionFlag
           -> LocatedN (WithUserRdr Name) -> Fixity -> LHsType GhcRn -> LHsType GhcRn
           -> RnM (HsType GhcRn)

mkHsOpTyRn prom1 op1 fix1 ty1 (L loc2 (HsOpTy _ prom2 ty2a op2 ty2b))
  = do  { fix2 <- lookupTyFixityRn (fmap getName op2)
        ; mk_hs_op_ty prom1 op1 fix1 ty1 prom2 op2 fix2 ty2a ty2b loc2 }

mkHsOpTyRn prom1 op1 _ ty1 ty2              -- Default case, no rearrangement
  = return (HsOpTy noExtField prom1 ty1 op1 ty2)

---------------
mk_hs_op_ty :: PromotionFlag -> LocatedN (WithUserRdr Name) -> Fixity -> LHsType GhcRn
            -> PromotionFlag -> LocatedN (WithUserRdr Name) -> Fixity -> LHsType GhcRn
            -> LHsType GhcRn -> SrcSpanAnnA
            -> RnM (HsType GhcRn)
mk_hs_op_ty prom1 op1 fix1 ty1 prom2 op2 fix2 ty2a ty2b loc2
  | nofix_error     = do { precParseErr (NormalOp (unLoc op1),fix1)
                                        (NormalOp (unLoc op2),fix2)
                         ; return (ty1 `op1ty` (L loc2 (ty2a `op2ty` ty2b))) }
  | associate_right = return (ty1 `op1ty` (L loc2 (ty2a `op2ty` ty2b)))
  | otherwise       = do { -- Rearrange to ((ty1 `op1` ty2a) `op2` ty2b)
                           new_ty <- mkHsOpTyRn prom1 op1 fix1 ty1 ty2a
                         ; return (noLocA new_ty `op2ty` ty2b) }
  where
    lhs `op1ty` rhs = HsOpTy noExtField prom1 lhs op1 rhs
    lhs `op2ty` rhs = HsOpTy noExtField prom2 lhs op2 rhs
    (nofix_error, associate_right) = compareFixity fix1 fix2


---------------------------
mkOpAppRn :: NegationHandling
          -> LHsExpr GhcRn             -- Left operand; already rearranged
          -> LHsExpr GhcRn -> Fixity   -- Operator and fixity
          -> LHsExpr GhcRn             -- Right operand (not an OpApp, but might
                                       -- be a NegApp)
          -> RnM (HsExpr GhcRn)

-- (e1a `op1` e1b) `op2` e2
mkOpAppRn negation_handling e1@(L _ (OpApp fix1 e1a op1 e1b)) op2 fix2 e2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (OpApp fix2 e1 op2 e2)

  | associate_right = do
    new_e <- mkOpAppRn negation_handling e1b op2 fix2 e2
    return (OpApp fix1 e1a op1 (L loc' new_e))
  where
    loc'= combineLocsA e1b e2
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--      (- neg_arg) `op` e2
mkOpAppRn ReassociateNegation e1@(L _ (NegApp _ neg_arg neg_name)) op2 fix2 e2
  | nofix_error
  = do precParseErr (NegateOp,negateFixity) (get_op op2,fix2)
       return (OpApp fix2 e1 op2 e2)

  | associate_right
  = do new_e <- mkOpAppRn ReassociateNegation neg_arg op2 fix2 e2
       return (NegApp noExtField (L loc' new_e) neg_name)
  where
    loc' = combineLocsA neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--      e1 `op` - neg_arg
mkOpAppRn ReassociateNegation e1 op1 fix1 e2@(L _ (NegApp {})) -- NegApp can occur on the right
  | not associate_right                        -- We *want* right association
  = do precParseErr (get_op op1, fix1) (NegateOp, negateFixity)
       return (OpApp fix1 e1 op1 e2)
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--      Default case
mkOpAppRn _ e1 op fix e2                  -- Default case, no rearrangement
  = assertPpr (right_op_ok fix (unLoc e2))
              (ppr e1 $$ text "---" $$ ppr op $$ text "---" $$ ppr fix $$ text "---" $$ ppr e2) $
    return (OpApp fix e1 op e2)

data NegationHandling = ReassociateNegation | KeepNegationIntact

----------------------------

get_op :: LHsExpr GhcRn -> OpName
-- An unbound name could be either HsVar or (HsHole (HoleVar _, _))
-- See GHC.Rename.Expr.rnUnboundVar
get_op (L _ (HsVar _ n))                 = NormalOp (unLoc n)
get_op (L _ (HsHole (HoleVar (L _ uv)))) = UnboundOp uv
get_op (L _ (XExpr (HsRecSelRn fld)))    = RecFldOp fld
get_op other                             = pprPanic "get_op" (ppr other)

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
mkNegAppRn :: LHsExpr GhcRn -> SyntaxExpr GhcRn -> RnM (HsExpr GhcRn)
mkNegAppRn neg_arg neg_name
  = assert (not_op_app (unLoc neg_arg)) $
    return (NegApp noExtField neg_arg neg_name)

not_op_app :: HsExpr id -> Bool
not_op_app (OpApp {}) = False
not_op_app _          = True

--------------------------------------
mkConOpPatRn :: LocatedN (WithUserRdr Name)
             -> Fixity -> LPat GhcRn -> LPat GhcRn
             -> RnM (Pat GhcRn)

mkConOpPatRn op2 fix2 p1@(L loc (ConPat NoExtField op1 (InfixCon p1a p1b))) p2
  = do  { fix1 <- lookupFixityRn (getName op1)
        ; let (nofix_error, associate_right) = compareFixity fix1 fix2

        ; if nofix_error then do
                { precParseErr (NormalOp (unLoc op1),fix1)
                               (NormalOp (unLoc op2),fix2)
                ; return $ ConPat
                    { pat_con_ext = noExtField
                    , pat_con  = op2
                    , pat_args = InfixCon p1 p2
                    }
                }

          else if associate_right then do
                { new_p <- mkConOpPatRn op2 fix2 p1b p2
                ; return $ ConPat
                    { pat_con_ext = noExtField
                    , pat_con = op1
                    , pat_args = InfixCon p1a (L loc new_p)
                    }
                }
                -- XXX loc right?
          else return $ ConPat
                 { pat_con_ext = noExtField
                 , pat_con = op2
                 , pat_args = InfixCon p1 p2
                 }
        }

mkConOpPatRn op _ p1 p2                         -- Default case, no rearrangement
  = assert (not_op_pat (unLoc p2)) $
    return $ ConPat
      { pat_con_ext = noExtField
      , pat_con = op
      , pat_args = InfixCon p1 p2
      }

not_op_pat :: Pat GhcRn -> Bool
not_op_pat (ConPat NoExtField _ (InfixCon _ _)) = False
not_op_pat _                                    = True

--------------------------------------
checkPrecMatch :: Name -> MatchGroup GhcRn body -> RnM ()
  -- Check precedence of a function binding written infix
  --   eg  a `op` b `C` c = ...
  -- See comments with rnExpr (OpApp ...) about "deriving"

checkPrecMatch op (MG { mg_alts = (L _ ms) })
  = mapM_ check ms
  where
    check (L _ (Match { m_pats = L _ ( (L l1 p1)
                                     : (L l2 p2)
                                     : _) }))
      = setSrcSpan (locA $ combineSrcSpansA l1 l2) $
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
checkPrec op (ConPat NoExtField op1 (InfixCon _ _)) right = do
    op_fix@(Fixity op_prec  op_dir) <- lookupFixityRn op
    op1_fix@(Fixity op1_prec op1_dir) <- lookupFixityRn (getName op1)
    let
        inf_ok = op1_prec > op_prec ||
                 (op1_prec == op_prec &&
                  (op1_dir == InfixR && op_dir == InfixR && right ||
                   op1_dir == InfixL && op_dir == InfixL && not right))

        info  = (NormalOp (noUserRdr op), op_fix)
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
    go_for_it arg_op arg_fix@(Fixity arg_prec assoc) = do
          op_fix@(Fixity op_prec _) <- lookupFixityOp op_name
          unless (op_prec < arg_prec
                  || (op_prec == arg_prec && direction == assoc))
                 (sectionPrecErr (get_op op, op_fix)
                                 (arg_op, arg_fix) section)

-- | Look up the fixity for an operator name.
lookupFixityOp :: OpName -> RnM Fixity
lookupFixityOp (NormalOp n)  = lookupFixityRn (getName n)
lookupFixityOp NegateOp      = lookupFixityRn negateName
lookupFixityOp (UnboundOp u) = lookupFixityRn (mkUnboundName (occName u))
lookupFixityOp (RecFldOp f)  = lookupFieldFixityRn f

-- Precedence-related error messages

precParseErr :: (OpName,Fixity) -> (OpName,Fixity) -> RnM ()
precParseErr op1@(n1,_) op2@(n2,_)
  | is_unbound n1 || is_unbound n2
  = return ()     -- Avoid error cascade
  | otherwise
  = addErr $ TcRnPrecedenceParsingError op1 op2

sectionPrecErr :: (OpName,Fixity) -> (OpName,Fixity) -> HsExpr GhcPs -> RnM ()
sectionPrecErr op@(n1,_) arg_op@(n2,_) section
  | is_unbound n1 || is_unbound n2
  = return ()     -- Avoid error cascade
  | otherwise
  = addErr $ TcRnSectionPrecedenceError op arg_op section

is_unbound :: OpName -> Bool
is_unbound (NormalOp n) = isUnboundName (getName n)
is_unbound UnboundOp{}  = True
is_unbound _            = False


{- *****************************************************
*                                                      *
                 Errors
*                                                      *
***************************************************** -}

unexpectedPatSigTypeErr :: HsPatSigType GhcPs -> TcRnMessage
unexpectedPatSigTypeErr ty
  = TcRnUnexpectedPatSigType ty

badKindSigErr :: HsDocContext -> LHsType GhcPs -> TcM ()
badKindSigErr doc (L loc ty)
  = setSrcSpanA loc $ addErr $
    TcRnWithHsDocContext doc $
    TcRnKindSignaturesDisabled (Left ty)

-- | Check for DataKinds violations in a type context, as well as \"obvious\"
-- violations in kind contexts.
-- See @Note [Checking for DataKinds]@ in "GHC.Tc.Validity" for more on this.
checkDataKinds :: RnTyKiEnv -> HsType GhcPs -> TcM ()
checkDataKinds env thing
  = do data_kinds <- xoptM LangExt.DataKinds
       unless data_kinds $
         addErr $ TcRnDataKindsError type_or_kind $ Left thing
  where
    type_or_kind | isRnKindLevel env = KindLevel
                 | otherwise         = TypeLevel

warnUnusedForAll :: OutputableBndrFlag flag 'Renamed
                 => HsDocContext -> LHsTyVarBndr flag GhcRn -> FreeVars -> TcM ()
warnUnusedForAll doc (L loc tvb) used_names =
  case hsBndrVar tvb of
    HsBndrWildCard _ -> return ()
    HsBndrVar _ (L _ tv) ->
      unless (tv `elemNameSet` used_names) $ do
        let msg = TcRnUnusedQuantifiedTypeVar doc (HsTyVarBndrExistentialFlag tvb)
        addDiagnosticAt (locA loc) msg

warnCapturedTerm :: LocatedN RdrName -> Either [GlobalRdrElt] Name -> TcM ()
warnCapturedTerm (L loc tv) shadowed_term_names
  = let msg = TcRnCapturedTermName tv shadowed_term_names
    in addDiagnosticAt (locA loc) msg

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

It is common for lists of free type variables to contain duplicates. For
example, in `f :: a -> a`, the free type variable list is [a, a]. When these
implicitly bound variables are brought into scope (with rnImplicitTvOccs),
duplicates are removed with nubL.

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
See Note [ScopedSort] in GHC.Core.Type.

Implicitly bound variables are collected by any function which returns a
FreeKiTyVars, which notably includes the `extract-` family of functions
(extractHsTysRdrTyVars, extractHsTyVarBndrsKVs, etc.).
These functions thus promise to keep left-to-right ordering.

Note that for 'HsFunTy m ty1 ty2', we quantify in the order ty1, m, ty2,
since this type is written ty1 %m -> ty2 in the source syntax.

Note [Implicit quantification in type synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We typically bind type/kind variables implicitly when they are in a kind
annotation on the LHS, for example:

  data Proxy (a :: k) = Proxy
  type KindOf (a :: k) = k

Here 'k' is in the kind annotation of a type variable binding, HsBndrKind, and
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

The logic resides in extractHsTyRdrTyVarsKindVars.

This was a stopgap solution until we could explicitly bind invisible
type/kind variables:

  type TySyn3 :: forall a. Maybe a
  type TySyn3 @a = 'Just ('Nothing :: Maybe a)

Now that the new syntax was proposed in #425 and implemented in 9.8, we issue a warning
-Wimplicit-rhs-quantification for TySyn2 and TySyn4 and will eventually disallow them.

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

-- A list of free type/kind variables, which can contain duplicates.
-- See Note [Kind and type-variable binders]
-- These lists are guaranteed to preserve left-to-right ordering of
-- the types the variables were extracted from. See also
-- Note [Ordering of implicit variables].
type FreeKiTyVars = [LocatedN RdrName]

-- When renaming a type, do we want to capture or ignore term variables?
-- Suppose we have a variable binding `a` and we are renaming a type signature
-- that mentions `a`:
--
--    f :: forall t -> ...
--    f a = ...
--      where g :: a -> Bool
--
-- Does the `a` in the signature for `g` refer to the term variable or is it
-- implicitly quantified, as if the user wrote `g :: forall a. a -> Bool`?
-- `CaptureTermVars` selects the former behavior, `DontCaptureTermVars` the latter.
data TermVariableCapture =
    CaptureTermVars
  | DontCaptureTermVars

getTermVariableCapture :: RnM TermVariableCapture
getTermVariableCapture
  = do { required_type_arguments <- xoptM LangExt.RequiredTypeArguments
       ; let tvc | required_type_arguments = CaptureTermVars
                 | otherwise               = DontCaptureTermVars
       ; return tvc }

-- | Filter out any type and kind variables that are already in scope in the
-- the supplied LocalRdrEnv. Note that this includes named wildcards, which
-- look like perfectly ordinary type variables at this point.
filterInScope :: TermVariableCapture -> (GlobalRdrEnv, LocalRdrEnv) -> FreeKiTyVars -> FreeKiTyVars
filterInScope tvc envs = filterOut (inScope tvc envs . unLoc)

-- | Like 'filterInScope', but keep parent class variables intact.
-- Used with associated types. See Note [Class variables and filterInScope]
filterInScopeNonClass :: [Name] -> TermVariableCapture -> (GlobalRdrEnv, LocalRdrEnv) -> FreeKiTyVars -> FreeKiTyVars
filterInScopeNonClass cls_tvs tvc envs = filterOut (in_scope_non_class . unLoc)
  where
    in_scope_non_class :: RdrName -> Bool
    in_scope_non_class rdr
      | occName rdr `elemOccSet` cls_tvs_set = False
      | otherwise = inScope tvc envs rdr

    cls_tvs_set :: OccSet
    cls_tvs_set = mkOccSet (map nameOccName cls_tvs)

-- | Filter out any type and kind variables that are already in scope in the
-- the environment's LocalRdrEnv. Note that this includes named wildcards,
-- which look like perfectly ordinary type variables at this point.
filterInScopeM :: FreeKiTyVars -> RnM FreeKiTyVars
filterInScopeM vars
  = do { tvc <- getTermVariableCapture
       ; envs <- getRdrEnvs
       ; return (filterInScope tvc envs vars) }

-- | Like 'filterInScopeM', but keep parent class variables intact.
-- Used with associated types. See Note [Class variables and filterInScope]
filterInScopeNonClassM :: [Name] -> FreeKiTyVars -> RnM FreeKiTyVars
filterInScopeNonClassM cls_tvs vars
  = do { tvc <- getTermVariableCapture
       ; envs <- getRdrEnvs
       ; return (filterInScopeNonClass cls_tvs tvc envs vars) }

inScope :: TermVariableCapture -> (GlobalRdrEnv, LocalRdrEnv) -> RdrName -> Bool
inScope tvc (gbl, lcl) rdr =
  case tvc of
    DontCaptureTermVars -> rdr_in_scope
    CaptureTermVars     -> rdr_in_scope || demoted_rdr_in_scope
  where
    rdr_in_scope, demoted_rdr_in_scope :: Bool
    rdr_in_scope         = elem_lcl rdr
    demoted_rdr_in_scope = maybe False (elem_lcl <||> elem_gbl) (demoteRdrNameTv rdr)

    elem_lcl, elem_gbl :: RdrName -> Bool
    elem_lcl name = elemLocalRdrEnv name lcl
    elem_gbl name = (not . null) (lookupGRE gbl (LookupRdrName name (RelevantGREsFOS WantBoth)))

{- Note [Class variables and filterInScope]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `bindHsQTyVars` free variables are collected and bound implicitly. Example:
  type ElemKind (a :: Maybe k) = k
Both occurrences of `k` are usages, so GHC creates an implicit binding for `k`.
We can also bind it explicitly with the help of TypeAbstractions:
  type ElemKind @k (a :: Maybe k) = k

This is similar to implicit quantification that happens in type signatures
  const :: a -> b -> a               -- `a` and `b` bound implicitly
  const :: forall a b. a -> b -> a   -- `a` and `b` bound explicitly

In both cases we need to compute the list of free variables to implicitly
quantify over (NB: list, not set, because order matters with TypeApplications)
  * implicitly bound variables in ElemKind:  [k]
  * implicitly bound variables in const:     [a, b]

However, variables that are already in scope are captured. Example:
  {-# LANGUAGE ScopedTypeVariables #-}
  f (x :: a) = ...
    where g :: a -> a
          g _ = x
When we look at `g` in isolation, `a` is a free variable:
  g :: a -> a
But due to ScopedTypeVariables, `a` is actually bound in the surrounding
context, namely the (x :: a) pattern.
It is important that we do /not/ insert an implicit forall in the type of `g`
  g :: forall a. a -> a   -- No! That would mean a different thing
The solution is to use `filterInScopeM` to remove variables already in scope
from candidates for implicit quantification.

Using `filterInScopeM` is additionally important when RequiredTypeArguments is
in effect. Consider
  import Prelude (id)
  data T (a :: id) = MkT    -- Test case T23740e

With RequiredTypeArguments, variables in the term namespace can be referred to
at the type level, so `id` in the `:: id` kind signature ought to refer to `id`
from Prelude. Of course it is not a valid program, but we want a proper error
message rather than implicit quantification:
  T23740e.hs:5:14: error: [GHC-45510]
      • Term variable ‘id’ cannot be used here
          (term variables cannot be promoted)

This leads us to use `filterInScopeM` in `bindHsQTyVars`.
Unfortunately, associated types make matters more complex. Consider
  class C (a::k1) (b::k2) (c::k3) where
    type T (a::k1) b
When renaming `T`, we have to implicitly quantify over the class variable `k1`,
despite the fact that `k1` is in scope from the class header. The type checker
expects to find `k1` in `HsQTvsRn` (the `hsq_ext` field of `LHsQTyVars`). But it
won't find it there if it is filtered out by `filterInScopeM`.

To account for that, we introduce another helper, `filterInScopeNonClassM`,
which acts much like `filterInScopeM` but leaves class variables intact. -}

extract_tyarg :: LHsTypeArg GhcPs -> FreeKiTyVars -> FreeKiTyVars
extract_tyarg (HsValArg _ ty) acc = extract_lty ty acc
extract_tyarg (HsTypeArg _ ki) acc = extract_lty ki acc
extract_tyarg (HsArgPar _) acc = acc

extract_tyargs :: [LHsTypeArg GhcPs] -> FreeKiTyVars -> FreeKiTyVars
extract_tyargs args acc = foldr extract_tyarg acc args

extractHsTyArgRdrKiTyVars :: [LHsTypeArg GhcPs] -> FreeKiTyVars
extractHsTyArgRdrKiTyVars args
  = extract_tyargs args []

-- | 'extractHsTyRdrTyVars' finds the type/kind variables
--                          of a HsType/HsKind.
-- It's used when making the @forall@s explicit.
-- See Note [Kind and type-variable binders]
extractHsTyRdrTyVars :: LHsType GhcPs -> FreeKiTyVars
extractHsTyRdrTyVars ty = extract_lty ty []

-- | Extracts the free type/kind variables from the kind signature of a HsType.
--   This is used to implicitly quantify over @k@ in @type T = Nothing :: Maybe k@.
-- The left-to-right order of variables is preserved.
-- See Note [Kind and type-variable binders] and
--     Note [Ordering of implicit variables] and
--     Note [Implicit quantification in type synonyms].
extractHsTyRdrTyVarsKindVars :: LHsType GhcPs -> FreeKiTyVars
extractHsTyRdrTyVarsKindVars (L _ ty) =
  case ty of
    HsParTy _ ty -> extractHsTyRdrTyVarsKindVars ty
    HsKindSig _ _ ki -> extractHsTyRdrTyVars ki
    _ -> []

-- | Extracts free type and kind variables from types in a list.
-- When the same name occurs multiple times in the types, all occurrences
-- are returned.
extractHsTysRdrTyVars :: [LHsType GhcPs] -> FreeKiTyVars -> FreeKiTyVars
extractHsTysRdrTyVars tys = extract_ltys tys

-- Returns the free kind variables of any explicitly-kinded binders, returning
-- variable occurrences in left-to-right order.
-- See Note [Ordering of implicit variables].
-- NB: Does /not/ delete the binders themselves.
--     E.g. given  [k1, a:k1, b:k2]
--          the function returns [k1,k2], even though k1 is bound here
extractHsTyVarBndrsKVs :: [LHsTyVarBndr flag GhcPs] -> FreeKiTyVars
extractHsTyVarBndrsKVs tv_bndrs = extract_hs_tv_bndrs_kvs tv_bndrs

-- Returns the free kind variables in a type family result signature, returning
-- variable occurrences in left-to-right order.
-- See Note [Ordering of implicit variables].
extractRdrKindSigVars :: LFamilyResultSig GhcPs -> FreeKiTyVars
extractRdrKindSigVars (L _ resultSig) = case resultSig of
  KindSig _ k -> extractHsTyRdrTyVars k
  TyVarSig _ (L _ tvb) | HsTvb { tvb_kind = HsBndrKind _ k } <- tvb
    -> extractHsTyRdrTyVars k
  _ -> []

-- | Extracts free type and kind variables from an argument in a GADT
-- constructor, returning variable occurrences in left-to-right order.
-- See @Note [Ordering of implicit variables]@.
extractConDeclGADTDetailsTyVars ::
  HsConDeclGADTDetails GhcPs -> FreeKiTyVars -> FreeKiTyVars
extractConDeclGADTDetailsTyVars con_args = case con_args of
  PrefixConGADT _ args    -> extract_scaled_ltys args
  RecConGADT _ (L _ flds) -> extract_scaled_ltys $ map (cdrf_spec . unLoc) $ flds

-- | Get type/kind variables mentioned in the kind signature, preserving
-- left-to-right order:
--
--  * data T a (b :: k1) :: k2 -> k1 -> k2 -> Type   -- result: [k2,k1]
--  * data T a (b :: k1)                             -- result: []
--
-- See Note [Ordering of implicit variables].
extractDataDefnKindVars :: HsDataDefn GhcPs ->  FreeKiTyVars
extractDataDefnKindVars (HsDataDefn { dd_kindSig = ksig })
  = maybe [] extractHsTyRdrTyVars ksig

extract_lctxt :: LHsContext GhcPs -> FreeKiTyVars -> FreeKiTyVars
extract_lctxt ctxt = extract_ltys (unLoc ctxt)

extract_scaled_ltys :: [HsConDeclField GhcPs]
                    -> FreeKiTyVars -> FreeKiTyVars
extract_scaled_ltys args acc = foldr extract_scaled_lty acc args

extract_scaled_lty :: HsConDeclField GhcPs
                   -> FreeKiTyVars -> FreeKiTyVars
extract_scaled_lty (CDF { cdf_multiplicity, cdf_type }) acc
  = extract_lty cdf_type $ extract_hs_mult_ann cdf_multiplicity acc

extract_ltys :: [LHsType GhcPs] -> FreeKiTyVars -> FreeKiTyVars
extract_ltys tys acc = foldr extract_lty acc tys

extract_lty :: LHsType GhcPs -> FreeKiTyVars -> FreeKiTyVars
extract_lty (L _ ty) acc
  = case ty of
      HsTyVar _ _  ltv            -> extract_tv ltv acc
      HsAppTy _ ty1 ty2           -> extract_lty ty1 $
                                     extract_lty ty2 acc
      HsAppKindTy _ ty k          -> extract_lty ty $
                                     extract_lty k acc
      HsListTy _ ty               -> extract_lty ty acc
      HsTupleTy _ _ tys           -> extract_ltys tys acc
      HsSumTy _ tys               -> extract_ltys tys acc
      HsFunTy _ m ty1 ty2         -> extract_lty ty1 $
                                     extract_hs_mult_ann m $ -- See Note [Ordering of implicit variables]
                                     extract_lty ty2 acc
      HsIParamTy _ _ ty           -> extract_lty ty acc
      HsOpTy _ _ ty1 tv ty2       -> extract_lty ty1 $
                                     extract_tv tv $
                                     extract_lty ty2 acc
      HsParTy _ ty                -> extract_lty ty acc
      HsSpliceTy {}               -> acc  -- Type splices mention no tvs
      HsDocTy _ ty _              -> extract_lty ty acc
      HsExplicitListTy _ _ tys    -> extract_ltys tys acc
      HsExplicitTupleTy _ _ tys   -> extract_ltys tys acc
      HsTyLit _ _                 -> acc
      HsStarTy _ _                -> acc
      HsKindSig _ ty ki           -> extract_kind_sig ty ki acc
      HsForAllTy { hst_tele = tele, hst_body = ty }
                                  -> extract_hs_for_all_telescope tele acc $
                                     extract_lty ty []
      HsQualTy { hst_ctxt = ctxt, hst_body = ty }
                                  -> extract_lctxt ctxt $
                                     extract_lty ty acc
      XHsType {}                  -> acc
      -- We deal with these separately in rnLHsTypeWithWildCards
      HsWildCardTy {}             -> acc

extract_kind_sig :: LHsType GhcPs -- type
                 -> LHsType GhcPs -- kind
                 -> FreeKiTyVars -> FreeKiTyVars
extract_kind_sig ty ki acc
  | (L _ HsForAllTy { hst_tele = HsForAllInvis { hsf_invis_bndrs = bndrs }
                    , hst_body = ki_body }) <- ki
  = extract_hs_tv_bndrs bndrs acc $
    extract_lty ty $
    extract_lty ki_body []
extract_kind_sig ty ki acc
  = extract_lty ty $
    extract_lty ki acc

extract_lhs_sig_ty :: LHsSigType GhcPs -> FreeKiTyVars
extract_lhs_sig_ty (L _ (HsSig{sig_bndrs = outer_bndrs, sig_body = body})) =
  extractHsOuterTvBndrs outer_bndrs $ extract_lty body []

extract_hs_mult_ann :: HsMultAnn GhcPs -> FreeKiTyVars -> FreeKiTyVars
extract_hs_mult_ann (HsExplicitMult _ p) acc = extract_lty p acc
extract_hs_mult_ann _ acc = acc

extract_hs_for_all_telescope :: HsForAllTelescope GhcPs
                             -> FreeKiTyVars -- Accumulator
                             -> FreeKiTyVars -- Free in body
                             -> FreeKiTyVars
extract_hs_for_all_telescope tele acc_vars body_fvs =
  case tele of
    HsForAllVis { hsf_vis_bndrs = bndrs } ->
      extract_hs_tv_bndrs bndrs acc_vars body_fvs
    HsForAllInvis { hsf_invis_bndrs = bndrs } ->
      extract_hs_tv_bndrs bndrs acc_vars body_fvs

extractHsOuterTvBndrs :: HsOuterTyVarBndrs flag GhcPs
                      -> FreeKiTyVars -- Free in body
                      -> FreeKiTyVars -- Free in result
extractHsOuterTvBndrs outer_bndrs body_fvs =
  case outer_bndrs of
    HsOuterImplicit{}                  -> body_fvs
    HsOuterExplicit{hso_bndrs = bndrs} -> extract_hs_tv_bndrs bndrs [] body_fvs

extract_hs_tv_bndrs :: [LHsTyVarBndr flag GhcPs]
                    -> FreeKiTyVars  -- Accumulator
                    -> FreeKiTyVars  -- Free in body
                    -> FreeKiTyVars
-- In (forall (a :: Maybe e). a -> b) we have
--     'a' is bound by the forall
--     'b' is a free type variable
--     'e' is a free kind variable
extract_hs_tv_bndrs tv_bndrs acc_vars body_vars = new_vars ++ acc_vars
  where
    new_vars
      | null tv_bndrs = body_vars
      | otherwise = filterFreeVarsToBind tv_bndr_rdrs $ bndr_vars ++ body_vars
    -- NB: delete all tv_bndr_rdrs from bndr_vars as well as body_vars.
    -- See Note [Kind variable scoping]
    bndr_vars = extract_hs_tv_bndrs_kvs tv_bndrs
    tv_bndr_rdrs = mapMaybe hsLTyVarLocName tv_bndrs

extract_hs_tv_bndrs_kvs :: [LHsTyVarBndr flag GhcPs] -> FreeKiTyVars
-- Returns the free kind variables of any explicitly-kinded binders, returning
-- variable occurrences in left-to-right order.
-- See Note [Ordering of implicit variables].
-- NB: Does /not/ delete the binders themselves.
--     E.g. given  [k1, a:k1, b:k2]
--          the function returns [k1,k2], even though k1 is bound here
extract_hs_tv_bndrs_kvs tv_bndrs =
    foldr extract_lty []
          [k | L _ (HsTvb { tvb_kind = HsBndrKind _ k }) <- tv_bndrs]

extract_tv :: LocatedN RdrName -> FreeKiTyVars -> FreeKiTyVars
extract_tv tv acc =
  if isRdrTyVar (unLoc tv) && (not . isQual) (unLoc tv) then tv:acc else acc

-- Deletes duplicates in a list of Located things. This is used to:
--
-- * Delete duplicate occurrences of implicitly bound type/kind variables when
--   bringing them into scope (in rnImplicitTvOccs).
--
-- * Delete duplicate occurrences of named wildcards (in rn_hs_sig_wc_type and
--   rnHsWcType).
--
-- Importantly, this function is stable with respect to the original ordering
-- of things in the list. This is important, as it is a property that GHC
-- relies on to maintain the left-to-right ordering of implicitly quantified
-- type variables.
-- See Note [Ordering of implicit variables].
nubL :: Eq a => [GenLocated l a] -> [GenLocated l a]
nubL = nubBy eqLocated

nubN :: Eq a => [LocatedN a] -> [LocatedN a]
nubN = nubBy eqLocated

-- | Filter out any potential implicit binders that are either
-- already in scope, or are explicitly bound in the binder.
filterFreeVarsToBind :: FreeKiTyVars
                     -- ^ Explicitly bound here
                     -> FreeKiTyVars
                     -- ^ Potential implicit binders
                     -> FreeKiTyVars
                     -- ^ Final implicit binders
filterFreeVarsToBind bndrs = filterOut is_in_scope
    -- Make sure to list the binder kvs before the body kvs, as mandated by
    -- Note [Ordering of implicit variables]
  where
    is_in_scope locc = any (eqLocated locc) bndrs
