
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

module GHC.Rename.HsType (
        -- Monad stuff
        rnHsTyKi, rnLHsTyKi, rnLHsTyKindSig, bindSigTyVarsTyM,
        -- Type related stuff
        rnHsType, rnLHsType, rnLHsTypes, rnMaybeContext,
        rnLHsKind, rnLHsTypeArgs,
        rnHsSigType, rnHsWcType, rnHsTyLit,
        HsPatSigTypeScoping(..), rnHsSigWcType, rnHsPatSigType,
        newTyVarNameRn,
        rnConDeclFields,
        lookupField, mkHsOpTyRn,
        rnLTyVar,

        rnScaledLHsType,

        -- Precence related stuff
        NegationHandling(..),
        mkOpAppRn, mkNegAppRn, mkOpFormRn, mkConOpPatRn,
        checkPrecMatch, checkSectionPrec,

        -- Binding related stuff
        bindHsOuterTyVarBndrs, bindHsForAllTelescope,
        bindLHsTyVarBndr, bindLHsTyVarBndrs, WarnUnusedForalls(..),
        bindSigTyVarsFV, bindHsQTyVars,
        FreeKiTyVars,
        -- Error helpers
        badKindSigErr
  ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Rename.Splice( rnSpliceType )

import GHC.Core.TyCo.FVs ( tyCoVarsOfTypeList )
import GHC.Hs
import GHC.Rename.Env
import GHC.Rename.Doc
import GHC.Rename.Utils  ( mapFvRn, bindLocalNamesFV
                         , typeAppErr, newLocalBndrRn, checkDupRdrNames
                         , checkShadowedRdrNames, warnForallIdentifier, delLocalNames, bindLocalNames, checkDupNames )
import GHC.Rename.Fixity ( lookupFieldFixityRn, lookupFixityRn
                         , lookupTyFixityRn )
import GHC.Rename.Unbound ( notInScopeErr, WhereLooking(WL_LocalOnly) )
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
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
import GHC.Types.Basic  ( TypeOrKind(..), maxPrec )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Data.Maybe
import qualified GHC.LanguageExtensions as LangExt

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import Control.Monad
import GHC.Rename.Cps
import GHC.Rename.HsType.Monad


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
    (L loc (HsSig{sig_bndrs = outer_bndrs, sig_body = body_ty })) })
  = bindHsOuterTyVarBndrs doc outer_bndrs $ do
      body_ty' <- rnWcBodyTyKi body_ty
      pure $ \ty_bndrs outer_bndrs' -> do
        assertNoExpBndrs (exp_bndrs ty_bndrs)
        let wcs = nwc_bndrs ty_bndrs
        pure ( HsWC  { hswc_ext = wcs, hswc_body = L loc $
              HsSig { sig_ext = noExtField
                    , sig_bndrs = outer_bndrs', sig_body = body_ty' }}
            , emptyFVs)

rnHsPatSigType :: HsPatSigTypeScoping
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
rnHsPatSigType scoping ctx sig_ty thing_inside
  = do { ty_sig_okay <- xoptM LangExt.ScopedTypeVariables
       ; checkErr ty_sig_okay (unexpectedPatSigTypeErr sig_ty)
       ; let implicit_bndrs = case scoping of
               AlwaysBind -> bindImplicitly
               NeverBind  -> id
       ; (pat_sig_ty', ty_bndrs, fvs1) <- runRnTypeM ctx (implicit_bndrs $ rnWcBodyTyKi pat_sig_ty)
       ; assertNoExpBndrs (exp_bndrs ty_bndrs)
       ; let nwcs = nwc_bndrs ty_bndrs
             imp_tvs = imp_bndrs ty_bndrs
             sig_names = HsPSRn { hsps_nwcs = nwcs, hsps_imp_tvs = imp_tvs }
             sig_ty'   = HsPS { hsps_ext = sig_names, hsps_body = pat_sig_ty' }
       ; (res, fvs2) <- bindLocalNamesFV imp_tvs $ thing_inside sig_ty'
       ; return (res, fvs1 `plusFV` fvs2) }
  where
    pat_sig_ty = hsPatSigType sig_ty

rnHsWcType :: HsDocContext -> LHsWcType GhcPs -> RnM (LHsWcType GhcRn, FreeVars)
rnHsWcType ctxt (HsWC { hswc_body = hs_ty })
  = do { (hs_ty', ty_bndrs, fvs) <- runRnTypeM ctxt (rnWcBodyTyKi hs_ty)
       ; assertNoExpBndrs (exp_bndrs ty_bndrs)
       ; assertNoImpBndrs (imp_bndrs ty_bndrs)
       ; let
          wcs = nwc_bndrs ty_bndrs
          sig_ty' = HsWC { hswc_ext = wcs, hswc_body = hs_ty' }
       ; return (sig_ty', fvs) }


-- rnWcBodyType :: HsDocContext -> [LocatedN RdrName] -> LHsType GhcPs
--   -> RnM ([Name], LHsType GhcRn, FreeVars)
-- rnWcBodyType = rnWcBodyTyKi TypeLevel

rnWcBodyTyKi :: LHsType GhcPs -> RnTypeM (LHsType GhcRn)
rnWcBodyTyKi hs_ty = collectNamedWildcards (rn_lty hs_ty)
  where
    rn_lty (L loc hs_ty)
      = setSrcSpanATypeM loc $
        do { hs_ty' <- rn_ty hs_ty
           ; return (L loc hs_ty') }

    rn_ty :: HsType GhcPs -> RnTypeM (HsType GhcRn)
    -- A lot of faff just to allow the extra-constraints wildcard to appear
    rn_ty (HsForAllTy { hst_tele = tele, hst_body = hs_body })
      = bindHsForAllTelescope tele $ \ tele' ->
        do { hs_body' <- rn_lty hs_body
           ; return (HsForAllTy { hst_xforall = noExtField
                                , hst_tele = tele', hst_body = hs_body' }
                    ) }

    rn_ty (HsQualTy { hst_ctxt = L cx hs_ctxt
                        , hst_body = hs_ty })
      | Just (hs_ctxt1, hs_ctxt_last) <- snocView hs_ctxt
      , L lx (HsWildCardTy _)  <- ignoreParens hs_ctxt_last
      = do { hs_ctxt1' <- mapM rn_top_constraint hs_ctxt1
           ; doc <- askDocContext
           ; liftRnM $ setSrcSpanA lx $
             checkExtraConstraintWildCard doc hs_ctxt1
           ; let hs_ctxt' = hs_ctxt1' ++ [L lx (HsWildCardTy noExtField)]
           ; hs_ty' <- rnLHsTyKi hs_ty
           ; return (HsQualTy { hst_xqual = noExtField
                              , hst_ctxt = L cx hs_ctxt'
                              , hst_body = hs_ty' }) }

      | otherwise
      = do { hs_ctxt' <- mapM rn_top_constraint hs_ctxt
           ; hs_ty'   <- rnLHsTyKi hs_ty
           ; return (HsQualTy { hst_xqual = noExtField
                              , hst_ctxt = L cx hs_ctxt'
                              , hst_body = hs_ty' }) }


    rn_ty hs_ty = rnHsTyKi hs_ty

    rn_top_constraint = renameTypeAs RnTopConstraint . rnLHsTyKi


checkExtraConstraintWildCard :: HsDocContext -> HsContext GhcPs -> RnM ()
-- Rename the extra-constraint spot in a type signature
--    (blah, _) => type
-- Check that extra-constraints are allowed at all, and
-- if so that it's an anonymous wildcard
checkExtraConstraintWildCard doc hs_ctxt
  = checkWildCard doc Nothing mb_bad
  where
    mb_bad | not (extraConstraintWildCardsAllowed doc)
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
           | DerivDeclCtx {} <- doc
           , not (null hs_ctxt)
           = Just $ ExtraConstraintWildcardNotAllowed
                      SoleExtraConstraintWildcardAllowed
           | otherwise
           = Nothing

extraConstraintWildCardsAllowed :: HsDocContext -> Bool
extraConstraintWildCardsAllowed ctxt
  = case ctxt of
      TypeSigCtx {}       -> True
      ExprWithTySigCtx {} -> True
      DerivDeclCtx {}     -> True
      StandaloneKindSigCtx {} -> False  -- See Note [Wildcards in standalone kind signatures] in "GHC.Hs.Decls"
      _                   -> False


-- TODO sand-witch: rewrite
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
           HsOuterExplicit{} -> checkPolyKinds level (HsSigType sig_ty)
           HsOuterImplicit{} -> pure ()
       ; bindHsOuterTyVarBndrs ctx outer_bndrs $
    do { body' <- atLevel level $ rnLHsTyKi body

       ; return $ \_ outer_bndrs' ->
            pure ( L loc $ HsSig { sig_ext = noExtField
                                 , sig_bndrs = outer_bndrs', sig_body = body' },
                   emptyFVs) } }


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

--------------
rnLHsType  :: HsDocContext -> LHsType GhcPs -> RnM (LHsType GhcRn, FreeVars)
rnLHsType ctxt ty = runRnTypeMNoBndrs ctxt (rnLHsTyKi ty)


rnLHsTypes :: HsDocContext -> [LHsType GhcPs] -> RnM ([LHsType GhcRn], FreeVars)
rnLHsTypes doc tys = mapFvRn (rnLHsType doc) tys

rnScaledLHsType :: HsScaled GhcPs (LHsType GhcPs)
                  -> RnTypeM (HsScaled GhcRn (LHsType GhcRn))
rnScaledLHsType (HsScaled w ty) = do
  ty' <- rnLHsTyKi ty
  w' <- rnHsArrow w
  return (HsScaled w' ty')

rnHsType  :: HsDocContext -> HsType GhcPs -> RnM (HsType GhcRn, FreeVars)
rnHsType ctxt ty = runRnTypeMNoBndrs ctxt (rnHsTyKi ty)

rnLHsKind  :: HsDocContext -> LHsKind GhcPs -> RnM (LHsKind GhcRn, FreeVars)
rnLHsKind ctxt kind = runRnTypeMNoBndrs ctxt (atKindLevel $ rnLHsTyKi kind)

-- renaming a type only, not a kind
rnLHsTypeArg :: LHsTypeArg GhcPs -> RnTypeM (LHsTypeArg GhcRn)
rnLHsTypeArg (HsValArg ty)
   = do { tys_rn <- rnLHsTyKi ty
        ; return (HsValArg tys_rn) }
rnLHsTypeArg (HsTypeArg l ki)
   = do { kis_rn <- atKindLevel $ rnLHsTyKi ki
        ; return (HsTypeArg l kis_rn) }
rnLHsTypeArg (HsArgPar sp)
   = return (HsArgPar sp)

rnLHsTypeArgs :: [LHsTypeArg GhcPs] -> RnTypeM ([LHsTypeArg GhcRn])
rnLHsTypeArgs args = mapM rnLHsTypeArg args

--------------
rnTyKiContext :: LHsContext GhcPs
              -> RnTypeM (LHsContext GhcRn)
rnTyKiContext (L loc cxt)
  = do { liftRnM $ traceRn "rncontext" (ppr cxt)
       ; cxt' <- renameTypeAs RnConstraint $ mapM rnLHsTyKi cxt
       ; return (L loc cxt') }

rnMaybeContext :: Maybe (LHsContext GhcPs) -> RnTypeM (Maybe (LHsContext GhcRn))
rnMaybeContext Nothing = return (Nothing)
rnMaybeContext (Just theta)
  = do { (theta') <- rnTyKiContext theta
       ; return (Just theta')
       }

--------------

rnLHsTyKindSig :: LHsType GhcPs -> RnTypeM (LHsType GhcRn)
rnLHsTyKindSig = atKindLevel . shiftBindModeM . rnLHsTyKi
  where
    shiftBindModeM = localBindMode shiftBindMode

    shiftBindMode BindExplicitly = BindImplicitly
    shiftBindMode x = x

rnLHsTyKi  :: LHsType GhcPs -> RnTypeM (LHsType GhcRn)
rnLHsTyKi (L loc ty) = setSrcSpanATypeM loc $ do
  ty' <- rnHsTyKi ty
  return (L loc ty')


rnHsTyKi :: HsType GhcPs -> RnTypeM (HsType GhcRn)

rnHsTyKi ty@(HsForAllTy { hst_tele = tele, hst_body = tau }) = do
  level <- askLevel
  liftRnM $ checkPolyKinds level (HsType ty)
  bindHsForAllTelescope tele $ \ tele' -> do
    tau' <- rnLHsTyKi tau
    return (HsForAllTy { hst_xforall = noExtField
                       , hst_tele = tele' , hst_body =  tau' })

rnHsTyKi (HsQualTy { hst_ctxt = lctxt, hst_body = tau }) = do
  -- no need to check type vs kind level here; this is
  -- checked in the type checker. See
  -- Note [No constraints in kinds] in GHC.Tc.Validity
  (ctxt') <- rnTyKiContext lctxt
  (tau') <- rnLHsTyKi tau
  return (HsQualTy { hst_xqual = noExtField, hst_ctxt = ctxt'
                 , hst_body =  tau' })

rnHsTyKi (HsTyVar _ ip (L loc rdr_name)) = do
  ctxt <- askDocContext
  whenOnKind $
    when (isRdrTyVar rdr_name) $ liftRnM $
    unlessXOptM LangExt.PolyKinds $ addErr $
    TcRnWithHsDocContext ctxt $
    TcRnUnexpectedKindVar rdr_name
    -- Any type variable at the kind level is illegal without the use
    -- of PolyKinds (see #14710)
  name <- rnTyVar loc rdr_name
  when (isDataConName name && not (isPromoted ip)) $
         -- NB: a prefix symbolic operator such as (:) is represented as HsTyVar.
        liftRnM $ addDiagnostic (TcRnUntickedPromotedThing $ UntickedConstructor Prefix name)
  return (HsTyVar noAnn ip (L loc name))

rnHsTyKi ty@(HsOpTy _ prom ty1 l_op ty2) = setSrcSpanTypeM (getLocA l_op) $ do
  ty1' <- rnLHsTyKi ty1
  l_op' <- rnHsTyOp (ppr ty) l_op
  ty2' <- rnLHsTyKi ty2

  fix   <- liftRnM $ lookupTyFixityRn l_op'
  let op_name = unLoc l_op'
  res_ty <- liftRnM $ mkHsOpTyRn prom l_op' fix ty1' ty2'
  when (isDataConName op_name && not (isPromoted prom)) $
    liftRnM $ addDiagnostic (TcRnUntickedPromotedThing $ UntickedConstructor Infix op_name)
  return res_ty

rnHsTyKi (HsParTy _ ty) = do
  ty' <- rnLHsTyKi ty
  return (HsParTy noAnn ty')

rnHsTyKi (HsBangTy x b ty) = do
  ty' <- rnLHsTyKi ty
  checkTypeBang b ty'
  return (HsBangTy x b ty')

rnHsTyKi ty@(HsRecTy _ flds) = do
  ctxt  <- askDocContext
  fls   <- liftRnM $ get_fields ctxt
  flds' <- rnConDeclFields fls flds
  return (HsRecTy noExtField flds')
  where
    get_fields ctxt@(ConDeclCtx names) = do
      res <- concatMapM (lookupConstructorFields . unLoc) names
      if equalLength res names
        -- Lookup can fail when the record syntax is incorrect, e.g.
        -- data D = D Int { fld :: Bool }. See T7943.
        then return res
        else err ctxt
    get_fields ctxt = err ctxt

    err ctxt = do
      addErr $
        TcRnWithHsDocContext ctxt $
        TcRnIllegalRecordSyntax (Left ty)
      return []

rnHsTyKi (HsFunTy u mult ty1 ty2) = do
  ty1' <- rnLHsTyKi ty1
  mult' <- rnHsArrow mult
  ty2' <- rnLHsTyKi ty2
  return (HsFunTy u mult' ty1' ty2')

rnHsTyKi listTy@(HsListTy x ty) = do
  whenOnKind $
    checkDataKinds listTy
  ty' <- rnLHsTyKi ty
  return (HsListTy x ty')

rnHsTyKi (HsKindSig x ty k) = do
  kind_sigs_ok <- xoptTyM LangExt.KindSignatures
  ctxt <- askDocContext
  unless kind_sigs_ok (liftRnM $ badKindSigErr ctxt k)
  k'  <- rnLHsTyKindSig k
  ty' <- bindSigTyVarsTyM (hsScopedKvs k') $
         rnLHsTyKi ty
  return (HsKindSig x ty' k')

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsTyKi tupleTy@(HsTupleTy x tup_con tys) = do
  whenOnKind $
    checkDataKinds tupleTy
  tys' <- mapM rnLHsTyKi tys
  return (HsTupleTy x tup_con tys')

rnHsTyKi sumTy@(HsSumTy x tys) = do
  whenOnKind $ checkDataKinds sumTy
  tys' <- mapM rnLHsTyKi tys
  return (HsSumTy x tys')

-- Ensure that a type-level integer is nonnegative (#8306, #8412)
rnHsTyKi tyLit@(HsTyLit src t) = do
  checkDataKinds tyLit
  t' <- liftRnM $ rnHsTyLit t
  return (HsTyLit src t')

rnHsTyKi (HsAppTy _ ty1 ty2) = do
  ty1' <- rnLHsTyKi ty1
  ty2' <- rnLHsTyKi ty2
  return (HsAppTy noExtField ty1' ty2')

rnHsTyKi (HsAppKindTy _ ty at k) = do
  kind_app <- xoptTyM LangExt.TypeApplications
  unless kind_app (liftRnM $ addErr (typeAppErr KindLevel k))
  ty' <- rnLHsTyKi ty
  k' <- atKindLevel $ rnLHsTyKi k
  return (HsAppKindTy noExtField ty' at k')

rnHsTyKi t@(HsIParamTy x n ty) = do
  notInKinds t
  ty' <- rnLHsTyKi ty
  return (HsIParamTy x n ty')

rnHsTyKi (HsStarTy _ isUni)
  = return (HsStarTy noExtField isUni)

rnHsTyKi (HsSpliceTy _ sp) = do
  res <- liftRnFV $ rnSpliceType sp
  case res of
    (rn_splice, HsUntypedSpliceTop mfs ty) -> do -- Splice was top-level and thus run, creating LHsType GhcPs
        ty' <- rnLHsTyKi ty
        pure (HsSpliceTy (HsUntypedSpliceTop mfs (mb_paren ty')) rn_splice)
    (rn_splice, HsUntypedSpliceNested splice_name) ->
        pure (HsSpliceTy (HsUntypedSpliceNested splice_name) rn_splice) -- Splice was nested and thus already renamed
  where
    -- Wrap a non-atomic result in HsParTy parens;
    -- but not if it's atomic to avoid double parens for operators
    -- This is to account for, say  foo :: $(blah) -> Int
    -- when we want $(blah) to expand to (this -> that), with parens.
    -- Sadly, it's awkward add precisely the correct parens, because
    -- that depends on the context.
    mb_paren :: LHsType GhcRn -> LHsType GhcRn
    mb_paren lhs_ty@(L loc hs_ty)
      | hsTypeNeedsParens maxPrec hs_ty = L loc (HsParTy noAnn lhs_ty)
      | otherwise                       = lhs_ty

rnHsTyKi (HsDocTy x ty haddock_doc) = do
  ty' <- rnLHsTyKi ty
  haddock_doc' <- liftRnM $ rnLHsDoc haddock_doc
  return (HsDocTy x ty' haddock_doc')

-- See Note [Renaming HsCoreTys]
rnHsTyKi (XHsType ty) = do
  ctxt <- askDocContext
  liftRnFV $
    do mapM_ (check_in_scope ctxt . nameRdrName) fvs_list
       return (XHsType ty, fvs)
  where
    fvs_list = map getName $ tyCoVarsOfTypeList ty
    fvs = mkFVs fvs_list

    check_in_scope :: HsDocContext -> RdrName -> RnM ()
    check_in_scope ctxt rdr_name = do
      mb_name <- lookupLocalOccRn_maybe rdr_name
      when (isNothing mb_name) $
        addErr $
          TcRnWithHsDocContext ctxt $
            TcRnNotInScope (notInScopeErr WL_LocalOnly rdr_name) rdr_name [] []

rnHsTyKi ty@(HsExplicitListTy _ ip tys) = do
  checkDataKinds ty
  tys' <- mapM rnLHsTyKi tys
  unless (isPromoted ip) $
    liftRnM $ addDiagnostic (TcRnUntickedPromotedThing $ UntickedExplicitList)
  return (HsExplicitListTy noExtField ip tys')

rnHsTyKi ty@(HsExplicitTupleTy _ tys) = do
  checkDataKinds ty
  tys' <- mapM rnLHsTyKi tys
  return (HsExplicitTupleTy noExtField tys')

rnHsTyKi (HsWildCardTy _) = do
  what <- askWhatWeRename
  ctxt <- askDocContext
  liftRnM $ checkAnonWildCard ctxt what
  return (HsWildCardTy noExtField)

rnHsTyLit :: HsTyLit GhcPs -> RnM (HsTyLit GhcRn)
rnHsTyLit (HsStrTy x s) = pure (HsStrTy x s)
rnHsTyLit tyLit@(HsNumTy x i) = do
  when (i < 0) $
    addErr $ TcRnNegativeNumTypeLiteral tyLit
  pure (HsNumTy x i)
rnHsTyLit (HsCharTy x c) = pure (HsCharTy x c)


rnHsArrow :: HsArrow GhcPs -> RnTypeM (HsArrow GhcRn)
rnHsArrow (HsUnrestrictedArrow arr) = return (HsUnrestrictedArrow arr)
rnHsArrow (HsLinearArrow (HsPct1 pct1 arr)) = return (HsLinearArrow (HsPct1 pct1 arr))
rnHsArrow (HsLinearArrow (HsLolly arr)) = return (HsLinearArrow (HsLolly arr))
rnHsArrow (HsExplicitMult pct p arr)
  = (\mult -> HsExplicitMult pct mult arr) <$> rnLHsTyKi p

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
rnTyVar :: SrcSpanAnnN -> RdrName -> RnTypeM Name
rnTyVar loc rdr_name = do
  bind_mode <- askBindMode
  locals <- askLocals
  name <- rn_ty_var bind_mode locals
  checkNamedWildCard name
  return name

  where
    rn_ty_var _ _
      | not (isRdrTyVar rdr_name) || isQual rdr_name -- constructor or term-level name
      = lookupTypeOccRnTyM rdr_name

    rn_ty_var BindExplicitly locals
      | elemOccSet (occName rdr_name) locals -- See Note [Locally bound names in type patterns] in GHC.Rename.HsType.Monad
      = lookupTypeOccRnTyM rdr_name

      | otherwise = do
        name <- newTyVarNameRn ExplicitBinder (L loc rdr_name)
        tellTypeBinders (explicitTypeBinder name)
        pure name

    rn_ty_var bind_mode _ = do
      m_name <- lookupTypeOccRnTyM_maybe rdr_name
      case m_name of
        Just name -> pure name
        Nothing -> do
          nwc_enabled <- xoptTyM LangExt.NamedWildCards
          nwc_need_collect <- askNamedWildCardMode
          if nwc_enabled && nwc_need_collect && is_wildcard
            then bind_nwc
            else case bind_mode of
              BindImplicitly -> bind_implicitly
              DontBind -> onLookupFailRnTyM rdr_name
      where
        is_wildcard ::  Bool
        is_wildcard = startsWithUnderscore (rdrNameOcc rdr_name)

        new_imp_bind bndr_type = do
          liftRnM $ warn_term_var_capture (L loc rdr_name)
          newTyVarNameRn bndr_type (L loc rdr_name)

        bind_nwc = do
          name <- new_imp_bind NamedWildCardBinder
          tellNamedWildCard name
          pure name

        bind_implicitly = do
          name <- new_imp_bind ImplicitBinder
          tellTypeBinders (implicitTypeBinder name)
          pure name

rnLTyVar :: LocatedN RdrName -> RnM (LocatedN Name)
-- Called externally; does not deal with wildcards
rnLTyVar (L loc rdr_name)
  = do { tyvar <- lookupTypeOccRn rdr_name
       ; return (L loc tyvar) }

--------------
rnHsTyOp :: SDoc -> LocatedN RdrName
         -> RnTypeM (LocatedN Name)
rnHsTyOp overall_ty (L loc op)
  = do { op' <- rnTyVar loc op
       ; liftRnM $
         unlessXOptM LangExt.TypeOperators $
           if (op' `hasKey` eqTyConKey) -- See [eqTyCon (~) compatibility fallback] in GHC.Rename.Env
           then addDiagnostic TcRnTypeEqualityRequiresOperators
           else addErr $ TcRnIllegalTypeOperator overall_ty op
       ; return (L loc op') }

--------------
checkWildCard :: HsDocContext
              -> Maybe Name -- ^ name of the wildcard,
                            -- or 'Nothing' for an anonymous wildcard
              -> Maybe BadAnonWildcardContext
              -> RnM ()
checkWildCard ctxt mb_name (Just bad)
  = addErr $ TcRnWithHsDocContext ctxt $
             TcRnIllegalWildcardInType mb_name bad
checkWildCard _ _ Nothing
  = return ()

checkAnonWildCard :: HsDocContext -> RnTyKiWhat -> RnM ()
-- Report an error if an anonymous wildcard is illegal here
checkAnonWildCard ctxt what
  = checkWildCard ctxt Nothing mb_bad
  where
    mb_bad :: Maybe BadAnonWildcardContext
    mb_bad | not (wildCardsAllowed ctxt)
           = Just WildcardsNotAllowedAtAll
           | otherwise
           = case what of
               RnTypeBody      -> Nothing
               RnTopConstraint -> Just WildcardNotLastInConstraint
               RnConstraint    -> Just WildcardNotLastInConstraint

checkNamedWildCard :: Name -> RnTypeM ()
-- Report an error if a named wildcard is illegal here
checkNamedWildCard name = do
  nwcs <- askNamedWildCards
  ctxt <- askDocContext
  what <- askWhatWeRename
  let mb_bad | not (name `elemNameSet` nwcs)
        = Nothing  -- Not a wildcard
        | not (wildCardsAllowed ctxt)
        = Just WildcardsNotAllowedAtAll
        | otherwise
        = case what of
            RnTypeBody      -> Nothing   -- Allowed
            RnTopConstraint -> Nothing   -- Allowed; e.g.
               -- f :: (Eq _a) => _a -> Int
               -- g :: (_a, _b) => T _a _b -> Int
               -- The named tyvars get filled in from elsewhere
            RnConstraint    -> Just WildcardNotLastInConstraint

  liftRnM $ checkWildCard ctxt (Just name) mb_bad


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
       HsTypeCtx {}        -> True
       HsTypePatCtx{}      -> True  -- Not named wildcards though
       StandaloneKindSigCtx {} -> False  -- See Note [Wildcards in standalone kind signatures] in "GHC.Hs.Decls"
       _                   -> False



---------------
-- | Ensures either that we're in a type or that -XPolyKinds is set
checkPolyKinds :: TypeOrKind -> HsTypeOrSigType GhcPs
               -> RnM ()
checkPolyKinds level ty
  | KindLevel <- level
  = do { polykinds <- xoptM LangExt.PolyKinds
       ; unless polykinds $
         addErr $ TcRnIllegalKind ty True }
  | otherwise = pure ()

notInKinds :: HsType GhcPs
           -> RnTypeM ()
notInKinds ty
  = whenOnKind $ liftRnM $ addErr $ TcRnIllegalKind (HsType ty) False


checkTypeBang :: HsSrcBang -> LHsType GhcRn -> RnTypeM ()
checkTypeBang bang (L _ ty) = do
  ctxt <- askDocContext
  case ctxt of
    ConDeclCtx{} -> pure ()

    _ -> liftRnM $ addErr $
          TcRnWithHsDocContext ctxt $
          TcRnUnexpectedAnnotation ty bang


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

bindSigTyVarsTyM :: [Name]
                -> RnTypeM a
                -> RnTypeM a
-- like `bindSigTyVarsFV`, but in RnTypeM monad
bindSigTyVarsTyM tvs thing_inside
  = do  { scoped_tyvars <- xoptTyM LangExt.ScopedTypeVariables
        ; if not scoped_tyvars then
                thing_inside
          else
                bindLocalTyVarsFV tvs thing_inside }
  where
    bindLocalTyVarsFV :: [Name] -> RnTypeM a -> RnTypeM a
    bindLocalTyVarsFV names =
      mapRnMScoped (bindLocalNamesFV names) (delLocalNames names)

---------------
bindHsQTyVars :: forall a b.
                 HsDocContext
              -> Maybe (a, [Name])  -- Just _  => an associated type decl
              -> LHsQTyVars GhcPs
              -> (RnTypeM (FreeVars -> LHsQTyVars GhcRn -> RnTypeBinders -> RnM (b, FreeVars)))
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
bindHsQTyVars doc mb_assoc hsq_bndrs thing_inside = do
  (rn_bndrs, kv_bndrs, fvs1) <- runRnTypeM doc $
    bindImplicitly $ -- Bind kind-variables in binders
    bind_lhs_ty_var_bndrs NoWarnUnusedForalls mb_assoc hs_tv_bndrs $ \ rn_bndrs ->
      -- This is the only call site for bindLHsTyVarBndrs where we pass
      -- NoWarnUnusedForalls, which suppresses -Wunused-foralls warnings.
      -- See Note [Suppress -Wunused-foralls when binding LHsQTyVars].
      traverse (liftRnM . rnLHsTyVarBndrVisFlag) rn_bndrs

  assertNoExpBndrs (exp_bndrs kv_bndrs)
  assertNoNWCsBndrs (nwc_bndrs kv_bndrs)

  let
    implicit_bndr_kvs = imp_bndrs kv_bndrs
    explicit_bndrs = map hsLTyVarName rn_bndrs
    all_bndrs = implicit_bndr_kvs ++ explicit_bndrs

  checkDupNames all_bndrs

  (cont, ty_bndrs, fvs2) <-
    bindLocalNamesFVType all_bndrs $
      runRnTypeM doc $ thing_inside
  let
    body_remaining = imp_bndrs ty_bndrs
    cls_bndrs = case mb_assoc of
      Just (_, nms) ->
        filter
          (\n -> n `elemNameSet` fvs1 || n `elemNameSet` fvs2)
          nms
      Nothing -> []
    implicit_kv_nms = cls_bndrs ++ implicit_bndr_kvs ++ body_remaining

  traceRn "bindHsQTyVars" $ vcat
    [ text "hsq_bndrs" <+> ppr hsq_bndrs
    , text "implicit_kv_nms" <+> ppr implicit_kv_nms
    , text "rn_bndrs" <+> ppr rn_bndrs
    , text "all_bndrs" <+> ppr all_bndrs]
  (res, fvs3) <- bindLocalNamesFV all_bndrs $
      cont fvs2 (HsQTvs { hsq_ext = implicit_kv_nms
                        , hsq_explicit  = rn_bndrs })
                      ty_bndrs

  pure (res, fvs1 `plusFV` fvs3)

  where
    hs_tv_bndrs = hsQTvExplicit hsq_bndrs

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

* Order is not important in these lists.  All we are doing is
  bring Names into scope.

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
                      -> HsOuterTyVarBndrs flag GhcPs
                      -> RnTypeM (RnTypeBinders -> HsOuterTyVarBndrs flag GhcRn -> RnM (a, FreeVars))
                      -> RnM (a, FreeVars)
bindHsOuterTyVarBndrs doc outer_bndrs thing_inside =
  case outer_bndrs of
    HsOuterImplicit{} -> do
      (cont, ty_bndrs, fvs1) <- runRnTypeM doc $ bindImplicitly $ thing_inside
      let hs_outer_bndrs = HsOuterImplicit {hso_ximplicit = imp_bndrs ty_bndrs}
      (res, fvs2) <- cont ty_bndrs hs_outer_bndrs
      pure (res, fvs1 `plusFV` fvs2)
    HsOuterExplicit{hso_bndrs = exp_bndrs} -> do
      -- Note: If we pass mb_cls instead of Nothing below, bindLHsTyVarBndrs
      -- will use class variables for any names the user meant to bring in
      -- scope here. This is an explicit forall, so we want fresh names, not
      -- class variables. Thus: always pass Nothing.
      ((cont, hs_outer_bndrs), ty_bndrs, fvs1) <- runRnTypeM doc $
        bindLHsTyVarBndrs WarnUnusedForalls Nothing exp_bndrs $ \exp_bndrs' -> do
          let hs_outer_bndrs =
                HsOuterExplicit { hso_xexplicit = noExtField
                                , hso_bndrs     = exp_bndrs' }
          cont <- thing_inside
          pure (cont, hs_outer_bndrs)
      (res, fvs2) <- cont ty_bndrs hs_outer_bndrs
      pure (res, fvs1 `plusFV` fvs2)

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

bindHsForAllTelescope :: HsForAllTelescope GhcPs
                      -> (HsForAllTelescope GhcRn -> RnTypeM a)
                      -> RnTypeM a
bindHsForAllTelescope tele thing_inside =
  case tele of
    HsForAllVis { hsf_vis_bndrs = bndrs } ->
      bindLHsTyVarBndrs WarnUnusedForalls Nothing bndrs $ \bndrs' ->
        thing_inside $ mkHsForAllVisTele noAnn bndrs'
    HsForAllInvis { hsf_invis_bndrs = bndrs } ->
      bindLHsTyVarBndrs WarnUnusedForalls Nothing bndrs $ \bndrs' ->
        thing_inside $ mkHsForAllInvisTele noAnn bndrs'

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
                  => WarnUnusedForalls
                  -> Maybe a               -- Just _  => an associated type decl
                  -> [LHsTyVarBndr flag GhcPs]  -- User-written tyvars
                  -> ([LHsTyVarBndr flag GhcRn] -> RnTypeM b)
                  -> RnTypeM b
bindLHsTyVarBndrs wuf mb_assoc tv_bndrs thing_inside
  = do { liftRnM $ checkDupRdrNames tv_names_w_loc
       ; bind_lhs_ty_var_bndrs wuf mb_assoc tv_bndrs thing_inside }
  where
    tv_names_w_loc = map hsLTyVarLocName tv_bndrs


bind_lhs_ty_var_bndrs :: (OutputableBndrFlag flag 'Renamed)
                  => WarnUnusedForalls
                  -> Maybe a               -- Just _  => an associated type decl
                  -> [LHsTyVarBndr flag GhcPs]  -- User-written tyvars
                  -> ([LHsTyVarBndr flag GhcRn] -> RnTypeM b)
                  -> RnTypeM b
bind_lhs_ty_var_bndrs wuf mb_assoc tv_bndrs thing_inside
  = do { when (isNothing mb_assoc) (liftRnM $ checkShadowedRdrNames tv_names_w_loc)
       ; go tv_bndrs thing_inside }
  where
    tv_names_w_loc = map hsLTyVarLocName tv_bndrs

    go []     thing_inside = thing_inside []
    go (b:bs) thing_inside = bindLHsTyVarBndr wuf mb_assoc b $ \ b' ->
                             go bs $ \ bs' ->
                             thing_inside (b' : bs')

bindLHsTyVarBndr :: WarnUnusedForalls
                 -> Maybe a  -- Just _  => an associated type decl
                 -> LHsTyVarBndr flag GhcPs
                 -> (LHsTyVarBndr flag GhcRn -> RnTypeM b)
                 -> RnTypeM b
bindLHsTyVarBndr wuf mb_assoc (L loc
                                 (UserTyVar x fl
                                    lrdr@(L lv _))) thing_inside
  = newBinderTyVarNameRn wuf mb_assoc lrdr $ \nm ->
    thing_inside (L loc (UserTyVar x fl (L lv nm)))

bindLHsTyVarBndr wuf mb_assoc (L loc (KindedTyVar x fl lrdr@(L lv _) kind))
                 thing_inside
  = do { sig_ok <- xoptTyM LangExt.KindSignatures
       ; doc <- askDocContext
       ; unless sig_ok (liftRnM $ badKindSigErr doc kind)
       ; kind' <- rnLHsTyKindSig kind
       ; b <- newBinderTyVarNameRn wuf mb_assoc lrdr $ \tv_nm ->
           thing_inside (L loc (KindedTyVar x fl (L lv tv_nm) kind'))
       ; return b }

newBinderTyVarNameRn :: forall a b .
                        WarnUnusedForalls ->
                        Maybe a ->
                        LocatedN RdrName ->
                        (Name -> RnTypeM b) ->
                        RnTypeM b
newBinderTyVarNameRn wuf m_assoc lrdr@(L _ rdr_name) cont = do
  rdr_env <- liftRnM $ getLocalRdrEnv
  case lookupLocalRdrEnv rdr_env rdr_name of
    Just n | Just{}  <- m_assoc -> cont n
    Just old_name -> new_binder_ty_var (\_ -> bindLocalNames [old_name])
    Nothing -> new_binder_ty_var delLocalNames
  where
    new_binder_ty_var :: (forall a . [Name] -> RnM a -> RnM a) -> RnTypeM b
    new_binder_ty_var return_scope_action = do
      doc <- askDocContext
      name <- new_ty_var_name_rn (const id) (warn_unused doc) lrdr
      addNamesToLocals [name] $ -- see Note [Locally bound names in type patterns] in GHC.Rename.HsType.Monad
        mapRnMScoped
          (bindLocalNames [name])
          (return_scope_action [name]) $
          cont name

    warn_unused doc name fvs = case wuf of
      WarnUnusedForalls   -> do warnUnusedForAll doc name fvs; pure (delFV (unLoc name) fvs)
      NoWarnUnusedForalls -> pure (delFV (unLoc name) fvs)

-- Check for TypeAbstractions and update the type parameter of HsBndrVis.
-- The binder itself is already renamed and is returned unmodified.
rnLHsTyVarBndrVisFlag
  :: LHsTyVarBndr (HsBndrVis GhcPs) GhcRn
  -> RnM (LHsTyVarBndr (HsBndrVis GhcRn) GhcRn)
rnLHsTyVarBndrVisFlag (L loc bndr) = do
  let lbndr = L loc (updateHsTyVarBndrFlag rnHsBndrVis bndr)
  unlessXOptM LangExt.TypeAbstractions $
    when (isHsBndrInvisible (hsTyVarBndrFlag bndr)) $
      addErr (TcRnIllegalInvisTyVarBndr lbndr)
  return lbndr

-- rnHsBndrVis is a no-op. We could use 'coerce' in an ideal world,
-- but GHC can't crack this nut because type families are involved:
-- HsBndrInvisible stores (LHsToken "@" pass), which is defined via XRec.
rnHsBndrVis :: HsBndrVis GhcPs -> HsBndrVis GhcRn
rnHsBndrVis HsBndrRequired = HsBndrRequired
rnHsBndrVis (HsBndrInvisible at) = HsBndrInvisible at


new_ty_var_name_rn :: (forall a . Name -> RnM a -> RnM a) -- how to add variable into scope
                   -> (LocatedN Name -> FreeVars -> RnM FreeVars) -- how to threat free variables
                   -> LocatedN RdrName -> RnTypeM Name
new_ty_var_name_rn scope_pacher free_vars_pacher lrdr@(L loc _) =
  liftTyRnCps $ CpsRn $ \thing_inside -> do
    name <- newLocalBndrRn lrdr
    (res, fvs) <- scope_pacher name $ thing_inside name
    fvs' <- free_vars_pacher (L loc name) fvs
    pure (res, fvs')

newTyVarNameRn :: BinderType -> LocatedN RdrName -> RnTypeM Name
newTyVarNameRn bndr_type rdr_name = do
  fv_patcher <- askFreeVarsPatcher bndr_type
  new_ty_var_name_rn
    (\n -> bindLocalNames [n])
    fv_patcher
    rdr_name

-- TODO sand-witch: change note
{- Note [Term variable capture and implicit quantification]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-Wterm-variable-capture is a warning introduced in GHC Proposal #281 "Visible forall in types of terms",
Section 7.3: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0281-visible-forall.rst#73implicit-quantification

Its purpose is to notify users when implicit quantification occurs that would
stop working under RequiredTypeArguments (a future GHC extension). Example:

   a = 42
   id :: a -> a

As it stands, the `a` in the signature `id :: a -> a` is considered free and
leads to implicit quantification, as if the user wrote `id :: forall a. a -> a`.
Under RequiredTypeArguments it will capture the term-level variable `a` (bound by `a = 42`),
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
        ConDeclField
*                                                       *
*********************************************************

When renaming a ConDeclField, we have to find the FieldLabel
associated with each field.  But we already have all the FieldLabels
available (since they were brought into scope by
GHC.Rename.Names.getLocalNonValBinders), so we just take the list as an
argument, build a map and look them up.
-}


rnConDeclFields :: [FieldLabel] -> [LConDeclField GhcPs]
                -> RnTypeM ([LConDeclField GhcRn])
-- No wildcards can appear in record fields
rnConDeclFields fls fields = mapM (rnField fl_env) fields
  where
    fl_env = mkFsEnv [ (field_label $ flLabel fl, fl) | fl <- fls ]

rnField :: FastStringEnv FieldLabel -> LConDeclField GhcPs
        -> RnTypeM (LConDeclField GhcRn)
rnField fl_env (L l (ConDeclField _ names ty haddock_doc))
  = do { mapM_ (\(L _ (FieldOcc _ rdr_name)) -> liftRnM $ warnForallIdentifier rdr_name) names
       ; let new_names = map (fmap (lookupField fl_env)) names
       ; new_ty <- rnLHsTyKi ty
       ; haddock_doc' <- traverse (liftRnM . rnLHsDoc) haddock_doc
       ; return (L l (ConDeclField noAnn new_names new_ty haddock_doc')) }

lookupField :: FastStringEnv FieldLabel -> FieldOcc GhcPs -> FieldOcc GhcRn
lookupField fl_env (FieldOcc _ (L lr rdr)) =
    FieldOcc sel (L lr $ mkRdrUnqual $ occName sel)
  where
    lbl = occNameFS $ rdrNameOcc rdr
    sel = flSelector
        $ expectJust "lookupField"
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
           -> LocatedN Name -> Fixity -> LHsType GhcRn -> LHsType GhcRn
           -> RnM (HsType GhcRn)

mkHsOpTyRn prom1 op1 fix1 ty1 (L loc2 (HsOpTy _ prom2 ty2a op2 ty2b))
  = do  { fix2 <- lookupTyFixityRn op2
        ; mk_hs_op_ty prom1 op1 fix1 ty1 prom2 op2 fix2 ty2a ty2b loc2 }

mkHsOpTyRn prom1 op1 _ ty1 ty2              -- Default case, no rearrangement
  = return (HsOpTy noAnn prom1 ty1 op1 ty2)

---------------
mk_hs_op_ty :: PromotionFlag -> LocatedN Name -> Fixity -> LHsType GhcRn
            -> PromotionFlag -> LocatedN Name -> Fixity -> LHsType GhcRn
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
    lhs `op1ty` rhs = HsOpTy noAnn prom1 lhs op1 rhs
    lhs `op2ty` rhs = HsOpTy noAnn prom2 lhs op2 rhs
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
-- An unbound name could be either HsVar or HsUnboundVar
-- See GHC.Rename.Expr.rnUnboundVar
get_op (L _ (HsVar _ n))         = NormalOp (unLoc n)
get_op (L _ (HsUnboundVar _ uv)) = UnboundOp uv
get_op (L _ (HsRecSel _ fld))    = RecFldOp fld
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
mkNegAppRn :: LHsExpr GhcRn -> SyntaxExpr GhcRn -> RnM (HsExpr GhcRn)
mkNegAppRn neg_arg neg_name
  = assert (not_op_app (unLoc neg_arg)) $
    return (NegApp noExtField neg_arg neg_name)

not_op_app :: HsExpr id -> Bool
not_op_app (OpApp {}) = False
not_op_app _          = True

---------------------------
mkOpFormRn :: LHsCmdTop GhcRn            -- Left operand; already rearranged
          -> LHsExpr GhcRn -> Fixity     -- Operator and fixity
          -> LHsCmdTop GhcRn             -- Right operand (not an infix)
          -> RnM (HsCmd GhcRn)

-- (e1a `op1` e1b) `op2` e2
mkOpFormRn e1@(L loc
                    (HsCmdTop _
                     (L _ (HsCmdArrForm x op1 f (Just fix1)
                        [e1a,e1b]))))
        op2 fix2 e2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (HsCmdArrForm x op2 f (Just fix2) [e1, e2])

  | associate_right
  = do new_c <- mkOpFormRn e1a op2 fix2 e2
       return (HsCmdArrForm noExtField op1 f (Just fix1)
               [e1b, L loc (HsCmdTop [] (L (l2l loc) new_c))])
        -- TODO: locs are wrong
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2

--      Default case
mkOpFormRn arg1 op fix arg2                     -- Default case, no rearrangement
  = return (HsCmdArrForm noExtField op Infix (Just fix) [arg1, arg2])


--------------------------------------
mkConOpPatRn :: LocatedN Name -> Fixity -> LPat GhcRn -> LPat GhcRn
             -> RnM (Pat GhcRn)

mkConOpPatRn op2 fix2 p1@(L loc (ConPat NoExtField op1 (InfixCon p1a p1b))) p2
  = do  { fix1 <- lookupFixityRn (unLoc op1)
        ; let (nofix_error, associate_right) = compareFixity fix1 fix2

        ; if nofix_error then do
                { precParseErr (NormalOp (unLoc op1),fix1)
                               (NormalOp (unLoc op2),fix2)
                ; return $ ConPat
                    { pat_con_ext = noExtField
                    , pat_con = op2
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
    check (L _ (Match { m_pats = (L l1 p1)
                               : (L l2 p2)
                               : _ }))
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

-- | Look up the fixity for an operator name.
lookupFixityOp :: OpName -> RnM Fixity
lookupFixityOp (NormalOp n)  = lookupFixityRn n
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
is_unbound (NormalOp n) = isUnboundName n
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

checkDataKinds :: HsType GhcPs -> RnTypeM ()
checkDataKinds thing = do
  level <- askLevel
  data_kinds <- xoptTyM LangExt.DataKinds
  liftRnM $
    unless data_kinds $
    addErr $
    TcRnDataKindsError level thing

warnUnusedForAll :: HsDocContext -> LocatedN Name -> FreeVars -> TcM ()
warnUnusedForAll doc (L loc tv) used_names
  = unless (tv `elemNameSet` used_names) $ do
      let msg = TcRnWithHsDocContext doc $ TcRnUnusedQuantifiedTypeVar tv
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
