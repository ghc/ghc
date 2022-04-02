
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                       -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- NamedThing, Outputable, OutputableBndrId

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


GHC.Hs.Type: Abstract syntax: user-defined types
-}

module GHC.Hs.Type (
        Mult, HsScaled(..),
        hsMult, hsScaledThing,
        HsArrow(..), arrowToHsType,
        HsLinearArrowTokens(..),
        hsLinear, hsUnrestricted, isUnrestricted,
        pprHsArrow,

        HsType(..), HsCoreTy, LHsType, HsKind, LHsKind,
        HsForAllTelescope(..), EpAnnForallTy, HsTyVarBndr(..), LHsTyVarBndr,
        LHsQTyVars(..),
        HsOuterTyVarBndrs(..), HsOuterFamEqnTyVarBndrs, HsOuterSigTyVarBndrs,
        HsWildCardBndrs(..),
        HsPatSigType(..), HsPSRn(..),
        HsSigType(..), LHsSigType, LHsSigWcType, LHsWcType,
        HsTupleSort(..),
        HsContext, LHsContext, fromMaybeContext,
        HsTyLit(..),
        HsIPName(..), hsIPNameFS,
        HsArg(..), numVisibleArgs, pprHsArgsApp,
        LHsTypeArg, lhsTypeArgSrcSpan,
        OutputableBndrFlag,

        LBangType, BangType,
        HsSrcBang(..), HsImplBang(..),
        SrcStrictness(..), SrcUnpackedness(..),
        getBangType, getBangStrictness,

        ConDeclField(..), LConDeclField, pprConDeclFields,

        HsConDetails(..), noTypeArgs,

        FieldOcc(..), LFieldOcc, mkFieldOcc,
        AmbiguousFieldOcc(..), LAmbiguousFieldOcc, mkAmbiguousFieldOcc,
        rdrNameAmbiguousFieldOcc, selectorAmbiguousFieldOcc,
        unambiguousFieldOcc, ambiguousFieldOcc,

        mkAnonWildCardTy, pprAnonWildCard,

        hsOuterTyVarNames, hsOuterExplicitBndrs, mapHsOuterImplicit,
        mkHsOuterImplicit, mkHsOuterExplicit,
        mkHsImplicitSigType, mkHsExplicitSigType,
        mkHsWildCardBndrs, mkHsPatSigType,
        mkEmptyWildCardBndrs,
        mkHsForAllVisTele, mkHsForAllInvisTele,
        mkHsQTvs, hsQTvExplicit, emptyLHsQTvs,
        isHsKindedTyVar, hsTvbAllKinded,
        hsScopedTvs, hsWcScopedTvs, dropWildCards,
        hsTyVarName, hsAllLTyVarNames, hsLTyVarLocNames,
        hsLTyVarName, hsLTyVarNames, hsLTyVarLocName, hsExplicitLTyVarNames,
        splitLHsInstDeclTy, getLHsInstDeclHead, getLHsInstDeclClass_maybe,
        splitLHsPatSynTy,
        splitLHsForAllTyInvis, splitLHsForAllTyInvis_KP, splitLHsQualTy,
        splitLHsSigmaTyInvis, splitLHsGadtTy,
        splitHsFunType, hsTyGetAppHead_maybe,
        mkHsOpTy, mkHsAppTy, mkHsAppTys, mkHsAppKindTy,
        ignoreParens, hsSigWcType, hsPatSigType,
        hsTyKindSig,
        setHsTyVarBndrFlag, hsTyVarBndrFlag,

        -- Printing
        pprHsType, pprHsForAll,
        pprHsOuterFamEqnTyVarBndrs, pprHsOuterSigTyVarBndrs,
        pprLHsContext,
        hsTypeNeedsParens, parenthesizeHsType, parenthesizeHsContext
    ) where

import GHC.Prelude

import Language.Haskell.Syntax.Type

import {-# SOURCE #-} GHC.Hs.Expr ( pprSplice )

import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension
import GHC.Parser.Annotation

import GHC.Types.Fixity ( LexicalFixity(..) )
import GHC.Types.Id ( Id )
import GHC.Types.SourceText
import GHC.Types.Name( Name, NamedThing(getName) )
import GHC.Types.Name.Reader ( RdrName )
import GHC.Types.Var ( VarBndr )
import GHC.Core.TyCo.Rep ( Type(..) )
import GHC.Builtin.Types( manyDataConName, oneDataConName, mkTupleStr )
import GHC.Core.Ppr ( pprOccWithTick)
import GHC.Core.Type
import GHC.Hs.Doc
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Utils.Outputable

import Data.Maybe

import qualified Data.Semigroup as S

{-
************************************************************************
*                                                                      *
\subsection{Bang annotations}
*                                                                      *
************************************************************************
-}

getBangType :: LHsType (GhcPass p) -> LHsType (GhcPass p)
getBangType                 (L _ (HsBangTy _ _ lty))       = lty
getBangType (L _ (HsDocTy x (L _ (HsBangTy _ _ lty)) lds)) =
  addCLocA lty lds (HsDocTy x lty lds)
getBangType lty                                            = lty

getBangStrictness :: LHsType (GhcPass p) -> HsSrcBang
getBangStrictness                 (L _ (HsBangTy _ s _))     = s
getBangStrictness (L _ (HsDocTy _ (L _ (HsBangTy _ s _)) _)) = s
getBangStrictness _ = (HsSrcBang NoSourceText NoSrcUnpack NoSrcStrict)

{-
************************************************************************
*                                                                      *
\subsection{Data types}
*                                                                      *
************************************************************************
-}

fromMaybeContext :: Maybe (LHsContext (GhcPass p)) -> HsContext (GhcPass p)
fromMaybeContext mctxt = unLoc $ fromMaybe (noLocA []) mctxt

type instance XHsForAllVis   (GhcPass _) = EpAnnForallTy
                                           -- Location of 'forall' and '->'
type instance XHsForAllInvis (GhcPass _) = EpAnnForallTy
                                           -- Location of 'forall' and '.'

type instance XXHsForAllTelescope (GhcPass _) = DataConCantHappen

type EpAnnForallTy = EpAnn (AddEpAnn, AddEpAnn)
  -- ^ Location of 'forall' and '->' for HsForAllVis
  -- Location of 'forall' and '.' for HsForAllInvis

type HsQTvsRn = [Name]  -- Implicit variables
  -- For example, in   data T (a :: k1 -> k2) = ...
  -- the 'a' is explicit while 'k1', 'k2' are implicit

type instance XHsQTvs GhcPs = NoExtField
type instance XHsQTvs GhcRn = HsQTvsRn
type instance XHsQTvs GhcTc = HsQTvsRn

type instance XXLHsQTyVars  (GhcPass _) = DataConCantHappen

mkHsForAllVisTele ::EpAnnForallTy ->
  [LHsTyVarBndr () (GhcPass p)] -> HsForAllTelescope (GhcPass p)
mkHsForAllVisTele an vis_bndrs =
  HsForAllVis { hsf_xvis = an, hsf_vis_bndrs = vis_bndrs }

mkHsForAllInvisTele :: EpAnnForallTy
  -> [LHsTyVarBndr Specificity (GhcPass p)] -> HsForAllTelescope (GhcPass p)
mkHsForAllInvisTele an invis_bndrs =
  HsForAllInvis { hsf_xinvis = an, hsf_invis_bndrs = invis_bndrs }

mkHsQTvs :: [LHsTyVarBndr () GhcPs] -> LHsQTyVars GhcPs
mkHsQTvs tvs = HsQTvs { hsq_ext = noExtField, hsq_explicit = tvs }

emptyLHsQTvs :: LHsQTyVars GhcRn
emptyLHsQTvs = HsQTvs { hsq_ext = [], hsq_explicit = [] }

------------------------------------------------
--            HsOuterTyVarBndrs

type instance XHsOuterImplicit GhcPs = NoExtField
type instance XHsOuterImplicit GhcRn = [Name]
type instance XHsOuterImplicit GhcTc = [TyVar]

type instance XHsOuterExplicit GhcPs _    = EpAnnForallTy
type instance XHsOuterExplicit GhcRn _    = NoExtField
type instance XHsOuterExplicit GhcTc flag = [VarBndr TyVar flag]

type instance XXHsOuterTyVarBndrs (GhcPass _) = DataConCantHappen

type instance XHsWC              GhcPs b = NoExtField
type instance XHsWC              GhcRn b = [Name]
type instance XHsWC              GhcTc b = [Name]

type instance XXHsWildCardBndrs (GhcPass _) _ = DataConCantHappen

type instance XHsPS GhcPs = EpAnn EpaLocation
type instance XHsPS GhcRn = HsPSRn
type instance XHsPS GhcTc = HsPSRn

type instance XXHsPatSigType (GhcPass _) = DataConCantHappen

type instance XHsSig (GhcPass _) = NoExtField
type instance XXHsSigType (GhcPass _) = DataConCantHappen

hsSigWcType :: forall p. UnXRec p => LHsSigWcType p -> LHsType p
hsSigWcType = sig_body . unXRec @p . hswc_body

dropWildCards :: LHsSigWcType pass -> LHsSigType pass
-- Drop the wildcard part of a LHsSigWcType
dropWildCards sig_ty = hswc_body sig_ty

hsOuterTyVarNames :: HsOuterTyVarBndrs flag GhcRn -> [Name]
hsOuterTyVarNames (HsOuterImplicit{hso_ximplicit = imp_tvs}) = imp_tvs
hsOuterTyVarNames (HsOuterExplicit{hso_bndrs = bndrs})       = hsLTyVarNames bndrs

hsOuterExplicitBndrs :: HsOuterTyVarBndrs flag (GhcPass p)
                     -> [LHsTyVarBndr flag (NoGhcTc (GhcPass p))]
hsOuterExplicitBndrs (HsOuterExplicit{hso_bndrs = bndrs}) = bndrs
hsOuterExplicitBndrs (HsOuterImplicit{})                  = []

mkHsOuterImplicit :: HsOuterTyVarBndrs flag GhcPs
mkHsOuterImplicit = HsOuterImplicit{hso_ximplicit = noExtField}

mkHsOuterExplicit :: EpAnnForallTy -> [LHsTyVarBndr flag GhcPs]
                  -> HsOuterTyVarBndrs flag GhcPs
mkHsOuterExplicit an bndrs = HsOuterExplicit { hso_xexplicit = an
                                             , hso_bndrs     = bndrs }

mkHsImplicitSigType :: LHsType GhcPs -> HsSigType GhcPs
mkHsImplicitSigType body =
  HsSig { sig_ext   = noExtField
        , sig_bndrs = mkHsOuterImplicit, sig_body = body }

mkHsExplicitSigType :: EpAnnForallTy
                    -> [LHsTyVarBndr Specificity GhcPs] -> LHsType GhcPs
                    -> HsSigType GhcPs
mkHsExplicitSigType an bndrs body =
  HsSig { sig_ext = noExtField
        , sig_bndrs = mkHsOuterExplicit an bndrs, sig_body = body }

mkHsWildCardBndrs :: thing -> HsWildCardBndrs GhcPs thing
mkHsWildCardBndrs x = HsWC { hswc_body = x
                           , hswc_ext  = noExtField }

mkHsPatSigType :: EpAnn EpaLocation -> LHsType GhcPs -> HsPatSigType GhcPs
mkHsPatSigType ann x = HsPS { hsps_ext  = ann
                            , hsps_body = x }

mkEmptyWildCardBndrs :: thing -> HsWildCardBndrs GhcRn thing
mkEmptyWildCardBndrs x = HsWC { hswc_body = x
                              , hswc_ext  = [] }

--------------------------------------------------

type instance XUserTyVar    (GhcPass _) = EpAnn [AddEpAnn]
type instance XKindedTyVar  (GhcPass _) = EpAnn [AddEpAnn]

type instance XXTyVarBndr   (GhcPass _) = DataConCantHappen

-- | Return the attached flag
hsTyVarBndrFlag :: HsTyVarBndr flag (GhcPass pass) -> flag
hsTyVarBndrFlag (UserTyVar _ fl _)     = fl
hsTyVarBndrFlag (KindedTyVar _ fl _ _) = fl

-- | Set the attached flag
setHsTyVarBndrFlag :: flag -> HsTyVarBndr flag' (GhcPass pass)
  -> HsTyVarBndr flag (GhcPass pass)
setHsTyVarBndrFlag f (UserTyVar x _ l)     = UserTyVar x f l
setHsTyVarBndrFlag f (KindedTyVar x _ l k) = KindedTyVar x f l k

-- | Do all type variables in this 'LHsQTyVars' come with kind annotations?
hsTvbAllKinded :: LHsQTyVars (GhcPass p) -> Bool
hsTvbAllKinded = all (isHsKindedTyVar . unLoc) . hsQTvExplicit

instance NamedThing (HsTyVarBndr flag GhcRn) where
  getName (UserTyVar _ _ v) = unLoc v
  getName (KindedTyVar _ _ v _) = unLoc v

type instance XForAllTy        (GhcPass _) = NoExtField
type instance XQualTy          (GhcPass _) = NoExtField
type instance XTyVar           (GhcPass _) = EpAnn [AddEpAnn]
type instance XAppTy           (GhcPass _) = NoExtField
type instance XFunTy           (GhcPass _) = EpAnnCO
type instance XListTy          (GhcPass _) = EpAnn AnnParen
type instance XTupleTy         (GhcPass _) = EpAnn AnnParen
type instance XSumTy           (GhcPass _) = EpAnn AnnParen
type instance XOpTy            (GhcPass _) = EpAnn [AddEpAnn]
type instance XParTy           (GhcPass _) = EpAnn AnnParen
type instance XIParamTy        (GhcPass _) = EpAnn [AddEpAnn]
type instance XStarTy          (GhcPass _) = NoExtField
type instance XKindSig         (GhcPass _) = EpAnn [AddEpAnn]

type instance XAppKindTy       (GhcPass _) = SrcSpan -- Where the `@` lives

type instance XSpliceTy        GhcPs = NoExtField
type instance XSpliceTy        GhcRn = NoExtField
type instance XSpliceTy        GhcTc = Kind

type instance XDocTy           (GhcPass _) = EpAnn [AddEpAnn]
type instance XBangTy          (GhcPass _) = EpAnn [AddEpAnn]

type instance XRecTy           GhcPs = EpAnn AnnList
type instance XRecTy           GhcRn = NoExtField
type instance XRecTy           GhcTc = NoExtField

type instance XExplicitListTy  GhcPs = EpAnn [AddEpAnn]
type instance XExplicitListTy  GhcRn = NoExtField
type instance XExplicitListTy  GhcTc = Kind

type instance XExplicitTupleTy GhcPs = EpAnn [AddEpAnn]
type instance XExplicitTupleTy GhcRn = NoExtField
type instance XExplicitTupleTy GhcTc = [Kind]

type instance XTyLit           (GhcPass _) = NoExtField

type instance XWildCardTy      (GhcPass _) = NoExtField

type instance XXType         (GhcPass _) = HsCoreTy


oneDataConHsTy :: HsType GhcRn
oneDataConHsTy = HsTyVar noAnn NotPromoted (noLocA oneDataConName)

manyDataConHsTy :: HsType GhcRn
manyDataConHsTy = HsTyVar noAnn NotPromoted (noLocA manyDataConName)

hsLinear :: a -> HsScaled (GhcPass p) a
hsLinear = HsScaled (HsLinearArrow (HsPct1 noHsTok noHsUniTok))

hsUnrestricted :: a -> HsScaled (GhcPass p) a
hsUnrestricted = HsScaled (HsUnrestrictedArrow noHsUniTok)

isUnrestricted :: HsArrow GhcRn -> Bool
isUnrestricted (arrowToHsType -> L _ (HsTyVar _ _ (L _ n))) = n == manyDataConName
isUnrestricted _ = False

-- | Convert an arrow into its corresponding multiplicity. In essence this
-- erases the information of whether the programmer wrote an explicit
-- multiplicity or a shorthand.
arrowToHsType :: HsArrow GhcRn -> LHsType GhcRn
arrowToHsType (HsUnrestrictedArrow _) = noLocA manyDataConHsTy
arrowToHsType (HsLinearArrow _) = noLocA oneDataConHsTy
arrowToHsType (HsExplicitMult _ p _) = p

instance
      (OutputableBndrId pass) =>
      Outputable (HsArrow (GhcPass pass)) where
  ppr arr = parens (pprHsArrow arr)

-- See #18846
pprHsArrow :: (OutputableBndrId pass) => HsArrow (GhcPass pass) -> SDoc
pprHsArrow (HsUnrestrictedArrow _) = arrow
pprHsArrow (HsLinearArrow _) = lollipop
pprHsArrow (HsExplicitMult _ p _) = mulArrow (ppr p)

type instance XConDeclField  (GhcPass _) = EpAnn [AddEpAnn]
type instance XXConDeclField (GhcPass _) = DataConCantHappen

instance OutputableBndrId p
       => Outputable (ConDeclField (GhcPass p)) where
  ppr (ConDeclField _ fld_n fld_ty _) = ppr fld_n <+> dcolon <+> ppr fld_ty

---------------------
hsWcScopedTvs :: LHsSigWcType GhcRn -> [Name]
-- Get the lexically-scoped type variables of an LHsSigWcType:
--  - the explicitly-given forall'd type variables;
--    see Note [Lexically scoped type variables]
--  - the named wildcards; see Note [Scoping of named wildcards]
-- because they scope in the same way
hsWcScopedTvs sig_wc_ty
  | HsWC { hswc_ext = nwcs, hswc_body = sig_ty }  <- sig_wc_ty
  , L _ (HsSig{sig_bndrs = outer_bndrs}) <- sig_ty
  = nwcs ++ hsLTyVarNames (hsOuterExplicitBndrs outer_bndrs)
    -- See Note [hsScopedTvs and visible foralls]

hsScopedTvs :: LHsSigType GhcRn -> [Name]
-- Same as hsWcScopedTvs, but for a LHsSigType
hsScopedTvs (L _ (HsSig{sig_bndrs = outer_bndrs}))
  = hsLTyVarNames (hsOuterExplicitBndrs outer_bndrs)
    -- See Note [hsScopedTvs and visible foralls]

---------------------
hsTyVarName :: HsTyVarBndr flag (GhcPass p) -> IdP (GhcPass p)
hsTyVarName (UserTyVar _ _ (L _ n))     = n
hsTyVarName (KindedTyVar _ _ (L _ n) _) = n

hsLTyVarName :: LHsTyVarBndr flag (GhcPass p) -> IdP (GhcPass p)
hsLTyVarName = hsTyVarName . unLoc

hsLTyVarNames :: [LHsTyVarBndr flag (GhcPass p)] -> [IdP (GhcPass p)]
hsLTyVarNames = map hsLTyVarName

hsExplicitLTyVarNames :: LHsQTyVars (GhcPass p) -> [IdP (GhcPass p)]
-- Explicit variables only
hsExplicitLTyVarNames qtvs = map hsLTyVarName (hsQTvExplicit qtvs)

hsAllLTyVarNames :: LHsQTyVars GhcRn -> [Name]
-- All variables
hsAllLTyVarNames (HsQTvs { hsq_ext = kvs
                         , hsq_explicit = tvs })
  = kvs ++ hsLTyVarNames tvs

hsLTyVarLocName :: LHsTyVarBndr flag (GhcPass p) -> LocatedN (IdP (GhcPass p))
hsLTyVarLocName (L l a) = L (l2l l) (hsTyVarName a)

hsLTyVarLocNames :: LHsQTyVars (GhcPass p) -> [LocatedN (IdP (GhcPass p))]
hsLTyVarLocNames qtvs = map hsLTyVarLocName (hsQTvExplicit qtvs)

-- | Get the kind signature of a type, ignoring parentheses:
--
--   hsTyKindSig   `Maybe                    `   =   Nothing
--   hsTyKindSig   `Maybe ::   Type -> Type  `   =   Just  `Type -> Type`
--   hsTyKindSig   `Maybe :: ((Type -> Type))`   =   Just  `Type -> Type`
--
-- This is used to extract the result kind of type synonyms with a CUSK:
--
--  type S = (F :: res_kind)
--                 ^^^^^^^^
--
hsTyKindSig :: LHsType (GhcPass p) -> Maybe (LHsKind (GhcPass p))
hsTyKindSig lty =
  case unLoc lty of
    HsParTy _ lty'    -> hsTyKindSig lty'
    HsKindSig _ _ k   -> Just k
    _                 -> Nothing

---------------------
ignoreParens :: LHsType (GhcPass p) -> LHsType (GhcPass p)
ignoreParens (L _ (HsParTy _ ty)) = ignoreParens ty
ignoreParens ty                   = ty

{-
************************************************************************
*                                                                      *
                Building types
*                                                                      *
************************************************************************
-}

mkAnonWildCardTy :: HsType GhcPs
mkAnonWildCardTy = HsWildCardTy noExtField

mkHsOpTy :: (Anno (IdGhcP p) ~ SrcSpanAnnN)
         => PromotionFlag
         -> LHsType (GhcPass p) -> LocatedN (IdP (GhcPass p))
         -> LHsType (GhcPass p) -> HsType (GhcPass p)
mkHsOpTy prom ty1 op ty2 = HsOpTy noAnn prom ty1 op ty2

mkHsAppTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
mkHsAppTy t1 t2
  = addCLocAA t1 t2 (HsAppTy noExtField t1 (parenthesizeHsType appPrec t2))

mkHsAppTys :: LHsType (GhcPass p) -> [LHsType (GhcPass p)]
           -> LHsType (GhcPass p)
mkHsAppTys = foldl' mkHsAppTy

mkHsAppKindTy :: XAppKindTy (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
              -> LHsType (GhcPass p)
mkHsAppKindTy ext ty k
  = addCLocAA ty k (HsAppKindTy ext ty k)

{-
************************************************************************
*                                                                      *
                Decomposing HsTypes
*                                                                      *
************************************************************************
-}

---------------------------------
-- splitHsFunType decomposes a type (t1 -> t2 ... -> tn)
-- Breaks up any parens in the result type:
--      splitHsFunType (a -> (b -> c)) = ([a,b], c)
-- It returns API Annotations for any parens removed
splitHsFunType ::
     LHsType (GhcPass p)
  -> ( [AddEpAnn], EpAnnComments -- The locations of any parens and
                                  -- comments discarded
     , [HsScaled (GhcPass p) (LHsType (GhcPass p))], LHsType (GhcPass p))
splitHsFunType ty = go ty
  where
    go (L l (HsParTy an ty))
      = let
          (anns, cs, args, res) = splitHsFunType ty
          anns' = anns ++ annParen2AddEpAnn an
          cs' = cs S.<> epAnnComments (ann l) S.<> epAnnComments an
        in (anns', cs', args, res)

    go (L ll (HsFunTy (EpAnn _ _ cs) mult x y))
      | (anns, csy, args, res) <- splitHsFunType y
      = (anns, csy S.<> epAnnComments (ann ll), HsScaled mult x':args, res)
      where
        L l t = x
        x' = L (addCommentsToSrcAnn l cs) t

    go other = ([], emptyComments, [], other)

-- | Retrieve the name of the \"head\" of a nested type application.
-- This is somewhat like @GHC.Tc.Gen.HsType.splitHsAppTys@, but a little more
-- thorough. The purpose of this function is to examine instance heads, so it
-- doesn't handle *all* cases (like lists, tuples, @(~)@, etc.).
hsTyGetAppHead_maybe :: (Anno (IdGhcP p) ~ SrcSpanAnnN)
                     => LHsType (GhcPass p)
                     -> Maybe (LocatedN (IdP (GhcPass p)))
hsTyGetAppHead_maybe = go
  where
    go (L _ (HsTyVar _ _ ln))          = Just ln
    go (L _ (HsAppTy _ l _))           = go l
    go (L _ (HsAppKindTy _ t _))       = go t
    go (L _ (HsOpTy _ _ _ ln _))       = Just ln
    go (L _ (HsParTy _ t))             = go t
    go (L _ (HsKindSig _ t _))         = go t
    go _                               = Nothing

------------------------------------------------------------

-- | Compute the 'SrcSpan' associated with an 'LHsTypeArg'.
lhsTypeArgSrcSpan :: LHsTypeArg (GhcPass pass) -> SrcSpan
lhsTypeArgSrcSpan arg = case arg of
  HsValArg  tm    -> getLocA tm
  HsTypeArg at ty -> at `combineSrcSpans` getLocA ty
  HsArgPar  sp    -> sp

--------------------------------

-- | Decompose a pattern synonym type signature into its constituent parts.
--
-- Note that this function looks through parentheses, so it will work on types
-- such as @(forall a. <...>)@. The downside to this is that it is not
-- generally possible to take the returned types and reconstruct the original
-- type (parentheses and all) from them.
splitLHsPatSynTy ::
     LHsSigType (GhcPass p)
  -> ( [LHsTyVarBndr Specificity (GhcPass (NoGhcTcPass p))] -- universals
     , Maybe (LHsContext (GhcPass p))                       -- required constraints
     , [LHsTyVarBndr Specificity (GhcPass p)]               -- existentials
     , Maybe (LHsContext (GhcPass p))                       -- provided constraints
     , LHsType (GhcPass p))                                 -- body type
splitLHsPatSynTy ty = (univs, reqs, exis, provs, ty4)
  where
    -- split_sig_ty ::
    --      LHsSigType (GhcPass p)
    --   -> ([LHsTyVarBndr Specificity (GhcPass (NoGhcTcPass p))], LHsType (GhcPass p))
    split_sig_ty (L _ HsSig{sig_bndrs = outer_bndrs, sig_body = body}) =
      case outer_bndrs of
        -- NB: Use ignoreParens here in order to be consistent with the use of
        -- splitLHsForAllTyInvis below, which also looks through parentheses.
        HsOuterImplicit{}                      -> ([], ignoreParens body)
        HsOuterExplicit{hso_bndrs = exp_bndrs} -> (exp_bndrs, body)

    (univs,       ty1) = split_sig_ty ty
    (reqs,        ty2) = splitLHsQualTy ty1
    ((_an, exis), ty3) = splitLHsForAllTyInvis ty2
    (provs,       ty4) = splitLHsQualTy ty3

-- | Decompose a sigma type (of the form @forall <tvs>. context => body@)
-- into its constituent parts.
-- Only splits type variable binders that were
-- quantified invisibly (e.g., @forall a.@, with a dot).
--
-- This function is used to split apart certain types, such as instance
-- declaration types, which disallow visible @forall@s. For instance, if GHC
-- split apart the @forall@ in @instance forall a -> Show (Blah a)@, then that
-- declaration would mistakenly be accepted!
--
-- Note that this function looks through parentheses, so it will work on types
-- such as @(forall a. <...>)@. The downside to this is that it is not
-- generally possible to take the returned types and reconstruct the original
-- type (parentheses and all) from them.
splitLHsSigmaTyInvis :: LHsType (GhcPass p)
                     -> ([LHsTyVarBndr Specificity (GhcPass p)]
                        , Maybe (LHsContext (GhcPass p)), LHsType (GhcPass p))
splitLHsSigmaTyInvis ty
  | ((_an,tvs), ty1) <- splitLHsForAllTyInvis ty
  , (ctxt,      ty2) <- splitLHsQualTy ty1
  = (tvs, ctxt, ty2)

-- | Decompose a GADT type into its constituent parts.
-- Returns @(outer_bndrs, mb_ctxt, body)@, where:
--
-- * @outer_bndrs@ are 'HsOuterExplicit' if the type has explicit, outermost
--   type variable binders. Otherwise, they are 'HsOuterImplicit'.
--
-- * @mb_ctxt@ is @Just@ the context, if it is provided.
--   Otherwise, it is @Nothing@.
--
-- * @body@ is the body of the type after the optional @forall@s and context.
--
-- This function is careful not to look through parentheses.
-- See @Note [GADT abstract syntax] (Wrinkle: No nested foralls or contexts)@
-- "GHC.Hs.Decls" for why this is important.
splitLHsGadtTy ::
     LHsSigType GhcPs
  -> (HsOuterSigTyVarBndrs GhcPs, Maybe (LHsContext GhcPs), LHsType GhcPs)
splitLHsGadtTy (L _ sig_ty)
  | (outer_bndrs, rho_ty) <- split_bndrs sig_ty
  , (mb_ctxt, tau_ty)     <- splitLHsQualTy_KP rho_ty
  = (outer_bndrs, mb_ctxt, tau_ty)
  where
    split_bndrs :: HsSigType GhcPs -> (HsOuterSigTyVarBndrs GhcPs, LHsType GhcPs)
    split_bndrs (HsSig{sig_bndrs = outer_bndrs, sig_body = body_ty}) =
      (outer_bndrs, body_ty)

-- | Decompose a type of the form @forall <tvs>. body@ into its constituent
-- parts. Only splits type variable binders that
-- were quantified invisibly (e.g., @forall a.@, with a dot).
--
-- This function is used to split apart certain types, such as instance
-- declaration types, which disallow visible @forall@s. For instance, if GHC
-- split apart the @forall@ in @instance forall a -> Show (Blah a)@, then that
-- declaration would mistakenly be accepted!
--
-- Note that this function looks through parentheses, so it will work on types
-- such as @(forall a. <...>)@. The downside to this is that it is not
-- generally possible to take the returned types and reconstruct the original
-- type (parentheses and all) from them.
-- Unlike 'splitLHsSigmaTyInvis', this function does not look through
-- parentheses, hence the suffix @_KP@ (short for \"Keep Parentheses\").
splitLHsForAllTyInvis ::
  LHsType (GhcPass pass) -> ( (EpAnnForallTy, [LHsTyVarBndr Specificity (GhcPass pass)])
                            , LHsType (GhcPass pass))
splitLHsForAllTyInvis ty
  | ((mb_tvbs), body) <- splitLHsForAllTyInvis_KP (ignoreParens ty)
  = (fromMaybe (EpAnnNotUsed,[]) mb_tvbs, body)

-- | Decompose a type of the form @forall <tvs>. body@ into its constituent
-- parts. Only splits type variable binders that
-- were quantified invisibly (e.g., @forall a.@, with a dot).
--
-- This function is used to split apart certain types, such as instance
-- declaration types, which disallow visible @forall@s. For instance, if GHC
-- split apart the @forall@ in @instance forall a -> Show (Blah a)@, then that
-- declaration would mistakenly be accepted!
--
-- Unlike 'splitLHsForAllTyInvis', this function does not look through
-- parentheses, hence the suffix @_KP@ (short for \"Keep Parentheses\").
splitLHsForAllTyInvis_KP ::
  LHsType (GhcPass pass) -> (Maybe (EpAnnForallTy, [LHsTyVarBndr Specificity (GhcPass pass)])
                            , LHsType (GhcPass pass))
splitLHsForAllTyInvis_KP lty@(L _ ty) =
  case ty of
    HsForAllTy { hst_tele = HsForAllInvis { hsf_xinvis = an
                                          , hsf_invis_bndrs = tvs }
               , hst_body = body }
      -> (Just (an, tvs), body)
    _ -> (Nothing, lty)

-- | Decompose a type of the form @context => body@ into its constituent parts.
--
-- Note that this function looks through parentheses, so it will work on types
-- such as @(context => <...>)@. The downside to this is that it is not
-- generally possible to take the returned types and reconstruct the original
-- type (parentheses and all) from them.
splitLHsQualTy :: LHsType (GhcPass pass)
               -> (Maybe (LHsContext (GhcPass pass)), LHsType (GhcPass pass))
splitLHsQualTy ty
  | (mb_ctxt, body) <- splitLHsQualTy_KP (ignoreParens ty)
  = (mb_ctxt, body)

-- | Decompose a type of the form @context => body@ into its constituent parts.
--
-- Unlike 'splitLHsQualTy', this function does not look through
-- parentheses, hence the suffix @_KP@ (short for \"Keep Parentheses\").
splitLHsQualTy_KP :: LHsType (GhcPass pass) -> (Maybe (LHsContext (GhcPass pass)), LHsType (GhcPass pass))
splitLHsQualTy_KP (L _ (HsQualTy { hst_ctxt = ctxt, hst_body = body }))
                       = (Just ctxt, body)
splitLHsQualTy_KP body = (Nothing, body)

-- | Decompose a type class instance type (of the form
-- @forall <tvs>. context => instance_head@) into its constituent parts.
-- Note that the @[Name]@s returned correspond to either:
--
-- * The implicitly bound type variables (if the type lacks an outermost
--   @forall@), or
--
-- * The explicitly bound type variables (if the type has an outermost
--   @forall@).
--
-- This function is careful not to look through parentheses.
-- See @Note [No nested foralls or contexts in instance types]@
-- for why this is important.
splitLHsInstDeclTy :: LHsSigType GhcRn
                   -> ([Name], Maybe (LHsContext GhcRn), LHsType GhcRn)
splitLHsInstDeclTy (L _ (HsSig{sig_bndrs = outer_bndrs, sig_body = inst_ty})) =
  (hsOuterTyVarNames outer_bndrs, mb_cxt, body_ty)
  where
    (mb_cxt, body_ty) = splitLHsQualTy_KP inst_ty

-- | Decompose a type class instance type (of the form
-- @forall <tvs>. context => instance_head@) into the @instance_head@.
getLHsInstDeclHead :: LHsSigType (GhcPass p) -> LHsType (GhcPass p)
getLHsInstDeclHead (L _ (HsSig{sig_body = qual_ty}))
  | (_mb_cxt, body_ty) <- splitLHsQualTy_KP qual_ty
  = body_ty

-- | Decompose a type class instance type (of the form
-- @forall <tvs>. context => instance_head@) into the @instance_head@ and
-- retrieve the underlying class type constructor (if it exists).
getLHsInstDeclClass_maybe :: (Anno (IdGhcP p) ~ SrcSpanAnnN)
                          => LHsSigType (GhcPass p)
                          -> Maybe (LocatedN (IdP (GhcPass p)))
-- Works on (LHsSigType GhcPs)
getLHsInstDeclClass_maybe inst_ty
  = do { let head_ty = getLHsInstDeclHead inst_ty
       ; hsTyGetAppHead_maybe head_ty
       }

{-
Note [No nested foralls or contexts in instance types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The type at the top of an instance declaration is one of the few places in GHC
where nested `forall`s or contexts are not permitted, even with RankNTypes
enabled. For example, the following will be rejected:

  instance forall a. forall b. Show (Either a b) where ...
  instance Eq a => Eq b => Show (Either a b) where ...
  instance (forall a. Show (Maybe a)) where ...
  instance (Eq a => Show (Maybe a)) where ...

This restriction is partly motivated by an unusual quirk of instance
declarations. Namely, if ScopedTypeVariables is enabled, then the type
variables from the top of an instance will scope over the bodies of the
instance methods, /even if the type variables are implicitly quantified/.
For example, GHC will accept the following:

  instance Monoid a => Monoid (Identity a) where
    mempty = Identity (mempty @a)

Moreover, the type in the top of an instance declaration must obey the
forall-or-nothing rule (see Note [forall-or-nothing rule]).
If instance types allowed nested `forall`s, this could
result in some strange interactions. For example, consider the following:

  class C a where
    m :: Proxy a
  instance (forall a. C (Either a b)) where
    m = Proxy @(Either a b)

Somewhat surprisingly, old versions of GHC would accept the instance above.
Even though the `forall` only quantifies `a`, the outermost parentheses mean
that the `forall` is nested, and per the forall-or-nothing rule, this means
that implicit quantification would occur. Therefore, the `a` is explicitly
bound and the `b` is implicitly bound. Moreover, ScopedTypeVariables would
bring /both/ sorts of type variables into scope over the body of `m`.
How utterly confusing!

To avoid this sort of confusion, we simply disallow nested `forall`s in
instance types, which makes things like the instance above become illegal.
For the sake of consistency, we also disallow nested contexts, even though they
don't have the same strange interaction with ScopedTypeVariables.

Just as we forbid nested `forall`s and contexts in normal instance
declarations, we also forbid them in SPECIALISE instance pragmas (#18455).
Unlike normal instance declarations, ScopedTypeVariables don't have any impact
on SPECIALISE instance pragmas, but we use the same validity checks for
SPECIALISE instance pragmas anyway to be consistent.

-----
-- Wrinkle: Derived instances
-----

`deriving` clauses and standalone `deriving` declarations also permit bringing
type variables into scope, either through explicit or implicit quantification.
Unlike in the tops of instance declarations, however, one does not need to
enable ScopedTypeVariables for this to take effect.

Just as GHC forbids nested `forall`s in the top of instance declarations, it
also forbids them in types involved with `deriving`:

1. In the `via` types in DerivingVia. For example, this is rejected:

     deriving via (forall x. V x) instance C (S x)

   Just like the types in instance declarations, `via` types can also bring
   both implicitly and explicitly bound type variables into scope. As a result,
   we adopt the same no-nested-`forall`s rule in `via` types to avoid confusing
   behavior like in the example below:

     deriving via (forall x. T x y) instance W x y (Foo a b)
     -- Both x and y are brought into scope???
2. In the classes in `deriving` clauses. For example, this is rejected:

     data T = MkT deriving (C1, (forall x. C2 x y))

   This is because the generated instance would look like:

     instance forall x y. C2 x y T where ...

   So really, the same concerns as instance declarations apply here as well.
-}

{-
************************************************************************
*                                                                      *
                FieldOcc
*                                                                      *
************************************************************************
-}

type instance XCFieldOcc GhcPs = NoExtField
type instance XCFieldOcc GhcRn = Name
type instance XCFieldOcc GhcTc = Id

type instance XXFieldOcc (GhcPass _) = DataConCantHappen

mkFieldOcc :: LocatedN RdrName -> FieldOcc GhcPs
mkFieldOcc rdr = FieldOcc noExtField rdr


type instance XUnambiguous GhcPs = NoExtField
type instance XUnambiguous GhcRn = Name
type instance XUnambiguous GhcTc = Id

type instance XAmbiguous GhcPs = NoExtField
type instance XAmbiguous GhcRn = NoExtField
type instance XAmbiguous GhcTc = Id

type instance XXAmbiguousFieldOcc (GhcPass _) = DataConCantHappen

instance Outputable (AmbiguousFieldOcc (GhcPass p)) where
  ppr = ppr . rdrNameAmbiguousFieldOcc

instance OutputableBndr (AmbiguousFieldOcc (GhcPass p)) where
  pprInfixOcc  = pprInfixOcc . rdrNameAmbiguousFieldOcc
  pprPrefixOcc = pprPrefixOcc . rdrNameAmbiguousFieldOcc

instance OutputableBndr (Located (AmbiguousFieldOcc (GhcPass p))) where
  pprInfixOcc  = pprInfixOcc . unLoc
  pprPrefixOcc = pprPrefixOcc . unLoc

mkAmbiguousFieldOcc :: LocatedN RdrName -> AmbiguousFieldOcc GhcPs
mkAmbiguousFieldOcc rdr = Unambiguous noExtField rdr

rdrNameAmbiguousFieldOcc :: AmbiguousFieldOcc (GhcPass p) -> RdrName
rdrNameAmbiguousFieldOcc (Unambiguous _ (L _ rdr)) = rdr
rdrNameAmbiguousFieldOcc (Ambiguous   _ (L _ rdr)) = rdr

selectorAmbiguousFieldOcc :: AmbiguousFieldOcc GhcTc -> Id
selectorAmbiguousFieldOcc (Unambiguous sel _) = sel
selectorAmbiguousFieldOcc (Ambiguous   sel _) = sel

unambiguousFieldOcc :: AmbiguousFieldOcc GhcTc -> FieldOcc GhcTc
unambiguousFieldOcc (Unambiguous rdr sel) = FieldOcc rdr sel
unambiguousFieldOcc (Ambiguous   rdr sel) = FieldOcc rdr sel

ambiguousFieldOcc :: FieldOcc GhcTc -> AmbiguousFieldOcc GhcTc
ambiguousFieldOcc (FieldOcc sel rdr) = Unambiguous sel rdr

{-
************************************************************************
*                                                                      *
\subsection{Pretty printing}
*                                                                      *
************************************************************************
-}

class OutputableBndrFlag flag p where
    pprTyVarBndr :: OutputableBndrId p => HsTyVarBndr flag (GhcPass p) -> SDoc

instance OutputableBndrFlag () p where
    pprTyVarBndr (UserTyVar _ _ n)     = ppr n
    pprTyVarBndr (KindedTyVar _ _ n k) = parens $ hsep [ppr n, dcolon, ppr k]

instance OutputableBndrFlag Specificity p where
    pprTyVarBndr (UserTyVar _ SpecifiedSpec n)     = ppr n
    pprTyVarBndr (UserTyVar _ InferredSpec n)      = braces $ ppr n
    pprTyVarBndr (KindedTyVar _ SpecifiedSpec n k) = parens $ hsep [ppr n, dcolon, ppr k]
    pprTyVarBndr (KindedTyVar _ InferredSpec n k)  = braces $ hsep [ppr n, dcolon, ppr k]

instance OutputableBndrId p => Outputable (HsSigType (GhcPass p)) where
    ppr (HsSig { sig_bndrs = outer_bndrs, sig_body = body }) =
      pprHsOuterSigTyVarBndrs outer_bndrs <+> ppr body

instance OutputableBndrId p => Outputable (HsType (GhcPass p)) where
    ppr ty = pprHsType ty

instance OutputableBndrId p
       => Outputable (LHsQTyVars (GhcPass p)) where
    ppr (HsQTvs { hsq_explicit = tvs }) = interppSP tvs

instance (OutputableBndrFlag flag p,
          OutputableBndrFlag flag (NoGhcTcPass p),
          OutputableBndrId p)
       => Outputable (HsOuterTyVarBndrs flag (GhcPass p)) where
    ppr (HsOuterImplicit{hso_ximplicit = imp_tvs}) =
      text "HsOuterImplicit:" <+> case ghcPass @p of
        GhcPs -> ppr imp_tvs
        GhcRn -> ppr imp_tvs
        GhcTc -> ppr imp_tvs
    ppr (HsOuterExplicit{hso_bndrs = exp_tvs}) =
      text "HsOuterExplicit:" <+> ppr exp_tvs

instance OutputableBndrId p
       => Outputable (HsForAllTelescope (GhcPass p)) where
    ppr (HsForAllVis { hsf_vis_bndrs = bndrs }) =
      text "HsForAllVis:" <+> ppr bndrs
    ppr (HsForAllInvis { hsf_invis_bndrs = bndrs }) =
      text "HsForAllInvis:" <+> ppr bndrs

instance (OutputableBndrId p, OutputableBndrFlag flag p)
       => Outputable (HsTyVarBndr flag (GhcPass p)) where
    ppr = pprTyVarBndr

instance Outputable thing
       => Outputable (HsWildCardBndrs (GhcPass p) thing) where
    ppr (HsWC { hswc_body = ty }) = ppr ty

instance (OutputableBndrId p)
       => Outputable (HsPatSigType (GhcPass p)) where
    ppr (HsPS { hsps_body = ty }) = ppr ty

pprAnonWildCard :: SDoc
pprAnonWildCard = char '_'

-- | Prints the explicit @forall@ in a type family equation if one is written.
-- If there is no explicit @forall@, nothing is printed.
pprHsOuterFamEqnTyVarBndrs :: OutputableBndrId p
                           => HsOuterFamEqnTyVarBndrs (GhcPass p) -> SDoc
pprHsOuterFamEqnTyVarBndrs (HsOuterImplicit{}) = empty
pprHsOuterFamEqnTyVarBndrs (HsOuterExplicit{hso_bndrs = qtvs}) =
  forAllLit <+> interppSP qtvs <> dot

-- | Prints the outermost @forall@ in a type signature if one is written.
-- If there is no outermost @forall@, nothing is printed.
pprHsOuterSigTyVarBndrs :: OutputableBndrId p
                        => HsOuterSigTyVarBndrs (GhcPass p) -> SDoc
pprHsOuterSigTyVarBndrs (HsOuterImplicit{}) = empty
pprHsOuterSigTyVarBndrs (HsOuterExplicit{hso_bndrs = bndrs}) =
  pprHsForAll (mkHsForAllInvisTele noAnn bndrs) Nothing

-- | Prints a forall; When passed an empty list, prints @forall .@/@forall ->@
-- only when @-dppr-debug@ is enabled.
pprHsForAll :: forall p. OutputableBndrId p
            => HsForAllTelescope (GhcPass p)
            -> Maybe (LHsContext (GhcPass p)) -> SDoc
pprHsForAll tele cxt
  = pp_tele tele <+> pprLHsContext cxt
  where
    pp_tele :: HsForAllTelescope (GhcPass p) -> SDoc
    pp_tele tele = case tele of
      HsForAllVis   { hsf_vis_bndrs   = qtvs } -> pp_forall (space <> arrow) qtvs
      HsForAllInvis { hsf_invis_bndrs = qtvs } -> pp_forall dot qtvs

    pp_forall :: forall flag p. (OutputableBndrId p, OutputableBndrFlag flag p)
              => SDoc -> [LHsTyVarBndr flag (GhcPass p)] -> SDoc
    pp_forall separator qtvs
      | null qtvs = whenPprDebug (forAllLit <> separator)
  -- Note: to fix the PprRecordDotSyntax1 ppr roundtrip test, the <>
  -- below needs to be <+>. But it means 94 other test results need to
  -- be updated to match.
      | otherwise = forAllLit <+> interppSP qtvs <> separator

pprLHsContext :: (OutputableBndrId p)
              => Maybe (LHsContext (GhcPass p)) -> SDoc
pprLHsContext Nothing = empty
pprLHsContext (Just lctxt) = pprLHsContextAlways lctxt

-- For use in a HsQualTy, which always gets printed if it exists.
pprLHsContextAlways :: (OutputableBndrId p)
                    => LHsContext (GhcPass p) -> SDoc
pprLHsContextAlways (L _ ctxt)
  = case ctxt of
      []       -> parens empty             <+> darrow
      [L _ ty] -> ppr_mono_ty ty           <+> darrow
      _        -> parens (interpp'SP ctxt) <+> darrow

pprConDeclFields :: OutputableBndrId p
                 => [LConDeclField (GhcPass p)] -> SDoc
pprConDeclFields fields = braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (L _ (ConDeclField { cd_fld_names = ns, cd_fld_type = ty,
                                 cd_fld_doc = doc }))
        = pprMaybeWithDoc doc (ppr_names ns <+> dcolon <+> ppr ty)

    ppr_names :: [LFieldOcc (GhcPass p)] -> SDoc
    ppr_names [n] = pprPrefixOcc n
    ppr_names ns = sep (punctuate comma (map pprPrefixOcc ns))

{-
Note [Printing KindedTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3830 reminded me that we should really only print the kind
signature on a KindedTyVar if the kind signature was put there by the
programmer.  During kind inference GHC now adds a PostTcKind to UserTyVars,
rather than converting to KindedTyVars as before.

(As it happens, the message in #3830 comes out a different way now,
and the problem doesn't show up; but having the flag on a KindedTyVar
seems like the Right Thing anyway.)
-}

-- Printing works more-or-less as for Types

pprHsType :: (OutputableBndrId p) => HsType (GhcPass p) -> SDoc
pprHsType ty = ppr_mono_ty ty

ppr_mono_lty :: OutputableBndrId p
             => LHsType (GhcPass p) -> SDoc
ppr_mono_lty ty = ppr_mono_ty (unLoc ty)

ppr_mono_ty :: (OutputableBndrId p) => HsType (GhcPass p) -> SDoc
ppr_mono_ty (HsForAllTy { hst_tele = tele, hst_body = ty })
  = sep [pprHsForAll tele Nothing, ppr_mono_lty ty]

ppr_mono_ty (HsQualTy { hst_ctxt = ctxt, hst_body = ty })
  = sep [pprLHsContextAlways ctxt, ppr_mono_lty ty]

ppr_mono_ty (HsBangTy _ b ty)           = ppr b <> ppr_mono_lty ty
ppr_mono_ty (HsRecTy _ flds)            = pprConDeclFields flds
ppr_mono_ty (HsTyVar _ prom (L _ name)) = pprOccWithTick Prefix prom name
ppr_mono_ty (HsFunTy _ mult ty1 ty2)    = ppr_fun_ty mult ty1 ty2
ppr_mono_ty (HsTupleTy _ con tys)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `Solo x`, not `(x)`
  | [ty] <- tys
  , BoxedTuple <- std_con
  = sep [text (mkTupleStr Boxed 1), ppr_mono_lty ty]
  | otherwise
  = tupleParens std_con (pprWithCommas ppr tys)
  where std_con = case con of
                    HsUnboxedTuple -> UnboxedTuple
                    _              -> BoxedTuple
ppr_mono_ty (HsSumTy _ tys)
  = tupleParens UnboxedTuple (pprWithBars ppr tys)
ppr_mono_ty (HsKindSig _ ty kind)
  = ppr_mono_lty ty <+> dcolon <+> ppr kind
ppr_mono_ty (HsListTy _ ty)       = brackets (ppr_mono_lty ty)
ppr_mono_ty (HsIParamTy _ n ty)   = (ppr n <+> dcolon <+> ppr_mono_lty ty)
ppr_mono_ty (HsSpliceTy _ s)      = pprSplice s
ppr_mono_ty (HsExplicitListTy _ prom tys)
  | isPromoted prom = quote $ brackets (maybeAddSpace tys $ interpp'SP tys)
  | otherwise       = brackets (interpp'SP tys)
ppr_mono_ty (HsExplicitTupleTy _ tys)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `'Solo x`, not `'(x)`
  | [ty] <- tys
  = quote $ sep [text (mkTupleStr Boxed 1), ppr_mono_lty ty]
  | otherwise
  = quote $ parens (maybeAddSpace tys $ interpp'SP tys)
ppr_mono_ty (HsTyLit _ t)       = ppr t
ppr_mono_ty (HsWildCardTy {})   = char '_'

ppr_mono_ty (HsStarTy _ isUni)  = char (if isUni then '★' else '*')

ppr_mono_ty (HsAppTy _ fun_ty arg_ty)
  = hsep [ppr_mono_lty fun_ty, ppr_mono_lty arg_ty]
ppr_mono_ty (HsAppKindTy _ ty k)
  = ppr_mono_lty ty <+> char '@' <> ppr_mono_lty k
ppr_mono_ty (HsOpTy _ prom ty1 (L _ op) ty2)
  = sep [ ppr_mono_lty ty1
        , sep [pprOccWithTick Infix prom op, ppr_mono_lty ty2 ] ]
ppr_mono_ty (HsParTy _ ty)
  = parens (ppr_mono_lty ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --    toHsType doesn't put in any HsParTys, so we may still need them

ppr_mono_ty (HsDocTy _ ty doc)
  = pprWithDoc doc $ ppr_mono_lty ty

ppr_mono_ty (XHsType t) = ppr t

--------------------------
ppr_fun_ty :: (OutputableBndrId p)
           => HsArrow (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p) -> SDoc
ppr_fun_ty mult ty1 ty2
  = let p1 = ppr_mono_lty ty1
        p2 = ppr_mono_lty ty2
        arr = pprHsArrow mult
    in
    sep [p1, arr <+> p2]

--------------------------
-- | @'hsTypeNeedsParens' p t@ returns 'True' if the type @t@ needs parentheses
-- under precedence @p@.
hsTypeNeedsParens :: PprPrec -> HsType (GhcPass p) -> Bool
hsTypeNeedsParens p = go_hs_ty
  where
    go_hs_ty (HsForAllTy{})           = p >= funPrec
    go_hs_ty (HsQualTy{})             = p >= funPrec
    go_hs_ty (HsBangTy{})             = p > topPrec
    go_hs_ty (HsRecTy{})              = False
    go_hs_ty (HsTyVar{})              = False
    go_hs_ty (HsFunTy{})              = p >= funPrec
    -- Special-case unary boxed tuple applications so that they are
    -- parenthesized as `Identity (Solo x)`, not `Identity Solo x` (#18612)
    -- See Note [One-tuples] in GHC.Builtin.Types
    go_hs_ty (HsTupleTy _ con [_])
      = case con of
          HsBoxedOrConstraintTuple   -> p >= appPrec
          HsUnboxedTuple             -> False
    go_hs_ty (HsTupleTy{})            = False
    go_hs_ty (HsSumTy{})              = False
    go_hs_ty (HsKindSig{})            = p >= sigPrec
    go_hs_ty (HsListTy{})             = False
    go_hs_ty (HsIParamTy{})           = p > topPrec
    go_hs_ty (HsSpliceTy{})           = False
    go_hs_ty (HsExplicitListTy{})     = False
    -- Special-case unary boxed tuple applications so that they are
    -- parenthesized as `Proxy ('Solo x)`, not `Proxy 'Solo x` (#18612)
    -- See Note [One-tuples] in GHC.Builtin.Types
    go_hs_ty (HsExplicitTupleTy _ [_])
                                      = p >= appPrec
    go_hs_ty (HsExplicitTupleTy{})    = False
    go_hs_ty (HsTyLit{})              = False
    go_hs_ty (HsWildCardTy{})         = False
    go_hs_ty (HsStarTy{})             = p >= starPrec
    go_hs_ty (HsAppTy{})              = p >= appPrec
    go_hs_ty (HsAppKindTy{})          = p >= appPrec
    go_hs_ty (HsOpTy{})               = p >= opPrec
    go_hs_ty (HsParTy{})              = False
    go_hs_ty (HsDocTy _ (L _ t) _)    = go_hs_ty t
    go_hs_ty (XHsType ty)             = go_core_ty ty

    go_core_ty (TyVarTy{})    = False
    go_core_ty (AppTy{})      = p >= appPrec
    go_core_ty (TyConApp _ args)
      | null args             = False
      | otherwise             = p >= appPrec
    go_core_ty (ForAllTy{})   = p >= funPrec
    go_core_ty (FunTy{})      = p >= funPrec
    go_core_ty (LitTy{})      = False
    go_core_ty (CastTy t _)   = go_core_ty t
    go_core_ty (CoercionTy{}) = False

maybeAddSpace :: [LHsType (GhcPass p)] -> SDoc -> SDoc
-- See Note [Printing promoted type constructors]
-- in GHC.Iface.Type.  This code implements the same
-- logic for printing HsType
maybeAddSpace tys doc
  | (ty : _) <- tys
  , lhsTypeHasLeadingPromotionQuote ty = space <> doc
  | otherwise                          = doc

lhsTypeHasLeadingPromotionQuote :: LHsType (GhcPass p) -> Bool
lhsTypeHasLeadingPromotionQuote ty
  = goL ty
  where
    goL (L _ ty) = go ty

    go (HsForAllTy{})        = False
    go (HsQualTy{ hst_ctxt = ctxt, hst_body = body})
      | (L _ (c:_)) <- ctxt = goL c
      | otherwise            = goL body
    go (HsBangTy{})          = False
    go (HsRecTy{})           = False
    go (HsTyVar _ p _)       = isPromoted p
    go (HsFunTy _ _ arg _)   = goL arg
    go (HsListTy{})          = False
    go (HsTupleTy{})         = False
    go (HsSumTy{})           = False
    go (HsOpTy _ _ t1 _ _)   = goL t1
    go (HsKindSig _ t _)     = goL t
    go (HsIParamTy{})        = False
    go (HsSpliceTy{})        = False
    go (HsExplicitListTy _ p _) = isPromoted p
    go (HsExplicitTupleTy{}) = True
    go (HsTyLit{})           = False
    go (HsWildCardTy{})      = False
    go (HsStarTy{})          = False
    go (HsAppTy _ t _)       = goL t
    go (HsAppKindTy _ t _)   = goL t
    go (HsParTy{})           = False
    go (HsDocTy _ t _)       = goL t
    go (XHsType{})           = False

-- | @'parenthesizeHsType' p ty@ checks if @'hsTypeNeedsParens' p ty@ is
-- true, and if so, surrounds @ty@ with an 'HsParTy'. Otherwise, it simply
-- returns @ty@.
parenthesizeHsType :: PprPrec -> LHsType (GhcPass p) -> LHsType (GhcPass p)
parenthesizeHsType p lty@(L loc ty)
  | hsTypeNeedsParens p ty = L loc (HsParTy noAnn lty)
  | otherwise              = lty

-- | @'parenthesizeHsContext' p ctxt@ checks if @ctxt@ is a single constraint
-- @c@ such that @'hsTypeNeedsParens' p c@ is true, and if so, surrounds @c@
-- with an 'HsParTy' to form a parenthesized @ctxt@. Otherwise, it simply
-- returns @ctxt@ unchanged.
parenthesizeHsContext :: PprPrec
                      -> LHsContext (GhcPass p) -> LHsContext (GhcPass p)
parenthesizeHsContext p lctxt@(L loc ctxt) =
  case ctxt of
    [c] -> L loc [parenthesizeHsType p c]
    _   -> lctxt -- Other contexts are already "parenthesized" by virtue of
                 -- being tuples.
{-
************************************************************************
*                                                                      *
\subsection{Anno instances}
*                                                                      *
************************************************************************
-}

type instance Anno (BangType (GhcPass p)) = SrcSpanAnnA
type instance Anno [LocatedA (HsType (GhcPass p))] = SrcSpanAnnC
type instance Anno (HsType (GhcPass p)) = SrcSpanAnnA
type instance Anno (HsSigType (GhcPass p)) = SrcSpanAnnA
type instance Anno (HsKind (GhcPass p)) = SrcSpanAnnA

type instance Anno (HsTyVarBndr _flag (GhcPass _)) = SrcSpanAnnA
  -- Explicit pass Anno instances needed because of the NoGhcTc field
type instance Anno (HsTyVarBndr _flag GhcPs) = SrcSpanAnnA
type instance Anno (HsTyVarBndr _flag GhcRn) = SrcSpanAnnA
type instance Anno (HsTyVarBndr _flag GhcTc) = SrcSpanAnnA

type instance Anno (HsOuterTyVarBndrs _ (GhcPass _)) = SrcSpanAnnA
type instance Anno HsIPName = SrcAnn NoEpAnns
type instance Anno (ConDeclField (GhcPass p)) = SrcSpanAnnA

type instance Anno (FieldOcc (GhcPass p)) = SrcAnn NoEpAnns
type instance Anno (AmbiguousFieldOcc (GhcPass p)) = SrcAnn NoEpAnns
