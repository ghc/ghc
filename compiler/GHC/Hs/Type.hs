
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
        Mult,
        HsMultAnn, HsMultAnnOf(..),
        multAnnToHsType, expandHsMultAnnOf,
        EpLinear(..), EpArrowOrColon(..),
        pprHsArrow, pprHsMultAnn,

        HsType(..), HsCoreTy, LHsType, HsKind, LHsKind,
        HsTypeGhcPsExt(..),
        HsForAllTelescope(..), EpAnnForallVis, EpAnnForallInvis,
        HsTyVarBndr(..), LHsTyVarBndr, AnnTyVarBndr(..),
        HsBndrKind(..),
        HsBndrVar(..),
        HsBndrVis(..), isHsBndrInvisible,
        LHsQTyVars(..),
        HsOuterTyVarBndrs(..), HsOuterFamEqnTyVarBndrs, HsOuterSigTyVarBndrs,
        HsWildCardBndrs(..),
        HsPatSigType(..), HsPSRn(..),
        HsTyPat(..), HsTyPatRn(..),
        HsTyPatRnBuilder(..), tpBuilderExplicitTV, tpBuilderPatSig, buildHsTyPatRn, builderFromHsTyPatRn,
        HsSigType(..), LHsSigType, LHsSigWcType, LHsWcType,
        HsTupleSort(..),
        HsContext, LHsContext, fromMaybeContext,
        HsTyLit(..),
        HsIPName(..), hsIPNameFS,
        HsArg(..), numVisibleArgs, pprHsArgsApp,
        LHsTypeArg, lhsTypeArgSrcSpan,
        OutputableBndrFlag,

        HsSrcBang(..), HsImplBang(..),
        SrcStrictness(..), SrcUnpackedness(..),

        HsConDeclRecField(..), LHsConDeclRecField, pprHsConDeclRecFields,

        HsConDetails(..),
        HsConDeclField(..), pprHsConDeclFieldWith, pprHsConDeclFieldNoMult,
        hsPlainTypeField, mkConDeclField,
        FieldOcc(..), LFieldOcc, mkFieldOcc,
        fieldOccRdrName, fieldOccLRdrName,

        OpName(..),

        mkAnonWildCardTy, pprAnonWildCard,

        hsOuterTyVarNames, hsOuterExplicitBndrs, mapHsOuterImplicit,
        mkHsOuterImplicit, mkHsOuterExplicit,
        mkHsImplicitSigType, mkHsExplicitSigType,
        mkHsWildCardBndrs, mkHsPatSigType, mkHsTyPat,
        mkEmptyWildCardBndrs,
        mkHsForAllVisTele, mkHsForAllInvisTele,
        mkHsQTvs, hsQTvExplicit, emptyLHsQTvs,
        isHsKindedTyVar, hsBndrVar, hsBndrKind, hsTvbAllKinded,
        hsScopedTvs, hsScopedKvs, hsWcScopedTvs, dropWildCards,
        hsTyVarLName, hsTyVarName,
        hsAllLTyVarNames, hsLTyVarLocNames,
        hsLTyVarName, hsLTyVarNames,
        hsForAllTelescopeBndrs,
        hsForAllTelescopeNames,
        hsLTyVarLocName, hsExplicitLTyVarNames,
        splitLHsInstDeclTy, getLHsInstDeclHead, getLHsInstDeclClass_maybe,
        splitLHsPatSynTy,
        splitLHsForAllTyInvis, splitLHsForAllTyInvis_KP, splitLHsQualTy,
        splitLHsSigmaTyInvis, splitLHsGadtTy,
        splitHsFunType, hsTyGetAppHead_maybe,
        mkHsOpTy, mkHsAppTy, mkHsAppTys, mkHsAppKindTy,
        ignoreParens, hsSigWcType, hsPatSigType,
        hsTyKindSig,
        setHsTyVarBndrFlag, hsTyVarBndrFlag, updateHsTyVarBndrFlag,

        -- Printing
        pprHsType, pprHsForAll, pprHsForAllTelescope,
        pprHsOuterFamEqnTyVarBndrs, pprHsOuterSigTyVarBndrs,
        pprLHsContext,
        hsTypeNeedsParens, parenthesizeHsType, parenthesizeHsContext
    ) where

import GHC.Prelude

import Language.Haskell.Syntax.Type

import {-# SOURCE #-} GHC.Hs.Expr ( pprUntypedSplice, HsUntypedSpliceResult(..) )

import Language.Haskell.Syntax.Extension
import GHC.Core.DataCon ( SrcStrictness(..), SrcUnpackedness(..)
                        , HsSrcBang(..), HsImplBang(..)
                        )
import GHC.Hs.Extension
import GHC.Parser.Annotation

import GHC.Types.Fixity ( LexicalFixity(..) )
import GHC.Types.SourceText
import GHC.Types.Name
import GHC.Types.Name.Reader ( RdrName, WithUserRdr(..), noUserRdr )
import GHC.Types.Var ( VarBndr, visArgTypeLike )
import GHC.Core.TyCo.Rep ( Type(..) )
import GHC.Builtin.Names ( negateName )
import GHC.Builtin.Types( oneDataConName, mkTupleStr )
import GHC.Core.Ppr ( pprOccWithTick)
import GHC.Core.Type
import GHC.Core.Multiplicity( pprArrowWithMultiplicity )
import GHC.Hs.Doc
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Misc (count)

import Data.Maybe
import Data.Data (Data)

import qualified Data.Semigroup as S
import GHC.Data.Bag

{-
************************************************************************
*                                                                      *
\subsection{Data types}
*                                                                      *
************************************************************************
-}

fromMaybeContext :: Maybe (LHsContext (GhcPass p)) -> HsContext (GhcPass p)
fromMaybeContext mctxt = unLoc $ fromMaybe (noLocA []) mctxt

type instance XHsForAllVis   (GhcPass _) = EpAnn (TokForall, TokRarrow)
                                           -- Location of 'forall' and '->'
type instance XHsForAllInvis (GhcPass _) = EpAnn (TokForall, EpToken ".")
                                           -- Location of 'forall' and '.'

type instance XXHsForAllTelescope (GhcPass _) = DataConCantHappen

type EpAnnForallVis   = EpAnn (TokForall, TokRarrow)
type EpAnnForallInvis = EpAnn (TokForall, EpToken ".")

type HsQTvsRn = [Name]  -- Implicit variables
  -- For example, in   data T (a :: k1 -> k2) = ...
  -- the 'a' is explicit while 'k1', 'k2' are implicit

type instance XHsQTvs GhcPs = NoExtField
type instance XHsQTvs GhcRn = HsQTvsRn
type instance XHsQTvs GhcTc = HsQTvsRn

type instance XXLHsQTyVars  (GhcPass _) = DataConCantHappen

mkHsForAllVisTele ::EpAnnForallVis ->
  [LHsTyVarBndr () (GhcPass p)] -> HsForAllTelescope (GhcPass p)
mkHsForAllVisTele an vis_bndrs =
  HsForAllVis { hsf_xvis = an, hsf_vis_bndrs = vis_bndrs }

mkHsForAllInvisTele :: EpAnnForallInvis
  -> [LHsTyVarBndr Specificity (GhcPass p)] -> HsForAllTelescope (GhcPass p)
mkHsForAllInvisTele an invis_bndrs =
  HsForAllInvis { hsf_xinvis = an, hsf_invis_bndrs = invis_bndrs }

mkHsQTvs :: [LHsTyVarBndr (HsBndrVis GhcPs) GhcPs] -> LHsQTyVars GhcPs
mkHsQTvs tvs = HsQTvs { hsq_ext = noExtField, hsq_explicit = tvs }

emptyLHsQTvs :: LHsQTyVars GhcRn
emptyLHsQTvs = HsQTvs { hsq_ext = [], hsq_explicit = [] }

------------------------------------------------
--            HsOuterTyVarBndrs

type instance XHsOuterImplicit GhcPs = NoExtField
type instance XHsOuterImplicit GhcRn = [Name]
type instance XHsOuterImplicit GhcTc = [TyVar]

type instance XHsOuterExplicit GhcPs _    = EpAnnForallInvis
type instance XHsOuterExplicit GhcRn _    = NoExtField
type instance XHsOuterExplicit GhcTc flag = [VarBndr TyVar flag]

type instance XXHsOuterTyVarBndrs (GhcPass _) = DataConCantHappen

type instance XHsWC              GhcPs b = NoExtField
type instance XHsWC              GhcRn b = [Name]
type instance XHsWC              GhcTc b = [Name]

type instance XXHsWildCardBndrs (GhcPass _) _ = DataConCantHappen

type instance XHsPS GhcPs = EpAnnCO
type instance XHsPS GhcRn = HsPSRn
type instance XHsPS GhcTc = HsPSRn

type instance XHsTP GhcPs = NoExtField
type instance XHsTP GhcRn = HsTyPatRn
type instance XHsTP GhcTc = DataConCantHappen

-- | The extension field for 'HsPatSigType', which is only used in the
-- renamer onwards. See @Note [Pattern signature binders and scoping]@.
data HsPSRn = HsPSRn
  { hsps_nwcs    :: [Name] -- ^ Wildcard names
  , hsps_imp_tvs :: [Name] -- ^ Implicitly bound variable names
  }
  deriving Data

-- HsTyPatRn is the extension field for `HsTyPat`, after renaming
-- E.g. pattern K @(Maybe (_x, a, b::Proxy k)
-- In the type pattern @(Maybe ...):
--    '_x' is a named wildcard
--    'a'  is explicitly bound
--    'k'  is implicitly bound
-- See Note [Implicit and explicit type variable binders] in GHC.Rename.Pat
data HsTyPatRn = HsTPRn
  { hstp_nwcs    :: [Name] -- ^ Wildcard names
  , hstp_imp_tvs :: [Name] -- ^ Implicitly bound variable names
  , hstp_exp_tvs :: [Name] -- ^ Explicitly bound variable names
  }
  deriving Data

-- | A variant of HsTyPatRn that uses Bags for efficient concatenation.
-- See Note [Implicit and explicit type variable binders]  in GHC.Rename.Pat
data HsTyPatRnBuilder =
  HsTPRnB {
    hstpb_nwcs :: Bag Name,
    hstpb_imp_tvs :: Bag Name,
    hstpb_exp_tvs :: Bag Name
  }

tpBuilderExplicitTV :: Name -> HsTyPatRnBuilder
tpBuilderExplicitTV name = mempty {hstpb_exp_tvs = unitBag name}

tpBuilderPatSig :: HsPSRn -> HsTyPatRnBuilder
tpBuilderPatSig HsPSRn {hsps_nwcs, hsps_imp_tvs} =
  mempty {
    hstpb_nwcs = listToBag hsps_nwcs,
    hstpb_imp_tvs = listToBag hsps_imp_tvs
  }

instance Semigroup HsTyPatRnBuilder where
  HsTPRnB nwcs1 imp_tvs1 exptvs1 <> HsTPRnB nwcs2 imp_tvs2 exptvs2 =
    HsTPRnB
      (nwcs1    `unionBags` nwcs2)
      (imp_tvs1 `unionBags` imp_tvs2)
      (exptvs1  `unionBags` exptvs2)

instance Monoid HsTyPatRnBuilder where
  mempty = HsTPRnB emptyBag emptyBag emptyBag

buildHsTyPatRn :: HsTyPatRnBuilder -> HsTyPatRn
buildHsTyPatRn HsTPRnB {hstpb_nwcs, hstpb_imp_tvs, hstpb_exp_tvs} =
  HsTPRn {
    hstp_nwcs =    bagToList hstpb_nwcs,
    hstp_imp_tvs = bagToList hstpb_imp_tvs,
    hstp_exp_tvs = bagToList hstpb_exp_tvs
  }

builderFromHsTyPatRn :: HsTyPatRn -> HsTyPatRnBuilder
builderFromHsTyPatRn HsTPRn{hstp_nwcs, hstp_imp_tvs, hstp_exp_tvs} =
  HsTPRnB {
    hstpb_nwcs =    listToBag hstp_nwcs,
    hstpb_imp_tvs = listToBag hstp_imp_tvs,
    hstpb_exp_tvs = listToBag hstp_exp_tvs
  }

type instance XXHsPatSigType (GhcPass _) = DataConCantHappen
type instance XXHsTyPat      (GhcPass _) = DataConCantHappen

type instance XHsSig (GhcPass _) = NoExtField
type instance XXHsSigType (GhcPass _) = DataConCantHappen


hsPatSigType :: HsPatSigType (GhcPass p) -> LHsType (GhcPass p)
hsPatSigType (HsPS { hsps_body = ty }) = ty

hsSigWcType :: LHsSigWcType (GhcPass p) -> LHsType (GhcPass p)
hsSigWcType = sig_body . unLoc . hswc_body

dropWildCards :: LHsSigWcType (GhcPass p) -> LHsSigType (GhcPass p)
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

mkHsOuterExplicit :: EpAnnForallInvis -> [LHsTyVarBndr flag GhcPs]
                  -> HsOuterTyVarBndrs flag GhcPs
mkHsOuterExplicit an bndrs = HsOuterExplicit { hso_xexplicit = an
                                             , hso_bndrs     = bndrs }

mkHsImplicitSigType :: LHsType GhcPs -> HsSigType GhcPs
mkHsImplicitSigType body =
  HsSig { sig_ext   = noExtField
        , sig_bndrs = mkHsOuterImplicit, sig_body = body }

mkHsExplicitSigType :: EpAnnForallInvis
                    -> [LHsTyVarBndr Specificity GhcPs] -> LHsType GhcPs
                    -> HsSigType GhcPs
mkHsExplicitSigType an bndrs body =
  HsSig { sig_ext = noExtField
        , sig_bndrs = mkHsOuterExplicit an bndrs, sig_body = body }

mkHsWildCardBndrs :: thing -> HsWildCardBndrs GhcPs thing
mkHsWildCardBndrs x = HsWC { hswc_body = x
                           , hswc_ext  = noExtField }

mkHsPatSigType :: EpAnnCO -> LHsType GhcPs -> HsPatSigType GhcPs
mkHsPatSigType ann x = HsPS { hsps_ext  = ann
                            , hsps_body = x }

mkHsTyPat :: LHsType GhcPs -> HsTyPat GhcPs
mkHsTyPat x = HsTP { hstp_ext  = noExtField
                   , hstp_body = x }

mkEmptyWildCardBndrs :: thing -> HsWildCardBndrs GhcRn thing
mkEmptyWildCardBndrs x = HsWC { hswc_body = x
                              , hswc_ext  = [] }

--------------------------------------------------

type instance XTyVarBndr    (GhcPass _) = AnnTyVarBndr
type instance XXTyVarBndr   (GhcPass _) = DataConCantHappen

type instance XBndrKind   (GhcPass p) = NoExtField
type instance XBndrNoKind (GhcPass p) = NoExtField
type instance XXBndrKind  (GhcPass p) = DataConCantHappen

type instance XBndrVar (GhcPass p) = NoExtField

type instance XBndrWildCard GhcPs = EpToken "_"
type instance XBndrWildCard GhcRn = NoExtField
type instance XBndrWildCard GhcTc = NoExtField

type instance XXBndrVar (GhcPass p) = DataConCantHappen

data AnnTyVarBndr
  = AnnTyVarBndr {
    atv_opens  :: [EpaLocation], -- all "(" or all "{"
    atv_closes :: [EpaLocation], -- all ")" or all "}"
    atv_tv     :: EpToken "'",
    atv_dcolon :: TokDcolon
  } deriving Data

instance NoAnn AnnTyVarBndr where
  noAnn = AnnTyVarBndr noAnn noAnn noAnn noAnn

-- | Return the attached flag
hsTyVarBndrFlag :: HsTyVarBndr flag (GhcPass pass) -> flag
hsTyVarBndrFlag = tvb_flag
-- By specialising to (GhcPass p) we know that XXTyVarBndr is DataConCantHappen
-- so the equation is exhaustive: extension construction can't happen

-- | Set the attached flag
setHsTyVarBndrFlag :: flag -> HsTyVarBndr flag' (GhcPass pass)
  -> HsTyVarBndr flag (GhcPass pass)
setHsTyVarBndrFlag fl tvb = tvb { tvb_flag = fl }

-- | Update the attached flag
updateHsTyVarBndrFlag
  :: (flag -> flag')
  -> HsTyVarBndr flag  (GhcPass pass)
  -> HsTyVarBndr flag' (GhcPass pass)
updateHsTyVarBndrFlag f tvb = tvb { tvb_flag = f (tvb_flag tvb) }

-- | Get the variable of the type variable binder
hsBndrVar :: HsTyVarBndr flag (GhcPass pass) -> HsBndrVar (GhcPass pass)
hsBndrVar = tvb_var

-- | Get the kind of the type variable binder
hsBndrKind :: HsTyVarBndr flag (GhcPass pass) -> HsBndrKind (GhcPass pass)
hsBndrKind = tvb_kind

-- | Do all type variables in this 'LHsQTyVars' come with kind annotations?
hsTvbAllKinded :: LHsQTyVars (GhcPass p) -> Bool
hsTvbAllKinded = all (isHsKindedTyVar . unLoc) . hsQTvExplicit

type instance XBndrRequired (GhcPass _) = NoExtField

type instance XBndrInvisible GhcPs = EpToken "@"
type instance XBndrInvisible GhcRn = NoExtField
type instance XBndrInvisible GhcTc = NoExtField

type instance XXBndrVis (GhcPass _) = DataConCantHappen

{- Note [Wildcard binders in disallowed contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In contexts where a type variable binder is expected (HsTyVarBndr), we usually
allow both named binders and wildcards, e.g.

  type Const1 a b = a     -- ok
  type Const2 a _ = a     -- ok, too

This applies to LHSs of data, newtype, type, class, type family and data family
declarations. However, we choose to reject wildcards in forall telescopes and
type family result variables (the latter being part of TypeFamilyDependencies):

  type family Fd a = _    -- disallowed  (WildcardBndrInTyFamResultVar)
  fn :: forall _. Int     -- disallowed  (WildcardBndrInForallTelescope)

This restriction is placed solely because such binders have not been proposed
and there is no known use case for them. If we see user demand for wildcard
binders in these contexts, adding support for them would be as easy as dropping
the checks that reject them. The rest of the compiler can handle all wildcard
binders regardless of context by generating a fresh name (see `tcHsBndrVarName`
in GHC.Tc.Gen.HsType and `repHsBndrVar` in GHC.HsToCore.Quote).

That is, in type declarations we have:

  type F _  = ...  -- equivalent to ...
  type F _a = ...  -- where _a is fresh

and the same principle could be applied to foralls:

  fn :: forall _.  Int   -- equivalent to ...
  fn :: forall _a. Int   -- where _a is fresh

except the `forall _.` example is rejected by checkForAllTelescopeWildcardBndrs.
-}

type instance XForAllTy        (GhcPass _) = NoExtField
type instance XQualTy          (GhcPass _) = NoExtField
type instance XTyVar           (GhcPass _) = EpToken "'"
type instance XAppTy           (GhcPass _) = NoExtField
type instance XFunTy           (GhcPass _) = NoExtField
type instance XListTy          (GhcPass _) = AnnParen
type instance XTupleTy         (GhcPass _) = AnnParen
type instance XSumTy           (GhcPass _) = AnnParen
type instance XOpTy            (GhcPass _) = NoExtField
type instance XParTy           (GhcPass _) = (EpToken "(", EpToken ")")
type instance XIParamTy        (GhcPass _) = TokDcolon
type instance XStarTy          (GhcPass _) = NoExtField
type instance XKindSig         (GhcPass _) = TokDcolon

type instance XAppKindTy       GhcPs = EpToken "@"
type instance XAppKindTy       GhcRn = NoExtField
type instance XAppKindTy       GhcTc = NoExtField

type instance XSpliceTy        GhcPs = NoExtField
type instance XSpliceTy        GhcRn = HsUntypedSpliceResult (LHsType GhcRn)
type instance XSpliceTy        GhcTc = Kind

type instance XDocTy           (GhcPass _) = NoExtField
type instance XConDeclField    (GhcPass _) = ((EpaLocation, EpToken "#-}", EpaLocation), SourceText)
type instance XXConDeclRecField   (GhcPass _) = DataConCantHappen

type instance XExplicitListTy  GhcPs = (EpToken "'", EpToken "[", EpToken "]")
type instance XExplicitListTy  GhcRn = NoExtField
type instance XExplicitListTy  GhcTc = Kind

type instance XExplicitTupleTy GhcPs = (EpToken "'", EpToken "(", EpToken ")")
type instance XExplicitTupleTy GhcRn = NoExtField
type instance XExplicitTupleTy GhcTc = [Kind]

type instance XTyLit           (GhcPass _) = NoExtField

type instance XWildCardTy      GhcPs = EpToken "_"
type instance XWildCardTy      GhcRn = NoExtField
type instance XWildCardTy      GhcTc = NoExtField

type instance XXType           GhcPs = HsTypeGhcPsExt
type instance XXType           GhcRn = HsCoreTy
type instance XXType           GhcTc = DataConCantHappen

type instance XNumTy         (GhcPass _) = SourceText
type instance XStrTy         (GhcPass _) = SourceText
type instance XCharTy        (GhcPass _) = SourceText
type instance XXTyLit        (GhcPass _) = DataConCantHappen

type HsCoreTy = Type

-- Extension of HsType during parsing.
-- see Note [Trees That Grow] in Language.Haskell.Syntax.Extension
data HsTypeGhcPsExt
  = HsCoreTy    HsCoreTy
    -- An escape hatch for tunnelling a Core 'Type' through 'HsType'.
    -- For more details on how this works, see:
    --
    -- @Note [Renaming HsCoreTys]@ in "GHC.Rename.HsType"
    --
    -- @Note [Typechecking HsCoreTys]@ in "GHC.Tc.Gen.HsType"

  | HsBangTy    (EpaLocation, EpToken "#-}", EpaLocation)
                HsSrcBang
                (LHsType GhcPs)
    -- See Note [Parsing data type declarations]

  | HsRecTy     (AnnList ())
                [LHsConDeclRecField GhcPs]
    -- See Note [Parsing data type declarations]

{- Note [Parsing data type declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When parsing it is not always clear if we're parsing a constructor field type
or not. So during parsing we extend the type syntax to support bang annotations
and record braces. We do this through the extension constructor of (HsType GhcPs),
namely `HsTypeGhcPsExt`, adding data constructors for `HsBangTy` and `HsRecTy`.
Once parsing is done (i.e. (HsType GhcRn) and (HsType GhcTc)) these constructors
are not needed; instead the data is stored in `HsConDeclField`. It is an error
if it turns out the extensions were used outside of a constructor field type.
-}

data EpArrowOrColon
  = EpArrow !TokRarrow
  | EpColon !TokDcolon
  | EpPatBind
  deriving Data

data EpLinear
  = EpPct1 !(EpToken "%1") !EpArrowOrColon
  | EpLolly !(EpToken "⊸")
  deriving Data

instance NoAnn EpLinear where
  noAnn = EpPct1 noAnn (EpArrow noAnn)

type instance XUnannotated  _ GhcPs = EpArrowOrColon
type instance XUnannotated  _ GhcRn = NoExtField
type instance XUnannotated  _ GhcTc = Mult

type instance XLinearAnn    _ GhcPs = EpLinear
type instance XLinearAnn    _ GhcRn = NoExtField
type instance XLinearAnn    _ GhcTc = Mult

type instance XExplicitMult _ GhcPs = (EpToken "%", EpArrowOrColon)
type instance XExplicitMult _ GhcRn = NoExtField
type instance XExplicitMult _ GhcTc = Mult

type instance XXMultAnnOf   _ (GhcPass _) = DataConCantHappen

multAnnToHsType :: HsMultAnn GhcRn -> Maybe (LHsType GhcRn)
multAnnToHsType = expandHsMultAnnOf (HsTyVar noAnn NotPromoted . fmap noUserRdr)

-- | Convert an multiplicity annotation into its corresponding multiplicity.
-- If no annotation was written, `Nothing` is returned.
-- In this polymorphic function, `t` can be `HsType` or `HsExpr`
expandHsMultAnnOf :: (LocatedN Name -> t GhcRn)
                  -> HsMultAnnOf (LocatedA (t GhcRn)) GhcRn
                  -> Maybe (LocatedA (t GhcRn))
expandHsMultAnnOf _mk_var HsUnannotated{} = Nothing
expandHsMultAnnOf mk_var (HsLinearAnn _) = Just (noLocA (mk_var (noLocA oneDataConName)))
expandHsMultAnnOf _mk_var (HsExplicitMult _ p) = Just p

instance
      (Outputable mult, OutputableBndrId pass) =>
      Outputable (HsMultAnnOf mult (GhcPass pass)) where
  ppr arr = parens (pprHsArrow arr)

-- See #18846
pprHsArrow :: (Outputable mult, OutputableBndrId pass) => HsMultAnnOf mult (GhcPass pass) -> SDoc
pprHsArrow (HsUnannotated _)    = pprArrowWithMultiplicity visArgTypeLike (Left False)
pprHsArrow (HsLinearAnn _)      = pprArrowWithMultiplicity visArgTypeLike (Left True)
pprHsArrow (HsExplicitMult _ p) = pprArrowWithMultiplicity visArgTypeLike (Right (ppr p))

-- Used to print, for instance, let bindings:
--   let %1 x = …
-- and record field declarations:
--   { x %1 :: … }
pprHsMultAnn :: forall id. OutputableBndrId id => HsMultAnn (GhcPass id) -> SDoc
pprHsMultAnn (HsUnannotated _) = empty
pprHsMultAnn (HsLinearAnn _) = text "%1"
pprHsMultAnn (HsExplicitMult _ p) = text "%" <> ppr p

type instance XConDeclRecField  (GhcPass _) = NoExtField
type instance XXConDeclRecField (GhcPass _) = DataConCantHappen

instance OutputableBndrId p
       => Outputable (HsConDeclRecField (GhcPass p)) where
  ppr (HsConDeclRecField _ fld_n cfs) = pprMaybeWithDoc (cdf_doc cfs) (ppr_names fld_n <+> pprHsConDeclFieldWith ppr_mult cfs { cdf_doc = Nothing })
    where
      ppr_names :: [LFieldOcc (GhcPass p)] -> SDoc
      ppr_names [n] = pprPrefixOcc n
      ppr_names ns = sep (punctuate comma (map pprPrefixOcc ns))

      ppr_mult :: HsMultAnn (GhcPass p) -> SDoc -> SDoc
      ppr_mult mult tyDoc = pprHsMultAnn mult <+> dcolon <+> tyDoc

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

hsScopedKvs :: LHsKind GhcRn -> [Name]
-- Same as hsScopedTvs, but for a LHsKind
hsScopedKvs  (L _ HsForAllTy { hst_tele = HsForAllInvis { hsf_invis_bndrs = bndrs }})
  = hsLTyVarNames bndrs
    -- See Note [hsScopedTvs and visible foralls]
hsScopedKvs _ = []

---------------------
hsTyVarLName :: HsTyVarBndr flag (GhcPass p) -> Maybe (LIdP (GhcPass p))
hsTyVarLName tvb =
  case hsBndrVar tvb of
    HsBndrVar      _ n -> Just n
    HsBndrWildCard _   -> Nothing

hsTyVarName :: HsTyVarBndr flag (GhcPass p) -> Maybe (IdP (GhcPass p))
hsTyVarName = fmap unLoc . hsTyVarLName

hsLTyVarName :: LHsTyVarBndr flag (GhcPass p) -> Maybe (IdP (GhcPass p))
hsLTyVarName = hsTyVarName . unLoc

hsLTyVarNames :: [LHsTyVarBndr flag (GhcPass p)] -> [IdP (GhcPass p)]
hsLTyVarNames = mapMaybe hsLTyVarName

hsForAllTelescopeBndrs :: HsForAllTelescope (GhcPass p) -> [LHsTyVarBndr ForAllTyFlag (GhcPass p)]
hsForAllTelescopeBndrs (HsForAllVis   _ bndrs) = map (fmap (setHsTyVarBndrFlag Required)) bndrs
hsForAllTelescopeBndrs (HsForAllInvis _ bndrs) = map (fmap (updateHsTyVarBndrFlag Invisible)) bndrs

hsForAllTelescopeNames :: HsForAllTelescope (GhcPass p) -> [IdP (GhcPass p)]
hsForAllTelescopeNames (HsForAllVis _ bndrs) = hsLTyVarNames bndrs
hsForAllTelescopeNames (HsForAllInvis _ bndrs) = hsLTyVarNames bndrs

hsExplicitLTyVarNames :: LHsQTyVars (GhcPass p) -> [IdP (GhcPass p)]
-- Explicit variables only
hsExplicitLTyVarNames qtvs = hsLTyVarNames (hsQTvExplicit qtvs)

hsAllLTyVarNames :: LHsQTyVars GhcRn -> [Name]
-- All variables
hsAllLTyVarNames (HsQTvs { hsq_ext = kvs
                         , hsq_explicit = tvs })
  = kvs ++ hsLTyVarNames tvs

hsLTyVarLocName :: Anno (IdGhcP p) ~ SrcSpanAnnN
                => LHsTyVarBndr flag (GhcPass p) -> Maybe (LocatedN (IdP (GhcPass p)))
hsLTyVarLocName (L _ a) = hsTyVarLName a

hsLTyVarLocNames :: Anno (IdGhcP p) ~ SrcSpanAnnN
                 => LHsQTyVars (GhcPass p) -> [LocatedN (IdP (GhcPass p))]
hsLTyVarLocNames qtvs = mapMaybe hsLTyVarLocName (hsQTvExplicit qtvs)

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

mkAnonWildCardTy :: EpToken "_" -> HsType GhcPs
mkAnonWildCardTy tok = HsWildCardTy tok

mkHsOpTy :: (Anno (IdOccGhcP p) ~ SrcSpanAnnN)
         => PromotionFlag
         -> LHsType (GhcPass p) -> LocatedN (IdOccP (GhcPass p))
         -> LHsType (GhcPass p) -> HsType (GhcPass p)
mkHsOpTy prom ty1 op ty2 = HsOpTy noExtField prom ty1 op ty2

mkHsAppTy :: LHsType (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p)
mkHsAppTy t1 t2 = addCLocA t1 t2 (HsAppTy noExtField t1 t2)

mkHsAppTys :: LHsType (GhcPass p) -> [LHsType (GhcPass p)]
           -> LHsType (GhcPass p)
mkHsAppTys = foldl' mkHsAppTy

mkHsAppKindTy :: XAppKindTy (GhcPass p)
              -> LHsType (GhcPass p) -> LHsType (GhcPass p)
              -> LHsType (GhcPass p)
mkHsAppKindTy at ty k = addCLocA ty k (HsAppKindTy at ty k)

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
     LHsType GhcPs
  -> ( ([EpToken "("], [EpToken ")"]) , EpAnnComments -- The locations of any parens and
                                  -- comments discarded
     , [HsConDeclField GhcPs], LHsType GhcPs)
splitHsFunType ty = go ty
  where
    go (L l (HsParTy (op,cp) ty))
      = let
          ((ops, cps), cs, args, res) = splitHsFunType ty
          cs' = cs S.<> epAnnComments l
        in ((ops++[op], cps ++ [cp]), cs', args, res)

    go (L ll (HsFunTy _ mult x y))
      | (anns, csy, args, res) <- splitHsFunType y
      = (anns, csy S.<> epAnnComments ll, mkConDeclField mult x:args, res)

    go other = (noAnn, emptyComments, [], other)

-- | Retrieve the name of the \"head\" of a nested type application.
-- This is somewhat like @GHC.Tc.Gen.HsType.splitHsAppTys@, but a little more
-- thorough. The purpose of this function is to examine instance heads, so it
-- doesn't handle *all* cases (like lists, tuples, @(~)@, etc.).
hsTyGetAppHead_maybe :: (Anno (IdOccGhcP p) ~ SrcSpanAnnN)
                     => LHsType (GhcPass p)
                     -> Maybe (LocatedN (IdOccP (GhcPass p)))
hsTyGetAppHead_maybe = go
  where
    go (L _ (HsTyVar _ _ ln))    = Just ln
    go (L _ (HsAppTy _ l _))     = go l
    go (L _ (HsAppKindTy _ t _)) = go t
    go (L _ (HsOpTy _ _ _ ln _)) = Just ln
    go (L _ (HsParTy _ t))       = go t
    go (L _ (HsKindSig _ t _))   = go t
    go _                         = Nothing

------------------------------------------------------------

type instance XValArg (GhcPass _) = NoExtField

type instance XTypeArg GhcPs = EpToken "@"
type instance XTypeArg GhcRn = NoExtField
type instance XTypeArg GhcTc = NoExtField

type instance XArgPar (GhcPass _) = SrcSpan

type instance XXArg (GhcPass _) = DataConCantHappen

-- | Compute the 'SrcSpan' associated with an 'LHsTypeArg'.
lhsTypeArgSrcSpan :: LHsTypeArg GhcPs -> SrcSpan
lhsTypeArgSrcSpan arg = case arg of
  HsValArg  _  tm -> getLocA tm
  HsTypeArg at ty -> getEpTokenSrcSpan at `combineSrcSpans` getLocA ty
  HsArgPar  sp    -> sp

--------------------------------

numVisibleArgs :: [HsArg p tm ty] -> Arity
numVisibleArgs = count is_vis
  where is_vis (HsValArg _ _) = True
        is_vis _              = False

--------------------------------

-- | @'pprHsArgsApp' id fixity args@ pretty-prints an application of @id@
-- to @args@, using the @fixity@ to tell whether @id@ should be printed prefix
-- or infix. Examples:
--
-- @
-- pprHsArgsApp T Prefix [HsTypeArg Bool, HsValArg Int]                        = T \@Bool Int
-- pprHsArgsApp T Prefix [HsTypeArg Bool, HsArgPar, HsValArg Int]              = (T \@Bool) Int
-- pprHsArgsApp (++) Infix [HsValArg Char, HsValArg Double]                    = Char ++ Double
-- pprHsArgsApp (++) Infix [HsValArg Char, HsValArg Double, HsVarArg Ordering] = (Char ++ Double) Ordering
-- @
pprHsArgsApp :: (OutputableBndr id, Outputable tm, Outputable ty)
             => id -> LexicalFixity -> [HsArg (GhcPass p) tm ty] -> SDoc
pprHsArgsApp thing fixity (argl:argr:args)
  | Infix <- fixity
  = let pp_op_app = hsep [ ppr_single_hs_arg argl
                         , pprInfixOcc thing
                         , ppr_single_hs_arg argr ] in
    case args of
      [] -> pp_op_app
      _  -> ppr_hs_args_prefix_app (parens pp_op_app) args

pprHsArgsApp thing _fixity args
  = ppr_hs_args_prefix_app (pprPrefixOcc thing) args

-- | Pretty-print a prefix identifier to a list of 'HsArg's.
ppr_hs_args_prefix_app :: (Outputable tm, Outputable ty)
                        => SDoc -> [HsArg (GhcPass p) tm ty] -> SDoc
ppr_hs_args_prefix_app acc []         = acc
ppr_hs_args_prefix_app acc (arg:args) =
  case arg of
    HsValArg{}  -> ppr_hs_args_prefix_app (acc <+> ppr_single_hs_arg arg) args
    HsTypeArg{} -> ppr_hs_args_prefix_app (acc <+> ppr_single_hs_arg arg) args
    HsArgPar{}  -> ppr_hs_args_prefix_app (parens acc) args

-- | Pretty-print an 'HsArg' in isolation.
ppr_single_hs_arg :: (Outputable tm, Outputable ty)
                  => HsArg (GhcPass p) tm ty -> SDoc
ppr_single_hs_arg (HsValArg _ tm)  = ppr tm
ppr_single_hs_arg (HsTypeArg _ ty) = char '@' <> ppr ty
-- GHC shouldn't be constructing ASTs such that this case is ever reached.
-- Still, it's possible some wily user might construct their own AST that
-- allows this to be reachable, so don't fail here.
ppr_single_hs_arg (HsArgPar{})     = empty

-- | This instance is meant for debug-printing purposes. If you wish to
-- pretty-print an application of 'HsArg's, use 'pprHsArgsApp' instead.
instance (Outputable tm, Outputable ty) => Outputable (HsArg (GhcPass p) tm ty) where
  ppr (HsValArg _ tm)   = text "HsValArg"  <+> ppr tm
  ppr (HsTypeArg _ ty)  = text "HsTypeArg" <+> ppr ty
  ppr (HsArgPar sp)     = text "HsArgPar"  <+> ppr sp

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

    (univs, ty1) = split_sig_ty ty
    (reqs,  ty2) = splitLHsQualTy ty1
    (exis,  ty3) = splitLHsForAllTyInvis ty2
    (provs, ty4) = splitLHsQualTy ty3

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
  | (tvs,  ty1) <- splitLHsForAllTyInvis ty
  , (ctxt, ty2) <- splitLHsQualTy ty1
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
  -> (HsOuterSigTyVarBndrs GhcPs, [HsForAllTelescope GhcPs], Maybe (LHsContext GhcPs), LHsType GhcPs)
splitLHsGadtTy (L _ sig_ty)
  | (outer_bndrs, sigma_ty) <- split_outer_bndrs sig_ty
  , (inner_bndrs, phi_ty)   <- split_inner_bndrs sigma_ty
  , (mb_ctxt, rho_ty)       <- splitLHsQualTy_KP phi_ty
  = case rho_ty of
      L _ (HsFunTy _ _ (L _ (XHsType HsRecTy{})) _) | not (null inner_bndrs)
        -- Bad! Record GADTs are not allowed to have inner_bndrs,
        -- undo the split to get a proper error message later
        -> (outer_bndrs, [], Nothing, sigma_ty)
      _ -> (outer_bndrs, inner_bndrs, mb_ctxt, rho_ty)
  where
    split_outer_bndrs :: HsSigType GhcPs -> (HsOuterSigTyVarBndrs GhcPs, LHsType GhcPs)
    split_outer_bndrs (HsSig{sig_bndrs = outer_bndrs, sig_body = body_ty}) =
      (outer_bndrs, body_ty)

    split_inner_bndrs :: LHsType GhcPs -> ([HsForAllTelescope GhcPs], LHsType GhcPs)
    split_inner_bndrs (L _ HsForAllTy { hst_tele = tele
                                      , hst_body = body })
      = let ~(teles, t) = split_inner_bndrs body
        in (tele:teles, t)
    split_inner_bndrs t = ([], t)

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
  LHsType (GhcPass pass) -> ( [LHsTyVarBndr Specificity (GhcPass pass)]
                            , LHsType (GhcPass pass))
splitLHsForAllTyInvis ty
  | ((mb_tvbs), body) <- splitLHsForAllTyInvis_KP (ignoreParens ty)
  = (fromMaybe [] mb_tvbs, body)

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
  LHsType (GhcPass pass) -> (Maybe ([LHsTyVarBndr Specificity (GhcPass pass)])
                            , LHsType (GhcPass pass))
splitLHsForAllTyInvis_KP lty@(L _ ty) =
  case ty of
    HsForAllTy { hst_tele = HsForAllInvis {hsf_invis_bndrs = tvs }
               , hst_body = body }
      -> (Just tvs, body)
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
getLHsInstDeclClass_maybe :: (Anno (IdOccGhcP p) ~ SrcSpanAnnN)
                          => LHsSigType (GhcPass p)
                          -> Maybe (LocatedN (IdOccP (GhcPass p)))
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

Note [Ambiguous FieldOcc in record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When renaming a "record field update" (`some_record{ field = expr }`), the field
occurrence may be ambiguous if there are multiple record types with that same
field label in scope. Instead of failing, we may attempt to do type-directed
disambiguation: if we typecheck the record field update, we can disambiguate
the `field` based on the record and field type.

In practice, this means an identifier of a field occurrence
(`FieldOcc`) may have to go straight from `RdrName` to `Id`, since field
ambiguity makes it impossible to construct a `Name` for the field.

Since type-directed disambiguation is a GHC property rather than a property of
the GHC-Haskell AST, we still parameterise a `FieldOcc` occurrence by `IdP p`,
but in the case of the ambiguity we do the unthinkable and insert a mkUnboundName
in the name. Very bad, yes, but since type-directed disambiguation is on the way
out (see proposal https://github.com/ghc-proposals/ghc-proposals/pull/366),
we consider this acceptable for now.

see also Wrinkle [Disambiguating fields] and note [Type-directed record disambiguation]

NB: FieldOcc preserves the RdrName throughout its lifecycle for
exact printing purposes.
-}

type instance XCFieldOcc GhcPs = NoExtField -- RdrName is stored in the proper IdP field
type instance XCFieldOcc GhcRn = RdrName
type instance XCFieldOcc GhcTc = RdrName

type instance XXFieldOcc (GhcPass p) = DataConCantHappen

--------------------------------------------------------------------------------

mkFieldOcc :: LocatedN RdrName -> FieldOcc GhcPs
mkFieldOcc rdr = FieldOcc noExtField rdr

fieldOccRdrName :: forall p. IsPass p => FieldOcc (GhcPass p) -> RdrName
fieldOccRdrName fo = case ghcPass @p of
  GhcPs -> unLoc $ foLabel fo
  GhcRn -> foExt fo
  GhcTc -> foExt fo

-- ToDo SPJ: remove
--fieldOccExt :: FieldOcc (GhcPass p) -> XCFieldOcc (GhcPass p)
--fieldOccExt (FieldOcc { foExt = ext }) = ext

fieldOccLRdrName :: forall p. IsPass p => FieldOcc (GhcPass p) -> LocatedN RdrName
fieldOccLRdrName fo = case ghcPass @p of
  GhcPs -> foLabel fo
  GhcRn -> case fo of
    FieldOcc rdr sel ->
      let (L l _) = sel
       in L l rdr
  GhcTc ->
    let (L l _) = foLabel fo
     in L l (foExt fo)


{-
************************************************************************
*                                                                      *
                OpName
*                                                                      *
************************************************************************
-}

-- | Name of an operator in an operator application or section
data OpName = NormalOp (WithUserRdr Name) -- ^ A normal identifier
            | NegateOp                    -- ^ Prefix negation
            | UnboundOp RdrName           -- ^ An unbound identifier
            | RecFldOp (FieldOcc GhcRn)   -- ^ A record field occurrence

instance Outputable OpName where
  ppr (NormalOp n)   = ppr n
  ppr NegateOp       = ppr negateName
  ppr (UnboundOp uv) = ppr uv
  ppr (RecFldOp fld) = ppr fld

{-
************************************************************************
*                                                                      *
\subsection{Pretty printing}
*                                                                      *
************************************************************************
-}

instance OutputableBndrId p => Outputable (HsBndrVar (GhcPass p)) where
  ppr (HsBndrVar _ name) = ppr name
  ppr (HsBndrWildCard _) = char '_'

class OutputableBndrFlag flag p where
  pprTyVarBndr :: OutputableBndrId p => HsTyVarBndr flag (GhcPass p) -> SDoc

instance OutputableBndrFlag () p where
  pprTyVarBndr (HsTvb _ _ bvar bkind) = decorate (ppr_hs_tvb bvar bkind)
    where decorate :: SDoc -> SDoc
          decorate d = parens_if_kind bkind d

instance OutputableBndrFlag Specificity p where
  pprTyVarBndr (HsTvb _ spec bvar bkind) = decorate (ppr_hs_tvb bvar bkind)
    where decorate :: SDoc -> SDoc
          decorate d = case spec of
            InferredSpec  -> braces d
            SpecifiedSpec -> parens_if_kind bkind d

instance OutputableBndrFlag (HsBndrVis (GhcPass p')) p where
  pprTyVarBndr (HsTvb _ bvis bvar bkind) = decorate (ppr_hs_tvb bvar bkind)
    where decorate :: SDoc -> SDoc
          decorate d = case bvis of
            HsBndrRequired  _ -> parens_if_kind bkind d
            HsBndrInvisible _ -> char '@' <> parens_if_kind bkind d

instance OutputableBndrFlag ForAllTyFlag p where
  pprTyVarBndr (HsTvb _ spec bvar bkind) =
      text "forall" <+> decorate (ppr_hs_tvb bvar bkind)
    where decorate :: SDoc -> SDoc
          decorate d = case spec of
            Inferred  -> braces d <> dot
            Specified -> parens_if_kind bkind d <> dot
            Required  -> parens_if_kind bkind d <+> text "->"

ppr_hs_tvb :: OutputableBndrId p => HsBndrVar (GhcPass p) -> HsBndrKind (GhcPass p) -> SDoc
ppr_hs_tvb bvar (HsBndrNoKind _) = ppr bvar
ppr_hs_tvb bvar (HsBndrKind _ k) = hsep [ppr bvar, dcolon, ppr k]

parens_if_kind :: HsBndrKind (GhcPass p) -> SDoc -> SDoc
parens_if_kind (HsBndrNoKind _) d = d
parens_if_kind (HsBndrKind _ _) d = parens d

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


instance (OutputableBndrId p)
       => Outputable (HsTyPat (GhcPass p)) where
    ppr (HsTP { hstp_body = ty }) = ppr ty


instance (OutputableBndrId p)
       => Outputable (HsTyLit (GhcPass p)) where
    ppr = ppr_tylit

instance Outputable HsIPName where
    ppr (HsIPName n) = char '?' <> ftext n -- Ordinary implicit parameters

instance OutputableBndr HsIPName where
    pprBndr _ n   = ppr n         -- Simple for now
    pprInfixOcc  n = ppr n
    pprPrefixOcc n = ppr n

instance (Outputable arg, Outputable rec)
         => Outputable (HsConDetails arg rec) where
  ppr (PrefixCon args) = text "PrefixCon:" <+> ppr args
  ppr (RecCon rec)     = text "RecCon:" <+> ppr rec
  ppr (InfixCon l r)   = text "InfixCon:" <+> ppr [l, r]

pprHsConDeclFieldWith :: (OutputableBndrId p)
                      => (HsMultAnn (GhcPass p) -> SDoc -> SDoc)
                      -> HsConDeclField (GhcPass p) -> SDoc
pprHsConDeclFieldWith ppr_mult (CDF _ prag mark mult ty doc) =
  pprMaybeWithDoc doc (ppr_mult mult (ppr prag <+> ppr mark <> ppr ty))

pprHsConDeclFieldNoMult :: (OutputableBndrId p) => HsConDeclField (GhcPass p) -> SDoc
pprHsConDeclFieldNoMult = pprHsConDeclFieldWith (\_ d -> d)

hsPlainTypeField :: LHsType GhcPs -> HsConDeclField GhcPs
hsPlainTypeField = mkConDeclField (HsUnannotated (EpColon noAnn))

mkConDeclField :: HsMultAnn GhcPs -> LHsType GhcPs -> HsConDeclField GhcPs
mkConDeclField mult (L _ (HsDocTy _ ty lds)) = (mkConDeclField mult ty) { cdf_doc = Just lds }
mkConDeclField mult (L _ (XHsType (HsBangTy ann (HsSrcBang srcTxt unp str) t))) = CDF (ann, srcTxt) unp str mult t Nothing
mkConDeclField mult t = CDF noAnn NoSrcUnpack NoSrcStrict mult t Nothing

instance Outputable (XRecGhc (IdGhcP p)) =>
       Outputable (FieldOcc (GhcPass p)) where
  ppr = ppr . foLabel

instance (OutputableBndrId pass) => OutputableBndr (FieldOcc (GhcPass pass)) where
  pprInfixOcc  = pprInfixOcc . unXRec @(GhcPass pass) . foLabel
  pprPrefixOcc = pprPrefixOcc . unXRec @(GhcPass pass) . foLabel

instance (OutputableBndrId pass) => OutputableBndr (GenLocated SrcSpan (FieldOcc (GhcPass pass))) where
  pprInfixOcc  = pprInfixOcc . unLoc
  pprPrefixOcc = pprPrefixOcc . unLoc

ppr_tylit :: (HsTyLit (GhcPass p)) -> SDoc
ppr_tylit (HsNumTy source i) = pprWithSourceText source (integer i)
ppr_tylit (HsStrTy source s) = pprWithSourceText source (text (show s))
ppr_tylit (HsCharTy source c) = pprWithSourceText source (text (show c))

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
  pprHsForAllTelescope (mkHsForAllInvisTele noAnn bndrs)

-- | Prints a forall; When passed an empty list, prints @forall .@/@forall ->@
-- only when @-dppr-debug@ is enabled.
pprHsForAll :: forall p. OutputableBndrId p
            => HsForAllTelescope (GhcPass p)
            -> Maybe (LHsContext (GhcPass p)) -> SDoc
pprHsForAll tele cxt
  = pprHsForAllTelescope tele <+> pprLHsContext cxt

pprHsForAllTelescope :: forall p. OutputableBndrId p
                     => HsForAllTelescope (GhcPass p)
                     -> SDoc
pprHsForAllTelescope tele =
  case tele of
      HsForAllVis   { hsf_vis_bndrs   = qtvs } -> pp_forall (space <> arrow) qtvs
      HsForAllInvis { hsf_invis_bndrs = qtvs } -> pp_forall dot qtvs
  where
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

pprHsConDeclRecFields :: forall p. OutputableBndrId p
                 => [LHsConDeclRecField (GhcPass p)] -> SDoc
pprHsConDeclRecFields fields = braces (sep (punctuate comma (map ppr fields)))

-- Printing works more-or-less as for Types

pprHsType :: (OutputableBndrId p) => HsType (GhcPass p) -> SDoc
pprHsType ty = ppr_mono_ty ty

ppr_mono_lty :: OutputableBndrId p
             => LHsType (GhcPass p) -> SDoc
ppr_mono_lty ty = ppr_mono_ty (unLoc ty)

ppr_mono_ty :: forall p. (OutputableBndrId p) => HsType (GhcPass p) -> SDoc
ppr_mono_ty (HsForAllTy { hst_tele = tele, hst_body = ty })
  = sep [pprHsForAll tele Nothing, ppr_mono_lty ty]

ppr_mono_ty (HsQualTy { hst_ctxt = ctxt, hst_body = ty })
  = sep [pprLHsContextAlways ctxt, ppr_mono_lty ty]

ppr_mono_ty (HsTyVar _ prom (L _ name)) = pprOccWithTick Prefix prom name
ppr_mono_ty (HsFunTy _ mult ty1 ty2)    = ppr_fun_ty mult ty1 ty2
ppr_mono_ty (HsTupleTy _ con tys)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `Solo x`, not `(x)`
  | [ty] <- tys
  , BoxedTuple <- std_con
  = sep [text (mkTupleStr Boxed tcName 1), ppr_mono_lty ty]
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
ppr_mono_ty (HsSpliceTy ext s)    =
    case ghcPass @p of
      GhcPs -> pprUntypedSplice True Nothing s
      GhcRn | HsUntypedSpliceNested n <- ext -> pprUntypedSplice True (Just n) s
      GhcRn | HsUntypedSpliceTop _ t  <- ext -> ppr t
      GhcTc -> pprUntypedSplice True Nothing s
ppr_mono_ty (HsExplicitListTy _ prom tys)
  | isPromoted prom = quote $ brackets (maybeAddSpace tys $ interpp'SP tys)
  | otherwise       = brackets (interpp'SP tys)
ppr_mono_ty (HsExplicitTupleTy _ prom tys)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `'MkSolo x`, not `'(x)`
  | [ty] <- tys
  = quote_tuple prom $ sep [text (mkTupleStr Boxed dataName 1), ppr_mono_lty ty]
  | otherwise
  = quote_tuple prom $ parens (maybeAddSpace tys $ interpp'SP tys)
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

ppr_mono_ty (XHsType t) = case ghcPass @p of
  GhcPs -> case t of
    HsCoreTy ty     -> ppr ty
    HsBangTy _ b ty -> ppr b <> ppr_mono_lty ty
    HsRecTy _ flds  -> pprHsConDeclRecFields flds
  GhcRn -> ppr t

--------------------------
ppr_fun_ty :: (OutputableBndrId p)
           => HsMultAnn (GhcPass p) -> LHsType (GhcPass p) -> LHsType (GhcPass p) -> SDoc
ppr_fun_ty mult ty1 ty2
  = let p1 = ppr_mono_lty ty1
        p2 = ppr_mono_lty ty2
        arr = pprHsArrow mult
    in
    sep [p1, arr <+> p2]

quote_tuple :: PromotionFlag -> SDoc -> SDoc
quote_tuple IsPromoted  doc = quote doc
quote_tuple NotPromoted doc = doc

--------------------------
-- | @'hsTypeNeedsParens' p t@ returns 'True' if the type @t@ needs parentheses
-- under precedence @p@.
hsTypeNeedsParens :: forall p. IsPass p => PprPrec -> HsType (GhcPass p) -> Bool
hsTypeNeedsParens p = go_hs_ty
  where
    go_hs_ty (HsForAllTy{})           = p >= funPrec
    go_hs_ty (HsQualTy{})             = p >= funPrec
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
    -- parenthesized as `Proxy ('MkSolo x)`, not `Proxy 'MkSolo x` (#18612)
    -- See Note [One-tuples] in GHC.Builtin.Types
    go_hs_ty (HsExplicitTupleTy _ _ [_])
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
    go_hs_ty (XHsType t)             = case ghcPass @p of
      GhcPs -> case t of
        HsCoreTy ty -> go_core_ty ty
        HsBangTy{}  -> p > topPrec
        HsRecTy{}   -> False
      GhcRn -> go_core_ty t

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
parenthesizeHsType :: IsPass p => PprPrec -> LHsType (GhcPass p) -> LHsType (GhcPass p)
parenthesizeHsType p lty@(L loc ty)
  | hsTypeNeedsParens p ty = L loc (HsParTy noAnn lty)
  | otherwise              = lty

-- | @'parenthesizeHsContext' p ctxt@ checks if @ctxt@ is a single constraint
-- @c@ such that @'hsTypeNeedsParens' p c@ is true, and if so, surrounds @c@
-- with an 'HsParTy' to form a parenthesized @ctxt@. Otherwise, it simply
-- returns @ctxt@ unchanged.
parenthesizeHsContext :: IsPass p => PprPrec -> LHsContext (GhcPass p) -> LHsContext (GhcPass p)
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
type instance Anno HsIPName = EpAnnCO
type instance Anno (HsConDeclRecField (GhcPass p)) = SrcSpanAnnA

type instance Anno (FieldOcc (GhcPass p)) = SrcSpanAnnA
