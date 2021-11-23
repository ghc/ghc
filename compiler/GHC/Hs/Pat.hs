{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PatSyntax]{Abstract Haskell syntax---patterns}
-}

module GHC.Hs.Pat (
        Pat(..), LPat, MatchPat(..), LMatchPat,
        EpAnnSumPat(..),
        ConPatTc (..),
        ConLikeP,
        HsPatExpansion(..),
        XXPatGhcTc(..),

        HsConPatDetails, hsConPatArgs,
        HsRecFields(..), HsFieldBind(..), LHsFieldBind,
        HsRecField, LHsRecField,
        HsRecUpdField, LHsRecUpdField,
        hsRecFields, hsRecFieldSel, hsRecFieldId, hsRecFieldsArgs,
        hsRecUpdFieldId, hsRecUpdFieldOcc, hsRecUpdFieldRdr,

        mkPrefixConPat, mkCharLitPat, mkNilPat, mkVisMatchPat,

        isSimplePat,
        looksLazyPatBind,
        isBangedLPat, isBangedLMatchPat,
        gParPat, patNeedsParens, parenthesizePat, parenthesizeLMatchPat,
        isIrrefutableHsPat,

        collectEvVarsPat, collectEvVarsPats,

        pprParendLPat, pprParendLMatchPat, pprConArgs,
        pprLPat, pprLMatchPat
    ) where

import GHC.Prelude

import Language.Haskell.Syntax.Pat
import Language.Haskell.Syntax.Expr ( HsExpr )

import {-# SOURCE #-} GHC.Hs.Expr (pprLExpr, pprSplice)

-- friends:
import GHC.Hs.Binds
import GHC.Hs.Lit
import Language.Haskell.Syntax.Extension
import GHC.Parser.Annotation
import GHC.Hs.Extension
import GHC.Hs.Type
import GHC.Tc.Types.Evidence
import GHC.Types.Basic
import GHC.Types.SourceText
-- others:
import GHC.Core.Ppr ( {- instance OutputableBndr TyVar -} )
import GHC.Builtin.Types
import GHC.Types.Var
import GHC.Types.Name.Reader ( RdrName )
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Utils.Outputable
import GHC.Core.Type
import GHC.Types.SrcLoc
import GHC.Data.Bag -- collect ev vars from pats
import GHC.Data.Maybe
import GHC.Types.Name (Name)
import GHC.Driver.Session
import qualified GHC.LanguageExtensions as LangExt
import Data.Data


type instance XWildPat GhcPs = NoExtField
type instance XWildPat GhcRn = NoExtField
type instance XWildPat GhcTc = Type

type instance XVarPat  (GhcPass _) = NoExtField

type instance XLazyPat GhcPs = EpAnn [AddEpAnn] -- For '~'
type instance XLazyPat GhcRn = NoExtField
type instance XLazyPat GhcTc = NoExtField

type instance XAsPat   GhcPs = EpAnn [AddEpAnn] -- For '@'
type instance XAsPat   GhcRn = NoExtField
type instance XAsPat   GhcTc = NoExtField

type instance XParPat (GhcPass _) = EpAnnCO

type instance XBangPat GhcPs = EpAnn [AddEpAnn] -- For '!'
type instance XBangPat GhcRn = NoExtField
type instance XBangPat GhcTc = NoExtField

type instance XListPat GhcPs = EpAnn AnnList
  -- After parsing, ListPat can refer to a built-in Haskell list pattern
  -- or an overloaded list pattern.
type instance XListPat GhcRn = NoExtField
  -- Built-in list patterns only.
  -- After renaming, overloaded list patterns are expanded to view patterns.
  -- See Note [Desugaring overloaded list patterns]
type instance XListPat GhcTc = Type
  -- List element type, for use in hsPatType.

type instance XTuplePat GhcPs = EpAnn [AddEpAnn]
type instance XTuplePat GhcRn = NoExtField
type instance XTuplePat GhcTc = [Type]

type instance XSumPat GhcPs = EpAnn EpAnnSumPat
type instance XSumPat GhcRn = NoExtField
type instance XSumPat GhcTc = [Type]

type instance XConPat GhcPs = EpAnn [AddEpAnn]
type instance XConPat GhcRn = NoExtField
type instance XConPat GhcTc = ConPatTc

type instance XViewPat GhcPs = EpAnn [AddEpAnn]
type instance XViewPat GhcRn = Maybe (HsExpr GhcRn)
  -- The @HsExpr GhcRn@ gives an inverse to the view function.
  -- This is used for overloaded lists in particular.
  -- See Note [Invertible view patterns] in GHC.Tc.TyCl.PatSyn.

type instance XViewPat GhcTc = Type
  -- Overall type of the pattern
  -- (= the argument type of the view function), for hsPatType.

type instance XSplicePat GhcPs = NoExtField
type instance XSplicePat GhcRn = NoExtField
type instance XSplicePat GhcTc = DataConCantHappen

type instance XLitPat    (GhcPass _) = NoExtField

type instance XNPat GhcPs = EpAnn [AddEpAnn]
type instance XNPat GhcRn = EpAnn [AddEpAnn]
type instance XNPat GhcTc = Type

type instance XNPlusKPat GhcPs = EpAnn EpaLocation -- Of the "+"
type instance XNPlusKPat GhcRn = NoExtField
type instance XNPlusKPat GhcTc = Type

type instance XSigPat GhcPs = EpAnn [AddEpAnn]
type instance XSigPat GhcRn = NoExtField
type instance XSigPat GhcTc = Type

type instance XXPat GhcPs = DataConCantHappen
type instance XXPat GhcRn = HsPatExpansion (Pat GhcRn) (Pat GhcRn)
  -- Original pattern and its desugaring/expansion.
  -- See Note [Rebindable syntax and HsExpansion].
type instance XXPat GhcTc = XXPatGhcTc
  -- After typechecking, we add extra constructors: CoPat and HsExpansion.
  -- HsExpansion allows us to handle RebindableSyntax in pattern position:
  -- see "XXExpr GhcTc" for the counterpart in expressions.

type instance ConLikeP GhcPs = RdrName -- IdP GhcPs
type instance ConLikeP GhcRn = Name    -- IdP GhcRn
type instance ConLikeP GhcTc = ConLike

type instance XHsFieldBind _ = EpAnn [AddEpAnn]

type instance XVisPat (GhcPass _) = NoExtField

type instance XInvisTyVarPat GhcPs = EpAnn [AddEpAnn]
type instance XInvisTyVarPat GhcRn = NoExtField
type instance XInvisTyVarPat GhcTc = NoExtField

type instance XInvisWildTyPat GhcPs = NoExtField
type instance XInvisWildTyPat GhcRn = NoExtField
type instance XInvisWildTyPat GhcTc = Type

type instance XXMatchPat (GhcPass _) = DataConCantHappen

mkVisMatchPat :: LPat (GhcPass pass) -> LMatchPat (GhcPass pass)
mkVisMatchPat lpat = L (getLoc lpat) (VisPat noExtField lpat)

-- ---------------------------------------------------------------------

-- API Annotations types

data EpAnnSumPat = EpAnnSumPat
      { sumPatParens      :: [AddEpAnn]
      , sumPatVbarsBefore :: [EpaLocation]
      , sumPatVbarsAfter  :: [EpaLocation]
      } deriving Data

-- ---------------------------------------------------------------------

-- | Extension constructor for Pat, added after typechecking.
data XXPatGhcTc
  = -- | Coercion Pattern (translation only)
    --
    -- During desugaring a (CoPat co pat) turns into a cast with 'co' on the
    -- scrutinee, followed by a match on 'pat'.
    CoPat
      { -- | Coercion Pattern
        -- If co :: t1 ~ t2, p :: t2,
        -- then (CoPat co p) :: t1
        co_cpt_wrap :: HsWrapper

      , -- | Why not LPat?  Ans: existing locn will do
        co_pat_inner :: Pat GhcTc

      , -- | Type of whole pattern, t1
        co_pat_ty :: Type
      }
  -- | Pattern expansion: original pattern, and desugared pattern,
  -- for RebindableSyntax and other overloaded syntax such as OverloadedLists.
  -- See Note [Rebindable syntax and HsExpansion].
  | ExpansionPat (Pat GhcRn) (Pat GhcTc)


-- See Note [Rebindable syntax and HsExpansion].
data HsPatExpansion a b
  = HsPatExpanded a b
  deriving Data

-- | This is the extension field for ConPat, added after typechecking
-- It adds quite a few extra fields, to support elaboration of pattern matching.
data ConPatTc
  = ConPatTc
    { -- | The universal arg types  1-1 with the universal
      -- tyvars of the constructor/pattern synonym
      -- Use (conLikeResTy pat_con cpt_arg_tys) to get
      -- the type of the pattern
      cpt_arg_tys :: [Type]

    , -- | Existentially bound type variables
      -- in correctly-scoped order e.g. [k:*  x:k]
      cpt_tvs   :: [TyVar]

    , -- | Ditto *coercion variables* and *dictionaries*
      -- One reason for putting coercion variable here  I think
      --      is to ensure their kinds are zonked
      cpt_dicts :: [EvVar]

    , -- | Bindings involving those dictionaries
      cpt_binds :: TcEvBinds

    , -- ^ Extra wrapper to pass to the matcher
      -- Only relevant for pattern-synonyms;
      --   ignored for data cons
      cpt_wrap  :: HsWrapper
    }

hsRecFieldId :: HsRecField GhcTc arg -> Id
hsRecFieldId = hsRecFieldSel

hsRecUpdFieldRdr :: HsRecUpdField (GhcPass p) -> Located RdrName
hsRecUpdFieldRdr = fmap rdrNameAmbiguousFieldOcc . reLoc . hfbLHS

hsRecUpdFieldId :: HsFieldBind (LAmbiguousFieldOcc GhcTc) arg -> Located Id
hsRecUpdFieldId = fmap foExt . reLoc . hsRecUpdFieldOcc

hsRecUpdFieldOcc :: HsFieldBind (LAmbiguousFieldOcc GhcTc) arg -> LFieldOcc GhcTc
hsRecUpdFieldOcc = fmap unambiguousFieldOcc . hfbLHS


{-
************************************************************************
*                                                                      *
*              Printing patterns
*                                                                      *
************************************************************************
-}

instance OutputableBndrId p => Outputable (Pat (GhcPass p)) where
    ppr = pprPat

-- See Note [Rebindable syntax and HsExpansion].
instance (Outputable a, Outputable b) => Outputable (HsPatExpansion a b) where
  ppr (HsPatExpanded a b) = ifPprDebug (vcat [ppr a, ppr b]) (ppr a)

instance OutputableBndrId p => Outputable (MatchPat (GhcPass p)) where
    ppr (VisPat _ lpat)     = ppr (unLoc lpat)
    ppr (InvisTyVarPat _ idp) = char '@' <> ppr idp
    ppr (InvisWildTyPat _)    = text "@_"

pprLPat :: (OutputableBndrId p) => LPat (GhcPass p) -> SDoc
pprLPat (L _ e) = pprPat e

pprLMatchPat :: (OutputableBndrId p) => LMatchPat (GhcPass p) -> SDoc
pprLMatchPat (L _ e) = ppr e

-- | Print with type info if -dppr-debug is on
pprPatBndr :: OutputableBndr name => name -> SDoc
pprPatBndr var
  = getPprDebug $ \case
      True -> parens (pprBndr LambdaBind var) -- Could pass the site to pprPat
                                              -- but is it worth it?
      False -> pprPrefixOcc var

pprParendLPat :: (OutputableBndrId p)
              => PprPrec -> LPat (GhcPass p) -> SDoc
pprParendLPat p = pprParendPat p . unLoc

pprParendLMatchPat :: (OutputableBndrId p)
                => PprPrec -> LMatchPat (GhcPass p) -> SDoc
pprParendLMatchPat p (L _ (VisPat _ pat))            = pprParendLPat p pat
pprParendLMatchPat _ (L _ (InvisTyVarPat _ tv_name)) = char '@' <> (ppr (unLoc tv_name))
pprParendLMatchPat _ (L _ (InvisWildTyPat _))        = text "@_"

pprParendPat :: forall p. OutputableBndrId p
             => PprPrec
             -> Pat (GhcPass p)
             -> SDoc
pprParendPat p pat = sdocOption sdocPrintTypecheckerElaboration $ \ print_tc_elab ->
    if need_parens print_tc_elab pat
    then parens (pprPat pat)
    else pprPat pat
  where
    need_parens print_tc_elab pat
      | GhcTc <- ghcPass @p
      , XPat (CoPat {}) <- pat
      = print_tc_elab

      | otherwise
      = patNeedsParens p pat
      -- For a CoPat we need parens if we are going to show it, which
      -- we do if -fprint-typechecker-elaboration is on (c.f. pprHsWrapper)
      -- But otherwise the CoPat is discarded, so it
      -- is the pattern inside that matters.  Sigh.

pprPat :: forall p. (OutputableBndrId p) => Pat (GhcPass p) -> SDoc
pprPat (VarPat _ lvar)          = pprPatBndr (unLoc lvar)
pprPat (WildPat _)              = char '_'
pprPat (LazyPat _ pat)          = char '~' <> pprParendLPat appPrec pat
pprPat (BangPat _ pat)          = char '!' <> pprParendLPat appPrec pat
pprPat (AsPat _ name pat)       = hcat [pprPrefixOcc (unLoc name), char '@',
                                        pprParendLPat appPrec pat]
pprPat (ViewPat _ expr pat)     = hcat [pprLExpr expr, text " -> ", ppr pat]
pprPat (ParPat _ _ pat _)      = parens (ppr pat)
pprPat (LitPat _ s)             = ppr s
pprPat (NPat _ l Nothing  _)    = ppr l
pprPat (NPat _ l (Just _) _)    = char '-' <> ppr l
pprPat (NPlusKPat _ n k _ _ _)  = hcat [ppr_n, char '+', ppr k]
  where ppr_n = case ghcPass @p of
                  GhcPs -> ppr n
                  GhcRn -> ppr n
                  GhcTc -> ppr n
pprPat (SplicePat _ splice)     = pprSplice splice
pprPat (SigPat _ pat ty)        = ppr pat <+> dcolon <+> ppr ty
pprPat (ListPat _ pats)         = brackets (interpp'SP pats)
pprPat (TuplePat _ pats bx)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `Solo x`, not `(x)`
  | [pat] <- pats
  , Boxed <- bx
  = hcat [text (mkTupleStr Boxed 1), pprParendLPat appPrec pat]
  | otherwise
  = tupleParens (boxityTupleSort bx) (pprWithCommas ppr pats)
pprPat (SumPat _ pat alt arity) = sumParens (pprAlternative ppr pat alt arity)
pprPat (ConPat { pat_con = con
               , pat_args = details
               , pat_con_ext = ext
               }
       )
  = case ghcPass @p of
      GhcPs -> pprUserCon (unLoc con) details
      GhcRn -> pprUserCon (unLoc con) details
      GhcTc -> sdocOption sdocPrintTypecheckerElaboration $ \case
        False -> pprUserCon (unLoc con) details
        True  ->
          -- Tiresome; in 'GHC.Tc.Gen.Bind.tcRhs' we print out a typechecked Pat in an
          -- error message, and we want to make sure it prints nicely
          ppr con
            <> braces (sep [ hsep (map pprPatBndr (tvs ++ dicts))
                           , ppr binds ])
            <+> pprConArgs details
        where ConPatTc { cpt_tvs = tvs
                       , cpt_dicts = dicts
                       , cpt_binds = binds
                       } = ext

pprPat (XPat ext) = case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
  GhcPs -> dataConCantHappen ext
#endif
  GhcRn -> case ext of
    HsPatExpanded orig _ -> pprPat orig
  GhcTc -> case ext of
    CoPat co pat _ ->
      pprHsWrapper co $ \parens ->
        if parens
        then pprParendPat appPrec pat
        else pprPat pat
    ExpansionPat orig _ -> pprPat orig

pprUserCon :: (OutputableBndr con, OutputableBndrId p,
                     Outputable (Anno (IdGhcP p)))
           => con -> HsConPatDetails (GhcPass p) -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details          = pprPrefixOcc c <+> pprConArgs details

pprConArgs :: (OutputableBndrId p,
                     Outputable (Anno (IdGhcP p)))
           => HsConPatDetails (GhcPass p) -> SDoc
pprConArgs (PrefixCon ts pats) = fsep (pprTyArgs ts : map (pprParendLPat appPrec) pats)
  where pprTyArgs tyargs = fsep (map (\ty -> char '@' <> ppr ty) tyargs)
pprConArgs (InfixCon p1 p2)    = sep [ pprParendLPat appPrec p1
                                     , pprParendLPat appPrec p2 ]
pprConArgs (RecCon rpats)      = ppr rpats

{-
************************************************************************
*                                                                      *
*              Building patterns
*                                                                      *
************************************************************************
-}

mkPrefixConPat :: DataCon ->
                  [LPat GhcTc] -> [Type] -> LPat GhcTc
-- Make a vanilla Prefix constructor pattern
mkPrefixConPat dc pats tys
  = noLocA $ ConPat { pat_con = noLocA (RealDataCon dc)
                    , pat_args = PrefixCon [] pats
                    , pat_con_ext = ConPatTc
                      { cpt_tvs = []
                      , cpt_dicts = []
                      , cpt_binds = emptyTcEvBinds
                      , cpt_arg_tys = tys
                      , cpt_wrap = idHsWrapper
                      }
                    }

mkNilPat :: Type -> LPat GhcTc
mkNilPat ty = mkPrefixConPat nilDataCon [] [ty]

mkCharLitPat :: SourceText -> Char -> LPat GhcTc
mkCharLitPat src c = mkPrefixConPat charDataCon
                          [noLocA $ LitPat noExtField (HsCharPrim src c)] []

{-
************************************************************************
*                                                                      *
* Predicates for checking things about pattern-lists in EquationInfo   *
*                                                                      *
************************************************************************

\subsection[Pat-list-predicates]{Look for interesting things in patterns}

Unlike in the Wadler chapter, where patterns are either ``variables''
or ``constructors,'' here we distinguish between:
\begin{description}
\item[unfailable:]
Patterns that cannot fail to match: variables, wildcards, and lazy
patterns.

These are the irrefutable patterns; the two other categories
are refutable patterns.

\item[constructor:]
A non-literal constructor pattern (see next category).

\item[literal patterns:]
At least the numeric ones may be overloaded.
\end{description}

A pattern is in {\em exactly one} of the above three categories; `as'
patterns are treated specially, of course.

The 1.3 report defines what ``irrefutable'' and ``failure-free'' patterns are.
-}
isBangedLMatchPat :: LMatchPat (GhcPass p) -> Bool
isBangedLMatchPat (L _ (VisPat _ pat)) = isBangedLPat pat
isBangedLMatchPat _                    = False

isBangedLPat :: LPat (GhcPass p) -> Bool
isBangedLPat = isBangedPat . unLoc

isBangedPat :: Pat (GhcPass p) -> Bool
isBangedPat (ParPat _ _ p _) = isBangedLPat p
isBangedPat (BangPat {}) = True
isBangedPat _            = False

looksLazyPatBind :: HsBind (GhcPass p) -> Bool
-- Returns True of anything *except*
--     a StrictHsBind (as above) or
--     a VarPat
-- In particular, returns True of a pattern binding with a compound pattern, like (I# x)
-- Looks through AbsBinds
looksLazyPatBind (PatBind { pat_lhs = p })
  = looksLazyLPat p
looksLazyPatBind (AbsBinds { abs_binds = binds })
  = anyBag (looksLazyPatBind . unLoc) binds
looksLazyPatBind _
  = False

looksLazyLPat :: LPat (GhcPass p) -> Bool
looksLazyLPat = looksLazyPat . unLoc

looksLazyPat :: Pat (GhcPass p) -> Bool
looksLazyPat (ParPat _ _ p _)  = looksLazyLPat p
looksLazyPat (AsPat _ _ p)     = looksLazyLPat p
looksLazyPat (BangPat {})  = False
looksLazyPat (VarPat {})   = False
looksLazyPat (WildPat {})  = False
looksLazyPat _             = True

isIrrefutableHsPat :: forall p. (OutputableBndrId p)
                   => DynFlags -> LPat (GhcPass p) -> Bool
-- (isIrrefutableHsPat p) is true if matching against p cannot fail,
-- in the sense of falling through to the next pattern.
--      (NB: this is not quite the same as the (silly) defn
--      in 3.17.2 of the Haskell 98 report.)
--
-- WARNING: isIrrefutableHsPat returns False if it's in doubt.
-- Specifically on a ConPatIn, which is what it sees for a
-- (LPat Name) in the renamer, it doesn't know the size of the
-- constructor family, so it returns False.  Result: only
-- tuple patterns are considered irrefutable at the renamer stage.
--
-- But if it returns True, the pattern is definitely irrefutable
isIrrefutableHsPat dflags =
    isIrrefutableHsPat' (xopt LangExt.Strict dflags)

{-
Note [-XStrict and irrefutability]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When -XStrict is enabled the rules for irrefutability are slightly modified.
Specifically, the pattern in a program like

    do ~(Just hi) <- expr

cannot be considered irrefutable. The ~ here merely disables the bang that
-XStrict would usually apply, rendering the program equivalent to the following
without -XStrict

    do Just hi <- expr

To achieve make this pattern irrefutable with -XStrict the user would rather
need to write

    do ~(~(Just hi)) <- expr

Failing to account for this resulted in #19027. To fix this isIrrefutableHsPat
takes care to check for two the irrefutability of the inner pattern when it
encounters a LazyPat and -XStrict is enabled.

See also Note [decideBangHood] in GHC.HsToCore.Utils.
-}

isIrrefutableHsPat' :: forall p. (OutputableBndrId p)
                    => Bool -- ^ Are we in a @-XStrict@ context?
                            -- See Note [-XStrict and irrefutability]
                    -> LPat (GhcPass p) -> Bool
isIrrefutableHsPat' is_strict = goL
  where
    goL :: LPat (GhcPass p) -> Bool
    goL = go . unLoc

    go :: Pat (GhcPass p) -> Bool
    go (WildPat {})        = True
    go (VarPat {})         = True
    go (LazyPat _ p')
      | is_strict
      = isIrrefutableHsPat' False p'
      | otherwise          = True
    go (BangPat _ pat)     = goL pat
    go (ParPat _ _ pat _)  = goL pat
    go (AsPat _ _ pat)     = goL pat
    go (ViewPat _ _ pat)   = goL pat
    go (SigPat _ pat _)    = goL pat
    go (TuplePat _ pats _) = all goL pats
    go (SumPat {})         = False
                    -- See Note [Unboxed sum patterns aren't irrefutable]
    go (ListPat {})        = False

    go (ConPat
        { pat_con  = con
        , pat_args = details })
                           = case ghcPass @p of
       GhcPs -> False -- Conservative
       GhcRn -> False -- Conservative
       GhcTc -> case con of
         L _ (PatSynCon _pat)  -> False -- Conservative
         L _ (RealDataCon con) ->
           isJust (tyConSingleDataCon_maybe (dataConTyCon con))
           && all goL (hsConPatArgs details)
    go (LitPat {})         = False
    go (NPat {})           = False
    go (NPlusKPat {})      = False

    -- We conservatively assume that no TH splices are irrefutable
    -- since we cannot know until the splice is evaluated.
    go (SplicePat {})      = False

    go (XPat ext)          = case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
      GhcPs -> dataConCantHappen ext
#endif
      GhcRn -> case ext of
        HsPatExpanded _ pat -> go pat
      GhcTc -> case ext of
        CoPat _ pat _ -> go pat
        ExpansionPat _ pat -> go pat

-- | Is the pattern any of combination of:
--
-- - (pat)
-- - pat :: Type
-- - ~pat
-- - !pat
-- - x (variable)
isSimplePat :: LPat (GhcPass x) -> Maybe (IdP (GhcPass x))
isSimplePat p = case unLoc p of
  ParPat _ _ x _ -> isSimplePat x
  SigPat _ x _ -> isSimplePat x
  LazyPat _ x -> isSimplePat x
  BangPat _ x -> isSimplePat x
  VarPat _ x -> Just (unLoc x)
  _ -> Nothing


{- Note [Unboxed sum patterns aren't irrefutable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Unlike unboxed tuples, unboxed sums are *not* irrefutable when used as
patterns. A simple example that demonstrates this is from #14228:

  pattern Just' x = (# x | #)
  pattern Nothing' = (# | () #)

  foo x = case x of
    Nothing' -> putStrLn "nothing"
    Just'    -> putStrLn "just"

In foo, the pattern Nothing' (that is, (# x | #)) is certainly not irrefutable,
as does not match an unboxed sum value of the same arity—namely, (# | y #)
(covered by Just'). In fact, no unboxed sum pattern is irrefutable, since the
minimum unboxed sum arity is 2.

Failing to mark unboxed sum patterns as non-irrefutable would cause the Just'
case in foo to be unreachable, as GHC would mistakenly believe that Nothing'
is the only thing that could possibly be matched!
-}

-- | @'patNeedsParens' p pat@ returns 'True' if the pattern @pat@ needs
-- parentheses under precedence @p@.
patNeedsParens :: forall p. IsPass p => PprPrec -> Pat (GhcPass p) -> Bool
patNeedsParens p = go @p
  where
    -- Remark: go needs to be polymorphic, as we call it recursively
    -- at a different GhcPass (see the case for GhcTc XPat below).
    go :: forall q. IsPass q => Pat (GhcPass q) -> Bool
    go (NPlusKPat {})    = p > opPrec
    go (SplicePat {})    = False
    go (ConPat { pat_args = ds })
                         = conPatNeedsParens p ds
    go (SigPat {})       = p >= sigPrec
    go (ViewPat {})      = True
    go (XPat ext)        = case ghcPass @q of
#if __GLASGOW_HASKELL__ < 901
      GhcPs -> dataConCantHappen ext
#endif
      GhcRn -> case ext of
        HsPatExpanded orig _ -> go orig
      GhcTc -> case ext of
        CoPat _ inner _ -> go inner
        ExpansionPat orig _ -> go orig
          --                   ^^^^^^^
          -- NB: recursive call of go at a different GhcPass.
    go (WildPat {})      = False
    go (VarPat {})       = False
    go (LazyPat {})      = False
    go (BangPat {})      = False
    go (ParPat {})       = False
    go (AsPat {})        = False
    -- Special-case unary boxed tuple applications so that they are
    -- parenthesized as `Identity (Solo x)`, not `Identity Solo x` (#18612)
    -- See Note [One-tuples] in GHC.Builtin.Types
    go (TuplePat _ [_] Boxed)
                         = p >= appPrec
    go (TuplePat{})      = False
    go (SumPat {})       = False
    go (ListPat {})      = False
    go (LitPat _ l)      = hsLitNeedsParens p l
    go (NPat _ lol _ _)  = hsOverLitNeedsParens p (unLoc lol)

-- | @'conPatNeedsParens' p cp@ returns 'True' if the constructor patterns @cp@
-- needs parentheses under precedence @p@.
conPatNeedsParens :: PprPrec -> HsConDetails t a b -> Bool
conPatNeedsParens p = go
  where
    go (PrefixCon ts args) = p >= appPrec && (not (null args) || not (null ts))
    go (InfixCon {})       = p >= opPrec -- type args should be empty in this case
    go (RecCon {})         = False


-- | Parenthesize a pattern without token information
gParPat :: LPat (GhcPass pass) -> Pat (GhcPass pass)
gParPat p = ParPat noAnn noHsTok p noHsTok

-- | @'parenthesizePat' p pat@ checks if @'patNeedsParens' p pat@ is true, and
-- if so, surrounds @pat@ with a 'ParPat'. Otherwise, it simply returns @pat@.
parenthesizePat :: IsPass p
                => PprPrec
                -> LPat (GhcPass p)
                -> LPat (GhcPass p)
parenthesizePat p lpat@(L loc pat)
  | patNeedsParens p pat = L loc (gParPat lpat)
  | otherwise            = lpat

parenthesizeLMatchPat :: IsPass p
                   => PprPrec
                   -> LMatchPat (GhcPass p)
                   -> LMatchPat (GhcPass p)
parenthesizeLMatchPat p (L l (VisPat x lpat)) =
  L l (VisPat x (parenthesizePat p lpat))
parenthesizeLMatchPat _ invis         = invis

{-
% Collect all EvVars from all constructor patterns
-}

-- May need to add more cases
collectEvVarsPats :: [Pat GhcTc] -> Bag EvVar
collectEvVarsPats = unionManyBags . map collectEvVarsPat

collectEvVarsLPat :: LPat GhcTc -> Bag EvVar
collectEvVarsLPat = collectEvVarsPat . unLoc

collectEvVarsPat :: Pat GhcTc -> Bag EvVar
collectEvVarsPat pat =
  case pat of
    LazyPat _ p      -> collectEvVarsLPat p
    AsPat _ _ p      -> collectEvVarsLPat p
    ParPat  _ _ p _  -> collectEvVarsLPat p
    BangPat _ p      -> collectEvVarsLPat p
    ListPat _ ps     -> unionManyBags $ map collectEvVarsLPat ps
    TuplePat _ ps _  -> unionManyBags $ map collectEvVarsLPat ps
    SumPat _ p _ _   -> collectEvVarsLPat p
    ConPat
      { pat_args  = args
      , pat_con_ext = ConPatTc
        { cpt_dicts = dicts
        }
      }
                     -> unionBags (listToBag dicts)
                                   $ unionManyBags
                                   $ map collectEvVarsLPat
                                   $ hsConPatArgs args
    SigPat  _ p _    -> collectEvVarsLPat p
    XPat ext -> case ext of
      CoPat _ p _      -> collectEvVarsPat p
      ExpansionPat _ p -> collectEvVarsPat p
    _other_pat       -> emptyBag

{-
************************************************************************
*                                                                      *
\subsection{Anno instances}
*                                                                      *
************************************************************************
-}

type instance Anno (Pat (GhcPass p)) = SrcSpanAnnA
type instance Anno (MatchPat (GhcPass p)) = SrcSpanAnnA
type instance Anno (HsOverLit (GhcPass p)) = SrcAnn NoEpAnns
type instance Anno ConLike = SrcSpanAnnN
type instance Anno (HsFieldBind lhs rhs) = SrcSpanAnnA
