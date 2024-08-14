{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PatSyntax]{Abstract Haskell syntax---patterns}
-}

module GHC.Hs.Pat (
        Pat(..), LPat,
        isInvisArgPat, isVisArgPat,
        EpAnnSumPat(..),
        ConPatTc (..),
        ConLikeP,
        HsPatExpansion(..),
        XXPatGhcTc(..),

        HsConPatDetails, hsConPatArgs, hsConPatTyArgs,
        HsConPatTyArg(..),
        HsRecFields(..), HsFieldBind(..), LHsFieldBind,
        HsRecField, LHsRecField,
        HsRecUpdField, LHsRecUpdField,
        RecFieldsDotDot(..),
        hsRecFields, hsRecFieldSel, hsRecFieldId, hsRecFieldsArgs,
        hsRecUpdFieldId, hsRecUpdFieldOcc, hsRecUpdFieldRdr,

        mkPrefixConPat, mkCharLitPat, mkNilPat,

        isSimplePat, isPatSyn,
        looksLazyPatBind,
        isBangedLPat,
        gParPat, patNeedsParens, parenthesizePat,
        isIrrefutableHsPat,

        isBoringHsPat,

        collectEvVarsPat, collectEvVarsPats,

        pprParendLPat, pprConArgs,
        pprLPat
    ) where

import GHC.Prelude

import Language.Haskell.Syntax.Pat
import Language.Haskell.Syntax.Expr ( HsExpr )

import {-# SOURCE #-} GHC.Hs.Expr (pprLExpr, pprUntypedSplice, HsUntypedSpliceResult(..))

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
import GHC.Types.Name.Reader
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Utils.Outputable
import GHC.Core.Type
import GHC.Types.SrcLoc
import GHC.Data.Bag -- collect ev vars from pats
import GHC.Types.Name
import Data.Data

import qualified Data.List.NonEmpty as NE

type instance XWildPat GhcPs = NoExtField
type instance XWildPat GhcRn = NoExtField
type instance XWildPat GhcTc = Type

type instance XVarPat  (GhcPass _) = NoExtField

type instance XLazyPat GhcPs = [AddEpAnn] -- For '~'
type instance XLazyPat GhcRn = NoExtField
type instance XLazyPat GhcTc = NoExtField

type instance XAsPat   GhcPs = EpToken "@"
type instance XAsPat   GhcRn = NoExtField
type instance XAsPat   GhcTc = NoExtField

type instance XParPat  GhcPs = (EpToken "(", EpToken ")")
type instance XParPat  GhcRn = NoExtField
type instance XParPat  GhcTc = NoExtField

type instance XBangPat GhcPs = [AddEpAnn] -- For '!'
type instance XBangPat GhcRn = NoExtField
type instance XBangPat GhcTc = NoExtField

type instance XListPat GhcPs = AnnList
  -- After parsing, ListPat can refer to a built-in Haskell list pattern
  -- or an overloaded list pattern.
type instance XListPat GhcRn = NoExtField
  -- Built-in list patterns only.
  -- After renaming, overloaded list patterns are expanded to view patterns.
  -- See Note [Desugaring overloaded list patterns]
type instance XListPat GhcTc = Type
  -- List element type, for use in hsPatType.

type instance XTuplePat GhcPs = [AddEpAnn]
type instance XTuplePat GhcRn = NoExtField
type instance XTuplePat GhcTc = [Type]

type instance XOrPat GhcPs = NoExtField
type instance XOrPat GhcRn = NoExtField
type instance XOrPat GhcTc = Type

type instance XSumPat GhcPs = EpAnnSumPat
type instance XSumPat GhcRn = NoExtField
type instance XSumPat GhcTc = [Type]

type instance XConPat GhcPs = [AddEpAnn]
type instance XConPat GhcRn = NoExtField
type instance XConPat GhcTc = ConPatTc

type instance XViewPat GhcPs = EpUniToken "->" "→"
type instance XViewPat GhcRn = Maybe (HsExpr GhcRn)
  -- The @HsExpr GhcRn@ gives an inverse to the view function.
  -- This is used for overloaded lists in particular.
  -- See Note [Invertible view patterns] in GHC.Tc.TyCl.PatSyn.

type instance XViewPat GhcTc = Type
  -- Overall type of the pattern
  -- (= the argument type of the view function), for hsPatType.

type instance XSplicePat GhcPs = NoExtField
type instance XSplicePat GhcRn = HsUntypedSpliceResult (Pat GhcRn) -- See Note [Lifecycle of a splice] in GHC.Hs.Expr
type instance XSplicePat GhcTc = DataConCantHappen

type instance XLitPat    (GhcPass _) = NoExtField

type instance XNPat GhcPs = [AddEpAnn]
type instance XNPat GhcRn = [AddEpAnn]
type instance XNPat GhcTc = Type

type instance XNPlusKPat GhcPs = EpaLocation -- Of the "+"
type instance XNPlusKPat GhcRn = NoExtField
type instance XNPlusKPat GhcTc = Type

type instance XSigPat GhcPs = [AddEpAnn]
type instance XSigPat GhcRn = NoExtField
type instance XSigPat GhcTc = Type

type instance XEmbTyPat GhcPs = EpToken "type"
type instance XEmbTyPat GhcRn = NoExtField
type instance XEmbTyPat GhcTc = Type

type instance XXPat GhcPs = DataConCantHappen
type instance XXPat GhcRn = HsPatExpansion (Pat GhcRn) (Pat GhcRn)
  -- Original pattern and its desugaring/expansion.
  -- See Note [Rebindable syntax and XXExprGhcRn].
type instance XXPat GhcTc = XXPatGhcTc
  -- After typechecking, we add extra constructors: CoPat and XXExprGhcRn.
  -- XXExprGhcRn allows us to handle RebindableSyntax in pattern position:
  -- see "XXExpr GhcTc" for the counterpart in expressions.

type instance ConLikeP GhcPs = RdrName -- IdP GhcPs
type instance ConLikeP GhcRn = Name    -- IdP GhcRn
type instance ConLikeP GhcTc = ConLike

type instance XConPatTyArg GhcPs = EpToken "@"
type instance XConPatTyArg GhcRn = NoExtField
type instance XConPatTyArg GhcTc = NoExtField

type instance XHsRecFields GhcPs = NoExtField
type instance XHsRecFields GhcRn = NoExtField
type instance XHsRecFields GhcTc = MultiplicityCheckCoercions

type instance XHsFieldBind _ = [AddEpAnn]

type instance XInvisPat GhcPs = EpToken "@"
type instance XInvisPat GhcRn = NoExtField
type instance XInvisPat GhcTc = Type


{- Note [Invisible binders in functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC Proposal #448 (section 1.5 Type arguments in lambda patterns) introduces
binders for invisible type arguments (@a-binders) in function equations and
lambdas, e.g.

  1.  {-# LANGUAGE TypeAbstractions #-}
      id1 :: a -> a
      id1 @t x = x :: t     -- @t-binder on the LHS of a function equation

  2.  {-# LANGUAGE TypeAbstractions #-}
      ex :: (Int8, Int16)
      ex = higherRank (\ @a x -> maxBound @a - x )
                            -- @a-binder in a lambda pattern in an argument
                            -- to a higher-order function
      higherRank :: (forall a. (Num a, Bounded a) => a -> a) -> (Int8, Int16)
      higherRank f = (f 42, f 42)

In the AST, invisible patterns are represented as InvisPat constructor inside of Pat:
    data Pat p
      = ...
      | InvisPat (LHsType p)
      ...

Just like `BangPat`, the `Pat` data type allows `InvisPat` to appear in
nested positions. But this is often not allowed; e.g.

   f @a x = rhs    -- YES
   f (@a,x) = rhs  -- NO

   g = do { @a <- e1; e2 }         -- NO
   h x = case x of { @a -> rhs }   -- NO

Rather than excluding these things syntactically, we reject them in the renamer
(see `rn_pats_general`).  This actually gives a better error message than we
would get if they were rejected in the parser.

Each pattern is either visible (not prefixed with @) or invisible (prefixed with @):
    f :: forall a. forall b -> forall c. Int -> ...
    f @a b @c x  = ...

In this example, the arg-patterns are
    1. InvisPat @a     -- in the type sig: forall a.
    2. VarPat b        -- in the type sig: forall b ->
    3. InvisPat @c     -- in the type sig: forall c.
    4. VarPat x        -- in the type sig: Int ->

Invisible patterns are always type patterns, i.e. they are matched with
forall-bound type variables in the signature. Consequently, those variables (and
their binders) are erased during compilation, having no effect on program
execution at runtime.

Visible patterns, on the other hand, may be matched with ordinary function
arguments (Int ->) as well as required type arguments (forall b ->). This means
that a visible pattern may either be erased or retained, and we only find out in
the type checker, namely in tcMatchPats, where we match up all arg-patterns with
quantifiers from the type signature.

In other words, invisible patterns are always /erased/, while visible patterns
are sometimes /erased/ and sometimes /retained/.

The desugarer has no use for erased patterns, as the type checker generates
HsWrappers to bind the corresponding type variables. Erased patterns are simply
discarded inside tcMatchPats, where we know if visible pattern retained or erased.
-}

-- ---------------------------------------------------------------------

-- API Annotations types

data EpAnnSumPat = EpAnnSumPat
      { sumPatParens      :: [AddEpAnn]
      , sumPatVbarsBefore :: [EpaLocation]
      , sumPatVbarsAfter  :: [EpaLocation]
      } deriving Data

instance NoAnn EpAnnSumPat where
  noAnn = EpAnnSumPat [] [] []

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
  -- See Note [Rebindable syntax and XXExprGhcRn].
  | ExpansionPat (Pat GhcRn) (Pat GhcTc)


-- See Note [Rebindable syntax and XXExprGhcRn].
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

    , -- | Extra wrapper to pass to the matcher
      -- Only relevant for pattern-synonyms;
      --   ignored for data cons
      cpt_wrap  :: HsWrapper
    }

hsRecFieldId :: HsRecField GhcTc arg -> Id
hsRecFieldId = hsRecFieldSel

hsRecUpdFieldRdr :: HsRecUpdField (GhcPass p) q -> Located RdrName
hsRecUpdFieldRdr = fmap ambiguousFieldOccRdrName . reLoc . hfbLHS

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

instance Outputable (HsTyPat p) => Outputable (HsConPatTyArg p) where
  ppr (HsConPatTyArg _ ty) = char '@' <> ppr ty

instance (Outputable arg, Outputable (XRec p (HsRecField p arg)), XRec p RecFieldsDotDot ~ LocatedE RecFieldsDotDot)
      => Outputable (HsRecFields p arg) where
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Nothing })
        = braces (fsep (punctuate comma (map ppr flds)))
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Just (unLoc -> RecFieldsDotDot n) })
        = braces (fsep (punctuate comma (map ppr (take n flds) ++ [dotdot])))
        where
          dotdot = text ".." <+> whenPprDebug (ppr (drop n flds))

instance (Outputable p, OutputableBndr p, Outputable arg)
      => Outputable (HsFieldBind p arg) where
  ppr (HsFieldBind { hfbLHS = f, hfbRHS = arg,
                     hfbPun = pun })
    = pprPrefixOcc f <+> (ppUnless pun $ equals <+> ppr arg)

instance OutputableBndrId p => Outputable (Pat (GhcPass p)) where
    ppr = pprPat

-- See Note [Rebindable syntax and XXExprGhcRn].
instance (Outputable a, Outputable b) => Outputable (HsPatExpansion a b) where
  ppr (HsPatExpanded a b) = ifPprDebug (vcat [ppr a, ppr b]) (ppr a)

pprLPat :: (OutputableBndrId p) => LPat (GhcPass p) -> SDoc
pprLPat (L _ e) = pprPat e

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
pprPat (ParPat _ pat)           = parens (ppr pat)
pprPat (LitPat _ s)             = ppr s
pprPat (NPat _ l Nothing  _)    = ppr l
pprPat (NPat _ l (Just _) _)    = char '-' <> ppr l
pprPat (NPlusKPat _ n k _ _ _)  = hcat [ppr_n, char '+', ppr k]
  where ppr_n = case ghcPass @p of
                  GhcPs -> ppr n
                  GhcRn -> ppr n
                  GhcTc -> ppr n
pprPat (SplicePat ext splice)   =
    case ghcPass @p of
      GhcPs -> pprUntypedSplice True Nothing splice
      GhcRn | HsUntypedSpliceNested n <- ext -> pprUntypedSplice True (Just n) splice
      GhcRn | HsUntypedSpliceTop _ p  <- ext -> ppr p
      GhcTc -> dataConCantHappen ext
pprPat (SigPat _ pat ty)        = ppr pat <+> dcolon <+> ppr ty
pprPat (ListPat _ pats)         = brackets (interpp'SP pats)
pprPat (OrPat _ pats)           = pprWithSemis ppr (NE.toList pats)
pprPat (TuplePat _ pats bx)
    -- Special-case unary boxed tuples so that they are pretty-printed as
    -- `MkSolo x`, not `(x)`
  | [pat] <- pats
  , Boxed <- bx
  = hcat [text (mkTupleStr Boxed dataName 1), pprParendLPat appPrec pat]
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
pprPat (EmbTyPat _ tp) = text "type" <+> ppr tp
pprPat (InvisPat _ tp) = char '@' <> ppr tp

pprPat (XPat ext) = case ghcPass @p of
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
  where pprTyArgs tyargs = fsep (map ppr tyargs)
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

isBangedLPat :: LPat (GhcPass p) -> Bool
isBangedLPat = isBangedPat . unLoc

isBangedPat :: Pat (GhcPass p) -> Bool
isBangedPat (ParPat _ p) = isBangedLPat p
isBangedPat (BangPat {}) = True
isBangedPat _            = False

looksLazyPatBind :: HsBind GhcTc -> Bool
-- Returns True of anything *except*
--     a StrictHsBind (as above) or
--     a VarPat
-- In particular, returns True of a pattern binding with a compound pattern, like (I# x)
-- Looks through AbsBinds
looksLazyPatBind (PatBind { pat_lhs = p })
  = looksLazyLPat p
looksLazyPatBind (XHsBindsLR (AbsBinds { abs_binds = binds }))
  = any (looksLazyPatBind . unLoc) binds
looksLazyPatBind _
  = False

looksLazyLPat :: LPat (GhcPass p) -> Bool
looksLazyLPat = looksLazyPat . unLoc

looksLazyPat :: Pat (GhcPass p) -> Bool
looksLazyPat (ParPat _ p)  = looksLazyLPat p
looksLazyPat (AsPat _ _ p) = looksLazyLPat p
looksLazyPat (BangPat {})  = False
looksLazyPat (VarPat {})   = False
looksLazyPat (WildPat {})  = False
looksLazyPat _             = True

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

-- | @isIrrefutableHsPat p@ is true if matching against @p@ cannot fail
-- in the sense of falling through to the next pattern.
--      (NB: this is not quite the same as the (silly) defn
--      in 3.17.2 of the Haskell 98 report.)
--
-- If isIrrefutableHsPat returns 'True', the pattern is definitely irrefutable.
--
-- However, isIrrefutableHsPat returns 'False' if it's in doubt. It's a
-- best effort guess with the information we have available:
--
--  - we sometimes call 'isIrrefutableHsPat' from the renamer, in which case
--    we don't have type information to hand. This means we can't properly
--    handle GADTs, nor the result TyCon of COMPLETE pragmas.
--  - even when calling 'isIrrefutableHsPat' in the typechecker, we don't keep
--    track of any long distance information like the pattern-match checker does.
isIrrefutableHsPat
  :: forall p
  .  IsPass p
  => Bool                           -- ^ Are we in a @-XStrict@ context?
                                    -- See Note [-XStrict and irrefutability]
  -> (ConLikeP (GhcPass p) -> Bool) -- ^ How to check whether the 'ConLike' in a
                                    -- 'ConPat' pattern is irrefutable
  -> LPat (GhcPass p)               -- ^ The (located) pattern to check
  -> Bool                           -- Is it irrefutable?
isIrrefutableHsPat is_strict irref_conLike pat = go (unLoc pat)
  where
    goL (L _ p) = go p

    go :: Pat (GhcPass p) -> Bool
    go (WildPat {})        = True
    go (VarPat {})         = True
    go (LazyPat _ p')
      | is_strict
      = isIrrefutableHsPat False irref_conLike p'
      | otherwise          = True
    go (BangPat _ pat)     = goL pat
    go (ParPat _ pat)      = goL pat
    go (AsPat _ _ pat)     = goL pat
    go (ViewPat _ _ pat)   = goL pat
    go (SigPat _ pat _)    = goL pat
    go (TuplePat _ pats _) = all goL pats
    go (OrPat _ pats)      = any goL pats -- This is simplistic; see Note [Irrefutable or-patterns]
    go (SumPat {})         = False -- See Note [Unboxed sum patterns aren't irrefutable]
    go (ListPat {})        = False

    -- See Note [Irrefutability of ConPat]
    go (ConPat { pat_con = L _ con, pat_args = details })
                           =  irref_conLike con
                           && all goL (hsConPatArgs details)
    go (LitPat {})         = False
    go (NPat {})           = False
    go (NPlusKPat {})      = False

    -- We conservatively assume that no TH splices are irrefutable
    -- since we cannot know until the splice is evaluated.
    go (SplicePat {})      = False

    -- The behavior of this case is unimportant, as GHC will throw an error shortly
    -- after reaching this case for other reasons (see TcRnIllegalTypePattern).
    go (EmbTyPat {})       = True
    go (InvisPat {})       = True

    go (XPat ext)          = case ghcPass @p of
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
  ParPat _ x -> isSimplePat x
  SigPat _ x _ -> isSimplePat x
  LazyPat _ x -> isSimplePat x
  BangPat _ x -> isSimplePat x
  VarPat _ x -> Just (unLoc x)
  _ -> Nothing

-- | Is this pattern boring from the perspective of pattern-match checking,
-- i.e. introduces no new pieces of long-distance information
-- which could influence pattern-match checking?
--
-- See Note [Boring patterns].
isBoringHsPat :: forall p. OutputableBndrId p => LPat (GhcPass p) -> Bool
-- NB: it's always safe to return 'False' in this function; that just means
-- performing potentially-redundant pattern-match checking.
isBoringHsPat = goL
  where
    goL :: forall p. OutputableBndrId p => LPat (GhcPass p) -> Bool
    goL = go . unLoc

    go :: forall p. OutputableBndrId p => Pat (GhcPass p) -> Bool
    go = \case
      WildPat {} -> True
      VarPat  {} -> True
      LazyPat {} -> True
      BangPat _ pat     -> goL pat
      ParPat _ pat      -> goL pat
      AsPat {} -> False -- the pattern x@y links x and y together,
                        -- which is a nontrivial piece of information
      ViewPat _ _ pat   -> goL pat
      SigPat _ pat _    -> goL pat
      TuplePat _ pats _ -> all goL pats
      SumPat  _ pat _ _ -> goL pat
      ListPat _ pats    -> all goL pats
      ConPat { pat_con = con, pat_args = details }
        -> case ghcPass @p of
            GhcPs -> False -- conservative
            GhcRn -> False -- conservative
            GhcTc
              | isVanillaConLike (unLoc con)
              -> all goL (hsConPatArgs details)
              | otherwise
              -- A pattern match on a GADT constructor can introduce
              -- type-level information (for example, T18572).
              -> False
      OrPat _ pats  -> all goL pats
      LitPat {}     -> True
      NPat {}       -> True
      NPlusKPat {}  -> True
      SplicePat {}  -> False
      EmbTyPat {}   -> True
      InvisPat {}   -> True
      XPat ext ->
        case ghcPass @p of
         GhcRn -> case ext of
           HsPatExpanded _ pat -> go pat
         GhcTc -> case ext of
           CoPat _ pat _      -> go pat
           ExpansionPat _ pat -> go pat

isPatSyn :: LPat GhcTc -> Bool
isPatSyn (L _ (ConPat {pat_con = L _ (PatSynCon{})})) = True
isPatSyn _ = False

{- Note [Irrefutability of ConPat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A constructor pattern `ConPat { pat_con, pat_args }` is irrefutable under two
conditions:

  Irref-ConLike: the constructor, pat_con, is itself irrefutable.
  Irref-args   : all of the argument patterns, pat_args, are irrefutable.

The (Irref-ConLike) condition can be stated as follows:

  Irref-DataCon: a DataCon is irrefutable iff it is the only constructor of its
                 parent type constructor.
  Irref-PatSyn:  a PatSyn is irrefutable iff there is a COMPLETE pragma
                 containing this PatSyn as its sole member.

To understand this, let's consider some simple examples:

  data A = MkA Int Bool
  data BC = B Int | C

  pattern P :: Maybe Int -> BC
  pattern P mb_i <- ( ( \ case { B i -> Just i; C -> Nothing } ) -> mb_i )
  {-# COMPLETE P #-}

In this case:

  - the pattern 'A p1 p2' (for patterns 'p1 :: Int', 'p2 :: Bool') is irrefutable
    precisely when both 'p1' and 'p2' are irrefutable (this is the same as
    irrefutability of tuple patterns);
  - neither of the patterns 'B p' (for any pattern 'p :: Int') or 'C' are irrefutable,
    because the parent type constructor 'BC' contains more than one data constructor,
  - the pattern 'P q', for a pattern 'q :: Maybe Int', is irrefutable precisely
    when 'q' is irrefutable, due to the COMPLETE pragma on 'P'.

Wrinkle [Irrefutability and COMPLETE pragma result TyCons]

  There is one subtlety in the Irref-PatSyn condition: COMPLETE pragmas may
  optionally specify a result TyCon, as explained in Note [Implementation of COMPLETE pragmas]
  in GHC.HsToCore.Pmc.Solver.

  So, for a COMPLETE pragma with a result TyCon, we would need to compute
  'completeMatchAppliesAtType' to ensure that the COMPLETE pragma is indeed
  applicable. Doing so is not so straightforward in 'isIrrefutableHsPat', for
  a couple of reasons:

    1. 'isIrrefutableHsPat' is called from within the renamer, which means
       we don't have the appropriate 'Type' to hand,
    2. Even when 'isIrrefutableHsPat' is called from within the typechecker,
       computing 'completeMatchAppliesAtType' for a 'ConPat' which might be
       nested deep inside the top-level call, such as

          ( ( _ , P (x :: Int) ) :: ( Int, Int )

        would require keeping track of types as we recur in 'isIrrefutableHsPat',
        which would be much more involved and require duplicating code from
        the pattern match checker (it performs this check using the notion
        of "match variables", which we don't have in the typechecker).

Note [Irrefutable or-patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When is an or-pattern ( p_1 ; ... ; p_n ) irrefutable? It certainly suffices
that individual pattern p_i is irrefutable, but it isn't necessary.

For example, with the datatype definition

  data ABC = A | B | C

the or-pattern ( B ; C ; A ) is irrefutable. Similarly, one can take into
account COMPLETE pragmas, e.g. (P ; R ; Q) is irrefutable in the presence of
{-# COMPLETE P, Q, R #-}. This would extend Note [Irrefutability of ConPat] to
the case of disjunctions of constructor patterns.

For now, the function 'isIrrefutableHsPat' does not take into account these
additional complications, and considers an or-pattern irrefutable precisely when
any of the summands are irrefutable. This pessimistic behaviour is OK: the contract
of 'isIrrefutableHsPat' is that it can only return 'True' for definitely irrefutable
patterns, but may conservatively return 'False' in other cases.

The justification for this design choice is as follows:

  1. Producing the correct answer in all cases would be rather difficult,
     for example for a complex pattern such as ( P ; !( R ; S ; ( Q :: Ty ) ) ).
  2. Irrefutable or-patterns aren't particularly common or useful, given that
     (currently) or-patterns aren't allowed to bind variables.

Note [Unboxed sum patterns aren't irrefutable]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Boring patterns]
~~~~~~~~~~~~~~~~~~~~~~
A pattern is called boring when no new information is gained upon successfully
matching on the pattern.

Some examples of boring patterns:

  - x, for a variable x. We learn nothing about x upon matching this pattern.
  - Just y. This pattern can fail, but if it matches, we don't learn anything
    about y.

Some examples of non-boring patterns:

  - x@(Just y). A match on this pattern introduces the fact that x is headed
    by the constructor Just, which means that a subsequent pattern match such as

      case x of { Just z -> ... }

    should not be marked as incomplete.
  - a@b. Matching on this pattern introduces a relation between 'a' and 'b',
    which means that we shouldn't emit any warnings in code of the form

      case a of
        True -> case b of { True -> .. } -- no warning here!
        False -> ...
  - GADT patterns. For example, with the GADT

      data G i where { MkGInt :: G Int }

    a match on the pattern 'MkGInt' introduces type-level information:

      foo :: G i -> i
      foo MkGInt = 3

    Here we learn that i ~ Int after matching on 'MkGInt', so this pattern
    is not boring.

When a pattern is boring, and we are only interested in additional long-distance
information (not whether the pattern itself is fallible), we can skip pattern-match
checking entirely. Doing this saves about 10% allocations in test T11195.

This happens when we are checking pattern-matches in do-notation, for example:

  do { x@(Just y) <- z
     ; ...
     ; return $ case x of { Just w -> ... } }

Here we *do not* want to emit a pattern-match warning on the first line for the
incomplete pattern-match, as incompleteness inside do-notation is handled
using MonadFail. However, we still want to propagate the fact that x is headed
by the 'Just' constructor, to avoid a pattern-match warning on the last line.

Note [Implementation of OrPatterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This Note describes the implementation of the extension -XOrPatterns.

* Proposal: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0522-or-patterns.rst
* Discussion: https://github.com/ghc-proposals/ghc-proposals/pull/522 and others

Parser
------
We parse an or-pattern `pat_1; ...; pat_k` into `OrPat [pat_1, ..., pat_k]`,
where `OrPat` is a constructor of `Pat` in Language.Haskell.Syntax.Pat.
We occasionally refer to any of the `pat_k` as "pattern alternatives" below.
The changes to the parser are as outlined in Section 8.1 of the proposal.
The main productions are

  orpats -> exp | exp ';' orpats
  aexp2 -> '(' orpats ')'
  pat -> orpats

Renamer and typechecker
-----------------------
The typing rule for or-patterns in terms of pattern types is

                   Γ0, Σ0 ⊢ pat_i : τ ⤳ Γ0,Σi,Ψi
            --------------------------------------------
            Γ0, Σ0 ⊢ ( pat_1; ...; pat_n ) : τ ⤳ Γ0,Σ0,∅

(See the proposal for what a pattern type `Γ, Σ ⊢ pat : τ ⤳ Γ,Σ,Ψ` is.)
The main points

  * None of the patterns may bind any variables, hence the same Γ0 in both input
    and output.
  * Any Given constraints bound by the pattern are discarded: the rule discards
    the Σi returned by each pattern.
  * Similarly any existentials Ψi bound by the pattern are discarded.

In GHC.Rename.Pat.rnPatAndThen, we reject visible term and type binders (i.e.
concerning Γ0).

Regarding the Givens Σi and existenials Ψi (i.e. invisible type binders)
introduced by the pattern alternatives `pat_i`, we discard them in
GHC.Tc.Gen.Pats.tc_pat in a manner similar to LazyPats;
see Note [Hopping the LIE in lazy patterns].

Why is it useful to allow Σi and Ψi only to discard them immediately after?
Consider

  data T a where MkT :: forall a x. Num a => x -> T a
  foo :: T a -> a
  foo (MkT{}; MkT{}) = 3

We do want to allow matching on MkT{} in or-patterns, despite them invisibly
binding an existential type variable `x` and a new Given constraint `Num a`.
Clearly, `x` must be dead in the RHS of foo, because there is no field binder
that brings it to life, so no harm done.
But we must be careful not to solve the `Num a` Wanted constraint in the RHS of
foo with the Given constraint from the pattern alternatives, hence we are
Hopping the LIE.

Desugarer
---------
The desugaring of or-patterns is complicated by the fact that we have to avoid
exponential code blowup. Consider
  f (LT; GT) (EQ; GT) = rhs1
  f _        _        = rhs2
The naïve desugaring of or-patterns would explode every or-pattern, thus
  f LT EQ = rhs1
  f LT GT = rhs1
  f GT EQ = rhs1
  f GT GT = rhs1
  f _  _  = rhs2
which leads to an exponential number of copies of `rhs1`.
Our current strategy, implemented in GHC.HsToCore.Match.tidy1, is to
desugar to LambdaCase and ViewPatterns,
  f ((\case LT -> True; GT -> True; _ -> False) -> True)
    ((\case EQ -> True; GT -> True; _ -> False) -> True)
    = rhs1
  f _ _ = rhs2
The existing code for ViewPatterns makes sure that we do not duplicate `rhs1`
and the Simplifier will take care to turn this into efficient code.

Pattern-match checker
---------------------
The changes to the pattern-match checker are described in detail in Section 4.9
of the 2024 revision of the "Lower Your Guards" paper.
What follows is a brief summary of that change.

The pattern-match checker desugars patterns as well, into syntactic variants of
*guard trees* such as `PmMatch`, describing a single Match `f ps | grhss`.
It used to be that each such guard trees nicely captured the effects of pattern
matching `ps` in a conjunctive list of `PmGrd`s, each of which refines
the set of Nablas that reach the RHS of the clause.
`PmGrd` is the heart of the Lower Your Guards approach: it is compositional,
simple, and *non-recursive*, unlike or-patterns!
Conjunction is implemented with the `...Pmc.Check.leftToRight` combinator.
But to desugar or-patterns, we need to compose with `Pmc.Check.topToBottom`
to model first match semantics!
This was previously impossible in the pattern fragment, and indeed is
incompatible with the simple "list of `PmGrd`s" desugaring of patterns.

So our solution is to generalise "sequence of `PmGrd`" into a series-parallel
graph `GrdDag`, a special kind of DAG, where "series" corresponds to
left-to-right sequence and "parallel" corresponds to top-to-bottom or-pattern
alternatives. Example

  f (LT; GT) True (EQ; GT) = rhs

desugars to

   /- LT <- x -\             /- EQ <- z -\
  .             . True <- y .             .-> rhs
   \- GT <- x ./             \- GT <- z -/

Branching is GdAlt and models first-match semantics of or-patterns, and
sequencing is GdSeq.

We must take care of exponential explosion of Covered sets for long matches like
  g (LT; GT) (LT; GT) ... True = 1
Fortunately, we can build on our existing throttling mechanism;
see Note [Countering exponential blowup] in GHC.HsToCore.Pmc.Check.
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
    go (OrPat {})        = p > topPrec
    go (SplicePat {})    = False
    go (ConPat { pat_args = ds })
                         = conPatNeedsParens p ds
    go (SigPat {})       = p >= sigPrec
    go (ViewPat {})      = True
    go (EmbTyPat {})     = True
    go (InvisPat{})      = False
    go (XPat ext)        = case ghcPass @q of
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
gParPat :: forall p. IsPass p => LPat (GhcPass p) -> Pat (GhcPass p)
gParPat pat = ParPat x pat
  where
    x = case ghcPass @p of
      GhcPs -> noAnn
      GhcRn -> noExtField
      GhcTc -> noExtField

-- | @'parenthesizePat' p pat@ checks if @'patNeedsParens' p pat@ is true, and
-- if so, surrounds @pat@ with a 'ParPat'. Otherwise, it simply returns @pat@.
parenthesizePat :: IsPass p
                => PprPrec
                -> LPat (GhcPass p)
                -> LPat (GhcPass p)
parenthesizePat p lpat@(L loc pat)
  | patNeedsParens p pat = L loc (gParPat lpat)
  | otherwise            = lpat


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
    ParPat  _ p      -> collectEvVarsLPat p
    BangPat _ p      -> collectEvVarsLPat p
    ListPat _ ps     -> unionManyBags $ map collectEvVarsLPat ps
    TuplePat _ ps _  -> unionManyBags $ map collectEvVarsLPat ps
    OrPat _ ps       -> unionManyBags $ map collectEvVarsLPat (NE.toList ps)
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
type instance Anno (HsOverLit (GhcPass p)) = EpAnnCO
type instance Anno ConLike = SrcSpanAnnN
type instance Anno (HsFieldBind lhs rhs) = SrcSpanAnnA
type instance Anno RecFieldsDotDot = EpaLocation
