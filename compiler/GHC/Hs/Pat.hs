
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE ViewPatterns         #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PatSyntax]{Abstract Haskell syntax---patterns}
-}

module GHC.Hs.Pat (
        Pat(..), LPat,
        ConPatTc (..),
        CoPat (..),
        ListPatTc(..),
        ConLikeP,

        HsConPatDetails, hsConPatArgs,
        HsRecFields(..), HsRecField'(..), LHsRecField',
        HsRecField, LHsRecField,
        HsRecUpdField, LHsRecUpdField,
        hsRecFields, hsRecFieldSel, hsRecFieldId, hsRecFieldsArgs,
        hsRecUpdFieldId, hsRecUpdFieldOcc, hsRecUpdFieldRdr,

        mkPrefixConPat, mkCharLitPat, mkNilPat,

        isSimplePat,
        looksLazyPatBind,
        isBangedLPat,
        patNeedsParens, parenthesizePat,
        isIrrefutableHsPat,

        collectEvVarsPat, collectEvVarsPats,

        pprParendLPat, pprConArgs
    ) where

import GHC.Prelude

import Language.Haskell.Syntax.Pat
import Language.Haskell.Syntax.Expr (SyntaxExpr)

import {-# SOURCE #-} GHC.Hs.Expr (pprLExpr, pprSplice)

-- friends:
import GHC.Hs.Binds
import GHC.Hs.Lit
import Language.Haskell.Syntax.Extension
import GHC.Hs.Extension.GhcPass
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


data ListPatTc
  = ListPatTc
      Type                             -- The type of the elements
      (Maybe (Type, SyntaxExpr GhcTc)) -- For rebindable syntax

type instance XWildPat GhcPs = NoExtField
type instance XWildPat GhcRn = NoExtField
type instance XWildPat GhcTc = Type

type instance XVarPat  (GhcPass _) = NoExtField
type instance XLazyPat (GhcPass _) = NoExtField
type instance XAsPat   (GhcPass _) = NoExtField
type instance XParPat  (GhcPass _) = NoExtField
type instance XBangPat (GhcPass _) = NoExtField

-- Note: XListPat cannot be extended when using GHC 8.0.2 as the bootstrap
-- compiler, as it triggers https://gitlab.haskell.org/ghc/ghc/issues/14396 for
-- `SyntaxExpr`
type instance XListPat GhcPs = NoExtField
type instance XListPat GhcRn = Maybe (SyntaxExpr GhcRn)
type instance XListPat GhcTc = ListPatTc

type instance XTuplePat GhcPs = NoExtField
type instance XTuplePat GhcRn = NoExtField
type instance XTuplePat GhcTc = [Type]

type instance XConPat GhcPs = NoExtField
type instance XConPat GhcRn = NoExtField
type instance XConPat GhcTc = ConPatTc

type instance XSumPat GhcPs = NoExtField
type instance XSumPat GhcRn = NoExtField
type instance XSumPat GhcTc = [Type]

type instance XViewPat GhcPs = NoExtField
type instance XViewPat GhcRn = NoExtField
type instance XViewPat GhcTc = Type

type instance XSplicePat (GhcPass _) = NoExtField
type instance XLitPat    (GhcPass _) = NoExtField

type instance XNPat GhcPs = NoExtField
type instance XNPat GhcRn = NoExtField
type instance XNPat GhcTc = Type

type instance XNPlusKPat GhcPs = NoExtField
type instance XNPlusKPat GhcRn = NoExtField
type instance XNPlusKPat GhcTc = Type

type instance XSigPat GhcPs = NoExtField
type instance XSigPat GhcRn = NoExtField
type instance XSigPat GhcTc = Type

type instance XXPat GhcPs = NoExtCon
type instance XXPat GhcRn = NoExtCon
type instance XXPat GhcTc = CoPat
  -- After typechecking, we add one extra constructor: CoPat

type instance ConLikeP GhcPs = RdrName -- IdP GhcPs
type instance ConLikeP GhcRn = Name    -- IdP GhcRn
type instance ConLikeP GhcTc = ConLike

-- ---------------------------------------------------------------------

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

-- | Coercion Pattern (translation only)
--
-- During desugaring a (CoPat co pat) turns into a cast with 'co' on the
-- scrutinee, followed by a match on 'pat'.
data CoPat
  = CoPat
    { -- | Coercion Pattern
      -- If co :: t1 ~ t2, p :: t2,
      -- then (CoPat co p) :: t1
      co_cpt_wrap :: HsWrapper

    , -- | Why not LPat?  Ans: existing locn will do
      co_pat_inner :: Pat GhcTc

    , -- | Type of whole pattern, t1
      co_pat_ty :: Type
    }

hsRecFieldId :: HsRecField GhcTc arg -> Located Id
hsRecFieldId = hsRecFieldSel

hsRecUpdFieldRdr :: HsRecUpdField (GhcPass p) -> Located RdrName
hsRecUpdFieldRdr = fmap rdrNameAmbiguousFieldOcc . hsRecFieldLbl

hsRecUpdFieldId :: HsRecField' (AmbiguousFieldOcc GhcTc) arg -> Located Id
hsRecUpdFieldId = fmap extFieldOcc . hsRecUpdFieldOcc

hsRecUpdFieldOcc :: HsRecField' (AmbiguousFieldOcc GhcTc) arg -> LFieldOcc GhcTc
hsRecUpdFieldOcc = fmap unambiguousFieldOcc . hsRecFieldLbl


{-
************************************************************************
*                                                                      *
*              Printing patterns
*                                                                      *
************************************************************************
-}

instance OutputableBndrId p => Outputable (Pat (GhcPass p)) where
    ppr = pprPat

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
      , XPat ext <- pat
      , CoPat {} <- ext
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
pprPat (NPlusKPat _ n k _ _ _)  = hcat [ppr n, char '+', ppr k]
pprPat (SplicePat _ splice)     = pprSplice splice
pprPat (SigPat _ pat ty)        = ppr pat <+> dcolon <+> ppr_ty
  where ppr_ty = case ghcPass @p of
                   GhcPs -> ppr ty
                   GhcRn -> ppr ty
                   GhcTc -> ppr ty
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
      GhcPs -> regular
      GhcRn -> regular
      GhcTc -> sdocOption sdocPrintTypecheckerElaboration $ \case
        False -> regular
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
  where
    regular :: OutputableBndr (ConLikeP (GhcPass p)) => SDoc
    regular = pprUserCon (unLoc con) details
pprPat (XPat ext) = case ghcPass @p of
#if __GLASGOW_HASKELL__ < 811
  GhcPs -> noExtCon ext
  GhcRn -> noExtCon ext
#endif
  GhcTc -> pprHsWrapper co $ \parens ->
      if parens
      then pprParendPat appPrec pat
      else pprPat pat
    where CoPat co pat _ = ext

pprUserCon :: (OutputableBndr con, OutputableBndrId p)
           => con -> HsConPatDetails (GhcPass p) -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details          = pprPrefixOcc c <+> pprConArgs details


pprConArgs :: (OutputableBndrId p)
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
  = noLoc $ ConPat { pat_con = noLoc (RealDataCon dc)
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
                          [noLoc $ LitPat noExtField (HsCharPrim src c)] []

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
looksLazyPat (ParPat _ p)  = looksLazyLPat p
looksLazyPat (AsPat _ _ p) = looksLazyLPat p
looksLazyPat (BangPat {})  = False
looksLazyPat (VarPat {})   = False
looksLazyPat (WildPat {})  = False
looksLazyPat _             = True

isIrrefutableHsPat :: forall p. (OutputableBndrId p) => LPat (GhcPass p) -> Bool
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
isIrrefutableHsPat
  = goL
  where
    goL :: LPat (GhcPass p) -> Bool
    goL = go . unLoc

    go :: Pat (GhcPass p) -> Bool
    go (WildPat {})        = True
    go (VarPat {})         = True
    go (LazyPat {})        = True
    go (BangPat _ pat)     = goL pat
    go (ParPat _ pat)      = goL pat
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
      GhcPs -> noExtCon ext
      GhcRn -> noExtCon ext
#endif
      GhcTc -> go pat
        where CoPat _ pat _ = ext

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
patNeedsParens p = go
  where
    go :: Pat (GhcPass p) -> Bool
    go (NPlusKPat {})    = p > opPrec
    go (SplicePat {})    = False
    go (ConPat { pat_args = ds })
                         = conPatNeedsParens p ds
    go (SigPat {})       = p >= sigPrec
    go (ViewPat {})      = True
    go (XPat ext)        = case ghcPass @p of
#if __GLASGOW_HASKELL__ <= 810
      GhcPs -> noExtCon ext
      GhcRn -> noExtCon ext
#endif
      GhcTc -> go inner
        where CoPat _ inner _ = ext
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

-- | @'parenthesizePat' p pat@ checks if @'patNeedsParens' p pat@ is true, and
-- if so, surrounds @pat@ with a 'ParPat'. Otherwise, it simply returns @pat@.
parenthesizePat :: IsPass p
                => PprPrec
                -> LPat (GhcPass p)
                -> LPat (GhcPass p)
parenthesizePat p lpat@(L loc pat)
  | patNeedsParens p pat = L loc (ParPat noExtField lpat)
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
    XPat (CoPat _ p _) -> collectEvVarsPat  p
    _other_pat       -> emptyBag
