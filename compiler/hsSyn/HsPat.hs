{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PatSyntax]{Abstract Haskell syntax---patterns}
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HsPat (
        Pat(..), InPat, OutPat, LPat,

        HsConPatDetails, hsConPatArgs,
        HsRecFields(..), HsRecField'(..), LHsRecField',
        HsRecField, LHsRecField,
        HsRecUpdField, LHsRecUpdField,
        hsRecFields, hsRecFieldSel, hsRecFieldId, hsRecFieldsArgs,
        hsRecUpdFieldId, hsRecUpdFieldOcc, hsRecUpdFieldRdr,

        mkPrefixConPat, mkCharLitPat, mkNilPat,

        looksLazyPatBind,
        isBangedLPat,
        hsPatNeedsParens,
        isCompoundPat, parenthesizeCompoundPat,
        isIrrefutableHsPat,

        collectEvVarsPats,

        pprParendLPat, pprConArgs
    ) where

import GhcPrelude

import {-# SOURCE #-} HsExpr            (SyntaxExpr, LHsExpr, HsSplice, pprLExpr, pprSplice)

-- friends:
import HsBinds
import HsLit
import HsExtension
import HsTypes
import TcEvidence
import BasicTypes
-- others:
import PprCore          ( {- instance OutputableBndr TyVar -} )
import TysWiredIn
import Var
import RdrName ( RdrName )
import ConLike
import DataCon
import TyCon
import Outputable
import Type
import SrcLoc
import Bag -- collect ev vars from pats
import DynFlags( gopt, GeneralFlag(..) )
import Maybes
-- libraries:
import Data.Data hiding (TyCon,Fixity)

type InPat p  = LPat p        -- No 'Out' constructors
type OutPat p = LPat p        -- No 'In' constructors

type LPat p = Located (Pat p)

-- | Pattern
--
-- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

-- For details on above see note [Api annotations] in ApiAnnotation
data Pat p
  =     ------------ Simple patterns ---------------
    WildPat     (PostTc p Type)        -- ^ Wildcard Pattern
        -- The sole reason for a type on a WildPat is to
        -- support hsPatType :: Pat Id -> Type

       -- AZ:TODO above comment needs to be updated
  | VarPat      (Located (IdP p))  -- ^ Variable Pattern

                             -- See Note [Located RdrNames] in HsExpr
  | LazyPat     (LPat p)                -- ^ Lazy Pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | AsPat       (Located (IdP p)) (LPat p)    -- ^ As pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | ParPat      (LPat p)                -- ^ Parenthesised pattern
                                        -- See Note [Parens in HsSyn] in HsExpr
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
    --                                    'ApiAnnotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in ApiAnnotation
  | BangPat     (LPat p)                -- ^ Bang pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

    -- For details on above see note [Api annotations] in ApiAnnotation

        ------------ Lists, tuples, arrays ---------------
  | ListPat     [LPat p]
                (PostTc p Type)                      -- The type of the elements
                (Maybe (PostTc p Type, SyntaxExpr p)) -- For rebindable syntax
                   -- For OverloadedLists a Just (ty,fn) gives
                   -- overall type of the pattern, and the toList
                   -- function to convert the scrutinee to a list value
    -- ^ Syntactic List
    --
    -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
    --                                    'ApiAnnotation.AnnClose' @']'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | TuplePat    [LPat p]         -- Tuple sub-patterns
                Boxity           -- UnitPat is TuplePat []
                [PostTc p Type]  -- [] before typechecker, filled in afterwards
                                 -- with the types of the tuple components
        -- You might think that the PostTc p Type was redundant, because we can
        -- get the pattern type by getting the types of the sub-patterns.
        -- But it's essential
        --      data T a where
        --        T1 :: Int -> T Int
        --      f :: (T a, a) -> Int
        --      f (T1 x, z) = z
        -- When desugaring, we must generate
        --      f = /\a. \v::a.  case v of (t::T a, w::a) ->
        --                       case t of (T1 (x::Int)) ->
        -- Note the (w::a), NOT (w::Int), because we have not yet
        -- refined 'a' to Int.  So we must know that the second component
        -- of the tuple is of type 'a' not Int.  See selectMatchVar
        -- (June 14: I'm not sure this comment is right; the sub-patterns
        --           will be wrapped in CoPats, no?)
    -- ^ Tuple sub-patterns
    --
    -- - 'ApiAnnotation.AnnKeywordId' :
    --            'ApiAnnotation.AnnOpen' @'('@ or @'(#'@,
    --            'ApiAnnotation.AnnClose' @')'@ or  @'#)'@

  | SumPat      (LPat p)           -- Sum sub-pattern
                ConTag             -- Alternative (one-based)
                Arity              -- Arity (INVARIANT: ≥ 2)
                (PostTc p [Type])  -- PlaceHolder before typechecker, filled in
                                   -- afterwards with the types of the
                                   -- alternative
    -- ^ Anonymous sum pattern
    --
    -- - 'ApiAnnotation.AnnKeywordId' :
    --            'ApiAnnotation.AnnOpen' @'(#'@,
    --            'ApiAnnotation.AnnClose' @'#)'@

    -- For details on above see note [Api annotations] in ApiAnnotation
  | PArrPat     [LPat p]                -- Syntactic parallel array
                (PostTc p Type)         -- The type of the elements
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
    --                                    'ApiAnnotation.AnnClose' @':]'@

    -- For details on above see note [Api annotations] in ApiAnnotation
        ------------ Constructor patterns ---------------
  | ConPatIn    (Located (IdP p))
                (HsConPatDetails p)
    -- ^ Constructor Pattern In

  | ConPatOut {
        pat_con     :: Located ConLike,
        pat_arg_tys :: [Type],          -- The universal arg types, 1-1 with the universal
                                        -- tyvars of the constructor/pattern synonym
                                        --   Use (conLikeResTy pat_con pat_arg_tys) to get
                                        --   the type of the pattern

        pat_tvs   :: [TyVar],           -- Existentially bound type variables
                                        -- in correctly-scoped order e.g. [k:*, x:k]
        pat_dicts :: [EvVar],           -- Ditto *coercion variables* and *dictionaries*
                                        -- One reason for putting coercion variable here, I think,
                                        --      is to ensure their kinds are zonked

        pat_binds :: TcEvBinds,         -- Bindings involving those dictionaries
        pat_args  :: HsConPatDetails p,
        pat_wrap  :: HsWrapper          -- Extra wrapper to pass to the matcher
                                        -- Only relevant for pattern-synonyms;
                                        --   ignored for data cons
    }
    -- ^ Constructor Pattern Out

        ------------ View patterns ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ViewPat       (LHsExpr p)
                  (LPat p)
                  (PostTc p Type)   -- The overall type of the pattern
                                    -- (= the argument type of the view function)
                                    -- for hsPatType.
    -- ^ View Pattern

        ------------ Pattern splices ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'$('@
  --        'ApiAnnotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SplicePat       (HsSplice p)    -- ^ Splice Pattern (Includes quasi-quotes)

        ------------ Literal and n+k patterns ---------------
  | LitPat          (HsLit p)           -- ^ Literal Pattern
                                        -- Used for *non-overloaded* literal patterns:
                                        -- Int#, Char#, Int, Char, String, etc.

  | NPat                -- Natural Pattern
                        -- Used for all overloaded literals,
                        -- including overloaded strings with -XOverloadedStrings
                    (Located (HsOverLit p))     -- ALWAYS positive
                    (Maybe (SyntaxExpr p)) -- Just (Name of 'negate') for
                                           -- negative patterns, Nothing
                                           -- otherwise
                    (SyntaxExpr p)       -- Equality checker, of type t->t->Bool
                    (PostTc p Type)      -- Overall type of pattern. Might be
                                         -- different than the literal's type
                                         -- if (==) or negate changes the type

  -- ^ Natural Pattern
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVal' @'+'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | NPlusKPat       (Located (IdP p))        -- n+k pattern
                    (Located (HsOverLit p))  -- It'll always be an HsIntegral
                    (HsOverLit p)       -- See Note [NPlusK patterns] in TcPat
                     -- NB: This could be (PostTc ...), but that induced a
                     -- a new hs-boot file. Not worth it.

                    (SyntaxExpr p)   -- (>=) function, of type t1->t2->Bool
                    (SyntaxExpr p)   -- Name of '-' (see RnEnv.lookupSyntaxName)
                    (PostTc p Type)  -- Type of overall pattern
  -- ^ n+k pattern

        ------------ Pattern type signatures ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SigPatIn        (LPat p)                  -- Pattern with a type signature
                    (LHsSigWcType p)          -- Signature can bind both
                                              -- kind and type vars
    -- ^ Pattern with a type signature

  | SigPatOut       (LPat p)
                    Type
    -- ^ Pattern with a type signature

        ------------ Pattern coercions (translation only) ---------------
  | CoPat       HsWrapper           -- Coercion Pattern
                                    -- If co :: t1 ~ t2, p :: t2,
                                    -- then (CoPat co p) :: t1
                (Pat p)             -- Why not LPat?  Ans: existing locn will do
                Type                -- Type of whole pattern, t1
        -- During desugaring a (CoPat co pat) turns into a cast with 'co' on
        -- the scrutinee, followed by a match on 'pat'
    -- ^ Coercion Pattern
deriving instance (DataId p) => Data (Pat p)

-- | Haskell Constructor Pattern Details
type HsConPatDetails p = HsConDetails (LPat p) (HsRecFields p (LPat p))

hsConPatArgs :: HsConPatDetails p -> [LPat p]
hsConPatArgs (PrefixCon ps)   = ps
hsConPatArgs (RecCon fs)      = map (hsRecFieldArg . unLoc) (rec_flds fs)
hsConPatArgs (InfixCon p1 p2) = [p1,p2]

-- | Haskell Record Fields
--
-- HsRecFields is used only for patterns and expressions (not data type
-- declarations)
data HsRecFields p arg         -- A bunch of record fields
                                --      { x = 3, y = True }
        -- Used for both expressions and patterns
  = HsRecFields { rec_flds   :: [LHsRecField p arg],
                  rec_dotdot :: Maybe Int }  -- Note [DotDot fields]
  deriving (Functor, Foldable, Traversable)
deriving instance (DataId p, Data arg) => Data (HsRecFields p arg)


-- Note [DotDot fields]
-- ~~~~~~~~~~~~~~~~~~~~
-- The rec_dotdot field means this:
--   Nothing => the normal case
--   Just n  => the group uses ".." notation,
--
-- In the latter case:
--
--   *before* renamer: rec_flds are exactly the n user-written fields
--
--   *after* renamer:  rec_flds includes *all* fields, with
--                     the first 'n' being the user-written ones
--                     and the remainder being 'filled in' implicitly

-- | Located Haskell Record Field
type LHsRecField' p arg = Located (HsRecField' p arg)

-- | Located Haskell Record Field
type LHsRecField  p arg = Located (HsRecField  p arg)

-- | Located Haskell Record Update Field
type LHsRecUpdField p   = Located (HsRecUpdField p)

-- | Haskell Record Field
type HsRecField    p arg = HsRecField' (FieldOcc p) arg

-- | Haskell Record Update Field
type HsRecUpdField p     = HsRecField' (AmbiguousFieldOcc p) (LHsExpr p)

-- | Haskell Record Field
--
-- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual',
--
-- For details on above see note [Api annotations] in ApiAnnotation
data HsRecField' id arg = HsRecField {
        hsRecFieldLbl :: Located id,
        hsRecFieldArg :: arg,           -- ^ Filled in by renamer when punning
        hsRecPun      :: Bool           -- ^ Note [Punning]
  } deriving (Data, Functor, Foldable, Traversable)


-- Note [Punning]
-- ~~~~~~~~~~~~~~
-- If you write T { x, y = v+1 }, the HsRecFields will be
--      HsRecField x x True ...
--      HsRecField y (v+1) False ...
-- That is, for "punned" field x is expanded (in the renamer)
-- to x=x; but with a punning flag so we can detect it later
-- (e.g. when pretty printing)
--
-- If the original field was qualified, we un-qualify it, thus
--    T { A.x } means T { A.x = x }


-- Note [HsRecField and HsRecUpdField]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- A HsRecField (used for record construction and pattern matching)
-- contains an unambiguous occurrence of a field (i.e. a FieldOcc).
-- We can't just store the Name, because thanks to
-- DuplicateRecordFields this may not correspond to the label the user
-- wrote.
--
-- A HsRecUpdField (used for record update) contains a potentially
-- ambiguous occurrence of a field (an AmbiguousFieldOcc).  The
-- renamer will fill in the selector function if it can, but if the
-- selector is ambiguous the renamer will defer to the typechecker.
-- After the typechecker, a unique selector will have been determined.
--
-- The renamer produces an Unambiguous result if it can, rather than
-- just doing the lookup in the typechecker, so that completely
-- unambiguous updates can be represented by 'DsMeta.repUpdFields'.
--
-- For example, suppose we have:
--
--     data S = MkS { x :: Int }
--     data T = MkT { x :: Int }
--
--     f z = (z { x = 3 }) :: S
--
-- The parsed HsRecUpdField corresponding to the record update will have:
--
--     hsRecFieldLbl = Unambiguous "x" PlaceHolder :: AmbiguousFieldOcc RdrName
--
-- After the renamer, this will become:
--
--     hsRecFieldLbl = Ambiguous   "x" PlaceHolder :: AmbiguousFieldOcc Name
--
-- (note that the Unambiguous constructor is not type-correct here).
-- The typechecker will determine the particular selector:
--
--     hsRecFieldLbl = Unambiguous "x" $sel:x:MkS  :: AmbiguousFieldOcc Id
--
-- See also Note [Disambiguating record fields] in TcExpr.

hsRecFields :: HsRecFields p arg -> [PostRn p (IdP p)]
hsRecFields rbinds = map (unLoc . hsRecFieldSel . unLoc) (rec_flds rbinds)

-- Probably won't typecheck at once, things have changed :/
hsRecFieldsArgs :: HsRecFields p arg -> [arg]
hsRecFieldsArgs rbinds = map (hsRecFieldArg . unLoc) (rec_flds rbinds)

hsRecFieldSel :: HsRecField pass arg -> Located (PostRn pass (IdP pass))
hsRecFieldSel = fmap selectorFieldOcc . hsRecFieldLbl

hsRecFieldId :: HsRecField GhcTc arg -> Located Id
hsRecFieldId = hsRecFieldSel

hsRecUpdFieldRdr :: HsRecUpdField p -> Located RdrName
hsRecUpdFieldRdr = fmap rdrNameAmbiguousFieldOcc . hsRecFieldLbl

hsRecUpdFieldId :: HsRecField' (AmbiguousFieldOcc GhcTc) arg -> Located Id
hsRecUpdFieldId = fmap selectorFieldOcc . hsRecUpdFieldOcc

hsRecUpdFieldOcc :: HsRecField' (AmbiguousFieldOcc GhcTc) arg -> LFieldOcc GhcTc
hsRecUpdFieldOcc = fmap unambiguousFieldOcc . hsRecFieldLbl


{-
************************************************************************
*                                                                      *
*              Printing patterns
*                                                                      *
************************************************************************
-}

instance (SourceTextX pass, OutputableBndrId pass)
       => Outputable (Pat pass) where
    ppr = pprPat

pprPatBndr :: OutputableBndr name => name -> SDoc
pprPatBndr var                  -- Print with type info if -dppr-debug is on
  = getPprStyle $ \ sty ->
    if debugStyle sty then
        parens (pprBndr LambdaBind var)         -- Could pass the site to pprPat
                                                -- but is it worth it?
    else
        pprPrefixOcc var

pprParendLPat :: (SourceTextX pass, OutputableBndrId pass) => LPat pass -> SDoc
pprParendLPat (L _ p) = pprParendPat p

pprParendPat :: (SourceTextX pass, OutputableBndrId pass) => Pat pass -> SDoc
pprParendPat p = sdocWithDynFlags $ \ dflags ->
                 if need_parens dflags p
                 then parens (pprPat p)
                 else  pprPat p
  where
    need_parens dflags p
      | CoPat {} <- p = gopt Opt_PrintTypecheckerElaboration dflags
      | otherwise     = hsPatNeedsParens p
      -- For a CoPat we need parens if we are going to show it, which
      -- we do if -fprint-typechecker-elaboration is on (c.f. pprHsWrapper)
      -- But otherwise the CoPat is discarded, so it
      -- is the pattern inside that matters.  Sigh.

pprPat :: (SourceTextX pass, OutputableBndrId pass) => Pat pass -> SDoc
pprPat (VarPat (L _ var))     = pprPatBndr var
pprPat (WildPat _)            = char '_'
pprPat (LazyPat pat)          = char '~' <> pprParendLPat pat
pprPat (BangPat pat)          = char '!' <> pprParendLPat pat
pprPat (AsPat name pat)       = hcat [pprPrefixOcc (unLoc name), char '@', pprParendLPat pat]
pprPat (ViewPat expr pat _)   = hcat [pprLExpr expr, text " -> ", ppr pat]
pprPat (ParPat pat)           = parens (ppr pat)
pprPat (LitPat s)             = ppr s
pprPat (NPat l Nothing  _ _)  = ppr l
pprPat (NPat l (Just _) _ _)  = char '-' <> ppr l
pprPat (NPlusKPat n k _ _ _ _)= hcat [ppr n, char '+', ppr k]
pprPat (SplicePat splice)     = pprSplice splice
pprPat (CoPat co pat _)       = pprHsWrapper co (\parens -> if parens
                                                            then pprParendPat pat
                                                            else pprPat pat)
pprPat (SigPatIn pat ty)      = ppr pat <+> dcolon <+> ppr ty
pprPat (SigPatOut pat ty)     = ppr pat <+> dcolon <+> ppr ty
pprPat (ListPat pats _ _)     = brackets (interpp'SP pats)
pprPat (PArrPat pats _)       = paBrackets (interpp'SP pats)
pprPat (TuplePat pats bx _)   = tupleParens (boxityTupleSort bx) (pprWithCommas ppr pats)
pprPat (SumPat pat alt arity _) = sumParens (pprAlternative ppr pat alt arity)
pprPat (ConPatIn con details) = pprUserCon (unLoc con) details
pprPat (ConPatOut { pat_con = con, pat_tvs = tvs, pat_dicts = dicts,
                    pat_binds = binds, pat_args = details })
  = sdocWithDynFlags $ \dflags ->
       -- Tiresome; in TcBinds.tcRhs we print out a
       -- typechecked Pat in an error message,
       -- and we want to make sure it prints nicely
    if gopt Opt_PrintTypecheckerElaboration dflags then
        ppr con
          <> braces (sep [ hsep (map pprPatBndr (tvs ++ dicts))
                         , ppr binds])
          <+> pprConArgs details
    else pprUserCon (unLoc con) details


pprUserCon :: (SourceTextX p, OutputableBndr con, OutputableBndrId p)
           => con -> HsConPatDetails p -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details          = pprPrefixOcc c <+> pprConArgs details

pprConArgs :: (SourceTextX p, OutputableBndrId p) => HsConPatDetails p -> SDoc
pprConArgs (PrefixCon pats) = sep (map pprParendLPat pats)
pprConArgs (InfixCon p1 p2) = sep [pprParendLPat p1, pprParendLPat p2]
pprConArgs (RecCon rpats)   = ppr rpats

instance (Outputable arg)
      => Outputable (HsRecFields p arg) where
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Nothing })
        = braces (fsep (punctuate comma (map ppr flds)))
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Just n })
        = braces (fsep (punctuate comma (map ppr (take n flds) ++ [dotdot])))
        where
          dotdot = text ".." <+> whenPprDebug (ppr (drop n flds))

instance (Outputable p, Outputable arg)
      => Outputable (HsRecField' p arg) where
  ppr (HsRecField { hsRecFieldLbl = f, hsRecFieldArg = arg,
                    hsRecPun = pun })
    = ppr f <+> (ppUnless pun $ equals <+> ppr arg)


{-
************************************************************************
*                                                                      *
*              Building patterns
*                                                                      *
************************************************************************
-}

mkPrefixConPat :: DataCon -> [OutPat p] -> [Type] -> OutPat p
-- Make a vanilla Prefix constructor pattern
mkPrefixConPat dc pats tys
  = noLoc $ ConPatOut { pat_con = noLoc (RealDataCon dc), pat_tvs = [], pat_dicts = [],
                        pat_binds = emptyTcEvBinds, pat_args = PrefixCon pats,
                        pat_arg_tys = tys, pat_wrap = idHsWrapper }

mkNilPat :: Type -> OutPat p
mkNilPat ty = mkPrefixConPat nilDataCon [] [ty]

mkCharLitPat :: (SourceTextX p) => SourceText -> Char -> OutPat p
mkCharLitPat src c = mkPrefixConPat charDataCon
                          [noLoc $ LitPat (HsCharPrim (setSourceText src) c)] []

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

isBangedLPat :: LPat p -> Bool
isBangedLPat (L _ (ParPat p))   = isBangedLPat p
isBangedLPat (L _ (BangPat {})) = True
isBangedLPat _                  = False

looksLazyPatBind :: HsBind p -> Bool
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

looksLazyLPat :: LPat p -> Bool
looksLazyLPat (L _ (ParPat p))             = looksLazyLPat p
looksLazyLPat (L _ (AsPat _ p))            = looksLazyLPat p
looksLazyLPat (L _ (BangPat {}))           = False
looksLazyLPat (L _ (VarPat {}))            = False
looksLazyLPat (L _ (WildPat {}))           = False
looksLazyLPat _                            = True

isIrrefutableHsPat :: (SourceTextX p, OutputableBndrId p) => LPat p -> Bool
-- (isIrrefutableHsPat p) is true if matching against p cannot fail,
-- in the sense of falling through to the next pattern.
--      (NB: this is not quite the same as the (silly) defn
--      in 3.17.2 of the Haskell 98 report.)
--
-- WARNING: isIrrefutableHsPat returns False if it's in doubt.
-- Specifically on a ConPatIn, which is what it sees for a
-- (LPat Name) in the renamer, it doesn't know the size of the
-- constructor family, so it returns False.  Result: only
-- tuple patterns are considered irrefuable at the renamer stage.
--
-- But if it returns True, the pattern is definitely irrefutable
isIrrefutableHsPat pat
  = go pat
  where
    go (L _ pat) = go1 pat

    go1 (WildPat {})        = True
    go1 (VarPat {})         = True
    go1 (LazyPat {})        = True
    go1 (BangPat pat)       = go pat
    go1 (CoPat _ pat _)     = go1 pat
    go1 (ParPat pat)        = go pat
    go1 (AsPat _ pat)       = go pat
    go1 (ViewPat _ pat _)   = go pat
    go1 (SigPatIn pat _)    = go pat
    go1 (SigPatOut pat _)   = go pat
    go1 (TuplePat pats _ _) = all go pats
    go1 (SumPat _ _ _ _)    = False
                    -- See Note [Unboxed sum patterns aren't irrefutable]
    go1 (ListPat {})        = False
    go1 (PArrPat {})        = False     -- ?

    go1 (ConPatIn {})       = False     -- Conservative
    go1 (ConPatOut{ pat_con = L _ (RealDataCon con), pat_args = details })
        =  isJust (tyConSingleDataCon_maybe (dataConTyCon con))
           -- NB: tyConSingleDataCon_maybe, *not* isProductTyCon, because
           -- the latter is false of existentials. See Trac #4439
        && all go (hsConPatArgs details)
    go1 (ConPatOut{ pat_con = L _ (PatSynCon _pat) })
        = False -- Conservative

    go1 (LitPat {})         = False
    go1 (NPat {})           = False
    go1 (NPlusKPat {})      = False

    -- We conservatively assume that no TH splices are irrefutable
    -- since we cannot know until the splice is evaluated.
    go1 (SplicePat {})      = False

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

-- | Returns 'True' if a pattern must be parenthesized in order to parse
-- (e.g., the @(x :: Int)@ in @f (x :: Int) = x@).
hsPatNeedsParens :: Pat a -> Bool
hsPatNeedsParens (NPlusKPat {})      = True
hsPatNeedsParens (SplicePat {})      = False
hsPatNeedsParens (ConPatIn _ ds)     = conPatNeedsParens ds
hsPatNeedsParens p@(ConPatOut {})    = conPatNeedsParens (pat_args p)
hsPatNeedsParens (SigPatIn {})       = True
hsPatNeedsParens (SigPatOut {})      = True
hsPatNeedsParens (ViewPat {})        = True
hsPatNeedsParens (CoPat _ p _)       = hsPatNeedsParens p
hsPatNeedsParens (WildPat {})        = False
hsPatNeedsParens (VarPat {})         = False
hsPatNeedsParens (LazyPat {})        = False
hsPatNeedsParens (BangPat {})        = False
hsPatNeedsParens (ParPat {})         = False
hsPatNeedsParens (AsPat {})          = False
hsPatNeedsParens (TuplePat {})       = False
hsPatNeedsParens (SumPat {})         = False
hsPatNeedsParens (ListPat {})        = False
hsPatNeedsParens (PArrPat {})        = False
hsPatNeedsParens (LitPat {})         = False
hsPatNeedsParens (NPat {})           = False

-- | Returns 'True' if a constructor pattern must be parenthesized in order
-- to parse.
conPatNeedsParens :: HsConDetails a b -> Bool
conPatNeedsParens (PrefixCon {}) = False
conPatNeedsParens (InfixCon {})  = True
conPatNeedsParens (RecCon {})    = False

-- | Returns 'True' for compound patterns that need parentheses when used in
-- an argument position.
--
-- Note that this is different from 'hsPatNeedsParens', which only says if
-- a pattern needs to be parenthesized to parse in /any/ position, whereas
-- 'isCompountPat' says if a pattern needs to be parenthesized in an /argument/
-- position. In other words, @'hsPatNeedsParens' x@ implies
-- @'isCompoundPat' x@, but not necessarily the other way around.
isCompoundPat :: Pat a -> Bool
isCompoundPat (NPlusKPat {})       = True
isCompoundPat (SplicePat {})       = False
isCompoundPat (ConPatIn _ ds)      = isCompoundConPat ds
isCompoundPat p@(ConPatOut {})     = isCompoundConPat (pat_args p)
isCompoundPat (SigPatIn {})        = True
isCompoundPat (SigPatOut {})       = True
isCompoundPat (ViewPat {})         = True
isCompoundPat (CoPat _ p _)        = isCompoundPat p
isCompoundPat (WildPat {})         = False
isCompoundPat (VarPat {})          = False
isCompoundPat (LazyPat {})         = False
isCompoundPat (BangPat {})         = False
isCompoundPat (ParPat {})          = False
isCompoundPat (AsPat {})           = False
isCompoundPat (TuplePat {})        = False
isCompoundPat (SumPat {})          = False
isCompoundPat (ListPat {})         = False
isCompoundPat (PArrPat {})         = False
isCompoundPat (LitPat p)           = isCompoundHsLit p
isCompoundPat (NPat (L _ p) _ _ _) = isCompoundHsOverLit p

-- | Returns 'True' for compound constructor patterns that need parentheses
-- when used in an argument position.
--
-- Note that this is different from 'conPatNeedsParens', which only says if
-- a constructor pattern needs to be parenthesized to parse in /any/ position,
-- whereas 'isCompountConPat' says if a pattern needs to be parenthesized in an
-- /argument/ position. In other words, @'conPatNeedsParens' x@ implies
-- @'isCompoundConPat' x@, but not necessarily the other way around.
isCompoundConPat :: HsConDetails a b -> Bool
isCompoundConPat (PrefixCon args) = not (null args)
isCompoundConPat (InfixCon {})    = True
isCompoundConPat (RecCon {})      = False

-- | @'parenthesizeCompoundPat' p@ checks if @'isCompoundPat' p@ is true, and
-- if so, surrounds @p@ with a 'ParPat'. Otherwise, it simply returns @p@.
parenthesizeCompoundPat :: LPat p -> LPat p
parenthesizeCompoundPat lp@(L loc p)
  | isCompoundPat p = L loc (ParPat lp)
  | otherwise       = lp

{-
% Collect all EvVars from all constructor patterns
-}

-- May need to add more cases
collectEvVarsPats :: [Pat p] -> Bag EvVar
collectEvVarsPats = unionManyBags . map collectEvVarsPat

collectEvVarsLPat :: LPat p -> Bag EvVar
collectEvVarsLPat (L _ pat) = collectEvVarsPat pat

collectEvVarsPat :: Pat p -> Bag EvVar
collectEvVarsPat pat =
  case pat of
    LazyPat  p        -> collectEvVarsLPat p
    AsPat _  p        -> collectEvVarsLPat p
    ParPat   p        -> collectEvVarsLPat p
    BangPat  p        -> collectEvVarsLPat p
    ListPat  ps _ _   -> unionManyBags $ map collectEvVarsLPat ps
    TuplePat ps _ _   -> unionManyBags $ map collectEvVarsLPat ps
    SumPat p _ _ _    -> collectEvVarsLPat p
    PArrPat  ps _     -> unionManyBags $ map collectEvVarsLPat ps
    ConPatOut {pat_dicts = dicts, pat_args  = args}
                      -> unionBags (listToBag dicts)
                                   $ unionManyBags
                                   $ map collectEvVarsLPat
                                   $ hsConPatArgs args
    SigPatOut p _     -> collectEvVarsLPat p
    CoPat _ p _       -> collectEvVarsPat  p
    ConPatIn _  _     -> panic "foldMapPatBag: ConPatIn"
    SigPatIn _ _      -> panic "foldMapPatBag: SigPatIn"
    _other_pat        -> emptyBag
