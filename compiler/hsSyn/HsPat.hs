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
        isBangedLPat, isBangedPatBind,
        hsPatNeedsParens,
        isIrrefutableHsPat,

        collectEvVarsPats,

        pprParendLPat, pprConArgs
    ) where

import {-# SOURCE #-} HsExpr            (SyntaxExpr, LHsExpr, HsSplice, pprLExpr, pprSplice)

-- friends:
import HsBinds
import HsLit
import PlaceHolder
import HsTypes
import HsEmbellished
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

type InPat id  = LPat id        -- No 'Out' constructors
type OutPat id = LPat id        -- No 'In' constructors

type LPat id = Located (Pat id)

-- | Pattern
--
-- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

-- For details on above see note [Api annotations] in ApiAnnotation
data Pat id
  =     ------------ Simple patterns ---------------
    WildPat     (PostTc id Type)        -- ^ Wildcard Pattern
        -- The sole reason for a type on a WildPat is to
        -- support hsPatType :: Pat Id -> Type

  | VarPat      (Located id) -- ^ Variable Pattern

                             -- See Note [Located RdrNames] in HsExpr
  | LazyPat     (LPat id)               -- ^ Lazy Pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | AsPat       (LEmbellished id) (LPat id)  -- ^ As pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | ParPat      (LPat id)               -- ^ Parenthesised pattern
                                        -- See Note [Parens in HsSyn] in HsExpr
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
    --                                    'ApiAnnotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in ApiAnnotation
  | BangPat     (LPat id)               -- ^ Bang pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

    -- For details on above see note [Api annotations] in ApiAnnotation

        ------------ Lists, tuples, arrays ---------------
  | ListPat     [LPat id]
                (PostTc id Type)                        -- The type of the elements
                (Maybe (PostTc id Type, SyntaxExpr id)) -- For rebindable syntax
                   -- For OverloadedLists a Just (ty,fn) gives
                   -- overall type of the pattern, and the toList
                   -- function to convert the scrutinee to a list value
    -- ^ Syntactic List
    --
    -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
    --                                    'ApiAnnotation.AnnClose' @']'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | TuplePat    [LPat id]        -- Tuple sub-patterns
                Boxity           -- UnitPat is TuplePat []
                [PostTc id Type] -- [] before typechecker, filled in afterwards
                                 -- with the types of the tuple components
        -- You might think that the PostTc id Type was redundant, because we can
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

  | SumPat      (LPat id)          -- Sum sub-pattern
                ConTag             -- Alternative (one-based)
                Arity              -- Arity
                (PostTc id [Type]) -- PlaceHolder before typechecker, filled in
                                   -- afterwards with the types of the
                                   -- alternative
    -- ^ Anonymous sum pattern
    --
    -- - 'ApiAnnotation.AnnKeywordId' :
    --            'ApiAnnotation.AnnOpen' @'(#'@,
    --            'ApiAnnotation.AnnClose' @'#)'@

    -- For details on above see note [Api annotations] in ApiAnnotation
  | PArrPat     [LPat id]               -- Syntactic parallel array
                (PostTc id Type)        -- The type of the elements
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
    --                                    'ApiAnnotation.AnnClose' @':]'@

    -- For details on above see note [Api annotations] in ApiAnnotation
        ------------ Constructor patterns ---------------
  | ConPatIn    (Located id)
                (HsConPatDetails id)
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
        pat_args  :: HsConPatDetails id,
        pat_wrap  :: HsWrapper          -- Extra wrapper to pass to the matcher
                                        -- Only relevant for pattern-synonyms;
                                        --   ignored for data cons
    }
    -- ^ Constructor Pattern Out

        ------------ View patterns ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | ViewPat       (LHsExpr id)
                  (LPat id)
                  (PostTc id Type)  -- The overall type of the pattern
                                    -- (= the argument type of the view function)
                                    -- for hsPatType.
    -- ^ View Pattern

        ------------ Pattern splices ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'$('@
  --        'ApiAnnotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SplicePat       (HsSplice id)   -- ^ Splice Pattern (Includes quasi-quotes)

        ------------ Literal and n+k patterns ---------------
  | LitPat          HsLit               -- ^ Literal Pattern
                                        -- Used for *non-overloaded* literal patterns:
                                        -- Int#, Char#, Int, Char, String, etc.

  | NPat                -- Natural Pattern
                        -- Used for all overloaded literals,
                        -- including overloaded strings with -XOverloadedStrings
                    (Located (HsOverLit id))    -- ALWAYS positive
                    (Maybe (SyntaxExpr id))     -- Just (Name of 'negate') for negative
                                                -- patterns, Nothing otherwise
                    (SyntaxExpr id)             -- Equality checker, of type t->t->Bool
                    (PostTc id Type)            -- Overall type of pattern. Might be
                                                -- different than the literal's type
                                                -- if (==) or negate changes the type

  -- ^ Natural Pattern
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVal' @'+'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | NPlusKPat       (Located id)        -- n+k pattern
                    (Located (HsOverLit id)) -- It'll always be an HsIntegral
                    (HsOverLit id)      -- See Note [NPlusK patterns] in TcPat
                     -- NB: This could be (PostTc ...), but that induced a
                     -- a new hs-boot file. Not worth it.

                    (SyntaxExpr id)     -- (>=) function, of type t1->t2->Bool
                    (SyntaxExpr id)     -- Name of '-' (see RnEnv.lookupSyntaxName)
                    (PostTc id Type)    -- Type of overall pattern
  -- ^ n+k pattern

        ------------ Pattern type signatures ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SigPatIn        (LPat id)                 -- Pattern with a type signature
                    (LHsSigWcType id)         -- Signature can bind both
                                              -- kind and type vars
    -- ^ Pattern with a type signature

  | SigPatOut       (LPat id)
                    Type
    -- ^ Pattern with a type signature

        ------------ Pattern coercions (translation only) ---------------
  | CoPat       HsWrapper               -- Coercion Pattern
                                        -- If co :: t1 ~ t2, p :: t2,
                                        -- then (CoPat co p) :: t1
                (Pat id)                -- Why not LPat?  Ans: existing locn will do
                Type                    -- Type of whole pattern, t1
        -- During desugaring a (CoPat co pat) turns into a cast with 'co' on
        -- the scrutinee, followed by a match on 'pat'
    -- ^ Coercion Pattern
deriving instance (DataId id) => Data (Pat id)

-- | Haskell Constructor Pattern Details
type HsConPatDetails id = HsConDetails (LPat id) (HsRecFields id (LPat id))

hsConPatArgs :: HsConPatDetails id -> [LPat id]
hsConPatArgs (PrefixCon ps)   = ps
hsConPatArgs (RecCon fs)      = map (hsRecFieldArg . unLoc) (rec_flds fs)
hsConPatArgs (InfixCon p1 p2) = [p1,p2]

-- | Haskell Record Fields
--
-- HsRecFields is used only for patterns and expressions (not data type
-- declarations)
data HsRecFields id arg         -- A bunch of record fields
                                --      { x = 3, y = True }
        -- Used for both expressions and patterns
  = HsRecFields { rec_flds   :: [LHsRecField id arg],
                  rec_dotdot :: Maybe Int }  -- Note [DotDot fields]
  deriving (Functor, Foldable, Traversable)
deriving instance (DataId id, Data arg) => Data (HsRecFields id arg)


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
type LHsRecField' id arg = Located (HsRecField' id arg)

-- | Located Haskell Record Field
type LHsRecField  id arg = Located (HsRecField  id arg)

-- | Located Haskell Record Update Field
type LHsRecUpdField id   = Located (HsRecUpdField id)

-- | Haskell Record Field
type HsRecField    id arg = HsRecField' (FieldOcc id) arg

-- | Haskell Record Update Field
type HsRecUpdField id     = HsRecField' (AmbiguousFieldOcc id) (LHsExpr id)

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

hsRecFields :: HsRecFields id arg -> [PostRn id id]
hsRecFields rbinds = map (unLoc . hsRecFieldSel . unLoc) (rec_flds rbinds)

-- Probably won't typecheck at once, things have changed :/
hsRecFieldsArgs :: HsRecFields id arg -> [arg]
hsRecFieldsArgs rbinds = map (hsRecFieldArg . unLoc) (rec_flds rbinds)

hsRecFieldSel :: HsRecField name arg -> Located (PostRn name name)
hsRecFieldSel = fmap selectorFieldOcc . hsRecFieldLbl

hsRecFieldId :: HsRecField Id arg -> Located Id
hsRecFieldId = hsRecFieldSel

hsRecUpdFieldRdr :: HsRecUpdField id -> LEmbellished RdrName
hsRecUpdFieldRdr = fmap rdrNameAmbiguousFieldOcc . hsRecFieldLbl

hsRecUpdFieldId :: HsRecField' (AmbiguousFieldOcc Id) arg -> Located Id
hsRecUpdFieldId = fmap selectorFieldOcc . hsRecUpdFieldOcc

hsRecUpdFieldOcc :: HsRecField' (AmbiguousFieldOcc Id) arg -> LFieldOcc Id
hsRecUpdFieldOcc = fmap unambiguousFieldOcc . hsRecFieldLbl


{-
************************************************************************
*                                                                      *
*              Printing patterns
*                                                                      *
************************************************************************
-}

instance (OutputableBndrId name) => Outputable (Pat name) where
    ppr = pprPat

pprPatBndr :: OutputableBndr name => name -> SDoc
pprPatBndr var                  -- Print with type info if -dppr-debug is on
  = getPprStyle $ \ sty ->
    if debugStyle sty then
        parens (pprBndr LambdaBind var)         -- Could pass the site to pprPat
                                                -- but is it worth it?
    else
        pprPrefixOcc var

pprParendLPat :: (OutputableBndrId name) => LPat name -> SDoc
pprParendLPat (L _ p) = pprParendPat p

pprParendPat :: (OutputableBndrId name) => Pat name -> SDoc
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

pprPat :: (OutputableBndrId name) => Pat name -> SDoc
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


pprUserCon :: (OutputableBndr con, OutputableBndrId id)
           => con -> HsConPatDetails id -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details          = pprPrefixOcc c <+> pprConArgs details

pprConArgs :: (OutputableBndrId id) => HsConPatDetails id -> SDoc
pprConArgs (PrefixCon pats) = sep (map pprParendLPat pats)
pprConArgs (InfixCon p1 p2) = sep [pprParendLPat p1, pprParendLPat p2]
pprConArgs (RecCon rpats)   = ppr rpats

instance (Outputable arg)
      => Outputable (HsRecFields id arg) where
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Nothing })
        = braces (fsep (punctuate comma (map ppr flds)))
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Just n })
        = braces (fsep (punctuate comma (map ppr (take n flds) ++ [dotdot])))
        where
          dotdot = text ".." <+> ifPprDebug (ppr (drop n flds))

instance (Outputable id, Outputable arg)
      => Outputable (HsRecField' id arg) where
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

mkPrefixConPat :: DataCon -> [OutPat id] -> [Type] -> OutPat id
-- Make a vanilla Prefix constructor pattern
mkPrefixConPat dc pats tys
  = noLoc $ ConPatOut { pat_con = noLoc (RealDataCon dc), pat_tvs = [], pat_dicts = [],
                        pat_binds = emptyTcEvBinds, pat_args = PrefixCon pats,
                        pat_arg_tys = tys, pat_wrap = idHsWrapper }

mkNilPat :: Type -> OutPat id
mkNilPat ty = mkPrefixConPat nilDataCon [] [ty]

mkCharLitPat :: SourceText -> Char -> OutPat id
mkCharLitPat src c = mkPrefixConPat charDataCon
                                    [noLoc $ LitPat (HsCharPrim src c)] []

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

isBangedPatBind :: HsBind id -> Bool
isBangedPatBind (PatBind {pat_lhs = pat}) = isBangedLPat pat
isBangedPatBind _ = False

isBangedLPat :: LPat id -> Bool
isBangedLPat (L _ (ParPat p))   = isBangedLPat p
isBangedLPat (L _ (BangPat {})) = True
isBangedLPat _                  = False

looksLazyPatBind :: HsBind id -> Bool
-- Returns True of anything *except*
--     a StrictHsBind (as above) or
--     a VarPat
-- In particular, returns True of a pattern binding with a compound pattern, like (I# x)
-- Looks through AbsBinds
looksLazyPatBind (PatBind { pat_lhs = p })
  = looksLazyLPat p
looksLazyPatBind (AbsBinds { abs_binds = binds })
  = anyBag (looksLazyPatBind . unLoc) binds
looksLazyPatBind (AbsBindsSig { abs_sig_bind = L _ bind })
  = looksLazyPatBind bind
looksLazyPatBind _
  = False

looksLazyLPat :: LPat id -> Bool
looksLazyLPat (L _ (ParPat p))             = looksLazyLPat p
looksLazyLPat (L _ (AsPat _ p))            = looksLazyLPat p
looksLazyLPat (L _ (BangPat {}))           = False
looksLazyLPat (L _ (VarPat {}))            = False
looksLazyLPat (L _ (WildPat {}))           = False
looksLazyLPat _                            = True

isIrrefutableHsPat :: (OutputableBndrId id) => LPat id -> Bool
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
    go1 (SumPat pat _ _  _) = go pat
    go1 (ListPat {}) = False
    go1 (PArrPat {})        = False     -- ?

    go1 (ConPatIn {})       = False     -- Conservative
    go1 (ConPatOut{ pat_con = L _ (RealDataCon con), pat_args = details })
        =  isJust (tyConSingleDataCon_maybe (dataConTyCon con))
           -- NB: tyConSingleDataCon_maybe, *not* isProductTyCon, because
           -- the latter is false of existentials. See Trac #4439
        && all go (hsConPatArgs details)
    go1 (ConPatOut{ pat_con = L _ (PatSynCon _pat) })
        = False -- Conservative

    go1 (LitPat {})    = False
    go1 (NPat {})      = False
    go1 (NPlusKPat {}) = False

    -- Both should be gotten rid of by renamer before
    -- isIrrefutablePat is called
    go1 (SplicePat {})     = urk pat

    urk pat = pprPanic "isIrrefutableHsPat:" (ppr pat)

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

conPatNeedsParens :: HsConDetails a b -> Bool
conPatNeedsParens (PrefixCon {}) = False
conPatNeedsParens (InfixCon {})  = True
conPatNeedsParens (RecCon {})    = False

{-
% Collect all EvVars from all constructor patterns
-}

-- May need to add more cases
collectEvVarsPats :: [Pat id] -> Bag EvVar
collectEvVarsPats = unionManyBags . map collectEvVarsPat

collectEvVarsLPat :: LPat id -> Bag EvVar
collectEvVarsLPat (L _ pat) = collectEvVarsPat pat

collectEvVarsPat :: Pat id -> Bag EvVar
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
