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
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}

module HsPat (
        Pat(..), InPat, OutPat, LPat,
        ListPatTc(..),

        HsConPatDetails, hsConPatArgs,
        HsRecFields(..), HsRecField'(..), LHsRecField',
        HsRecField, LHsRecField,
        HsRecUpdField, LHsRecUpdField,
        hsRecFields, hsRecFieldSel, hsRecFieldId, hsRecFieldsArgs,
        hsRecUpdFieldId, hsRecUpdFieldOcc, hsRecUpdFieldRdr,

        mkPrefixConPat, mkCharLitPat, mkNilPat,

        looksLazyPatBind,
        isBangedLPat,
        patNeedsParens, parenthesizePat,
        isIrrefutableHsPat,

        collectEvVarsPat, collectEvVarsPats,

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

type LPat p = Pat p

-- | Pattern
--
-- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

-- For details on above see note [Api annotations] in ApiAnnotation
data Pat p
  =     ------------ Simple patterns ---------------
    WildPat     (XWildPat p)        -- ^ Wildcard Pattern
        -- The sole reason for a type on a WildPat is to
        -- support hsPatType :: Pat Id -> Type

       -- AZ:TODO above comment needs to be updated
  | VarPat      (XVarPat p)
                (Located (IdP p))  -- ^ Variable Pattern

                             -- See Note [Located RdrNames] in HsExpr
  | LazyPat     (XLazyPat p)
                (LPat p)                -- ^ Lazy Pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | AsPat       (XAsPat p)
                (Located (IdP p)) (LPat p)    -- ^ As pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnAt'

    -- For details on above see note [Api annotations] in ApiAnnotation

  | ParPat      (XParPat p)
                (LPat p)                -- ^ Parenthesised pattern
                                        -- See Note [Parens in HsSyn] in HsExpr
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
    --                                    'ApiAnnotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in ApiAnnotation
  | BangPat     (XBangPat p)
                (LPat p)                -- ^ Bang pattern
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnBang'

    -- For details on above see note [Api annotations] in ApiAnnotation

        ------------ Lists, tuples, arrays ---------------
  | ListPat     (XListPat p)
                [LPat p]
                   -- For OverloadedLists a Just (ty,fn) gives
                   -- overall type of the pattern, and the toList
-- function to convert the scrutinee to a list value

    -- ^ Syntactic List
    --
    -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
    --                                    'ApiAnnotation.AnnClose' @']'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | TuplePat    (XTuplePat p)
                  -- after typechecking, holds the types of the tuple components
                [LPat p]         -- Tuple sub-patterns
                Boxity           -- UnitPat is TuplePat []
        -- You might think that the post typechecking Type was redundant,
        -- because we can get the pattern type by getting the types of the
        -- sub-patterns.
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

  | SumPat      (XSumPat p)        -- PlaceHolder before typechecker, filled in
                                   -- afterwards with the types of the
                                   -- alternative
                (LPat p)           -- Sum sub-pattern
                ConTag             -- Alternative (one-based)
                Arity              -- Arity (INVARIANT: ≥ 2)
    -- ^ Anonymous sum pattern
    --
    -- - 'ApiAnnotation.AnnKeywordId' :
    --            'ApiAnnotation.AnnOpen' @'(#'@,
    --            'ApiAnnotation.AnnClose' @'#)'@

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
  | ViewPat       (XViewPat p)     -- The overall type of the pattern
                                   -- (= the argument type of the view function)
                                   -- for hsPatType.
                  (LHsExpr p)
                  (LPat p)
    -- ^ View Pattern

        ------------ Pattern splices ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'$('@
  --        'ApiAnnotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SplicePat       (XSplicePat p)
                    (HsSplice p)    -- ^ Splice Pattern (Includes quasi-quotes)

        ------------ Literal and n+k patterns ---------------
  | LitPat          (XLitPat p)
                    (HsLit p)           -- ^ Literal Pattern
                                        -- Used for *non-overloaded* literal patterns:
                                        -- Int#, Char#, Int, Char, String, etc.

  | NPat                -- Natural Pattern
                        -- Used for all overloaded literals,
                        -- including overloaded strings with -XOverloadedStrings
                    (XNPat p)            -- Overall type of pattern. Might be
                                         -- different than the literal's type
                                         -- if (==) or negate changes the type
                    (Located (HsOverLit p))     -- ALWAYS positive
                    (Maybe (SyntaxExpr p)) -- Just (Name of 'negate') for
                                           -- negative patterns, Nothing
                                           -- otherwise
                    (SyntaxExpr p)       -- Equality checker, of type t->t->Bool

  -- ^ Natural Pattern
  --
  -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnVal' @'+'@

  -- For details on above see note [Api annotations] in ApiAnnotation
  | NPlusKPat       (XNPlusKPat p)           -- Type of overall pattern
                    (Located (IdP p))        -- n+k pattern
                    (Located (HsOverLit p))  -- It'll always be an HsIntegral
                    (HsOverLit p)       -- See Note [NPlusK patterns] in TcPat
                     -- NB: This could be (PostTc ...), but that induced a
                     -- a new hs-boot file. Not worth it.

                    (SyntaxExpr p)   -- (>=) function, of type t1->t2->Bool
                    (SyntaxExpr p)   -- Name of '-' (see RnEnv.lookupSyntaxName)
  -- ^ n+k pattern

        ------------ Pattern type signatures ---------------
  -- | - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

  -- For details on above see note [Api annotations] in ApiAnnotation
  | SigPat          (XSigPat p)             -- After typechecker: Type
                    (LPat p)                -- Pattern with a type signature
                    (LHsSigWcType (NoGhcTc p)) --  Signature can bind both
                                               --  kind and type vars

    -- ^ Pattern with a type signature

        ------------ Pattern coercions (translation only) ---------------
  | CoPat       (XCoPat p)
                HsWrapper           -- Coercion Pattern
                                    -- If co :: t1 ~ t2, p :: t2,
                                    -- then (CoPat co p) :: t1
                (Pat p)             -- Why not LPat?  Ans: existing locn will do
                Type                -- Type of whole pattern, t1
        -- During desugaring a (CoPat co pat) turns into a cast with 'co' on
        -- the scrutinee, followed by a match on 'pat'
    -- ^ Coercion Pattern

  -- | Trees that Grow extension point for new constructors
  | XPat
      (XXPat p)

-- ---------------------------------------------------------------------

data ListPatTc
  = ListPatTc
      Type                             -- The type of the elements
      (Maybe (Type, SyntaxExpr GhcTc)) -- For rebindable syntax

type instance XWildPat GhcPs = NoExt
type instance XWildPat GhcRn = NoExt
type instance XWildPat GhcTc = Type

type instance XVarPat  (GhcPass _) = NoExt
type instance XLazyPat (GhcPass _) = NoExt
type instance XAsPat   (GhcPass _) = NoExt
type instance XParPat  (GhcPass _) = NoExt
type instance XBangPat (GhcPass _) = NoExt

-- Note: XListPat cannot be extended when using GHC 8.0.2 as the bootstrap
-- compiler, as it triggers https://ghc.haskell.org/trac/ghc/ticket/14396 for
-- `SyntaxExpr`
type instance XListPat GhcPs = NoExt
type instance XListPat GhcRn = Maybe (SyntaxExpr GhcRn)
type instance XListPat GhcTc = ListPatTc

type instance XTuplePat GhcPs = NoExt
type instance XTuplePat GhcRn = NoExt
type instance XTuplePat GhcTc = [Type]

type instance XSumPat GhcPs = NoExt
type instance XSumPat GhcRn = NoExt
type instance XSumPat GhcTc = [Type]

type instance XViewPat GhcPs = NoExt
type instance XViewPat GhcRn = NoExt
type instance XViewPat GhcTc = Type

type instance XSplicePat (GhcPass _) = NoExt
type instance XLitPat    (GhcPass _) = NoExt

type instance XNPat GhcPs = NoExt
type instance XNPat GhcRn = NoExt
type instance XNPat GhcTc = Type

type instance XNPlusKPat GhcPs = NoExt
type instance XNPlusKPat GhcRn = NoExt
type instance XNPlusKPat GhcTc = Type

type instance XSigPat GhcPs = NoExt
type instance XSigPat GhcRn = NoExt
type instance XSigPat GhcTc = Type

type instance XCoPat  (GhcPass _) = NoExt
type instance XXPat   (GhcPass p) = Located (Pat (GhcPass p))


{-
************************************************************************
*                                                                      *
*              HasSrcSpan Instance
*                                                                      *
************************************************************************
-}

type instance SrcSpanLess (LPat (GhcPass p)) = Pat (GhcPass p)
instance HasSrcSpan (LPat (GhcPass p)) where
  -- NB: The following chooses the behaviour of the outer location
  --     wrapper replacing the inner ones.
  composeSrcSpan (L sp p) =  if sp == noSrcSpan
                             then p
                             else XPat (L sp (stripSrcSpanPat p))

  -- NB: The following only returns the top-level location, if any.
  decomposeSrcSpan (XPat (L sp p)) = L sp (stripSrcSpanPat p)
  decomposeSrcSpan p               = L noSrcSpan p

stripSrcSpanPat :: LPat (GhcPass p) -> Pat (GhcPass p)
stripSrcSpanPat (XPat (L _  p)) = stripSrcSpanPat p
stripSrcSpanPat p               = p



-- ---------------------------------------------------------------------


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
--     hsRecFieldLbl = Unambiguous "x" NoExt :: AmbiguousFieldOcc RdrName
--
-- After the renamer, this will become:
--
--     hsRecFieldLbl = Ambiguous   "x" NoExt :: AmbiguousFieldOcc Name
--
-- (note that the Unambiguous constructor is not type-correct here).
-- The typechecker will determine the particular selector:
--
--     hsRecFieldLbl = Unambiguous "x" $sel:x:MkS  :: AmbiguousFieldOcc Id
--
-- See also Note [Disambiguating record fields] in TcExpr.

hsRecFields :: HsRecFields p arg -> [XCFieldOcc p]
hsRecFields rbinds = map (unLoc . hsRecFieldSel . unLoc) (rec_flds rbinds)

-- Probably won't typecheck at once, things have changed :/
hsRecFieldsArgs :: HsRecFields p arg -> [arg]
hsRecFieldsArgs rbinds = map (hsRecFieldArg . unLoc) (rec_flds rbinds)

hsRecFieldSel :: HsRecField pass arg -> Located (XCFieldOcc pass)
hsRecFieldSel = fmap extFieldOcc . hsRecFieldLbl

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

instance (p ~ GhcPass pass, OutputableBndrId p) => Outputable (Pat p) where
    ppr = pprPat

pprPatBndr :: OutputableBndr name => name -> SDoc
pprPatBndr var                  -- Print with type info if -dppr-debug is on
  = getPprStyle $ \ sty ->
    if debugStyle sty then
        parens (pprBndr LambdaBind var)         -- Could pass the site to pprPat
                                                -- but is it worth it?
    else
        pprPrefixOcc var

pprParendLPat :: (OutputableBndrId (GhcPass p))
              => PprPrec -> LPat (GhcPass p) -> SDoc
pprParendLPat p = pprParendPat p . unLoc

pprParendPat :: (OutputableBndrId (GhcPass p))
             => PprPrec -> Pat (GhcPass p) -> SDoc
pprParendPat p pat = sdocWithDynFlags $ \ dflags ->
                     if need_parens dflags pat
                     then parens (pprPat pat)
                     else  pprPat pat
  where
    need_parens dflags pat
      | CoPat {} <- pat = gopt Opt_PrintTypecheckerElaboration dflags
      | otherwise       = patNeedsParens p pat
      -- For a CoPat we need parens if we are going to show it, which
      -- we do if -fprint-typechecker-elaboration is on (c.f. pprHsWrapper)
      -- But otherwise the CoPat is discarded, so it
      -- is the pattern inside that matters.  Sigh.

pprPat :: (OutputableBndrId (GhcPass p)) => Pat (GhcPass p) -> SDoc
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
pprPat (CoPat _ co pat _)       = pprHsWrapper co $ \parens
                                            -> if parens
                                                 then pprParendPat appPrec pat
                                                 else pprPat pat
pprPat (SigPat _ pat ty)        = ppr pat <+> dcolon <+> ppr ty
pprPat (ListPat _ pats)         = brackets (interpp'SP pats)
pprPat (TuplePat _ pats bx)     = tupleParens (boxityTupleSort bx)
                                              (pprWithCommas ppr pats)
pprPat (SumPat _ pat alt arity) = sumParens (pprAlternative ppr pat alt arity)
pprPat (ConPatIn con details)   = pprUserCon (unLoc con) details
pprPat (ConPatOut { pat_con = con
                  , pat_tvs = tvs
                  , pat_dicts = dicts
                  , pat_binds = binds
                  , pat_args = details })
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
pprPat (XPat x)               = ppr x


pprUserCon :: (OutputableBndr con, OutputableBndrId (GhcPass p))
           => con -> HsConPatDetails (GhcPass p) -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details          = pprPrefixOcc c <+> pprConArgs details

pprConArgs :: (OutputableBndrId (GhcPass p))
           => HsConPatDetails (GhcPass p) -> SDoc
pprConArgs (PrefixCon pats) = sep (map (pprParendLPat appPrec) pats)
pprConArgs (InfixCon p1 p2) = sep [ pprParendLPat appPrec p1
                                  , pprParendLPat appPrec p2 ]
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

mkPrefixConPat :: DataCon ->
                  [OutPat (GhcPass p)] -> [Type] -> OutPat (GhcPass p)
-- Make a vanilla Prefix constructor pattern
mkPrefixConPat dc pats tys
  = noLoc $ ConPatOut { pat_con = noLoc (RealDataCon dc)
                      , pat_tvs = []
                      , pat_dicts = []
                      , pat_binds = emptyTcEvBinds
                      , pat_args = PrefixCon pats
                      , pat_arg_tys = tys
                      , pat_wrap = idHsWrapper }

mkNilPat :: Type -> OutPat (GhcPass p)
mkNilPat ty = mkPrefixConPat nilDataCon [] [ty]

mkCharLitPat :: SourceText -> Char -> OutPat (GhcPass p)
mkCharLitPat src c = mkPrefixConPat charDataCon
                          [noLoc $ LitPat NoExt (HsCharPrim src c)] []

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

isIrrefutableHsPat :: (OutputableBndrId (GhcPass p)) => LPat (GhcPass p) -> Bool
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
isIrrefutableHsPat
  = goL
  where
    goL = go . unLoc

    go (WildPat {})        = True
    go (VarPat {})         = True
    go (LazyPat {})        = True
    go (BangPat _ pat)     = goL pat
    go (CoPat _ _ pat _)   = go  pat
    go (ParPat _ pat)      = goL pat
    go (AsPat _ _ pat)     = goL pat
    go (ViewPat _ _ pat)   = goL pat
    go (SigPat _ pat _)    = goL pat
    go (TuplePat _ pats _) = all goL pats
    go (SumPat {})         = False
                    -- See Note [Unboxed sum patterns aren't irrefutable]
    go (ListPat {})        = False

    go (ConPatIn {})       = False     -- Conservative
    go (ConPatOut
        { pat_con  = (dL->L _ (RealDataCon con))
        , pat_args = details })
                           =
      isJust (tyConSingleDataCon_maybe (dataConTyCon con))
      -- NB: tyConSingleDataCon_maybe, *not* isProductTyCon, because
      -- the latter is false of existentials. See Trac #4439
      && all goL (hsConPatArgs details)
    go (ConPatOut
        { pat_con = (dL->L _ (PatSynCon _pat)) })
                           = False -- Conservative
    go (ConPatOut{})       = panic "ConPatOut:Impossible Match" -- due to #15884
    go (LitPat {})         = False
    go (NPat {})           = False
    go (NPlusKPat {})      = False

    -- We conservatively assume that no TH splices are irrefutable
    -- since we cannot know until the splice is evaluated.
    go (SplicePat {})      = False

    go (XPat {})           = False

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
patNeedsParens :: PprPrec -> Pat p -> Bool
patNeedsParens p = go
  where
    go (NPlusKPat {})    = p > opPrec
    go (SplicePat {})    = False
    go (ConPatIn _ ds)   = conPatNeedsParens p ds
    go cp@(ConPatOut {}) = conPatNeedsParens p (pat_args cp)
    go (SigPat {})       = p >= sigPrec
    go (ViewPat {})      = True
    go (CoPat _ _ p _)   = go p
    go (WildPat {})      = False
    go (VarPat {})       = False
    go (LazyPat {})      = False
    go (BangPat {})      = False
    go (ParPat {})       = False
    go (AsPat {})        = False
    go (TuplePat {})     = False
    go (SumPat {})       = False
    go (ListPat {})      = False
    go (LitPat _ l)      = hsLitNeedsParens p l
    go (NPat _ lol _ _)  = hsOverLitNeedsParens p (unLoc lol)
    go (XPat {})         = True -- conservative default

-- | @'conPatNeedsParens' p cp@ returns 'True' if the constructor patterns @cp@
-- needs parentheses under precedence @p@.
conPatNeedsParens :: PprPrec -> HsConDetails a b -> Bool
conPatNeedsParens p = go
  where
    go (PrefixCon args) = p >= appPrec && not (null args)
    go (InfixCon {})    = p >= opPrec
    go (RecCon {})      = False

-- | @'parenthesizePat' p pat@ checks if @'patNeedsParens' p pat@ is true, and
-- if so, surrounds @pat@ with a 'ParPat'. Otherwise, it simply returns @pat@.
parenthesizePat :: PprPrec -> LPat (GhcPass p) -> LPat (GhcPass p)
parenthesizePat p lpat@(dL->L loc pat)
  | patNeedsParens p pat = cL loc (ParPat NoExt lpat)
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
    ConPatOut {pat_dicts = dicts, pat_args  = args}
                     -> unionBags (listToBag dicts)
                                   $ unionManyBags
                                   $ map collectEvVarsLPat
                                   $ hsConPatArgs args
    SigPat  _ p _    -> collectEvVarsLPat p
    CoPat _ _ p _    -> collectEvVarsPat  p
    ConPatIn _  _    -> panic "foldMapPatBag: ConPatIn"
    _other_pat       -> emptyBag
