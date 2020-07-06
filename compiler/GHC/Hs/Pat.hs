
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
                                      -- in module GHC.Hs.Extension
{-# LANGUAGE ViewPatterns         #-}
{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PatSyntax]{Abstract Haskell syntax---patterns}
-}

module GHC.Hs.Pat (
        Pat(..), LPat,
        ApiAnnSumPat(..),
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

        pprParendLPat, pprConArgs,
        pprLPat
    ) where

import GHC.Prelude

import {-# SOURCE #-} GHC.Hs.Expr (SyntaxExpr, LHsExpr, HsSplice, pprLExpr, pprSplice)

-- friends:
import GHC.Hs.Binds
import GHC.Hs.Lit
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
import GHC.Parser.Annotation
-- libraries:
import Data.Data hiding (TyCon,Fixity)

type LPat p = XRec p (Pat p)

type instance Anno (Pat (GhcPass p)) = SrcSpanAnnA

-- | Pattern
--
-- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnBang'

-- For details on above see note [Api annotations] in GHC.Parser.Annotation
data Pat p
  =     ------------ Simple patterns ---------------
    WildPat     (XWildPat p)        -- ^ Wildcard Pattern
        -- The sole reason for a type on a WildPat is to
        -- support hsPatType :: Pat Id -> Type

       -- AZ:TODO above comment needs to be updated
  | VarPat      (XVarPat p)
                (LIdP p)     -- ^ Variable Pattern
                -- (LocatedN (IdP p))  -- ^ Variable Pattern
                       -- AZ: old one

                             -- See Note [Located RdrNames] in GHC.Hs.Expr
  | LazyPat     (XLazyPat p)
                (LPat p)                -- ^ Lazy Pattern
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnTilde'

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | AsPat       (XAsPat p)
                (LIdP p) (LPat p)    -- ^ As pattern
                -- (LocatedN (IdP p)) (LPat p)    -- ^ As pattern
                       -- AZ: old one
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnAt'

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

  | ParPat      (XParPat p)
                (LPat p)                -- ^ Parenthesised pattern
                                        -- See Note [Parens in HsSyn] in GHC.Hs.Expr
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'('@,
    --                                    'GHC.Parser.Annotation.AnnClose' @')'@

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | BangPat     (XBangPat p)
                (LPat p)                -- ^ Bang pattern
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnBang'

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

        ------------ Lists, tuples, arrays ---------------
  | ListPat     (XListPat p)
                [LPat p]
                   -- For OverloadedLists a Just (ty,fn) gives
                   -- overall type of the pattern, and the toList
-- function to convert the scrutinee to a list value

    -- ^ Syntactic List
    --
    -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'['@,
    --                                    'GHC.Parser.Annotation.AnnClose' @']'@

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

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
    -- - 'GHC.Parser.Annotation.AnnKeywordId' :
    --            'GHC.Parser.Annotation.AnnOpen' @'('@ or @'(#'@,
    --            'GHC.Parser.Annotation.AnnClose' @')'@ or  @'#)'@

  | SumPat      (XSumPat p)        -- after typechecker, types of the alternative
                (LPat p)           -- Sum sub-pattern
                ConTag             -- Alternative (one-based)
                Arity              -- Arity (INVARIANT: ≥ 2)
    -- ^ Anonymous sum pattern
    --
    -- - 'GHC.Parser.Annotation.AnnKeywordId' :
    --            'GHC.Parser.Annotation.AnnOpen' @'(#'@,
    --            'GHC.Parser.Annotation.AnnClose' @'#)'@

    -- For details on above see note [Api annotations] in GHC.Parser.Annotation

        ------------ Constructor patterns ---------------
  | ConPat {
        pat_con_ext :: XConPat p,
        pat_con     :: XRec p (ConLikeP p),
        -- pat_con     :: LocatedN (ConLikeP p),
                       -- AZ: old one
        pat_args    :: HsConPatDetails p
    }
    -- ^ Constructor Pattern

        ------------ View patterns ---------------
  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnRarrow'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | ViewPat       (XViewPat p)     -- The overall type of the pattern
                                   -- (= the argument type of the view function)
                                   -- for hsPatType.
                  (LHsExpr p)
                  (LPat p)
    -- ^ View Pattern

        ------------ Pattern splices ---------------
  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'$('@
  --        'GHC.Parser.Annotation.AnnClose' @')'@

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
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
                    (XRec p (HsOverLit p))     -- ALWAYS positive
                    (Maybe (SyntaxExpr p)) -- Just (Name of 'negate') for
                                           -- negative patterns, Nothing
                                           -- otherwise
                    (SyntaxExpr p)       -- Equality checker, of type t->t->Bool

  -- ^ Natural Pattern
  --
  -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnVal' @'+'@

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | NPlusKPat       (XNPlusKPat p)           -- Type of overall pattern
                    (LIdP p)                 -- n+k pattern
                    -- (LocatedA (IdP p))       -- n+k pattern
                       -- AZ: old one
                    (XRec p (HsOverLit p))   -- It'll always be an HsIntegral
                    -- (Located (HsOverLit p))  -- It'll always be an HsIntegral
                       -- AZ: old one
                    (HsOverLit p)            -- See Note [NPlusK patterns] in GHC.Tc.Gen.Pat
                     -- NB: This could be (PostTc ...), but that induced a
                     -- a new hs-boot file. Not worth it.

                    (SyntaxExpr p)   -- (>=) function, of type t1->t2->Bool
                    (SyntaxExpr p)   -- Name of '-' (see GHC.Rename.Env.lookupSyntax)
  -- ^ n+k pattern

        ------------ Pattern type signatures ---------------
  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon'

  -- For details on above see note [Api annotations] in GHC.Parser.Annotation
  | SigPat          (XSigPat p)             -- After typechecker: Type
                    (LPat p)                -- Pattern with a type signature
                    (HsPatSigType (NoGhcTc p)) --  Signature can bind both
                                               --  kind and type vars

    -- ^ Pattern with a type signature

  -- | Trees that Grow extension point for new constructors
  | XPat
      !(XXPat p)


-- ---------------------------------------------------------------------

data ListPatTc
  = ListPatTc
      Type                             -- The type of the elements
      (Maybe (Type, SyntaxExpr GhcTc)) -- For rebindable syntax

type instance XWildPat GhcPs = NoExtField
type instance XWildPat GhcRn = NoExtField
type instance XWildPat GhcTc = Type

type instance XVarPat  (GhcPass _) = NoExtField

type instance XLazyPat GhcPs = ApiAnn -- For '~'
type instance XLazyPat GhcRn = NoExtField
type instance XLazyPat GhcTc = NoExtField

type instance XAsPat   GhcPs = ApiAnn -- For '@'
type instance XAsPat   GhcRn = NoExtField
type instance XAsPat   GhcTc = NoExtField

type instance XParPat  (GhcPass _) = ApiAnn' AnnParen

type instance XBangPat GhcPs = ApiAnn -- For '!'
type instance XBangPat GhcRn = NoExtField
type instance XBangPat GhcTc = NoExtField

-- Note: XListPat cannot be extended when using GHC 8.0.2 as the bootstrap
-- compiler, as it triggers https://gitlab.haskell.org/ghc/ghc/issues/14396 for
-- `SyntaxExpr`
type instance XListPat GhcPs = ApiAnn' AnnList
type instance XListPat GhcRn = Maybe (SyntaxExpr GhcRn)
type instance XListPat GhcTc = ListPatTc

type instance XTuplePat GhcPs = ApiAnn
type instance XTuplePat GhcRn = NoExtField
type instance XTuplePat GhcTc = [Type]

type instance XSumPat GhcPs = ApiAnn' ApiAnnSumPat
type instance XSumPat GhcRn = NoExtField
type instance XSumPat GhcTc = [Type]

type instance XConPat GhcPs = ApiAnn
type instance XConPat GhcRn = NoExtField
type instance XConPat GhcTc = ConPatTc

type instance XViewPat GhcPs = ApiAnn
type instance XViewPat GhcRn = NoExtField
type instance XViewPat GhcTc = Type

type instance XSplicePat (GhcPass _) = NoExtField
type instance XLitPat    (GhcPass _) = NoExtField

type instance XNPat GhcPs = ApiAnn
type instance XNPat GhcRn = ApiAnn
type instance XNPat GhcTc = Type

type instance XNPlusKPat GhcPs = ApiAnn
type instance XNPlusKPat GhcRn = NoExtField
type instance XNPlusKPat GhcTc = Type

type instance XSigPat GhcPs = ApiAnn
type instance XSigPat GhcRn = NoExtField
type instance XSigPat GhcTc = Type

type instance XXPat GhcPs = NoExtCon
type instance XXPat GhcRn = NoExtCon
type instance XXPat GhcTc = CoPat
  -- After typechecking, we add one extra constructor: CoPat

type instance Anno (HsOverLit (GhcPass p)) = SrcSpan

type family ConLikeP x

type instance ConLikeP GhcPs = RdrName -- IdP GhcPs
type instance ConLikeP GhcRn = Name    -- IdP GhcRn
type instance ConLikeP GhcTc = ConLike

type instance Anno ConLike = SrcSpanAnnName

-- ---------------------------------------------------------------------

-- API Annotations types

data ApiAnnSumPat = ApiAnnSumPat
      { sumPatParens      :: [AddApiAnn]
      , sumPatVbarsBefore :: [RealSrcSpan]
      , sumPatVbarsAfter  :: [RealSrcSpan]
      } deriving Data

-- ---------------------------------------------------------------------


-- | Haskell Constructor Pattern Details
type HsConPatDetails p = HsConDetails (LPat p) (HsRecFields p (LPat p))

hsConPatArgs :: HsConPatDetails p -> [LPat p]
hsConPatArgs (PrefixCon ps)   = ps
hsConPatArgs (RecCon fs)      = map (hsRecFieldArg . unLoc) (rec_flds fs)
hsConPatArgs (InfixCon p1 p2) = [p1,p2]

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

-- | Haskell Record Fields
--
-- HsRecFields is used only for patterns and expressions (not data type
-- declarations)
data HsRecFields p arg         -- A bunch of record fields
                                --      { x = 3, y = True }
        -- Used for both expressions and patterns
  = HsRecFields { rec_flds   :: [LHsRecField p arg],
                  rec_dotdot :: Maybe (Located Int) }  -- Note [DotDot fields]
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
type LHsRecField' p arg = LocatedA (HsRecField' p arg)

-- | Located Haskell Record Field
type LHsRecField  p arg = LocatedA (HsRecField  p arg)

-- | Located Haskell Record Update Field
type LHsRecUpdField p   = LocatedA (HsRecUpdField p)

-- | Haskell Record Field
type HsRecField    p arg = HsRecField' (FieldOcc p) arg

-- | Haskell Record Update Field
type HsRecUpdField p     = HsRecField' (AmbiguousFieldOcc p) (LHsExpr p)

-- | Haskell Record Field
--
-- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnEqual',
--
-- For details on above see note [Api annotations] in GHC.Parser.Annotation
data HsRecField' id arg = HsRecField {
        hsRecFieldAnn :: ApiAnn,
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
-- unambiguous updates can be represented by 'GHC.HsToCore.Quote.repUpdFields'.
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
--     hsRecFieldLbl = Unambiguous "x" noExtField :: AmbiguousFieldOcc RdrName
--
-- After the renamer, this will become:
--
--     hsRecFieldLbl = Ambiguous   "x" noExtField :: AmbiguousFieldOcc Name
--
-- (note that the Unambiguous constructor is not type-correct here).
-- The typechecker will determine the particular selector:
--
--     hsRecFieldLbl = Unambiguous "x" $sel:x:MkS  :: AmbiguousFieldOcc Id
--
-- See also Note [Disambiguating record fields] in GHC.Tc.Gen.Head.

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

instance (OutputableBndrId p)
    => Outputable (Pat (GhcPass p)) where
    ppr = pprPat

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

pprParendPat :: forall p. (OutputableBndrId p)
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

pprPat :: forall p. (OutputableBndrId p)
       => Pat (GhcPass p) -> SDoc
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
  GhcPs -> noExtCon ext
  GhcRn -> noExtCon ext
#endif
  GhcTc -> pprHsWrapper co $ \parens ->
      if parens
      then pprParendPat appPrec pat
      else pprPat pat
    where CoPat co pat _ = ext

pprUserCon :: (OutputableBndr con, OutputableBndrId p,
                     Outputable (Anno (IdGhcP p)))
           => con -> HsConPatDetails (GhcPass p) -> SDoc
pprUserCon c (InfixCon p1 p2) = ppr p1 <+> pprInfixOcc c <+> ppr p2
pprUserCon c details          = pprPrefixOcc c <+> pprConArgs details

pprConArgs :: (OutputableBndrId p,
                     Outputable (Anno (IdGhcP p)))
           => HsConPatDetails (GhcPass p) -> SDoc
pprConArgs (PrefixCon pats) = fsep (map (pprParendLPat appPrec) pats)
pprConArgs (InfixCon p1 p2) = sep [ pprParendLPat appPrec p1
                                  , pprParendLPat appPrec p2 ]
pprConArgs (RecCon rpats)   = ppr rpats

instance (Outputable arg)
      => Outputable (HsRecFields p arg) where
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Nothing })
        = braces (fsep (punctuate comma (map ppr flds)))
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Just (unLoc -> n) })
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
                  [LPat GhcTc] -> [Type] -> LPat GhcTc
-- Make a vanilla Prefix constructor pattern
mkPrefixConPat dc pats tys
  = noLocA $ ConPat { pat_con = noLocA (RealDataCon dc)
                    , pat_args = PrefixCon pats
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
           -- NB: tyConSingleDataCon_maybe, *not* isProductTyCon, because
           -- the latter is false of existentials. See #4439
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
    go (ConPat { pat_args = ds})
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
conPatNeedsParens :: PprPrec -> HsConDetails a b -> Bool
conPatNeedsParens p = go
  where
    go (PrefixCon args) = p >= appPrec && not (null args)
    go (InfixCon {})    = p >= opPrec
    go (RecCon {})      = False

-- | @'parenthesizePat' p pat@ checks if @'patNeedsParens' p pat@ is true, and
-- if so, surrounds @pat@ with a 'ParPat'. Otherwise, it simply returns @pat@.
parenthesizePat :: IsPass p
                => PprPrec
                -> LPat (GhcPass p)
                -> LPat (GhcPass p)
parenthesizePat p lpat@(L loc pat)
  | patNeedsParens p pat = L loc (ParPat noAnn lpat)
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
