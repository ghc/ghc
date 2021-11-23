
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PatSyntax]{Abstract Haskell syntax---patterns}
-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*
module Language.Haskell.Syntax.Pat (
        Pat(..), LPat, MatchPat(..), LMatchPat,
        isVis, discardLInvisPats, discardLVisPats, ConLikeP,

        HsConPatDetails, hsConPatArgs,
        HsRecFields(..), HsFieldBind(..), LHsFieldBind,
        HsRecField, LHsRecField,
        HsRecUpdField, LHsRecUpdField,
        hsRecFields, hsRecFieldSel, hsRecFieldsArgs,
    ) where

import GHC.Prelude

import {-# SOURCE #-} Language.Haskell.Syntax.Expr (SyntaxExpr, LHsExpr, HsSplice)

-- friends:
import Language.Haskell.Syntax.Lit
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Type
import GHC.Types.Basic
-- others:
import GHC.Core.Ppr ( {- instance OutputableBndr TyVar -} )
import GHC.Utils.Outputable
import GHC.Types.SrcLoc
-- libraries:

type LPat p = XRec p (Pat p)

-- | Pattern
--
-- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnBang'

-- For details on above see note [exact print annotations] in GHC.Parser.Annotation
data Pat p
  =     ------------ Simple patterns ---------------
    WildPat     (XWildPat p)        -- ^ Wildcard Pattern
        -- The sole reason for a type on a WildPat is to
        -- support hsPatType :: Pat Id -> Type

       -- AZ:TODO above comment needs to be updated
  | VarPat      (XVarPat p)
                (LIdP p)     -- ^ Variable Pattern

                             -- See Note [Located RdrNames] in GHC.Hs.Expr
  | LazyPat     (XLazyPat p)
                (LPat p)                -- ^ Lazy Pattern
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnTilde'

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

  | AsPat       (XAsPat p)
                (LIdP p) (LPat p)    -- ^ As pattern
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnAt'

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

  | ParPat      (XParPat p)
               !(LHsToken "(" p)
                (LPat p)                -- ^ Parenthesised pattern
               !(LHsToken ")" p)
                                        -- See Note [Parens in HsSyn] in GHC.Hs.Expr
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'('@,
    --                                    'GHC.Parser.Annotation.AnnClose' @')'@

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | BangPat     (XBangPat p)
                (LPat p)                -- ^ Bang pattern
    -- ^ - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnBang'

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

        ------------ Lists, tuples, arrays ---------------
  | ListPat     (XListPat p)
                [LPat p]

    -- ^ Syntactic List
    --
    -- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'['@,
    --                                    'GHC.Parser.Annotation.AnnClose' @']'@

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

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

    -- For details on above see note [exact print annotations] in GHC.Parser.Annotation

        ------------ Constructor patterns ---------------
  | ConPat {
        pat_con_ext :: XConPat p,
        pat_con     :: XRec p (ConLikeP p),
        pat_args    :: HsConPatDetails p
    }
    -- ^ Constructor Pattern

        ------------ View patterns ---------------
  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnRarrow'

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | ViewPat       (XViewPat p)
                  (LHsExpr p)
                  (LPat p)
    -- ^ View Pattern

        ------------ Pattern splices ---------------
  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnOpen' @'$('@
  --        'GHC.Parser.Annotation.AnnClose' @')'@

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
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

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | NPlusKPat       (XNPlusKPat p)           -- Type of overall pattern
                    (LIdP p)                 -- n+k pattern
                    (XRec p (HsOverLit p))   -- It'll always be an HsIntegral
                    (HsOverLit p)            -- See Note [NPlusK patterns] in GHC.Tc.Gen.Pat
                     -- NB: This could be (PostTc ...), but that induced a
                     -- a new hs-boot file. Not worth it.

                    (SyntaxExpr p)   -- (>=) function, of type t1->t2->Bool
                    (SyntaxExpr p)   -- Name of '-' (see GHC.Rename.Env.lookupSyntax)
  -- ^ n+k pattern

        ------------ Pattern type signatures ---------------
  -- | - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnDcolon'

  -- For details on above see note [exact print annotations] in GHC.Parser.Annotation
  | SigPat          (XSigPat p)             -- After typechecker: Type
                    (LPat p)                -- Pattern with a type signature
                    (HsPatSigType (NoGhcTc p)) --  Signature can bind both
                                               --  kind and type vars

    -- ^ Pattern with a type signature

  -- Extension point; see Note [Trees That Grow] in Language.Haskell.Syntax.Extension
  | XPat
      !(XXPat p)

type family ConLikeP x

data MatchPat pass
  = VisPat (XVisPat pass) (LPat pass)
  | InvisTyVarPat (XInvisTyVarPat pass) (LIdP pass)
  -- this accepts only f @a, f @(a :: k) is in progress
  | InvisWildTyPat (XInvisWildTyPat pass)
  | XMatchPat !(XXMatchPat pass)

type LMatchPat pass = XRec pass (MatchPat pass)

isVis :: forall pass. UnXRec pass => LMatchPat pass -> Bool
isVis pat =
  case unXRec @pass pat of
    VisPat _ _ -> True
    _          -> False

discardLInvisPats :: forall pass. UnXRec pass => [LMatchPat pass] -> [LPat pass]
-- this is a temporary function that we remove for the final version
discardLInvisPats [] = []
discardLInvisPats (x : xs) =
  case unXRec @pass x of
    VisPat _ pat -> pat : discardLInvisPats xs
    _            -> discardLInvisPats xs

discardLVisPats :: forall pass. UnXRec pass => [LMatchPat pass] -> [MatchPat pass]
    -- this is a temporary function that we remove for the final version
discardLVisPats [] = []
discardLVisPats (x : xs) =
  case unXRec @pass x of
    pat@(InvisTyVarPat _ _) -> pat : discardLVisPats xs
    pat@(InvisWildTyPat _)  -> pat : discardLVisPats xs
    _                       -> discardLVisPats xs

-- ---------------------------------------------------------------------


-- | Haskell Constructor Pattern Details
type HsConPatDetails p = HsConDetails (HsPatSigType (NoGhcTc p)) (LPat p) (HsRecFields p (LPat p))

hsConPatArgs :: forall p . (UnXRec p) => HsConPatDetails p -> [LPat p]
hsConPatArgs (PrefixCon _ ps) = ps
hsConPatArgs (RecCon fs)      = map (hfbRHS . unXRec @p) (rec_flds fs)
hsConPatArgs (InfixCon p1 p2) = [p1,p2]

-- | Haskell Record Fields
--
-- HsRecFields is used only for patterns and expressions (not data type
-- declarations)
data HsRecFields p arg         -- A bunch of record fields
                                --      { x = 3, y = True }
        -- Used for both expressions and patterns
  = HsRecFields { rec_flds   :: [LHsRecField p arg],
                  rec_dotdot :: Maybe (Located Int) }  -- Note [DotDot fields]
  -- AZ:The XRec for LHsRecField makes the derivings fail.
  -- deriving (Functor, Foldable, Traversable)


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
type LHsFieldBind p id arg = XRec p (HsFieldBind id arg)

-- | Located Haskell Record Field
type LHsRecField  p arg = XRec p (HsRecField  p arg)

-- | Located Haskell Record Update Field
type LHsRecUpdField p   = XRec p (HsRecUpdField p)

-- | Haskell Record Field
type HsRecField p arg   = HsFieldBind (LFieldOcc p) arg

-- | Haskell Record Update Field
type HsRecUpdField p    = HsFieldBind (LAmbiguousFieldOcc p) (LHsExpr p)

-- | Haskell Field Binding
--
-- - 'GHC.Parser.Annotation.AnnKeywordId' : 'GHC.Parser.Annotation.AnnEqual',
--
-- For details on above see note [exact print annotations] in GHC.Parser.Annotation
data HsFieldBind lhs rhs = HsFieldBind {
        hfbAnn :: XHsFieldBind lhs,
        hfbLHS :: lhs,
        hfbRHS :: rhs,           -- ^ Filled in by renamer when punning
        hfbPun :: Bool           -- ^ Note [Punning]
  } deriving (Functor, Foldable, Traversable)


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
--     hfbLHS = Unambiguous "x" noExtField :: AmbiguousFieldOcc RdrName
--
-- After the renamer, this will become:
--
--     hfbLHS = Ambiguous   "x" noExtField :: AmbiguousFieldOcc Name
--
-- (note that the Unambiguous constructor is not type-correct here).
-- The typechecker will determine the particular selector:
--
--     hfbLHS = Unambiguous "x" $sel:x:MkS  :: AmbiguousFieldOcc Id
--
-- See also Note [Disambiguating record fields] in GHC.Tc.Gen.Head.

hsRecFields :: forall p arg.UnXRec p => HsRecFields p arg -> [XCFieldOcc p]
hsRecFields rbinds = map (hsRecFieldSel . unXRec @p) (rec_flds rbinds)

hsRecFieldsArgs :: forall p arg. UnXRec p => HsRecFields p arg -> [arg]
hsRecFieldsArgs rbinds = map (hfbRHS . unXRec @p) (rec_flds rbinds)

hsRecFieldSel :: forall p arg. UnXRec p => HsRecField p arg -> XCFieldOcc p
hsRecFieldSel = foExt . unXRec @p . hfbLHS


{-
************************************************************************
*                                                                      *
*              Printing patterns
*                                                                      *
************************************************************************
-}

instance (Outputable arg, Outputable (XRec p (HsRecField p arg)))
      => Outputable (HsRecFields p arg) where
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Nothing })
        = braces (fsep (punctuate comma (map ppr flds)))
  ppr (HsRecFields { rec_flds = flds, rec_dotdot = Just (unLoc -> n) })
        = braces (fsep (punctuate comma (map ppr (take n flds) ++ [dotdot])))
        where
          dotdot = text ".." <+> whenPprDebug (ppr (drop n flds))

instance (Outputable p, OutputableBndr p, Outputable arg)
      => Outputable (HsFieldBind p arg) where
  ppr (HsFieldBind { hfbLHS = f, hfbRHS = arg,
                     hfbPun = pun })
    = pprPrefixOcc f <+> (ppUnless pun $ equals <+> ppr arg)
