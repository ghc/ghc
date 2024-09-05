{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]
                                      -- in module Language.Haskell.Syntax.Extension
{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[PatSyntax]{Abstract Haskell syntax---patterns}
-}

-- See Note [Language.Haskell.Syntax.* Hierarchy] for why not GHC.Hs.*
module Language.Haskell.Syntax.Pat (
        Pat(..), LPat,
        ConLikeP,
        isInvisArgPat, isInvisArgLPat,
        isVisArgPat, isVisArgLPat,

        HsConPatDetails, hsConPatArgs,
        takeHsConPatTyArgs, dropHsConPatTyArgs,
        HsRecFields(..), XHsRecFields, HsFieldBind(..), LHsFieldBind,
        HsRecField, LHsRecField,
        HsRecUpdField, LHsRecUpdField,
        RecFieldsDotDot(..)
    ) where

import {-# SOURCE #-} Language.Haskell.Syntax.Expr (SyntaxExpr, LHsExpr, HsUntypedSplice)

-- friends:
import Language.Haskell.Syntax.Basic
import Language.Haskell.Syntax.Lit
import Language.Haskell.Syntax.Extension
import Language.Haskell.Syntax.Type

-- libraries:
import Data.Maybe
import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Bool
import Data.Data
import Data.Eq
import Data.Ord
import Data.Int
import Data.Function
import qualified Data.List
import Data.List.NonEmpty (NonEmpty)

type LPat p = XRec p (Pat p)

-- | Pattern
data Pat p
  =     ------------ Simple patterns ---------------
    WildPat     (XWildPat p)
    -- ^ Wildcard Pattern, i.e. @_@

  | VarPat      (XVarPat p)
                (LIdP p)
    -- ^ Variable Pattern, e.g. @x@

    -- See Note [Located RdrNames] in GHC.Hs.Expr
  | LazyPat     (XLazyPat p)
                (LPat p)
    -- ^ Lazy Pattern, e.g. @~x@
  | AsPat       (XAsPat p)
                (LIdP p)
                (LPat p)
    -- ^ As pattern, e.g. @x\@pat@
  | ParPat      (XParPat p)
                (LPat p)
    -- ^ Parenthesised pattern, e.g. @(x)@

    -- See Note [Parens in HsSyn] in GHC.Hs.Expr
  | BangPat     (XBangPat p)
                (LPat p)
    -- ^ Bang pattern, e.g. @!x@

        ------------ Lists, tuples, arrays ---------------
  | ListPat     (XListPat p)
                [LPat p]
    -- ^ Syntactic List, e.g. @[x]@ or @[x,y]@.
    -- Note that @[]@ and @(x:xs)@ patterns are both represented using 'ConPat'.

  | -- | Tuple pattern, e.g. @(x, y)@ (boxed tuples) or @(# x, y #)@ (requires @-XUnboxedTuples@)
    TuplePat    (XTuplePat p)    -- ^ After typechecking, holds the types of the tuple components
                [LPat p]         -- ^ Tuple sub-patterns
                Boxity

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

  | OrPat       (XOrPat p)
                (NonEmpty (LPat p))
    -- ^ Or Pattern, e.g. @(pat_1; ...; pat_n)@. Used by @-XOrPatterns@
    --
    -- @since 9.12.1

  | SumPat      (XSumPat p)        -- after typechecker, types of the alternative
                (LPat p)           -- Sum sub-pattern
                ConTag             -- Alternative (one-based)
                SumWidth           -- Arity (INVARIANT: ≥ 2)

    -- ^ Anonymous sum pattern, e.g. @(# x | #)@. Used by @-XUnboxedSums@

        ------------ Constructor patterns ---------------
  | ConPat {
        pat_con_ext :: XConPat p,
        pat_con     :: XRec p (ConLikeP p),
        pat_args    :: HsConPatDetails p
    }
    -- ^ Constructor Pattern, e.g. @()@, @[]@ or @Nothing@

        ------------ View patterns ---------------

  | ViewPat       (XViewPat p)
                  (LHsExpr p)
                  (LPat p)
    -- ^ View Pattern, e.g. @someFun -> pat@. Used by @-XViewPatterns@

        ------------ Pattern splices ---------------

  | SplicePat       (XSplicePat p)
                    (HsUntypedSplice p)
  -- ^  Splice Pattern, e.g. @$(pat)@

        ------------ Literal and n+k patterns ---------------
  | LitPat          (XLitPat p)
                    (HsLit p)
    -- ^ Literal Pattern
    --
    -- Used for __non-overloaded__ literal patterns:
    -- Int#, Char#, Int, Char, String, etc.

  | QualLitPat      (XQualLitPat p)
                    (HsQualLit p)
    -- ^ Qualified Literal Pattern

  | NPat            (XNPat p)            -- Overall type of pattern. Might be
                                         -- different than the literal's type
                                         -- if (==) or negate changes the type
                    (XRec p (HsOverLit p))     -- ALWAYS positive
                    (Maybe (SyntaxExpr p)) -- Just (Name of 'negate') for
                                           -- negative patterns, Nothing
                                           -- otherwise
                    (SyntaxExpr p)       -- Equality checker, of type t->t->Bool

  -- ^ Natural Pattern, used for all overloaded literals, including overloaded Strings
  -- with @-XOverloadedStrings@

  | -- | n+k pattern, e.g. @n+1@, used by @-XNPlusKPatterns@
   NPlusKPat       (XNPlusKPat p)           -- Type of overall pattern
                    (LIdP p)                 -- n+k pattern
                    (XRec p (HsOverLit p))   -- It'll always be an HsIntegral
                    (HsOverLit p)            -- See Note [NPlusK patterns] in GHC.Tc.Gen.Pat
                     -- NB: This could be (PostTc ...), but that induced a
                     -- a new hs-boot file. Not worth it.

                    (SyntaxExpr p)   -- (>=) function, of type t1->t2->Bool
                    (SyntaxExpr p)   -- Name of '-' (see GHC.Rename.Env.lookupSyntax)

        ------------ Pattern type signatures ---------------

  | SigPat          (XSigPat p)             -- After typechecker: Type
                    (LPat p)                -- Pattern with a type signature
                    (HsPatSigType (NoGhcTc p)) --  Signature can bind both
                                               --  kind and type vars

   -- ^ Pattern with a type signature, e.g. @x :: Int@

  | -- | Embed the syntax of types into patterns, e.g. @fn (type t) = rhs@.
    -- Enabled by @-XExplicitNamespaces@ in conjunction with @-XRequiredTypeArguments@.
    EmbTyPat        (XEmbTyPat p)
                    (HsTyPat (NoGhcTc p))

  | InvisPat (XInvisPat p) (HsTyPat (NoGhcTc p))
  -- ^ Type abstraction which brings into scope type variables associated with invisible forall.
  -- E.g. @fn \@t ... = rhs@. Used by @-XTypeAbstractions@.

  | ModifiedPat (XModifiedPat p) [HsModifier p] (LPat p)
  -- ^ Pattern with attached modifiers. See Note [Overview of Modifiers] and
  -- Note [Modifiers on patterns vs bindings].
  -- E.g. @(%X x) = ...@

  -- See Note [Invisible binders in functions] in GHC.Hs.Pat

  | -- | TTG Extension point; see Note [Trees That Grow] in Language.Haskell.Syntax.Extension
    XPat !(XXPat p)

type family ConLikeP x

{-
Note [Modifiers on patterns vs bindings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Modifiers can be attached both to patterns and to bindings. Modifiers on
bindings are used with @-XLinearTypes@ (see Note [Modifiers on bindings] in
Language.Haskell.Syntax.Binds). Modifiers on patterns are currently unused.

The modifiers proposal
(https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0370-modifiers.rst)
specifies that modifiers can be attached to patterns. These modifiers are
currently unused. The proposal also takes as a given that they can be attached
to bindings. (That syntax already existed when the proposal was written.) These
modifiers are used with @-XLinearTypes@, see Note [Modifiers on bindings] in
Language.Haskell.Syntax.Binds.

But the proposal isn't explicit about how to parse @let %m p = ...@. Naively it
could be either of

- @PatBind [%m] p@
- @PatBind [] (ModifiedPat [%m] p)@

But we need it to work with linear types, so we need to pick the first. We pick
it by parsing @%m p@ as a pattern (actually a 'PatBuilder'), and moving
modifiers at the root of the pattern to the binding, so that a 'PatBind' never
directly contains a 'ModifiedPat'. This logic is implemented in
'extract_pat_builder_modifiers' in GHC.Parser.PostProcess. In source code:

- @let %m p = ...@ attaches the modifier to the binding: @PatBind [%m] p@.
- @let (%m p) = ...@ attaches it to the pattern:
  @PatBind [] (ParPat (ModifiedPat [%m] p))@.
- @let %m x:xs = ...@ attaches it to the subpattern @x@:
  @PatBind [] (ConPat : [ModifiedPat [%m] x, xs])@.

This last is a breaking change: before modifiers were implemented, it would
instead have attached the %m to the binding. (And @let %m Just x = ...@ would
have done the same, where now it fails to parse.) So some programs that used to
be accepted with @-XLinearTypes@ are no longer accepted; but since linear
bindings must be strict, such programs would be rare.

Another possible solution (untried) might have been to specifically recognize
modifiers at the start of the binding, and not parse them as part of a pattern,
such that:

- @let %m p = ...@ and @let (%m p) = ...@ would be as above.
- @let %m x:xs = ...@ would be @PatBind [%m] (ConPat : [x, xs])@.
- @let %m Just x = ...@ would be @PatBind [%m] (ConPat Just [x])@.

This would make modifier parsing significantly different from how strict
bindings are parsed:

- @let !x = ...@ is a 'FunBind' with a 'SrcStrict' annotation.
- @let (!p) x = ...@ is a PatBind' holding a 'BangPat'.
- @let !x:y = ...@ parses like @let (!x):y = ...@.
- @let !Just x = ...@ fails to parse.

See Note [FunBind vs PatBind] in Language.Haskell.Syntax.Binds.
-}

-- ---------------------------------------------------------------------

isInvisArgPat :: Pat p -> Bool
isInvisArgPat InvisPat{} = True
isInvisArgPat _   = False

isInvisArgLPat :: forall p. (UnXRec p) => LPat p -> Bool
isInvisArgLPat = isInvisArgPat . unXRec @p

isVisArgPat :: Pat p -> Bool
isVisArgPat = not . isInvisArgPat

isVisArgLPat :: forall p. (UnXRec p) => LPat p -> Bool
isVisArgLPat = isVisArgPat . unXRec @p

-- | Haskell Constructor Pattern Details
type HsConPatDetails p = HsConDetails (LPat p) (HsRecFields p (LPat p))

hsConPatArgs :: forall p . (UnXRec p) => HsConPatDetails p -> [LPat p]
hsConPatArgs (PrefixCon ps)   = ps
hsConPatArgs (RecCon fs)      = Data.List.map (hfbRHS . unXRec @p) (rec_flds fs)
hsConPatArgs (InfixCon p1 p2) = [p1,p2]

takeHsConPatTyArgs :: forall p. (UnXRec p) => [LPat p] -> [HsTyPat (NoGhcTc p)]
takeHsConPatTyArgs (p : ps)
  | InvisPat _ tp <- unXRec @p p
  = tp : takeHsConPatTyArgs ps
takeHsConPatTyArgs _ = []

dropHsConPatTyArgs :: forall p. (UnXRec p) => [LPat p] -> [LPat p]
dropHsConPatTyArgs = Data.List.dropWhile (isInvisArgPat . unXRec @p)

-- | Haskell Record Fields
--
-- HsRecFields is used only for patterns and expressions (not data type
-- declarations)
data HsRecFields p arg         -- A bunch of record fields
                                --      { x = 3, y = True }
        -- Used for both expressions and patterns
  = HsRecFields { rec_ext    :: !(XHsRecFields p),
                  rec_flds   :: [LHsRecField p arg],
                  rec_dotdot :: Maybe (XRec p RecFieldsDotDot) }  -- Note [DotDot fields]
  -- AZ:The XRec for LHsRecField makes the derivings fail.
  -- deriving (Functor, Foldable, Traversable)

type family XHsRecFields p

-- | Newtype to be able to have a specific XRec instance for the Int in `rec_dotdot`
newtype RecFieldsDotDot = RecFieldsDotDot { unRecFieldsDotDot :: Int }
    deriving (Data, Eq, Ord)

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

-- | Haskell Record Field
type HsRecField p arg   = HsFieldBind (LFieldOcc p) arg

-- | Located Haskell Record Update Field
type LHsRecUpdField p q = XRec p (HsRecUpdField p q)

-- | Haskell Record Update Field
type HsRecUpdField p q  = HsFieldBind (LFieldOcc p) (LHsExpr q)

-- | Haskell Field Binding
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
-- See also Note [Disambiguating record updates] in GHC.Rename.Pat.
