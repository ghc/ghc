{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1994-1998

Core-syntax unfoldings

Unfoldings (which can travel across module boundaries) are in Core
syntax (namely @CoreExpr@s).

The type @Unfolding@ sits ``above'' simply-Core-expressions
unfoldings, capturing ``higher-level'' things we know about a binding,
usually things that the simplifier found out (e.g., ``it's a
literal'').  In the corner of a @CoreUnfolding@ unfolding, you will
find, unsurprisingly, a Core expression.
-}



module GHC.Core.Unfold (
        Unfolding, UnfoldingGuidance,   -- Abstract types

        ExprTree, mkExprTree, exprTreeSize, altTreesSize,
        exprTreeWillInline, couldBeSmallEnoughToInline,
        ArgDigest(..), hasArgInfo,

        Size, InlineContext(..),

        UnfoldingOpts (..), defaultUnfoldingOpts,
        updateCreationThreshold, updateUseThreshold,
        updateFunAppDiscount, updateDictDiscount,
        updateVeryAggressive, updateCaseScaling,
        updateCaseThreshold, updateReportPrefix,

        inlineBoringOk, calcUnfoldingGuidance,
        uncondInlineJoin
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Utils
import GHC.Core.DataCon
import GHC.Core.Type
import GHC.Core.Class( Class )
import GHC.Core.Predicate( isUnaryClass )

import GHC.Types.Id
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Literal
import GHC.Types.Id.Info
import GHC.Types.RepType ( isZeroBitTy )
import GHC.Types.Basic  ( Arity )
import GHC.Types.Tickish

import GHC.Builtin.Names
import GHC.Builtin.PrimOps

import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.Bag
import GHC.Data.Maybe

import qualified Data.ByteString as BS
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NE


{- Note [Overview of inlining heuristics]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These inlining heurstics all concern callSiteInline; that is, the
decision about whether or not to inline a let-binding.  It does not
concern inlining used-once things, or things with a trivial RHS, which
kills the let-binding altogether.

Key examples
------------
Example 1:

   let f x = case x of
               A -> True
               B -> <big>
   in ...(f A)....(f B)...

Even though f's entire RHS is big, it collapses to something small when applied
to A.  We'd like to spot this.

Example 2:

   let f x = case x of
               (p,q) -> case p of
                           A -> True
                           B -> <big>
   in ...(f (A,3))....

This is similar to Example 1, but nested.

Example 3:

   let j x = case y of
               A -> True
               B -> <big>
   in case y of
         A -> ..(j 3)...(j 4)....
         B -> ...

Here we want to spot that although the free far `y` is unknown at j's definition
site, we know that y=A at the two calls in the A-alternative of the body. If `y`
had been an argument we'd have spotted this; we'd like to get the same goodness
when `y` is a free variable.

This kind of thing can occur a lot with join points.

Example 4: result discounts:

   let f x = case x of
               A -> (e1,e2)
               B -> (e3,e4)
               C -> e5
    in \y -> ...case f y of { (a,b) -> blah }

Here there is nothing interesting about f's /argument/, but:
  * Many of f's cases return a data constructur (True or False)
  * The call of `f` scrutinises its result

If we inline `f`, the 'case' will cancel with pair constrution, we should be keener
to inline `f` than if it was called in a boring context. We say that `f`  has a
/result discount/ meaning that we should apply a discount if `f` is called in
a case context.

Example 5: totally boring

   let f x = not (g x x)
   in ....(\y. f y)...

Here,there is /nothing/ interesting about either the arguments or the result
coninuation of the call (f y).  There is no point in inlining, even if f's RHS
is small, as it is here.

Design overview
---------------
The question is whether or not to inline f = rhs.
The key idea is to abstract `rhs` to an ExprTree, which gives a measure of
size, but records structure for case-expressions.

The moving parts
-----------------
* An unfolding is accompanied (in its UnfoldingGuidance) with its GHC.Core.ExprTree,
  computed by GHC.Core.Unfold.mkExprTree.

* At a call site, GHC.Core.Opt.Simplify.Inline.contArgDigests constructs an ArgDigest
  for each value argument. This reflects any nested data construtors.

* Then GHC.Core.Unfold.exprTreeSize takes information about the context of the
  call (particularly the ArgDigest for each argument) and computes a final size
  for the inlined body, taking account of case-of-known-consructor.

-}

{- *********************************************************************
*                                                                      *
                     UnfoldingOpts
*                                                                      *
********************************************************************* -}

-- | Unfolding options
data UnfoldingOpts = UnfoldingOpts
   { unfoldingCreationThreshold :: !Size
      -- ^ Threshold above which unfoldings are not *created*

   , unfoldingUseThreshold :: !Size
      -- ^ Threshold above which unfoldings are not *inlined*

   , unfoldingFunAppDiscount :: !Discount
      -- ^ Discount for lambdas that are used (applied)

   , unfoldingDictDiscount :: !Discount
      -- ^ Discount for dictionaries

   , unfoldingVeryAggressive :: !Bool
      -- ^ Force inlining in many more cases

   , unfoldingCaseThreshold :: !Int
      -- ^ Don't consider depth up to x

   , unfoldingCaseScaling :: !Int
      -- ^ Penalize depth with 1/x

   , exprTreeCaseWidth :: !Int
      -- ^ Bale out entirely with a case width greater than this
      -- See Note [Bale out on very wide case expressions]

   , exprTreeCaseDepth :: !Int
      -- ^ Don't make ExprTrees with a case depth greater than this

   , unfoldingReportPrefix :: !(Maybe String)
      -- ^ Only report inlining decisions for names with this prefix
   }

defaultUnfoldingOpts :: UnfoldingOpts
defaultUnfoldingOpts = UnfoldingOpts
   { unfoldingCreationThreshold = 750
      -- The unfoldingCreationThreshold threshold must be reasonably high
      -- to take account of possible discounts.
      -- E.g. 450 is not enough in 'fulsom' for Interval.sqr to
      -- inline into Csg.calc (The unfolding for sqr never makes it
      -- into the interface file.)

   , unfoldingUseThreshold   = 80
      -- Adjusted 90 -> 75 when adding discounts for free variables which
      -- generally make things more likely to inline.  Reducing the threshold
      -- eliminates some undesirable compile-time regressions (e.g. T10412a)
      --
      -- Previously: adjusted upwards in #18282, when I reduced
      -- the result discount for constructors.

   , unfoldingFunAppDiscount = 45
      -- Be fairly keen to inline a function if that means
      -- we'll be able to pick the right method from a dictionary

   , unfoldingDictDiscount   = 30
      -- Be fairly keen to inline a function if that means
      -- we'll be able to pick the right method from a dictionary

   , unfoldingVeryAggressive = False

      -- Only apply scaling once we are deeper than threshold cases
      -- in an RHS.
   , unfoldingCaseThreshold = 2

      -- Penalize depth with (size*depth)/scaling
   , unfoldingCaseScaling = 30

      -- Don't filter inlining decision reports
   , unfoldingReportPrefix = Nothing

     -- Bale out at exprTreeCaseWidth
     -- See Note [Bale out on very wide case expressions]
   , exprTreeCaseWidth = 20

     -- Don't record CaseOf beyond exprTreeCaseDepth
   , exprTreeCaseDepth = 4
   }

-- Helpers for "GHC.Driver.Session"

updateCreationThreshold :: Size -> UnfoldingOpts -> UnfoldingOpts
updateCreationThreshold n opts = opts { unfoldingCreationThreshold = n }

updateUseThreshold :: Size -> UnfoldingOpts -> UnfoldingOpts
updateUseThreshold n opts = opts { unfoldingUseThreshold = n }

updateFunAppDiscount :: Discount -> UnfoldingOpts -> UnfoldingOpts
updateFunAppDiscount n opts = opts { unfoldingFunAppDiscount = n }

updateDictDiscount :: Discount -> UnfoldingOpts -> UnfoldingOpts
updateDictDiscount n opts = opts { unfoldingDictDiscount = n }

updateVeryAggressive :: Bool -> UnfoldingOpts -> UnfoldingOpts
updateVeryAggressive n opts = opts { unfoldingVeryAggressive = n }


updateCaseThreshold :: Size -> UnfoldingOpts -> UnfoldingOpts
updateCaseThreshold n opts = opts { unfoldingCaseThreshold = n }

updateCaseScaling :: Int -> UnfoldingOpts -> UnfoldingOpts
updateCaseScaling n opts = opts { unfoldingCaseScaling = n }

updateReportPrefix :: Maybe String -> UnfoldingOpts -> UnfoldingOpts
updateReportPrefix n opts = opts { unfoldingReportPrefix = n }


{-
************************************************************************
*                                                                      *
\subsection{The UnfoldingGuidance type}
*                                                                      *
************************************************************************
-}

{- Note [inlineBoringOk]
~~~~~~~~~~~~~~~~~~~~~~~~
See Note [INLINE for small functions]

The function `inlineBoringOk` returns True (boringCxtOk) if the supplied
unfolding, which looks like (\x y z. body), is such that the result of
inlining a saturated call is no bigger than `body`.  Some wrinkles:

(IB1) An important case is
    - \x. (x `cast` co)

(IB2) If `body` looks like a data constructor worker, we become keener
  to inline, by ignoring the number of arguments; we just insist they
  are all trivial.  Reason: in a call like `f (g x y)`, if `g` unfolds
  to a data construtor, we can allocate a data constructor instead of
  a thunk (g x y).

  A case in point where a GADT data constructor failed to inline (#25713)
      $WK = /\a \x. K @a <co> x
  We really want to inline a boring call to $WK so that we allocate
  a data constructor not a thunk ($WK @ty x).

  But not for nullary constructors!  We don't want to turn
     f ($WRefl @ty)
  into
     f (Refl @ty <co>)
   because the latter might allocate, whereas the former shares.
   (You might wonder if (Refl @ty <co>) should allocate, but I think
   that currently it does.)  So for nullary constructors, `inlineBoringOk`
   returns False.

(IB3) Types and coercions do not count towards the expression size.
      They are ultimately erased.

(IB4) If there are no value arguments, `inlineBoringOk` we have to be
  careful (#17182).  If we have
      let y = x @Int in f y y
  there’s no reason not to inline y at both use sites — no work is
  actually duplicated.

  But not so for coercion arguments! Unlike type arguments, which have
  no runtime representation, coercion arguments *do* have a runtime
  representation (albeit the zero-width VoidRep, see Note [Coercion
  tokens] in "GHC.CoreToStg").  For example:
       let y = g @Int <co> in g y y
  Here `co` is a value argument, and calling it twice might duplicate
  work.

  Even if `g` is a data constructor, so no work is duplicated,
  inlining `y` might duplicate allocation of a data constructor object
  (#17787). See also (IB2).

  TL;DR: if `is_fun` is False, so we have no value arguments, we /do/
  count coercion arguments, despite (IB3).

(IB5) You might wonder about an unfolding like  (\x y z -> x (y z)),
  whose body is, in some sense, just as small as (g x y z).
  But `inlineBoringOk` doesn't attempt anything fancy; it just looks
  for a function call with trivial arguments, Keep it simple.

(IB6) If we have an unfolding (K op) where K is a unary-class data constructor,
  we want to inline it!  So that we get calls (f op), which in turn can see (in
  STG land) that `op` is already evaluated and properly tagged. (If `op` isn't
  trivial we will have baled out before we get to the Var case.)  This made
  a big difference in benchmarks for the `effectful` library; details in !10479.

  See Note [Unary class magic] in GHC/Core/TyCon.
-}

inlineBoringOk :: CoreExpr -> Bool
-- True => the result of inlining the expression is
--         no bigger than the expression itself
--     eg      (\x y -> f y x)
-- See Note [inlineBoringOk]
inlineBoringOk e
  = go 0 e
  where
    is_fun = isValFun e

    go :: Int -> CoreExpr -> Bool
    -- credit = #(value lambdas) = #(value args)
    go credit (Lam x e) | isRuntimeVar x  = go (credit+1) e
                        | otherwise       = go credit e      -- See (IB3)

    go credit (App f (Type {}))           = go credit f      -- See (IB3)
    go credit (App f (Coercion {}))
      | is_fun                            = go credit f      -- See (IB3)
      | otherwise                         = go (credit-1) f  -- See (IB4)
    go credit (App f a) | exprIsTrivial a = go (credit-1) f

    go credit (Case e b _ alts)
      | null alts
      = go credit e   -- EmptyCase is like e
      | Just rhs <- isUnsafeEqualityCase e b alts
      = go credit rhs -- See Note [Inline unsafeCoerce]

    go credit (Tick _ e) = go credit e      -- dubious
    go credit (Cast e _) = go credit e      -- See (IB3)

    -- Lit: we assume credit >= 0; literals aren't functions
    go _      (Lit l)    = litIsTrivial l && boringCxtOk

    go credit (Var v) | isDataConWorkId v, is_fun = boringCxtOk  -- See (IB2)
                      | isUnaryClassId v          = boringCxtOk  -- See (IB6)
                      | credit >= 0               = boringCxtOk
                      | otherwise                 = boringCxtNotOk

    go _ _ = boringCxtNotOk

isValFun :: CoreExpr -> Bool
-- True of functions with at least
-- one top-level value lambda
isValFun (Lam b e) | isRuntimeVar b = True
                   | otherwise      = isValFun e
isValFun _                          = False

calcUnfoldingGuidance
        :: UnfoldingOpts
        -> Bool          -- Definitely a top-level, bottoming binding
        -> Bool          -- True <=> join point
        -> CoreExpr      -- Expression to look at
        -> UnfoldingGuidance
calcUnfoldingGuidance opts is_top_bottoming is_join (Tick t expr)
  | not (tickishIsCode t)  -- non-code ticks don't matter for unfolding
  = calcUnfoldingGuidance opts is_top_bottoming is_join expr
calcUnfoldingGuidance opts is_top_bottoming is_join expr
  = case mkExprTree opts val_bndrs body of
      Nothing -> UnfNever
      Just et@(ExprTree { et_wc_tot = tot })
        | uncondInline is_join expr bndrs n_val_bndrs body tot
        -> UnfWhen { ug_unsat_ok  = unSaturatedOk
                   , ug_boring_ok =  boringCxtOk
                   , ug_arity     = n_val_bndrs }   -- Note [INLINE for small functions]

        | is_top_bottoming
        -> UnfNever   -- See Note [Do not inline top-level bottoming functions]

        | otherwise
        -> UnfIfGoodArgs { ug_args = val_bndrs, ug_tree = et }

  where
    (bndrs, body) = collectBinders expr
    val_bndrs   = filter isId bndrs
    n_val_bndrs = length val_bndrs

couldBeSmallEnoughToInline :: UnfoldingOpts -> Size -> CoreExpr -> Bool
-- We use 'couldBeSmallEnoughToInline' to avoid exporting inlinings that
-- we ``couldn't possibly use'' on the other side.  Can be overridden
-- w/flaggery.  Just the same as smallEnoughToInline, except that it has no
-- actual arguments.
couldBeSmallEnoughToInline opts threshold rhs
  = isJust (mkExprTree opts' [] body)
  where
    opts' = opts { unfoldingCreationThreshold = threshold }
            -- We use a different (and larger) theshold here for
            -- creating specialised copies of the function
    (_, body) = collectBinders rhs

uncondInline :: Bool -> CoreExpr -> [Var] -> Arity -> CoreExpr -> Size -> Bool
-- Inline unconditionally if there no size increase
-- Size of call is arity (+1 for the function)
-- See Note [INLINE for small functions]
uncondInline is_join rhs bndrs arity body size
  | is_join   = uncondInlineJoin bndrs body
  | arity > 0 = size <= 10 * (arity + 1) -- See Note [INLINE for small functions] (1)
  | otherwise = exprIsTrivial rhs        -- See Note [INLINE for small functions] (4)

uncondInlineJoin :: [Var] -> CoreExpr -> Bool
-- See #25723 and Note [Duplicating join points] point (DJ3)
--                in GHC.Core.Opt.Simplify.Iteration
uncondInlineJoin bndrs body

  -- (DJ3)(a)
  | exprIsTrivial body
  = True   -- Nullary constructors, literals

  -- (DJ3)(b) and (DJ3)(c) combined
  | indirectionOrAppWithoutFVs
  = True

  | otherwise
  = False

  where
    -- (DJ3)(b):
    -- - $j1 x = $j2 y x |> co  -- YES, inline indirection regardless of free vars
    -- (DJ3)(c):
    -- - $j1 x y = K y x |> co  -- YES, inline!
    -- - $j2 x = K f x          -- No, don't! (because f is free)
    indirectionOrAppWithoutFVs = go False body

    go !seen_fv (App f a)
      | Just has_fv <- go_arg a
                          = go (seen_fv || has_fv) f
      | otherwise         = False       -- Not trivial
    go seen_fv (Var v)
      | isJoinId v        = True        -- Indirection to another join point; always inline
      | isDataConId v     = not seen_fv -- e.g. $j a b = K a b
      | v `elem` bndrs    = not seen_fv -- e.g. $j a b = b a
    go seen_fv (Cast e _) = go seen_fv e
    go seen_fv (Tick _ e) = go seen_fv e
    go _ _                = False

    -- go_arg returns:
    --  - `Nothing` if arg is not trivial
    --  - `Just True` if arg is trivial but contains free var, literal, or constructor
    --  - `Just False` if arg is trivial without free vars
    go_arg (Type {})     = Just False
    go_arg (Coercion {}) = Just False
    go_arg (Lit l)
      | litIsTrivial l   = Just True    -- e.g. $j x = $j2 x 7 YES, but $j x = K x 7 NO
      | otherwise        = Nothing
    go_arg (App f a)
      | isTyCoArg a      = go_arg f     -- e.g. $j f = K (f @a)
      | otherwise        = Nothing
    go_arg (Cast e _)    = go_arg e
    go_arg (Tick _ e)    = go_arg e
    go_arg (Var f)       = Just $! f `notElem` bndrs
    go_arg _             = Nothing

{- Note [Inline unsafeCoerce]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We really want to inline unsafeCoerce, even when applied to boring
arguments.  It doesn't look as if its RHS is smaller than the call
   unsafeCoerce x = case unsafeEqualityProof @a @b of UnsafeRefl -> x
but that case is discarded in CoreToStg -- see Note [Implementing unsafeCoerce]
in base:Unsafe.Coerce.

Moreover, if we /don't/ inline it, we may be left with
          f (unsafeCoerce x)
which will build a thunk -- bad, bad, bad.

Conclusion: we really want inlineBoringOk to be True of the RHS of
unsafeCoerce. And it really is, because we regard
  case unsafeEqualityProof @a @b of UnsafeRefl -> rhs
as trivial iff rhs is. This is (U4) in Note [Implementing unsafeCoerce].

Note [Do not inline top-level bottoming functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The FloatOut pass has gone to some trouble to float out calls to 'error'
and similar friends.  See Note [Bottoming floats] in GHC.Core.Opt.SetLevels.
Do not re-inline them!  But we *do* still inline if they are very small
(the uncondInline stuff).

Note [INLINE for small functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider        {-# INLINE f #-}
                f x = Just x
                g y = f y
Then f's RHS is no larger than its LHS, so we should inline it into
even the most boring context.  In general, f the function is
sufficiently small that its body is as small as the call itself, the
inline unconditionally, regardless of how boring the context is.

Things to note:

(1) We inline *unconditionally* if inlined thing is smaller (using exprSizeTree)
    than the thing it's replacing.  Notice that
      (f x) --> (g 3)             -- YES, unconditionally
      (f x) --> x : []            -- YES, *even though* there are two
                                  --      arguments to the cons
      x     --> g 3               -- NO
      x     --> Just v            -- NO

    It's very important not to unconditionally replace a variable by
    a non-atomic term.

(2) We do this even if the thing isn't saturated, else we end up with the
    silly situation that
       f x y = x
       ...map (f 3)...
    doesn't inline.  Even in a boring context, inlining without being
    saturated will give a lambda instead of a PAP, and will be more
    efficient at runtime.

(3) However, when the function's arity > 0, we do insist that it
    has at least one value argument at the call site.  (This check is
    made in the UnfWhen case of callSiteInline.) Otherwise we find this:
         f = /\a \x:a. x
         d = /\b. MkD (f b)
    If we inline f here we get
         d = /\b. MkD (\x:b. x)
    and then prepareRhs floats out the argument, abstracting the type
    variables, so we end up with the original again!

(4) We must be much more cautious about arity-zero things. Consider
       let x = y +# z in ...
    In *size* terms primops look very small, because the generate a
    single instruction, but we do not want to unconditionally replace
    every occurrence of x with (y +# z).  So we only do the
    unconditional-inline thing for *trivial* expressions.

    NB: you might think that PostInlineUnconditionally would do this
    but it doesn't fire for top-level things; see GHC.Core.Opt.Simplify.Utils
    Note [Top level and postInlineUnconditionally]
-}


{- *********************************************************************
*                                                                      *
            From CoreExpr to ExprTree
     This is used when we make the unfolding for a function
*                                                                      *
********************************************************************* -}

{- Note [Constructing an ExprTree]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Given a function      f = \x1...xn. body
in the typical case we will record an UnfoldingGuidance of

   UnfoldIfGoodArgs { ug_args = filter isValArg [x1,..,xn]
                    , ug_tree = mkExprTree body }

The ug_tree :: ExprTree is an abstaction or "digest" of the body
of the function.  An ExprTree has

  * A CaseOf for each case-expression that scrutinises an argument or
    free variable, with a branch for each alternative.

  * A ScrutOf for each other interesting use a variable, giving a discount
    to apply if that argument has structure. e.g. a function that is applied.

How mkExprTree works
------------------
ETVars maintains a partition of in-scope variables into avs, lvs and fvs,
defined as follows:

* avs: argument variables, or variables bound by a case on an
       argument variable.

  We record a CaseOf or ScrutOf for the `avs`

* lvs: variables bound by lambda and lets in the body; and by
       case expressions that scrutinise one of the `lvs`, or
       a non-variable.

  We never record a CaseOf or ScrutOf for one of the `lvs`.

* We record a CaseOf, but not ScrutOf, for other variables; that is,
  variables free in the entire function definition.  For example:
        let  f x = case y of
                     A -> True
                     B -> <big>
        in
        case y of
          A -> ....f 3....f 4....
          B -> blah
  At the calls site of `f` we know that the free var `y` is equal to A, so
  f should definitely inline.

  But consider instead this example
        let f x = y x 3 <big>
        in  ...(f 3)...
  There nothing we will learn about the free `y` that will make the inining of
  `f` more attractive.  Hence we don't record ScrutOf for y.

  This is IMPORTANT, because even a call like (reverse xs) would otherwise record
  a ScrutOf for `reverse` which is very silly.

Wrinkles:

(ET1) We must be careful about recording enormous functions, with very wide or very
  deep case trees. (This can happen with Generics; e.g. test T5642.)  We limit
  both with UnfoldingOpts: see the use of `exprTreeCaseWidth` and `exprTreeCaseDepth`
  in `mkExprTree`.
  * When we exceed the maximum case-depth we just record ScrutOf info (instead of
    CaseOf) for the scrutinee.

  * When we exceed the maximum case width we just bale out entirely and say that
    the function is too big. See Note [Bale out on very wide case expressions].

Note [Computing the size of an expression]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The basic idea of exprSizeTree is obvious enough: count nodes.  But getting the
heuristics right has taken a long time.  Here's the basic strategy:

    * Variables, literals: 0
      (Exception for string literals, see litSize.)

    * Function applications (f e1 .. en): 10 + #value args

    * Constructor applications: 10, regardless of #args

    * Let(rec): 10 + size of components

    * Primops: size 1.  They look fantastically cheap. Done partly
      as a result of #4978.

    * Note, Cast: 0

Examples

  Size  Term
  --------------
     0     42#
     0     x
     0     True
    20     f x
    10     Just x
    40     f (g x)

Notice that 'x' counts 0, while (f x) counts 20.  That's deliberate: there's
a function call to account for.  Notice also that constructor applications
are very cheap, because exposing them to a caller is so valuable.
-}

type ETVars = (VarSet,VarSet)  -- (avs, lvs)
              -- See Note [Constructing an ExprTree]
              -- These sets include type variables, which do no harm,
              -- and are a bit tiresome to filter out.

mkExprTree :: UnfoldingOpts -> [Var] -> CoreExpr -> Maybe ExprTree
-- Nothing => too big
-- See Note [Overview of inlining heuristics]
-- See Note [Computing the size of an expression]

mkExprTree opts args expr
  = go (exprTreeCaseDepth opts) (mkVarSet args, emptyVarSet) expr
  where
    !max_width     = exprTreeCaseWidth opts
    !bOMB_OUT_SIZE = unfoldingCreationThreshold opts
       -- Bomb out if size gets bigger than this
       -- Forcing bOMB_OUT_SIZE early prevents repeated
       -- unboxing of the Int argument.

    met_add     = metAdd bOMB_OUT_SIZE
    met_add_alt = metAddAlt bOMB_OUT_SIZE

    go :: Int -> ETVars -> CoreExpr -> Maybe ExprTree
          -- rcd is the /remaining/ case depth; decreases toward zero
          -- (avs,lvs): see Note [Constructing an ExprTree]
    go rcd vs (Cast e _)      = go rcd vs e
    go rcd vs (Tick _ e)      = go rcd vs e
    go _   _  (Type _)        = Just (exprTreeS 0)
    go _   _  (Coercion _)    = Just (exprTreeS 0)
    go _   _  (Lit lit)       = Just (exprTreeS (litSize lit))
    go rcd vs (Case e b _ as) = go_case rcd vs e b as
    go rcd vs (Let bind body) = go_let rcd vs bind body
    go rcd vs (Lam b e)       = go_lam rcd vs b e
    go rcd vs e@(App {})      = go_app rcd vs e []
    go _   vs (Var f)         = Just (callTree opts vs f [])
                                -- Same as (go_app rcd vs (Var f) [] 0)
                                -- Use callTree to ensure we get constructor
                                -- discounts even on nullary constructors

    ----------- Lambdas ------------------
    go_lam rcd vs bndr body
      | isId bndr, not (isZeroBitId bndr) = go rcd vs' body `met_add` Just (lamSize opts)
      | otherwise                         = go rcd vs' body
      where
        vs' = vs `add_lv` bndr

    ----------- Applications ------------------
    go_app :: Int -> ETVars -> CoreExpr -> [CoreExpr] -> Maybe ExprTree
      -- args:  all the value args
      -- voids: counts the zero-bit arguments; don't charge for these
      --        This makes a difference in ST-heavy code which does a lot
      --        of state passing, and which can be in an inner loop.
    go_app rcd vs (Tick _ expr) args = go_app rcd vs expr args
    go_app rcd vs (Cast expr _) args = go_app rcd vs expr args
    go_app rcd vs (App fun arg) args
      | isTypeArg arg     = go_app rcd vs fun args
      | otherwise         = go rcd vs arg `met_add`
                            go_app rcd vs fun (arg:args)
    go_app _   vs (Var fun) args = Just (callTree opts vs fun args)
    go_app rcd vs other     args = vanillaCallSize args `metAddS` go rcd vs other
    -- If the lhs is not an App or a Var, or an invisible thing like a
    -- Tick or Cast, then we should charge for a complete call plus the
    -- size of the lhs itself.

    ----------- Let-expressions ------------------
    go_let rcd vs (NonRec binder rhs) body
      = go_bind rcd vs (binder, rhs)  `met_add`
        go rcd (vs `add_lv` binder) body

    go_let rcd vs (Rec pairs) body
      = foldr (met_add . go_bind rcd vs') (go rcd vs' body) pairs
      where
        vs' = vs `add_lvs` map fst pairs

    go_bind rcd vs (bndr, rhs)
      | isTyVar bndr
      = Just (exprTreeS 0)

      | JoinPoint join_arity <- idJoinPointHood bndr
      , (bndrs, body) <- collectNBinders join_arity rhs
                          -- Skip arguments to join point
      = go rcd (vs `add_lvs` bndrs) body

      | isUnliftedType (idType bndr) -- Doesn't live in heap
      = go rcd vs rhs

      | otherwise
      = closureSize `metAddS` go rcd vs rhs

    -----------Case expressions ------------------
    go_case :: Int -> ETVars -> CoreExpr -> Id -> [CoreAlt] -> Maybe ExprTree
    -- Empty case
    go_case rcd vs scrut _ [] = go rcd vs scrut
         -- case e of {} never returns, so take size of scrutinee

    -- Record a CaseOf
    go_case remaining_case_depth vs scrut b alts
      | alts `lengthExceeds` max_width
      = Nothing   -- See Note [Bale out on very wide case expressions]

      | Just scrut_id <- interestingVarScrut vs scrut
      = if   remaining_case_depth > 0
        then do { alts' <- mapM (alt_alt_tree scrut_id) alts
                ; etCaseOf bOMB_OUT_SIZE scrut_id b alts' }
        else Just (etScrutOf scrut_id caseElimDiscount) `met_add`
              -- When this scrutinee has structure, we expect to eliminate the case
             go_alts remaining_case_depth vs b alts
      where
        -- Decremement remaining case depth when going inside
        -- a case with more than one alternative.
        -- Don't do so for single-alt cases, becuase they don't give rise
        -- to exponential blow-up, and it's very common to have deep nests
        -- of case x of (a,b) -> case a of I# a' -> ...
        rcd1 | isSingleton alts = remaining_case_depth
             | otherwise        = remaining_case_depth - 1

        alt_alt_tree :: Id -> Alt Var -> Maybe AltTree
        alt_alt_tree scrut_id (Alt con bs rhs)
          = do { let val_bs = filter isId bs  -- The AltTree only has value binders
               ; rhs <- go rcd1 (add_alt_lvs vs scrut_id (b:val_bs)) rhs
                        -- (b:bs) don't forget to include the case binder
               ; return (AltTree con val_bs rhs) }

    -- Don't record a CaseOf
    go_case rcd vs scrut b alts    -- alts is non-empty
      = -- caseDiscount scrut alts  `metAddS`   -- It is a bit odd that this `caseDiscount` business is only
        --                                  -- applied in this equation, not in the previous ones
        go rcd vs scrut      `met_add`
        go_alts (rcd-1) vs b alts

    go_alts :: Int -> ETVars -> Id -> [CoreAlt] -> Maybe ExprTree
    -- Add up the sizes of all RHSs.  Only used for ScrutOf.
    -- IMPORTANT: include a charge for the case itself, else we
    -- find that giant case nests are treated as practically free
    -- A good example is Foreign.C.Error.errnoToIOError
    go_alts rcd vs case_bndr alts
      = caseSize case_bndr alts `metAddS`
        foldr1 met_add_alt (map alt_expr_tree alts)
      where
        alt_expr_tree :: Alt Var -> Maybe ExprTree
        alt_expr_tree (Alt _con bs rhs) = go rcd (vs `add_lvs` (case_bndr : bs)) rhs
            -- Don't charge for bndrs, so that wrappers look cheap
            -- (See comments about wrappers with Case)
            -- Don't forget to add the case binder, b, to lvs.

add_lv :: ETVars -> Var -> ETVars
add_lv (avs,lvs) b = (avs, lvs `extendVarSet` b)

add_lvs :: ETVars -> [Var] -> ETVars
add_lvs (avs,lvs) bs = (avs, lvs `extendVarSetList` bs)

add_alt_lvs :: ETVars
            -> Var       -- Scrutinised variable
            -> [Var]     -- Binders of the alterantive (including the case binder)
            -> ETVars
add_alt_lvs vs@(avs, lvs) scrut_id alt_bndrs
  | scrut_id `elemVarSet` avs = (avs `extendVarSetList` alt_bndrs, lvs)
  | otherwise                 = vs

interestingVarScrut :: ETVars -> CoreExpr -> Maybe Id
-- The scrutinee of a case is worth recording
interestingVarScrut (_,lvs) (Var v)
     | v `elemVarSet` lvs  = Nothing
     | otherwise           = Just v
interestingVarScrut vs (Tick _ e) = interestingVarScrut vs e
interestingVarScrut vs (Cast e _) = interestingVarScrut vs e
interestingVarScrut _     _       = Nothing

isZeroBitArg :: CoreExpr -> Bool
-- We could take ticks and casts into account, but it makes little
-- difference, and avoiding a recursive function here is good.
isZeroBitArg (Var id) = isZeroBitId id
isZeroBitArg _        = False

isZeroBitId :: Id -> Bool
-- Don't count expressions such as State# RealWorld
isZeroBitId id = assertPpr (not (isJoinId id)) (ppr id) $
                   -- Exclude join points, because they can be rep-polymorphic
                   -- and typePrimRep will crash
                 isZeroBitTy (idType id)


-- | Finds a nominal size of a string literal.
litSize :: Literal -> Size
-- Used by GHC.Core.Unfold.mkExprTree
litSize (LitNumber LitNumBigNat _)  = 100
litSize (LitString str) = 10 + 10 * ((BS.length str + 3) `div` 4)
        -- If size could be 0 then @f "x"@ might be too small
        -- [Sept03: make literal strings a bit bigger to avoid fruitless
        --  duplication of little strings]
litSize _other = 0    -- Must match size of nullary constructors
                      -- Key point: if  x |-> 4, then x must inline unconditionally
                      --            (eg via case binding)

----------------------------
callTree :: UnfoldingOpts -> ETVars -> Id -> [CoreExpr] -> ExprTree
callTree opts vs fun val_args
  = case idDetails fun of
      FCallId _        -> exprTreeS (vanillaCallSize val_args)
      JoinId {}        -> exprTreeS (jumpSize        val_args)
      PrimOpId op _    -> exprTreeS (primOpSize op   val_args)
      DataConWorkId dc -> conAppET dc val_args
      ClassOpId cls _  -> classOpAppET opts cls vs fun val_args
      _                -> genAppET opts vs fun val_args

-- | The size of a function call
vanillaCallSize :: [CoreExpr] -> Size
vanillaCallSize val_args = foldl' arg_sz 2 val_args
  where
    arg_sz n arg
      | isZeroBitArg arg  = n
      | exprIsTrivial arg = n+2
      | otherwise         = n+closureSize
-- 10 * (1 + n_val_args - voids)
        -- The 1+ is for the function itself
        -- Add 1 for each non-trivial value arg

-- | The size of a jump to a join point
jumpSize :: [CoreExpr] -> Size
jumpSize val_args = vanillaCallSize val_args
  -- Old version
  --    2 * (1 + n_val_args - voids)
  -- A jump isn't so much smaller than a function call, but it's definitely
  --   a known, exactly saturated call, so we make it very cheap
  -- A jump is 20% the size of a function call. Making jumps free reopens
  -- bug #6048, but making them any more expensive loses a 21% improvement in
  -- spectral/puzzle. TODO Perhaps adjusting the default threshold would be a
  -- better solution?

classOpAppET :: UnfoldingOpts -> Class -> ETVars -> Id -> [CoreExpr] -> ExprTree
classOpAppET opts vs fn val_args
  | [] <- val_args
  = etZero

  | isUnaryClass cls
  = etZero   -- See (UCM4) in Note [Unary class magic] in GHC.Core.TyCon

  | arg1 : _ <- val_args
  , Just dict <- interestingVarScrut vs arg1
  = warnPprTrace (not (isId dict)) "classOpAppET" (ppr fn <+> ppr val_args) $
    vanillaCallSize val_args `etAddS`
    etScrutOf dict (unfoldingDictDiscount opts)
           -- If the class op is scrutinising a lambda bound dictionary then
           -- give it a discount, to encourage the inlining of this function
           -- The actual discount is rather arbitrarily chosen

  | otherwise
  = exprTreeS (vanillaCallSize val_args)

genAppET :: UnfoldingOpts -> ETVars -> Id -> [CoreExpr] -> ExprTree
-- Size for function calls that are not constructors or primops
-- Note [Function applications]
genAppET opts (avs,_) fun val_args
  | fun `hasKey` buildIdKey   = etZero  -- We want to inline applications of build/augment
  | fun `hasKey` augmentIdKey = etZero  -- so we give size zero to the whole call
  | otherwise = ExprTree { et_wc_tot = size, et_size  = size
                         , et_cases = cases
                         , et_ret   = res_discount }
  where
    size | null val_args = 0    -- Naked variable counts zero
         | otherwise     = vanillaCallSize val_args

    -- Discount if this is an interesting variable, and is applied
    -- If the function is an argument and is applied to some values,
    -- give it a discount -- maybe we can apply that lambda.
    --  See Note [Function and non-function discounts]
    cases | (_:_) <- val_args, fun `elemVarSet` avs
          = unitBag (ScrutOf fun (unfoldingFunAppDiscount opts))
          | otherwise
          = emptyBag

    res_discount | val_args `lengthLessThan` idArity fun = unfoldingFunAppDiscount opts
                 | otherwise                             = 0
        -- If the function is partially applied, show a result discount

lamSize :: UnfoldingOpts -> ExprTree
-- Does not include the size of the body, just the lambda itself
lamSize _ = etZero  -- Lambdas themselves cost nothing

conAppET :: DataCon -> [CoreExpr] -> ExprTree
-- Does not need to include the size of the arguments themselves
conAppET _dc _n_val_args = etZero
{-
  | isUnboxedTupleDataCon dc
  = etZero     -- See Note [Unboxed tuple size and result discount]

  | isUnaryClassDataCon dc
  = etZero

  | n_val_args == 0    -- Like variables
  = etZero

  | otherwise  -- See Note [Constructor size and result discount]
  = ExprTree { et_size = 10, et_wc_tot = 10
             , et_cases = emptyBag, et_ret = 10 }
-}

primOpSize :: PrimOp -> [CoreExpr] -> Size
primOpSize op val_args
  | primOpOutOfLine op = op_size + length val_args
  | otherwise          = op_size
 where
   op_size = primOpCodeSize op

closureSize :: Size  -- Size for a heap-allocated closure
closureSize = 15

caseSize :: Id -> [alt] -> Size
-- For a case expression we charge for charge for each alternative.
-- (This does /not/ include the cost of the alternatives themselves)
-- If there are no alternatives (case e of {}), we get zero
--
-- Unlifted cases are much, much cheaper becuase they don't need to
-- save live variables, push a return address create an info table
-- An unlifted case is just a conditional; and if there is only one
-- alternative, it's not even a conditional, hence size zero
caseSize scrut_id alts
  | isUnliftedType (idType scrut_id)
  = if isSingleton alts then 0
                        else 5 * length alts
  | otherwise
  = 10 * length alts

caseElimDiscount :: Discount
-- Bonus for eliminating a case
caseElimDiscount = 10

{- Note [Bale out on very wide case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With very wide case trees (say N) we get a size N*altSize, which usually
prevents inlining (e.g. 20*altSize = 200 currently, which is way above the
inlining thresold of 90-ish).  Plus, we risk getting big CaseOf trees in the
ExprTree.

If we aren't going to inline it anyway, then retaining the unfolding in an
interface file is plain silly; T5642 (involving Generics) is a good example.
We had a very wide case whose branches mentioned dozens of data structures,
each of which had very large types.

Sebastian wonders about the effect of this choice on #11068.

For example, if we apply such a function to a data constructor, we could in
principle get a huge discount (because all but one branches fall away).
Something a bit like this happens in nofib/real/cacheprof, in module Main:
    instance PP Reg where
       pp ppm ST_0 = "%st"
       ... other special cases ...
       pp ppm r    = "%" ++ map toLower (show r)
If we inline that call (show r), itself a 32-wide case,  we get a lot of CAFs
which can be floated out.  Results in a 4% allocation change.

So perhaps we could use a different setting
* when generating an unfolding /within a module/
* when generating an unfolding /for an interface file/

Currently we aren't doing this, but we could consider it

See (ET1) in Note [Constructing an ExprTree].

Note [Constructor size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Treat a constructors application as size 10, regardless of how many
arguments it has; we are keen to expose them (and we charge separately
for their args).  We can't treat them as size zero, else we find that
(Just x) has size 0, which is the same as a lone variable; and hence
'v' will always be replaced by (Just x), where v is bound to Just x.

The "result discount" is applied if the result of the call is
scrutinised (say by a case).  For a constructor application that will
mean the constructor application will disappear, so we don't need to
charge it to the function.  So the discount should at least match the
cost of the constructor application, namely 10.

Historical note 1: Until Jun 2020 we gave it a "bit of extra
incentive" via a discount of 10*(1 + n_val_args), but that was FAR too
much (#18282).  In particular, consider a huge case tree like

   let r = case y1 of
          Nothing -> B1 a b c
          Just v1 -> case y2 of
                      Nothing -> B1 c b a
                      Just v2 -> ...

If conAppET gives a cost of 10 (regardless of n_val_args) and a
discount of 10, that'll make each alternative RHS cost zero.  We
charge 10 for each case alternative (see size_up_alt).  If we give a
bigger discount (say 20) in conAppET, we'll make the case expression
cost *nothing*, and that can make a huge case tree cost nothing. This
leads to massive, sometimes exponential inlinings (#18282).  In short,
don't give a discount that give a negative size to a sub-expression!

Historical note 2: Much longer ago, Simon M tried a MUCH bigger
discount: (10 * (10 + n_val_args)), and said it was an "unambiguous
win", but its terribly dangerous because a function with many many
case branches, each finishing with a constructor, can have an
arbitrarily large discount.  This led to terrible code bloat: see #6099.

Note [Unboxed tuple size and result discount]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
However, unboxed tuples count as size zero. I found occasions where we had
        f x y z = case op# x y z of { s -> (# s, () #) }
and f wasn't getting inlined.

I tried giving unboxed tuples a *result discount* of zero (see the
commented-out line).  Why?  When returned as a result they do not
allocate, so maybe we don't want to charge so much for them. If you
have a non-zero discount here, we find that workers often get inlined
back into wrappers, because it look like
    f x = case $wf x of (# a,b #) -> (a,b)
and we are keener because of the case.  However while this change
shrank binary sizes by 0.5% it also made spectral/boyer allocate 5%
more. All other changes were very small. So it's not a big deal but I
didn't adopt the idea.

When fixing #18282 (see Note [Constructor size and result discount])
I changed the result discount to be just 10, not 10*(1+n_val_args).

Note [Function and non-function discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want a discount if the function is applied. A good example is
monadic combinators with continuation arguments, where inlining is
quite important.

But we don't want a big discount when a function is called many times
(see the detailed comments with #6048) because if the function is
big it won't be inlined at its many call sites and no benefit results.
Indeed, we can get exponentially big inlinings this way; that is what
#6048 is about.

On the other hand, for data-valued arguments, if there are lots of
case expressions in the body, each one will get smaller if we apply
the function to a constructor application, so we *want* a big discount
if the argument is scrutinised by many case expressions.

Conclusion:
  - For functions, take the max of the discounts
  - For data values, take the sum of the discounts


Note [Literal integer size]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Literal integers *can* be big (mkInteger [...coefficients...]), but
need not be (IS n).  We just use an arbitrary big-ish constant here
so that, in particular, we don't inline top-level defns like
   n = IS 5
There's no point in doing so -- any optimisations will see the IS
through n's unfolding.  Nor will a big size inhibit unfoldings functions
that mention a literal Integer, because the float-out pass will float
all those constants to top level.

Note [etAddAlt result discounts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding the size of alternatives, we *add* the result discounts
too, rather than take the *maximum*.  For a multi-branch case, this
gives a discount for each branch that returns a constructor, making us
keener to inline.  I did try using 'max' instead, but it makes nofib
'rewrite' and 'puzzle' allocate significantly more, and didn't make
binary sizes shrink significantly either.

Note [Discounts and thresholds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constants for discounts and thresholds are defined in 'UnfoldingOpts'. They are:

unfoldingCreationThreshold
     At a definition site, if the unfolding is bigger than this, we
     may discard it altogether

unfoldingUseThreshold
     At a call site, if the unfolding, less discounts, is smaller than
     this, then it's small enough inline

unfoldingDictDiscount
     The discount for each occurrence of a dictionary argument
     as an argument of a class method.  Should be pretty small
     else big functions may get inlined

unfoldingFunAppDiscount
     Discount for a function argument that is applied.  Quite
     large, because if we inline we avoid the higher-order call.

unfoldingVeryAggressive
     If True, the compiler ignores all the thresholds and inlines very
     aggressively. It still adheres to arity, simplifier phase control and
     loop breakers.


Historical Note: Before April 2020 we had another factor,
ufKeenessFactor, which would scale the discounts before they were subtracted
from the size. This was justified with the following comment:

  -- We multiply the raw discounts (args_discount and result_discount)
  -- ty opt_UnfoldingKeenessFactor because the former have to do with
  --  *size* whereas the discounts imply that there's some extra
  --  *efficiency* to be gained (e.g. beta reductions, case reductions)
  -- by inlining.

However, this is highly suspect since it means that we subtract a *scaled* size
from an absolute size, resulting in crazy (e.g. negative) scores in some cases
(#15304). We consequently killed off ufKeenessFactor and bumped up the
ufUseThreshold to compensate.


Note [Function applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a function application (f a b)

  - If 'f' is an argument to the function being analysed,
    and there's at least one value arg, record a FunAppDiscount for f

  - If the application if a PAP (arity > 2 in this example)
    record a *result* discount (because inlining
    with "extra" args in the call may mean that we now
    get a saturated application)

Code for manipulating sizes
-}

---------------------------------------
-- Right associative; predence level unimportant
infixr 5 `metAddS`, `etAddS`, `metAdd`, `metAddAlt`

-- Nomenclature:
-- * The "met" is for "Maybe ExprTree"
-- * The "S" is for Size

metAddS :: Size -> Maybe ExprTree -> Maybe ExprTree
metAddS n met = fmap (etAddS n) met

etAddS :: Size -> ExprTree -> ExprTree
-- Does not account for et_wc_tot geting too big, but that doesn't
-- matter; the extra increment is always small, and we never get
-- a long cascade of etAddSs
etAddS n1 (ExprTree { et_wc_tot = t2, et_size = n2, et_cases = c2, et_ret = ret2 })
  = ExprTree { et_wc_tot = n1+t2, et_size = n1+n2, et_cases = c2, et_ret = ret2 }

---------------------------------------------------
-- metAdd vs metAddAlt are identical except
-- * metAdd    takesthe return discount from the second argument
-- * metAddAlt adds the return discounts

metAdd :: Size -> Maybe ExprTree -> Maybe ExprTree -> Maybe ExprTree
metAdd _ Nothing _ = Nothing
metAdd _ _ Nothing = Nothing
metAdd bOMB_OUT_SIZE (Just et1) (Just et2)
  | ExprTree { et_wc_tot = t1, et_size = n1, et_cases = c1, et_ret = _ret1 } <- et1
  , ExprTree { et_wc_tot = t2, et_size = n2, et_cases = c2, et_ret =  ret2 } <- et2
  , let t12 = t1 + t2
  = if   t12 >= bOMB_OUT_SIZE
    then Nothing
    else Just (ExprTree { et_wc_tot = t12
                        , et_size   = n1 + n2
                        , et_cases  = c1 `unionBags` c2
                        , et_ret    = ret2 })

metAddAlt :: Size -> Maybe ExprTree -> Maybe ExprTree -> Maybe ExprTree
-- Adds return discounts from both args
metAddAlt _ Nothing _ = Nothing
metAddAlt _ _ Nothing = Nothing
metAddAlt bOMB_OUT_SIZE (Just et1) (Just et2)
  | ExprTree { et_wc_tot = t1, et_size = n1, et_cases = c1, et_ret = ret1 } <- et1
  , ExprTree { et_wc_tot = t2, et_size = n2, et_cases = c2, et_ret = ret2 } <- et2
  , let t12 = t1 + t2
  = if   t12 >= bOMB_OUT_SIZE
    then Nothing
    else Just (ExprTree { et_wc_tot = t12
                        , et_size   = n1 + n2
                        , et_cases  = c1 `unionBags` c2
                        , et_ret    = ret1 + ret2 -- See Note [Result discount for case alternatives]
          })


---------------------------------------------------
-- | The "expression tree"; an abstraction of the RHS of the function
--   The "S" signals the Size argument
exprTreeS :: Size -> ExprTree
exprTreeS n = ExprTree { et_size = n, et_wc_tot = n, et_cases = emptyBag, et_ret = 0 }

etZero :: ExprTree
etZero = exprTreeS 0

etCaseOf :: Size -> Id -> Id -> [AltTree] -> Maybe ExprTree
-- We make the case itself free (remember that in this case the scrutinee
-- is a variable) but charge for each alternative (included in `altTreesSize`)
etCaseOf bOMB_OUT_SIZE scrut case_bndr alts
  | tot >= bOMB_OUT_SIZE = Nothing
  | otherwise            = Just (ExprTree { et_wc_tot = tot, et_ret = ret
                                          , et_size = 0
                                          , et_cases = unitBag case_tree })
  where
    case_tree = CaseOf scrut case_bndr alts
    tot       = altTreesSize scrut alts
    ret       = altTreesDiscount alts

altTreesSize :: Id -> [AltTree] -> Size
-- Total worst-case size of a [AltTree], including the per-alternative cost of altSize
altTreesSize scrut_id alts
  = foldl' add_alt (caseSize scrut_id alts) alts
  where
    add_alt :: Size -> AltTree -> Size
    add_alt sz (AltTree _ _ (ExprTree { et_wc_tot = alt_tot })) = sz + alt_tot

altTreesDiscount :: [AltTree] -> Discount
-- See Note [Result discount for case alternatives]
altTreesDiscount alts = foldl' add_alt 0 alts
  where
    add_alt n (AltTree _ _ (ExprTree { et_ret = ret })) = n + ret

etScrutOf :: Id -> Discount -> ExprTree
etScrutOf v d = etZero { et_cases = unitBag (ScrutOf v d) }

{- Note [Result discount for case alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When adding the size of alternatives, we *add* the result discounts
too, rather than take the *maximum*.  For a multi-branch case, this
gives a discount for each branch that returns a constructor, making us
keener to inline.  I did try using 'max' instead, but it makes nofib
'rewrite' and 'puzzle' allocate significantly more, and didn't make
binary sizes shrink significantly either.
-}

{- *********************************************************************
*                                                                      *
            From ExprTree to Size
     This is used when we have an acutal call site
*                                                                      *
********************************************************************* -}

data InlineContext
   = IC { ic_free  :: Id -> ArgDigest  -- Current unfoldings for free variables
        , ic_bound :: IdEnv ArgDigest  -- Digests for local variables
     }

data ArgDigest
  = ArgNoInfo
  | ArgIsCon AltCon [ArgDigest]   -- Arg is a data-con application;
                                  --   the [ArgDigest] is for value args only
  | ArgIsNot [AltCon]             -- Arg is a value, not headed by these construtors
  | ArgIsLam                      -- Arg is a lambda
  | ArgNonTriv                    -- The arg has some struture, but is not a value
                                  --   e.g. it might be a call (f x)

hasArgInfo :: ArgDigest -> Bool
hasArgInfo ArgNoInfo = False
hasArgInfo _         = True

instance Outputable ArgDigest where
  ppr ArgNoInfo       = text "ArgNoInfo"
  ppr ArgIsLam        = text "ArgIsLam"
  ppr ArgNonTriv      = text "ArgNonTriv"
  ppr (ArgIsCon c as) = ppr c <> ppr as
  ppr (ArgIsNot cs)   = text "ArgIsNot" <> ppr cs

-------------------------
exprTreeWillInline :: Size -> ExprTree -> Bool
-- (cheapExprTreeSize limit et) takes an upper bound `n` on the
-- size of et; i.e. without discounts etc.
-- Return True if (s <= limit), False otherwise
exprTreeWillInline limit (ExprTree { et_wc_tot = tot }) = tot <= limit

-------------------------
exprTreeSize :: InlineContext -> ExprTree -> Size
-- See Note [Overview of inlining heuristics]
exprTreeSize !ic (ExprTree { et_size  = size, et_cases = cases })
  = foldr ((+) . caseTreeSize ic) size cases

caseTreeSize :: InlineContext -> CaseTree -> Size
caseTreeSize ic (ScrutOf bndr disc)
  = case lookupBndr ic bndr of
      ArgNoInfo   -> 0
      ArgNonTriv  -> -10    -- E.g. bndr is a DFun application
                            --      T8732 need to inline mapM_

      -- Apply full discount for values
      ArgIsLam    -> -disc  -- Apply discount
      ArgIsNot {} -> -disc
      ArgIsCon {} -> -disc  -- Apply discount

caseTreeSize ic (CaseOf scrut_var case_bndr alts)
  = case lookupBndr ic scrut_var of
      ArgNoInfo  -> caseAltsSize ic case_bndr alts + case_size
      ArgNonTriv -> caseAltsSize ic case_bndr alts + case_size

      ArgIsNot cons -> caseAltsSize ic case_bndr (trim_alts cons alts)
         -- The case-expression may not disappear, but it scrutinises
         -- a variable bound to something with structure; may lead to
         -- avoiding a thunk, or other benefits.  So we give a discount
         -- compared to ArgNoInfo.  How much?  Rather a guess, but simply
         -- not adding case_size is convenient.
         --
         -- The function 'radiance' in nofib/real/smallpt benefits a lot from this

      ArgIsLam -> caseAltsSize ic case_bndr alts  -- Case will disappear altogether

      arg_digest@(ArgIsCon con args)
         | Just at@(AltTree alt_con bndrs rhs) <- find_alt con alts
         , let new_digests :: [(Id,ArgDigest)]
               new_digests = (case_bndr,arg_digest) : bndrs `zip` args
                  -- Don't forget to add a digest for the case binder!
               ic' = ic { ic_bound = ic_bound ic `extendVarEnvList` new_digests }
                     -- In DEFAULT case, bs is empty, so extending is a no-op
         -> assertPpr ((alt_con == DEFAULT) || (bndrs `equalLength` args))
                      (ppr arg_digest $$ ppr at) $
            exprTreeSize ic' rhs - caseElimDiscount
              -- Take off an extra discount for eliminating the case expression itself

         | otherwise  -- Happens for empty alternatives
         -> caseAltsSize ic case_bndr alts
  where
    case_size = caseSize scrut_var alts

find_alt :: AltCon -> [AltTree] -> Maybe AltTree
find_alt _   []                     = Nothing
find_alt con (alt:alts)
   | AltTree DEFAULT _ _ <- alt = go alts       (Just alt)
   | otherwise                  = go (alt:alts) Nothing
   where
     go []         deflt              = deflt
     go (alt:alts) deflt
       | AltTree con' _ _ <- alt, con==con' = Just alt
       | otherwise                          = go alts deflt

trim_alts :: [AltCon] -> [AltTree] -> [AltTree]
trim_alts cons alts
  | null cons = alts
  | otherwise = go alts
  where
    go [] = []
    go (alt:alts) | AltTree con _ _ <- alt, con `elem` cons = go alts
                  | otherwise                               = alt : go alts

caseAltsSize :: InlineContext -> Id -> [AltTree] -> Size
-- Size of a (retained) case expression
-- Do /not/ include the per-alternative cost, just the alternatives themselves
caseAltsSize ic case_bndr alts = foldr ((+) . size_alt) 0 alts
  -- Just add up the  sizes of the alternatives
  -- We recurse in case we have
  --    args = [a,b], expr_tree = [CaseOf a [ X -> CaseOf b [...]
  --                                        , Y -> CaseOf b [...] ] ]
  -- Then for a call with ArgInfo for `b`, but not `a`, we want to get
  -- the trimmed trees in the X and Y branches
  where
    size_alt :: AltTree -> Size
    size_alt (AltTree _ bndrs rhs) = exprTreeSize ic' rhs
        -- Cost for the alternative is already in `rhs`
      where
        -- Must extend ic_bound, lest a captured variable
        -- is looked up in ic_free by lookupBndr
        new_digests :: [(Id,ArgDigest)]
        new_digests = [(b,ArgNoInfo) | b <- case_bndr:bndrs]
        ic' = ic { ic_bound = ic_bound ic `extendVarEnvList` new_digests }

lookupBndr :: HasDebugCallStack => InlineContext -> Id -> ArgDigest
lookupBndr (IC { ic_bound = bound_env, ic_free = lookup_free }) var
  | Just info <- assertPpr (isId var) (ppr var) $
                 lookupVarEnv bound_env var = info
  | otherwise                               = lookup_free var
