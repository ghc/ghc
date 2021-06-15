{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998

\section[BasicTypes]{Miscellaneous types}

This module defines a miscellaneously collection of very simple
types that

\begin{itemize}
\item have no other obvious home
\item don't depend on any other complicated types
\item are used in more than one "part" of the compiler
\end{itemize}
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Types.Basic (
        LeftOrRight(..),
        pickLR,

        ConTag, ConTagZ, fIRST_TAG,

        Arity, RepArity, JoinArity, FullArgCount,

        Alignment, mkAlignment, alignmentOf, alignmentBytes,

        PromotionFlag(..), isPromoted,
        FunctionOrData(..),

        RecFlag(..), isRec, isNonRec, boolToRecFlag,
        Origin(..), isGenerated,

        RuleName, pprRuleName,

        TopLevelFlag(..), isTopLevel, isNotTopLevel,

        OverlapFlag(..), OverlapMode(..), setOverlapModeMaybe,
        hasOverlappingFlag, hasOverlappableFlag, hasIncoherentFlag,

        Boxity(..), isBoxed,

        PprPrec(..), topPrec, sigPrec, opPrec, funPrec, starPrec, appPrec,
        maybeParen,

        TupleSort(..), tupleSortBoxity, boxityTupleSort,
        tupleParens,

        sumParens, pprAlternative,

        -- ** The OneShotInfo type
        OneShotInfo(..),
        noOneShotInfo, hasNoOneShotInfo, isOneShotInfo,
        bestOneShot, worstOneShot,

        OccInfo(..), noOccInfo, seqOccInfo, zapFragileOcc, isOneOcc,
        isDeadOcc, isStrongLoopBreaker, isWeakLoopBreaker, isManyOccs,
        isNoOccInfo, strongLoopBreaker, weakLoopBreaker,

        InsideLam(..),
        BranchCount, oneBranch,
        InterestingCxt(..),
        TailCallInfo(..), tailCallInfo, zapOccTailCallInfo,
        isAlwaysTailCalled,

        EP(..),

        DefMethSpec(..),
        SwapFlag(..), flipSwap, unSwap, isSwapped,

        CompilerPhase(..), PhaseNum, beginPhase, nextPhase, laterPhase,

        Activation(..), isActive, competesWith,
        isNeverActive, isAlwaysActive, activeInFinalPhase,
        activateAfterInitial, activateDuringFinal, activeAfter,

        RuleMatchInfo(..), isConLike, isFunLike,
        InlineSpec(..), noUserInlineSpec,
        InlinePragma(..), defaultInlinePragma, alwaysInlinePragma,
        neverInlinePragma, dfunInlinePragma,
        isDefaultInlinePragma,
        isInlinePragma, isInlinablePragma, isNoInlinePragma,
        isAnyInlinePragma, alwaysInlineConLikePragma,
        inlinePragmaSource,
        inlinePragmaName, inlineSpecSource,
        inlinePragmaSpec, inlinePragmaSat,
        inlinePragmaActivation, inlinePragmaRuleMatchInfo,
        setInlinePragmaActivation, setInlinePragmaRuleMatchInfo,
        pprInline, pprInlineDebug,

        SuccessFlag(..), succeeded, failed, successIf,

        IntWithInf, infinity, treatZeroAsInf, subWithInf, mkIntWithInf, intGtLimit,

        SpliceExplicitFlag(..),

        TypeOrKind(..), isTypeLevel, isKindLevel,

        NonStandardDefaultingStrategy(..),
        DefaultingStrategy(..), defaultNonStandardTyVars,

        ForeignSrcLang (..)
   ) where

import GHC.Prelude

import GHC.ForeignSrcLang
import GHC.Data.FastString
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Binary
import GHC.Types.SourceText
import Data.Data
import qualified Data.Semigroup as Semi

{-
************************************************************************
*                                                                      *
          Binary choice
*                                                                      *
************************************************************************
-}

data LeftOrRight = CLeft | CRight
                 deriving( Eq, Data )

pickLR :: LeftOrRight -> (a,a) -> a
pickLR CLeft  (l,_) = l
pickLR CRight (_,r) = r

instance Outputable LeftOrRight where
  ppr CLeft    = text "Left"
  ppr CRight   = text "Right"

instance Binary LeftOrRight where
   put_ bh CLeft  = putByte bh 0
   put_ bh CRight = putByte bh 1

   get bh = do { h <- getByte bh
               ; case h of
                   0 -> return CLeft
                   _ -> return CRight }


{-
************************************************************************
*                                                                      *
\subsection[Arity]{Arity}
*                                                                      *
************************************************************************
-}

-- | The number of value arguments that can be applied to a value before it does
-- "real work". So:
--  fib 100     has arity 0
--  \x -> fib x has arity 1
-- See also Note [Definition of arity] in "GHC.Core.Opt.Arity"
type Arity = Int

-- | Representation Arity
--
-- The number of represented arguments that can be applied to a value before it does
-- "real work". So:
--  fib 100                    has representation arity 0
--  \x -> fib x                has representation arity 1
--  \(# x, y #) -> fib (x + y) has representation arity 2
type RepArity = Int

-- | The number of arguments that a join point takes. Unlike the arity of a
-- function, this is a purely syntactic property and is fixed when the join
-- point is created (or converted from a value). Both type and value arguments
-- are counted.
type JoinArity = Int

-- | FullArgCount is the number of type or value arguments in an application,
-- or the number of type or value binders in a lambda.  Note: it includes
-- both type and value arguments!
type FullArgCount = Int

{-
************************************************************************
*                                                                      *
              Constructor tags
*                                                                      *
************************************************************************
-}

-- | A *one-index* constructor tag
--
-- Type of the tags associated with each constructor possibility or superclass
-- selector
type ConTag = Int

-- | A *zero-indexed* constructor tag
type ConTagZ = Int

fIRST_TAG :: ConTag
-- ^ Tags are allocated from here for real constructors
--   or for superclass selectors
fIRST_TAG =  1

{-
************************************************************************
*                                                                      *
\subsection[Alignment]{Alignment}
*                                                                      *
************************************************************************
-}

-- | A power-of-two alignment
newtype Alignment = Alignment { alignmentBytes :: Int } deriving (Eq, Ord)

-- Builds an alignment, throws on non power of 2 input. This is not
-- ideal, but convenient for internal use and better then silently
-- passing incorrect data.
mkAlignment :: Int -> Alignment
mkAlignment n
  | n == 1 = Alignment 1
  | n == 2 = Alignment 2
  | n == 4 = Alignment 4
  | n == 8 = Alignment 8
  | n == 16 = Alignment 16
  | n == 32 = Alignment 32
  | n == 64 = Alignment 64
  | n == 128 = Alignment 128
  | n == 256 = Alignment 256
  | n == 512 = Alignment 512
  | otherwise = panic "mkAlignment: received either a non power of 2 argument or > 512"

-- Calculates an alignment of a number. x is aligned at N bytes means
-- the remainder from x / N is zero. Currently, interested in N <= 8,
-- but can be expanded to N <= 16 or N <= 32 if used within SSE or AVX
-- context.
alignmentOf :: Int -> Alignment
alignmentOf x = case x .&. 7 of
  0 -> Alignment 8
  4 -> Alignment 4
  2 -> Alignment 2
  _ -> Alignment 1

instance Outputable Alignment where
  ppr (Alignment m) = ppr m

instance OutputableP env Alignment where
  pdoc _ = ppr

{-
************************************************************************
*                                                                      *
         One-shot information
*                                                                      *
************************************************************************
-}

{-
Note [OneShotInfo overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lambda-bound Ids (and only lambda-bound Ids) may be decorated with
one-shot info.  The idea is that if we see
    (\x{one-shot}. e)
it means that this lambda will only be applied once.  In particular
that means we can float redexes under the lambda without losing
work.  For example, consider
    let t = expensive in
    (\x{one-shot}. case t of { True -> ...; False -> ... })

Because it's a one-shot lambda, we can safely inline t, giving
    (\x{one_shot}. case <expensive> of
                       { True -> ...; False -> ... })

Moving parts:

* Usage analysis, performed as part of demand-analysis, finds
  out whether functions call their argument once.  Consider
     f g x = Just (case g x of { ... })

  Here 'f' is lazy in 'g', but it guarantees to call it no
  more than once.  So g will get a C1(U) usage demand.

* Occurrence analysis propagates this usage information
  (in the demand signature of a function) to its calls.
  Example, given 'f' above
     f (\x.e) blah

  Since f's demand signature says it has a C1(U) usage demand on its
  first argument, the occurrence analyser sets the \x to be one-shot.
  This is done via the occ_one_shots field of OccEnv.

* Float-in and float-out take account of one-shot-ness

* Occurrence analysis doesn't set "inside-lam" for occurrences inside
  a one-shot lambda

Other notes

* A one-shot lambda can use its argument many times.  To elaborate
  the example above
    let t = expensive in
    (\x{one-shot}. case t of { True -> x+x; False -> x*x })

  Here the '\x' is one-shot, which justifies inlining 't',
  but x is used many times. That's absolutely fine.

* It's entirely possible to have
     (\x{one-shot}. \y{many-shot}. e)

  For example
     let t = expensive
         g = \x -> let v = x+t in
             \y -> x + v
     in map (g 5) xs

  Here the `\x` is a one-shot binder: `g` is applied to one argument
  exactly once.  And because the `\x` is one-shot, it would be fine to
  float that `let t = expensive` binding inside the `\x`.

  But the `\y` is most definitely not one-shot!
-}

-- | If the 'Id' is a lambda-bound variable then it may have lambda-bound
-- variable info. Sometimes we know whether the lambda binding this variable
-- is a "one-shot" lambda; that is, whether it is applied at most once.
--
-- This information may be useful in optimisation, as computations may
-- safely be floated inside such a lambda without risk of duplicating
-- work.
--
-- See also Note [OneShotInfo overview] above.
data OneShotInfo
  = NoOneShotInfo -- ^ No information
  | OneShotLam    -- ^ The lambda is applied at most once.
  deriving (Eq)

-- | It is always safe to assume that an 'Id' has no lambda-bound variable information
noOneShotInfo :: OneShotInfo
noOneShotInfo = NoOneShotInfo

isOneShotInfo, hasNoOneShotInfo :: OneShotInfo -> Bool
isOneShotInfo OneShotLam = True
isOneShotInfo _          = False

hasNoOneShotInfo NoOneShotInfo = True
hasNoOneShotInfo _             = False

worstOneShot, bestOneShot :: OneShotInfo -> OneShotInfo -> OneShotInfo
worstOneShot NoOneShotInfo _             = NoOneShotInfo
worstOneShot OneShotLam    os            = os

bestOneShot NoOneShotInfo os         = os
bestOneShot OneShotLam    _          = OneShotLam

pprOneShotInfo :: OneShotInfo -> SDoc
pprOneShotInfo NoOneShotInfo = text "NoOS"
pprOneShotInfo OneShotLam    = text "OneShot"

instance Outputable OneShotInfo where
    ppr = pprOneShotInfo

{-
************************************************************************
*                                                                      *
           Swap flag
*                                                                      *
************************************************************************
-}

data SwapFlag
  = NotSwapped  -- Args are: actual,   expected
  | IsSwapped   -- Args are: expected, actual

instance Outputable SwapFlag where
  ppr IsSwapped  = text "Is-swapped"
  ppr NotSwapped = text "Not-swapped"

flipSwap :: SwapFlag -> SwapFlag
flipSwap IsSwapped  = NotSwapped
flipSwap NotSwapped = IsSwapped

isSwapped :: SwapFlag -> Bool
isSwapped IsSwapped  = True
isSwapped NotSwapped = False

unSwap :: SwapFlag -> (a->a->b) -> a -> a -> b
unSwap NotSwapped f a b = f a b
unSwap IsSwapped  f a b = f b a


{- *********************************************************************
*                                                                      *
           Promotion flag
*                                                                      *
********************************************************************* -}

-- | Is a TyCon a promoted data constructor or just a normal type constructor?
data PromotionFlag
  = NotPromoted
  | IsPromoted
  deriving ( Eq, Data )

isPromoted :: PromotionFlag -> Bool
isPromoted IsPromoted  = True
isPromoted NotPromoted = False

instance Outputable PromotionFlag where
  ppr NotPromoted = text "NotPromoted"
  ppr IsPromoted  = text "IsPromoted"

instance Binary PromotionFlag where
   put_ bh NotPromoted = putByte bh 0
   put_ bh IsPromoted  = putByte bh 1

   get bh = do
       n <- getByte bh
       case n of
         0 -> return NotPromoted
         1 -> return IsPromoted
         _ -> fail "Binary(IsPromoted): fail)"

{-
************************************************************************
*                                                                      *
\subsection[FunctionOrData]{FunctionOrData}
*                                                                      *
************************************************************************
-}

data FunctionOrData = IsFunction | IsData
    deriving (Eq, Ord, Data)

instance Outputable FunctionOrData where
    ppr IsFunction = text "(function)"
    ppr IsData     = text "(data)"

instance Binary FunctionOrData where
    put_ bh IsFunction = putByte bh 0
    put_ bh IsData     = putByte bh 1
    get bh = do
        h <- getByte bh
        case h of
          0 -> return IsFunction
          1 -> return IsData
          _ -> panic "Binary FunctionOrData"

{-
************************************************************************
*                                                                      *
                Rules
*                                                                      *
************************************************************************
-}

type RuleName = FastString

pprRuleName :: RuleName -> SDoc
pprRuleName rn = doubleQuotes (ftext rn)


{-
************************************************************************
*                                                                      *
\subsection[Top-level/local]{Top-level/not-top level flag}
*                                                                      *
************************************************************************
-}

data TopLevelFlag
  = TopLevel
  | NotTopLevel
  deriving Data

isTopLevel, isNotTopLevel :: TopLevelFlag -> Bool

isNotTopLevel NotTopLevel = True
isNotTopLevel TopLevel    = False

isTopLevel TopLevel     = True
isTopLevel NotTopLevel  = False

instance Outputable TopLevelFlag where
  ppr TopLevel    = text "<TopLevel>"
  ppr NotTopLevel = text "<NotTopLevel>"

{-
************************************************************************
*                                                                      *
                Boxity flag
*                                                                      *
************************************************************************
-}

data Boxity
  = Boxed
  | Unboxed
  deriving( Eq, Data )

isBoxed :: Boxity -> Bool
isBoxed Boxed   = True
isBoxed Unboxed = False

instance Outputable Boxity where
  ppr Boxed   = text "Boxed"
  ppr Unboxed = text "Unboxed"

instance Binary Boxity where -- implemented via isBoxed-isomorphism to Bool
  put_ bh = put_ bh . isBoxed
  get bh  = do
    b <- get bh
    pure $ if b then Boxed else Unboxed

{-
************************************************************************
*                                                                      *
                Recursive/Non-Recursive flag
*                                                                      *
************************************************************************
-}

-- | Recursivity Flag
data RecFlag = Recursive
             | NonRecursive
             deriving( Eq, Data )

isRec :: RecFlag -> Bool
isRec Recursive    = True
isRec NonRecursive = False

isNonRec :: RecFlag -> Bool
isNonRec Recursive    = False
isNonRec NonRecursive = True

boolToRecFlag :: Bool -> RecFlag
boolToRecFlag True  = Recursive
boolToRecFlag False = NonRecursive

instance Outputable RecFlag where
  ppr Recursive    = text "Recursive"
  ppr NonRecursive = text "NonRecursive"

instance Binary RecFlag where
    put_ bh Recursive =
            putByte bh 0
    put_ bh NonRecursive =
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> return Recursive
              _ -> return NonRecursive

{-
************************************************************************
*                                                                      *
                Code origin
*                                                                      *
************************************************************************
-}

data Origin = FromSource
            | Generated
            deriving( Eq, Data )

isGenerated :: Origin -> Bool
isGenerated Generated = True
isGenerated FromSource = False

instance Outputable Origin where
  ppr FromSource  = text "FromSource"
  ppr Generated   = text "Generated"

{-
************************************************************************
*                                                                      *
                Instance overlap flag
*                                                                      *
************************************************************************
-}

-- | The semantics allowed for overlapping instances for a particular
-- instance. See Note [Safe Haskell isSafeOverlap] (in "GHC.Core.InstEnv") for a
-- explanation of the `isSafeOverlap` field.
--
-- - 'GHC.Parser.Annotation.AnnKeywordId' :
--      'GHC.Parser.Annotation.AnnOpen' @'\{-\# OVERLAPPABLE'@ or
--                              @'\{-\# OVERLAPPING'@ or
--                              @'\{-\# OVERLAPS'@ or
--                              @'\{-\# INCOHERENT'@,
--      'GHC.Parser.Annotation.AnnClose' @`\#-\}`@,

-- For details on above see note [exact print annotations] in "GHC.Parser.Annotation"
data OverlapFlag = OverlapFlag
  { overlapMode   :: OverlapMode
  , isSafeOverlap :: Bool
  } deriving (Eq, Data)

setOverlapModeMaybe :: OverlapFlag -> Maybe OverlapMode -> OverlapFlag
setOverlapModeMaybe f Nothing  = f
setOverlapModeMaybe f (Just m) = f { overlapMode = m }

hasIncoherentFlag :: OverlapMode -> Bool
hasIncoherentFlag mode =
  case mode of
    Incoherent   _ -> True
    _              -> False

hasOverlappableFlag :: OverlapMode -> Bool
hasOverlappableFlag mode =
  case mode of
    Overlappable _ -> True
    Overlaps     _ -> True
    Incoherent   _ -> True
    _              -> False

hasOverlappingFlag :: OverlapMode -> Bool
hasOverlappingFlag mode =
  case mode of
    Overlapping  _ -> True
    Overlaps     _ -> True
    Incoherent   _ -> True
    _              -> False

data OverlapMode  -- See Note [Rules for instance lookup] in GHC.Core.InstEnv
  = NoOverlap SourceText
                  -- See Note [Pragma source text]
    -- ^ This instance must not overlap another `NoOverlap` instance.
    -- However, it may be overlapped by `Overlapping` instances,
    -- and it may overlap `Overlappable` instances.


  | Overlappable SourceText
                  -- See Note [Pragma source text]
    -- ^ Silently ignore this instance if you find a
    -- more specific one that matches the constraint
    -- you are trying to resolve
    --
    -- Example: constraint (Foo [Int])
    --   instance                      Foo [Int]
    --   instance {-# OVERLAPPABLE #-} Foo [a]
    --
    -- Since the second instance has the Overlappable flag,
    -- the first instance will be chosen (otherwise
    -- its ambiguous which to choose)


  | Overlapping SourceText
                  -- See Note [Pragma source text]
    -- ^ Silently ignore any more general instances that may be
    --   used to solve the constraint.
    --
    -- Example: constraint (Foo [Int])
    --   instance {-# OVERLAPPING #-} Foo [Int]
    --   instance                     Foo [a]
    --
    -- Since the first instance has the Overlapping flag,
    -- the second---more general---instance will be ignored (otherwise
    -- it is ambiguous which to choose)


  | Overlaps SourceText
                  -- See Note [Pragma source text]
    -- ^ Equivalent to having both `Overlapping` and `Overlappable` flags.

  | Incoherent SourceText
                  -- See Note [Pragma source text]
    -- ^ Behave like Overlappable and Overlapping, and in addition pick
    -- an arbitrary one if there are multiple matching candidates, and
    -- don't worry about later instantiation
    --
    -- Example: constraint (Foo [b])
    -- instance {-# INCOHERENT -} Foo [Int]
    -- instance                   Foo [a]
    -- Without the Incoherent flag, we'd complain that
    -- instantiating 'b' would change which instance
    -- was chosen. See also note [Incoherent instances] in "GHC.Core.InstEnv"

  deriving (Eq, Data)


instance Outputable OverlapFlag where
   ppr flag = ppr (overlapMode flag) <+> pprSafeOverlap (isSafeOverlap flag)

instance Outputable OverlapMode where
   ppr (NoOverlap    _) = empty
   ppr (Overlappable _) = text "[overlappable]"
   ppr (Overlapping  _) = text "[overlapping]"
   ppr (Overlaps     _) = text "[overlap ok]"
   ppr (Incoherent   _) = text "[incoherent]"

instance Binary OverlapMode where
    put_ bh (NoOverlap    s) = putByte bh 0 >> put_ bh s
    put_ bh (Overlaps     s) = putByte bh 1 >> put_ bh s
    put_ bh (Incoherent   s) = putByte bh 2 >> put_ bh s
    put_ bh (Overlapping  s) = putByte bh 3 >> put_ bh s
    put_ bh (Overlappable s) = putByte bh 4 >> put_ bh s
    get bh = do
        h <- getByte bh
        case h of
            0 -> (get bh) >>= \s -> return $ NoOverlap s
            1 -> (get bh) >>= \s -> return $ Overlaps s
            2 -> (get bh) >>= \s -> return $ Incoherent s
            3 -> (get bh) >>= \s -> return $ Overlapping s
            4 -> (get bh) >>= \s -> return $ Overlappable s
            _ -> panic ("get OverlapMode" ++ show h)


instance Binary OverlapFlag where
    put_ bh flag = do put_ bh (overlapMode flag)
                      put_ bh (isSafeOverlap flag)
    get bh = do
        h <- get bh
        b <- get bh
        return OverlapFlag { overlapMode = h, isSafeOverlap = b }

pprSafeOverlap :: Bool -> SDoc
pprSafeOverlap True  = text "[safe]"
pprSafeOverlap False = empty

{-
************************************************************************
*                                                                      *
                Precedence
*                                                                      *
************************************************************************
-}

-- | A general-purpose pretty-printing precedence type.
newtype PprPrec = PprPrec Int deriving (Eq, Ord, Show)
-- See Note [Precedence in types]

topPrec, sigPrec, funPrec, opPrec, starPrec, appPrec :: PprPrec
topPrec = PprPrec 0 -- No parens
sigPrec = PprPrec 1 -- Explicit type signatures
funPrec = PprPrec 2 -- Function args; no parens for constructor apps
                    -- See [Type operator precedence] for why both
                    -- funPrec and opPrec exist.
opPrec  = PprPrec 2 -- Infix operator
starPrec = PprPrec 3 -- Star syntax for the type of types, i.e. the * in (* -> *)
                     -- See Note [Star kind precedence]
appPrec  = PprPrec 4 -- Constructor args; no parens for atomic

maybeParen :: PprPrec -> PprPrec -> SDoc -> SDoc
maybeParen ctxt_prec inner_prec pretty
  | ctxt_prec < inner_prec = pretty
  | otherwise              = parens pretty

{- Note [Precedence in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Many pretty-printing functions have type
    ppr_ty :: PprPrec -> Type -> SDoc

The PprPrec gives the binding strength of the context.  For example, in
   T ty1 ty2
we will pretty-print 'ty1' and 'ty2' with the call
  (ppr_ty appPrec ty)
to indicate that the context is that of an argument of a TyConApp.

We use this consistently for Type and HsType.

Note [Type operator precedence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't keep the fixity of type operators in the operator. So the
pretty printer follows the following precedence order:

   TyConPrec         Type constructor application
   TyOpPrec/FunPrec  Operator application and function arrow

We have funPrec and opPrec to represent the precedence of function
arrow and type operators respectively, but currently we implement
funPrec == opPrec, so that we don't distinguish the two. Reason:
it's hard to parse a type like
    a ~ b => c * d -> e - f

By treating opPrec = funPrec we end up with more parens
    (a ~ b) => (c * d) -> (e - f)

But the two are different constructors of PprPrec so we could make
(->) bind more or less tightly if we wanted.

Note [Star kind precedence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
We parenthesize the (*) kind to avoid two issues:

1. Printing invalid or incorrect code.
   For example, instead of  type F @(*) x = x
         GHC used to print  type F @*   x = x
   However, (@*) is a type operator, not a kind application.

2. Printing kinds that are correct but hard to read.
   Should  Either * Int  be read as  Either (*) Int
                              or as  (*) Either Int  ?
   This depends on whether -XStarIsType is enabled, but it would be
   easier if we didn't have to check for the flag when reading the code.

At the same time, we cannot parenthesize (*) blindly.
Consider this Haskell98 kind:          ((* -> *) -> *) -> *
With parentheses, it is less readable: (((*) -> (*)) -> (*)) -> (*)

The solution is to assign a special precedence to (*), 'starPrec', which is
higher than 'funPrec' but lower than 'appPrec':

   F * * *   becomes  F (*) (*) (*)
   F A * B   becomes  F A (*) B
   Proxy *   becomes  Proxy (*)
   a * -> *  becomes  a (*) -> *
-}

{-
************************************************************************
*                                                                      *
                Tuples
*                                                                      *
************************************************************************
-}

data TupleSort
  = BoxedTuple
  | UnboxedTuple
  | ConstraintTuple
  deriving( Eq, Data )

instance Outputable TupleSort where
  ppr ts = text $
    case ts of
      BoxedTuple      -> "BoxedTuple"
      UnboxedTuple    -> "UnboxedTuple"
      ConstraintTuple -> "ConstraintTuple"

instance Binary TupleSort where
    put_ bh BoxedTuple      = putByte bh 0
    put_ bh UnboxedTuple    = putByte bh 1
    put_ bh ConstraintTuple = putByte bh 2
    get bh = do
      h <- getByte bh
      case h of
        0 -> return BoxedTuple
        1 -> return UnboxedTuple
        _ -> return ConstraintTuple


tupleSortBoxity :: TupleSort -> Boxity
tupleSortBoxity BoxedTuple      = Boxed
tupleSortBoxity UnboxedTuple    = Unboxed
tupleSortBoxity ConstraintTuple = Boxed

boxityTupleSort :: Boxity -> TupleSort
boxityTupleSort Boxed   = BoxedTuple
boxityTupleSort Unboxed = UnboxedTuple

tupleParens :: TupleSort -> SDoc -> SDoc
tupleParens BoxedTuple      p = parens p
tupleParens UnboxedTuple    p = text "(#" <+> p <+> text "#)"
tupleParens ConstraintTuple p   -- In debug-style write (% Eq a, Ord b %)
  = ifPprDebug (text "(%" <+> p <+> text "%)")
               (parens p)

{-
************************************************************************
*                                                                      *
                Sums
*                                                                      *
************************************************************************
-}

sumParens :: SDoc -> SDoc
sumParens p = text "(#" <+> p <+> text "#)"

-- | Pretty print an alternative in an unboxed sum e.g. "| a | |".
pprAlternative :: (a -> SDoc) -- ^ The pretty printing function to use
               -> a           -- ^ The things to be pretty printed
               -> ConTag      -- ^ Alternative (one-based)
               -> Arity       -- ^ Arity
               -> SDoc        -- ^ 'SDoc' where the alternative havs been pretty
                              -- printed and finally packed into a paragraph.
pprAlternative pp x alt arity =
    fsep (replicate (alt - 1) vbar ++ [pp x] ++ replicate (arity - alt) vbar)

{-
************************************************************************
*                                                                      *
\subsection[Generic]{Generic flag}
*                                                                      *
************************************************************************

This is the "Embedding-Projection pair" datatype, it contains
two pieces of code (normally either RenamedExpr's or Id's)
If we have a such a pair (EP from to), the idea is that 'from' and 'to'
represents functions of type

        from :: T -> Tring
        to   :: Tring -> T

And we should have

        to (from x) = x

T and Tring are arbitrary, but typically T is the 'main' type while
Tring is the 'representation' type.  (This just helps us remember
whether to use 'from' or 'to'.
-}

-- | Embedding Projection pair
data EP a = EP { fromEP :: a,   -- :: T -> Tring
                 toEP   :: a }  -- :: Tring -> T

{-
Embedding-projection pairs are used in several places:

First of all, each type constructor has an EP associated with it, the
code in EP converts (datatype T) from T to Tring and back again.

Secondly, when we are filling in Generic methods (in the typechecker,
tcMethodBinds), we are constructing bimaps by induction on the structure
of the type of the method signature.


************************************************************************
*                                                                      *
\subsection{Occurrence information}
*                                                                      *
************************************************************************

This data type is used exclusively by the simplifier, but it appears in a
SubstResult, which is currently defined in GHC.Types.Var.Env, which is pretty
near the base of the module hierarchy.  So it seemed simpler to put the defn of
OccInfo here, safely at the bottom
-}

-- | identifier Occurrence Information
data OccInfo
  = ManyOccs        { occ_tail    :: !TailCallInfo }
                        -- ^ There are many occurrences, or unknown occurrences

  | IAmDead             -- ^ Marks unused variables.  Sometimes useful for
                        -- lambda and case-bound variables.

  | OneOcc          { occ_in_lam  :: !InsideLam
                    , occ_n_br    :: {-# UNPACK #-} !BranchCount
                    , occ_int_cxt :: !InterestingCxt
                    , occ_tail    :: !TailCallInfo }
                        -- ^ Occurs exactly once (per branch), not inside a rule

  -- | This identifier breaks a loop of mutually recursive functions. The field
  -- marks whether it is only a loop breaker due to a reference in a rule
  | IAmALoopBreaker { occ_rules_only :: !RulesOnly
                    , occ_tail       :: !TailCallInfo }
                        -- Note [LoopBreaker OccInfo]
  deriving (Eq)

type RulesOnly = Bool

type BranchCount = Int
  -- For OneOcc, the BranchCount says how many syntactic occurrences there are
  -- At the moment we really only check for 1 or >1, but in principle
  --   we could pay attention to how *many* occurrences there are
  --   (notably in postInlineUnconditionally).
  -- But meanwhile, Ints are very efficiently represented.

oneBranch :: BranchCount
oneBranch = 1

{-
Note [LoopBreaker OccInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~
   IAmALoopBreaker True  <=> A "weak" or rules-only loop breaker
                             Do not preInlineUnconditionally

   IAmALoopBreaker False <=> A "strong" loop breaker
                             Do not inline at all

See OccurAnal Note [Weak loop breakers]
-}

noOccInfo :: OccInfo
noOccInfo = ManyOccs { occ_tail = NoTailCallInfo }

isNoOccInfo :: OccInfo -> Bool
isNoOccInfo ManyOccs { occ_tail = NoTailCallInfo } = True
isNoOccInfo _ = False

isManyOccs :: OccInfo -> Bool
isManyOccs ManyOccs{} = True
isManyOccs _          = False

seqOccInfo :: OccInfo -> ()
seqOccInfo occ = occ `seq` ()

-----------------
-- | Interesting Context
data InterestingCxt
  = IsInteresting
    -- ^ Function: is applied
    --   Data value: scrutinised by a case with at least one non-DEFAULT branch
  | NotInteresting
  deriving (Eq)

-- | If there is any 'interesting' identifier occurrence, then the
-- aggregated occurrence info of that identifier is considered interesting.
instance Semi.Semigroup InterestingCxt where
  NotInteresting <> x = x
  IsInteresting  <> _ = IsInteresting

instance Monoid InterestingCxt where
  mempty = NotInteresting
  mappend = (Semi.<>)

-----------------
-- | Inside Lambda
data InsideLam
  = IsInsideLam
    -- ^ Occurs inside a non-linear lambda
    -- Substituting a redex for this occurrence is
    -- dangerous because it might duplicate work.
  | NotInsideLam
  deriving (Eq)

-- | If any occurrence of an identifier is inside a lambda, then the
-- occurrence info of that identifier marks it as occurring inside a lambda
instance Semi.Semigroup InsideLam where
  NotInsideLam <> x = x
  IsInsideLam  <> _ = IsInsideLam

instance Monoid InsideLam where
  mempty = NotInsideLam
  mappend = (Semi.<>)

-----------------
data TailCallInfo = AlwaysTailCalled JoinArity -- See Note [TailCallInfo]
                  | NoTailCallInfo
  deriving (Eq)

tailCallInfo :: OccInfo -> TailCallInfo
tailCallInfo IAmDead   = NoTailCallInfo
tailCallInfo other     = occ_tail other

zapOccTailCallInfo :: OccInfo -> OccInfo
zapOccTailCallInfo IAmDead   = IAmDead
zapOccTailCallInfo occ       = occ { occ_tail = NoTailCallInfo }

isAlwaysTailCalled :: OccInfo -> Bool
isAlwaysTailCalled occ
  = case tailCallInfo occ of AlwaysTailCalled{} -> True
                             NoTailCallInfo     -> False

instance Outputable TailCallInfo where
  ppr (AlwaysTailCalled ar) = sep [ text "Tail", int ar ]
  ppr _                     = empty

-----------------
strongLoopBreaker, weakLoopBreaker :: OccInfo
strongLoopBreaker = IAmALoopBreaker False NoTailCallInfo
weakLoopBreaker   = IAmALoopBreaker True  NoTailCallInfo

isWeakLoopBreaker :: OccInfo -> Bool
isWeakLoopBreaker (IAmALoopBreaker{}) = True
isWeakLoopBreaker _                   = False

isStrongLoopBreaker :: OccInfo -> Bool
isStrongLoopBreaker (IAmALoopBreaker { occ_rules_only = False }) = True
  -- Loop-breaker that breaks a non-rule cycle
isStrongLoopBreaker _                                            = False

isDeadOcc :: OccInfo -> Bool
isDeadOcc IAmDead = True
isDeadOcc _       = False

isOneOcc :: OccInfo -> Bool
isOneOcc (OneOcc {}) = True
isOneOcc _           = False

zapFragileOcc :: OccInfo -> OccInfo
-- Keep only the most robust data: deadness, loop-breaker-hood
zapFragileOcc (OneOcc {}) = noOccInfo
zapFragileOcc occ         = zapOccTailCallInfo occ

instance Outputable OccInfo where
  -- only used for debugging; never parsed.  KSW 1999-07
  ppr (ManyOccs tails)     = pprShortTailCallInfo tails
  ppr IAmDead              = text "Dead"
  ppr (IAmALoopBreaker rule_only tails)
        = text "LoopBreaker" <> pp_ro <> pprShortTailCallInfo tails
        where
          pp_ro | rule_only = char '!'
                | otherwise = empty
  ppr (OneOcc inside_lam one_branch int_cxt tail_info)
        = text "Once" <> pp_lam inside_lam <> ppr one_branch <> pp_args int_cxt <> pp_tail
        where
          pp_lam IsInsideLam     = char 'L'
          pp_lam NotInsideLam    = empty
          pp_args IsInteresting  = char '!'
          pp_args NotInteresting = empty
          pp_tail                = pprShortTailCallInfo tail_info

pprShortTailCallInfo :: TailCallInfo -> SDoc
pprShortTailCallInfo (AlwaysTailCalled ar) = char 'T' <> brackets (int ar)
pprShortTailCallInfo NoTailCallInfo        = empty

{-
Note [TailCallInfo]
~~~~~~~~~~~~~~~~~~~
The occurrence analyser determines what can be made into a join point, but it
doesn't change the binder into a JoinId because then it would be inconsistent
with the occurrences. Thus it's left to the simplifier (or to simpleOptExpr) to
change the IdDetails.

The AlwaysTailCalled marker actually means slightly more than simply that the
function is always tail-called. See Note [Invariants on join points].

This info is quite fragile and should not be relied upon unless the occurrence
analyser has *just* run. Use 'Id.isJoinId_maybe' for the permanent state of
the join-point-hood of a binder; a join id itself will not be marked
AlwaysTailCalled.

Note that there is a 'TailCallInfo' on a 'ManyOccs' value. One might expect that
being tail-called would mean that the variable could only appear once per branch
(thus getting a `OneOcc { }` occurrence info), but a join
point can also be invoked from other join points, not just from case branches:

  let j1 x = ...
      j2 y = ... j1 z {- tail call -} ...
  in case w of
       A -> j1 v
       B -> j2 u
       C -> j2 q

Here both 'j1' and 'j2' will get marked AlwaysTailCalled, but j1 will get
ManyOccs and j2 will get `OneOcc { occ_n_br = 2 }`.

************************************************************************
*                                                                      *
                Default method specification
*                                                                      *
************************************************************************

The DefMethSpec enumeration just indicates what sort of default method
is used for a class. It is generated from source code, and present in
interface files; it is converted to Class.DefMethInfo before begin put in a
Class object.
-}

-- | Default Method Specification
data DefMethSpec ty
  = VanillaDM     -- Default method given with polymorphic code
  | GenericDM ty  -- Default method given with code of this type

instance Outputable (DefMethSpec ty) where
  ppr VanillaDM      = text "{- Has default method -}"
  ppr (GenericDM {}) = text "{- Has generic default method -}"

{-
************************************************************************
*                                                                      *
\subsection{Success flag}
*                                                                      *
************************************************************************
-}

data SuccessFlag = Succeeded | Failed

instance Outputable SuccessFlag where
    ppr Succeeded = text "Succeeded"
    ppr Failed    = text "Failed"

successIf :: Bool -> SuccessFlag
successIf True  = Succeeded
successIf False = Failed

succeeded, failed :: SuccessFlag -> Bool
succeeded Succeeded = True
succeeded Failed    = False

failed Succeeded = False
failed Failed    = True

{-
************************************************************************
*                                                                      *
\subsection{Activation}
*                                                                      *
************************************************************************

When a rule or inlining is active

Note [Compiler phases]
~~~~~~~~~~~~~~~~~~~~~~
The CompilerPhase says which phase the simplifier is running in:

* InitialPhase: before all user-visible phases

* Phase 2,1,0: user-visible phases; the phase number
  controls rule ordering an inlining.

* FinalPhase: used for all subsequent simplifier
  runs. By delaying inlining of wrappers to FinalPhase we can
  ensure that RULE have a good chance to fire. See
  Note [Wrapper activation] in GHC.Core.Opt.WorkWrap

  NB: FinalPhase is run repeatedly, not just once.

  NB: users don't have access to InitialPhase or FinalPhase.
  They write {-# INLINE[n] f #-}, meaning (Phase n)

The phase sequencing is done by GHC.Opt.Simplify.Driver
-}

-- | Phase Number
type PhaseNum = Int  -- Compilation phase
                     -- Phases decrease towards zero
                     -- Zero is the last phase

data CompilerPhase
  = InitialPhase    -- The first phase -- number = infinity!
  | Phase PhaseNum  -- User-specificable phases
  | FinalPhase      -- The last phase  -- number = -infinity!
  deriving Eq

instance Outputable CompilerPhase where
   ppr (Phase n)    = int n
   ppr InitialPhase = text "InitialPhase"
   ppr FinalPhase   = text "FinalPhase"

-- See note [Pragma source text]
data Activation
  = AlwaysActive
  | ActiveBefore SourceText PhaseNum  -- Active only *strictly before* this phase
  | ActiveAfter  SourceText PhaseNum  -- Active in this phase and later
  | FinalActive                       -- Active in final phase only
  | NeverActive
  deriving( Eq, Data )
    -- Eq used in comparing rules in GHC.Hs.Decls

beginPhase :: Activation -> CompilerPhase
-- First phase in which the Activation is active
-- or FinalPhase if it is never active
beginPhase AlwaysActive      = InitialPhase
beginPhase (ActiveBefore {}) = InitialPhase
beginPhase (ActiveAfter _ n) = Phase n
beginPhase FinalActive       = FinalPhase
beginPhase NeverActive       = FinalPhase

activeAfter :: CompilerPhase -> Activation
-- (activeAfter p) makes an Activation that is active in phase p and after
-- Invariant: beginPhase (activeAfter p) = p
activeAfter InitialPhase = AlwaysActive
activeAfter (Phase n)    = ActiveAfter NoSourceText n
activeAfter FinalPhase   = FinalActive

nextPhase :: CompilerPhase -> CompilerPhase
-- Tells you the next phase after this one
-- Currently we have just phases [2,1,0,FinalPhase,FinalPhase,...]
-- Where FinalPhase means GHC's internal simplification steps
-- after all rules have run
nextPhase InitialPhase = Phase 2
nextPhase (Phase 0)    = FinalPhase
nextPhase (Phase n)    = Phase (n-1)
nextPhase FinalPhase   = FinalPhase

laterPhase :: CompilerPhase -> CompilerPhase -> CompilerPhase
-- Returns the later of two phases
laterPhase (Phase n1)   (Phase n2)   = Phase (n1 `min` n2)
laterPhase InitialPhase p2           = p2
laterPhase FinalPhase   _            = FinalPhase
laterPhase p1           InitialPhase = p1
laterPhase _            FinalPhase   = FinalPhase

activateAfterInitial :: Activation
-- Active in the first phase after the initial phase
activateAfterInitial = activeAfter (nextPhase InitialPhase)

activateDuringFinal :: Activation
-- Active in the final simplification phase (which is repeated)
activateDuringFinal = FinalActive

isActive :: CompilerPhase -> Activation -> Bool
isActive InitialPhase act = activeInInitialPhase act
isActive (Phase p)    act = activeInPhase p act
isActive FinalPhase   act = activeInFinalPhase act

activeInInitialPhase :: Activation -> Bool
activeInInitialPhase AlwaysActive      = True
activeInInitialPhase (ActiveBefore {}) = True
activeInInitialPhase _                 = False

activeInPhase :: PhaseNum -> Activation -> Bool
activeInPhase _ AlwaysActive       = True
activeInPhase _ NeverActive        = False
activeInPhase _ FinalActive        = False
activeInPhase p (ActiveAfter  _ n) = p <= n
activeInPhase p (ActiveBefore _ n) = p >  n

activeInFinalPhase :: Activation -> Bool
activeInFinalPhase AlwaysActive     = True
activeInFinalPhase FinalActive      = True
activeInFinalPhase (ActiveAfter {}) = True
activeInFinalPhase _                = False

isNeverActive, isAlwaysActive :: Activation -> Bool
isNeverActive NeverActive = True
isNeverActive _           = False

isAlwaysActive AlwaysActive = True
isAlwaysActive _            = False

competesWith :: Activation -> Activation -> Bool
-- See Note [Activation competition]
competesWith AlwaysActive      _                = True

competesWith NeverActive       _                = False
competesWith _                 NeverActive      = False

competesWith FinalActive       FinalActive      = True
competesWith FinalActive       _                = False

competesWith (ActiveBefore {})  AlwaysActive      = True
competesWith (ActiveBefore {})  FinalActive       = False
competesWith (ActiveBefore {})  (ActiveBefore {}) = True
competesWith (ActiveBefore _ a) (ActiveAfter _ b) = a < b

competesWith (ActiveAfter {})  AlwaysActive      = False
competesWith (ActiveAfter {})  FinalActive       = True
competesWith (ActiveAfter {})  (ActiveBefore {}) = False
competesWith (ActiveAfter _ a) (ActiveAfter _ b) = a >= b

{- Note [Competing activations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes a RULE and an inlining may compete, or two RULES.
See Note [Rules and inlining/other rules] in GHC.HsToCore.

We say that act1 "competes with" act2 iff
   act1 is active in the phase when act2 *becomes* active
NB: remember that phases count *down*: 2, 1, 0!

It's too conservative to ensure that the two are never simultaneously
active.  For example, a rule might be always active, and an inlining
might switch on in phase 2.  We could switch off the rule, but it does
no harm.
-}


{- *********************************************************************
*                                                                      *
                 InlinePragma, InlineSpec, RuleMatchInfo
*                                                                      *
********************************************************************* -}


data InlinePragma            -- Note [InlinePragma]
  = InlinePragma
      { inl_src    :: SourceText -- Note [Pragma source text]
      , inl_inline :: InlineSpec -- See Note [inl_inline and inl_act]

      , inl_sat    :: Maybe Arity    -- Just n <=> Inline only when applied to n
                                     --            explicit (non-type, non-dictionary) args
                                     --   That is, inl_sat describes the number of *source-code*
                                     --   arguments the thing must be applied to.  We add on the
                                     --   number of implicit, dictionary arguments when making
                                     --   the Unfolding, and don't look at inl_sat further

      , inl_act    :: Activation     -- Says during which phases inlining is allowed
                                     -- See Note [inl_inline and inl_act]

      , inl_rule   :: RuleMatchInfo  -- Should the function be treated like a constructor?
    } deriving( Eq, Data )

-- | Rule Match Information
data RuleMatchInfo = ConLike                    -- See Note [CONLIKE pragma]
                   | FunLike
                   deriving( Eq, Data, Show )
        -- Show needed for GHC.Parser.Lexer

-- | Inline Specification
data InlineSpec   -- What the user's INLINE pragma looked like
  = Inline    SourceText       -- User wrote INLINE
  | Inlinable SourceText       -- User wrote INLINABLE
  | NoInline  SourceText       -- User wrote NOINLINE
                               -- Each of the above keywords is accompanied with
                               -- a string of type SourceText written by the user
  | NoUserInlinePrag -- User did not write any of INLINE/INLINABLE/NOINLINE
                     -- e.g. in `defaultInlinePragma` or when created by CSE
  deriving( Eq, Data, Show )
        -- Show needed for GHC.Parser.Lexer

{- Note [InlinePragma]
~~~~~~~~~~~~~~~~~~~~~~
This data type mirrors what you can write in an INLINE or NOINLINE pragma in
the source program.

If you write nothing at all, you get defaultInlinePragma:
   inl_inline = NoUserInlinePrag
   inl_act    = AlwaysActive
   inl_rule   = FunLike

It's not possible to get that combination by *writing* something, so
if an Id has defaultInlinePragma it means the user didn't specify anything.

If inl_inline = Inline or Inlineable, then the Id should have an InlineRule unfolding.

If you want to know where InlinePragmas take effect: Look in GHC.HsToCore.Binds.makeCorePair

Note [inl_inline and inl_act]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* inl_inline says what the user wrote: did they say INLINE, NOINLINE,
  INLINABLE, or nothing at all

* inl_act says in what phases the unfolding is active or inactive
  E.g  If you write INLINE[1]    then inl_act will be set to ActiveAfter 1
       If you write NOINLINE[1]  then inl_act will be set to ActiveBefore 1
       If you write NOINLINE[~1] then inl_act will be set to ActiveAfter 1
  So note that inl_act does not say what pragma you wrote: it just
  expresses its consequences

* inl_act just says when the unfolding is active; it doesn't say what
  to inline.  If you say INLINE f, then f's inl_act will be AlwaysActive,
  but in addition f will get a "stable unfolding" with UnfoldingGuidance
  that tells the inliner to be pretty eager about it.

Note [CONLIKE pragma]
~~~~~~~~~~~~~~~~~~~~~
The ConLike constructor of a RuleMatchInfo is aimed at the following.
Consider first
    {-# RULE "r/cons" forall a as. r (a:as) = f (a+1) #-}
    g b bs = let x = b:bs in ..x...x...(r x)...
Now, the rule applies to the (r x) term, because GHC "looks through"
the definition of 'x' to see that it is (b:bs).

Now consider
    {-# RULE "r/f" forall v. r (f v) = f (v+1) #-}
    g v = let x = f v in ..x...x...(r x)...
Normally the (r x) would *not* match the rule, because GHC would be
scared about duplicating the redex (f v), so it does not "look
through" the bindings.

However the CONLIKE modifier says to treat 'f' like a constructor in
this situation, and "look through" the unfolding for x.  So (r x)
fires, yielding (f (v+1)).

This is all controlled with a user-visible pragma:
     {-# NOINLINE CONLIKE [1] f #-}

The main effects of CONLIKE are:

    - The occurrence analyser (OccAnal) and simplifier (Simplify) treat
      CONLIKE thing like constructors, by ANF-ing them

    - New function GHC.Core.Utils.exprIsExpandable is like exprIsCheap, but
      additionally spots applications of CONLIKE functions

    - A CoreUnfolding has a field that caches exprIsExpandable

    - The rule matcher consults this field.  See
      Note [Expanding variables] in GHC.Core.Rules.
-}

isConLike :: RuleMatchInfo -> Bool
isConLike ConLike = True
isConLike _       = False

isFunLike :: RuleMatchInfo -> Bool
isFunLike FunLike = True
isFunLike _       = False

noUserInlineSpec :: InlineSpec -> Bool
noUserInlineSpec NoUserInlinePrag = True
noUserInlineSpec _                = False

defaultInlinePragma, alwaysInlinePragma, neverInlinePragma, dfunInlinePragma
  :: InlinePragma
defaultInlinePragma = InlinePragma { inl_src = SourceText "{-# INLINE"
                                   , inl_act = AlwaysActive
                                   , inl_rule = FunLike
                                   , inl_inline = NoUserInlinePrag
                                   , inl_sat = Nothing }

alwaysInlinePragma = defaultInlinePragma { inl_inline = Inline (inlinePragmaSource defaultInlinePragma) }
neverInlinePragma  = defaultInlinePragma { inl_act    = NeverActive }

alwaysInlineConLikePragma :: InlinePragma
alwaysInlineConLikePragma = alwaysInlinePragma { inl_rule = ConLike }

inlinePragmaSpec :: InlinePragma -> InlineSpec
inlinePragmaSpec = inl_inline

inlinePragmaSource :: InlinePragma -> SourceText
inlinePragmaSource prag = case inl_inline prag of
                            Inline    x      -> x
                            Inlinable y      -> y
                            NoInline  z      -> z
                            NoUserInlinePrag -> NoSourceText

inlineSpecSource :: InlineSpec -> SourceText
inlineSpecSource spec = case spec of
                            Inline    x      -> x
                            Inlinable y      -> y
                            NoInline  z      -> z
                            NoUserInlinePrag -> NoSourceText

-- A DFun has an always-active inline activation so that
-- exprIsConApp_maybe can "see" its unfolding
-- (However, its actual Unfolding is a DFunUnfolding, which is
--  never inlined other than via exprIsConApp_maybe.)
dfunInlinePragma   = defaultInlinePragma { inl_act  = AlwaysActive
                                         , inl_rule = ConLike }

isDefaultInlinePragma :: InlinePragma -> Bool
isDefaultInlinePragma (InlinePragma { inl_act = activation
                                    , inl_rule = match_info
                                    , inl_inline = inline })
  = noUserInlineSpec inline && isAlwaysActive activation && isFunLike match_info

isInlinePragma :: InlinePragma -> Bool
isInlinePragma prag = case inl_inline prag of
                        Inline _  -> True
                        _         -> False

isInlinablePragma :: InlinePragma -> Bool
isInlinablePragma prag = case inl_inline prag of
                           Inlinable _  -> True
                           _            -> False

isNoInlinePragma :: InlinePragma -> Bool
isNoInlinePragma prag = case inl_inline prag of
                          NoInline _   -> True
                          _            -> False

isAnyInlinePragma :: InlinePragma -> Bool
-- INLINE or INLINABLE
isAnyInlinePragma prag = case inl_inline prag of
                        Inline    _   -> True
                        Inlinable _   -> True
                        _             -> False

inlinePragmaSat :: InlinePragma -> Maybe Arity
inlinePragmaSat = inl_sat

inlinePragmaActivation :: InlinePragma -> Activation
inlinePragmaActivation (InlinePragma { inl_act = activation }) = activation

inlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo
inlinePragmaRuleMatchInfo (InlinePragma { inl_rule = info }) = info

setInlinePragmaActivation :: InlinePragma -> Activation -> InlinePragma
setInlinePragmaActivation prag activation = prag { inl_act = activation }

setInlinePragmaRuleMatchInfo :: InlinePragma -> RuleMatchInfo -> InlinePragma
setInlinePragmaRuleMatchInfo prag info = prag { inl_rule = info }

instance Outputable Activation where
   ppr AlwaysActive       = empty
   ppr NeverActive        = brackets (text "~")
   ppr (ActiveBefore _ n) = brackets (char '~' <> int n)
   ppr (ActiveAfter  _ n) = brackets (int n)
   ppr FinalActive        = text "[final]"

instance Binary Activation where
    put_ bh NeverActive =
            putByte bh 0
    put_ bh FinalActive =
            putByte bh 1
    put_ bh AlwaysActive =
            putByte bh 2
    put_ bh (ActiveBefore src aa) = do
            putByte bh 3
            put_ bh src
            put_ bh aa
    put_ bh (ActiveAfter src ab) = do
            putByte bh 4
            put_ bh src
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> return NeverActive
              1 -> return FinalActive
              2 -> return AlwaysActive
              3 -> do src <- get bh
                      aa <- get bh
                      return (ActiveBefore src aa)
              _ -> do src <- get bh
                      ab <- get bh
                      return (ActiveAfter src ab)

instance Outputable RuleMatchInfo where
   ppr ConLike = text "CONLIKE"
   ppr FunLike = text "FUNLIKE"

instance Binary RuleMatchInfo where
    put_ bh FunLike = putByte bh 0
    put_ bh ConLike = putByte bh 1
    get bh = do
            h <- getByte bh
            if h == 1 then return ConLike
                      else return FunLike

instance Outputable InlineSpec where
    ppr (Inline          src)  = text "INLINE" <+> pprWithSourceText src empty
    ppr (NoInline        src)  = text "NOINLINE" <+> pprWithSourceText src empty
    ppr (Inlinable       src)  = text "INLINABLE" <+> pprWithSourceText src empty
    ppr NoUserInlinePrag       = empty

instance Binary InlineSpec where
    put_ bh NoUserInlinePrag = putByte bh 0
    put_ bh (Inline s)       = do putByte bh 1
                                  put_ bh s
    put_ bh (Inlinable s)    = do putByte bh 2
                                  put_ bh s
    put_ bh (NoInline s)     = do putByte bh 3
                                  put_ bh s

    get bh = do h <- getByte bh
                case h of
                  0 -> return NoUserInlinePrag
                  1 -> do
                        s <- get bh
                        return (Inline s)
                  2 -> do
                        s <- get bh
                        return (Inlinable s)
                  _ -> do
                        s <- get bh
                        return (NoInline s)

instance Outputable InlinePragma where
  ppr = pprInline

instance Binary InlinePragma where
    put_ bh (InlinePragma s a b c d) = do
            put_ bh s
            put_ bh a
            put_ bh b
            put_ bh c
            put_ bh d

    get bh = do
           s <- get bh
           a <- get bh
           b <- get bh
           c <- get bh
           d <- get bh
           return (InlinePragma s a b c d)

-- | Outputs string for pragma name for any of INLINE/INLINABLE/NOINLINE. This
-- differs from the Outputable instance for the InlineSpec type where the pragma
-- name string as well as the accompanying SourceText (if any) is printed.
inlinePragmaName :: InlineSpec -> SDoc
inlinePragmaName (Inline            _)  = text "INLINE"
inlinePragmaName (Inlinable         _)  = text "INLINABLE"
inlinePragmaName (NoInline          _)  = text "NOINLINE"
inlinePragmaName NoUserInlinePrag       = empty

pprInline :: InlinePragma -> SDoc
pprInline = pprInline' True

pprInlineDebug :: InlinePragma -> SDoc
pprInlineDebug = pprInline' False

pprInline' :: Bool           -- True <=> do not display the inl_inline field
           -> InlinePragma
           -> SDoc
pprInline' emptyInline (InlinePragma
                        { inl_inline = inline,
                          inl_act = activation,
                          inl_rule = info,
                          inl_sat = mb_arity })
    = pp_inl inline <> pp_act inline activation <+> pp_sat <+> pp_info
    where
      pp_inl x = if emptyInline then empty else inlinePragmaName x

      pp_act Inline   {}  AlwaysActive = empty
      pp_act NoInline {}  NeverActive  = empty
      pp_act _            act          = ppr act

      pp_sat | Just ar <- mb_arity = parens (text "sat-args=" <> int ar)
             | otherwise           = empty
      pp_info | isFunLike info = empty
              | otherwise      = ppr info



{-
************************************************************************
*                                                                      *
    IntWithInf
*                                                                      *
************************************************************************

Represents an integer or positive infinity

-}

-- | An integer or infinity
data IntWithInf = Int {-# UNPACK #-} !Int
                | Infinity
  deriving Eq

-- | A representation of infinity
infinity :: IntWithInf
infinity = Infinity

instance Ord IntWithInf where
  compare Infinity Infinity = EQ
  compare (Int _)  Infinity = LT
  compare Infinity (Int _)  = GT
  compare (Int a)  (Int b)  = a `compare` b

instance Outputable IntWithInf where
  ppr Infinity = char '∞'
  ppr (Int n)  = int n

instance Num IntWithInf where
  (+) = plusWithInf
  (*) = mulWithInf

  abs Infinity = Infinity
  abs (Int n)  = Int (abs n)

  signum Infinity = Int 1
  signum (Int n)  = Int (signum n)

  fromInteger = Int . fromInteger

  (-) = panic "subtracting IntWithInfs"

intGtLimit :: Int -> IntWithInf -> Bool
intGtLimit _ Infinity = False
intGtLimit n (Int m)  = n > m

-- | Add two 'IntWithInf's
plusWithInf :: IntWithInf -> IntWithInf -> IntWithInf
plusWithInf Infinity _        = Infinity
plusWithInf _        Infinity = Infinity
plusWithInf (Int a)  (Int b)  = Int (a + b)

-- | Multiply two 'IntWithInf's
mulWithInf :: IntWithInf -> IntWithInf -> IntWithInf
mulWithInf Infinity _        = Infinity
mulWithInf _        Infinity = Infinity
mulWithInf (Int a)  (Int b)  = Int (a * b)

-- | Subtract an 'Int' from an 'IntWithInf'
subWithInf :: IntWithInf -> Int -> IntWithInf
subWithInf Infinity _ = Infinity
subWithInf (Int a)  b = Int (a - b)

-- | Turn a positive number into an 'IntWithInf', where 0 represents infinity
treatZeroAsInf :: Int -> IntWithInf
treatZeroAsInf 0 = Infinity
treatZeroAsInf n = Int n

-- | Inject any integer into an 'IntWithInf'
mkIntWithInf :: Int -> IntWithInf
mkIntWithInf = Int

data SpliceExplicitFlag
          = ExplicitSplice | -- ^ <=> $(f x y)
            ImplicitSplice   -- ^ <=> f x y,  i.e. a naked top level expression
    deriving Data

{- *********************************************************************
*                                                                      *
                        Types vs Kinds
*                                                                      *
********************************************************************* -}

-- | Flag to see whether we're type-checking terms or kind-checking types
data TypeOrKind = TypeLevel | KindLevel
  deriving Eq

instance Outputable TypeOrKind where
  ppr TypeLevel = text "TypeLevel"
  ppr KindLevel = text "KindLevel"

isTypeLevel :: TypeOrKind -> Bool
isTypeLevel TypeLevel = True
isTypeLevel KindLevel = False

isKindLevel :: TypeOrKind -> Bool
isKindLevel TypeLevel = False
isKindLevel KindLevel = True

{- *********************************************************************
*                                                                      *
                        Defaulting options
*                                                                      *
********************************************************************* -}

{- Note [Type variable defaulting options]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is an overview of the current type variable defaulting mechanisms,
in the order in which they happen.

GHC.Tc.Utils.TcMType.defaultTyVar

  This is a built-in defaulting mechanism for the following type variables:

    (1) kind variables with -XNoPolyKinds,
    (2) type variables of kind 'RuntimeRep' default to 'LiftedRep',
        of kind 'Levity' to 'Lifted', and of kind 'Multiplicity' to 'Many'.

  It is used in many situations:

    - inferring a type (e.g. a declaration with no type signature or a
      partial type signature), in 'GHC.Tc.Solver.simplifyInfer',
    - simplifying top-level constraints in 'GHC.Tc.Solver.simplifyTop',
    - kind checking a CUSK in 'GHC.Tc.Gen.kcCheckDeclHeader_cusk',
    - 'GHC.Tc.TyCl.generaliseTcTyCon',
    - type checking type family and data family instances,
      in 'GHC.Tc.TyCl.tcTyFamInstEqnGuts' and 'GHC.Tc.TyCl.Instance.tcDataFamInstHeader'
      respectively,
    - type-checking rules in 'GHC.Tc.Gen.tcRule',
    - kind generalisation in 'GHC.Tc.Gen.HsType.kindGeneralizeSome'
      and 'GHC.Tc.Gen.HsType.kindGeneralizeAll'.

  Different situations call for a different defaulting strategy,
  so 'defaultTyVar' takes a strategy parameter which determines which
  type variables to default.
  Currently, this strategy is set as follows:

    - Kind variables:
      - with -XNoPolyKinds, these must be defaulted. This includes kind variables
        of kind 'RuntimeRep', 'Levity' and 'Multiplicity'.
        Test case: T20584.
      - with -XPolyKinds, behave as if they were type variables (see below).
    - Type variables of kind 'RuntimeRep', 'Levity' or 'Multiplicity'
      - in type and data families instances, these are not defaulted.
        Test case: T17536.
      - otherwise: default variables of these three kinds. This ensures
        that in a program such as

          foo :: forall a. a -> a
          foo x = x

        we continue to infer `a :: Type`.

  Note that the strategy is set in two steps: callers of 'defaultTyVars' only
  specify whether to default type variables of "non-standard" kinds
  (that is, of kinds 'RuntimeRep'/'Levity'/'Multiplicity'). Then 'defaultTyVars'
  determines which variables are type variables and which are kind variables,
  and if the user has asked for -XNoPolyKinds we default the kind variables.

GHC.Tc.Solver.defaultTyVarTcS

  This is a built-in defaulting mechanism that happens after
  the constraint solver has run, in 'GHC.Tc.Solver.simplifyTopWanteds'.

  It only defaults type (and kind) variables of kind 'RuntimeRep',
  'Levity', 'Multiplicity'.

  It is not configurable, neither by options nor by the user.

GHC.Tc.Solver.applyDefaultingRules

  This is typeclass defaulting, and includes defaulting plugins.
  It happens right after 'defaultTyVarTcS' in 'GHC.Tc.Solver.simplifyTopWanteds'.
  It is user configurable, using default declarations (/plugins).

GHC.Iface.Type.defaultIfaceTyVarsOfKind

  This is a built-in defaulting mechanism that only applies when pretty-printing.
  It defaults 'RuntimeRep'/'Levity' variables unless -fprint-explicit-kinds is enabled,
  and 'Multiplicity' variables unless -XLinearTypes is enabled.

-}

-- | Specify whether to default type variables of kind 'RuntimeRep'/'Levity'/'Multiplicity'.
data NonStandardDefaultingStrategy
  -- | Default type variables of the given kinds:
  --
  --   - default 'RuntimeRep' variables to 'LiftedRep'
  --   - default 'Levity' variables to 'Lifted'
  --   - default 'Multiplicity' variables to 'Many'
  = DefaultNonStandardTyVars
  -- | Try not to default type variables of the kinds 'RuntimeRep'/'Levity'/'Multiplicity'.
  --
  -- Note that these might get defaulted anyway, if they are kind variables
  -- and `-XNoPolyKinds` is enabled.
  | TryNotToDefaultNonStandardTyVars

-- | Specify whether to default kind variables, and type variables
-- of kind 'RuntimeRep'/'Levity'/'Multiplicity'.
data DefaultingStrategy
  -- | Default kind variables:
  --
  --   - default kind variables of kind 'Type' to 'Type',
  --   - default 'RuntimeRep'/'Levity'/'Multiplicity' kind variables
  --     to 'LiftedRep'/'Lifted'/'Many', respectively.
  --
  -- When this strategy is used, it means that we have determined that
  -- the variables we are considering defaulting are all kind variables.
  --
  -- Usually, we pass this option when -XNoPolyKinds is enabled.
  = DefaultKindVars
  -- | Default (or don't default) non-standard variables, of kinds
  -- 'RuntimeRep', 'Levity' and 'Multiplicity'.
  | NonStandardDefaulting NonStandardDefaultingStrategy

defaultNonStandardTyVars :: DefaultingStrategy -> Bool
defaultNonStandardTyVars DefaultKindVars                                          = True
defaultNonStandardTyVars (NonStandardDefaulting DefaultNonStandardTyVars)         = True
defaultNonStandardTyVars (NonStandardDefaulting TryNotToDefaultNonStandardTyVars) = False

instance Outputable NonStandardDefaultingStrategy where
  ppr DefaultNonStandardTyVars         = text "DefaultOnlyNonStandardTyVars"
  ppr TryNotToDefaultNonStandardTyVars = text "TryNotToDefaultNonStandardTyVars"

instance Outputable DefaultingStrategy where
  ppr DefaultKindVars            = text "DefaultKindVars"
  ppr (NonStandardDefaulting ns) = text "NonStandardDefaulting" <+> ppr ns
