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

{-# OPTIONS_GHC -Wno-orphans #-} -- Outputable PromotionFlag, Binary PromotionFlag, Outputable Boxity, Binay Boxity
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module GHC.Types.Basic (
        LeftOrRight(..),
        pickLR,

        ConTag, ConTagZ, fIRST_TAG,

        Arity, VisArity, RepArity, JoinArity, FullArgCount,
        JoinPointHood(..), isJoinPoint,

        Alignment, mkAlignment, alignmentOf, alignmentBytes,

        PromotionFlag(..), isPromoted,
        FunctionOrData(..),

        RecFlag(..), isRec, isNonRec, boolToRecFlag,
        Origin(..), isGenerated, DoPmc(..), requiresPMC,
        GenReason(..), isDoExpansionGenerated, doExpansionFlavour,
        doExpansionOrigin,

        RuleName, pprRuleName,

        TopLevelFlag(..), isTopLevel, isNotTopLevel,

        Boxity(..), isBoxed,

        CbvMark(..), isMarkedCbv,

        PprPrec(..), topPrec, sigPrec, opPrec, funPrec,
        starPrec, appPrec, maxPrec,
        maybeParen,

        TupleSort(..), tupleSortBoxity, boxityTupleSort,
        tupleParens,

        UnboxedTupleOrSum(..), unboxedTupleOrSumExtension,
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
        SwapFlag(..), flipSwap, unSwap, notSwapped, isSwapped, pickSwap,

        UnfoldingSource(..), isStableSource, isStableUserSource,
        isStableSystemSource, isCompulsorySource,

        SuccessFlag(..), succeeded, failed, successIf,

        IntWithInf, infinity, treatZeroAsInf, subWithInf, mkIntWithInf, intGtLimit,

        TypeOrKind(..), isTypeLevel, isKindLevel,

        Levity(..), mightBeLifted, mightBeUnlifted,
        TypeOrConstraint(..),
        NonStandardDefaultingStrategy(..),
        DefaultingStrategy(..), defaultNonStandardTyVars,

        ForeignSrcLang (..)
   ) where

import GHC.Prelude

import GHC.ForeignSrcLang
import GHC.Utils.Outputable
import GHC.Utils.Panic ( panic )
import GHC.Utils.Binary
import qualified GHC.LanguageExtensions as LangExt

import Language.Haskell.Syntax.Basic  (Boxity(..), isBoxed, ConTag
                                      , RuleName
                                      )
import {-# SOURCE #-} Language.Haskell.Syntax.Type (PromotionFlag(..), isPromoted)
import {-# SOURCE #-} Language.Haskell.Syntax.Expr (HsDoFlavour)

import Data.Data
import Data.Maybe
import qualified Data.Semigroup as Semi

{-
************************************************************************
*                                                                      *
          Binary choice
*                                                                      *
********************************************************************* -}

data LeftOrRight = CLeft | CRight
                 deriving( Eq, Data, Ord )

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

-- | Syntactic (visibility) arity, i.e. the number of visible arguments.
-- See Note [Visibility and arity]
type VisArity = Int

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

{- Note [Visibility and arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Arity is the number of arguments that a function expects. In a curried language
like Haskell, there is more than one way to count those arguments.

* `Arity` is the classic notion of arity, concerned with evalution, so it counts
  the number of /value/ arguments that need to be supplied before evaluation can
  take place, as described in notes
    Note [Definition of arity]      in GHC.Core.Opt.Arity
    Note [Arity and function types] in GHC.Types.Id.Info

  Examples:
    Int                       has arity == 0
    Int -> Int                has arity <= 1
    Int -> Bool -> Int        has arity <= 2
  We write (<=) rather than (==) as sometimes evaluation can occur before all
  value arguments are supplied, depending on the actual function definition.

  This evaluation-focused notion of arity ignores type arguments, so:
    forall a.   a             has arity == 0
    forall a.   a -> a        has arity <= 1
    forall a b. a -> b -> a   has arity <= 2
  This is true regardless of ForAllTyFlag, so the arity is also unaffected by
  (forall {a}. ty) or (forall a -> ty).

  Class dictionaries count towards the arity, as they are passed at runtime
    forall a.   (Num a)        => a            has arity <= 1
    forall a.   (Num a)        => a -> a       has arity <= 2
    forall a b. (Num a, Ord b) => a -> b -> a  has arity <= 4

* `VisArity` is the syntactic notion of arity. It is the number of /visible/
  arguments, i.e. arguments that occur visibly in the source code.

  In a function call `f x y z`, we can confidently say that f's vis-arity >= 3,
  simply because we see three arguments [x,y,z]. We write (>=) rather than (==)
  as this could be a partial application.

  At definition sites, we can acquire an underapproximation of vis-arity by
  counting the patterns on the LHS, e.g. `f a b = rhs` has vis-arity >= 2.
  The actual vis-arity can be higher if there is a lambda on the RHS,
  e.g. `f a b = \c -> rhs`.

  If we look at the types, we can observe the following
    * function arrows   (a -> b)        add to the vis-arity
    * visible foralls   (forall a -> b) add to the vis-arity
    * constraint arrows (a => b)        do not affect the vis-arity
    * invisible foralls (forall a. b)   do not affect the vis-arity

  This means that ForAllTyFlag matters for VisArity (in contrast to Arity),
  while the type/value distinction is unimportant (again in contrast to Arity).

  Examples:
    Int                         -- vis-arity == 0   (no args)
    Int -> Int                  -- vis-arity == 1   (1 funarg)
    forall a. a -> a            -- vis-arity == 1   (1 funarg)
    forall a. Num a => a -> a   -- vis-arity == 1   (1 funarg)
    forall a -> Num a => a      -- vis-arity == 1   (1 req tyarg, 0 funargs)
    forall a -> a -> a          -- vis-arity == 2   (1 req tyarg, 1 funarg)
    Int -> forall a -> Int      -- vis-arity == 2   (1 funarg, 1 req tyarg)

  Wrinkle: with TypeApplications and TypeAbstractions, it is possible to visibly
  bind and pass invisible arguments, e.g. `f @a x = ...` or `f @Int 42`. Those
  @-prefixed arguments are ignored for the purposes of vis-arity.
-}

{-
************************************************************************
*                                                                      *
              Constructor tags
*                                                                      *
************************************************************************
-}

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
  more than once.  So g will get a C(1,U) usage demand.

* Occurrence analysis propagates this usage information
  (in the demand signature of a function) to its calls.
  Example, given 'f' above
     f (\x.e) blah

  Since f's demand signature says it has a C(1,U) usage demand on its
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
  deriving( Eq )

instance Outputable SwapFlag where
  ppr IsSwapped  = text "Is-swapped"
  ppr NotSwapped = text "Not-swapped"

flipSwap :: SwapFlag -> SwapFlag
flipSwap IsSwapped  = NotSwapped
flipSwap NotSwapped = IsSwapped

isSwapped :: SwapFlag -> Bool
isSwapped IsSwapped  = True
isSwapped NotSwapped = False

notSwapped :: SwapFlag -> Bool
notSwapped NotSwapped = True
notSwapped IsSwapped  = False

pickSwap :: SwapFlag -> a -> a -> a
pickSwap NotSwapped a _ = a
pickSwap IsSwapped  _ b = b

unSwap :: SwapFlag -> (a->a->b) -> a -> a -> b
unSwap NotSwapped f a b = f a b
unSwap IsSwapped  f a b = f b a


{- *********************************************************************
*                                                                      *
           Promotion flag
*                                                                      *
********************************************************************* -}

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
                Call by value flag
*                                                                      *
************************************************************************
-}

-- | Should an argument be passed evaluated *and* tagged.
data CbvMark = MarkedCbv | NotMarkedCbv
    deriving Eq

instance Outputable CbvMark where
  ppr MarkedCbv    = text "!"
  ppr NotMarkedCbv = text "~"

instance Binary CbvMark where
    put_ bh NotMarkedCbv = putByte bh 0
    put_ bh MarkedCbv    = putByte bh 1
    get bh =
      do h <- getByte bh
         case h of
           0 -> return NotMarkedCbv
           1 -> return MarkedCbv
           _ -> panic "Invalid binary format"

isMarkedCbv :: CbvMark -> Bool
isMarkedCbv MarkedCbv = True
isMarkedCbv NotMarkedCbv = False


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

-- | Was this piece of code user-written or generated by the compiler?
--
-- See Note [Generated code and pattern-match checking].
data Origin = FromSource
            | Generated GenReason DoPmc
            deriving( Eq, Data )

isGenerated :: Origin -> Bool
isGenerated Generated{}  = True
isGenerated FromSource   = False

-- | This metadata stores the information as to why was the piece of code generated
--   It is useful for generating the right error context
-- See Part 3 in Note [Expanding HsDo with XXExprGhcRn] in `GHC.Tc.Gen.Do`
data GenReason = DoExpansion HsDoFlavour
               | OtherExpansion
               deriving (Eq, Data)

instance Outputable GenReason where
  ppr DoExpansion{}  = text "DoExpansion"
  ppr OtherExpansion = text "OtherExpansion"

doExpansionFlavour :: Origin -> Maybe HsDoFlavour
doExpansionFlavour (Generated (DoExpansion f) _) = Just f
doExpansionFlavour _ = Nothing

-- See Part 3 in Note [Expanding HsDo with XXExprGhcRn] in `GHC.Tc.Gen.Do`
isDoExpansionGenerated :: Origin -> Bool
isDoExpansionGenerated = isJust . doExpansionFlavour

-- See Part 3 in Note [Expanding HsDo with XXExprGhcRn] in `GHC.Tc.Gen.Do`
doExpansionOrigin :: HsDoFlavour -> Origin
doExpansionOrigin f = Generated (DoExpansion f) DoPmc
                    -- It is important that we perfrom PMC
                    -- on the expressions generated by do statements
                    -- to get the right pattern match checker warnings
                    -- See `GHC.HsToCore.Pmc.pmcMatches`

instance Outputable Origin where
  ppr FromSource             = text "FromSource"
  ppr (Generated reason pmc) = text "Generated" <+> ppr reason <+> ppr pmc

-- | Whether to run pattern-match checks in generated code.
--
-- See Note [Generated code and pattern-match checking].
data DoPmc = SkipPmc
           | DoPmc
           deriving( Eq, Data )

instance Outputable DoPmc where
  ppr SkipPmc     = text "SkipPmc"
  ppr DoPmc       = text "DoPmc"

-- | Does this 'Origin' require us to run pattern-match checking,
-- or should we skip these checks?
--
-- See Note [Generated code and pattern-match checking].
requiresPMC :: Origin -> Bool
requiresPMC (Generated _ SkipPmc) = False
requiresPMC _ = True

{- Note [Generated code and pattern-match checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some parts of the compiler generate code that is then typechecked. For example:

  - the XXExprGhcRn mechanism described in Note [Rebindable syntax and XXExprGhcRn]
    in GHC.Hs.Expr,
  - the deriving mechanism.

It is usually the case that we want to avoid generating error messages that
refer to generated code. The way this is handled is that we mark certain
parts of the AST as being generated (using the Origin datatype); this is then
used to set the tcl_in_gen_code flag in TcLclEnv, as explained in
Note [Error contexts in generated code] in GHC.Tc.Utils.Monad.

Being in generated code is usually taken to mean we should also skip doing
pattern-match checking, but not always. For example, when desugaring a record
update (as described in Note [Record Updates] in GHC.Tc.Gen.Expr), we still want
to do pattern-match checking, in order to report incomplete record updates
(failing to do so lead to #23250). So, for a 'Generated' 'Origin', we keep track
of whether we should do pattern-match checks; see the calls of the requiresPMC
function (e.g. isMatchContextPmChecked and needToRunPmCheck in GHC.HsToCore.Pmc.Utils).
-}

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

topPrec, sigPrec, funPrec, opPrec, starPrec, appPrec, maxPrec :: PprPrec
topPrec  = PprPrec 0 -- No parens
sigPrec  = PprPrec 1 -- Explicit type signatures
funPrec  = PprPrec 2 -- Function args; no parens for constructor apps
                     -- See [Type operator precedence] for why both
                     -- funPrec and opPrec exist.
opPrec   = PprPrec 2 -- Infix operator
starPrec = PprPrec 3 -- Star syntax for the type of types, i.e. the * in (* -> *)
                     -- See Note [Star kind precedence]
appPrec  = PprPrec 4 -- Constructor args; no parens for atomic
maxPrec  = appPrec   -- Maximum precendence

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
  deriving( Eq, Data, Ord )

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

-- | Are we dealing with an unboxed tuple or an unboxed sum?
--
-- Used when validity checking, see 'check_ubx_tuple_or_sum'.
data UnboxedTupleOrSum
  = UnboxedTupleType
  | UnboxedSumType
  deriving Eq

instance Outputable UnboxedTupleOrSum where
  ppr UnboxedTupleType = text "UnboxedTupleType"
  ppr UnboxedSumType   = text "UnboxedSumType"

unboxedTupleOrSumExtension :: UnboxedTupleOrSum -> LangExt.Extension
unboxedTupleOrSumExtension UnboxedTupleType = LangExt.UnboxedTuples
unboxedTupleOrSumExtension UnboxedSumType   = LangExt.UnboxedSums

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

Note [OccInfo]
~~~~~~~~~~~~~
The OccInfo data type is used exclusively by the simplifier, but it appears in a
SubstResult, which is currently defined in GHC.Types.Var.Env, which is pretty
near the base of the module hierarchy.  So it seemed simpler to put the defn of
OccInfo here, safely at the bottom.

Note that `OneOcc` doesn't meant that it occurs /syntactially/ only once; it
means that it is /used/ only once. It might occur syntactically many times.
For example, in (case x of A -> y; B -> y; C -> True),
* `y` is used only once
* but it occurs syntactically twice

-}

-- | identifier Occurrence Information
data OccInfo -- See Note [OccInfo]
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
data TailCallInfo
  = AlwaysTailCalled {-# UNPACK #-} !JoinArity -- See Note [TailCallInfo]
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
analyser has *just* run. Use 'Id.idJoinPointHood' for the permanent state of
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

instance Semigroup SuccessFlag where
  Failed <> _ = Failed
  _ <> Failed = Failed
  _ <> _      = Succeeded


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
*********************************************************************
*                                                                      *
                 UnfoldingSource
*                                                                      *
********************************************************************* -}

data UnfoldingSource
  = -- See also Note [Historical note: unfoldings for wrappers]
    VanillaSrc         -- The current rhs of the function
                       -- Replace uf_tmpl each time around

  -- See Note [Stable unfoldings] in GHC.Core
  | StableUserSrc   -- From a user-specified INLINE or INLINABLE pragma
  | StableSystemSrc -- From a wrapper, or system-generated unfolding

  | CompulsorySrc   -- Something that *has* no binding, so you *must* inline it
                    -- Only a few primop-like things have this property
                    -- (see "GHC.Types.Id.Make", calls to mkCompulsoryUnfolding).
                    -- Inline absolutely always, however boring the context.

isStableUserSource :: UnfoldingSource -> Bool
isStableUserSource StableUserSrc = True
isStableUserSource _             = False

isStableSystemSource :: UnfoldingSource -> Bool
isStableSystemSource StableSystemSrc = True
isStableSystemSource _               = False

isCompulsorySource :: UnfoldingSource -> Bool
isCompulsorySource CompulsorySrc = True
isCompulsorySource _             = False

isStableSource :: UnfoldingSource -> Bool
isStableSource CompulsorySrc   = True
isStableSource StableSystemSrc = True
isStableSource StableUserSrc   = True
isStableSource VanillaSrc      = False

instance Binary UnfoldingSource where
    put_ bh CompulsorySrc   = putByte bh 0
    put_ bh StableUserSrc   = putByte bh 1
    put_ bh StableSystemSrc = putByte bh 2
    put_ bh VanillaSrc      = putByte bh 3
    get bh = do
        h <- getByte bh
        case h of
            0 -> return CompulsorySrc
            1 -> return StableUserSrc
            2 -> return StableSystemSrc
            _ -> return VanillaSrc

instance Outputable UnfoldingSource where
  ppr CompulsorySrc     = text "Compulsory"
  ppr StableUserSrc     = text "StableUser"
  ppr StableSystemSrc   = text "StableSystem"
  ppr VanillaSrc        = text "<vanilla>"

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
                 Levity and TypeOrConstraint
*                                                                      *
********************************************************************* -}

{- The types `Levity` and `TypeOrConstraint` are internal to GHC.
   They have the same shape as the eponymous types in the library
      ghc-prim:GHC.Types
   but they aren't the same types -- after all, they are defined in a
   different module.
-}

data Levity
  = Lifted
  | Unlifted
  deriving (Data,Eq,Ord,Show)

instance Outputable Levity where
  ppr Lifted   = text "Lifted"
  ppr Unlifted = text "Unlifted"

instance Binary Levity where
  put_ bh = \case
    Lifted   -> putByte bh 0
    Unlifted -> putByte bh 1
  get bh = getByte bh >>= \case
    0 -> pure Lifted
    _ -> pure Unlifted

mightBeLifted :: Maybe Levity -> Bool
mightBeLifted (Just Unlifted) = False
mightBeLifted _               = True

mightBeUnlifted :: Maybe Levity -> Bool
mightBeUnlifted (Just Lifted) = False
mightBeUnlifted _             = True

data TypeOrConstraint
  = TypeLike | ConstraintLike
  deriving( Eq, Ord, Data )


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
  It defaults 'RuntimeRep'/'Levity' variables unless -fprint-explicit-runtime-reps is enabled,
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
