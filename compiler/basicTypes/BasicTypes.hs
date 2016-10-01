{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998

\section[BasicTypes]{Miscellanous types}

This module defines a miscellaneously collection of very simple
types that

\begin{itemize}
\item have no other obvious home
\item don't depend on any other complicated types
\item are used in more than one "part" of the compiler
\end{itemize}
-}

{-# LANGUAGE DeriveDataTypeable #-}

module BasicTypes(
        Version, bumpVersion, initialVersion,

        ConTag, ConTagZ, fIRST_TAG,

        Arity, RepArity,

        Alignment,

        FunctionOrData(..),

        WarningTxt(..), StringLiteral(..),

        Fixity(..), FixityDirection(..),
        defaultFixity, maxPrecedence, minPrecedence,
        negateFixity, funTyFixity,
        compareFixity,

        RecFlag(..), isRec, isNonRec, boolToRecFlag,
        Origin(..), isGenerated,

        RuleName, pprRuleName,

        TopLevelFlag(..), isTopLevel, isNotTopLevel,

        DerivStrategy(..),

        OverlapFlag(..), OverlapMode(..), setOverlapModeMaybe,
        hasOverlappingFlag, hasOverlappableFlag, hasIncoherentFlag,

        Boxity(..), isBoxed,

        TupleSort(..), tupleSortBoxity, boxityTupleSort,
        tupleParens,

        sumParens, pprAlternative,

        -- ** The OneShotInfo type
        OneShotInfo(..),
        noOneShotInfo, hasNoOneShotInfo, isOneShotInfo,
        bestOneShot, worstOneShot,

        OccInfo(..), seqOccInfo, zapFragileOcc, isOneOcc,
        isDeadOcc, isStrongLoopBreaker, isWeakLoopBreaker, isNoOcc,
        strongLoopBreaker, weakLoopBreaker,

        InsideLam, insideLam, notInsideLam,
        OneBranch, oneBranch, notOneBranch,
        InterestingCxt,

        EP(..),

        DefMethSpec(..),
        SwapFlag(..), flipSwap, unSwap, isSwapped,

        CompilerPhase(..), PhaseNum,

        Activation(..), isActive, isActiveIn, competesWith,
        isNeverActive, isAlwaysActive, isEarlyActive,

        RuleMatchInfo(..), isConLike, isFunLike,
        InlineSpec(..), isEmptyInlineSpec,
        InlinePragma(..), defaultInlinePragma, alwaysInlinePragma,
        neverInlinePragma, dfunInlinePragma,
        isDefaultInlinePragma,
        isInlinePragma, isInlinablePragma, isAnyInlinePragma,
        inlinePragmaSpec, inlinePragmaSat,
        inlinePragmaActivation, inlinePragmaRuleMatchInfo,
        setInlinePragmaActivation, setInlinePragmaRuleMatchInfo,

        SuccessFlag(..), succeeded, failed, successIf,

        FractionalLit(..), negateFractionalLit, integralFractionalLit,

        SourceText,

        IntWithInf, infinity, treatZeroAsInf, mkIntWithInf, intGtLimit
   ) where

import FastString
import Outputable
import SrcLoc ( Located,unLoc )
import StaticFlags( opt_PprStyle_Debug )
import Data.Data hiding (Fixity)
import Data.Function (on)

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
-- See also Note [Definition of arity] in CoreArity
type Arity = Int

-- | Representation Arity
--
-- The number of represented arguments that can be applied to a value before it does
-- "real work". So:
--  fib 100                    has representation arity 0
--  \x -> fib x                has representation arity 1
--  \(# x, y #) -> fib (x + y) has representation arity 2
type RepArity = Int

{-
************************************************************************
*                                                                      *
              Constructor tags
*                                                                      *
************************************************************************
-}

-- | Constructor Tag
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

type Alignment = Int -- align to next N-byte boundary (N must be a power of 2).

{-
************************************************************************
*                                                                      *
         One-shot information
*                                                                      *
************************************************************************
-}

-- | If the 'Id' is a lambda-bound variable then it may have lambda-bound
-- variable info. Sometimes we know whether the lambda binding this variable
-- is a \"one-shot\" lambda; that is, whether it is applied at most once.
--
-- This information may be useful in optimisation, as computations may
-- safely be floated inside such a lambda without risk of duplicating
-- work.
data OneShotInfo
  = NoOneShotInfo -- ^ No information
  | ProbOneShot   -- ^ The lambda is probably applied at most once
                  -- See Note [Computing one-shot info, and ProbOneShot] in Demand
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
worstOneShot ProbOneShot   NoOneShotInfo = NoOneShotInfo
worstOneShot ProbOneShot   _             = ProbOneShot
worstOneShot OneShotLam    os            = os

bestOneShot NoOneShotInfo os         = os
bestOneShot ProbOneShot   OneShotLam = OneShotLam
bestOneShot ProbOneShot   _          = ProbOneShot
bestOneShot OneShotLam    _          = OneShotLam

pprOneShotInfo :: OneShotInfo -> SDoc
pprOneShotInfo NoOneShotInfo = empty
pprOneShotInfo ProbOneShot   = text "ProbOneShot"
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

{-
************************************************************************
*                                                                      *
\subsection[Version]{Module and identifier version numbers}
*                                                                      *
************************************************************************
-}

type Version = Int

bumpVersion :: Version -> Version
bumpVersion v = v+1

initialVersion :: Version
initialVersion = 1

{-
************************************************************************
*                                                                      *
                Deprecations
*                                                                      *
************************************************************************
-}

-- | A String Literal in the source, including its original raw format for use by
-- source to source manipulation tools.
data StringLiteral = StringLiteral
                       { sl_st :: SourceText, -- literal raw source.
                                              -- See not [Literal source text]
                         sl_fs :: FastString  -- literal string value
                       } deriving Data

instance Eq StringLiteral where
  (StringLiteral _ a) == (StringLiteral _ b) = a == b

-- | Warning Text
--
-- reason/explanation from a WARNING or DEPRECATED pragma
data WarningTxt = WarningTxt (Located SourceText)
                             [Located StringLiteral]
                | DeprecatedTxt (Located SourceText)
                                [Located StringLiteral]
    deriving (Eq, Data)

instance Outputable WarningTxt where
    ppr (WarningTxt    _ ws)
                         = doubleQuotes (vcat (map (ftext . sl_fs . unLoc) ws))
    ppr (DeprecatedTxt _ ds)
                         = text "Deprecated:" <+>
                           doubleQuotes (vcat (map (ftext . sl_fs . unLoc) ds))

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
\subsection[Fixity]{Fixity info}
*                                                                      *
************************************************************************
-}

------------------------
data Fixity = Fixity SourceText Int FixityDirection
  -- Note [Pragma source text]
  deriving Data

instance Outputable Fixity where
    ppr (Fixity _ prec dir) = hcat [ppr dir, space, int prec]

instance Eq Fixity where -- Used to determine if two fixities conflict
  (Fixity _ p1 dir1) == (Fixity _ p2 dir2) = p1==p2 && dir1 == dir2

------------------------
data FixityDirection = InfixL | InfixR | InfixN
                     deriving (Eq, Data)

instance Outputable FixityDirection where
    ppr InfixL = text "infixl"
    ppr InfixR = text "infixr"
    ppr InfixN = text "infix"

------------------------
maxPrecedence, minPrecedence :: Int
maxPrecedence = 9
minPrecedence = 0

defaultFixity :: Fixity
defaultFixity = Fixity (show maxPrecedence) maxPrecedence InfixL

negateFixity, funTyFixity :: Fixity
-- Wired-in fixities
negateFixity = Fixity "6" 6 InfixL  -- Fixity of unary negate
funTyFixity  = Fixity "0" 0 InfixR  -- Fixity of '->'

{-
Consider

\begin{verbatim}
        a `op1` b `op2` c
\end{verbatim}
@(compareFixity op1 op2)@ tells which way to arrange appication, or
whether there's an error.
-}

compareFixity :: Fixity -> Fixity
              -> (Bool,         -- Error please
                  Bool)         -- Associate to the right: a op1 (b op2 c)
compareFixity (Fixity _ prec1 dir1) (Fixity _ prec2 dir2)
  = case prec1 `compare` prec2 of
        GT -> left
        LT -> right
        EQ -> case (dir1, dir2) of
                        (InfixR, InfixR) -> right
                        (InfixL, InfixL) -> left
                        _                -> error_please
  where
    right        = (False, True)
    left         = (False, False)
    error_please = (True,  False)

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
                Deriving strategies
*                                                                      *
************************************************************************
-}

-- | Which technique the user explicitly requested when deriving an instance.
data DerivStrategy
  -- See Note [Deriving strategies] in TcDeriv
  = DerivStock    -- ^ GHC's \"standard\" strategy, which is to implement a
                  --   custom instance for the data type. This only works for
                  --   certain types that GHC knows about (e.g., 'Eq', 'Show',
                  --   'Functor' when @-XDeriveFunctor@ is enabled, etc.)
  | DerivAnyclass -- ^ @-XDeriveAnyClass@
  | DerivNewtype  -- ^ @-XGeneralizedNewtypeDeriving@
  deriving (Eq, Data)

instance Outputable DerivStrategy where
    ppr DerivStock    = text "stock"
    ppr DerivAnyclass = text "anyclass"
    ppr DerivNewtype  = text "newtype"

{-
************************************************************************
*                                                                      *
                Instance overlap flag
*                                                                      *
************************************************************************
-}

-- | The semantics allowed for overlapping instances for a particular
-- instance. See Note [Safe Haskell isSafeOverlap] (in `InstEnv.hs`) for a
-- explanation of the `isSafeOverlap` field.
--
-- - 'ApiAnnotation.AnnKeywordId' :
--      'ApiAnnotation.AnnOpen' @'\{-\# OVERLAPPABLE'@ or
--                              @'\{-\# OVERLAPPING'@ or
--                              @'\{-\# OVERLAPS'@ or
--                              @'\{-\# INCOHERENT'@,
--      'ApiAnnotation.AnnClose' @`\#-\}`@,

-- For details on above see note [Api annotations] in ApiAnnotation
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

data OverlapMode  -- See Note [Rules for instance lookup] in InstEnv
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
    -- an an arbitrary one if there are multiple matching candidates, and
    -- don't worry about later instantiation
    --
    -- Example: constraint (Foo [b])
    -- instance {-# INCOHERENT -} Foo [Int]
    -- instance                   Foo [a]
    -- Without the Incoherent flag, we'd complain that
    -- instantiating 'b' would change which instance
    -- was chosen. See also note [Incoherent instances] in InstEnv

  deriving (Eq, Data)


instance Outputable OverlapFlag where
   ppr flag = ppr (overlapMode flag) <+> pprSafeOverlap (isSafeOverlap flag)

instance Outputable OverlapMode where
   ppr (NoOverlap    _) = empty
   ppr (Overlappable _) = text "[overlappable]"
   ppr (Overlapping  _) = text "[overlapping]"
   ppr (Overlaps     _) = text "[overlap ok]"
   ppr (Incoherent   _) = text "[incoherent]"

pprSafeOverlap :: Bool -> SDoc
pprSafeOverlap True  = text "[safe]"
pprSafeOverlap False = empty

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

tupleSortBoxity :: TupleSort -> Boxity
tupleSortBoxity BoxedTuple      = Boxed
tupleSortBoxity UnboxedTuple    = Unboxed
tupleSortBoxity ConstraintTuple = Boxed

boxityTupleSort :: Boxity -> TupleSort
boxityTupleSort Boxed   = BoxedTuple
boxityTupleSort Unboxed = UnboxedTuple

tupleParens :: TupleSort -> SDoc -> SDoc
tupleParens BoxedTuple      p = parens p
tupleParens UnboxedTuple    p = text "(#" <+> p <+> ptext (sLit "#)")
tupleParens ConstraintTuple p   -- In debug-style write (% Eq a, Ord b %)
  | opt_PprStyle_Debug        = text "(%" <+> p <+> ptext (sLit "%)")
  | otherwise                 = parens p

{-
************************************************************************
*                                                                      *
                Sums
*                                                                      *
************************************************************************
-}

sumParens :: SDoc -> SDoc
sumParens p = ptext (sLit "(#") <+> p <+> ptext (sLit "#)")

-- | Pretty print an alternative in an unboxed sum e.g. "| a | |".
pprAlternative :: (a -> SDoc) -- ^ The pretty printing function to use
               -> a           -- ^ The things to be pretty printed
               -> ConTag      -- ^ Alternative (one-based)
               -> Arity       -- ^ Arity
               -> SDoc        -- ^ 'SDoc' where the alternative havs been pretty
                              -- printed and finally packed into a paragraph.
pprAlternative pp x alt arity =
    fsep (replicate (alt - 1) vbar ++ [pp x] ++ replicate (arity - alt - 1) vbar)

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
SubstResult, which is currently defined in VarEnv, which is pretty near
the base of the module hierarchy.  So it seemed simpler to put the
defn of OccInfo here, safely at the bottom
-}

-- | identifier Occurrence Information
data OccInfo
  = NoOccInfo           -- ^ There are many occurrences, or unknown occurrences

  | IAmDead             -- ^ Marks unused variables.  Sometimes useful for
                        -- lambda and case-bound variables.

  | OneOcc
        !InsideLam
        !OneBranch
        !InterestingCxt -- ^ Occurs exactly once, not inside a rule

  -- | This identifier breaks a loop of mutually recursive functions. The field
  -- marks whether it is only a loop breaker due to a reference in a rule
  | IAmALoopBreaker     -- Note [LoopBreaker OccInfo]
        !RulesOnly

  deriving (Eq)

type RulesOnly = Bool

{-
Note [LoopBreaker OccInfo]
~~~~~~~~~~~~~~~~~~~~~~~~~~
   IAmALoopBreaker True  <=> A "weak" or rules-only loop breaker
                             Do not preInlineUnconditionally

   IAmALoopBreaker False <=> A "strong" loop breaker
                             Do not inline at all

See OccurAnal Note [Weak loop breakers]
-}

isNoOcc :: OccInfo -> Bool
isNoOcc NoOccInfo = True
isNoOcc _         = False

seqOccInfo :: OccInfo -> ()
seqOccInfo occ = occ `seq` ()

-----------------
-- | Interesting Context
type InterestingCxt = Bool      -- True <=> Function: is applied
                                --          Data value: scrutinised by a case with
                                --                      at least one non-DEFAULT branch

-----------------
-- | Inside Lambda
type InsideLam = Bool   -- True <=> Occurs inside a non-linear lambda
                        -- Substituting a redex for this occurrence is
                        -- dangerous because it might duplicate work.
insideLam, notInsideLam :: InsideLam
insideLam    = True
notInsideLam = False

-----------------
type OneBranch = Bool   -- True <=> Occurs in only one case branch
                        --      so no code-duplication issue to worry about
oneBranch, notOneBranch :: OneBranch
oneBranch    = True
notOneBranch = False

strongLoopBreaker, weakLoopBreaker :: OccInfo
strongLoopBreaker = IAmALoopBreaker False
weakLoopBreaker   = IAmALoopBreaker True

isWeakLoopBreaker :: OccInfo -> Bool
isWeakLoopBreaker (IAmALoopBreaker _) = True
isWeakLoopBreaker _                   = False

isStrongLoopBreaker :: OccInfo -> Bool
isStrongLoopBreaker (IAmALoopBreaker False) = True   -- Loop-breaker that breaks a non-rule cycle
isStrongLoopBreaker _                       = False

isDeadOcc :: OccInfo -> Bool
isDeadOcc IAmDead = True
isDeadOcc _       = False

isOneOcc :: OccInfo -> Bool
isOneOcc (OneOcc {}) = True
isOneOcc _           = False

zapFragileOcc :: OccInfo -> OccInfo
zapFragileOcc (OneOcc {}) = NoOccInfo
zapFragileOcc occ         = occ

instance Outputable OccInfo where
  -- only used for debugging; never parsed.  KSW 1999-07
  ppr NoOccInfo            = empty
  ppr (IAmALoopBreaker ro) = text "LoopBreaker" <> if ro then char '!' else empty
  ppr IAmDead              = text "Dead"
  ppr (OneOcc inside_lam one_branch int_cxt)
        = text "Once" <> pp_lam <> pp_br <> pp_args
        where
          pp_lam | inside_lam = char 'L'
                 | otherwise  = empty
          pp_br  | one_branch = empty
                 | otherwise  = char '*'
          pp_args | int_cxt   = char '!'
                  | otherwise = empty

{-
************************************************************************
*                                                                      *
                Default method specfication
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
\subsection{Source Text}
*                                                                      *
************************************************************************
Keeping Source Text for source to source conversions

Note [Pragma source text]
~~~~~~~~~~~~~~~~~~~~~~~~~
The lexer does a case-insensitive match for pragmas, as well as
accepting both UK and US spelling variants.

So

  {-# SPECIALISE #-}
  {-# SPECIALIZE #-}
  {-# Specialize #-}

will all generate ITspec_prag token for the start of the pragma.

In order to be able to do source to source conversions, the original
source text for the token needs to be preserved, hence the
`SourceText` field.

So the lexer will then generate

  ITspec_prag "{ -# SPECIALISE"
  ITspec_prag "{ -# SPECIALIZE"
  ITspec_prag "{ -# Specialize"

for the cases above.
 [without the space between '{' and '-', otherwise this comment won't parse]


Note [Literal source text]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The lexer/parser converts literals from their original source text
versions to an appropriate internal representation. This is a problem
for tools doing source to source conversions, so the original source
text is stored in literals where this can occur.

Motivating examples for HsLit

  HsChar          '\n'       == '\x20`
  HsCharPrim      '\x41`#    == `A`
  HsString        "\x20\x41" == " A"
  HsStringPrim    "\x20"#    == " "#
  HsInt           001        == 1
  HsIntPrim       002#       == 2#
  HsWordPrim      003##      == 3##
  HsInt64Prim     004##      == 4##
  HsWord64Prim    005##      == 5##
  HsInteger       006        == 6

For OverLitVal

  HsIntegral      003      == 0x003
  HsIsString      "\x41nd" == "And"
-}

type SourceText = String -- Note [Literal source text],[Pragma source text]


{-
************************************************************************
*                                                                      *
\subsection{Activation}
*                                                                      *
************************************************************************

When a rule or inlining is active
-}

-- | Phase Number
type PhaseNum = Int  -- Compilation phase
                     -- Phases decrease towards zero
                     -- Zero is the last phase

data CompilerPhase
  = Phase PhaseNum
  | InitialPhase    -- The first phase -- number = infinity!

instance Outputable CompilerPhase where
   ppr (Phase n)    = int n
   ppr InitialPhase = text "InitialPhase"

-- See note [Pragma source text]
data Activation = NeverActive
                | AlwaysActive
                | ActiveBefore SourceText PhaseNum
                  -- Active only *strictly before* this phase
                | ActiveAfter SourceText PhaseNum
                  -- Active in this phase and later
                deriving( Eq, Data )
                  -- Eq used in comparing rules in HsDecls

-- | Rule Match Information
data RuleMatchInfo = ConLike                    -- See Note [CONLIKE pragma]
                   | FunLike
                   deriving( Eq, Data, Show )
        -- Show needed for Lexer.x

data InlinePragma            -- Note [InlinePragma]
  = InlinePragma
      { inl_src    :: SourceText -- Note [Pragma source text]
      , inl_inline :: InlineSpec

      , inl_sat    :: Maybe Arity    -- Just n <=> Inline only when applied to n
                                     --            explicit (non-type, non-dictionary) args
                                     --   That is, inl_sat describes the number of *source-code*
                                     --   arguments the thing must be applied to.  We add on the
                                     --   number of implicit, dictionary arguments when making
                                     --   the InlineRule, and don't look at inl_sat further

      , inl_act    :: Activation     -- Says during which phases inlining is allowed

      , inl_rule   :: RuleMatchInfo  -- Should the function be treated like a constructor?
    } deriving( Eq, Data )

-- | Inline Specification
data InlineSpec   -- What the user's INLINE pragma looked like
  = Inline
  | Inlinable
  | NoInline
  | EmptyInlineSpec  -- Used in a place-holder InlinePragma in SpecPrag or IdInfo,
                     -- where there isn't any real inline pragma at all
  deriving( Eq, Data, Show )
        -- Show needed for Lexer.x

{-
Note [InlinePragma]
~~~~~~~~~~~~~~~~~~~
This data type mirrors what you can write in an INLINE or NOINLINE pragma in
the source program.

If you write nothing at all, you get defaultInlinePragma:
   inl_inline = EmptyInlineSpec
   inl_act    = AlwaysActive
   inl_rule   = FunLike

It's not possible to get that combination by *writing* something, so
if an Id has defaultInlinePragma it means the user didn't specify anything.

If inl_inline = Inline or Inlineable, then the Id should have an InlineRule unfolding.

If you want to know where InlinePragmas take effect: Look in DsBinds.makeCorePair

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

    - New function coreUtils.exprIsExpandable is like exprIsCheap, but
      additionally spots applications of CONLIKE functions

    - A CoreUnfolding has a field that caches exprIsExpandable

    - The rule matcher consults this field.  See
      Note [Expanding variables] in Rules.hs.
-}

isConLike :: RuleMatchInfo -> Bool
isConLike ConLike = True
isConLike _       = False

isFunLike :: RuleMatchInfo -> Bool
isFunLike FunLike = True
isFunLike _       = False

isEmptyInlineSpec :: InlineSpec -> Bool
isEmptyInlineSpec EmptyInlineSpec = True
isEmptyInlineSpec _               = False

defaultInlinePragma, alwaysInlinePragma, neverInlinePragma, dfunInlinePragma
  :: InlinePragma
defaultInlinePragma = InlinePragma { inl_src = "{-# INLINE"
                                   , inl_act = AlwaysActive
                                   , inl_rule = FunLike
                                   , inl_inline = EmptyInlineSpec
                                   , inl_sat = Nothing }

alwaysInlinePragma = defaultInlinePragma { inl_inline = Inline }
neverInlinePragma  = defaultInlinePragma { inl_act    = NeverActive }

inlinePragmaSpec :: InlinePragma -> InlineSpec
inlinePragmaSpec = inl_inline

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
  = isEmptyInlineSpec inline && isAlwaysActive activation && isFunLike match_info

isInlinePragma :: InlinePragma -> Bool
isInlinePragma prag = case inl_inline prag of
                        Inline -> True
                        _      -> False

isInlinablePragma :: InlinePragma -> Bool
isInlinablePragma prag = case inl_inline prag of
                           Inlinable -> True
                           _         -> False

isAnyInlinePragma :: InlinePragma -> Bool
-- INLINE or INLINABLE
isAnyInlinePragma prag = case inl_inline prag of
                        Inline    -> True
                        Inlinable -> True
                        _         -> False

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
   ppr AlwaysActive       = brackets (text "ALWAYS")
   ppr NeverActive        = brackets (text "NEVER")
   ppr (ActiveBefore _ n) = brackets (char '~' <> int n)
   ppr (ActiveAfter  _ n) = brackets (int n)

instance Outputable RuleMatchInfo where
   ppr ConLike = text "CONLIKE"
   ppr FunLike = text "FUNLIKE"

instance Outputable InlineSpec where
   ppr Inline          = text "INLINE"
   ppr NoInline        = text "NOINLINE"
   ppr Inlinable       = text "INLINABLE"
   ppr EmptyInlineSpec = empty

instance Outputable InlinePragma where
  ppr (InlinePragma { inl_inline = inline, inl_act = activation
                    , inl_rule = info, inl_sat = mb_arity })
    = ppr inline <> pp_act inline activation <+> pp_sat <+> pp_info
    where
      pp_act Inline   AlwaysActive = empty
      pp_act NoInline NeverActive  = empty
      pp_act _        act          = ppr act

      pp_sat | Just ar <- mb_arity = parens (text "sat-args=" <> int ar)
             | otherwise           = empty
      pp_info | isFunLike info = empty
              | otherwise      = ppr info

isActive :: CompilerPhase -> Activation -> Bool
isActive InitialPhase AlwaysActive      = True
isActive InitialPhase (ActiveBefore {}) = True
isActive InitialPhase _                 = False
isActive (Phase p)    act               = isActiveIn p act

isActiveIn :: PhaseNum -> Activation -> Bool
isActiveIn _ NeverActive        = False
isActiveIn _ AlwaysActive       = True
isActiveIn p (ActiveAfter _ n)  = p <= n
isActiveIn p (ActiveBefore _ n) = p >  n

competesWith :: Activation -> Activation -> Bool
-- See Note [Activation competition]
competesWith NeverActive       _                = False
competesWith _                 NeverActive      = False
competesWith AlwaysActive      _                = True

competesWith (ActiveBefore {})  AlwaysActive      = True
competesWith (ActiveBefore {})  (ActiveBefore {}) = True
competesWith (ActiveBefore _ a) (ActiveAfter _ b) = a < b

competesWith (ActiveAfter {})  AlwaysActive      = False
competesWith (ActiveAfter {})  (ActiveBefore {}) = False
competesWith (ActiveAfter _ a) (ActiveAfter _ b) = a >= b

{- Note [Competing activations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes a RULE and an inlining may compete, or two RULES.
See Note [Rules and inlining/other rules] in Desugar.

We say that act1 "competes with" act2 iff
   act1 is active in the phase when act2 *becomes* active
NB: remember that phases count *down*: 2, 1, 0!

It's too conservative to ensure that the two are never simultaneously
active.  For example, a rule might be always active, and an inlining
might switch on in phase 2.  We could switch off the rule, but it does
no harm.
-}

isNeverActive, isAlwaysActive, isEarlyActive :: Activation -> Bool
isNeverActive NeverActive = True
isNeverActive _           = False

isAlwaysActive AlwaysActive = True
isAlwaysActive _            = False

isEarlyActive AlwaysActive      = True
isEarlyActive (ActiveBefore {}) = True
isEarlyActive _                 = False

-- | Fractional Literal
--
-- Used (instead of Rational) to represent exactly the floating point literal that we
-- encountered in the user's source program. This allows us to pretty-print exactly what
-- the user wrote, which is important e.g. for floating point numbers that can't represented
-- as Doubles (we used to via Double for pretty-printing). See also #2245.
data FractionalLit
  = FL { fl_text :: String         -- How the value was written in the source
       , fl_value :: Rational      -- Numeric value of the literal
       }
  deriving (Data, Show)
  -- The Show instance is required for the derived Lexer.x:Token instance when DEBUG is on

negateFractionalLit :: FractionalLit -> FractionalLit
negateFractionalLit (FL { fl_text = '-':text, fl_value = value }) = FL { fl_text = text, fl_value = negate value }
negateFractionalLit (FL { fl_text = text, fl_value = value }) = FL { fl_text = '-':text, fl_value = negate value }

integralFractionalLit :: Integer -> FractionalLit
integralFractionalLit i = FL { fl_text = show i, fl_value = fromInteger i }

-- Comparison operations are needed when grouping literals
-- for compiling pattern-matching (module MatchLit)

instance Eq FractionalLit where
  (==) = (==) `on` fl_value

instance Ord FractionalLit where
  compare = compare `on` fl_value

instance Outputable FractionalLit where
  ppr = text . fl_text

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

-- | Turn a positive number into an 'IntWithInf', where 0 represents infinity
treatZeroAsInf :: Int -> IntWithInf
treatZeroAsInf 0 = Infinity
treatZeroAsInf n = Int n

-- | Inject any integer into an 'IntWithInf'
mkIntWithInf :: Int -> IntWithInf
mkIntWithInf = Int
