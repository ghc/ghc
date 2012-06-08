%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[ConFold]{Constant Folder}

Conceptually, constant folding should be parameterized with the kind
of target machine to get identical behaviour during compilation time
and runtime. We cheat a little bit here...

ToDo:
   check boundaries before folding, e.g. we can fold the Float addition
   (i1 + i2) only if it results in a valid Float.

\begin{code}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module PrelRules ( primOpRules, builtinRules ) where

#include "HsVersions.h"

import CoreSyn
import MkCore
import Id
import Literal
import CoreSubst   ( exprIsLiteral_maybe )
import PrimOp      ( PrimOp(..), tagToEnumKey )
import TysWiredIn
import TysPrim
import TyCon       ( tyConDataCons_maybe, isEnumerationTyCon, isNewTyCon )
import DataCon     ( dataConTag, dataConTyCon, dataConWorkId, fIRST_TAG )
import CoreUtils   ( cheapEqExpr, exprIsHNF )
import CoreUnfold  ( exprIsConApp_maybe )
import Type
import TypeRep
import OccName     ( occNameFS )
import PrelNames
import Maybes      ( orElse )
import Name        ( Name, nameOccName )
import Outputable
import FastString
import StaticFlags ( opt_SimplExcessPrecision )
import Constants
import BasicTypes
import Util

import Data.Bits as Bits
import Data.Int    ( Int64 )
import Data.Word   ( Word, Word64 )
\end{code}


Note [Constant folding]
~~~~~~~~~~~~~~~~~~~~~~~
primOpRules generates the rewrite rules for each primop
These rules do what is often called "constant folding"
E.g. the rules for +# might say
        4 +# 5 = 9
Well, of course you'd need a lot of rules if you did it
like that, so we use a BuiltinRule instead, so that we
can match in any two literal values.  So the rule is really
more like
        (Lit x) +# (Lit y) = Lit (x+#y)
where the (+#) on the rhs is done at compile time

That is why these rules are built in here.  Other rules
which don't need to be built in are in GHC.Base. For
example:
        x +# 0 = x


\begin{code}
primOpRules :: PrimOp -> Name -> [CoreRule]
primOpRules op op_name = primop_rule op
  where
    -- A useful shorthand
    one_lit   = oneLit  op_name
    two_lits  = twoLits op_name
    relop cmp = two_lits (cmpOp (\ord -> ord `cmp` EQ))
    -- Cunning.  cmpOp compares the values to give an Ordering.
    -- It applies its argument to that ordering value to turn
    -- the ordering into a boolean value.  (`cmp` EQ) is just the job.

    -- ToDo: something for integer-shift ops?
    --       NotOp

    primop_rule TagToEnumOp = mkBasicRule op_name 2 tagToEnumRule
    primop_rule DataToTagOp = mkBasicRule op_name 2 dataToTagRule

    -- Int operations
    primop_rule IntAddOp    = two_lits (intOp2     (+))
    primop_rule IntSubOp    = two_lits (intOp2     (-))
    primop_rule IntMulOp    = two_lits (intOp2     (*))
    primop_rule IntQuotOp   = two_lits (intOp2Z    quot)
    primop_rule IntRemOp    = two_lits (intOp2Z    rem)
    primop_rule IntNegOp    = one_lit  negOp
    primop_rule ISllOp      = two_lits (intShiftOp2 Bits.shiftL)
    primop_rule ISraOp      = two_lits (intShiftOp2 Bits.shiftR)
    primop_rule ISrlOp      = two_lits (intShiftOp2 shiftRightLogical)

    -- Word operations
    primop_rule WordAddOp   = two_lits (wordOp2    (+))
    primop_rule WordSubOp   = two_lits (wordOp2    (-))
    primop_rule WordMulOp   = two_lits (wordOp2    (*))
    primop_rule WordQuotOp  = two_lits (wordOp2Z   quot)
    primop_rule WordRemOp   = two_lits (wordOp2Z   rem)
    primop_rule AndOp       = two_lits (wordBitOp2 (.&.))
    primop_rule OrOp        = two_lits (wordBitOp2 (.|.))
    primop_rule XorOp       = two_lits (wordBitOp2 xor)
    primop_rule SllOp       = two_lits (wordShiftOp2 Bits.shiftL)
    primop_rule SrlOp       = two_lits (wordShiftOp2 shiftRightLogical)

    -- coercions
    primop_rule Word2IntOp     = one_lit (litCoerce word2IntLit)
    primop_rule Int2WordOp     = one_lit (litCoerce int2WordLit)
    primop_rule Narrow8IntOp   = one_lit (litCoerce narrow8IntLit)
    primop_rule Narrow16IntOp  = one_lit (litCoerce narrow16IntLit)
    primop_rule Narrow32IntOp  = one_lit (litCoerce narrow32IntLit)
    primop_rule Narrow8WordOp  = one_lit (litCoerce narrow8WordLit)
    primop_rule Narrow16WordOp = one_lit (litCoerce narrow16WordLit)
    primop_rule Narrow32WordOp = one_lit (litCoerce narrow32WordLit)
    primop_rule OrdOp          = one_lit (litCoerce char2IntLit)
    primop_rule ChrOp          = one_lit (predLitCoerce litFitsInChar int2CharLit)
    primop_rule Float2IntOp    = one_lit (litCoerce float2IntLit)
    primop_rule Int2FloatOp    = one_lit (litCoerce int2FloatLit)
    primop_rule Double2IntOp   = one_lit (litCoerce double2IntLit)
    primop_rule Int2DoubleOp   = one_lit (litCoerce int2DoubleLit)
    -- SUP: Not sure what the standard says about precision in the following 2 cases
    primop_rule Float2DoubleOp = one_lit (litCoerce float2DoubleLit)
    primop_rule Double2FloatOp = one_lit (litCoerce double2FloatLit)

    -- Float
    primop_rule FloatAddOp   = two_lits (floatOp2  (+))
    primop_rule FloatSubOp   = two_lits (floatOp2  (-))
    primop_rule FloatMulOp   = two_lits (floatOp2  (*))
    primop_rule FloatDivOp   = two_lits (floatOp2Z (/))
    primop_rule FloatNegOp   = one_lit  negOp

    -- Double
    primop_rule DoubleAddOp   = two_lits (doubleOp2  (+))
    primop_rule DoubleSubOp   = two_lits (doubleOp2  (-))
    primop_rule DoubleMulOp   = two_lits (doubleOp2  (*))
    primop_rule DoubleDivOp   = two_lits (doubleOp2Z (/))
    primop_rule DoubleNegOp   = one_lit  negOp

    -- Relational operators
    primop_rule IntEqOp    = relop (==) ++ litEq op_name True
    primop_rule IntNeOp    = relop (/=) ++ litEq op_name False
    primop_rule CharEqOp   = relop (==) ++ litEq op_name True
    primop_rule CharNeOp   = relop (/=) ++ litEq op_name False

    primop_rule IntGtOp    = relop (>)  ++ boundsCmp op_name Gt
    primop_rule IntGeOp    = relop (>=) ++ boundsCmp op_name Ge
    primop_rule IntLeOp    = relop (<=) ++ boundsCmp op_name Le
    primop_rule IntLtOp    = relop (<)  ++ boundsCmp op_name Lt

    primop_rule CharGtOp   = relop (>)  ++ boundsCmp op_name Gt
    primop_rule CharGeOp   = relop (>=) ++ boundsCmp op_name Ge
    primop_rule CharLeOp   = relop (<=) ++ boundsCmp op_name Le
    primop_rule CharLtOp   = relop (<)  ++ boundsCmp op_name Lt

    primop_rule FloatGtOp  = relop (>)
    primop_rule FloatGeOp  = relop (>=)
    primop_rule FloatLeOp  = relop (<=)
    primop_rule FloatLtOp  = relop (<)
    primop_rule FloatEqOp  = relop (==)
    primop_rule FloatNeOp  = relop (/=)

    primop_rule DoubleGtOp = relop (>)
    primop_rule DoubleGeOp = relop (>=)
    primop_rule DoubleLeOp = relop (<=)
    primop_rule DoubleLtOp = relop (<)
    primop_rule DoubleEqOp = relop (==)
    primop_rule DoubleNeOp = relop (/=)

    primop_rule WordGtOp   = relop (>)  ++ boundsCmp op_name Gt
    primop_rule WordGeOp   = relop (>=) ++ boundsCmp op_name Ge
    primop_rule WordLeOp   = relop (<=) ++ boundsCmp op_name Le
    primop_rule WordLtOp   = relop (<)  ++ boundsCmp op_name Lt
    primop_rule WordEqOp   = relop (==)
    primop_rule WordNeOp   = relop (/=)

    primop_rule SeqOp      = mkBasicRule op_name 4 seqRule
    primop_rule SparkOp    = mkBasicRule op_name 4 sparkRule

    primop_rule _          = []
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Doing the business}
%*                                                                      *
%************************************************************************

ToDo: the reason these all return Nothing is because there used to be
the possibility of an argument being a litlit.  Litlits are now gone,
so this could be cleaned up.

\begin{code}
--------------------------
litCoerce :: (Literal -> Literal) -> Literal -> Maybe CoreExpr
litCoerce fn lit = Just (Lit (fn lit))

predLitCoerce :: (Literal -> Bool) -> (Literal -> Literal) -> Literal -> Maybe CoreExpr
predLitCoerce p fn lit
   | p lit     = Just (Lit (fn lit))
   | otherwise = Nothing

--------------------------
cmpOp :: (Ordering -> Bool) -> Literal -> Literal -> Maybe CoreExpr
cmpOp cmp l1 l2
  = go l1 l2
  where
    done res | cmp res   = Just trueVal
             | otherwise = Just falseVal

    -- These compares are at different types
    go (MachChar i1)   (MachChar i2)   = done (i1 `compare` i2)
    go (MachInt i1)    (MachInt i2)    = done (i1 `compare` i2)
    go (MachInt64 i1)  (MachInt64 i2)  = done (i1 `compare` i2)
    go (MachWord i1)   (MachWord i2)   = done (i1 `compare` i2)
    go (MachWord64 i1) (MachWord64 i2) = done (i1 `compare` i2)
    go (MachFloat i1)  (MachFloat i2)  = done (i1 `compare` i2)
    go (MachDouble i1) (MachDouble i2) = done (i1 `compare` i2)
    go _               _               = Nothing

--------------------------

negOp :: Literal -> Maybe CoreExpr  -- Negate
negOp (MachFloat 0.0)  = Nothing  -- can't represent -0.0 as a Rational
negOp (MachFloat f)    = Just (mkFloatVal (-f))
negOp (MachDouble 0.0) = Nothing
negOp (MachDouble d)   = Just (mkDoubleVal (-d))
negOp (MachInt i)      = intResult (-i)
negOp _                = Nothing

--------------------------
intOp2 :: (Integer->Integer->Integer) -> Literal -> Literal -> Maybe CoreExpr
intOp2 op (MachInt i1) (MachInt i2) = intResult (i1 `op` i2)
intOp2 _  _            _            = Nothing  -- Could find LitLit

intOp2Z :: (Integer->Integer->Integer) -> Literal -> Literal -> Maybe CoreExpr
-- Like intOp2, but Nothing if i2=0
intOp2Z op (MachInt i1) (MachInt i2)
  | i2 /= 0 = intResult (i1 `op` i2)
intOp2Z _ _ _ = Nothing  -- LitLit or zero dividend

intShiftOp2 :: (Integer->Int->Integer) -> Literal -> Literal -> Maybe CoreExpr
-- Shifts take an Int; hence second arg of op is Int
intShiftOp2 op (MachInt i1) (MachInt i2) = intResult (i1 `op` fromInteger i2)
intShiftOp2 _  _            _            = Nothing

shiftRightLogical :: Integer -> Int -> Integer
-- Shift right, putting zeros in rather than sign-propagating as Bits.shiftR would do
-- Do this by converting to Word and back.  Obviously this won't work for big
-- values, but its ok as we use it here
shiftRightLogical x n = fromIntegral (fromInteger x `shiftR` n :: Word)


--------------------------
wordOp2 :: (Integer->Integer->Integer) -> Literal -> Literal -> Maybe CoreExpr
wordOp2 op (MachWord w1) (MachWord w2)
  = wordResult (w1 `op` w2)
wordOp2 _ _ _ = Nothing  -- Could find LitLit

wordOp2Z :: (Integer->Integer->Integer) -> Literal -> Literal -> Maybe CoreExpr
wordOp2Z op (MachWord w1) (MachWord w2)
  | w2 /= 0 = wordResult (w1 `op` w2)
wordOp2Z _ _ _ = Nothing  -- LitLit or zero dividend

wordBitOp2 :: (Integer->Integer->Integer) -> Literal -> Literal
           -> Maybe CoreExpr
wordBitOp2 op (MachWord w1) (MachWord w2)
  = wordResult (w1 `op` w2)
wordBitOp2 _ _ _ = Nothing  -- Could find LitLit

wordShiftOp2 :: (Integer->Int->Integer) -> Literal -> Literal -> Maybe CoreExpr
-- Shifts take an Int; hence second arg of op is Int
wordShiftOp2 op (MachWord x) (MachInt n)
  = wordResult (x `op` fromInteger n)
    -- Do the shift at type Integer
wordShiftOp2 _ _ _ = Nothing

--------------------------
floatOp2 :: (Rational -> Rational -> Rational) -> Literal -> Literal
         -> Maybe (Expr CoreBndr)
floatOp2  op (MachFloat f1) (MachFloat f2)
  = Just (mkFloatVal (f1 `op` f2))
floatOp2 _ _ _ = Nothing

floatOp2Z :: (Rational -> Rational -> Rational) -> Literal -> Literal
          -> Maybe (Expr CoreBndr)
floatOp2Z op (MachFloat f1) (MachFloat f2)
  | (f1 /= 0 || f2 > 0)  -- see Note [negative zero]
  && f2 /= 0             -- avoid NaN and Infinity/-Infinity
  = Just (mkFloatVal (f1 `op` f2))
floatOp2Z _ _ _ = Nothing

--------------------------
doubleOp2 :: (Rational -> Rational -> Rational) -> Literal -> Literal
          -> Maybe (Expr CoreBndr)
doubleOp2  op (MachDouble f1) (MachDouble f2)
  = Just (mkDoubleVal (f1 `op` f2))
doubleOp2 _ _ _ = Nothing

doubleOp2Z :: (Rational -> Rational -> Rational) -> Literal -> Literal
           -> Maybe (Expr CoreBndr)
doubleOp2Z op (MachDouble f1) (MachDouble f2)
  | (f1 /= 0 || f2 > 0)  -- see Note [negative zero]
  && f2 /= 0             -- avoid NaN and Infinity/-Infinity
  = Just (mkDoubleVal (f1 `op` f2))
  -- Note [negative zero] Avoid (0 / -d), otherwise 0/(-1) reduces to
  -- zero, but we might want to preserve the negative zero here which
  -- is representable in Float/Double but not in (normalised)
  -- Rational. (#3676) Perhaps we should generate (0 :% (-1)) instead?
doubleOp2Z _ _ _ = Nothing


--------------------------
-- This stuff turns
--      n ==# 3#
-- into
--      case n of
--        3# -> True
--        m  -> False
--
-- This is a Good Thing, because it allows case-of case things
-- to happen, and case-default absorption to happen.  For
-- example:
--
--      if (n ==# 3#) || (n ==# 4#) then e1 else e2
-- will transform to
--      case n of
--        3# -> e1
--        4# -> e1
--        m  -> e2
-- (modulo the usual precautions to avoid duplicating e1)

litEq :: Name
      -> Bool  -- True <=> equality, False <=> inequality
      -> [CoreRule]
litEq op_name is_eq
  = [BuiltinRule { ru_name = occNameFS (nameOccName op_name)
                                `appendFS` (fsLit "->case"),
                   ru_fn = op_name,
                   ru_nargs = 2, ru_try = rule_fn }]
  where
    rule_fn _ _ [Lit lit, expr] = do_lit_eq lit expr
    rule_fn _ _ [expr, Lit lit] = do_lit_eq lit expr
    rule_fn _ _ _               = Nothing

    do_lit_eq lit expr
      | litIsLifted lit 
      = Nothing
      | otherwise
      = Just (mkWildCase expr (literalType lit) boolTy
                    [(DEFAULT,    [], val_if_neq),
                     (LitAlt lit, [], val_if_eq)])
    val_if_eq  | is_eq     = trueVal
               | otherwise = falseVal
    val_if_neq | is_eq     = falseVal
               | otherwise = trueVal


-- | Check if there is comparison with minBound or maxBound, that is
-- always true or false. For instance, an Int cannot be smaller than its
-- minBound, so we can replace such comparison with False.
boundsCmp :: Name -> Comparison -> [CoreRule]
boundsCmp op_name op = [ rule ]
  where
    rule = BuiltinRule
      { ru_name = occNameFS (nameOccName op_name)
                    `appendFS` (fsLit "min/maxBound")
      , ru_fn = op_name
      , ru_nargs = 2
      , ru_try = rule_fn
      }
    rule_fn _ _ [a, b] = mkRuleFn op a b
    rule_fn _ _ _      = Nothing

data Comparison = Gt | Ge | Lt | Le

mkRuleFn :: Comparison -> CoreExpr -> CoreExpr -> Maybe CoreExpr
mkRuleFn Gt (Lit lit) _ | isMinBound lit = Just falseVal
mkRuleFn Le (Lit lit) _ | isMinBound lit = Just trueVal
mkRuleFn Ge _ (Lit lit) | isMinBound lit = Just trueVal
mkRuleFn Lt _ (Lit lit) | isMinBound lit = Just falseVal
mkRuleFn Ge (Lit lit) _ | isMaxBound lit = Just trueVal
mkRuleFn Lt (Lit lit) _ | isMaxBound lit = Just falseVal
mkRuleFn Gt _ (Lit lit) | isMaxBound lit = Just falseVal
mkRuleFn Le _ (Lit lit) | isMaxBound lit = Just trueVal
mkRuleFn _ _ _                           = Nothing

isMinBound :: Literal -> Bool
isMinBound (MachChar c)   = c == minBound
isMinBound (MachInt i)    = i == toInteger (minBound :: Int)
isMinBound (MachInt64 i)  = i == toInteger (minBound :: Int64)
isMinBound (MachWord i)   = i == toInteger (minBound :: Word)
isMinBound (MachWord64 i) = i == toInteger (minBound :: Word64)
isMinBound _              = False

isMaxBound :: Literal -> Bool
isMaxBound (MachChar c)   = c == maxBound
isMaxBound (MachInt i)    = i == toInteger (maxBound :: Int)
isMaxBound (MachInt64 i)  = i == toInteger (maxBound :: Int64)
isMaxBound (MachWord i)   = i == toInteger (maxBound :: Word)
isMaxBound (MachWord64 i) = i == toInteger (maxBound :: Word64)
isMaxBound _              = False


-- Note that we *don't* warn the user about overflow. It's not done at
-- runtime either, and compilation of completely harmless things like
--    ((124076834 :: Word32) + (2147483647 :: Word32))
-- would yield a warning. Instead we simply squash the value into the
-- *target* Int/Word range.
intResult :: Integer -> Maybe CoreExpr
intResult result
  = Just (mkIntVal (toInteger (fromInteger result :: TargetInt)))

wordResult :: Integer -> Maybe CoreExpr
wordResult result
  = Just (mkWordVal (toInteger (fromInteger result :: TargetWord)))
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Vaguely generic functions}
%*                                                                      *
%************************************************************************

\begin{code}
mkBasicRule :: Name -> Int
            -> (IdUnfoldingFun -> [CoreExpr] -> Maybe CoreExpr)
            -> [CoreRule]
-- Gives the Rule the same name as the primop itself
mkBasicRule op_name n_args rule_fn
  = [BuiltinRule { ru_name = occNameFS (nameOccName op_name),
                   ru_fn = op_name,
                   ru_nargs = n_args, ru_try = \_ -> rule_fn }]

oneLit :: Name -> (Literal -> Maybe CoreExpr)
       -> [CoreRule]
oneLit op_name test
  = mkBasicRule op_name 1 rule_fn
  where
    rule_fn _ [Lit l1] = test (convFloating l1)
    rule_fn _ _        = Nothing

twoLits :: Name -> (Literal -> Literal -> Maybe CoreExpr)
        -> [CoreRule]
twoLits op_name test
  = mkBasicRule op_name 2 rule_fn
  where
    rule_fn _ [Lit l1, Lit l2] = test (convFloating l1) (convFloating l2)
    rule_fn _ _                = Nothing

-- When excess precision is not requested, cut down the precision of the
-- Rational value to that of Float/Double. We confuse host architecture
-- and target architecture here, but it's convenient (and wrong :-).
convFloating :: Literal -> Literal
convFloating (MachFloat  f) | not opt_SimplExcessPrecision =
   MachFloat  (toRational ((fromRational f) :: Float ))
convFloating (MachDouble d) | not opt_SimplExcessPrecision =
   MachDouble (toRational ((fromRational d) :: Double))
convFloating l = l

trueVal, falseVal :: Expr CoreBndr
trueVal       = Var trueDataConId
falseVal      = Var falseDataConId

ltVal, eqVal, gtVal :: Expr CoreBndr
ltVal = Var ltDataConId
eqVal = Var eqDataConId
gtVal = Var gtDataConId

mkIntVal :: Integer -> Expr CoreBndr
mkIntVal    i = Lit (mkMachInt  i)
mkWordVal :: Integer -> Expr CoreBndr
mkWordVal   w = Lit (mkMachWord w)
mkFloatVal :: Rational -> Expr CoreBndr
mkFloatVal  f = Lit (convFloating (MachFloat  f))
mkDoubleVal :: Rational -> Expr CoreBndr
mkDoubleVal d = Lit (convFloating (MachDouble d))
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Special rules for seq, tagToEnum, dataToTag}
%*                                                                      *
%************************************************************************

Note [tagToEnum#]
~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  Unification may refine the type later, but this
check won't see that, alas.  It's crude but it works.

Here's are two cases that should fail
        f :: forall a. a
        f = tagToEnum# 0        -- Can't do tagToEnum# at a type variable

        g :: Int
        g = tagToEnum# 0        -- Int is not an enumeration

We used to make this check in the type inference engine, but it's quite
ugly to do so, because the delayed constraint solving means that we don't
really know what's going on until the end. It's very much a corner case
because we don't expect the user to call tagToEnum# at all; we merely
generate calls in derived instances of Enum.  So we compromise: a
rewrite rule rewrites a bad instance of tagToEnum# to an error call,
and emits a warning.

\begin{code}
tagToEnumRule :: IdUnfoldingFun -> [Expr CoreBndr] -> Maybe (Expr CoreBndr)
-- If     data T a = A | B | C
-- then   tag2Enum# (T ty) 2# -->  B ty
tagToEnumRule _ [Type ty, Lit (MachInt i)]
  | Just (tycon, tc_args) <- splitTyConApp_maybe ty
  , isEnumerationTyCon tycon
  = case filter correct_tag (tyConDataCons_maybe tycon `orElse` []) of
        []        -> Nothing  -- Abstract type
        (dc:rest) -> ASSERT( null rest )
                     Just (mkTyApps (Var (dataConWorkId dc)) tc_args)
  | otherwise  -- See Note [tagToEnum#]
  = WARN( True, ptext (sLit "tagToEnum# on non-enumeration type") <+> ppr ty )
    Just (mkRuntimeErrorApp rUNTIME_ERROR_ID ty "tagToEnum# on non-enumeration type")
  where
    correct_tag dc = (dataConTag dc - fIRST_TAG) == tag
    tag = fromInteger i

tagToEnumRule _ _ = Nothing
\end{code}


For dataToTag#, we can reduce if either

        (a) the argument is a constructor
        (b) the argument is a variable whose unfolding is a known constructor

\begin{code}
dataToTagRule :: IdUnfoldingFun -> [Expr CoreBndr] -> Maybe (Arg CoreBndr)
dataToTagRule _ [Type ty1, Var tag_to_enum `App` Type ty2 `App` tag]
  | tag_to_enum `hasKey` tagToEnumKey
  , ty1 `eqType` ty2
  = Just tag  -- dataToTag (tagToEnum x)   ==>   x

dataToTagRule id_unf [_, val_arg]
  | Just (dc,_,_) <- exprIsConApp_maybe id_unf val_arg
  = ASSERT( not (isNewTyCon (dataConTyCon dc)) )
    Just (mkIntVal (toInteger (dataConTag dc - fIRST_TAG)))

dataToTagRule _ _ = Nothing
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Rules for seq# and spark#}
%*                                                                      *
%************************************************************************

\begin{code}
-- seq# :: forall a s . a -> State# s -> (# State# s, a #)
seqRule :: IdUnfoldingFun -> [CoreExpr] -> Maybe CoreExpr
seqRule _ [ty_a, Type ty_s, a, s] | exprIsHNF a
   = Just (mkConApp (tupleCon UnboxedTuple 2)
                    [Type (mkStatePrimTy ty_s), ty_a, s, a])
seqRule _ _ = Nothing

-- spark# :: forall a s . a -> State# s -> (# State# s, a #)
sparkRule :: IdUnfoldingFun -> [CoreExpr] -> Maybe CoreExpr
sparkRule = seqRule -- reduce on HNF, just the same
  -- XXX perhaps we shouldn't do this, because a spark eliminated by
  -- this rule won't be counted as a dud at runtime?
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Built in rules}
%*                                                                      *
%************************************************************************

Note [Scoping for Builtin rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When compiling a (base-package) module that defines one of the
functions mentioned in the RHS of a built-in rule, there's a danger
that we'll see

        f = ...(eq String x)....

        ....and lower down...

        eqString = ...

Then a rewrite would give

        f = ...(eqString x)...
        ....and lower down...
        eqString = ...

and lo, eqString is not in scope.  This only really matters when we get to code
generation.  With -O we do a GlomBinds step that does a new SCC analysis on the whole
set of bindings, which sorts out the dependency.  Without -O we don't do any rule
rewriting so again we are fine.

(This whole thing doesn't show up for non-built-in rules because their dependencies
are explicit.)


\begin{code}
builtinRules :: [CoreRule]
-- Rules for non-primops that can't be expressed using a RULE pragma
builtinRules
  = [BuiltinRule { ru_name = fsLit "AppendLitString",
                   ru_fn = unpackCStringFoldrName,
                   ru_nargs = 4, ru_try = \_ -> match_append_lit },
     BuiltinRule { ru_name = fsLit "EqString", ru_fn = eqStringName,
                   ru_nargs = 2, ru_try = \_ -> match_eq_string },
     BuiltinRule { ru_name = fsLit "Inline", ru_fn = inlineIdName,
                   ru_nargs = 2, ru_try = \_ -> match_inline }]
 ++ builtinIntegerRules

builtinIntegerRules :: [CoreRule]
builtinIntegerRules =
 [rule_IntToInteger   "smallInteger"        smallIntegerName,
  rule_WordToInteger  "wordToInteger"       wordToIntegerName,
  rule_Int64ToInteger  "int64ToInteger"     int64ToIntegerName,
  rule_Word64ToInteger "word64ToInteger"    word64ToIntegerName,
  rule_convert        "integerToWord"       integerToWordName       mkWordLitWord,
  rule_convert        "integerToInt"        integerToIntName        mkIntLitInt,
  rule_convert        "integerToWord64"     integerToWord64Name     mkWord64LitWord64,
  rule_convert        "integerToInt64"      integerToInt64Name      mkInt64LitInt64,
  rule_binop          "plusInteger"         plusIntegerName         (+),
  rule_binop          "minusInteger"        minusIntegerName        (-),
  rule_binop          "timesInteger"        timesIntegerName        (*),
  rule_unop           "negateInteger"       negateIntegerName       negate,
  rule_binop_Bool     "eqInteger"           eqIntegerName           (==),
  rule_binop_Bool     "neqInteger"          neqIntegerName          (/=),
  rule_unop           "absInteger"          absIntegerName          abs,
  rule_unop           "signumInteger"       signumIntegerName       signum,
  rule_binop_Bool     "leInteger"           leIntegerName           (<=),
  rule_binop_Bool     "gtInteger"           gtIntegerName           (>),
  rule_binop_Bool     "ltInteger"           ltIntegerName           (<),
  rule_binop_Bool     "geInteger"           geIntegerName           (>=),
  rule_binop_Ordering "compareInteger"      compareIntegerName      compare,
  rule_divop_both     "divModInteger"       divModIntegerName       divMod,
  rule_divop_both     "quotRemInteger"      quotRemIntegerName      quotRem,
  rule_divop_one      "quotInteger"         quotIntegerName         quot,
  rule_divop_one      "remInteger"          remIntegerName          rem,
  rule_encodeFloat    "encodeFloatInteger"  encodeFloatIntegerName  mkFloatLitFloat,
  rule_convert        "floatFromInteger"    floatFromIntegerName    mkFloatLitFloat,
  rule_encodeFloat    "encodeDoubleInteger" encodeDoubleIntegerName mkDoubleLitDouble,
  rule_decodeDouble   "decodeDoubleInteger" decodeDoubleIntegerName,
  rule_convert        "doubleFromInteger"   doubleFromIntegerName   mkDoubleLitDouble,
  rule_binop          "gcdInteger"          gcdIntegerName          gcd,
  rule_binop          "lcmInteger"          lcmIntegerName          lcm,
  rule_binop          "andInteger"          andIntegerName          (.&.),
  rule_binop          "orInteger"           orIntegerName           (.|.),
  rule_binop          "xorInteger"          xorIntegerName          xor,
  rule_unop           "complementInteger"   complementIntegerName   complement,
  rule_Int_binop      "shiftLInteger"       shiftLIntegerName       shiftL,
  rule_Int_binop      "shiftRInteger"       shiftRIntegerName       shiftR]
    where rule_convert str name convert
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_Integer_convert convert }
          rule_IntToInteger str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_IntToInteger }
          rule_WordToInteger str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_WordToInteger }
          rule_Int64ToInteger str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_Int64ToInteger }
          rule_Word64ToInteger str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_Word64ToInteger }
          rule_unop str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_Integer_unop op }
          rule_binop str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_binop op }
          rule_divop_both str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_divop_both op }
          rule_divop_one str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_divop_one op }
          rule_Int_binop str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_Int_binop op }
          rule_binop_Bool str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_binop_Bool op }
          rule_binop_Ordering str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_binop_Ordering op }
          rule_encodeFloat str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_Int_encodeFloat op }
          rule_decodeDouble str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_decodeDouble }

---------------------------------------------------
-- The rule is this:
--      unpackFoldrCString# "foo" c (unpackFoldrCString# "baz" c n)
--      =  unpackFoldrCString# "foobaz" c n

match_append_lit :: IdUnfoldingFun -> [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_append_lit _ [Type ty1,
                    Lit (MachStr s1),
                    c1,
                    Var unpk `App` Type ty2
                             `App` Lit (MachStr s2)
                             `App` c2
                             `App` n
                   ]
  | unpk `hasKey` unpackCStringFoldrIdKey &&
    c1 `cheapEqExpr` c2
  = ASSERT( ty1 `eqType` ty2 )
    Just (Var unpk `App` Type ty1
                   `App` Lit (MachStr (s1 `appendFS` s2))
                   `App` c1
                   `App` n)

match_append_lit _ _ = Nothing

---------------------------------------------------
-- The rule is this:
--      eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2

match_eq_string :: IdUnfoldingFun -> [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_eq_string _ [Var unpk1 `App` Lit (MachStr s1),
                   Var unpk2 `App` Lit (MachStr s2)]
  | unpk1 `hasKey` unpackCStringIdKey,
    unpk2 `hasKey` unpackCStringIdKey
  = Just (if s1 == s2 then trueVal else falseVal)

match_eq_string _ _ = Nothing


---------------------------------------------------
-- The rule is this:
--      inline f_ty (f a b c) = <f's unfolding> a b c
-- (if f has an unfolding, EVEN if it's a loop breaker)
--
-- It's important to allow the argument to 'inline' to have args itself
-- (a) because its more forgiving to allow the programmer to write
--       inline f a b c
--   or  inline (f a b c)
-- (b) because a polymorphic f wll get a type argument that the
--     programmer can't avoid
--
-- Also, don't forget about 'inline's type argument!
match_inline :: IdUnfoldingFun -> [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_inline _ (Type _ : e : _)
  | (Var f, args1) <- collectArgs e,
    Just unf <- maybeUnfoldingTemplate (realIdUnfolding f)
             -- Ignore the IdUnfoldingFun here!
  = Just (mkApps unf args1)

match_inline _ _ = Nothing

-- Integer rules

match_IntToInteger :: Id
                   -> IdUnfoldingFun
                   -> [Expr CoreBndr]
                   -> Maybe (Expr CoreBndr)
match_IntToInteger id id_unf [xl]
  | Just (MachInt x) <- exprIsLiteral_maybe id_unf xl
  = case idType id of
    FunTy _ integerTy ->
        Just (Lit (LitInteger x integerTy))
    _ ->
        panic "match_IntToInteger: Id has the wrong type"
match_IntToInteger _ _ _ = Nothing

match_WordToInteger :: Id
                    -> IdUnfoldingFun
                    -> [Expr CoreBndr]
                    -> Maybe (Expr CoreBndr)
match_WordToInteger id id_unf [xl]
  | Just (MachWord x) <- exprIsLiteral_maybe id_unf xl
  = case idType id of
    FunTy _ integerTy ->
        Just (Lit (LitInteger x integerTy))
    _ ->
        panic "match_WordToInteger: Id has the wrong type"
match_WordToInteger _ _ _ = Nothing

match_Int64ToInteger :: Id
                     -> IdUnfoldingFun
                     -> [Expr CoreBndr]
                     -> Maybe (Expr CoreBndr)
match_Int64ToInteger id id_unf [xl]
  | Just (MachInt64 x) <- exprIsLiteral_maybe id_unf xl
  = case idType id of
    FunTy _ integerTy ->
        Just (Lit (LitInteger x integerTy))
    _ ->
        panic "match_Int64ToInteger: Id has the wrong type"
match_Int64ToInteger _ _ _ = Nothing

match_Word64ToInteger :: Id
                      -> IdUnfoldingFun
                      -> [Expr CoreBndr]
                      -> Maybe (Expr CoreBndr)
match_Word64ToInteger id id_unf [xl]
  | Just (MachWord x) <- exprIsLiteral_maybe id_unf xl
  = case idType id of
    FunTy _ integerTy ->
        Just (Lit (LitInteger x integerTy))
    _ ->
        panic "match_Word64ToInteger: Id has the wrong type"
match_Word64ToInteger _ _ _ = Nothing

match_Integer_convert :: Num a
                      => (a -> Expr CoreBndr)
                      -> Id
                      -> IdUnfoldingFun
                      -> [Expr CoreBndr]
                      -> Maybe (Expr CoreBndr)
match_Integer_convert convert _ id_unf [xl]
  | Just (LitInteger x _) <- exprIsLiteral_maybe id_unf xl
  = Just (convert (fromInteger x))
match_Integer_convert _ _ _ _ = Nothing

match_Integer_unop :: (Integer -> Integer)
                   -> Id
                   -> IdUnfoldingFun
                   -> [Expr CoreBndr]
                   -> Maybe (Expr CoreBndr)
match_Integer_unop unop _ id_unf [xl]
  | Just (LitInteger x i) <- exprIsLiteral_maybe id_unf xl
  = Just (Lit (LitInteger (unop x) i))
match_Integer_unop _ _ _ _ = Nothing

match_Integer_binop :: (Integer -> Integer -> Integer)
                    -> Id
                    -> IdUnfoldingFun
                    -> [Expr CoreBndr]
                    -> Maybe (Expr CoreBndr)
match_Integer_binop binop _ id_unf [xl,yl]
  | Just (LitInteger x i) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  = Just (Lit (LitInteger (x `binop` y) i))
match_Integer_binop _ _ _ _ = Nothing

-- This helper is used for the quotRem and divMod functions
match_Integer_divop_both :: (Integer -> Integer -> (Integer, Integer))
                         -> Id
                         -> IdUnfoldingFun
                         -> [Expr CoreBndr]
                         -> Maybe (Expr CoreBndr)
match_Integer_divop_both divop _ id_unf [xl,yl]
  | Just (LitInteger x t) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  , y /= 0
  , (r,s) <- x `divop` y
  = Just $ mkConApp (tupleCon UnboxedTuple 2)
                    [Type t,
                     Type t,
                     Lit (LitInteger r t),
                     Lit (LitInteger s t)]
match_Integer_divop_both _ _ _ _ = Nothing

-- This helper is used for the quotRem and divMod functions
match_Integer_divop_one :: (Integer -> Integer -> Integer)
                        -> Id
                        -> IdUnfoldingFun
                        -> [Expr CoreBndr]
                        -> Maybe (Expr CoreBndr)
match_Integer_divop_one divop _ id_unf [xl,yl]
  | Just (LitInteger x i) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  , y /= 0
  = Just (Lit (LitInteger (x `divop` y) i))
match_Integer_divop_one _ _ _ _ = Nothing

match_Integer_Int_binop :: (Integer -> Int -> Integer)
                        -> Id
                        -> IdUnfoldingFun
                        -> [Expr CoreBndr]
                        -> Maybe (Expr CoreBndr)
match_Integer_Int_binop binop _ id_unf [xl,yl]
  | Just (LitInteger x i) <- exprIsLiteral_maybe id_unf xl
  , Just (MachInt y)      <- exprIsLiteral_maybe id_unf yl
  = Just (Lit (LitInteger (x `binop` fromIntegral y) i))
match_Integer_Int_binop _ _ _ _ = Nothing

match_Integer_binop_Bool :: (Integer -> Integer -> Bool)
                         -> Id
                         -> IdUnfoldingFun
                         -> [Expr CoreBndr]
                         -> Maybe (Expr CoreBndr)
match_Integer_binop_Bool binop _ id_unf [xl, yl]
  | Just (LitInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  = Just (if x `binop` y then trueVal else falseVal)
match_Integer_binop_Bool _ _ _ _ = Nothing

match_Integer_binop_Ordering :: (Integer -> Integer -> Ordering)
                             -> Id
                             -> IdUnfoldingFun
                             -> [Expr CoreBndr]
                             -> Maybe (Expr CoreBndr)
match_Integer_binop_Ordering binop _ id_unf [xl, yl]
  | Just (LitInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  = Just $ case x `binop` y of
             LT -> ltVal
             EQ -> eqVal
             GT -> gtVal
match_Integer_binop_Ordering _ _ _ _ = Nothing

match_Integer_Int_encodeFloat :: RealFloat a
                              => (a -> Expr CoreBndr)
                              -> Id
                              -> IdUnfoldingFun
                              -> [Expr CoreBndr]
                              -> Maybe (Expr CoreBndr)
match_Integer_Int_encodeFloat mkLit _ id_unf [xl,yl]
  | Just (LitInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (MachInt y)      <- exprIsLiteral_maybe id_unf yl
  = Just (mkLit $ encodeFloat x (fromInteger y))
match_Integer_Int_encodeFloat _ _ _ _ = Nothing

match_decodeDouble :: Id
                   -> IdUnfoldingFun
                   -> [Expr CoreBndr]
                   -> Maybe (Expr CoreBndr)
match_decodeDouble fn id_unf [xl]
  | Just (MachDouble x) <- exprIsLiteral_maybe id_unf xl
  = case idType fn of
    FunTy _ (TyConApp _ [integerTy, intHashTy]) ->
        case decodeFloat (fromRational x :: Double) of
        (y, z) ->
            Just $ mkConApp (tupleCon UnboxedTuple 2)
                            [Type integerTy,
                             Type intHashTy,
                             Lit (LitInteger y integerTy),
                             Lit (MachInt (toInteger z))]
    _ ->
        panic "match_decodeDouble: Id has the wrong type"
match_decodeDouble _ _ _ = Nothing
\end{code}
