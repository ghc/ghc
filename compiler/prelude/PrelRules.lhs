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
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -optc-DNON_POSIX_SOURCE #-}

module PrelRules ( primOpRules, builtinRules ) where

#include "HsVersions.h"
#include "../includes/MachDeps.h"

import {-# SOURCE #-} MkId ( mkPrimOpId, magicSingIId )

import CoreSyn
import MkCore
import Id
import Var         (setVarType)
import Literal
import CoreSubst   ( exprIsLiteral_maybe )
import PrimOp      ( PrimOp(..), tagToEnumKey )
import TysWiredIn
import TysPrim
import TyCon       ( tyConDataCons_maybe, isEnumerationTyCon, isNewTyCon, unwrapNewTyCon_maybe )
import DataCon     ( dataConTag, dataConTyCon, dataConWorkId )
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
import BasicTypes
import DynFlags
import Platform
import Util
import Coercion     (mkUnbranchedAxInstCo,mkSymCo,Role(..))

import Control.Applicative ( Applicative(..), Alternative(..) )
import Control.Monad
import Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.Int
import Data.Ratio
import Data.Word
\end{code}


Note [Constant folding]
~~~~~~~~~~~~~~~~~~~~~~~
primOpRules generates a rewrite rule for each primop
These rules do what is often called "constant folding"
E.g. the rules for +# might say
        4 +# 5 = 9
Well, of course you'd need a lot of rules if you did it
like that, so we use a BuiltinRule instead, so that we
can match in any two literal values.  So the rule is really
more like
        (Lit x) +# (Lit y) = Lit (x+#y)
where the (+#) on the rhs is done at compile time

That is why these rules are built in here.


\begin{code}
primOpRules :: Name -> PrimOp -> Maybe CoreRule
    -- ToDo: something for integer-shift ops?
    --       NotOp
primOpRules nm TagToEnumOp = mkPrimOpRule nm 2 [ tagToEnumRule ]
primOpRules nm DataToTagOp = mkPrimOpRule nm 2 [ dataToTagRule ]

-- Int operations
primOpRules nm IntAddOp    = mkPrimOpRule nm 2 [ binaryLit (intOp2 (+))
                                               , identityDynFlags zeroi ]
primOpRules nm IntSubOp    = mkPrimOpRule nm 2 [ binaryLit (intOp2 (-))
                                               , rightIdentityDynFlags zeroi
                                               , equalArgs >> retLit zeroi ]
primOpRules nm IntMulOp    = mkPrimOpRule nm 2 [ binaryLit (intOp2 (*))
                                               , zeroElem zeroi
                                               , identityDynFlags onei ]
primOpRules nm IntQuotOp   = mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (intOp2 quot)
                                               , leftZero zeroi
                                               , rightIdentityDynFlags onei
                                               , equalArgs >> retLit onei ]
primOpRules nm IntRemOp    = mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (intOp2 rem)
                                               , leftZero zeroi
                                               , do l <- getLiteral 1
                                                    dflags <- getDynFlags
                                                    guard (l == onei dflags)
                                                    retLit zeroi
                                               , equalArgs >> retLit zeroi
                                               , equalArgs >> retLit zeroi ]
primOpRules nm AndIOp      = mkPrimOpRule nm 2 [ binaryLit (intOp2 (.&.))
                                               , idempotent
                                               , zeroElem zeroi ]
primOpRules nm OrIOp       = mkPrimOpRule nm 2 [ binaryLit (intOp2 (.|.))
                                               , idempotent
                                               , identityDynFlags zeroi ]
primOpRules nm XorIOp      = mkPrimOpRule nm 2 [ binaryLit (intOp2 xor)
                                               , identityDynFlags zeroi
                                               , equalArgs >> retLit zeroi ]
primOpRules nm IntNegOp    = mkPrimOpRule nm 1 [ unaryLit negOp
                                               , inversePrimOp IntNegOp ]
primOpRules nm ISllOp      = mkPrimOpRule nm 2 [ binaryLit (intOp2 Bits.shiftL)
                                               , rightIdentityDynFlags zeroi ]
primOpRules nm ISraOp      = mkPrimOpRule nm 2 [ binaryLit (intOp2 Bits.shiftR)
                                               , rightIdentityDynFlags zeroi ]
primOpRules nm ISrlOp      = mkPrimOpRule nm 2 [ binaryLit (intOp2 shiftRightLogical)
                                               , rightIdentityDynFlags zeroi ]

-- Word operations
primOpRules nm WordAddOp   = mkPrimOpRule nm 2 [ binaryLit (wordOp2 (+))
                                               , identityDynFlags zerow ]
primOpRules nm WordSubOp   = mkPrimOpRule nm 2 [ binaryLit (wordOp2 (-))
                                               , rightIdentityDynFlags zerow
                                               , equalArgs >> retLit zerow ]
primOpRules nm WordMulOp   = mkPrimOpRule nm 2 [ binaryLit (wordOp2 (*))
                                               , identityDynFlags onew ]
primOpRules nm WordQuotOp  = mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (wordOp2 quot)
                                               , rightIdentityDynFlags onew ]
primOpRules nm WordRemOp   = mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (wordOp2 rem)
                                               , rightIdentityDynFlags onew ]
primOpRules nm AndOp       = mkPrimOpRule nm 2 [ binaryLit (wordOp2 (.&.))
                                               , idempotent
                                               , zeroElem zerow ]
primOpRules nm OrOp        = mkPrimOpRule nm 2 [ binaryLit (wordOp2 (.|.))
                                               , idempotent
                                               , identityDynFlags zerow ]
primOpRules nm XorOp       = mkPrimOpRule nm 2 [ binaryLit (wordOp2 xor)
                                               , identityDynFlags zerow
                                               , equalArgs >> retLit zerow ]
primOpRules nm SllOp       = mkPrimOpRule nm 2 [ binaryLit (wordShiftOp2 Bits.shiftL)
                                               , rightIdentityDynFlags zeroi ]
primOpRules nm SrlOp       = mkPrimOpRule nm 2 [ binaryLit (wordShiftOp2 shiftRightLogical)
                                               , rightIdentityDynFlags zeroi ]

-- coercions
primOpRules nm Word2IntOp     = mkPrimOpRule nm 1 [ liftLitDynFlags word2IntLit
                                                  , inversePrimOp Int2WordOp ]
primOpRules nm Int2WordOp     = mkPrimOpRule nm 1 [ liftLitDynFlags int2WordLit
                                                  , inversePrimOp Word2IntOp ]
primOpRules nm Narrow8IntOp   = mkPrimOpRule nm 1 [ liftLit narrow8IntLit
                                                  , subsumedByPrimOp Narrow8IntOp
                                                  , Narrow8IntOp `subsumesPrimOp` Narrow16IntOp
                                                  , Narrow8IntOp `subsumesPrimOp` Narrow32IntOp ]
primOpRules nm Narrow16IntOp  = mkPrimOpRule nm 1 [ liftLit narrow16IntLit
                                                  , subsumedByPrimOp Narrow8IntOp
                                                  , subsumedByPrimOp Narrow16IntOp
                                                  , Narrow16IntOp `subsumesPrimOp` Narrow32IntOp ]
primOpRules nm Narrow32IntOp  = mkPrimOpRule nm 1 [ liftLit narrow32IntLit
                                                  , subsumedByPrimOp Narrow8IntOp
                                                  , subsumedByPrimOp Narrow16IntOp
                                                  , subsumedByPrimOp Narrow32IntOp
                                                  , removeOp32 ]
primOpRules nm Narrow8WordOp  = mkPrimOpRule nm 1 [ liftLit narrow8WordLit
                                                  , subsumedByPrimOp Narrow8WordOp
                                                  , Narrow8WordOp `subsumesPrimOp` Narrow16WordOp
                                                  , Narrow8WordOp `subsumesPrimOp` Narrow32WordOp ]
primOpRules nm Narrow16WordOp = mkPrimOpRule nm 1 [ liftLit narrow16WordLit
                                                  , subsumedByPrimOp Narrow8WordOp
                                                  , subsumedByPrimOp Narrow16WordOp
                                                  , Narrow16WordOp `subsumesPrimOp` Narrow32WordOp ]
primOpRules nm Narrow32WordOp = mkPrimOpRule nm 1 [ liftLit narrow32WordLit
                                                  , subsumedByPrimOp Narrow8WordOp
                                                  , subsumedByPrimOp Narrow16WordOp
                                                  , subsumedByPrimOp Narrow32WordOp
                                                  , removeOp32 ]
primOpRules nm OrdOp          = mkPrimOpRule nm 1 [ liftLit char2IntLit
                                                  , inversePrimOp ChrOp ]
primOpRules nm ChrOp          = mkPrimOpRule nm 1 [ do [Lit lit] <- getArgs
                                                       guard (litFitsInChar lit)
                                                       liftLit int2CharLit
                                                  , inversePrimOp OrdOp ]
primOpRules nm Float2IntOp    = mkPrimOpRule nm 1 [ liftLit float2IntLit ]
primOpRules nm Int2FloatOp    = mkPrimOpRule nm 1 [ liftLit int2FloatLit ]
primOpRules nm Double2IntOp   = mkPrimOpRule nm 1 [ liftLit double2IntLit ]
primOpRules nm Int2DoubleOp   = mkPrimOpRule nm 1 [ liftLit int2DoubleLit ]
-- SUP: Not sure what the standard says about precision in the following 2 cases
primOpRules nm Float2DoubleOp = mkPrimOpRule nm 1 [ liftLit float2DoubleLit ]
primOpRules nm Double2FloatOp = mkPrimOpRule nm 1 [ liftLit double2FloatLit ]

-- Float
primOpRules nm FloatAddOp   = mkPrimOpRule nm 2 [ binaryLit (floatOp2 (+))
                                                , identity zerof ]
primOpRules nm FloatSubOp   = mkPrimOpRule nm 2 [ binaryLit (floatOp2 (-))
                                                , rightIdentity zerof ]
primOpRules nm FloatMulOp   = mkPrimOpRule nm 2 [ binaryLit (floatOp2 (*))
                                                , identity onef
                                                , strengthReduction twof FloatAddOp  ]
                         -- zeroElem zerof doesn't hold because of NaN
primOpRules nm FloatDivOp   = mkPrimOpRule nm 2 [ guardFloatDiv >> binaryLit (floatOp2 (/))
                                                , rightIdentity onef ]
primOpRules nm FloatNegOp   = mkPrimOpRule nm 1 [ unaryLit negOp
                                                , inversePrimOp FloatNegOp ]

-- Double
primOpRules nm DoubleAddOp   = mkPrimOpRule nm 2 [ binaryLit (doubleOp2 (+))
                                                 , identity zerod ]
primOpRules nm DoubleSubOp   = mkPrimOpRule nm 2 [ binaryLit (doubleOp2 (-))
                                                 , rightIdentity zerod ]
primOpRules nm DoubleMulOp   = mkPrimOpRule nm 2 [ binaryLit (doubleOp2 (*))
                                                 , identity oned
                                                 , strengthReduction twod DoubleAddOp  ]
                          -- zeroElem zerod doesn't hold because of NaN
primOpRules nm DoubleDivOp   = mkPrimOpRule nm 2 [ guardDoubleDiv >> binaryLit (doubleOp2 (/))
                                                 , rightIdentity oned ]
primOpRules nm DoubleNegOp   = mkPrimOpRule nm 1 [ unaryLit negOp
                                                 , inversePrimOp DoubleNegOp ]

-- Relational operators

primOpRules nm IntEqOp    = mkRelOpRule nm (==) [ litEq True ]
primOpRules nm IntNeOp    = mkRelOpRule nm (/=) [ litEq False ]
primOpRules nm CharEqOp   = mkRelOpRule nm (==) [ litEq True ]
primOpRules nm CharNeOp   = mkRelOpRule nm (/=) [ litEq False ]

primOpRules nm IntGtOp    = mkRelOpRule nm (>)  [ boundsCmp Gt ]
primOpRules nm IntGeOp    = mkRelOpRule nm (>=) [ boundsCmp Ge ]
primOpRules nm IntLeOp    = mkRelOpRule nm (<=) [ boundsCmp Le ]
primOpRules nm IntLtOp    = mkRelOpRule nm (<)  [ boundsCmp Lt ]

primOpRules nm CharGtOp   = mkRelOpRule nm (>)  [ boundsCmp Gt ]
primOpRules nm CharGeOp   = mkRelOpRule nm (>=) [ boundsCmp Ge ]
primOpRules nm CharLeOp   = mkRelOpRule nm (<=) [ boundsCmp Le ]
primOpRules nm CharLtOp   = mkRelOpRule nm (<)  [ boundsCmp Lt ]

primOpRules nm FloatGtOp  = mkFloatingRelOpRule nm (>)  []
primOpRules nm FloatGeOp  = mkFloatingRelOpRule nm (>=) []
primOpRules nm FloatLeOp  = mkFloatingRelOpRule nm (<=) []
primOpRules nm FloatLtOp  = mkFloatingRelOpRule nm (<)  []
primOpRules nm FloatEqOp  = mkFloatingRelOpRule nm (==) [ litEq True ]
primOpRules nm FloatNeOp  = mkFloatingRelOpRule nm (/=) [ litEq False ]

primOpRules nm DoubleGtOp = mkFloatingRelOpRule nm (>)  []
primOpRules nm DoubleGeOp = mkFloatingRelOpRule nm (>=) []
primOpRules nm DoubleLeOp = mkFloatingRelOpRule nm (<=) []
primOpRules nm DoubleLtOp = mkFloatingRelOpRule nm (<)  []
primOpRules nm DoubleEqOp = mkFloatingRelOpRule nm (==) [ litEq True ]
primOpRules nm DoubleNeOp = mkFloatingRelOpRule nm (/=) [ litEq False ]

primOpRules nm WordGtOp   = mkRelOpRule nm (>)  [ boundsCmp Gt ]
primOpRules nm WordGeOp   = mkRelOpRule nm (>=) [ boundsCmp Ge ]
primOpRules nm WordLeOp   = mkRelOpRule nm (<=) [ boundsCmp Le ]
primOpRules nm WordLtOp   = mkRelOpRule nm (<)  [ boundsCmp Lt ]
primOpRules nm WordEqOp   = mkRelOpRule nm (==) [ litEq True ]
primOpRules nm WordNeOp   = mkRelOpRule nm (/=) [ litEq False ]

primOpRules nm AddrAddOp  = mkPrimOpRule nm 2 [ rightIdentityDynFlags zeroi ]

primOpRules nm SeqOp      = mkPrimOpRule nm 4 [ seqRule ]
primOpRules nm SparkOp    = mkPrimOpRule nm 4 [ sparkRule ]

primOpRules _  _          = Nothing

\end{code}

%************************************************************************
%*                                                                      *
\subsection{Doing the business}
%*                                                                      *
%************************************************************************

\begin{code}

-- useful shorthands
mkPrimOpRule :: Name -> Int -> [RuleM CoreExpr] -> Maybe CoreRule
mkPrimOpRule nm arity rules = Just $ mkBasicRule nm arity (msum rules)

mkRelOpRule :: Name -> (forall a . Ord a => a -> a -> Bool)
            -> [RuleM CoreExpr] -> Maybe CoreRule
mkRelOpRule nm cmp extra
  = mkPrimOpRule nm 2 $ rules ++ extra
  where
    rules = [ binaryCmpLit cmp
            , do equalArgs
              -- x `cmp` x does not depend on x, so
              -- compute it for the arbitrary value 'True'
              -- and use that result
                 dflags <- getDynFlags
                 return (if cmp True True
                         then trueValInt  dflags
                         else falseValInt dflags) ]

-- Note [Rules for floating-point comparisons]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We need different rules for floating-point values because for floats
-- it is not true that x = x. The special case when this does not occur
-- are NaNs.

mkFloatingRelOpRule :: Name -> (forall a . Ord a => a -> a -> Bool)
                    -> [RuleM CoreExpr] -> Maybe CoreRule
mkFloatingRelOpRule nm cmp extra -- See Note [Rules for floating-point comparisons]
  = mkPrimOpRule nm 2 $ binaryCmpLit cmp : extra

-- common constants
zeroi, onei, zerow, onew :: DynFlags -> Literal
zeroi dflags = mkMachInt  dflags 0
onei  dflags = mkMachInt  dflags 1
zerow dflags = mkMachWord dflags 0
onew  dflags = mkMachWord dflags 1

zerof, onef, twof, zerod, oned, twod :: Literal
zerof = mkMachFloat 0.0
onef  = mkMachFloat 1.0
twof  = mkMachFloat 2.0
zerod = mkMachDouble 0.0
oned  = mkMachDouble 1.0
twod  = mkMachDouble 2.0

cmpOp :: DynFlags -> (forall a . Ord a => a -> a -> Bool)
      -> Literal -> Literal -> Maybe CoreExpr
cmpOp dflags cmp = go
  where
    done True  = Just $ trueValInt  dflags
    done False = Just $ falseValInt dflags

    -- These compares are at different types
    go (MachChar i1)   (MachChar i2)   = done (i1 `cmp` i2)
    go (MachInt i1)    (MachInt i2)    = done (i1 `cmp` i2)
    go (MachInt64 i1)  (MachInt64 i2)  = done (i1 `cmp` i2)
    go (MachWord i1)   (MachWord i2)   = done (i1 `cmp` i2)
    go (MachWord64 i1) (MachWord64 i2) = done (i1 `cmp` i2)
    go (MachFloat i1)  (MachFloat i2)  = done (i1 `cmp` i2)
    go (MachDouble i1) (MachDouble i2) = done (i1 `cmp` i2)
    go _               _               = Nothing

--------------------------

negOp :: DynFlags -> Literal -> Maybe CoreExpr  -- Negate
negOp _      (MachFloat 0.0)  = Nothing  -- can't represent -0.0 as a Rational
negOp dflags (MachFloat f)    = Just (mkFloatVal dflags (-f))
negOp _      (MachDouble 0.0) = Nothing
negOp dflags (MachDouble d)   = Just (mkDoubleVal dflags (-d))
negOp dflags (MachInt i)      = intResult dflags (-i)
negOp _      _                = Nothing

--------------------------
intOp2 :: (Integral a, Integral b)
       => (a -> b -> Integer)
       -> DynFlags -> Literal -> Literal -> Maybe CoreExpr
intOp2 op dflags (MachInt i1) (MachInt i2) = intResult dflags (fromInteger i1 `op` fromInteger i2)
intOp2 _  _      _            _            = Nothing  -- Could find LitLit

shiftRightLogical :: Integer -> Int -> Integer
-- Shift right, putting zeros in rather than sign-propagating as Bits.shiftR would do
-- Do this by converting to Word and back.  Obviously this won't work for big
-- values, but its ok as we use it here
shiftRightLogical x n = fromIntegral (fromInteger x `shiftR` n :: Word)


--------------------------
retLit :: (DynFlags -> Literal) -> RuleM CoreExpr
retLit l = do dflags <- getDynFlags
              return $ Lit $ l dflags

wordOp2 :: (Integral a, Integral b)
        => (a -> b -> Integer)
        -> DynFlags -> Literal -> Literal -> Maybe CoreExpr
wordOp2 op dflags (MachWord w1) (MachWord w2)
    = wordResult dflags (fromInteger w1 `op` fromInteger w2)
wordOp2 _ _ _ _ = Nothing  -- Could find LitLit

wordShiftOp2 :: (Integer -> Int -> Integer)
             -> DynFlags -> Literal -> Literal
             -> Maybe CoreExpr
-- Shifts take an Int; hence second arg of op is Int
wordShiftOp2 op dflags (MachWord x) (MachInt n)
  = wordResult dflags (x `op` fromInteger n)
    -- Do the shift at type Integer
wordShiftOp2 _ _ _ _ = Nothing

--------------------------
floatOp2 :: (Rational -> Rational -> Rational)
         -> DynFlags -> Literal -> Literal
         -> Maybe (Expr CoreBndr)
floatOp2 op dflags (MachFloat f1) (MachFloat f2)
  = Just (mkFloatVal dflags (f1 `op` f2))
floatOp2 _ _ _ _ = Nothing

--------------------------
doubleOp2 :: (Rational -> Rational -> Rational)
          -> DynFlags -> Literal -> Literal
          -> Maybe (Expr CoreBndr)
doubleOp2 op dflags (MachDouble f1) (MachDouble f2)
  = Just (mkDoubleVal dflags (f1 `op` f2))
doubleOp2 _ _ _ _ = Nothing

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

litEq :: Bool  -- True <=> equality, False <=> inequality
      -> RuleM CoreExpr
litEq is_eq = msum
  [ do [Lit lit, expr] <- getArgs
       dflags <- getDynFlags
       do_lit_eq dflags lit expr
  , do [expr, Lit lit] <- getArgs
       dflags <- getDynFlags
       do_lit_eq dflags lit expr ]
  where
    do_lit_eq dflags lit expr = do
      guard (not (litIsLifted lit))
      return (mkWildCase expr (literalType lit) intPrimTy
                    [(DEFAULT,    [], val_if_neq),
                     (LitAlt lit, [], val_if_eq)])
      where
        val_if_eq  | is_eq     = trueValInt  dflags
                   | otherwise = falseValInt dflags
        val_if_neq | is_eq     = falseValInt dflags
                   | otherwise = trueValInt  dflags


-- | Check if there is comparison with minBound or maxBound, that is
-- always true or false. For instance, an Int cannot be smaller than its
-- minBound, so we can replace such comparison with False.
boundsCmp :: Comparison -> RuleM CoreExpr
boundsCmp op = do
  dflags <- getDynFlags
  [a, b] <- getArgs
  liftMaybe $ mkRuleFn dflags op a b

data Comparison = Gt | Ge | Lt | Le

mkRuleFn :: DynFlags -> Comparison -> CoreExpr -> CoreExpr -> Maybe CoreExpr
mkRuleFn dflags Gt (Lit lit) _ | isMinBound dflags lit = Just $ falseValInt dflags
mkRuleFn dflags Le (Lit lit) _ | isMinBound dflags lit = Just $ trueValInt  dflags
mkRuleFn dflags Ge _ (Lit lit) | isMinBound dflags lit = Just $ trueValInt  dflags
mkRuleFn dflags Lt _ (Lit lit) | isMinBound dflags lit = Just $ falseValInt dflags
mkRuleFn dflags Ge (Lit lit) _ | isMaxBound dflags lit = Just $ trueValInt  dflags
mkRuleFn dflags Lt (Lit lit) _ | isMaxBound dflags lit = Just $ falseValInt dflags
mkRuleFn dflags Gt _ (Lit lit) | isMaxBound dflags lit = Just $ falseValInt dflags
mkRuleFn dflags Le _ (Lit lit) | isMaxBound dflags lit = Just $ trueValInt  dflags
mkRuleFn _ _ _ _                                       = Nothing

isMinBound :: DynFlags -> Literal -> Bool
isMinBound _      (MachChar c)   = c == minBound
isMinBound dflags (MachInt i)    = i == tARGET_MIN_INT dflags
isMinBound _      (MachInt64 i)  = i == toInteger (minBound :: Int64)
isMinBound _      (MachWord i)   = i == 0
isMinBound _      (MachWord64 i) = i == 0
isMinBound _      _              = False

isMaxBound :: DynFlags -> Literal -> Bool
isMaxBound _      (MachChar c)   = c == maxBound
isMaxBound dflags (MachInt i)    = i == tARGET_MAX_INT dflags
isMaxBound _      (MachInt64 i)  = i == toInteger (maxBound :: Int64)
isMaxBound dflags (MachWord i)   = i == tARGET_MAX_WORD dflags
isMaxBound _      (MachWord64 i) = i == toInteger (maxBound :: Word64)
isMaxBound _      _              = False


-- Note that we *don't* warn the user about overflow. It's not done at
-- runtime either, and compilation of completely harmless things like
--    ((124076834 :: Word32) + (2147483647 :: Word32))
-- would yield a warning. Instead we simply squash the value into the
-- *target* Int/Word range.
intResult :: DynFlags -> Integer -> Maybe CoreExpr
intResult dflags result = Just (mkIntVal dflags result')
    where result' = case platformWordSize (targetPlatform dflags) of
                    4 -> toInteger (fromInteger result :: Int32)
                    8 -> toInteger (fromInteger result :: Int64)
                    w -> panic ("intResult: Unknown platformWordSize: " ++ show w)

wordResult :: DynFlags -> Integer -> Maybe CoreExpr
wordResult dflags result = Just (mkWordVal dflags result')
    where result' = case platformWordSize (targetPlatform dflags) of
                    4 -> toInteger (fromInteger result :: Word32)
                    8 -> toInteger (fromInteger result :: Word64)
                    w -> panic ("wordResult: Unknown platformWordSize: " ++ show w)

inversePrimOp :: PrimOp -> RuleM CoreExpr
inversePrimOp primop = do
  [Var primop_id `App` e] <- getArgs
  matchPrimOpId primop primop_id
  return e

subsumesPrimOp :: PrimOp -> PrimOp -> RuleM CoreExpr
this `subsumesPrimOp` that = do
  [Var primop_id `App` e] <- getArgs
  matchPrimOpId that primop_id
  return (Var (mkPrimOpId this) `App` e)

subsumedByPrimOp :: PrimOp -> RuleM CoreExpr
subsumedByPrimOp primop = do
  [e@(Var primop_id `App` _)] <- getArgs
  matchPrimOpId primop primop_id
  return e

idempotent :: RuleM CoreExpr
idempotent = do [e1, e2] <- getArgs
                guard $ cheapEqExpr e1 e2
                return e1
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Vaguely generic functions}
%*                                                                      *
%************************************************************************

\begin{code}
mkBasicRule :: Name -> Int -> RuleM CoreExpr -> CoreRule
-- Gives the Rule the same name as the primop itself
mkBasicRule op_name n_args rm
  = BuiltinRule { ru_name = occNameFS (nameOccName op_name),
                  ru_fn = op_name,
                  ru_nargs = n_args,
                  ru_try = \ dflags in_scope _ -> runRuleM rm dflags in_scope }

newtype RuleM r = RuleM
  { runRuleM :: DynFlags -> InScopeEnv -> [CoreExpr] -> Maybe r }

instance Functor RuleM where
    fmap = liftM

instance Applicative RuleM where
    pure = return
    (<*>) = ap

instance Monad RuleM where
  return x = RuleM $ \_ _ _ -> Just x
  RuleM f >>= g = RuleM $ \dflags iu e -> case f dflags iu e of
    Nothing -> Nothing
    Just r -> runRuleM (g r) dflags iu e
  fail _ = mzero

instance Alternative RuleM where
    empty = mzero
    (<|>) = mplus

instance MonadPlus RuleM where
  mzero = RuleM $ \_ _ _ -> Nothing
  mplus (RuleM f1) (RuleM f2) = RuleM $ \dflags iu args ->
    f1 dflags iu args `mplus` f2 dflags iu args

instance HasDynFlags RuleM where
    getDynFlags = RuleM $ \dflags _ _ -> Just dflags

liftMaybe :: Maybe a -> RuleM a
liftMaybe Nothing = mzero
liftMaybe (Just x) = return x

liftLit :: (Literal -> Literal) -> RuleM CoreExpr
liftLit f = liftLitDynFlags (const f)

liftLitDynFlags :: (DynFlags -> Literal -> Literal) -> RuleM CoreExpr
liftLitDynFlags f = do
  dflags <- getDynFlags
  [Lit lit] <- getArgs
  return $ Lit (f dflags lit)

removeOp32 :: RuleM CoreExpr
#if WORD_SIZE_IN_BITS == 32
removeOp32 = do
  [e] <- getArgs
  return e
#else
removeOp32 = mzero
#endif

getArgs :: RuleM [CoreExpr]
getArgs = RuleM $ \_ _ args -> Just args

getInScopeEnv :: RuleM InScopeEnv
getInScopeEnv = RuleM $ \_ iu _ -> Just iu

-- return the n-th argument of this rule, if it is a literal
-- argument indices start from 0
getLiteral :: Int -> RuleM Literal
getLiteral n = RuleM $ \_ _ exprs -> case drop n exprs of
  (Lit l:_) -> Just l
  _ -> Nothing

unaryLit :: (DynFlags -> Literal -> Maybe CoreExpr) -> RuleM CoreExpr
unaryLit op = do
  dflags <- getDynFlags
  [Lit l] <- getArgs
  liftMaybe $ op dflags (convFloating dflags l)

binaryLit :: (DynFlags -> Literal -> Literal -> Maybe CoreExpr) -> RuleM CoreExpr
binaryLit op = do
  dflags <- getDynFlags
  [Lit l1, Lit l2] <- getArgs
  liftMaybe $ op dflags (convFloating dflags l1) (convFloating dflags l2)

binaryCmpLit :: (forall a . Ord a => a -> a -> Bool) -> RuleM CoreExpr
binaryCmpLit op = do
  dflags <- getDynFlags
  binaryLit (\_ -> cmpOp dflags op)

leftIdentity :: Literal -> RuleM CoreExpr
leftIdentity id_lit = leftIdentityDynFlags (const id_lit)

rightIdentity :: Literal -> RuleM CoreExpr
rightIdentity id_lit = rightIdentityDynFlags (const id_lit)

identity :: Literal -> RuleM CoreExpr
identity lit = leftIdentity lit `mplus` rightIdentity lit

leftIdentityDynFlags :: (DynFlags -> Literal) -> RuleM CoreExpr
leftIdentityDynFlags id_lit = do
  dflags <- getDynFlags
  [Lit l1, e2] <- getArgs
  guard $ l1 == id_lit dflags
  return e2

rightIdentityDynFlags :: (DynFlags -> Literal) -> RuleM CoreExpr
rightIdentityDynFlags id_lit = do
  dflags <- getDynFlags
  [e1, Lit l2] <- getArgs
  guard $ l2 == id_lit dflags
  return e1

identityDynFlags :: (DynFlags -> Literal) -> RuleM CoreExpr
identityDynFlags lit = leftIdentityDynFlags lit `mplus` rightIdentityDynFlags lit

leftZero :: (DynFlags -> Literal) -> RuleM CoreExpr
leftZero zero = do
  dflags <- getDynFlags
  [Lit l1, _] <- getArgs
  guard $ l1 == zero dflags
  return $ Lit l1

rightZero :: (DynFlags -> Literal) -> RuleM CoreExpr
rightZero zero = do
  dflags <- getDynFlags
  [_, Lit l2] <- getArgs
  guard $ l2 == zero dflags
  return $ Lit l2

zeroElem :: (DynFlags -> Literal) -> RuleM CoreExpr
zeroElem lit = leftZero lit `mplus` rightZero lit

equalArgs :: RuleM ()
equalArgs = do
  [e1, e2] <- getArgs
  guard $ e1 `cheapEqExpr` e2

nonZeroLit :: Int -> RuleM ()
nonZeroLit n = getLiteral n >>= guard . not . isZeroLit

-- When excess precision is not requested, cut down the precision of the
-- Rational value to that of Float/Double. We confuse host architecture
-- and target architecture here, but it's convenient (and wrong :-).
convFloating :: DynFlags -> Literal -> Literal
convFloating dflags (MachFloat  f) | not (gopt Opt_ExcessPrecision dflags) =
   MachFloat  (toRational (fromRational f :: Float ))
convFloating dflags (MachDouble d) | not (gopt Opt_ExcessPrecision dflags) =
   MachDouble (toRational (fromRational d :: Double))
convFloating _ l = l

guardFloatDiv :: RuleM ()
guardFloatDiv = do
  [Lit (MachFloat f1), Lit (MachFloat f2)] <- getArgs
  guard $ (f1 /=0 || f2 > 0) -- see Note [negative zero]
       && f2 /= 0            -- avoid NaN and Infinity/-Infinity

guardDoubleDiv :: RuleM ()
guardDoubleDiv = do
  [Lit (MachDouble d1), Lit (MachDouble d2)] <- getArgs
  guard $ (d1 /=0 || d2 > 0) -- see Note [negative zero]
       && d2 /= 0            -- avoid NaN and Infinity/-Infinity
-- Note [negative zero] Avoid (0 / -d), otherwise 0/(-1) reduces to
-- zero, but we might want to preserve the negative zero here which
-- is representable in Float/Double but not in (normalised)
-- Rational. (#3676) Perhaps we should generate (0 :% (-1)) instead?

strengthReduction :: Literal -> PrimOp -> RuleM CoreExpr
strengthReduction two_lit add_op = do -- Note [Strength reduction]
  arg <- msum [ do [arg, Lit mult_lit] <- getArgs
                   guard (mult_lit == two_lit)
                   return arg
              , do [Lit mult_lit, arg] <- getArgs
                   guard (mult_lit == two_lit)
                   return arg ]
  return $ Var (mkPrimOpId add_op) `App` arg `App` arg

-- Note [Strength reduction]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- This rule turns floating point multiplications of the form 2.0 * x and
-- x * 2.0 into x + x addition, because addition costs less than multiplication.
-- See #7116

-- Note [What's true and false]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- trueValInt and falseValInt represent true and false values returned by
-- comparison primops for Char, Int, Word, Integer, Double, Float and Addr.
-- True is represented as an unboxed 1# literal, while false is represented
-- as 0# literal.
-- We still need Bool data constructors (True and False) to use in a rule
-- for constant folding of equal Strings

trueValInt, falseValInt :: DynFlags -> Expr CoreBndr
trueValInt  dflags = Lit $ onei  dflags -- see Note [What's true and false]
falseValInt dflags = Lit $ zeroi dflags

trueValBool, falseValBool :: Expr CoreBndr
trueValBool   = Var trueDataConId -- see Note [What's true and false]
falseValBool  = Var falseDataConId

ltVal, eqVal, gtVal :: Expr CoreBndr
ltVal = Var ltDataConId
eqVal = Var eqDataConId
gtVal = Var gtDataConId

mkIntVal :: DynFlags -> Integer -> Expr CoreBndr
mkIntVal dflags i = Lit (mkMachInt dflags i)
mkWordVal :: DynFlags -> Integer -> Expr CoreBndr
mkWordVal dflags w = Lit (mkMachWord dflags w)
mkFloatVal :: DynFlags -> Rational -> Expr CoreBndr
mkFloatVal dflags f = Lit (convFloating dflags (MachFloat  f))
mkDoubleVal :: DynFlags -> Rational -> Expr CoreBndr
mkDoubleVal dflags d = Lit (convFloating dflags (MachDouble d))

matchPrimOpId :: PrimOp -> Id -> RuleM ()
matchPrimOpId op id = do
  op' <- liftMaybe $ isPrimOpId_maybe id
  guard $ op == op'

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
tagToEnumRule :: RuleM CoreExpr
-- If     data T a = A | B | C
-- then   tag2Enum# (T ty) 2# -->  B ty
tagToEnumRule = do
  [Type ty, Lit (MachInt i)] <- getArgs
  case splitTyConApp_maybe ty of
    Just (tycon, tc_args) | isEnumerationTyCon tycon -> do
      let tag = fromInteger i
          correct_tag dc = (dataConTag dc - fIRST_TAG) == tag
      (dc:rest) <- return $ filter correct_tag (tyConDataCons_maybe tycon `orElse` [])
      ASSERT(null rest) return ()
      return $ mkTyApps (Var (dataConWorkId dc)) tc_args

    -- See Note [tagToEnum#]
    _ -> WARN( True, ptext (sLit "tagToEnum# on non-enumeration type") <+> ppr ty )
         return $ mkRuntimeErrorApp rUNTIME_ERROR_ID ty "tagToEnum# on non-enumeration type"
\end{code}


For dataToTag#, we can reduce if either

        (a) the argument is a constructor
        (b) the argument is a variable whose unfolding is a known constructor

\begin{code}
dataToTagRule :: RuleM CoreExpr
dataToTagRule = a `mplus` b
  where
    a = do
      [Type ty1, Var tag_to_enum `App` Type ty2 `App` tag] <- getArgs
      guard $ tag_to_enum `hasKey` tagToEnumKey
      guard $ ty1 `eqType` ty2
      return tag -- dataToTag (tagToEnum x)   ==>   x
    b = do
      dflags <- getDynFlags
      [_, val_arg] <- getArgs
      in_scope <- getInScopeEnv
      (dc,_,_) <- liftMaybe $ exprIsConApp_maybe in_scope val_arg
      ASSERT( not (isNewTyCon (dataConTyCon dc)) ) return ()
      return $ mkIntVal dflags (toInteger (dataConTag dc - fIRST_TAG))
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Rules for seq# and spark#}
%*                                                                      *
%************************************************************************

\begin{code}
-- seq# :: forall a s . a -> State# s -> (# State# s, a #)
seqRule :: RuleM CoreExpr
seqRule = do
  [ty_a, Type ty_s, a, s] <- getArgs
  guard $ exprIsHNF a
  return $ mkConApp (tupleCon UnboxedTuple 2)
    [Type (mkStatePrimTy ty_s), ty_a, s, a]

-- spark# :: forall a s . a -> State# s -> (# State# s, a #)
sparkRule :: RuleM CoreExpr
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
                   ru_nargs = 4, ru_try = \_ _ _ -> match_append_lit },
     BuiltinRule { ru_name = fsLit "EqString", ru_fn = eqStringName,
                   ru_nargs = 2, ru_try = \dflags _ _ -> match_eq_string dflags },
     BuiltinRule { ru_name = fsLit "Inline", ru_fn = inlineIdName,
                   ru_nargs = 2, ru_try = \_ _ _ -> match_inline },
     BuiltinRule { ru_name = fsLit "MagicSingI", ru_fn = idName magicSingIId,
                   ru_nargs = 3, ru_try = \_ _ _ -> match_magicSingI }
     ]
 ++ builtinIntegerRules

builtinIntegerRules :: [CoreRule]
builtinIntegerRules =
 [rule_IntToInteger   "smallInteger"        smallIntegerName,
  rule_WordToInteger  "wordToInteger"       wordToIntegerName,
  rule_Int64ToInteger  "int64ToInteger"     int64ToIntegerName,
  rule_Word64ToInteger "word64ToInteger"    word64ToIntegerName,
  rule_convert        "integerToWord"       integerToWordName       mkWordLitWord,
  rule_convert        "integerToInt"        integerToIntName        mkIntLitInt,
  rule_convert        "integerToWord64"     integerToWord64Name     (\_ -> mkWord64LitWord64),
  rule_convert        "integerToInt64"      integerToInt64Name      (\_ -> mkInt64LitInt64),
  rule_binop          "plusInteger"         plusIntegerName         (+),
  rule_binop          "minusInteger"        minusIntegerName        (-),
  rule_binop          "timesInteger"        timesIntegerName        (*),
  rule_unop           "negateInteger"       negateIntegerName       negate,
  rule_binop_Prim     "eqInteger#"          eqIntegerPrimName       (==),
  rule_binop_Prim     "neqInteger#"         neqIntegerPrimName      (/=),
  rule_unop           "absInteger"          absIntegerName          abs,
  rule_unop           "signumInteger"       signumIntegerName       signum,
  rule_binop_Prim     "leInteger#"          leIntegerPrimName       (<=),
  rule_binop_Prim     "gtInteger#"          gtIntegerPrimName       (>),
  rule_binop_Prim     "ltInteger#"          ltIntegerPrimName       (<),
  rule_binop_Prim     "geInteger#"          geIntegerPrimName       (>=),
  rule_binop_Ordering "compareInteger"      compareIntegerName      compare,
  rule_encodeFloat    "encodeFloatInteger"  encodeFloatIntegerName  mkFloatLitFloat,
  rule_convert        "floatFromInteger"    floatFromIntegerName    (\_ -> mkFloatLitFloat),
  rule_encodeFloat    "encodeDoubleInteger" encodeDoubleIntegerName mkDoubleLitDouble,
  rule_decodeDouble   "decodeDoubleInteger" decodeDoubleIntegerName,
  rule_convert        "doubleFromInteger"   doubleFromIntegerName   (\_ -> mkDoubleLitDouble),
  rule_rationalTo     "rationalToFloat"     rationalToFloatName     mkFloatExpr,
  rule_rationalTo     "rationalToDouble"    rationalToDoubleName    mkDoubleExpr,
  rule_binop          "gcdInteger"          gcdIntegerName          gcd,
  rule_binop          "lcmInteger"          lcmIntegerName          lcm,
  rule_binop          "andInteger"          andIntegerName          (.&.),
  rule_binop          "orInteger"           orIntegerName           (.|.),
  rule_binop          "xorInteger"          xorIntegerName          xor,
  rule_unop           "complementInteger"   complementIntegerName   complement,
  rule_Int_binop      "shiftLInteger"       shiftLIntegerName       shiftL,
  rule_Int_binop      "shiftRInteger"       shiftRIntegerName       shiftR,
  -- See Note [Integer division constant folding] in libraries/base/GHC/Real.lhs
  rule_divop_one      "quotInteger"         quotIntegerName         quot,
  rule_divop_one      "remInteger"          remIntegerName          rem,
  rule_divop_one      "divInteger"          divIntegerName          div,
  rule_divop_one      "modInteger"          modIntegerName          mod,
  rule_divop_both     "divModInteger"       divModIntegerName       divMod,
  rule_divop_both     "quotRemInteger"      quotRemIntegerName      quotRem,
  -- These rules below don't actually have to be built in, but if we
  -- put them in the Haskell source then we'd have to duplicate them
  -- between all Integer implementations
  rule_XToIntegerToX "smallIntegerToInt"       integerToIntName    smallIntegerName,
  rule_XToIntegerToX "wordToIntegerToWord"     integerToWordName   wordToIntegerName,
  rule_XToIntegerToX "int64ToIntegerToInt64"   integerToInt64Name  int64ToIntegerName,
  rule_XToIntegerToX "word64ToIntegerToWord64" integerToWord64Name word64ToIntegerName,
  rule_smallIntegerTo "smallIntegerToWord"   integerToWordName     Int2WordOp,
  rule_smallIntegerTo "smallIntegerToFloat"  floatFromIntegerName  Int2FloatOp,
  rule_smallIntegerTo "smallIntegerToDouble" doubleFromIntegerName Int2DoubleOp
  ]
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
          rule_binop_Prim str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_binop_Prim op }
          rule_binop_Ordering str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_binop_Ordering op }
          rule_encodeFloat str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_Int_encodeFloat op }
          rule_decodeDouble str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_decodeDouble }
          rule_XToIntegerToX str name toIntegerName
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_XToIntegerToX toIntegerName }
          rule_smallIntegerTo str name primOp
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_smallIntegerTo primOp }
          rule_rationalTo str name mkLit
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_rationalTo mkLit }

---------------------------------------------------
-- The rule is this:
--      unpackFoldrCString# "foo" c (unpackFoldrCString# "baz" c n)
--      =  unpackFoldrCString# "foobaz" c n

match_append_lit :: [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_append_lit [Type ty1,
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
                   `App` Lit (MachStr (s1 `BS.append` s2))
                   `App` c1
                   `App` n)

match_append_lit _ = Nothing

---------------------------------------------------
-- The rule is this:
--      eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2) = s1==s2

match_eq_string :: DynFlags -> [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_eq_string _ [Var unpk1 `App` Lit (MachStr s1),
                        Var unpk2 `App` Lit (MachStr s2)]
  | unpk1 `hasKey` unpackCStringIdKey,
    unpk2 `hasKey` unpackCStringIdKey
  = Just (if s1 == s2 then trueValBool else falseValBool)

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
match_inline :: [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_inline (Type _ : e : _)
  | (Var f, args1) <- collectArgs e,
    Just unf <- maybeUnfoldingTemplate (realIdUnfolding f)
             -- Ignore the IdUnfoldingFun here!
  = Just (mkApps unf args1)

match_inline _ = Nothing


-- See Note [magicSingIId magic] in `basicTypes/MkId.lhs`
-- for a description of what is going on here.
match_magicSingI :: [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_magicSingI (Type t : e : Lam b _ : _)
  | ((_ : _ : fu : _),_)  <- splitFunTys t
  , (sI_type,_)           <- splitFunTy fu
  , Just (sI_tc,xs)       <- splitTyConApp_maybe sI_type
  , Just (_,_,co)         <- unwrapNewTyCon_maybe sI_tc
  = Just $ let f = setVarType b fu
           in Lam f $ Var f `App` Cast e (mkSymCo (mkUnbranchedAxInstCo Representational co xs))

match_magicSingI _ = Nothing

-------------------------------------------------
-- Integer rules
--   smallInteger  (79::Int#)  = 79::Integer
--   wordToInteger (79::Word#) = 79::Integer
-- Similarly Int64, Word64

match_IntToInteger :: RuleFun
match_IntToInteger _ id_unf fn [xl]
  | Just (MachInt x) <- exprIsLiteral_maybe id_unf xl
  = case idType fn of
    FunTy _ integerTy ->
        Just (Lit (LitInteger x integerTy))
    _ ->
        panic "match_IntToInteger: Id has the wrong type"
match_IntToInteger _ _ _ _ = Nothing

match_WordToInteger :: RuleFun
match_WordToInteger _ id_unf id [xl]
  | Just (MachWord x) <- exprIsLiteral_maybe id_unf xl
  = case idType id of
    FunTy _ integerTy ->
        Just (Lit (LitInteger x integerTy))
    _ ->
        panic "match_WordToInteger: Id has the wrong type"
match_WordToInteger _ _ _ _ = Nothing

match_Int64ToInteger :: RuleFun
match_Int64ToInteger _ id_unf id [xl]
  | Just (MachInt64 x) <- exprIsLiteral_maybe id_unf xl
  = case idType id of
    FunTy _ integerTy ->
        Just (Lit (LitInteger x integerTy))
    _ ->
        panic "match_Int64ToInteger: Id has the wrong type"
match_Int64ToInteger _ _ _ _ = Nothing

match_Word64ToInteger :: RuleFun
match_Word64ToInteger _ id_unf id [xl]
  | Just (MachWord64 x) <- exprIsLiteral_maybe id_unf xl
  = case idType id of
    FunTy _ integerTy ->
        Just (Lit (LitInteger x integerTy))
    _ ->
        panic "match_Word64ToInteger: Id has the wrong type"
match_Word64ToInteger _ _ _ _ = Nothing

-------------------------------------------------
match_Integer_convert :: Num a
                      => (DynFlags -> a -> Expr CoreBndr)
                      -> RuleFun
match_Integer_convert convert dflags id_unf _ [xl]
  | Just (LitInteger x _) <- exprIsLiteral_maybe id_unf xl
  = Just (convert dflags (fromInteger x))
match_Integer_convert _ _ _ _ _ = Nothing

match_Integer_unop :: (Integer -> Integer) -> RuleFun
match_Integer_unop unop _ id_unf _ [xl]
  | Just (LitInteger x i) <- exprIsLiteral_maybe id_unf xl
  = Just (Lit (LitInteger (unop x) i))
match_Integer_unop _ _ _ _ _ = Nothing

match_Integer_binop :: (Integer -> Integer -> Integer) -> RuleFun
match_Integer_binop binop _ id_unf _ [xl,yl]
  | Just (LitInteger x i) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  = Just (Lit (LitInteger (x `binop` y) i))
match_Integer_binop _ _ _ _ _ = Nothing

-- This helper is used for the quotRem and divMod functions
match_Integer_divop_both
   :: (Integer -> Integer -> (Integer, Integer)) -> RuleFun
match_Integer_divop_both divop _ id_unf _ [xl,yl]
  | Just (LitInteger x t) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  , y /= 0
  , (r,s) <- x `divop` y
  = Just $ mkConApp (tupleCon UnboxedTuple 2)
                    [Type t,
                     Type t,
                     Lit (LitInteger r t),
                     Lit (LitInteger s t)]
match_Integer_divop_both _ _ _ _ _ = Nothing

-- This helper is used for the quot and rem functions
match_Integer_divop_one :: (Integer -> Integer -> Integer) -> RuleFun
match_Integer_divop_one divop _ id_unf _ [xl,yl]
  | Just (LitInteger x i) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  , y /= 0
  = Just (Lit (LitInteger (x `divop` y) i))
match_Integer_divop_one _ _ _ _ _ = Nothing

match_Integer_Int_binop :: (Integer -> Int -> Integer) -> RuleFun
match_Integer_Int_binop binop _ id_unf _ [xl,yl]
  | Just (LitInteger x i) <- exprIsLiteral_maybe id_unf xl
  , Just (MachInt y)      <- exprIsLiteral_maybe id_unf yl
  = Just (Lit (LitInteger (x `binop` fromIntegral y) i))
match_Integer_Int_binop _ _ _ _ _ = Nothing

match_Integer_binop_Prim :: (Integer -> Integer -> Bool) -> RuleFun
match_Integer_binop_Prim binop dflags id_unf _ [xl, yl]
  | Just (LitInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  = Just (if x `binop` y then trueValInt dflags else falseValInt dflags)
match_Integer_binop_Prim _ _ _ _ _ = Nothing

match_Integer_binop_Ordering :: (Integer -> Integer -> Ordering) -> RuleFun
match_Integer_binop_Ordering binop _ id_unf _ [xl, yl]
  | Just (LitInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  = Just $ case x `binop` y of
             LT -> ltVal
             EQ -> eqVal
             GT -> gtVal
match_Integer_binop_Ordering _ _ _ _ _ = Nothing

match_Integer_Int_encodeFloat :: RealFloat a
                              => (a -> Expr CoreBndr)
                              -> RuleFun
match_Integer_Int_encodeFloat mkLit _ id_unf _ [xl,yl]
  | Just (LitInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (MachInt y)      <- exprIsLiteral_maybe id_unf yl
  = Just (mkLit $ encodeFloat x (fromInteger y))
match_Integer_Int_encodeFloat _ _ _ _ _ = Nothing

---------------------------------------------------
-- constant folding for Float/Double
--
-- This turns
--      rationalToFloat n d
-- into a literal Float, and similarly for Doubles.
--
-- it's important to not match d == 0, because that may represent a
-- literal "0/0" or similar, and we can't produce a literal value for
-- NaN or +-Inf
match_rationalTo :: RealFloat a
                 => (a -> Expr CoreBndr)
                 -> RuleFun
match_rationalTo mkLit _ id_unf _ [xl, yl]
  | Just (LitInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (LitInteger y _) <- exprIsLiteral_maybe id_unf yl
  , y /= 0
  = Just (mkLit (fromRational (x % y)))
match_rationalTo _ _ _ _ _ = Nothing

match_decodeDouble :: RuleFun
match_decodeDouble _ id_unf fn [xl]
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
match_decodeDouble _ _ _ _ = Nothing

match_XToIntegerToX :: Name -> RuleFun
match_XToIntegerToX n _ _ _ [App (Var x) y]
  | idName x == n
  = Just y
match_XToIntegerToX _ _ _ _ _ = Nothing

match_smallIntegerTo :: PrimOp -> RuleFun
match_smallIntegerTo primOp _ _ _ [App (Var x) y]
  | idName x == smallIntegerName
  = Just $ App (Var (mkPrimOpId primOp)) y
match_smallIntegerTo _ _ _ _ _ = Nothing
\end{code}
