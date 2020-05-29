{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[ConFold]{Constant Folder}

Conceptually, constant folding should be parameterized with the kind
of target machine to get identical behaviour during compilation time
and runtime. We cheat a little bit here...

ToDo:
   check boundaries before folding, e.g. we can fold the Float addition
   (i1 + i2) only if it results in a valid Float.
-}

{-# LANGUAGE CPP, RankNTypes, PatternSynonyms, ViewPatterns, RecordWildCards,
    DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE -Wno-incomplete-uni-patterns #-}

module GHC.Core.Opt.ConstantFold
   ( primOpRules
   , builtinRules
   , caseRules
   )
where

#include "HsVersions.h"

import GHC.Prelude

import {-# SOURCE #-} GHC.Types.Id.Make ( mkPrimOpId, magicDictId )

import GHC.Core
import GHC.Core.Make
import GHC.Types.Id
import GHC.Types.Literal
import GHC.Core.SimpleOpt ( exprIsLiteral_maybe )
import GHC.Builtin.PrimOps ( PrimOp(..), tagToEnumKey )
import GHC.Builtin.Types
import GHC.Builtin.Types.Prim
import GHC.Core.TyCon
   ( tyConDataCons_maybe, isAlgTyCon, isEnumerationTyCon
   , isNewTyCon, unwrapNewTyCon_maybe, tyConDataCons
   , tyConFamilySize )
import GHC.Core.DataCon ( dataConTagZ, dataConTyCon, dataConWrapId, dataConWorkId )
import GHC.Core.Utils  ( eqExpr, cheapEqExpr, exprIsHNF, exprType
                       , stripTicksTop, stripTicksTopT, mkTicks )
import GHC.Core.Unfold ( exprIsConApp_maybe )
import GHC.Core.FVs
import GHC.Core.Type
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Name.Occurrence ( occNameFS )
import GHC.Builtin.Names
import GHC.Data.Maybe      ( orElse )
import GHC.Types.Name ( Name, nameOccName )
import GHC.Utils.Outputable
import GHC.Data.FastString
import GHC.Types.Basic
import GHC.Platform
import GHC.Utils.Misc
import GHC.Core.Coercion   (mkUnbranchedAxInstCo,mkSymCo,Role(..))

import Control.Applicative ( Alternative(..) )

import Control.Monad
import Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.Int
import Data.Ratio
import Data.Word
import Data.Maybe (fromMaybe)

{-
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
-}

primOpRules ::  Name -> PrimOp -> Maybe CoreRule
primOpRules nm = \case
   TagToEnumOp -> mkPrimOpRule nm 2 [ tagToEnumRule ]
   DataToTagOp -> mkPrimOpRule nm 2 [ dataToTagRule ]

   -- Int operations
   IntAddOp    -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (+))
                                    , identityPlatform zeroi
                                    , numFoldingRules IntAddOp intPrimOps
                                    ]
   IntSubOp    -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (-))
                                    , rightIdentityPlatform zeroi
                                    , equalArgs >> retLit zeroi
                                    , numFoldingRules IntSubOp intPrimOps
                                    ]
   IntAddCOp   -> mkPrimOpRule nm 2 [ binaryLit (intOpC2 (+))
                                    , identityCPlatform zeroi ]
   IntSubCOp   -> mkPrimOpRule nm 2 [ binaryLit (intOpC2 (-))
                                    , rightIdentityCPlatform zeroi
                                    , equalArgs >> retLitNoC zeroi ]
   IntMulOp    -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (*))
                                    , zeroElem zeroi
                                    , identityPlatform onei
                                    , numFoldingRules IntMulOp intPrimOps
                                    ]
   IntQuotOp   -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (intOp2 quot)
                                    , leftZero zeroi
                                    , rightIdentityPlatform onei
                                    , equalArgs >> retLit onei ]
   IntRemOp    -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (intOp2 rem)
                                    , leftZero zeroi
                                    , do l <- getLiteral 1
                                         platform <- getPlatform
                                         guard (l == onei platform)
                                         retLit zeroi
                                    , equalArgs >> retLit zeroi
                                    , equalArgs >> retLit zeroi ]
   AndIOp      -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (.&.))
                                    , idempotent
                                    , zeroElem zeroi ]
   OrIOp       -> mkPrimOpRule nm 2 [ binaryLit (intOp2 (.|.))
                                    , idempotent
                                    , identityPlatform zeroi ]
   XorIOp      -> mkPrimOpRule nm 2 [ binaryLit (intOp2 xor)
                                    , identityPlatform zeroi
                                    , equalArgs >> retLit zeroi ]
   NotIOp      -> mkPrimOpRule nm 1 [ unaryLit complementOp
                                    , inversePrimOp NotIOp ]
   IntNegOp    -> mkPrimOpRule nm 1 [ unaryLit negOp
                                    , inversePrimOp IntNegOp ]
   ISllOp      -> mkPrimOpRule nm 2 [ shiftRule (const Bits.shiftL)
                                    , rightIdentityPlatform zeroi ]
   ISraOp      -> mkPrimOpRule nm 2 [ shiftRule (const Bits.shiftR)
                                    , rightIdentityPlatform zeroi ]
   ISrlOp      -> mkPrimOpRule nm 2 [ shiftRule shiftRightLogical
                                    , rightIdentityPlatform zeroi ]

   -- Word operations
   WordAddOp   -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (+))
                                    , identityPlatform zerow
                                    , numFoldingRules WordAddOp wordPrimOps
                                    ]
   WordSubOp   -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (-))
                                    , rightIdentityPlatform zerow
                                    , equalArgs >> retLit zerow
                                    , numFoldingRules WordSubOp wordPrimOps
                                    ]
   WordAddCOp  -> mkPrimOpRule nm 2 [ binaryLit (wordOpC2 (+))
                                    , identityCPlatform zerow ]
   WordSubCOp  -> mkPrimOpRule nm 2 [ binaryLit (wordOpC2 (-))
                                    , rightIdentityCPlatform zerow
                                    , equalArgs >> retLitNoC zerow ]
   WordMulOp   -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (*))
                                    , identityPlatform onew
                                    , numFoldingRules WordMulOp wordPrimOps
                                    ]
   WordQuotOp  -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (wordOp2 quot)
                                    , rightIdentityPlatform onew ]
   WordRemOp   -> mkPrimOpRule nm 2 [ nonZeroLit 1 >> binaryLit (wordOp2 rem)
                                    , leftZero zerow
                                    , do l <- getLiteral 1
                                         platform <- getPlatform
                                         guard (l == onew platform)
                                         retLit zerow
                                    , equalArgs >> retLit zerow ]
   AndOp       -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (.&.))
                                    , idempotent
                                    , zeroElem zerow ]
   OrOp        -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 (.|.))
                                    , idempotent
                                    , identityPlatform zerow ]
   XorOp       -> mkPrimOpRule nm 2 [ binaryLit (wordOp2 xor)
                                    , identityPlatform zerow
                                    , equalArgs >> retLit zerow ]
   NotOp       -> mkPrimOpRule nm 1 [ unaryLit complementOp
                                    , inversePrimOp NotOp ]
   SllOp       -> mkPrimOpRule nm 2 [ shiftRule (const Bits.shiftL) ]
   SrlOp       -> mkPrimOpRule nm 2 [ shiftRule shiftRightLogical ]

   -- coercions
   Word2IntOp     -> mkPrimOpRule nm 1 [ liftLitPlatform word2IntLit
                                       , inversePrimOp Int2WordOp ]
   Int2WordOp     -> mkPrimOpRule nm 1 [ liftLitPlatform int2WordLit
                                       , inversePrimOp Word2IntOp ]
   Narrow8IntOp   -> mkPrimOpRule nm 1 [ liftLit narrow8IntLit
                                       , subsumedByPrimOp Narrow8IntOp
                                       , Narrow8IntOp `subsumesPrimOp` Narrow16IntOp
                                       , Narrow8IntOp `subsumesPrimOp` Narrow32IntOp
                                       , narrowSubsumesAnd AndIOp Narrow8IntOp 8 ]
   Narrow16IntOp  -> mkPrimOpRule nm 1 [ liftLit narrow16IntLit
                                       , subsumedByPrimOp Narrow8IntOp
                                       , subsumedByPrimOp Narrow16IntOp
                                       , Narrow16IntOp `subsumesPrimOp` Narrow32IntOp
                                       , narrowSubsumesAnd AndIOp Narrow16IntOp 16 ]
   Narrow32IntOp  -> mkPrimOpRule nm 1 [ liftLit narrow32IntLit
                                       , subsumedByPrimOp Narrow8IntOp
                                       , subsumedByPrimOp Narrow16IntOp
                                       , subsumedByPrimOp Narrow32IntOp
                                       , removeOp32
                                       , narrowSubsumesAnd AndIOp Narrow32IntOp 32 ]
   Narrow8WordOp  -> mkPrimOpRule nm 1 [ liftLit narrow8WordLit
                                       , subsumedByPrimOp Narrow8WordOp
                                       , Narrow8WordOp `subsumesPrimOp` Narrow16WordOp
                                       , Narrow8WordOp `subsumesPrimOp` Narrow32WordOp
                                       , narrowSubsumesAnd AndOp Narrow8WordOp 8 ]
   Narrow16WordOp -> mkPrimOpRule nm 1 [ liftLit narrow16WordLit
                                       , subsumedByPrimOp Narrow8WordOp
                                       , subsumedByPrimOp Narrow16WordOp
                                       , Narrow16WordOp `subsumesPrimOp` Narrow32WordOp
                                       , narrowSubsumesAnd AndOp Narrow16WordOp 16 ]
   Narrow32WordOp -> mkPrimOpRule nm 1 [ liftLit narrow32WordLit
                                       , subsumedByPrimOp Narrow8WordOp
                                       , subsumedByPrimOp Narrow16WordOp
                                       , subsumedByPrimOp Narrow32WordOp
                                       , removeOp32
                                       , narrowSubsumesAnd AndOp Narrow32WordOp 32 ]
   OrdOp          -> mkPrimOpRule nm 1 [ liftLit char2IntLit
                                       , inversePrimOp ChrOp ]
   ChrOp          -> mkPrimOpRule nm 1 [ do [Lit lit] <- getArgs
                                            guard (litFitsInChar lit)
                                            liftLit int2CharLit
                                       , inversePrimOp OrdOp ]
   Float2IntOp    -> mkPrimOpRule nm 1 [ liftLit float2IntLit ]
   Int2FloatOp    -> mkPrimOpRule nm 1 [ liftLit int2FloatLit ]
   Double2IntOp   -> mkPrimOpRule nm 1 [ liftLit double2IntLit ]
   Int2DoubleOp   -> mkPrimOpRule nm 1 [ liftLit int2DoubleLit ]
   -- SUP: Not sure what the standard says about precision in the following 2 cases
   Float2DoubleOp -> mkPrimOpRule nm 1 [ liftLit float2DoubleLit ]
   Double2FloatOp -> mkPrimOpRule nm 1 [ liftLit double2FloatLit ]

   -- Float
   FloatAddOp   -> mkPrimOpRule nm 2 [ binaryLit (floatOp2 (+))
                                     , identity zerof ]
   FloatSubOp   -> mkPrimOpRule nm 2 [ binaryLit (floatOp2 (-))
                                     , rightIdentity zerof ]
   FloatMulOp   -> mkPrimOpRule nm 2 [ binaryLit (floatOp2 (*))
                                     , identity onef
                                     , strengthReduction twof FloatAddOp  ]
             -- zeroElem zerof doesn't hold because of NaN
   FloatDivOp   -> mkPrimOpRule nm 2 [ guardFloatDiv >> binaryLit (floatOp2 (/))
                                     , rightIdentity onef ]
   FloatNegOp   -> mkPrimOpRule nm 1 [ unaryLit negOp
                                     , inversePrimOp FloatNegOp ]

   -- Double
   DoubleAddOp   -> mkPrimOpRule nm 2 [ binaryLit (doubleOp2 (+))
                                      , identity zerod ]
   DoubleSubOp   -> mkPrimOpRule nm 2 [ binaryLit (doubleOp2 (-))
                                      , rightIdentity zerod ]
   DoubleMulOp   -> mkPrimOpRule nm 2 [ binaryLit (doubleOp2 (*))
                                      , identity oned
                                      , strengthReduction twod DoubleAddOp  ]
              -- zeroElem zerod doesn't hold because of NaN
   DoubleDivOp   -> mkPrimOpRule nm 2 [ guardDoubleDiv >> binaryLit (doubleOp2 (/))
                                      , rightIdentity oned ]
   DoubleNegOp   -> mkPrimOpRule nm 1 [ unaryLit negOp
                                      , inversePrimOp DoubleNegOp ]

   -- Relational operators

   IntEqOp    -> mkRelOpRule nm (==) [ litEq True ]
   IntNeOp    -> mkRelOpRule nm (/=) [ litEq False ]
   CharEqOp   -> mkRelOpRule nm (==) [ litEq True ]
   CharNeOp   -> mkRelOpRule nm (/=) [ litEq False ]

   IntGtOp    -> mkRelOpRule nm (>)  [ boundsCmp Gt ]
   IntGeOp    -> mkRelOpRule nm (>=) [ boundsCmp Ge ]
   IntLeOp    -> mkRelOpRule nm (<=) [ boundsCmp Le ]
   IntLtOp    -> mkRelOpRule nm (<)  [ boundsCmp Lt ]

   CharGtOp   -> mkRelOpRule nm (>)  [ boundsCmp Gt ]
   CharGeOp   -> mkRelOpRule nm (>=) [ boundsCmp Ge ]
   CharLeOp   -> mkRelOpRule nm (<=) [ boundsCmp Le ]
   CharLtOp   -> mkRelOpRule nm (<)  [ boundsCmp Lt ]

   FloatGtOp  -> mkFloatingRelOpRule nm (>)
   FloatGeOp  -> mkFloatingRelOpRule nm (>=)
   FloatLeOp  -> mkFloatingRelOpRule nm (<=)
   FloatLtOp  -> mkFloatingRelOpRule nm (<)
   FloatEqOp  -> mkFloatingRelOpRule nm (==)
   FloatNeOp  -> mkFloatingRelOpRule nm (/=)

   DoubleGtOp -> mkFloatingRelOpRule nm (>)
   DoubleGeOp -> mkFloatingRelOpRule nm (>=)
   DoubleLeOp -> mkFloatingRelOpRule nm (<=)
   DoubleLtOp -> mkFloatingRelOpRule nm (<)
   DoubleEqOp -> mkFloatingRelOpRule nm (==)
   DoubleNeOp -> mkFloatingRelOpRule nm (/=)

   WordGtOp   -> mkRelOpRule nm (>)  [ boundsCmp Gt ]
   WordGeOp   -> mkRelOpRule nm (>=) [ boundsCmp Ge ]
   WordLeOp   -> mkRelOpRule nm (<=) [ boundsCmp Le ]
   WordLtOp   -> mkRelOpRule nm (<)  [ boundsCmp Lt ]
   WordEqOp   -> mkRelOpRule nm (==) [ litEq True ]
   WordNeOp   -> mkRelOpRule nm (/=) [ litEq False ]

   AddrAddOp  -> mkPrimOpRule nm 2 [ rightIdentityPlatform zeroi ]

   SeqOp      -> mkPrimOpRule nm 4 [ seqRule ]
   SparkOp    -> mkPrimOpRule nm 4 [ sparkRule ]

   _          -> Nothing

{-
************************************************************************
*                                                                      *
\subsection{Doing the business}
*                                                                      *
************************************************************************
-}

-- useful shorthands
mkPrimOpRule :: Name -> Int -> [RuleM CoreExpr] -> Maybe CoreRule
mkPrimOpRule nm arity rules = Just $ mkBasicRule nm arity (msum rules)

mkRelOpRule :: Name -> (forall a . Ord a => a -> a -> Bool)
            -> [RuleM CoreExpr] -> Maybe CoreRule
mkRelOpRule nm cmp extra
  = mkPrimOpRule nm 2 $
    binaryCmpLit cmp : equal_rule : extra
  where
        -- x `cmp` x does not depend on x, so
        -- compute it for the arbitrary value 'True'
        -- and use that result
    equal_rule = do { equalArgs
                    ; platform <- getPlatform
                    ; return (if cmp True True
                              then trueValInt  platform
                              else falseValInt platform) }

{- Note [Rules for floating-point comparisons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need different rules for floating-point values because for floats
it is not true that x = x (for NaNs); so we do not want the equal_rule
rule that mkRelOpRule uses.

Note also that, in the case of equality/inequality, we do /not/
want to switch to a case-expression.  For example, we do not want
to convert
   case (eqFloat# x 3.8#) of
     True -> this
     False -> that
to
  case x of
    3.8#::Float# -> this
    _            -> that
See #9238.  Reason: comparing floating-point values for equality
delicate, and we don't want to implement that delicacy in the code for
case expressions.  So we make it an invariant of Core that a case
expression never scrutinises a Float# or Double#.

This transformation is what the litEq rule does;
see Note [The litEq rule: converting equality to case].
So we /refrain/ from using litEq for mkFloatingRelOpRule.
-}

mkFloatingRelOpRule :: Name -> (forall a . Ord a => a -> a -> Bool)
                    -> Maybe CoreRule
-- See Note [Rules for floating-point comparisons]
mkFloatingRelOpRule nm cmp
  = mkPrimOpRule nm 2 [binaryCmpLit cmp]

-- common constants
zeroi, onei, zerow, onew :: Platform -> Literal
zeroi platform = mkLitInt  platform 0
onei  platform = mkLitInt  platform 1
zerow platform = mkLitWord platform 0
onew  platform = mkLitWord platform 1

zerof, onef, twof, zerod, oned, twod :: Literal
zerof = mkLitFloat 0.0
onef  = mkLitFloat 1.0
twof  = mkLitFloat 2.0
zerod = mkLitDouble 0.0
oned  = mkLitDouble 1.0
twod  = mkLitDouble 2.0

cmpOp :: Platform -> (forall a . Ord a => a -> a -> Bool)
      -> Literal -> Literal -> Maybe CoreExpr
cmpOp platform cmp = go
  where
    done True  = Just $ trueValInt  platform
    done False = Just $ falseValInt platform

    -- These compares are at different types
    go (LitChar i1)   (LitChar i2)   = done (i1 `cmp` i2)
    go (LitFloat i1)  (LitFloat i2)  = done (i1 `cmp` i2)
    go (LitDouble i1) (LitDouble i2) = done (i1 `cmp` i2)
    go (LitNumber nt1 i1 _) (LitNumber nt2 i2 _)
      | nt1 /= nt2 = Nothing
      | otherwise  = done (i1 `cmp` i2)
    go _               _               = Nothing

--------------------------

negOp :: RuleOpts -> Literal -> Maybe CoreExpr  -- Negate
negOp env = \case
   (LitFloat 0.0)  -> Nothing  -- can't represent -0.0 as a Rational
   (LitFloat f)    -> Just (mkFloatVal env (-f))
   (LitDouble 0.0) -> Nothing
   (LitDouble d)   -> Just (mkDoubleVal env (-d))
   (LitNumber nt i t)
      | litNumIsSigned nt -> Just (Lit (mkLitNumberWrap (roPlatform env) nt (-i) t))
   _ -> Nothing

complementOp :: RuleOpts -> Literal -> Maybe CoreExpr  -- Binary complement
complementOp env (LitNumber nt i t) =
   Just (Lit (mkLitNumberWrap (roPlatform env) nt (complement i) t))
complementOp _      _            = Nothing

--------------------------
intOp2 :: (Integral a, Integral b)
       => (a -> b -> Integer)
       -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
intOp2 = intOp2' . const

intOp2' :: (Integral a, Integral b)
        => (RuleOpts -> a -> b -> Integer)
        -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
intOp2' op env (LitNumber LitNumInt i1 _) (LitNumber LitNumInt i2 _) =
  let o = op env
  in  intResult (roPlatform env) (fromInteger i1 `o` fromInteger i2)
intOp2' _  _      _            _            = Nothing  -- Could find LitLit

intOpC2 :: (Integral a, Integral b)
        => (a -> b -> Integer)
        -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
intOpC2 op env (LitNumber LitNumInt i1 _) (LitNumber LitNumInt i2 _) = do
  intCResult (roPlatform env) (fromInteger i1 `op` fromInteger i2)
intOpC2 _  _      _            _            = Nothing  -- Could find LitLit

shiftRightLogical :: Platform -> Integer -> Int -> Integer
-- Shift right, putting zeros in rather than sign-propagating as Bits.shiftR would do
-- Do this by converting to Word and back.  Obviously this won't work for big
-- values, but its ok as we use it here
shiftRightLogical platform x n =
    case platformWordSize platform of
      PW4 -> fromIntegral (fromInteger x `shiftR` n :: Word32)
      PW8 -> fromIntegral (fromInteger x `shiftR` n :: Word64)

--------------------------
retLit :: (Platform -> Literal) -> RuleM CoreExpr
retLit l = do platform <- getPlatform
              return $ Lit $ l platform

retLitNoC :: (Platform -> Literal) -> RuleM CoreExpr
retLitNoC l = do platform <- getPlatform
                 let lit = l platform
                 let ty = literalType lit
                 return $ mkCoreUbxTup [ty, ty] [Lit lit, Lit (zeroi platform)]

wordOp2 :: (Integral a, Integral b)
        => (a -> b -> Integer)
        -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
wordOp2 op env (LitNumber LitNumWord w1 _) (LitNumber LitNumWord w2 _)
    = wordResult (roPlatform env) (fromInteger w1 `op` fromInteger w2)
wordOp2 _ _ _ _ = Nothing  -- Could find LitLit

wordOpC2 :: (Integral a, Integral b)
        => (a -> b -> Integer)
        -> RuleOpts -> Literal -> Literal -> Maybe CoreExpr
wordOpC2 op env (LitNumber LitNumWord w1 _) (LitNumber LitNumWord w2 _) =
  wordCResult (roPlatform env) (fromInteger w1 `op` fromInteger w2)
wordOpC2 _ _ _ _ = Nothing  -- Could find LitLit

shiftRule :: (Platform -> Integer -> Int -> Integer) -> RuleM CoreExpr
-- Shifts take an Int; hence third arg of op is Int
-- Used for shift primops
--    ISllOp, ISraOp, ISrlOp :: Word# -> Int# -> Word#
--    SllOp, SrlOp           :: Word# -> Int# -> Word#
shiftRule shift_op
  = do { platform <- getPlatform
       ; [e1, Lit (LitNumber LitNumInt shift_len _)] <- getArgs
       ; case e1 of
           _ | shift_len == 0
             -> return e1
             -- See Note [Guarding against silly shifts]
             | shift_len < 0 || shift_len > toInteger (platformWordSizeInBits platform)
             -> return $ Lit $ mkLitNumberWrap platform LitNumInt 0 (exprType e1)

           -- Do the shift at type Integer, but shift length is Int
           Lit (LitNumber nt x t)
             | 0 < shift_len
             , shift_len <= toInteger (platformWordSizeInBits platform)
             -> let op = shift_op platform
                    y  = x `op` fromInteger shift_len
                in  liftMaybe $ Just (Lit (mkLitNumberWrap platform nt y t))

           _ -> mzero }

--------------------------
floatOp2 :: (Rational -> Rational -> Rational)
         -> RuleOpts -> Literal -> Literal
         -> Maybe (Expr CoreBndr)
floatOp2 op env (LitFloat f1) (LitFloat f2)
  = Just (mkFloatVal env (f1 `op` f2))
floatOp2 _ _ _ _ = Nothing

--------------------------
doubleOp2 :: (Rational -> Rational -> Rational)
          -> RuleOpts -> Literal -> Literal
          -> Maybe (Expr CoreBndr)
doubleOp2 op env (LitDouble f1) (LitDouble f2)
  = Just (mkDoubleVal env (f1 `op` f2))
doubleOp2 _ _ _ _ = Nothing

--------------------------
{- Note [The litEq rule: converting equality to case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This stuff turns
     n ==# 3#
into
     case n of
       3# -> True
       m  -> False

This is a Good Thing, because it allows case-of case things
to happen, and case-default absorption to happen.  For
example:

     if (n ==# 3#) || (n ==# 4#) then e1 else e2
will transform to
     case n of
       3# -> e1
       4# -> e1
       m  -> e2
(modulo the usual precautions to avoid duplicating e1)
-}

litEq :: Bool  -- True <=> equality, False <=> inequality
      -> RuleM CoreExpr
litEq is_eq = msum
  [ do [Lit lit, expr] <- getArgs
       platform <- getPlatform
       do_lit_eq platform lit expr
  , do [expr, Lit lit] <- getArgs
       platform <- getPlatform
       do_lit_eq platform lit expr ]
  where
    do_lit_eq platform lit expr = do
      guard (not (litIsLifted lit))
      return (mkWildCase expr (literalType lit) intPrimTy
                    [(DEFAULT,    [], val_if_neq),
                     (LitAlt lit, [], val_if_eq)])
      where
        val_if_eq  | is_eq     = trueValInt  platform
                   | otherwise = falseValInt platform
        val_if_neq | is_eq     = falseValInt platform
                   | otherwise = trueValInt  platform


-- | Check if there is comparison with minBound or maxBound, that is
-- always true or false. For instance, an Int cannot be smaller than its
-- minBound, so we can replace such comparison with False.
boundsCmp :: Comparison -> RuleM CoreExpr
boundsCmp op = do
  platform <- getPlatform
  [a, b] <- getArgs
  liftMaybe $ mkRuleFn platform op a b

data Comparison = Gt | Ge | Lt | Le

mkRuleFn :: Platform -> Comparison -> CoreExpr -> CoreExpr -> Maybe CoreExpr
mkRuleFn platform Gt (Lit lit) _ | isMinBound platform lit = Just $ falseValInt platform
mkRuleFn platform Le (Lit lit) _ | isMinBound platform lit = Just $ trueValInt  platform
mkRuleFn platform Ge _ (Lit lit) | isMinBound platform lit = Just $ trueValInt  platform
mkRuleFn platform Lt _ (Lit lit) | isMinBound platform lit = Just $ falseValInt platform
mkRuleFn platform Ge (Lit lit) _ | isMaxBound platform lit = Just $ trueValInt  platform
mkRuleFn platform Lt (Lit lit) _ | isMaxBound platform lit = Just $ falseValInt platform
mkRuleFn platform Gt _ (Lit lit) | isMaxBound platform lit = Just $ falseValInt platform
mkRuleFn platform Le _ (Lit lit) | isMaxBound platform lit = Just $ trueValInt  platform
mkRuleFn _ _ _ _                                           = Nothing

isMinBound :: Platform -> Literal -> Bool
isMinBound _        (LitChar c)        = c == minBound
isMinBound platform (LitNumber nt i _) = case nt of
   LitNumInt     -> i == platformMinInt platform
   LitNumInt64   -> i == toInteger (minBound :: Int64)
   LitNumWord    -> i == 0
   LitNumWord64  -> i == 0
   LitNumNatural -> i == 0
   LitNumInteger -> False
isMinBound _        _                  = False

isMaxBound :: Platform -> Literal -> Bool
isMaxBound _        (LitChar c)        = c == maxBound
isMaxBound platform (LitNumber nt i _) = case nt of
   LitNumInt     -> i == platformMaxInt platform
   LitNumInt64   -> i == toInteger (maxBound :: Int64)
   LitNumWord    -> i == platformMaxWord platform
   LitNumWord64  -> i == toInteger (maxBound :: Word64)
   LitNumNatural -> False
   LitNumInteger -> False
isMaxBound _        _                  = False

-- | Create an Int literal expression while ensuring the given Integer is in the
-- target Int range
intResult :: Platform -> Integer -> Maybe CoreExpr
intResult platform result = Just (intResult' platform result)

intResult' :: Platform -> Integer -> CoreExpr
intResult' platform result = Lit (mkLitIntWrap platform result)

-- | Create an unboxed pair of an Int literal expression, ensuring the given
-- Integer is in the target Int range and the corresponding overflow flag
-- (@0#@/@1#@) if it wasn't.
intCResult :: Platform -> Integer -> Maybe CoreExpr
intCResult platform result = Just (mkPair [Lit lit, Lit c])
  where
    mkPair = mkCoreUbxTup [intPrimTy, intPrimTy]
    (lit, b) = mkLitIntWrapC platform result
    c = if b then onei platform else zeroi platform

-- | Create a Word literal expression while ensuring the given Integer is in the
-- target Word range
wordResult :: Platform -> Integer -> Maybe CoreExpr
wordResult platform result = Just (wordResult' platform result)

wordResult' :: Platform -> Integer -> CoreExpr
wordResult' platform result = Lit (mkLitWordWrap platform result)

-- | Create an unboxed pair of a Word literal expression, ensuring the given
-- Integer is in the target Word range and the corresponding carry flag
-- (@0#@/@1#@) if it wasn't.
wordCResult :: Platform -> Integer -> Maybe CoreExpr
wordCResult platform result = Just (mkPair [Lit lit, Lit c])
  where
    mkPair = mkCoreUbxTup [wordPrimTy, intPrimTy]
    (lit, b) = mkLitWordWrapC platform result
    c = if b then onei platform else zeroi platform

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

-- | narrow subsumes bitwise `and` with full mask (cf #16402):
--
--       narrowN (x .&. m)
--       m .&. (2^N-1) = 2^N-1
--       ==> narrowN x
--
-- e.g.  narrow16 (x .&. 0xFFFF)
--       ==> narrow16 x
--
narrowSubsumesAnd :: PrimOp -> PrimOp -> Int -> RuleM CoreExpr
narrowSubsumesAnd and_primop narrw n = do
  [Var primop_id `App` x `App` y] <- getArgs
  matchPrimOpId and_primop primop_id
  let mask = bit n -1
      g v (Lit (LitNumber _ m _)) = do
         guard (m .&. mask == mask)
         return (Var (mkPrimOpId narrw) `App` v)
      g _ _ = mzero
  g x y <|> g y x

idempotent :: RuleM CoreExpr
idempotent = do [e1, e2] <- getArgs
                guard $ cheapEqExpr e1 e2
                return e1

{-
Note [Guarding against silly shifts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this code:

  import Data.Bits( (.|.), shiftL )
  chunkToBitmap :: [Bool] -> Word32
  chunkToBitmap chunk = foldr (.|.) 0 [ 1 `shiftL` n | (True,n) <- zip chunk [0..] ]

This optimises to:
Shift.$wgo = \ (w_sCS :: GHC.Prim.Int#) (w1_sCT :: [GHC.Types.Bool]) ->
    case w1_sCT of _ {
      [] -> 0##;
      : x_aAW xs_aAX ->
        case x_aAW of _ {
          GHC.Types.False ->
            case w_sCS of wild2_Xh {
              __DEFAULT -> Shift.$wgo (GHC.Prim.+# wild2_Xh 1) xs_aAX;
              9223372036854775807 -> 0## };
          GHC.Types.True ->
            case GHC.Prim.>=# w_sCS 64 of _ {
              GHC.Types.False ->
                case w_sCS of wild3_Xh {
                  __DEFAULT ->
                    case Shift.$wgo (GHC.Prim.+# wild3_Xh 1) xs_aAX of ww_sCW { __DEFAULT ->
                      GHC.Prim.or# (GHC.Prim.narrow32Word#
                                      (GHC.Prim.uncheckedShiftL# 1## wild3_Xh))
                                   ww_sCW
                     };
                  9223372036854775807 ->
                    GHC.Prim.narrow32Word#
!!!!-->                  (GHC.Prim.uncheckedShiftL# 1## 9223372036854775807)
                };
              GHC.Types.True ->
                case w_sCS of wild3_Xh {
                  __DEFAULT -> Shift.$wgo (GHC.Prim.+# wild3_Xh 1) xs_aAX;
                  9223372036854775807 -> 0##
                } } } }

Note the massive shift on line "!!!!".  It can't happen, because we've checked
that w < 64, but the optimiser didn't spot that. We DO NOT want to constant-fold this!
Moreover, if the programmer writes (n `uncheckedShiftL` 9223372036854775807), we
can't constant fold it, but if it gets to the assembler we get
     Error: operand type mismatch for `shl'

So the best thing to do is to rewrite the shift with a call to error,
when the second arg is large. However, in general we cannot do this; consider
this case

    let x = I# (uncheckedIShiftL# n 80)
    in ...

Here x contains an invalid shift and consequently we would like to rewrite it
as follows:

    let x = I# (error "invalid shift)
    in ...

This was originally done in the fix to #16449 but this breaks the let/app
invariant (see Note [Core let/app invariant] in GHC.Core) as noted in #16742.
For the reasons discussed in Note [Checking versus non-checking primops] (in
the PrimOp module) there is no safe way rewrite the argument of I# such that
it bottoms.

Consequently we instead take advantage of the fact that large shifts are
undefined behavior (see associated documentation in primops.txt.pp) and
transform the invalid shift into an "obviously incorrect" value.

There are two cases:

- Shifting fixed-width things: the primops ISll, Sll, etc
  These are handled by shiftRule.

  We are happy to shift by any amount up to wordSize but no more.

- Shifting Integers: the function shiftLInteger, shiftRInteger
  from the 'integer' library.   These are handled by rule_shift_op,
  and match_Integer_shift_op.

  Here we could in principle shift by any amount, but we arbitrary
  limit the shift to 4 bits; in particular we do not want shift by a
  huge amount, which can happen in code like that above.

The two cases are more different in their code paths that is comfortable,
but that is only a historical accident.


************************************************************************
*                                                                      *
\subsection{Vaguely generic functions}
*                                                                      *
************************************************************************
-}

mkBasicRule :: Name -> Int -> RuleM CoreExpr -> CoreRule
-- Gives the Rule the same name as the primop itself
mkBasicRule op_name n_args rm
  = BuiltinRule { ru_name  = occNameFS (nameOccName op_name),
                  ru_fn    = op_name,
                  ru_nargs = n_args,
                  ru_try   = runRuleM rm }

newtype RuleM r = RuleM
  { runRuleM :: RuleOpts -> InScopeEnv -> Id -> [CoreExpr] -> Maybe r }
  deriving (Functor)

instance Applicative RuleM where
    pure x = RuleM $ \_ _ _ _ -> Just x
    (<*>) = ap

instance Monad RuleM where
  RuleM f >>= g
    = RuleM $ \env iu fn args ->
              case f env iu fn args of
                Nothing -> Nothing
                Just r  -> runRuleM (g r) env iu fn args

instance MonadFail RuleM where
    fail _ = mzero

instance Alternative RuleM where
  empty = RuleM $ \_ _ _ _ -> Nothing
  RuleM f1 <|> RuleM f2 = RuleM $ \env iu fn args ->
    f1 env iu fn args <|> f2 env iu fn args

instance MonadPlus RuleM

getPlatform :: RuleM Platform
getPlatform = roPlatform <$> getEnv

getEnv :: RuleM RuleOpts
getEnv = RuleM $ \env _ _ _ -> Just env

liftMaybe :: Maybe a -> RuleM a
liftMaybe Nothing = mzero
liftMaybe (Just x) = return x

liftLit :: (Literal -> Literal) -> RuleM CoreExpr
liftLit f = liftLitPlatform (const f)

liftLitPlatform :: (Platform -> Literal -> Literal) -> RuleM CoreExpr
liftLitPlatform f = do
  platform <- getPlatform
  [Lit lit] <- getArgs
  return $ Lit (f platform lit)

removeOp32 :: RuleM CoreExpr
removeOp32 = do
  platform <- getPlatform
  case platformWordSize platform of
    PW4 -> do
      [e] <- getArgs
      return e
    PW8 ->
      mzero

getArgs :: RuleM [CoreExpr]
getArgs = RuleM $ \_ _ _ args -> Just args

getInScopeEnv :: RuleM InScopeEnv
getInScopeEnv = RuleM $ \_ iu _ _ -> Just iu

getFunction :: RuleM Id
getFunction = RuleM $ \_ _ fn _ -> Just fn

-- return the n-th argument of this rule, if it is a literal
-- argument indices start from 0
getLiteral :: Int -> RuleM Literal
getLiteral n = RuleM $ \_ _ _ exprs -> case drop n exprs of
  (Lit l:_) -> Just l
  _ -> Nothing

unaryLit :: (RuleOpts -> Literal -> Maybe CoreExpr) -> RuleM CoreExpr
unaryLit op = do
  env <- getEnv
  [Lit l] <- getArgs
  liftMaybe $ op env (convFloating env l)

binaryLit :: (RuleOpts -> Literal -> Literal -> Maybe CoreExpr) -> RuleM CoreExpr
binaryLit op = do
  env <- getEnv
  [Lit l1, Lit l2] <- getArgs
  liftMaybe $ op env (convFloating env l1) (convFloating env l2)

binaryCmpLit :: (forall a . Ord a => a -> a -> Bool) -> RuleM CoreExpr
binaryCmpLit op = do
  platform <- getPlatform
  binaryLit (\_ -> cmpOp platform op)

leftIdentity :: Literal -> RuleM CoreExpr
leftIdentity id_lit = leftIdentityPlatform (const id_lit)

rightIdentity :: Literal -> RuleM CoreExpr
rightIdentity id_lit = rightIdentityPlatform (const id_lit)

identity :: Literal -> RuleM CoreExpr
identity lit = leftIdentity lit `mplus` rightIdentity lit

leftIdentityPlatform :: (Platform -> Literal) -> RuleM CoreExpr
leftIdentityPlatform id_lit = do
  platform <- getPlatform
  [Lit l1, e2] <- getArgs
  guard $ l1 == id_lit platform
  return e2

-- | Left identity rule for PrimOps like 'IntAddC' and 'WordAddC', where, in
-- addition to the result, we have to indicate that no carry/overflow occurred.
leftIdentityCPlatform :: (Platform -> Literal) -> RuleM CoreExpr
leftIdentityCPlatform id_lit = do
  platform <- getPlatform
  [Lit l1, e2] <- getArgs
  guard $ l1 == id_lit platform
  let no_c = Lit (zeroi platform)
  return (mkCoreUbxTup [exprType e2, intPrimTy] [e2, no_c])

rightIdentityPlatform :: (Platform -> Literal) -> RuleM CoreExpr
rightIdentityPlatform id_lit = do
  platform <- getPlatform
  [e1, Lit l2] <- getArgs
  guard $ l2 == id_lit platform
  return e1

-- | Right identity rule for PrimOps like 'IntSubC' and 'WordSubC', where, in
-- addition to the result, we have to indicate that no carry/overflow occurred.
rightIdentityCPlatform :: (Platform -> Literal) -> RuleM CoreExpr
rightIdentityCPlatform id_lit = do
  platform <- getPlatform
  [e1, Lit l2] <- getArgs
  guard $ l2 == id_lit platform
  let no_c = Lit (zeroi platform)
  return (mkCoreUbxTup [exprType e1, intPrimTy] [e1, no_c])

identityPlatform :: (Platform -> Literal) -> RuleM CoreExpr
identityPlatform lit =
  leftIdentityPlatform lit `mplus` rightIdentityPlatform lit

-- | Identity rule for PrimOps like 'IntAddC' and 'WordAddC', where, in addition
-- to the result, we have to indicate that no carry/overflow occurred.
identityCPlatform :: (Platform -> Literal) -> RuleM CoreExpr
identityCPlatform lit =
  leftIdentityCPlatform lit `mplus` rightIdentityCPlatform lit

leftZero :: (Platform -> Literal) -> RuleM CoreExpr
leftZero zero = do
  platform <- getPlatform
  [Lit l1, _] <- getArgs
  guard $ l1 == zero platform
  return $ Lit l1

rightZero :: (Platform -> Literal) -> RuleM CoreExpr
rightZero zero = do
  platform <- getPlatform
  [_, Lit l2] <- getArgs
  guard $ l2 == zero platform
  return $ Lit l2

zeroElem :: (Platform -> Literal) -> RuleM CoreExpr
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
convFloating :: RuleOpts -> Literal -> Literal
convFloating env (LitFloat  f) | not (roExcessRationalPrecision env) =
   LitFloat  (toRational (fromRational f :: Float ))
convFloating env (LitDouble d) | not (roExcessRationalPrecision env) =
   LitDouble (toRational (fromRational d :: Double))
convFloating _ l = l

guardFloatDiv :: RuleM ()
guardFloatDiv = do
  [Lit (LitFloat f1), Lit (LitFloat f2)] <- getArgs
  guard $ (f1 /=0 || f2 > 0) -- see Note [negative zero]
       && f2 /= 0            -- avoid NaN and Infinity/-Infinity

guardDoubleDiv :: RuleM ()
guardDoubleDiv = do
  [Lit (LitDouble d1), Lit (LitDouble d2)] <- getArgs
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

trueValInt, falseValInt :: Platform -> Expr CoreBndr
trueValInt  platform = Lit $ onei  platform -- see Note [What's true and false]
falseValInt platform = Lit $ zeroi platform

trueValBool, falseValBool :: Expr CoreBndr
trueValBool   = Var trueDataConId -- see Note [What's true and false]
falseValBool  = Var falseDataConId

ltVal, eqVal, gtVal :: Expr CoreBndr
ltVal = Var ordLTDataConId
eqVal = Var ordEQDataConId
gtVal = Var ordGTDataConId

mkIntVal :: Platform -> Integer -> Expr CoreBndr
mkIntVal platform i = Lit (mkLitInt platform i)
mkFloatVal :: RuleOpts -> Rational -> Expr CoreBndr
mkFloatVal env f = Lit (convFloating env (LitFloat  f))
mkDoubleVal :: RuleOpts -> Rational -> Expr CoreBndr
mkDoubleVal env d = Lit (convFloating env (LitDouble d))

matchPrimOpId :: PrimOp -> Id -> RuleM ()
matchPrimOpId op id = do
  op' <- liftMaybe $ isPrimOpId_maybe id
  guard $ op == op'

{-
************************************************************************
*                                                                      *
\subsection{Special rules for seq, tagToEnum, dataToTag}
*                                                                      *
************************************************************************

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
-}

tagToEnumRule :: RuleM CoreExpr
-- If     data T a = A | B | C
-- then   tagToEnum# (T ty) 2# -->  B ty
tagToEnumRule = do
  [Type ty, Lit (LitNumber LitNumInt i _)] <- getArgs
  case splitTyConApp_maybe ty of
    Just (tycon, tc_args) | isEnumerationTyCon tycon -> do
      let tag = fromInteger i
          correct_tag dc = (dataConTagZ dc) == tag
      (dc:rest) <- return $ filter correct_tag (tyConDataCons_maybe tycon `orElse` [])
      ASSERT(null rest) return ()
      return $ mkTyApps (Var (dataConWorkId dc)) tc_args

    -- See Note [tagToEnum#]
    _ -> WARN( True, text "tagToEnum# on non-enumeration type" <+> ppr ty )
         return $ mkRuntimeErrorApp rUNTIME_ERROR_ID ty "tagToEnum# on non-enumeration type"

------------------------------
dataToTagRule :: RuleM CoreExpr
-- See Note [dataToTag#] in primops.txt.pp
dataToTagRule = a `mplus` b
  where
    -- dataToTag (tagToEnum x)   ==>   x
    a = do
      [Type ty1, Var tag_to_enum `App` Type ty2 `App` tag] <- getArgs
      guard $ tag_to_enum `hasKey` tagToEnumKey
      guard $ ty1 `eqType` ty2
      return tag

    -- dataToTag (K e1 e2)  ==>   tag-of K
    -- This also works (via exprIsConApp_maybe) for
    --   dataToTag x
    -- where x's unfolding is a constructor application
    b = do
      dflags <- getPlatform
      [_, val_arg] <- getArgs
      in_scope <- getInScopeEnv
      (_,floats, dc,_,_) <- liftMaybe $ exprIsConApp_maybe in_scope val_arg
      ASSERT( not (isNewTyCon (dataConTyCon dc)) ) return ()
      return $ wrapFloats floats (mkIntVal dflags (toInteger (dataConTagZ dc)))

{- Note [dataToTag# magic]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The primop dataToTag# is unusual because it evaluates its argument.
Only `SeqOp` shares that property.  (Other primops do not do anything
as fancy as argument evaluation.)  The special handling for dataToTag#
is:

* GHC.Core.Utils.exprOkForSpeculation has a special case for DataToTagOp,
  (actually in app_ok).  Most primops with lifted arguments do not
  evaluate those arguments, but DataToTagOp and SeqOp are two
  exceptions.  We say that they are /never/ ok-for-speculation,
  regardless of the evaluated-ness of their argument.
  See GHC.Core.Utils Note [exprOkForSpeculation and SeqOp/DataToTagOp]

* There is a special case for DataToTagOp in GHC.StgToCmm.Expr.cgExpr,
  that evaluates its argument and then extracts the tag from
  the returned value.

* An application like (dataToTag# (Just x)) is optimised by
  dataToTagRule in GHC.Core.Opt.ConstantFold.

* A case expression like
     case (dataToTag# e) of <alts>
  gets transformed t
     case e of <transformed alts>
  by GHC.Core.Opt.ConstantFold.caseRules; see Note [caseRules for dataToTag]

See #15696 for a long saga.
-}

{- *********************************************************************
*                                                                      *
             unsafeEqualityProof
*                                                                      *
********************************************************************* -}

-- unsafeEqualityProof k t t  ==>  UnsafeRefl (Refl t)
-- That is, if the two types are equal, it's not unsafe!

unsafeEqualityProofRule :: RuleM CoreExpr
unsafeEqualityProofRule
  = do { [Type rep, Type t1, Type t2] <- getArgs
       ; guard (t1 `eqType` t2)
       ; fn <- getFunction
       ; let (_, ue) = splitForAllTys (idType fn)
             tc      = tyConAppTyCon ue  -- tycon:    UnsafeEquality
             (dc:_)  = tyConDataCons tc  -- data con: UnsafeRefl
             -- UnsafeRefl :: forall (r :: RuntimeRep) (a :: TYPE r).
             --               UnsafeEquality r a a
       ; return (mkTyApps (Var (dataConWrapId dc)) [rep, t1]) }


{- *********************************************************************
*                                                                      *
             Rules for seq# and spark#
*                                                                      *
********************************************************************* -}

{- Note [seq# magic]
~~~~~~~~~~~~~~~~~~~~
The primop
   seq# :: forall a s . a -> State# s -> (# State# s, a #)

is /not/ the same as the Prelude function seq :: a -> b -> b
as you can see from its type.  In fact, seq# is the implementation
mechanism for 'evaluate'

   evaluate :: a -> IO a
   evaluate a = IO $ \s -> seq# a s

The semantics of seq# is
  * evaluate its first argument
  * and return it

Things to note

* Why do we need a primop at all?  That is, instead of
      case seq# x s of (# x, s #) -> blah
  why not instead say this?
      case x of { DEFAULT -> blah)

  Reason (see #5129): if we saw
    catch# (\s -> case x of { DEFAULT -> raiseIO# exn s }) handler

  then we'd drop the 'case x' because the body of the case is bottom
  anyway. But we don't want to do that; the whole /point/ of
  seq#/evaluate is to evaluate 'x' first in the IO monad.

  In short, we /always/ evaluate the first argument and never
  just discard it.

* Why return the value?  So that we can control sharing of seq'd
  values: in
     let x = e in x `seq` ... x ...
  We don't want to inline x, so better to represent it as
       let x = e in case seq# x RW of (# _, x' #) -> ... x' ...
  also it matches the type of rseq in the Eval monad.

Implementing seq#.  The compiler has magic for SeqOp in

- GHC.Core.Opt.ConstantFold.seqRule: eliminate (seq# <whnf> s)

- GHC.StgToCmm.Expr.cgExpr, and cgCase: special case for seq#

- GHC.Core.Utils.exprOkForSpeculation;
  see Note [exprOkForSpeculation and SeqOp/DataToTagOp] in GHC.Core.Utils

- Simplify.addEvals records evaluated-ness for the result; see
  Note [Adding evaluatedness info to pattern-bound variables]
  in GHC.Core.Opt.Simplify
-}

seqRule :: RuleM CoreExpr
seqRule = do
  [Type ty_a, Type _ty_s, a, s] <- getArgs
  guard $ exprIsHNF a
  return $ mkCoreUbxTup [exprType s, ty_a] [s, a]

-- spark# :: forall a s . a -> State# s -> (# State# s, a #)
sparkRule :: RuleM CoreExpr
sparkRule = seqRule -- reduce on HNF, just the same
  -- XXX perhaps we shouldn't do this, because a spark eliminated by
  -- this rule won't be counted as a dud at runtime?

{-
************************************************************************
*                                                                      *
\subsection{Built in rules}
*                                                                      *
************************************************************************

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

and lo, eqString is not in scope.  This only really matters when we
get to code generation.  But the occurrence analyser does a GlomBinds
step when necessary, that does a new SCC analysis on the whole set of
bindings (see occurAnalysePgm), which sorts out the dependency, so all
is fine.
-}

builtinRules :: [CoreRule]
-- Rules for non-primops that can't be expressed using a RULE pragma
builtinRules
  = [BuiltinRule { ru_name = fsLit "AppendLitString",
                   ru_fn = unpackCStringFoldrName,
                   ru_nargs = 4, ru_try = match_append_lit_C },
     BuiltinRule { ru_name = fsLit "AppendLitStringUtf8",
                   ru_fn = unpackCStringFoldrUtf8Name,
                   ru_nargs = 4, ru_try = match_append_lit_utf8 },
     BuiltinRule { ru_name = fsLit "EqString", ru_fn = eqStringName,
                   ru_nargs = 2, ru_try = match_eq_string },
     BuiltinRule { ru_name = fsLit "CStringLength", ru_fn = cstringLengthName,
                   ru_nargs = 1, ru_try = match_cstring_length },
     BuiltinRule { ru_name = fsLit "Inline", ru_fn = inlineIdName,
                   ru_nargs = 2, ru_try = \_ _ _ -> match_inline },
     BuiltinRule { ru_name = fsLit "MagicDict", ru_fn = idName magicDictId,
                   ru_nargs = 4, ru_try = \_ _ _ -> match_magicDict },

     mkBasicRule unsafeEqualityProofName 3 unsafeEqualityProofRule,

     mkBasicRule divIntName 2 $ msum
        [ nonZeroLit 1 >> binaryLit (intOp2 div)
        , leftZero zeroi
        , do
          [arg, Lit (LitNumber LitNumInt d _)] <- getArgs
          Just n <- return $ exactLog2 d
          platform <- getPlatform
          return $ Var (mkPrimOpId ISraOp) `App` arg `App` mkIntVal platform n
        ],

     mkBasicRule modIntName 2 $ msum
        [ nonZeroLit 1 >> binaryLit (intOp2 mod)
        , leftZero zeroi
        , do
          [arg, Lit (LitNumber LitNumInt d _)] <- getArgs
          Just _ <- return $ exactLog2 d
          platform <- getPlatform
          return $ Var (mkPrimOpId AndIOp)
            `App` arg `App` mkIntVal platform (d - 1)
        ]
     ]
 ++ builtinIntegerRules
 ++ builtinNaturalRules
{-# NOINLINE builtinRules #-}
-- there is no benefit to inlining these yet, despite this, GHC produces
-- unfoldings for this regardless since the floated list entries look small.

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
  rule_shift_op       "shiftLInteger"       shiftLIntegerName       shiftL,
  rule_shift_op       "shiftRInteger"       shiftRIntegerName       shiftR,
  rule_bitInteger     "bitInteger"          bitIntegerName,
  -- See Note [Integer division constant folding] in libraries/base/GHC/Real.hs
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
          rule_bitInteger str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_bitInteger }
          rule_binop str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_binop op }
          rule_divop_both str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_divop_both op }
          rule_divop_one str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_divop_one op }
          rule_shift_op str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Integer_shift_op op }
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

builtinNaturalRules :: [CoreRule]
builtinNaturalRules =
 [rule_binop              "plusNatural"        plusNaturalName         (+)
 ,rule_partial_binop      "minusNatural"       minusNaturalName        (\a b -> if a >= b then Just (a - b) else Nothing)
 ,rule_binop              "timesNatural"       timesNaturalName        (*)
 ,rule_NaturalFromInteger "naturalFromInteger" naturalFromIntegerName
 ,rule_NaturalToInteger   "naturalToInteger"   naturalToIntegerName
 ,rule_WordToNatural      "wordToNatural"      wordToNaturalName
 ]
    where rule_binop str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Natural_binop op }
          rule_partial_binop str name op
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 2,
                           ru_try = match_Natural_partial_binop op }
          rule_NaturalToInteger str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_NaturalToInteger }
          rule_NaturalFromInteger str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_NaturalFromInteger }
          rule_WordToNatural str name
           = BuiltinRule { ru_name = fsLit str, ru_fn = name, ru_nargs = 1,
                           ru_try = match_WordToNatural }

---------------------------------------------------
-- The rule is this:
--      unpackFoldrCString*# "foo"# c (unpackFoldrCString*# "baz"# c n)
--      =  unpackFoldrCString*# "foobaz"# c n
--
-- See also Note [String literals in GHC] in CString.hs

-- CString version
match_append_lit_C :: RuleFun
match_append_lit_C = match_append_lit unpackCStringFoldrIdKey

-- CStringUTF8 version
match_append_lit_utf8 :: RuleFun
match_append_lit_utf8 = match_append_lit unpackCStringFoldrUtf8IdKey

{-# INLINE match_append_lit #-}
match_append_lit :: Unique -> RuleFun
match_append_lit foldVariant _ id_unf _
        [ Type ty1
        , lit1
        , c1
        , e2
        ]
  -- N.B. Ensure that we strip off any ticks (e.g. source notes) from the
  -- `lit` and `c` arguments, lest this may fail to fire when building with
  -- -g3. See #16740.
  | (strTicks, Var unpk `App` Type ty2
                        `App` lit2
                        `App` c2
                        `App` n) <- stripTicksTop tickishFloatable e2
  , unpk `hasKey` foldVariant
  , Just (LitString s1) <- exprIsLiteral_maybe id_unf lit1
  , Just (LitString s2) <- exprIsLiteral_maybe id_unf lit2
  , let freeVars = (mkInScopeSet (exprFreeVars c1 `unionVarSet` exprFreeVars c2))
    in eqExpr freeVars c1 c2
  , (c1Ticks, c1') <- stripTicksTop tickishFloatable c1
  , c2Ticks <- stripTicksTopT tickishFloatable c2
  = ASSERT( ty1 `eqType` ty2 )
    Just $ mkTicks strTicks
         $ Var unpk `App` Type ty1
                    `App` Lit (LitString (s1 `BS.append` s2))
                    `App` mkTicks (c1Ticks ++ c2Ticks) c1'
                    `App` n

match_append_lit _ _ _ _ _ = Nothing

---------------------------------------------------
-- The rule is this:
--      eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2)) = s1==s2
-- Also  matches unpackCStringUtf8#

match_eq_string :: RuleFun
match_eq_string _ id_unf _
        [Var unpk1 `App` lit1, Var unpk2 `App` lit2]
  | unpk_key1 <- getUnique unpk1
  , unpk_key2 <- getUnique unpk2
  , unpk_key1 == unpk_key2
  -- For now we insist the literals have to agree in their encoding
  -- to keep the rule simple. But we could check if the decoded strings
  -- compare equal in here as well.
  , unpk_key1 `elem` [unpackCStringUtf8IdKey, unpackCStringIdKey]
  , Just (LitString s1) <- exprIsLiteral_maybe id_unf lit1
  , Just (LitString s2) <- exprIsLiteral_maybe id_unf lit2
  = Just (if s1 == s2 then trueValBool else falseValBool)

match_eq_string _ _ _ _ = Nothing

-----------------------------------------------------------------------
-- Illustration of this rule:
--
-- cstringLength# "foobar"# --> 6
-- cstringLength# "fizz\NULzz"# --> 4
--
-- Nota bene: Addr# literals are suffixed by a NUL byte when they are
-- compiled to read-only data sections. That's why cstringLength# is
-- well defined on Addr# literals that do not explicitly have an embedded
-- NUL byte.
--
-- See GHC issue #5218, MR 2165, and bytestring PR 191. This is particularly
-- helpful when using OverloadedStrings to create a ByteString since the
-- function computing the length of such ByteStrings can often be constant
-- folded.
match_cstring_length :: RuleFun
match_cstring_length env id_unf _ [lit1]
  | Just (LitString str) <- exprIsLiteral_maybe id_unf lit1
    -- If elemIndex returns Just, it has the index of the first embedded NUL
    -- in the string. If no NUL bytes are present (the common case) then use
    -- full length of the byte string.
  = let len = fromMaybe (BS.length str) (BS.elemIndex 0 str)
     in Just (Lit (mkLitInt (roPlatform env) (fromIntegral len)))
match_cstring_length _ _ _ _ = Nothing

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


-- See Note [magicDictId magic] in "GHC.Types.Id.Make"
-- for a description of what is going on here.
match_magicDict :: [Expr CoreBndr] -> Maybe (Expr CoreBndr)
match_magicDict [Type _, Var wrap `App` Type a `App` Type _ `App` f, x, y ]
  | Just (fieldTy, _)   <- splitFunTy_maybe $ dropForAlls $ idType wrap
  , Just (dictTy, _)    <- splitFunTy_maybe fieldTy
  , Just dictTc         <- tyConAppTyCon_maybe dictTy
  , Just (_,_,co)       <- unwrapNewTyCon_maybe dictTc
  = Just
  $ f `App` Cast x (mkSymCo (mkUnbranchedAxInstCo Representational co [a] []))
      `App` y

match_magicDict _ = Nothing

-------------------------------------------------
-- Integer rules
--   smallInteger  (79::Int#)  = 79::Integer
--   wordToInteger (79::Word#) = 79::Integer
-- Similarly Int64, Word64

match_IntToInteger :: RuleFun
match_IntToInteger = match_IntToInteger_unop id

match_WordToInteger :: RuleFun
match_WordToInteger _ id_unf id [xl]
  | Just (LitNumber LitNumWord x _) <- exprIsLiteral_maybe id_unf xl
  = case splitFunTy_maybe (idType id) of
    Just (_, integerTy) ->
        Just (Lit (mkLitInteger x integerTy))
    _ ->
        panic "match_WordToInteger: Id has the wrong type"
match_WordToInteger _ _ _ _ = Nothing

match_Int64ToInteger :: RuleFun
match_Int64ToInteger _ id_unf id [xl]
  | Just (LitNumber LitNumInt64 x _) <- exprIsLiteral_maybe id_unf xl
  = case splitFunTy_maybe (idType id) of
    Just (_, integerTy) ->
        Just (Lit (mkLitInteger x integerTy))
    _ ->
        panic "match_Int64ToInteger: Id has the wrong type"
match_Int64ToInteger _ _ _ _ = Nothing

match_Word64ToInteger :: RuleFun
match_Word64ToInteger _ id_unf id [xl]
  | Just (LitNumber LitNumWord64 x _) <- exprIsLiteral_maybe id_unf xl
  = case splitFunTy_maybe (idType id) of
    Just (_, integerTy) ->
        Just (Lit (mkLitInteger x integerTy))
    _ ->
        panic "match_Word64ToInteger: Id has the wrong type"
match_Word64ToInteger _ _ _ _ = Nothing

match_NaturalToInteger :: RuleFun
match_NaturalToInteger _ id_unf id [xl]
  | Just (LitNumber LitNumNatural x _) <- exprIsLiteral_maybe id_unf xl
  = case splitFunTy_maybe (idType id) of
    Just (_, naturalTy) ->
        Just (Lit (LitNumber LitNumInteger x naturalTy))
    _ ->
        panic "match_NaturalToInteger: Id has the wrong type"
match_NaturalToInteger _ _ _ _ = Nothing

match_NaturalFromInteger :: RuleFun
match_NaturalFromInteger _ id_unf id [xl]
  | Just (LitNumber LitNumInteger x _) <- exprIsLiteral_maybe id_unf xl
  , x >= 0
  = case splitFunTy_maybe (idType id) of
    Just (_, naturalTy) ->
        Just (Lit (LitNumber LitNumNatural x naturalTy))
    _ ->
        panic "match_NaturalFromInteger: Id has the wrong type"
match_NaturalFromInteger _ _ _ _ = Nothing

match_WordToNatural :: RuleFun
match_WordToNatural _ id_unf id [xl]
  | Just (LitNumber LitNumWord x _) <- exprIsLiteral_maybe id_unf xl
  = case splitFunTy_maybe (idType id) of
    Just (_, naturalTy) ->
        Just (Lit (LitNumber LitNumNatural x naturalTy))
    _ ->
        panic "match_WordToNatural: Id has the wrong type"
match_WordToNatural _ _ _ _ = Nothing

-------------------------------------------------
{- Note [Rewriting bitInteger]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For most types the bitInteger operation can be implemented in terms of shifts.
The integer-gmp package, however, can do substantially better than this if
allowed to provide its own implementation. However, in so doing it previously lost
constant-folding (see #8832). The bitInteger rule above provides constant folding
specifically for this function.

There is, however, a bit of trickiness here when it comes to ranges. While the
AST encodes all integers as Integers, `bit` expects the bit
index to be given as an Int. Hence we coerce to an Int in the rule definition.
This will behave a bit funny for constants larger than the word size, but the user
should expect some funniness given that they will have at very least ignored a
warning in this case.
-}

match_bitInteger :: RuleFun
-- Just for GHC.Integer.Type.bitInteger :: Int# -> Integer
match_bitInteger env id_unf fn [arg]
  | Just (LitNumber LitNumInt x _) <- exprIsLiteral_maybe id_unf arg
  , x >= 0
  , x <= (toInteger (platformWordSizeInBits (roPlatform env)) - 1)
    -- Make sure x is small enough to yield a decently small integer
    -- Attempting to construct the Integer for
    --    (bitInteger 9223372036854775807#)
    -- would be a bad idea (#14959)
  , let x_int = fromIntegral x :: Int
  = case splitFunTy_maybe (idType fn) of
    Just (_, integerTy)
      -> Just (Lit (LitNumber LitNumInteger (bit x_int) integerTy))
    _ -> panic "match_IntToInteger_unop: Id has the wrong type"

match_bitInteger _ _ _ _ = Nothing


-------------------------------------------------
match_Integer_convert :: Num a
                      => (Platform -> a -> Expr CoreBndr)
                      -> RuleFun
match_Integer_convert convert env id_unf _ [xl]
  | Just (LitNumber LitNumInteger x _) <- exprIsLiteral_maybe id_unf xl
  = Just (convert (roPlatform env) (fromInteger x))
match_Integer_convert _ _ _ _ _ = Nothing

match_Integer_unop :: (Integer -> Integer) -> RuleFun
match_Integer_unop unop _ id_unf _ [xl]
  | Just (LitNumber LitNumInteger x i) <- exprIsLiteral_maybe id_unf xl
  = Just (Lit (LitNumber LitNumInteger (unop x) i))
match_Integer_unop _ _ _ _ _ = Nothing

match_IntToInteger_unop :: (Integer -> Integer) -> RuleFun
match_IntToInteger_unop unop _ id_unf fn [xl]
  | Just (LitNumber LitNumInt x _) <- exprIsLiteral_maybe id_unf xl
  = case splitFunTy_maybe (idType fn) of
    Just (_, integerTy) ->
        Just (Lit (LitNumber LitNumInteger (unop x) integerTy))
    _ ->
        panic "match_IntToInteger_unop: Id has the wrong type"
match_IntToInteger_unop _ _ _ _ _ = Nothing

match_Integer_binop :: (Integer -> Integer -> Integer) -> RuleFun
match_Integer_binop binop _ id_unf _ [xl,yl]
  | Just (LitNumber LitNumInteger x i) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumInteger y _) <- exprIsLiteral_maybe id_unf yl
  = Just (Lit (mkLitInteger (x `binop` y) i))
match_Integer_binop _ _ _ _ _ = Nothing

match_Natural_binop :: (Integer -> Integer -> Integer) -> RuleFun
match_Natural_binop binop _ id_unf _ [xl,yl]
  | Just (LitNumber LitNumNatural x i) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumNatural y _) <- exprIsLiteral_maybe id_unf yl
  = Just (Lit (mkLitNatural (x `binop` y) i))
match_Natural_binop _ _ _ _ _ = Nothing

match_Natural_partial_binop :: (Integer -> Integer -> Maybe Integer) -> RuleFun
match_Natural_partial_binop binop _ id_unf _ [xl,yl]
  | Just (LitNumber LitNumNatural x i) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumNatural y _) <- exprIsLiteral_maybe id_unf yl
  , Just z <- x `binop` y
  = Just (Lit (mkLitNatural z i))
match_Natural_partial_binop _ _ _ _ _ = Nothing

-- This helper is used for the quotRem and divMod functions
match_Integer_divop_both
   :: (Integer -> Integer -> (Integer, Integer)) -> RuleFun
match_Integer_divop_both divop _ id_unf _ [xl,yl]
  | Just (LitNumber LitNumInteger x t) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumInteger y _) <- exprIsLiteral_maybe id_unf yl
  , y /= 0
  , (r,s) <- x `divop` y
  = Just $ mkCoreUbxTup [t,t] [Lit (mkLitInteger r t), Lit (mkLitInteger s t)]
match_Integer_divop_both _ _ _ _ _ = Nothing

-- This helper is used for the quot and rem functions
match_Integer_divop_one :: (Integer -> Integer -> Integer) -> RuleFun
match_Integer_divop_one divop _ id_unf _ [xl,yl]
  | Just (LitNumber LitNumInteger x i) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumInteger y _) <- exprIsLiteral_maybe id_unf yl
  , y /= 0
  = Just (Lit (mkLitInteger (x `divop` y) i))
match_Integer_divop_one _ _ _ _ _ = Nothing

match_Integer_shift_op :: (Integer -> Int -> Integer) -> RuleFun
-- Used for shiftLInteger, shiftRInteger :: Integer -> Int# -> Integer
-- See Note [Guarding against silly shifts]
match_Integer_shift_op binop _ id_unf _ [xl,yl]
  | Just (LitNumber LitNumInteger x i) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumInt y _)     <- exprIsLiteral_maybe id_unf yl
  , y >= 0
  , y <= 4   -- Restrict constant-folding of shifts on Integers, somewhat
             -- arbitrary.  We can get huge shifts in inaccessible code
             -- (#15673)
  = Just (Lit (mkLitInteger (x `binop` fromIntegral y) i))
match_Integer_shift_op _ _ _ _ _ = Nothing

match_Integer_binop_Prim :: (Integer -> Integer -> Bool) -> RuleFun
match_Integer_binop_Prim binop env id_unf _ [xl, yl]
  | Just (LitNumber LitNumInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumInteger y _) <- exprIsLiteral_maybe id_unf yl
  = Just (if x `binop` y then trueValInt (roPlatform env) else falseValInt (roPlatform env))
match_Integer_binop_Prim _ _ _ _ _ = Nothing

match_Integer_binop_Ordering :: (Integer -> Integer -> Ordering) -> RuleFun
match_Integer_binop_Ordering binop _ id_unf _ [xl, yl]
  | Just (LitNumber LitNumInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumInteger y _) <- exprIsLiteral_maybe id_unf yl
  = Just $ case x `binop` y of
             LT -> ltVal
             EQ -> eqVal
             GT -> gtVal
match_Integer_binop_Ordering _ _ _ _ _ = Nothing

match_Integer_Int_encodeFloat :: RealFloat a
                              => (a -> Expr CoreBndr)
                              -> RuleFun
match_Integer_Int_encodeFloat mkLit _ id_unf _ [xl,yl]
  | Just (LitNumber LitNumInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumInt y _)     <- exprIsLiteral_maybe id_unf yl
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
  | Just (LitNumber LitNumInteger x _) <- exprIsLiteral_maybe id_unf xl
  , Just (LitNumber LitNumInteger y _) <- exprIsLiteral_maybe id_unf yl
  , y /= 0
  = Just (mkLit (fromRational (x % y)))
match_rationalTo _ _ _ _ _ = Nothing

match_decodeDouble :: RuleFun
match_decodeDouble env id_unf fn [xl]
  | Just (LitDouble x) <- exprIsLiteral_maybe id_unf xl
  = case splitFunTy_maybe (idType fn) of
    Just (_, res)
      | Just [_lev1, _lev2, integerTy, intHashTy] <- tyConAppArgs_maybe res
      -> case decodeFloat (fromRational x :: Double) of
           (y, z) ->
             Just $ mkCoreUbxTup [integerTy, intHashTy]
                                 [Lit (mkLitInteger y integerTy),
                                  Lit (mkLitInt (roPlatform env) (toInteger z))]
    _ ->
        pprPanic "match_decodeDouble: Id has the wrong type"
          (ppr fn <+> dcolon <+> ppr (idType fn))
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



--------------------------------------------------------
-- Note [Constant folding through nested expressions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We use rewrites rules to perform constant folding. It means that we don't
-- have a global view of the expression we are trying to optimise. As a
-- consequence we only perform local (small-step) transformations that either:
--    1) reduce the number of operations
--    2) rearrange the expression to increase the odds that other rules will
--    match
--
-- We don't try to handle more complex expression optimisation cases that would
-- require a global view. For example, rewriting expressions to increase
-- sharing (e.g., Horner's method); optimisations that require local
-- transformations increasing the number of operations; rearrangements to
-- cancel/factorize terms (e.g., (a+b-a-b) isn't rearranged to reduce to 0).
--
-- We already have rules to perform constant folding on expressions with the
-- following shape (where a and/or b are literals):
--
--          D)    op
--                /\
--               /  \
--              /    \
--             a      b
--
-- To support nested expressions, we match three other shapes of expression
-- trees:
--
-- A)   op1          B)       op1       C)       op1
--      /\                    /\                 /\
--     /  \                  /  \               /  \
--    /    \                /    \             /    \
--   a     op2            op2     c          op2    op3
--          /\            /\                 /\      /\
--         /  \          /  \               /  \    /  \
--        b    c        a    b             a    b  c    d
--
--
-- R1) +/- simplification:
--    ops = + or -, two literals (not siblings)
--
--    Examples:
--       A: 5 + (10-x)  ==> 15-x
--       B: (10+x) + 5  ==> 15+x
--       C: (5+a)-(5-b) ==> 0+(a+b)
--
-- R2) * simplification
--    ops = *, two literals (not siblings)
--
--    Examples:
--       A: 5 * (10*x)  ==> 50*x
--       B: (10*x) * 5  ==> 50*x
--       C: (5*a)*(5*b) ==> 25*(a*b)
--
-- R3) * distribution over +/-
--    op1 = *, op2 = + or -, two literals (not siblings)
--
--    This transformation doesn't reduce the number of operations but switches
--    the outer and the inner operations so that the outer is (+) or (-) instead
--    of (*). It increases the odds that other rules will match after this one.
--
--    Examples:
--       A: 5 * (10-x)  ==> 50 - (5*x)
--       B: (10+x) * 5  ==> 50 + (5*x)
--       C: Not supported as it would increase the number of operations:
--          (5+a)*(5-b) ==> 25 - 5*b + 5*a - a*b
--
-- R4) Simple factorization
--
--    op1 = + or -, op2/op3 = *,
--    one literal for each innermost * operation (except in the D case),
--    the two other terms are equals
--
--    Examples:
--       A: x - (10*x)  ==> (-9)*x
--       B: (10*x) + x  ==> 11*x
--       C: (5*x)-(x*3) ==> 2*x
--       D: x+x         ==> 2*x
--
-- R5) +/- propagation
--
--    ops = + or -, one literal
--
--    This transformation doesn't reduce the number of operations but propagates
--    the constant to the outer level. It increases the odds that other rules
--    will match after this one.
--
--    Examples:
--       A: x - (10-y)  ==> (x+y) - 10
--       B: (10+x) - y  ==> 10 + (x-y)
--       C: N/A (caught by the A and B cases)
--
--------------------------------------------------------

-- | Rules to perform constant folding into nested expressions
--
--See Note [Constant folding through nested expressions]
numFoldingRules :: PrimOp -> (Platform -> PrimOps) -> RuleM CoreExpr
numFoldingRules op dict = do
  env <- getEnv
  if not (roNumConstantFolding env)
   then mzero
   else do
    [e1,e2] <- getArgs
    platform <- getPlatform
    let PrimOps{..} = dict platform
    case BinOpApp e1 op e2 of
     -- R1) +/- simplification
     x    :++: (y :++: v)          -> return $ mkL (x+y)   `add` v
     x    :++: (L y :-: v)         -> return $ mkL (x+y)   `sub` v
     x    :++: (v   :-: L y)       -> return $ mkL (x-y)   `add` v
     L x  :-:  (y :++: v)          -> return $ mkL (x-y)   `sub` v
     L x  :-:  (L y :-: v)         -> return $ mkL (x-y)   `add` v
     L x  :-:  (v   :-: L y)       -> return $ mkL (x+y)   `sub` v

     (y :++: v)    :-: L x         -> return $ mkL (y-x)   `add` v
     (L y :-: v)   :-: L x         -> return $ mkL (y-x)   `sub` v
     (v   :-: L y) :-: L x         -> return $ mkL (0-y-x) `add` v

     (x :++: w)  :+: (y :++: v)    -> return $ mkL (x+y)   `add` (w `add` v)
     (w :-: L x) :+: (L y :-: v)   -> return $ mkL (y-x)   `add` (w `sub` v)
     (w :-: L x) :+: (v   :-: L y) -> return $ mkL (0-x-y) `add` (w `add` v)
     (L x :-: w) :+: (L y :-: v)   -> return $ mkL (x+y)   `sub` (w `add` v)
     (L x :-: w) :+: (v   :-: L y) -> return $ mkL (x-y)   `add` (v `sub` w)
     (w :-: L x) :+: (y :++: v)    -> return $ mkL (y-x)   `add` (w `add` v)
     (L x :-: w) :+: (y :++: v)    -> return $ mkL (x+y)   `add` (v `sub` w)
     (y :++: v)  :+: (w :-: L x)   -> return $ mkL (y-x)   `add` (w `add` v)
     (y :++: v)  :+: (L x :-: w)   -> return $ mkL (x+y)   `add` (v `sub` w)

     (v   :-: L y) :-: (w :-: L x) -> return $ mkL (x-y)   `add` (v `sub` w)
     (v   :-: L y) :-: (L x :-: w) -> return $ mkL (0-x-y) `add` (v `add` w)
     (L y :-:   v) :-: (w :-: L x) -> return $ mkL (x+y)   `sub` (v `add` w)
     (L y :-:   v) :-: (L x :-: w) -> return $ mkL (y-x)   `add` (w `sub` v)
     (x :++: w)    :-: (y :++: v)  -> return $ mkL (x-y)   `add` (w `sub` v)
     (w :-: L x)   :-: (y :++: v)  -> return $ mkL (0-y-x) `add` (w `sub` v)
     (L x :-: w)   :-: (y :++: v)  -> return $ mkL (x-y)   `sub` (v `add` w)
     (y :++: v)    :-: (w :-: L x) -> return $ mkL (y+x)   `add` (v `sub` w)
     (y :++: v)    :-: (L x :-: w) -> return $ mkL (y-x)   `add` (v `add` w)

     -- R2) * simplification
     x :**: (y :**: v)             -> return $ mkL (x*y)   `mul` v
     (x :**: w) :*: (y :**: v)     -> return $ mkL (x*y)   `mul` (w `mul` v)

     -- R3) * distribution over +/-
     x :**: (y :++: v)             -> return $ mkL (x*y)   `add` (mkL x `mul` v)
     x :**: (L y :-: v)            -> return $ mkL (x*y)   `sub` (mkL x `mul` v)
     x :**: (v   :-: L y)          -> return $ (mkL x `mul` v) `sub` mkL (x*y)

     -- R4) Simple factorization
     v :+: w
      | w `cheapEqExpr` v          -> return $ mkL 2       `mul` v
     w :+: (y :**: v)
      | w `cheapEqExpr` v          -> return $ mkL (1+y)   `mul` v
     w :-: (y :**: v)
      | w `cheapEqExpr` v          -> return $ mkL (1-y)   `mul` v
     (y :**: v) :+: w
      | w `cheapEqExpr` v          -> return $ mkL (y+1)   `mul` v
     (y :**: v) :-: w
      | w `cheapEqExpr` v          -> return $ mkL (y-1)   `mul` v
     (x :**: w) :+: (y :**: v)
      | w `cheapEqExpr` v          -> return $ mkL (x+y)   `mul` v
     (x :**: w) :-: (y :**: v)
      | w `cheapEqExpr` v          -> return $ mkL (x-y)   `mul` v

     -- R5) +/- propagation
     w  :+: (y :++: v)             -> return $ mkL y `add` (w `add` v)
     (y :++: v) :+: w              -> return $ mkL y       `add` (w `add` v)
     w  :-: (y :++: v)             -> return $ (w `sub` v) `sub` mkL y
     (y :++: v) :-: w              -> return $ mkL y       `add` (v `sub` w)
     w    :-: (L y :-: v)          -> return $ (w `add` v) `sub` mkL y
     (L y :-: v) :-: w             -> return $ mkL y       `sub` (w `add` v)
     w    :+: (L y :-: v)          -> return $ mkL y       `add` (w `sub` v)
     w    :+: (v :-: L y)          -> return $ (w `add` v) `sub` mkL y
     (L y :-: v) :+: w             -> return $ mkL y       `add` (w `sub` v)
     (v :-: L y) :+: w             -> return $ (w `add` v) `sub` mkL y

     _                             -> mzero



-- | Match the application of a binary primop
pattern BinOpApp  :: Arg CoreBndr -> PrimOp -> Arg CoreBndr -> CoreExpr
pattern BinOpApp  x op y =  OpVal op `App` x `App` y

-- | Match a primop
pattern OpVal   :: PrimOp  -> Arg CoreBndr
pattern OpVal   op     <- Var (isPrimOpId_maybe -> Just op) where
   OpVal op = Var (mkPrimOpId op)



-- | Match a literal
pattern L :: Integer -> Arg CoreBndr
pattern L l <- Lit (isLitValue_maybe -> Just l)

-- | Match an addition
pattern (:+:) :: Arg CoreBndr -> Arg CoreBndr -> CoreExpr
pattern x :+: y <- BinOpApp x (isAddOp -> True) y

-- | Match an addition with a literal (handle commutativity)
pattern (:++:) :: Integer -> Arg CoreBndr -> CoreExpr
pattern l :++: x <- (isAdd -> Just (l,x))

isAdd :: CoreExpr -> Maybe (Integer,CoreExpr)
isAdd e = case e of
   L l :+: x   -> Just (l,x)
   x   :+: L l -> Just (l,x)
   _           -> Nothing

-- | Match a multiplication
pattern (:*:) :: Arg CoreBndr -> Arg CoreBndr -> CoreExpr
pattern x :*: y <- BinOpApp x (isMulOp -> True) y

-- | Match a multiplication with a literal (handle commutativity)
pattern (:**:) :: Integer -> Arg CoreBndr -> CoreExpr
pattern l :**: x <- (isMul -> Just (l,x))

isMul :: CoreExpr -> Maybe (Integer,CoreExpr)
isMul e = case e of
   L l :*: x   -> Just (l,x)
   x   :*: L l -> Just (l,x)
   _           -> Nothing


-- | Match a subtraction
pattern (:-:) :: Arg CoreBndr -> Arg CoreBndr -> CoreExpr
pattern x :-: y <- BinOpApp x (isSubOp -> True) y

isSubOp :: PrimOp -> Bool
isSubOp IntSubOp  = True
isSubOp WordSubOp = True
isSubOp _         = False

isAddOp :: PrimOp -> Bool
isAddOp IntAddOp  = True
isAddOp WordAddOp = True
isAddOp _         = False

isMulOp :: PrimOp -> Bool
isMulOp IntMulOp  = True
isMulOp WordMulOp = True
isMulOp _         = False

-- | Explicit "type-class"-like dictionary for numeric primops
--
-- Depends on Platform because creating a literal value depends on Platform
data PrimOps = PrimOps
   { add :: CoreExpr -> CoreExpr -> CoreExpr -- ^ Add two numbers
   , sub :: CoreExpr -> CoreExpr -> CoreExpr -- ^ Sub two numbers
   , mul :: CoreExpr -> CoreExpr -> CoreExpr -- ^ Multiply two numbers
   , mkL :: Integer -> CoreExpr              -- ^ Create a literal value
   }

intPrimOps :: Platform -> PrimOps
intPrimOps platform = PrimOps
   { add = \x y -> BinOpApp x IntAddOp y
   , sub = \x y -> BinOpApp x IntSubOp y
   , mul = \x y -> BinOpApp x IntMulOp y
   , mkL = intResult' platform
   }

wordPrimOps :: Platform -> PrimOps
wordPrimOps platform = PrimOps
   { add = \x y -> BinOpApp x WordAddOp y
   , sub = \x y -> BinOpApp x WordSubOp y
   , mul = \x y -> BinOpApp x WordMulOp y
   , mkL = wordResult' platform
   }


--------------------------------------------------------
-- Constant folding through case-expressions
--
-- cf Scrutinee Constant Folding in simplCore/GHC.Core.Opt.Simplify.Utils
--------------------------------------------------------

-- | Match the scrutinee of a case and potentially return a new scrutinee and a
-- function to apply to each literal alternative.
caseRules :: Platform
          -> CoreExpr                       -- Scrutinee
          -> Maybe ( CoreExpr               -- New scrutinee
                   , AltCon -> Maybe AltCon -- How to fix up the alt pattern
                                            --   Nothing <=> Unreachable
                                            -- See Note [Unreachable caseRules alternatives]
                   , Id -> CoreExpr)        -- How to reconstruct the original scrutinee
                                            -- from the new case-binder
-- e.g  case e of b {
--         ...;
--         con bs -> rhs;
--         ... }
--  ==>
--      case e' of b' {
--         ...;
--         fixup_altcon[con] bs -> let b = mk_orig[b] in rhs;
--         ... }

caseRules platform (App (App (Var f) v) (Lit l))   -- v `op` x#
  | Just op <- isPrimOpId_maybe f
  , Just x  <- isLitValue_maybe l
  , Just adjust_lit <- adjustDyadicRight op x
  = Just (v, tx_lit_con platform adjust_lit
           , \v -> (App (App (Var f) (Var v)) (Lit l)))

caseRules platform (App (App (Var f) (Lit l)) v)   -- x# `op` v
  | Just op <- isPrimOpId_maybe f
  , Just x  <- isLitValue_maybe l
  , Just adjust_lit <- adjustDyadicLeft x op
  = Just (v, tx_lit_con platform adjust_lit
           , \v -> (App (App (Var f) (Lit l)) (Var v)))


caseRules platform (App (Var f) v              )   -- op v
  | Just op <- isPrimOpId_maybe f
  , Just adjust_lit <- adjustUnary op
  = Just (v, tx_lit_con platform adjust_lit
           , \v -> App (Var f) (Var v))

-- See Note [caseRules for tagToEnum]
caseRules platform (App (App (Var f) type_arg) v)
  | Just TagToEnumOp <- isPrimOpId_maybe f
  = Just (v, tx_con_tte platform
           , \v -> (App (App (Var f) type_arg) (Var v)))

-- See Note [caseRules for dataToTag]
caseRules _ (App (App (Var f) (Type ty)) v)       -- dataToTag x
  | Just DataToTagOp <- isPrimOpId_maybe f
  , Just (tc, _) <- tcSplitTyConApp_maybe ty
  , isAlgTyCon tc
  = Just (v, tx_con_dtt ty
           , \v -> App (App (Var f) (Type ty)) (Var v))

caseRules _ _ = Nothing


tx_lit_con :: Platform -> (Integer -> Integer) -> AltCon -> Maybe AltCon
tx_lit_con _        _      DEFAULT    = Just DEFAULT
tx_lit_con platform adjust (LitAlt l) = Just $ LitAlt (mapLitValue platform adjust l)
tx_lit_con _        _      alt        = pprPanic "caseRules" (ppr alt)
   -- NB: mapLitValue uses mkLitIntWrap etc, to ensure that the
   -- literal alternatives remain in Word/Int target ranges
   -- (See Note [Word/Int underflow/overflow] in GHC.Types.Literal and #13172).

adjustDyadicRight :: PrimOp -> Integer -> Maybe (Integer -> Integer)
-- Given (x `op` lit) return a function 'f' s.t.  f (x `op` lit) = x
adjustDyadicRight op lit
  = case op of
         WordAddOp -> Just (\y -> y-lit      )
         IntAddOp  -> Just (\y -> y-lit      )
         WordSubOp -> Just (\y -> y+lit      )
         IntSubOp  -> Just (\y -> y+lit      )
         XorOp     -> Just (\y -> y `xor` lit)
         XorIOp    -> Just (\y -> y `xor` lit)
         _         -> Nothing

adjustDyadicLeft :: Integer -> PrimOp -> Maybe (Integer -> Integer)
-- Given (lit `op` x) return a function 'f' s.t.  f (lit `op` x) = x
adjustDyadicLeft lit op
  = case op of
         WordAddOp -> Just (\y -> y-lit      )
         IntAddOp  -> Just (\y -> y-lit      )
         WordSubOp -> Just (\y -> lit-y      )
         IntSubOp  -> Just (\y -> lit-y      )
         XorOp     -> Just (\y -> y `xor` lit)
         XorIOp    -> Just (\y -> y `xor` lit)
         _         -> Nothing


adjustUnary :: PrimOp -> Maybe (Integer -> Integer)
-- Given (op x) return a function 'f' s.t.  f (op x) = x
adjustUnary op
  = case op of
         NotOp     -> Just (\y -> complement y)
         NotIOp    -> Just (\y -> complement y)
         IntNegOp  -> Just (\y -> negate y    )
         _         -> Nothing

tx_con_tte :: Platform -> AltCon -> Maybe AltCon
tx_con_tte _        DEFAULT         = Just DEFAULT
tx_con_tte _        alt@(LitAlt {}) = pprPanic "caseRules" (ppr alt)
tx_con_tte platform (DataAlt dc)  -- See Note [caseRules for tagToEnum]
  = Just $ LitAlt $ mkLitInt platform $ toInteger $ dataConTagZ dc

tx_con_dtt :: Type -> AltCon -> Maybe AltCon
tx_con_dtt _  DEFAULT = Just DEFAULT
tx_con_dtt ty (LitAlt (LitNumber LitNumInt i _))
   | tag >= 0
   , tag < n_data_cons
   = Just (DataAlt (data_cons !! tag))   -- tag is zero-indexed, as is (!!)
   | otherwise
   = Nothing
   where
     tag         = fromInteger i :: ConTagZ
     tc          = tyConAppTyCon ty
     n_data_cons = tyConFamilySize tc
     data_cons   = tyConDataCons tc

tx_con_dtt _ alt = pprPanic "caseRules" (ppr alt)


{- Note [caseRules for tagToEnum]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to transform
   case tagToEnum x of
     False -> e1
     True  -> e2
into
   case x of
     0# -> e1
     1# -> e2

This rule eliminates a lot of boilerplate. For
  if (x>y) then e2 else e1
we generate
  case tagToEnum (x ># y) of
    False -> e1
    True  -> e2
and it is nice to then get rid of the tagToEnum.

Beware (#14768): avoid the temptation to map constructor 0 to
DEFAULT, in the hope of getting this
  case (x ># y) of
    DEFAULT -> e1
    1#      -> e2
That fails utterly in the case of
   data Colour = Red | Green | Blue
   case tagToEnum x of
      DEFAULT -> e1
      Red     -> e2

We don't want to get this!
   case x of
      DEFAULT -> e1
      DEFAULT -> e2

Instead, we deal with turning one branch into DEFAULT in GHC.Core.Opt.Simplify.Utils
(add_default in mkCase3).

Note [caseRules for dataToTag]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [dataToTag#] in primpops.txt.pp

We want to transform
  case dataToTag x of
    DEFAULT -> e1
    1# -> e2
into
  case x of
    DEFAULT -> e1
    (:) _ _ -> e2

Note the need for some wildcard binders in
the 'cons' case.

For the time, we only apply this transformation when the type of `x` is a type
headed by a normal tycon. In particular, we do not apply this in the case of a
data family tycon, since that would require carefully applying coercion(s)
between the data family and the data family instance's representation type,
which caseRules isn't currently engineered to handle (#14680).

Note [Unreachable caseRules alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Take care if we see something like
  case dataToTag x of
    DEFAULT -> e1
    -1# -> e2
    100 -> e3
because there isn't a data constructor with tag -1 or 100. In this case the
out-of-range alternative is dead code -- we know the range of tags for x.

Hence caseRules returns (AltCon -> Maybe AltCon), with Nothing indicating
an alternative that is unreachable.

You may wonder how this can happen: check out #15436.
-}
