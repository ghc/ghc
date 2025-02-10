{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.JS.Opt.Expr
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
--                Josh Meredith  <josh.meredith@iohk.io>
-- Stability   :  experimental
--
--
--  This module contains a simple expression optimizer that performs constant
--  folding and some boolean expression optimizations.
-----------------------------------------------------------------------------

module GHC.JS.Opt.Expr (optExprs) where

import GHC.Prelude hiding (shiftL, shiftR)

import GHC.JS.Syntax

import Data.Bifunctor (second)
import Data.Bits (shiftL, shiftR, (.^.))
import Data.Int (Int32)

{-
  Optimize expressions in a statement.

  This is best done after running the simple optimizer in GHC.JS.Opt.Simple,
  which eliminates redundant assignments and produces expressions that can be
  optimized more effectively.
 -}
optExprs :: JStat -> JStat
optExprs s = go s
  where
    go (DeclStat v mb_e) = DeclStat v (fmap opt mb_e)
    go (AssignStat lhs op rhs) = AssignStat (opt lhs) op (opt rhs)
    go (ReturnStat e) = ReturnStat (opt e)
    go (BlockStat ss) = BlockStat (map go ss)
    go (IfStat e s1 s2) = IfStat (optCond e) (go s1) (go s2)
    go (WhileStat b e s) = WhileStat b (optCond e) (go s)
    go (ForStat s1 e s2 s3) = ForStat (go s1) (optCond e) (go s2) (go s3)
    go (ForInStat b v e s) = ForInStat b v (opt e) (go s)
    go (SwitchStat e cases s) = SwitchStat (opt e)
                                           (map (second go) cases)
                                           (go s)
    go (TryStat s1 v s2 s3) = TryStat (go s1) v (go s2) (go s3)
    go (ApplStat e es) = ApplStat (opt e) (map opt es)
    go (UOpStat op e) = UOpStat op (opt e)
    go (LabelStat lbl s) = LabelStat lbl (go s)
    go s@(BreakStat{}) = s
    go s@(ContinueStat{}) = s
    go (FuncStat n vs s) = FuncStat n vs (go s)

 -- remove double negation if we're using the expression in a loop/if condition
optCond :: JExpr -> JExpr
optCond e = let f (UOpExpr NotOp (UOpExpr NotOp e')) = f e'
                f e' = e'
            in f (opt e)

opt :: JExpr -> JExpr
opt (ValExpr v)          = ValExpr v
opt (SelExpr e i)        = SelExpr (opt e) i
opt (IdxExpr e1 e2)      = IdxExpr (opt e1) (opt e2)
-- ((c_e ? 1 : 0) === 1)   ==> !!c_e
-- ((c_e ? 1 : 0) === 0)   ==> !c_e
opt(InfixExpr StrictEqOp (IfExpr c_e (opt -> t_e) (opt -> f_e)) (opt -> e))
    | ValExpr t_v <- t_e
    , ValExpr v <- e
    , eqVal t_v v = UOpExpr NotOp (UOpExpr NotOp c_e)
    | ValExpr f_v <- f_e
    , ValExpr v <- e
    , eqVal f_v v = UOpExpr NotOp (opt c_e)
    | otherwise = InfixExpr StrictEqOp (IfExpr c_e t_e f_e) e
-- (1 === (c_e ? 1 : 0))   ==> !!c_e
-- (0 === (c_e ? 1 : 0))   ==> !c_e
opt(InfixExpr StrictEqOp (opt -> e) (IfExpr (opt -> c_e) (opt -> t_e) (opt -> f_e)))
    | ValExpr t_v <- t_e
    , ValExpr v <- e
    , eqVal t_v v = UOpExpr NotOp (UOpExpr NotOp c_e)
    | ValExpr f_v <- f_e
    , ValExpr v <- e
    , eqVal f_v v = UOpExpr NotOp c_e
    | otherwise = InfixExpr StrictEqOp e (IfExpr c_e t_e f_e)
opt (InfixExpr op (opt -> e1) (opt -> e2))
  | (ValExpr (JInt n1)) <- e1
  , (ValExpr (JInt n2)) <- e2
  , Just v <- optInt op n1 n2 = ValExpr v
  | (ValExpr (JBool b1)) <- e1
  , (ValExpr (JBool b2)) <- e2
  , Just v <- optBool op b1 b2 = ValExpr v
  | otherwise = InfixExpr op e1 e2
opt (UOpExpr op e)       = UOpExpr op (opt e)
opt (IfExpr e1 e2 e3)    = IfExpr (optCond e1) (opt e2) (opt e3)
opt (ApplExpr e es)      = ApplExpr (opt e) (map opt es)

{-
  Optimizations for operations on two known boolean values
 -}
optBool :: Op -> Bool -> Bool -> Maybe JVal
optBool LAndOp x y = Just (JBool (x && y))
optBool LOrOp x y = Just (JBool (x || y))
optBool EqOp x y = Just (JBool (x == y))
optBool StrictEqOp x y = Just (JBool (x == y))
optBool NeqOp x y = Just (JBool (x /= y))
optBool StrictNeqOp x y = Just (JBool (x /= y))
optBool _ _ _ = Nothing

{-
  Optimizations for operations on two known integer values
 -}
optInt :: Op -> Integer -> Integer -> Maybe JVal
optInt ZRightShiftOp n m = Just $
  JInt (toInteger $ (n .&. 0xffffffff) `shiftR` fromInteger (m .&. 0x1f))
optInt BOrOp n m = Just (truncOp (.|.) n m)
optInt BAndOp n m = Just (truncOp (.&.) n m)
optInt BXorOp n m = Just (truncOp (.^.) n m)
optInt RightShiftOp n m = Just (shiftOp shiftR n m)
optInt LeftShiftOp n m = Just (shiftOp shiftL n m)
optInt AddOp n m = smallIntOp (+) n m
optInt SubOp n m = smallIntOp (-) n m
optInt MulOp n m = smallIntOp (*) n m
optInt op n m
  | Just cmp <- getCmpOp op, isSmall52 n && isSmall52 m
  = Just (JBool (cmp n m))
optInt _ _ _ = Nothing

smallIntOp :: (Integer -> Integer -> Integer)
           -> Integer -> Integer -> Maybe JVal
smallIntOp op n m
  | isSmall52 n && isSmall52 m && isSmall52 r = Just (JInt r)
  | otherwise                                 = Nothing
  where
    r = op n m

getCmpOp :: Op -> Maybe (Integer -> Integer -> Bool)
getCmpOp EqOp = Just (==)
getCmpOp StrictEqOp = Just (==)
getCmpOp NeqOp = Just (/=)
getCmpOp StrictNeqOp = Just (/=)
getCmpOp GtOp = Just (>)
getCmpOp GeOp = Just (>=)
getCmpOp LtOp = Just (<)
getCmpOp LeOp = Just (<=)
getCmpOp _ = Nothing

shiftOp :: (Int32 -> Int -> Int32) -> Integer -> Integer -> JVal
shiftOp op n m = JInt $ toInteger
   (fromInteger n `op` (fromInteger m .&. 0x1f))

{-
  JavaScript bitwise operations truncate numbers to 32 bit signed integers.
  Here we do the same when constant folding with this kind of operators.
 -}
truncOp :: (Int32 -> Int32 -> Int32) -> Integer -> Integer -> JVal
truncOp op n m = JInt $ toInteger
   (fromInteger n `op` fromInteger m)

{-
  JavaScript numbers are IEEE 754 double precision floats, which have a
  52-bit mantissa. This returns True if the given integer can definitely
  be represented without loss of precision in a JavaScript number.
 -}
isSmall52 :: Integer -> Bool
isSmall52 n = n >= -0x10000000000000 && n <= 0xfffffffffffff

{-
  In JavaScript, e1 === e2 is not always true even if expressions e1 and e2
  are syntactically equal, examples:

    - NaN !== NaN  (NaN is not equal to itself)
    - [1] !== [1]  (different arrays allocated)
    - f() !== f()

  This returns True if the values are definitely equal in JavaScript
 -}
eqVal :: JVal -> JVal -> Bool
eqVal (JInt n1) (JInt n2)   = n1 == n2
eqVal (JStr s1) (JStr s2)   = s1 == s2
eqVal (JBool b1) (JBool b2) = b1 == b2
eqVal (JDouble (SaneDouble d1)) (JDouble (SaneDouble d2))
  | not (isNaN d1) && not (isNaN d2) = d1 == d2
eqVal _ _ = False
