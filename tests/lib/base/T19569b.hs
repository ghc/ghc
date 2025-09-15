-- This program is meant to compare the bracketing produced by the
-- actual implementation of (^) with the bracketing in the RHS of its
-- rewrite rules for known small powers, and complains if they disagree.

{-# OPTIONS_GHC -O -Wno-missing-methods #-}

module Main where

import Control.Monad
import Data.Typeable
import Numeric.Natural
import Text.Printf

data MulTree = X | FromInteger Integer | Mul MulTree MulTree
  deriving (Eq, Show)

instance Num MulTree where
  fromInteger = FromInteger
  (*) = Mul

opaquePow :: (Num a, Integral b) => a -> b -> a
{-# NOINLINE opaquePow #-}
opaquePow k e = k ^ e

checkRules
  :: forall expTy. (Integral expTy, Show expTy, Typeable expTy)
  => expTy -> IO ()
{-# INLINE checkRules #-}
checkRules _ = let
  checkOne :: expTy -> IO ()
  {-# INLINE checkOne #-}
  checkOne e = when (X ^ e /= opaquePow X e) (reportProblem (X ^ e) e)
  reportProblem :: MulTree -> expTy -> IO ()
  reportProblem wrongVal e = do
    printf "Problem with exponent (%s :: %s)\n" (show e) (show $ typeOf e)
    printf "  Expected: %s\n" (show $ opaquePow X e)
    printf "  Actual:   %s\n" (show wrongVal)
  in  do
  checkOne 0
  checkOne 1
  checkOne 2
  checkOne 3
  checkOne 4
  checkOne 5
  checkOne 6
  checkOne 7
  checkOne 8
  checkOne 9
  checkOne 10

main :: IO ()
main = do
  checkRules (0 :: Integer)
  checkRules (0 :: Natural)
  checkRules (0 :: Int)
  checkRules (0 :: Word)
