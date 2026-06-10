{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ForeignFunctionInterface, GHCForeignImportPrim, UnliftedFFITypes #-}

module Main where

import Control.Monad
  ( unless )
import Data.Bits
  ( shiftL )
import GHC.Exts
  ( Int64# )
import GHC.Int
  ( Int64(..) )

foreign import prim "test_mul2_clobber"
  test_mul2_clobber :: Int64# -> Int64# -> Int64#

main :: IO ()
main = do
  let
    I64# x = 1 `shiftL` 32
    hi = I64# $ test_mul2_clobber x x

  unless ( hi == 1 ) $
    error $ unlines
      [ "Incorrect result for Mul2 operation."
      , "Expected high word: 1"
      , "  Actual high word: " ++ show hi
      ]
