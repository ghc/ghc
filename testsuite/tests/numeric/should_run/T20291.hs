{-# LANGUAGE CPP, TypeApplications #-}

#include "MachDeps.h"

module Main where

import Control.Exception
import Numeric.Natural

ok :: Natural -> IO ()
ok n | fromEnum n == i = putStrLn "OK"
     | otherwise       = putStrLn errmsg
  where
    i = fromIntegral n
    errmsg = ("fromEnum " ++) . shows n . (" == " ++) . shows i $ ": BAD"

bad :: Natural -> IO ()
bad n = do
  r <- try @ErrorCall . evaluate $ fromEnum n
  case r of
    Left _  ->
      putStrLn "Exception thrown as expected."
    Right _ ->
      putStrLn $
        ("fromEnum " ++) . shows n $ ": Exception not thrown when expected."

main :: IO ()
main = do
  let sizem2 = WORD_SIZE_IN_BITS - 2 :: Int
      sizem1 = WORD_SIZE_IN_BITS - 1 :: Int
      size   = WORD_SIZE_IN_BITS     :: Int
      sizep1 = WORD_SIZE_IN_BITS + 1 :: Int
  mapM_ ok
    [ 0
    , 2 ^ sizem2
    , 2 ^ sizem1 - 1
    ]
  mapM_ bad
    [ 2 ^ sizem1
    , 2 ^ size - 1
    , 2 ^ size
    , 2 ^ size + 2 ^ sizem1
    , 2 ^ sizep1 - 42
    , 2 ^ sizep1
    ]
