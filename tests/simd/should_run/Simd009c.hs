{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Simd009c where

import Control.Monad ( unless )
import Data.Foldable ( for_ )
import GHC.Exts
import GHC.Prim
import Language.Haskell.TH ( CodeQ )
import Language.Haskell.TH.Syntax ( Lift(liftTyped) )

import Simd009b

floatX4ShuffleTest :: CodeQ FloatX4 -> CodeQ FloatX4 -> CodeQ (Int, Int, Int, Int) -> CodeQ (IO ())
floatX4ShuffleTest v1 v2 ijkl =
  [||
  do
    let (I# i#, I# j#, I# k#, I# l#) = $$ijkl
        FX4# v1# = $$v1
        FX4# v2# = $$v2
        s1 = shuffleFloatX4# v1# v2# (# i#, j#, k#, l# #)
        s2 = myShuffleFloatX4 v1# v2# (# i#, j#, k#, l# #)
    unless (FX4# s1 == FX4# s2) $ do
      putStrLn $ "Failed test: FloatX4# shuffle " ++ show (I# i#, I# j#, I# k#, I# l# )
      putStrLn $ "     SIMD: " ++ show (FX4# s1)
      putStrLn $ "reference: " ++ show (FX4# s2)
  ||]

doubleX2ShuffleTest :: CodeQ DoubleX2 -> CodeQ DoubleX2 -> CodeQ (Int, Int) -> CodeQ (IO ())
doubleX2ShuffleTest v1 v2 ij =
  [||
  do
    let (I# i#, I# j#) = $$ij
        DX2# v1# = $$v1
        DX2# v2# = $$v2
        s1 = shuffleDoubleX2# v1# v2# (# i#, j# #)
        s2 = myShuffleDoubleX2 v1# v2# (# i#, j# #)
    unless (DX2# s1 == DX2# s2) $ do
      putStrLn $ "Failed test: DoubleX2# shuffle " ++ show (I# i#, I# j#)
      putStrLn $ "     SIMD: " ++ show (DX2# s1)
      putStrLn $ "reference: " ++ show (DX2# s2)
  ||]

forQ_ :: Lift i => [i] -> (CodeQ i -> CodeQ (IO ())) -> CodeQ (IO ())
forQ_ [] _ = [|| return () ||]
forQ_ (i:is) f = [|| $$( f (liftTyped i) ) *> $$( forQ_ is f ) ||]
