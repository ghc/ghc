{-# LANGUAGE BangPatterns, MagicHash, UnboxedTuples #-}
{-# OPTIONS_GHC -Wall #-}

module Main ( main ) where

import GHC.Base

data BA = BA ByteArray#

-- Checks that setByteArray# works
main :: IO ()
main = do BA ba <- IO $ \s0 ->
                        case newByteArray# 8# s0 of
                        (# !s1, !mba #) ->
                            case setByteArray# mba 0# 8# 65# s1 of
                            !s2 ->
                                case setByteArray# mba 1# 6# 67# s2 of
                                !s3 ->
                                    case unsafeFreezeByteArray# mba s3 of
                                    (# s4, ba #) -> (# s4, BA ba #)
          let f (I# i) = putStrLn [C# (indexCharArray# ba i)]
          mapM_ f [0..7]

