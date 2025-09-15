{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Exts
import GHC.IO

main :: IO ()
main = do
  res <- IO \ s0 ->
    case newMutVar# (41 :: Int) s0 of
      (# s1, mvar #) ->
        case newMutVar# mvar s1 of
          (# s2, mvarmvar #) ->
            case writeMutVar# mvar (17 :: Int) s2 of
              s3 ->
                case readMutVar# mvarmvar s3 of
                  (# s4, read_mvar #) ->
                    readMutVar# read_mvar s4
  print res
