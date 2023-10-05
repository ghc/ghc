{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import GHC.Exts
import GHC.IO

type U :: UnliftedType
data U = X | Y

showU :: U -> String
showU X = "X"
showU Y = "Y"

main :: IO ()
main = do
  res <- IO \ s0 ->
    case newMutVar# X s0 of
      (# s1, mvar #) ->
        case readMutVar# mvar s1 of
          (# s2, r1 #) ->
            case writeMutVar# mvar Y s2 of
              s3 -> case readMutVar# mvar s3 of
                (# s4, r2 #) ->
                  (# s4, [ showU r1, showU r2 ] #)
  putStrLn (unwords res)
