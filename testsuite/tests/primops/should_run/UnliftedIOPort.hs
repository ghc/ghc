{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import Data.Kind
import GHC.Exts
import GHC.IO

type U :: Type
data U = U Int#

main :: IO ()
main = do
  res <- IO \ s0 ->
    case newIOPort# s0 of
      (# s1, port #) ->
        case writeIOPort# port (U 17#) s1 of
          (# s2, i #) ->
            case catch# (writeIOPort# port (U 19#)) (\ _ s -> (# s, 3# #)) s2 of
              (# s3, j #) ->
                case readIOPort# port s3 of
                  (# s4, U r1 #) ->
                    case catch# (readIOPort# port) (\ _ s -> (# s, U 4# #)) s4 of
                      (# s5, U r2 #) ->
                        (# s5, [ I# i, I# j, I# r1, I# r2 ] #)
  print res
