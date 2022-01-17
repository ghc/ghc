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

type U :: UnliftedType
data U = U Int#

main :: IO ()
main = do
  res <- IO \ s0 ->
    case newTVar# (U 1612#) s0 of
      (# s1, tvar #) ->
        case atomically# (readAndWrite tvar) s1 of
          (# s2, U r #) ->
            case readTVarIO# tvar s2 of
              (# s3, U res #) ->
                (# s3, [ I# r, I# res ] #)
  print res

readAndWrite :: TVar# s U -> State# s -> (# State# s, U #)
readAndWrite tvar = go
  where
    go s0 =
      case readTVar# tvar s0 of
        (# s1, U i #) ->
          case writeTVar# tvar (U (i *# 100#)) s1 of
            s2 -> (# s2, U i #)
