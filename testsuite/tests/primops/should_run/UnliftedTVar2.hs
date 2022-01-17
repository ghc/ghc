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
  (x,y) <- IO \ s0 ->
    case newTVar# (U 0#) s0 of
      (# s1, tvar #) ->
        case fork# (increment tvar) s1 of
          (# s2, t_id #) ->
            case atomically# (readUntil tvar) s2 of
              (# s3, U r #) ->
                case killThread# t_id 13 s3 of
                  s4 ->
                    case readTVarIO# tvar s4 of
                      (# s5, U res #) ->
                        (# s5, ( I# r, I# res ) #)
  print (x == y, x > 100000)

increment :: TVar# RealWorld U -> State# RealWorld -> (# State# RealWorld, Int #)
increment tvar = go
  where
    go :: State# RealWorld -> (# State# RealWorld, Int #)
    go s0 = case atomically# inc s0 of
      (# s1, res #) -> go s1

    inc :: State# RealWorld -> (# State# RealWorld, Int #)
    inc s0 =
      case readTVar# tvar s0 of
        (# s1, U v #) ->
          case writeTVar# tvar (U (v +# 1#)) s1 of
            s2 -> (# s2, I# v #)

readUntil :: TVar# RealWorld U -> State# RealWorld -> (# State# RealWorld, U #)
readUntil tvar = go
  where
    go s0 =
      case readTVar# tvar s0 of
        (# s1, r@(U i) #)
          | I# i >= 100000
          -> (# s1, r #)
          | otherwise
          -> retry# s1
