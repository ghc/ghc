{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import Data.Kind
import System.Mem (performGC)
import GHC.Exts
import GHC.IO

type U :: UnliftedType
data U = U Int#

main :: IO ()
main = do
  res <- IO \ s0 ->
    case newMVar# s0 of
      (# s1, mvar #) ->
        case newMutVar# False s1 of
          (# s2, val_var #) ->
            case keepAlive# val_var s2 (inner mvar val_var) of
              (# s3, Res wk strs #) ->
                case unIO performGC s3 of
                  (# s4, _ #) ->
                    case deRefWeak# wk s4 of
                      (# s5, j, _ #) ->
                        case takeMVar# mvar s5 of
                          (# s6, r #) ->
                            (# s6, strs ++ [ show (I# j), r ] #)
  print res

data Res = Res (Weak# (MutableByteArray# RealWorld)) [String]

inner :: MVar# RealWorld String
      -> MutVar# RealWorld Bool
      -> State# RealWorld
      -> (# State# RealWorld, Res #)
inner mvar u s0 =
  case newByteArray# 42# s0 of
    (# s1, ba# #) ->
      case mkWeak# u ba# (finalise mvar) s1 of
        (# s2, wk #) ->
          case deRefWeak# wk s2 of
            (# s3, i, ba'# #) -> (# s3, Res wk [ show (I# i), show (I# (sizeofMutableByteArray# ba'#)) ] #)

finalise :: MVar# RealWorld String -> State# RealWorld -> (# State# RealWorld, () #)
finalise mvar s0 =
  case putMVar# mvar "finalised!" s0 of
    s1 -> (# s1, () #)
